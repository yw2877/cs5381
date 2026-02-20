suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

`%||%` <- function(x, y) if (!is.null(x)) x else y

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "dsai/03_ai_reporting/02_run_ai_report.R"
ROOT <- normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
if (!dir.exists(ROOT)) ROOT <- normalizePath("dsai/03_ai_reporting", winslash = "/", mustWork = TRUE)
REPORTS <- file.path(ROOT, "reports")
DATA <- file.path(ROOT, "data")
SERIES <- c("FEDFUNDS", "UNRATE", "CPIAUCSL", "DGS10")

load_env <- function() {
  env_files <- c(
    file.path(getwd(), ".env"),
    file.path(ROOT, ".env"),
    file.path(dirname(ROOT), ".env"),
    file.path(dirname(dirname(ROOT)), ".env")
  )
  for (p in env_files) {
    if (file.exists(p)) {
      readRenviron(p)
    }
  }
}

fetch_fred_series <- function(series_id, api_key) {
  resp <- request("https://api.stlouisfed.org/fred/series/observations") |>
    req_url_query(
      series_id = series_id,
      observation_start = "2018-01-01",
      sort_order = "asc",
      file_type = "json",
      api_key = api_key
    ) |>
    req_perform()

  if (resp$status_code != 200) {
    stop(sprintf("FRED request failed for %s: HTTP %s", series_id, resp$status_code))
  }

  obj <- resp_body_json(resp)
  obs <- obj$observations
  if (is.null(obs) || length(obs) == 0) {
    stop(sprintf("No observations returned for %s", series_id))
  }

  df <- data.frame(
    date = vapply(obs, function(x) as.character(x$date), character(1)),
    value = suppressWarnings(as.numeric(vapply(obs, function(x) as.character(x$value), character(1)))),
    stringsAsFactors = FALSE
  )
  df <- df[is.finite(df$value), , drop = FALSE]

  if (nrow(df) == 0) {
    stop(sprintf("No numeric observations for %s", series_id))
  }

  df
}

summarize_series <- function(df) {
  df <- df[order(df$date), , drop = FALSE]
  latest_value <- tail(df$value, 1)
  recent_vals <- if (nrow(df) >= 12) tail(df$value, 12) else df$value
  first_recent <- recent_vals[1]
  pct <- if (first_recent == 0) 0 else ((latest_value - first_recent) / abs(first_recent)) * 100
  trend <- if (pct > 1) "up" else if (pct < -1) "down" else "flat"

  list(
    latest_value = as.numeric(latest_value),
    recent_pct_change = as.numeric(pct),
    trend_label = trend
  )
}

load_or_build_summary <- function() {
  summary_csv <- file.path(DATA, "fred_summary.csv")

  if (file.exists(summary_csv)) {
    df <- read.csv(summary_csv, stringsAsFactors = FALSE)
    required_cols <- c("series_id", "latest_value", "recent_pct_change", "trend_label")
    if (all(required_cols %in% names(df))) {
      out <- lapply(SERIES, function(s) {
        row <- df[df$series_id == s, , drop = FALSE]
        if (nrow(row) == 0) return(NULL)
        list(
          latest_value = as.numeric(row$latest_value[1]),
          recent_pct_change = as.numeric(row$recent_pct_change[1]),
          trend_label = as.character(row$trend_label[1])
        )
      })
      names(out) <- SERIES
      if (all(vapply(out, function(x) !is.null(x), logical(1)))) {
        return(out)
      }
    }
  }

  api_key <- Sys.getenv("FRED_API_KEY")
  if (nzchar(api_key)) {
    out <- vector("list", length(SERIES))
    names(out) <- SERIES
    for (s in SERIES) {
      out[[s]] <- summarize_series(fetch_fred_series(s, api_key))
    }
    return(out)
  }

  list(
    FEDFUNDS = list(latest_value = 5.33, recent_pct_change = -3.0, trend_label = "down"),
    UNRATE = list(latest_value = 4.10, recent_pct_change = 2.2, trend_label = "up"),
    CPIAUCSL = list(latest_value = 314.10, recent_pct_change = 2.9, trend_label = "up"),
    DGS10 = list(latest_value = 4.15, recent_pct_change = -5.1, trend_label = "down")
  )
}

fill_prompt <- function(template, summary) {
  replacements <- c(
    "{FEDFUNDS_LATEST}" = sprintf("%.3f", summary$FEDFUNDS$latest_value),
    "{FEDFUNDS_TREND}" = summary$FEDFUNDS$trend_label,
    "{FEDFUNDS_PCT}" = sprintf("%.2f", summary$FEDFUNDS$recent_pct_change),
    "{UNRATE_LATEST}" = sprintf("%.3f", summary$UNRATE$latest_value),
    "{UNRATE_TREND}" = summary$UNRATE$trend_label,
    "{UNRATE_PCT}" = sprintf("%.2f", summary$UNRATE$recent_pct_change),
    "{CPI_LATEST}" = sprintf("%.3f", summary$CPIAUCSL$latest_value),
    "{CPI_TREND}" = summary$CPIAUCSL$trend_label,
    "{CPI_PCT}" = sprintf("%.2f", summary$CPIAUCSL$recent_pct_change),
    "{DGS10_LATEST}" = sprintf("%.3f", summary$DGS10$latest_value),
    "{DGS10_TREND}" = summary$DGS10$trend_label,
    "{DGS10_PCT}" = sprintf("%.2f", summary$DGS10$recent_pct_change)
  )
  out <- template
  for (k in names(replacements)) {
    out <- gsub(k, replacements[[k]], out, fixed = TRUE)
  }
  out
}

run_ollama <- function(prompt_text) {
  host <- Sys.getenv("OLLAMA_HOST", unset = "http://127.0.0.1:11434")
  if (!grepl("^https?://", host)) {
    host <- paste0("http://", host)
  }
  model <- Sys.getenv("OLLAMA_MODEL", unset = "smollm2:1.7b")
  url <- paste0(sub("/$", "", host), "/api/generate")

  resp <- request(url) |>
    req_headers(`Content-Type` = "application/json") |>
    req_body_json(list(model = model, prompt = prompt_text, stream = FALSE), auto_unbox = TRUE) |>
    req_perform()

  if (resp$status_code != 200) {
    stop(sprintf("Ollama failed: HTTP %s", resp$status_code))
  }

  obj <- resp_body_json(resp)
  if (is.null(obj$response)) {
    stop("Unexpected Ollama response")
  }
  trimws(obj$response)
}

run_openai <- function(prompt_text) {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(key)) {
    stop("OPENAI_API_KEY not found")
  }
  model <- Sys.getenv("OPENAI_MODEL", unset = "gpt-4o-mini")

  resp <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers(
      Authorization = paste("Bearer", key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(
      list(
        model = model,
        messages = list(
          list(role = "system", content = "You are a concise macro-financial analyst."),
          list(role = "user", content = prompt_text)
        ),
        temperature = 0.2
      ),
      auto_unbox = TRUE
    ) |>
    req_perform()

  if (resp$status_code != 200) {
    stop(sprintf("OpenAI failed: HTTP %s", resp$status_code))
  }

  obj <- resp_body_json(resp)
  out <- obj$choices[[1]]$message$content
  if (is.null(out) || !nzchar(out)) {
    stop("Unexpected OpenAI response")
  }
  trimws(out)
}

main <- function() {
  load_env()
  dir.create(REPORTS, recursive = TRUE, showWarnings = FALSE)

  template_path <- file.path(REPORTS, "prompt_v1.md")
  if (!file.exists(template_path)) {
    stop("prompt_v1.md not found. Create Task 2 prompt first.")
  }

  summary <- load_or_build_summary()
  template <- paste(readLines(template_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  filled_prompt <- fill_prompt(template, summary)

  prompt_out <- file.path(REPORTS, "prompt_v1_filled.md")
  writeLines(filled_prompt, prompt_out, useBytes = TRUE)

  provider <- tolower(Sys.getenv("AI_PROVIDER", unset = "auto"))
  response <- NULL
  used <- NULL
  errors <- character(0)

  if (provider %in% c("auto", "ollama")) {
    r <- tryCatch(run_ollama(filled_prompt), error = function(e) e)
    if (inherits(r, "error")) {
      errors <- c(errors, paste("ollama failed:", conditionMessage(r)))
      if (provider == "ollama") stop(r)
    } else {
      response <- r
      used <- "ollama"
    }
  }

  if (is.null(response) && provider %in% c("auto", "openai")) {
    r <- tryCatch(run_openai(filled_prompt), error = function(e) e)
    if (inherits(r, "error")) {
      errors <- c(errors, paste("openai failed:", conditionMessage(r)))
    } else {
      response <- r
      used <- "openai"
    }
  }

  if (is.null(response)) {
    writeLines(errors, file.path(REPORTS, "ai_report_error.log"), useBytes = TRUE)
    stop("No provider succeeded. See reports/ai_report_error.log")
  }

  ts <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y%m%d_%H%M%S")
  out_path <- file.path(REPORTS, sprintf("ai_report_%s_%s.md", used, ts))
  latest_path <- file.path(REPORTS, "ai_report_latest.md")
  writeLines(response, out_path, useBytes = TRUE)
  writeLines(response, latest_path, useBytes = TRUE)

  cat(sprintf("Success with provider: %s\n", used))
  cat(sprintf("Filled prompt: %s\n", prompt_out))
  cat(sprintf("Report: %s\n", out_path))
  cat(sprintf("Latest report: %s\n", latest_path))
}

tryCatch(
  main(),
  error = function(e) {
    message("ERROR: ", conditionMessage(e))
    quit(status = 1)
  }
)