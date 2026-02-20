library(httr2)
library(jsonlite)

load_fred_key <- function() {
  env_candidates <- c(
    ".env",
    "dsai/01_query_api/.env",
    "../.env",
    "../../.env"
  )

  for (f in env_candidates) {
    if (file.exists(f)) {
      readRenviron(f)
      key <- Sys.getenv("FRED_API_KEY")
      if (nzchar(key)) {
        return(key)
      }
    }
  }

  Sys.getenv("FRED_API_KEY")
}

fetch_series <- function(series_id, api_key, start_date = "2015-01-01", end_date = Sys.Date()) {
  resp <- request("https://api.stlouisfed.org/fred/series/observations") |>
    req_url_query(
      series_id = series_id,
      observation_start = start_date,
      observation_end = as.character(end_date),
      sort_order = "asc",
      file_type = "json",
      api_key = api_key
    ) |>
    req_timeout(30) |>
    req_perform()

  if (resp$status_code != 200) {
    stop(paste("FRED request failed for", series_id, "with status", resp$status_code))
  }

  obj <- resp_body_json(resp)
  if (is.null(obj$observations) || length(obj$observations) == 0) {
    stop(paste("No observations returned for", series_id))
  }

  df <- data.frame(
    date = as.Date(vapply(obj$observations, function(x) x$date, character(1))),
    value = suppressWarnings(as.numeric(vapply(obj$observations, function(x) x$value, character(1)))),
    series_id = series_id,
    stringsAsFactors = FALSE
  )

  df <- df[!is.na(df$value), ]
  if (nrow(df) == 0) {
    stop(paste("All values are NA for", series_id))
  }

  df
}

calc_summary <- function(df) {
  df <- df[order(df$date), ]
  latest_n <- min(12, nrow(df))
  recent <- tail(df, latest_n)

  latest_value <- tail(df$value, 1)
  first_recent <- recent$value[1]
  pct_change_recent <- if (!isTRUE(all.equal(first_recent, 0))) {
    (latest_value - first_recent) / abs(first_recent) * 100
  } else {
    NA_real_
  }

  data.frame(
    series_id = unique(df$series_id),
    obs_count = nrow(df),
    latest_date = as.character(tail(df$date, 1)),
    latest_value = latest_value,
    mean_value = mean(df$value),
    min_value = min(df$value),
    max_value = max(df$value),
    sd_value = sd(df$value),
    recent_window = latest_n,
    recent_pct_change = pct_change_recent,
    trend_label = ifelse(is.na(pct_change_recent), "unknown",
                         ifelse(pct_change_recent > 1, "up",
                                ifelse(pct_change_recent < -1, "down", "flat"))),
    stringsAsFactors = FALSE
  )
}

build_prompt_payload <- function(summary_df) {
  bullets <- apply(summary_df, 1, function(r) {
    paste0(
      "- ", r[["series_id"]],
      ": latest=", sprintf("%.3f", as.numeric(r[["latest_value"]])),
      ", recent trend=", r[["trend_label"]],
      ", recent pct change=", sprintf("%.2f", as.numeric(r[["recent_pct_change"]])), "%"
    )
  })

  paste(
    "You are a macro risk analyst.",
    "Given the summary data below, write:",
    "1) a 5-bullet executive summary,",
    "2) key risks to monitor next month,",
    "3) one cautious and one aggressive positioning idea.",
    "Use concise professional language.",
    "",
    "Data summary:",
    paste(bullets, collapse = "\n"),
    sep = "\n"
  )
}

main <- function() {
  api_key <- load_fred_key()
  if (!nzchar(api_key)) {
    stop("FRED_API_KEY not found. Add FRED_API_KEY=... in .env")
  }

  series_ids <- c("FEDFUNDS", "UNRATE", "CPIAUCSL", "DGS10")

  all_data <- do.call(rbind, lapply(series_ids, function(s) {
    fetch_series(series_id = s, api_key = api_key)
  }))

  summary_df <- do.call(rbind, lapply(split(all_data, all_data$series_id), calc_summary))
  summary_df <- summary_df[order(summary_df$series_id), ]

  # Wide table for easier reporter/LLM ingestion
  merged_wide <- reshape(
    all_data[, c("date", "series_id", "value")],
    idvar = "date",
    timevar = "series_id",
    direction = "wide"
  )
  merged_wide <- merged_wide[order(merged_wide$date), ]

  dir.create("dsai/03_ai_reporting/data", recursive = TRUE, showWarnings = FALSE)
  dir.create("dsai/03_ai_reporting/reports", recursive = TRUE, showWarnings = FALSE)

  write.csv(all_data, "dsai/03_ai_reporting/data/fred_raw_long.csv", row.names = FALSE)
  write.csv(merged_wide, "dsai/03_ai_reporting/data/fred_processed_wide.csv", row.names = FALSE)
  write.csv(summary_df, "dsai/03_ai_reporting/data/fred_summary.csv", row.names = FALSE)

  payload <- list(
    generated_at = as.character(Sys.time()),
    series = series_ids,
    summary = summary_df,
    sample_recent_rows = tail(merged_wide, 20)
  )

  write_json(payload, "dsai/03_ai_reporting/data/fred_ai_payload.json", auto_unbox = TRUE, pretty = TRUE)

  prompt_text <- build_prompt_payload(summary_df)
  writeLines(prompt_text, con = "dsai/03_ai_reporting/reports/prompt_input.txt")

  cat("Pipeline complete. Files created:\n")
  cat("- dsai/03_ai_reporting/data/fred_raw_long.csv\n")
  cat("- dsai/03_ai_reporting/data/fred_processed_wide.csv\n")
  cat("- dsai/03_ai_reporting/data/fred_summary.csv\n")
  cat("- dsai/03_ai_reporting/data/fred_ai_payload.json\n")
  cat("- dsai/03_ai_reporting/reports/prompt_input.txt\n")
}

main()