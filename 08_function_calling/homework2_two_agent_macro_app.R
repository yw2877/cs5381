# Homework 2: Two-Agent Macro Briefing App
#
# This single-file script compiles work from the previous three labs:
# - 06_agents/LAB_prompt_design.md -> two-agent orchestration and prompt design
# - 07_rag/LAB_custom_rag_query.md -> Mishkin knowledge-base retrieval (RAG)
# - 08_function_calling/LAB_multi_agent_with_tools.md -> tool calling with live data
#
# The result is a terminal-based app prototype that answers a macroeconomic
# question using:
# 1. Agent 1: tool-backed evidence building
# 2. Agent 2: concise macro briefing
#
# The app combines live FRED data, textbook retrieval, and market-risk proxy
# indicators into a user-readable result without modifying the original lab files.

suppressWarnings(suppressPackageStartupMessages(library(httr2)))
suppressWarnings(suppressPackageStartupMessages(library(jsonlite)))
suppressWarnings(suppressPackageStartupMessages(library(knitr)))
suppressWarnings(suppressPackageStartupMessages(library(ollamar)))


# 0. Paths and configuration ###################################################

args_all = commandArgs(trailingOnly = FALSE)
file_arg = grep("^--file=", args_all, value = TRUE)
SCRIPT_PATH = if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg[1]))
} else if (!is.null(sys.frames()[[1]]$ofile)) {
  normalizePath(sys.frames()[[1]]$ofile)
} else {
  normalizePath(
    file.path(getwd(), "08_function_calling", "homework2_two_agent_macro_app.R"),
    mustWork = FALSE
  )
}
SCRIPT_DIR = dirname(SCRIPT_PATH)
REPO_ROOT = normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = TRUE)

if (Sys.getenv("OLLAMA_HOST") == "") {
  Sys.setenv(OLLAMA_HOST = "http://127.0.0.1:11434")
}

APP_NAME = "Homework 2 Two-Agent Macro Briefing App"
APP_PURPOSE = paste(
  "A terminal app prototype that combines multi-agent orchestration,",
  "RAG retrieval, and function calling to explain current U.S. macro conditions."
)

KNOWLEDGE_BASE = file.path(REPO_ROOT, "07_rag", "mishkin_fred_explained.txt")
DEFAULT_SERIES = c("FEDFUNDS", "UNRATE", "CPIAUCSL", "DGS10")
DEFAULT_START_DATE = "2021-01-01"
DEFAULT_LIMIT = 240L
DEFAULT_TOP_K = 2L
TODAY = as.character(Sys.Date())

SERIES_LIBRARY = data.frame(
  series_id = c("FEDFUNDS", "EFFR", "UNRATE", "CPIAUCSL", "PCEPI", "DGS10", "DGS2", "SP500", "VIXCLS"),
  title = c(
    "Federal Funds Rate",
    "Effective Federal Funds Rate",
    "Unemployment Rate",
    "Consumer Price Index for All Urban Consumers",
    "Personal Consumption Expenditures Price Index",
    "10-Year Treasury Constant Maturity Rate",
    "2-Year Treasury Constant Maturity Rate",
    "S&P 500",
    "CBOE Volatility Index: VIX"
  ),
  context_note = c(
    "Higher values usually imply tighter short-term policy.",
    "Higher values usually imply tighter short-term policy.",
    "Higher values usually imply a softer labor market.",
    "Higher year-over-year inflation implies stronger price pressure.",
    "Higher year-over-year inflation implies stronger price pressure.",
    "Higher values imply tighter long-term financing conditions.",
    "Useful for term-spread comparisons against DGS10.",
    "Useful for market-wide equity risk proxies such as historical index VaR.",
    "Higher values imply a more stressed implied-volatility backdrop."
  ),
  stringsAsFactors = FALSE
)

DEFAULT_QUERY = paste(
  "Use live FRED data, the Mishkin knowledge base, and the market-risk appendix",
  "to explain whether current U.S. macro conditions look restrictive.",
  "Discuss FEDFUNDS, UNRATE, CPIAUCSL, DGS10, and the risk backdrop in a way",
  "that a student or stakeholder can understand."
)

user_args = commandArgs(trailingOnly = TRUE)
USER_QUERY = if (length(user_args) > 0) {
  paste(user_args, collapse = " ")
} else {
  DEFAULT_QUERY
}


# 1. Multi-agent helpers inherited from prior labs #############################

chat_with_diagnostics = function(model, messages, output = "text", tools = NULL) {
  tryCatch(
    {
      if (is.null(tools)) {
        ollamar::chat(model = model, messages = messages, output = output, stream = FALSE)
      } else {
        ollamar::chat(model = model, messages = messages, tools = tools, output = output, stream = FALSE)
      }
    },
    error = function(e) {
      msg = conditionMessage(e)
      if (grepl("HTTP 404", msg, fixed = TRUE)) {
        host = Sys.getenv("OLLAMA_HOST")
        if (identical(host, "")) {
          host = "http://127.0.0.1:11434"
        }
        stop(
          paste0(
            "Ollama returned HTTP 404 for model '", model, "' at ", host, ". ",
            "This usually means the model is not installed on that Ollama server. ",
            "Run ensure_ollama_model('", model, "') before agent_run(), or pull it manually with ollamar::pull()."
          ),
          call. = FALSE
        )
      }
      stop(e)
    }
  )
}

ensure_ollama_model = function(model, auto_pull = TRUE) {
  host = Sys.getenv("OLLAMA_HOST")
  if (identical(host, "")) {
    host = "http://127.0.0.1:11434"
  }

  models = tryCatch(
    ollamar::list_models(),
    error = function(e) {
      stop(
        paste0(
          "Could not reach Ollama at ", host, ". ",
          "Start the Ollama server or set OLLAMA_HOST correctly. Original error: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  model_names = if ("name" %in% names(models)) models$name else character()
  has_model = any(model_names == model)
  if (has_model) {
    return(invisible(model))
  }

  if (!auto_pull) {
    installed = if (length(model_names) > 0) paste(model_names, collapse = ", ") else "<none>"
    stop(
      paste0(
        "Ollama model '", model, "' is not installed on ", host, ". Installed models: ", installed
      ),
      call. = FALSE
    )
  }

  message("Ollama model '", model, "' is not installed. Pulling it now...")
  tryCatch(
    {
      ollamar::pull(model)
      invisible(model)
    },
    error = function(e) {
      stop(
        paste0(
          "Failed to pull Ollama model '", model, "'. Original error: ", conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

df_as_text = function(df) {
  tab = knitr::kable(df, format = "markdown")
  paste0(as.character(tab), collapse = "\n")
}

object_as_text = function(x) {
  trim_text = function(text, max_chars = 8000) {
    if (nchar(text) <= max_chars) {
      return(text)
    }
    paste0(substr(text, 1, max_chars), "\n\n[truncated for prompt length]")
  }

  if (is.null(x)) {
    return("<null>")
  }

  if (is.data.frame(x)) {
    return(trim_text(df_as_text(x)))
  }

  if (is.atomic(x) && length(x) == 1) {
    return(as.character(x))
  }

  json_attempt = tryCatch(
    jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    error = function(e) NULL
  )
  if (!is.null(json_attempt)) {
    return(trim_text(as.character(json_attempt)))
  }

  trim_text(paste(utils::capture.output(print(x)), collapse = "\n"))
}

execute_tool_call = function(tool_call) {
  tryCatch(
    do.call(tool_call$name, tool_call$arguments),
    error = function(e) {
      list(
        error = TRUE,
        tool = tool_call$name,
        message = conditionMessage(e)
      )
    }
  )
}

tool_calls_as_text = function(tool_calls) {
  if (length(tool_calls) == 0) {
    return("No tool calls were made.")
  }

  blocks = lapply(seq_along(tool_calls), function(i) {
    tool_call = tool_calls[[i]]
    arg_text = object_as_text(tool_call$arguments)
    out_text = object_as_text(tool_call$output)
    paste(
      paste0("Tool ", i, ": ", tool_call$name),
      "Arguments:",
      arg_text,
      "Output:",
      out_text,
      sep = "\n"
    )
  })

  paste(blocks, collapse = "\n\n")
}

agent = function(messages, model = "smollm2:1.7b", output = "text", tools = NULL, all = FALSE) {
  if (is.null(tools)) {
    return(chat_with_diagnostics(model = model, messages = messages, output = output))
  }

  tool_calls = chat_with_diagnostics(
    model = model,
    messages = messages,
    tools = tools,
    output = "tools"
  )

  n_calls = length(tool_calls)
  if (n_calls > 0) {
    for (i in seq_len(n_calls)) {
      tool_calls[[i]]$output = execute_tool_call(tool_calls[[i]])
    }
  }

  if (all || identical(output, "tools")) {
    return(tool_calls)
  }

  if (n_calls == 0) {
    stop(
      paste0(
        "The model did not request any tool calls for a tool-enabled agent. ",
        "Try a more capable model or tighten the agent prompt."
      ),
      call. = FALSE
    )
  }

  followup_messages = c(
    messages,
    ollamar::create_message(
      role = "user",
      content = paste(
        "Tool results are now available.",
        "Use only these tool results plus the original request to produce the final answer.",
        "If a tool output includes an error, acknowledge the limitation instead of inventing a result.",
        "",
        tool_calls_as_text(tool_calls),
        sep = "\n"
      )
    )
  )

  chat_with_diagnostics(model = model, messages = followup_messages, output = output)
}

agent_run = function(role, task, tools = NULL, output = "text", model = MODEL) {
  messages = ollamar::create_messages(
    ollamar::create_message(role = "system", content = role),
    ollamar::create_message(role = "user", content = task)
  )

  agent(messages = messages, model = model, output = output, tools = tools)
}


# 2. RAG helpers inherited from the 07 lab ####################################

read_text = function(document_path) {
  lines = readLines(document_path, warn = FALSE, encoding = "UTF-8")
  paste(lines, collapse = "\n")
}

extract_field = function(section_text, label) {
  pattern = paste0(label, "\n([\\s\\S]*?)(\n\n|$)")
  match = regexec(pattern, section_text, perl = TRUE)
  parts = regmatches(section_text, match)[[1]]

  if (length(parts) >= 2) {
    return(trimws(parts[2]))
  }

  ""
}

normalize_tokens = function(text) {
  stopwords = c(
    "what", "does", "mean", "usually", "with", "from", "that", "this",
    "into", "about", "when", "where", "which", "would", "could", "should",
    "have", "has", "had", "using", "used", "your", "their", "how", "can",
    "estimate", "still", "matter", "data", "current", "live", "explain",
    "include", "discuss", "whether", "look", "looks", "backdrop"
  )

  clean = tolower(text)
  clean = gsub("[^a-z0-9 ]", " ", clean)
  pieces = unlist(strsplit(clean, "\\s+"))
  pieces = pieces[nchar(pieces) >= 3]
  pieces = pieces[!(pieces %in% stopwords)]
  unique(pieces)
}

make_preview = function(section_text, max_chars = 360) {
  compact = gsub("\\s+", " ", section_text)
  compact = trimws(compact)

  if (nchar(compact) <= max_chars) {
    return(compact)
  }

  paste0(substr(compact, 1, max_chars), "...")
}

split_into_sections = function(document_text) {
  lines = unlist(strsplit(document_text, "\n"))
  header_pattern = "^(TOPIC [0-9]+: .+|HIGH-VALUE CROSS-TOPIC INTERPRETATION RULES FOR RAG ANSWERS)$"
  header_idx = grep(header_pattern, lines, perl = TRUE)
  routing_idx = grep("^QUERY-TO-TOPIC ROUTING GUIDE$", lines, perl = TRUE)

  if (length(header_idx) == 0) {
    return(list())
  }

  sections = vector("list", length(header_idx))

  for (i in seq_along(header_idx)) {
    start_idx = header_idx[i]
    if (i < length(header_idx)) {
      end_idx = header_idx[i + 1] - 1
    } else if (length(routing_idx) > 0) {
      end_idx = routing_idx[1] - 1
    } else {
      end_idx = length(lines)
    }

    chunk_lines = lines[start_idx:end_idx]
    chunk = trimws(paste(chunk_lines, collapse = "\n"))

    sections[[i]] = list(
      title = trimws(chunk_lines[1]),
      entry_id = extract_field(chunk, "Entry ID:"),
      retrieval_label = extract_field(chunk, "Retrieval Label:"),
      keywords = extract_field(chunk, "Keywords:"),
      primary_fred_series = extract_field(chunk, "Primary FRED Series:"),
      typical_questions = extract_field(chunk, "Typical Questions:"),
      answer_focus = extract_field(chunk, "Answer Focus:"),
      content = chunk,
      preview = make_preview(chunk)
    )
  }

  sections
}

count_token_hits = function(token, text) {
  matches = gregexpr(token, text, fixed = TRUE)[[1]]

  if (length(matches) == 1 && matches[1] == -1) {
    return(0)
  }

  length(matches)
}

score_section = function(query, section) {
  query_text = tolower(query)
  query_tokens = normalize_tokens(query)

  title = tolower(section$title)
  keywords = tolower(section$keywords)
  questions = tolower(section$typical_questions)
  answer_focus = tolower(section$answer_focus)
  content = tolower(section$content)

  score = 0
  matched_terms = character(0)

  if (grepl(query_text, content, fixed = TRUE)) {
    score = score + 10
  }

  for (token in query_tokens) {
    token_score = 0

    if (grepl(token, title, fixed = TRUE)) token_score = token_score + 8
    if (grepl(token, keywords, fixed = TRUE)) token_score = token_score + 6
    if (grepl(token, questions, fixed = TRUE)) token_score = token_score + 4
    if (grepl(token, answer_focus, fixed = TRUE)) token_score = token_score + 3

    body_hits = count_token_hits(token, content)
    if (body_hits > 0) token_score = token_score + min(body_hits, 5)

    if (token_score > 0) {
      matched_terms = c(matched_terms, token)
      score = score + token_score
    }
  }

  list(score = score, matched_terms = unique(matched_terms))
}

extract_formulas_from_section = function(section_text, max_formulas = 3) {
  lines = trimws(unlist(strsplit(section_text, "\n")))
  header_idx = grep("^(Formula [0-9]+:|FRED-Linked Formula [0-9]+:)", lines)

  if (length(header_idx) == 0) {
    return(character(0))
  }

  formulas = character(0)
  for (idx in header_idx) {
    title_line = lines[idx]
    next_line = if (idx < length(lines)) lines[idx + 1] else ""
    if (nzchar(next_line) && !grepl("^[A-Za-z].+:", next_line)) {
      formulas = c(formulas, paste(title_line, next_line, sep = " | "))
    } else {
      formulas = c(formulas, title_line)
    }
  }

  unique(formulas[seq_len(min(max_formulas, length(formulas)))])
}

search_mishkin_context = function(query, document_path, top_k = DEFAULT_TOP_K) {
  if (!file.exists(document_path)) {
    stop(paste("Knowledge base file not found:", document_path))
  }

  document_text = read_text(document_path)
  sections = split_into_sections(document_text)

  if (length(sections) == 0) {
    stop("No retrievable sections were found in the Mishkin knowledge base.")
  }

  scored_results = lapply(sections, function(section) {
    scored = score_section(query, section)

    list(
      title = section$title,
      entry_id = section$entry_id,
      retrieval_label = section$retrieval_label,
      primary_fred_series = section$primary_fred_series,
      score = scored$score,
      matched_terms = scored$matched_terms,
      preview = section$preview,
      formulas = extract_formulas_from_section(section$content)
    )
  })

  scores = vapply(scored_results, function(x) x$score, numeric(1))
  order_idx = order(scores, decreasing = TRUE)
  ranked_results = scored_results[order_idx]
  top_results = ranked_results[seq_len(min(top_k, length(ranked_results)))]

  list(
    query = query,
    document = basename(document_path),
    num_sections_considered = length(sections),
    num_sections_returned = length(top_results),
    results = top_results
  )
}


# 3. Data and risk helpers inherited from the 07/08 labs ######################

lookup_series_field = function(series_id, field) {
  idx = match(series_id, SERIES_LIBRARY$series_id)
  if (is.na(idx)) {
    if (identical(field, "title")) {
      return(series_id)
    }
    return("")
  }
  SERIES_LIBRARY[[field]][idx]
}

format_num = function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("NA")
  }
  format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE, scientific = FALSE)
}

format_pct = function(x, digits = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("NA")
  }
  paste0(format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE, scientific = FALSE), "%")
}

normalize_series_ids = function(series_ids) {
  if (is.null(series_ids) || length(series_ids) == 0) {
    return(DEFAULT_SERIES)
  }

  if (is.list(series_ids)) {
    series_ids = unlist(series_ids, use.names = FALSE)
  }

  series_ids = as.character(series_ids)
  if (length(series_ids) == 1 && grepl(",", series_ids, fixed = TRUE)) {
    series_ids = unlist(strsplit(series_ids, ",", fixed = TRUE))
  }

  series_ids = toupper(trimws(series_ids))
  series_ids = series_ids[nzchar(series_ids)]

  if (length(series_ids) == 0) {
    DEFAULT_SERIES
  } else {
    unique(series_ids)
  }
}

normalize_flag = function(x, default = TRUE) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  if (is.logical(x)) {
    return(isTRUE(x[[1]]))
  }
  tolower(as.character(x[[1]])) %in% c("true", "1", "yes", "y")
}

load_fred_key = function() {
  env_candidates = c(
    file.path(SCRIPT_DIR, ".env"),
    file.path(REPO_ROOT, ".env"),
    file.path(REPO_ROOT, "01_query_api", ".env"),
    file.path(REPO_ROOT, "02_productivity", "shiny_app", ".env"),
    file.path(REPO_ROOT, "07_rag", ".env")
  )

  for (candidate in env_candidates) {
    if (file.exists(candidate)) {
      readRenviron(candidate)
      key = Sys.getenv("FRED_API_KEY")
      if (nzchar(key)) {
        return(key)
      }
    }
  }

  Sys.getenv("FRED_API_KEY")
}

fetch_fred_observations = function(series_id, start_date, end_date, limit, api_key) {
  resp = request("https://api.stlouisfed.org/fred/series/observations") |>
    req_url_query(
      series_id = series_id,
      observation_start = start_date,
      observation_end = end_date,
      sort_order = "desc",
      limit = limit,
      file_type = "json",
      api_key = api_key
    ) |>
    req_timeout(25) |>
    req_perform()

  obj = resp_body_json(resp)
  if (is.null(obj$observations) || length(obj$observations) == 0) {
    stop(paste("No observations returned for", series_id))
  }

  df = data.frame(
    date = as.Date(vapply(obj$observations, function(x) x$date, character(1))),
    value_raw = vapply(obj$observations, function(x) x$value, character(1)),
    stringsAsFactors = FALSE
  )

  df$value = suppressWarnings(as.numeric(df$value_raw))
  df = df[!is.na(df$value), c("date", "value")]
  df = df[order(df$date), , drop = FALSE]

  if (nrow(df) == 0) {
    stop(paste("Returned observations are non-numeric for", series_id))
  }

  df
}

summarize_series = function(series_id, df) {
  latest_row = df[nrow(df), , drop = FALSE]
  previous_row = if (nrow(df) >= 2) df[nrow(df) - 1, , drop = FALSE] else latest_row

  latest_value = latest_row$value[[1]]
  previous_value = previous_row$value[[1]]
  absolute_change = latest_value - previous_value
  percent_change = if (!is.na(previous_value) && previous_value != 0) {
    100 * (latest_value / previous_value - 1)
  } else {
    NA_real_
  }

  direction = if (is.na(absolute_change) || abs(absolute_change) < 1e-12) {
    "stable"
  } else if (absolute_change > 0) {
    "up"
  } else {
    "down"
  }

  data.frame(
    series_id = series_id,
    title = lookup_series_field(series_id, "title"),
    latest_date = as.character(latest_row$date[[1]]),
    latest_value = round(latest_value, 3),
    previous_date = as.character(previous_row$date[[1]]),
    previous_value = round(previous_value, 3),
    absolute_change = round(absolute_change, 3),
    percent_change = round(percent_change, 2),
    direction = direction,
    absolute_formula = paste(
      format_num(latest_value),
      "-",
      format_num(previous_value),
      "=",
      format_num(absolute_change)
    ),
    percent_formula = if (is.na(percent_change)) {
      "Percent change undefined because previous value is zero or missing."
    } else {
      paste(
        "100 * (",
        format_num(latest_value),
        "/",
        format_num(previous_value),
        "- 1) =",
        format_pct(percent_change),
        sep = " "
      )
    },
    context_note = lookup_series_field(series_id, "context_note"),
    observation_count = nrow(df),
    stringsAsFactors = FALSE
  )
}

compute_yoy_metric = function(df, source_series, metric_name, label) {
  if (nrow(df) < 13) {
    return(NULL)
  }

  latest_row = df[nrow(df), , drop = FALSE]
  ref_idx = which(df$date <= latest_row$date[[1]] - 364)
  if (length(ref_idx) == 0) {
    return(NULL)
  }

  ref_row = df[max(ref_idx), , drop = FALSE]
  yoy = 100 * (latest_row$value[[1]] / ref_row$value[[1]] - 1)

  data.frame(
    metric = metric_name,
    label = label,
    latest_date = as.character(latest_row$date[[1]]),
    value = round(yoy, 3),
    formula = paste(
      "100 * (",
      format_num(latest_row$value[[1]]),
      "/",
      format_num(ref_row$value[[1]]),
      "- 1) =",
      format_pct(yoy),
      sep = " "
    ),
    meaning = "Year-over-year inflation proxy from the price index.",
    source_series = source_series,
    stringsAsFactors = FALSE
  )
}

compute_monthly_annualized_inflation = function(df, source_series, metric_name, label) {
  if (nrow(df) < 2) {
    return(NULL)
  }

  latest_row = df[nrow(df), , drop = FALSE]
  previous_row = df[nrow(df) - 1, , drop = FALSE]
  annualized_inflation = 1200 * log(latest_row$value[[1]] / previous_row$value[[1]])

  data.frame(
    metric = metric_name,
    label = label,
    latest_date = as.character(latest_row$date[[1]]),
    value = round(annualized_inflation, 3),
    formula = paste(
      "1200 * ln(",
      format_num(latest_row$value[[1]]),
      "/",
      format_num(previous_row$value[[1]]),
      ") =",
      format_pct(annualized_inflation),
      sep = " "
    ),
    meaning = "Short-run annualized inflation momentum from the latest monthly price change.",
    source_series = source_series,
    stringsAsFactors = FALSE
  )
}

compute_derived_metrics = function(series_frames, summary_df) {
  rows = list()

  if ("CPIAUCSL" %in% names(series_frames)) {
    rows[[length(rows) + 1]] = compute_yoy_metric(
      series_frames[["CPIAUCSL"]],
      source_series = "CPIAUCSL",
      metric_name = "cpi_yoy_inflation",
      label = "CPI year-over-year inflation"
    )
    rows[[length(rows) + 1]] = compute_monthly_annualized_inflation(
      series_frames[["CPIAUCSL"]],
      source_series = "CPIAUCSL",
      metric_name = "cpi_monthly_annualized_inflation",
      label = "CPI monthly annualized inflation"
    )
  }

  if ("PCEPI" %in% names(series_frames)) {
    rows[[length(rows) + 1]] = compute_yoy_metric(
      series_frames[["PCEPI"]],
      source_series = "PCEPI",
      metric_name = "pce_yoy_inflation",
      label = "PCE year-over-year inflation"
    )
    rows[[length(rows) + 1]] = compute_monthly_annualized_inflation(
      series_frames[["PCEPI"]],
      source_series = "PCEPI",
      metric_name = "pce_monthly_annualized_inflation",
      label = "PCE monthly annualized inflation"
    )
  }

  rows = Filter(Negate(is.null), rows)
  derived_df = if (length(rows) > 0) do.call(rbind, rows) else NULL

  inflation_row = NULL
  if (!is.null(derived_df) && any(derived_df$metric == "cpi_yoy_inflation")) {
    inflation_row = derived_df[derived_df$metric == "cpi_yoy_inflation", , drop = FALSE][1, ]
  } else if (!is.null(derived_df) && any(derived_df$metric == "pce_yoy_inflation")) {
    inflation_row = derived_df[derived_df$metric == "pce_yoy_inflation", , drop = FALSE][1, ]
  }

  if ("FEDFUNDS" %in% summary_df$series_id && !is.null(inflation_row)) {
    fedfunds = summary_df[summary_df$series_id == "FEDFUNDS", , drop = FALSE][1, ]
    real_policy_rate = fedfunds$latest_value - inflation_row$value
    new_row = data.frame(
      metric = "real_policy_rate",
      label = "Ex post real policy rate",
      latest_date = fedfunds$latest_date,
      value = round(real_policy_rate, 3),
      formula = paste(
        format_num(fedfunds$latest_value),
        "-",
        format_num(inflation_row$value),
        "=",
        format_num(real_policy_rate)
      ),
      meaning = "Positive values suggest more restrictive inflation-adjusted policy conditions.",
      source_series = paste("FEDFUNDS +", inflation_row$source_series),
      stringsAsFactors = FALSE
    )
    derived_df = if (is.null(derived_df)) new_row else rbind(derived_df, new_row)
  }

  if ("DGS10" %in% summary_df$series_id && !is.null(inflation_row)) {
    dgs10 = summary_df[summary_df$series_id == "DGS10", , drop = FALSE][1, ]
    real_10y = dgs10$latest_value - inflation_row$value
    new_row = data.frame(
      metric = "real_10y_yield",
      label = "Ex post real 10-year Treasury yield",
      latest_date = dgs10$latest_date,
      value = round(real_10y, 3),
      formula = paste(
        format_num(dgs10$latest_value),
        "-",
        format_num(inflation_row$value),
        "=",
        format_num(real_10y)
      ),
      meaning = "Higher real long yields imply tighter long-term financing conditions.",
      source_series = paste("DGS10 +", inflation_row$source_series),
      stringsAsFactors = FALSE
    )
    derived_df = if (is.null(derived_df)) new_row else rbind(derived_df, new_row)
  }

  if (all(c("DGS10", "DGS2") %in% summary_df$series_id)) {
    dgs10 = summary_df[summary_df$series_id == "DGS10", , drop = FALSE][1, ]
    dgs2 = summary_df[summary_df$series_id == "DGS2", , drop = FALSE][1, ]
    term_spread = dgs10$latest_value - dgs2$latest_value
    new_row = data.frame(
      metric = "term_spread_10y_2y",
      label = "10-year minus 2-year Treasury spread",
      latest_date = dgs10$latest_date,
      value = round(term_spread, 3),
      formula = paste(
        format_num(dgs10$latest_value),
        "-",
        format_num(dgs2$latest_value),
        "=",
        format_num(term_spread)
      ),
      meaning = "Positive values imply a normal upward-sloping yield curve; negative values imply inversion.",
      source_series = "DGS10 + DGS2",
      stringsAsFactors = FALSE
    )
    derived_df = if (is.null(derived_df)) new_row else rbind(derived_df, new_row)
  }

  if (is.null(derived_df)) {
    data.frame(
      metric = character(0),
      label = character(0),
      latest_date = character(0),
      value = numeric(0),
      formula = character(0),
      meaning = character(0),
      source_series = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    derived_df
  }
}

compute_market_risk_appendix = function(series_frames) {
  unavailable = list(
    available = FALSE,
    note = "Market risk appendix unavailable because the auxiliary SP500 or VIXCLS data could not be prepared.",
    formula_note = "Formula trace unavailable because market-risk inputs could not be prepared.",
    lookback_observations = 0L,
    sample_start = NA_character_,
    sample_end = NA_character_,
    metrics = data.frame(
      metric = character(0),
      label = character(0),
      value = character(0),
      formula = character(0),
      meaning = character(0),
      stringsAsFactors = FALSE
    ),
    worst_losses = data.frame(
      Date = character(0),
      `SP500 Return` = character(0),
      `Loss Magnitude` = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )

  if (!("SP500" %in% names(series_frames))) {
    unavailable$note = "Market risk appendix unavailable because SP500 observations were not available."
    return(unavailable)
  }

  sp500_df = series_frames[["SP500"]]
  if (nrow(sp500_df) < 21) {
    unavailable$note = "Market risk appendix unavailable because SP500 returned too few daily observations for a meaningful historical VaR sample."
    return(unavailable)
  }

  return_dates = as.Date(sp500_df$date[-1])
  daily_returns = 100 * (sp500_df$value[-1] / sp500_df$value[-nrow(sp500_df)] - 1)
  valid_idx = which(!is.na(daily_returns) & is.finite(daily_returns))

  if (length(valid_idx) < 20) {
    unavailable$note = "Market risk appendix unavailable because SP500 daily returns could not be computed cleanly."
    return(unavailable)
  }

  return_dates = return_dates[valid_idx]
  daily_returns = daily_returns[valid_idx]
  var_95 = max(0, -as.numeric(stats::quantile(daily_returns, probs = 0.05, na.rm = TRUE, names = FALSE)))
  var_99 = max(0, -as.numeric(stats::quantile(daily_returns, probs = 0.01, na.rm = TRUE, names = FALSE)))

  loss_order = order(daily_returns)
  worst_n = min(5, length(loss_order))
  worst_idx = loss_order[seq_len(worst_n)]
  worst_losses = data.frame(
    Date = as.character(return_dates[worst_idx]),
    `SP500 Return` = vapply(daily_returns[worst_idx], format_pct, character(1)),
    `Loss Magnitude` = vapply(-daily_returns[worst_idx], function(x) format_pct(max(0, x)), character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  metrics = list(
    data.frame(
      metric = "sp500_hist_var_95",
      label = "SP500 1-day historical VaR (95%)",
      value = format_pct(var_95),
      formula = "VaR_95 = max(0, -quantile(r_t, 5%))",
      meaning = "Approximate one-day S&P 500 loss threshold from the recent historical return sample.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      metric = "sp500_hist_var_99",
      label = "SP500 1-day historical VaR (99%)",
      value = format_pct(var_99),
      formula = "VaR_99 = max(0, -quantile(r_t, 1%))",
      meaning = "More extreme tail-loss threshold from the same recent S&P 500 return sample.",
      stringsAsFactors = FALSE
    )
  )

  if ("VIXCLS" %in% names(series_frames) && nrow(series_frames[["VIXCLS"]]) > 0) {
    vix_df = series_frames[["VIXCLS"]]
    latest_vix = vix_df$value[[nrow(vix_df)]]
    regime = if (is.na(latest_vix)) {
      "unavailable"
    } else if (latest_vix < 15) {
      "low"
    } else if (latest_vix < 25) {
      "moderate"
    } else {
      "high"
    }

    metrics[[length(metrics) + 1]] = data.frame(
      metric = "latest_vix",
      label = "Latest VIX close",
      value = format_num(latest_vix),
      formula = paste0("Latest VIXCLS close on ", as.character(vix_df$date[[nrow(vix_df)]])),
      meaning = "Options-implied near-term equity volatility from the CBOE VIX index.",
      stringsAsFactors = FALSE
    )
    metrics[[length(metrics) + 1]] = data.frame(
      metric = "vix_regime",
      label = "Volatility regime",
      value = regime,
      formula = "Heuristic: low < 15, moderate 15-25, high >= 25",
      meaning = "Simple interpretation layer for the latest VIX level; this is a market proxy, not portfolio VaR.",
      stringsAsFactors = FALSE
    )
  }

  list(
    available = TRUE,
    note = paste(
      "This appendix uses recent SP500 daily returns as an index-based market risk proxy, not a portfolio VaR.",
      "The sample window spans",
      as.character(return_dates[[1]]),
      "to",
      as.character(return_dates[[length(return_dates)]]),
      "with",
      length(daily_returns),
      "daily returns."
    ),
    formula_note = "Formula trace: VaR_95 = max(0, -quantile(r_t, 5%)); VaR_99 = max(0, -quantile(r_t, 1%)); VIX regime heuristic = low < 15, moderate 15-25, high >= 25.",
    lookback_observations = length(daily_returns),
    sample_start = as.character(return_dates[[1]]),
    sample_end = as.character(return_dates[[length(return_dates)]]),
    metrics = do.call(rbind, metrics),
    worst_losses = worst_losses
  )
}

build_demo_evidence_packet = function(question, series_ids, start_date, end_date, limit, top_k, knowledge_result, failure_reason) {
  failure_parts = trimws(unlist(strsplit(failure_reason, "|", fixed = TRUE)))
  failed_series = unique(trimws(sub("\\s*-.*$", "", failure_parts)))
  failed_series = failed_series[nzchar(failed_series)]
  if (length(failed_series) > 0) {
    compact_failure_reason = paste(
      "Network or API access failed for",
      paste(failed_series, collapse = ", "),
      "in this environment."
    )
  } else {
    compact_failure_reason = "Network or API access failed in this environment."
  }

  demo_rows = data.frame(
    series_id = c("FEDFUNDS", "UNRATE", "CPIAUCSL", "DGS10"),
    title = c(
      "Federal Funds Rate",
      "Unemployment Rate",
      "Consumer Price Index for All Urban Consumers",
      "10-Year Treasury Constant Maturity Rate"
    ),
    latest_date = c("2026-02-01", "2026-02-01", "2026-02-01", "2026-02-01"),
    latest_value = c(5.000, 4.100, 320.100, 4.050),
    previous_date = c("2026-01-01", "2026-01-01", "2026-01-01", "2026-01-01"),
    previous_value = c(5.250, 3.900, 319.000, 4.180),
    absolute_change = c(-0.250, 0.200, 1.100, -0.130),
    percent_change = c(-4.76, 5.13, 0.34, -3.11),
    direction = c("down", "up", "up", "down"),
    absolute_formula = c(
      "5.000 - 5.250 = -0.250",
      "4.100 - 3.900 = 0.200",
      "320.100 - 319.000 = 1.100",
      "4.050 - 4.180 = -0.130"
    ),
    percent_formula = c(
      "100 * ( 5.000 / 5.250 - 1) = -4.76%",
      "100 * ( 4.100 / 3.900 - 1) = 5.13%",
      "100 * ( 320.100 / 319.000 - 1) = 0.34%",
      "100 * ( 4.050 / 4.180 - 1) = -3.11%"
    ),
    context_note = c(
      lookup_series_field("FEDFUNDS", "context_note"),
      lookup_series_field("UNRATE", "context_note"),
      lookup_series_field("CPIAUCSL", "context_note"),
      lookup_series_field("DGS10", "context_note")
    ),
    observation_count = c(240, 240, 240, 240),
    stringsAsFactors = FALSE
  )

  demo_rows = demo_rows[demo_rows$series_id %in% series_ids, , drop = FALSE]

  derived_metrics = data.frame(
    metric = c("cpi_yoy_inflation", "cpi_monthly_annualized_inflation", "real_policy_rate", "real_10y_yield", "term_spread_10y_2y"),
    label = c(
      "CPI year-over-year inflation",
      "CPI monthly annualized inflation",
      "Ex post real policy rate",
      "Ex post real 10-year Treasury yield",
      "10-year minus 2-year Treasury spread"
    ),
    latest_date = c("2026-02-01", "2026-02-01", "2026-02-01", "2026-02-01", "2026-02-01"),
    value = c(2.820, 4.130, 2.180, 1.230, -0.150),
    formula = c(
      "100 * ( 320.100 / 311.321 - 1) = 2.82%",
      "1200 * ln( 320.100 / 319.000 ) = 4.13%",
      "5.000 - 2.820 = 2.180",
      "4.050 - 2.820 = 1.230",
      "4.050 - 4.200 = -0.150"
    ),
    meaning = c(
      "Year-over-year inflation proxy from the price index.",
      "Short-run annualized inflation momentum from the latest monthly price change.",
      "Positive values suggest more restrictive inflation-adjusted policy conditions.",
      "Higher real long yields imply tighter long-term financing conditions.",
      "Positive values imply a normal upward-sloping yield curve; negative values imply inversion."
    ),
    source_series = c("CPIAUCSL", "CPIAUCSL", "FEDFUNDS + CPIAUCSL", "DGS10 + CPIAUCSL", "DGS10 + DGS2"),
    stringsAsFactors = FALSE
  )

  keep_metric = c(
    "cpi_yoy_inflation" = "CPIAUCSL" %in% series_ids,
    "cpi_monthly_annualized_inflation" = "CPIAUCSL" %in% series_ids,
    "real_policy_rate" = all(c("FEDFUNDS", "CPIAUCSL") %in% series_ids),
    "real_10y_yield" = all(c("DGS10", "CPIAUCSL") %in% series_ids),
    "term_spread_10y_2y" = "DGS10" %in% series_ids
  )
  derived_metrics = derived_metrics[keep_metric[derived_metrics$metric], , drop = FALSE]

  market_risk = list(
    available = TRUE,
    note = paste(
      "This appendix uses labeled demo fallback values for SP500/VIX market-risk proxies rather than live market data.",
      "It is still an index-based risk proxy, not a portfolio VaR."
    ),
    formula_note = "Formula trace: VaR_95 = max(0, -quantile(r_t, 5%)); VaR_99 = max(0, -quantile(r_t, 1%)); VIX regime heuristic = low < 15, moderate 15-25, high >= 25.",
    lookback_observations = 239L,
    sample_start = "2025-03-18",
    sample_end = "2026-02-27",
    metrics = data.frame(
      metric = c("sp500_hist_var_95", "sp500_hist_var_99", "latest_vix", "vix_regime"),
      label = c(
        "SP500 1-day historical VaR (95%)",
        "SP500 1-day historical VaR (99%)",
        "Latest VIX close",
        "Volatility regime"
      ),
      value = c("2.14%", "3.86%", "19.800", "moderate"),
      formula = c(
        "VaR_95 = max(0, -quantile(r_t, 5%))",
        "VaR_99 = max(0, -quantile(r_t, 1%))",
        "Latest VIXCLS close on 2026-02-27",
        "Heuristic: low < 15, moderate 15-25, high >= 25"
      ),
      meaning = c(
        "Approximate one-day S&P 500 loss threshold from the recent historical return sample.",
        "More extreme tail-loss threshold from the same recent S&P 500 return sample.",
        "Options-implied near-term equity volatility from the CBOE VIX index.",
        "Simple interpretation layer for the latest VIX level; this is a market proxy, not portfolio VaR."
      ),
      stringsAsFactors = FALSE
    ),
    worst_losses = data.frame(
      Date = c("2025-04-04", "2025-10-13", "2025-08-01", "2025-12-18", "2025-06-17"),
      `SP500 Return` = c("-4.22%", "-3.91%", "-3.47%", "-3.10%", "-2.88%"),
      `Loss Magnitude` = c("4.22%", "3.91%", "3.47%", "3.10%", "2.88%"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )

  list(
    question = question,
    data_source = "demo_fallback",
    source_note = paste(
      "Live FRED data could not be fetched, so the tool returned a labeled demo packet.",
      "Reason:",
      compact_failure_reason
    ),
    knowledge_base = basename(KNOWLEDGE_BASE),
    date_window = list(start_date = start_date, end_date = end_date, limit = limit),
    series_requested = series_ids,
    auxiliary_series = unique(c(if ("DGS10" %in% series_ids) "DGS2" else character(0), "SP500", "VIXCLS")),
    live_snapshot = demo_rows,
    derived_metrics = derived_metrics,
    market_risk = market_risk,
    knowledge_matches = knowledge_result$results,
    notes = c(
      "This packet is safe for prompt-testing when the API key or network is unavailable.",
      "Any stakeholder-facing interpretation should mention that the values are demo fallback numbers."
    ),
    top_k = top_k
  )
}


# 4. Tool calling layer inherited from the 08 lab ##############################

build_macro_evidence_packet = function(
  question,
  series_ids = DEFAULT_SERIES,
  start_date = DEFAULT_START_DATE,
  end_date = as.character(Sys.Date()),
  limit = DEFAULT_LIMIT,
  top_k = DEFAULT_TOP_K,
  allow_demo_fallback = TRUE
) {
  series_ids = normalize_series_ids(series_ids)
  auxiliary_series = character(0)
  if ("DGS10" %in% series_ids && !("DGS2" %in% series_ids)) {
    auxiliary_series = c(auxiliary_series, "DGS2")
  }
  auxiliary_series = unique(c(auxiliary_series, "SP500", "VIXCLS"))
  fetch_series_ids = unique(c(series_ids, auxiliary_series))
  limit = as.integer(limit[[1]])
  top_k = as.integer(top_k[[1]])
  allow_demo_fallback = normalize_flag(allow_demo_fallback, default = TRUE)

  knowledge_result = tryCatch(
    search_mishkin_context(question, document_path = KNOWLEDGE_BASE, top_k = top_k),
    error = function(e) {
      list(
        query = question,
        document = basename(KNOWLEDGE_BASE),
        num_sections_considered = 0,
        num_sections_returned = 0,
        results = list(
          list(
            title = "Knowledge base unavailable",
            entry_id = "knowledge_base_error",
            retrieval_label = "knowledge_base_error",
            primary_fred_series = paste(fetch_series_ids, collapse = ", "),
            score = 0,
            matched_terms = character(0),
            preview = conditionMessage(e),
            formulas = character(0)
          )
        )
      )
    }
  )

  api_key = load_fred_key()
  if (!nzchar(api_key)) {
    if (allow_demo_fallback) {
      return(build_demo_evidence_packet(
        question = question,
        series_ids = series_ids,
        start_date = start_date,
        end_date = end_date,
        limit = limit,
        top_k = top_k,
        knowledge_result = knowledge_result,
        failure_reason = "FRED_API_KEY not found in any expected .env location."
      ))
    }
    stop("FRED_API_KEY not found in any expected .env location.")
  }

  series_frames = list()
  fetch_errors = character(0)

  for (series_id in fetch_series_ids) {
    result = tryCatch(
      fetch_fred_observations(
        series_id = series_id,
        start_date = start_date,
        end_date = end_date,
        limit = limit,
        api_key = api_key
      ),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      fetch_errors = c(fetch_errors, paste(series_id, "-", conditionMessage(result)))
    } else {
      series_frames[[series_id]] = result
    }
  }

  if (length(series_frames) == 0) {
    if (allow_demo_fallback) {
      return(build_demo_evidence_packet(
        question = question,
        series_ids = series_ids,
        start_date = start_date,
        end_date = end_date,
        limit = limit,
        top_k = top_k,
        knowledge_result = knowledge_result,
        failure_reason = paste(fetch_errors, collapse = " | ")
      ))
    }
    stop(paste(fetch_errors, collapse = " | "))
  }

  summary_rows = lapply(series_ids[series_ids %in% names(series_frames)], function(series_id) {
    summarize_series(series_id, series_frames[[series_id]])
  })
  summary_df = do.call(rbind, summary_rows)
  full_summary_rows = lapply(names(series_frames), function(series_id) {
    summarize_series(series_id, series_frames[[series_id]])
  })
  full_summary_df = do.call(rbind, full_summary_rows)
  derived_df = compute_derived_metrics(series_frames, full_summary_df)
  market_risk = compute_market_risk_appendix(series_frames)

  list(
    question = question,
    data_source = "live_fred",
    source_note = if (length(fetch_errors) == 0) {
      "Live FRED data fetched successfully."
    } else {
      paste(
        "Live FRED data fetched for the available series, but some requested series failed:",
        paste(fetch_errors, collapse = " | ")
      )
    },
    knowledge_base = basename(KNOWLEDGE_BASE),
    date_window = list(start_date = start_date, end_date = end_date, limit = limit),
    series_requested = series_ids,
    auxiliary_series = auxiliary_series,
    live_snapshot = summary_df,
    derived_metrics = derived_df,
    market_risk = market_risk,
    knowledge_matches = knowledge_result$results,
    failed_series = setdiff(series_ids, names(series_frames)),
    notes = c(
      if (length(fetch_errors) == 0) {
        "All requested series were fetched successfully."
      } else {
        paste("Some requested series could not be fetched:", paste(fetch_errors, collapse = " | "))
      },
      "All deterministic calculations were computed in R before being handed to the model."
    ),
    top_k = top_k
  )
}

tool_build_macro_evidence_packet = list(
  type = "function",
  "function" = list(
    name = "build_macro_evidence_packet",
    description = paste(
      "Fetch live U.S. macro data from FRED, compute deterministic change metrics and formula traces,",
      "retrieve relevant Mishkin textbook formula context from the local knowledge base,",
      "and add a small SP500/VIX market-risk appendix before returning a structured evidence packet."
    ),
    parameters = list(
      type = "object",
      required = list("question", "series_ids", "start_date", "end_date", "limit", "top_k"),
      properties = list(
        question = list(
          type = "string",
          description = "The user's macroeconomic question or briefing request."
        ),
        series_ids = list(
          type = "array",
          description = "FRED series codes to include. Recommended macro options: FEDFUNDS, EFFR, UNRATE, CPIAUCSL, PCEPI, DGS10, DGS2. Optional market-risk series include SP500 and VIXCLS.",
          items = list(type = "string")
        ),
        start_date = list(
          type = "string",
          description = "Observation start date in YYYY-MM-DD format."
        ),
        end_date = list(
          type = "string",
          description = "Observation end date in YYYY-MM-DD format."
        ),
        limit = list(
          type = "integer",
          description = "Maximum number of observations per series."
        ),
        top_k = list(
          type = "integer",
          description = "Number of Mishkin knowledge-base sections to retrieve."
        ),
        allow_demo_fallback = list(
          type = "boolean",
          description = "Whether to return a clearly labeled demo packet if live FRED data cannot be fetched."
        )
      )
    )
  )
)

is_valid_evidence_packet = function(packet) {
  is.list(packet) &&
    !isTRUE(packet$error) &&
    "data_source" %in% names(packet) &&
    "live_snapshot" %in% names(packet) &&
    "derived_metrics" %in% names(packet) &&
    is.data.frame(packet$live_snapshot) &&
    is.data.frame(packet$derived_metrics)
}

matches_requested_configuration = function(packet, series_ids, start_date, end_date) {
  identical(as.character(packet$series_requested), as.character(series_ids)) &&
    identical(as.character(packet$date_window$start_date), as.character(start_date)) &&
    identical(as.character(packet$date_window$end_date), as.character(end_date))
}

resolve_agent1_packet = function(tool_calls, question) {
  if (
    length(tool_calls) > 0 &&
      is_valid_evidence_packet(tool_calls[[1]]$output) &&
      matches_requested_configuration(
        tool_calls[[1]]$output,
        series_ids = DEFAULT_SERIES,
        start_date = DEFAULT_START_DATE,
        end_date = TODAY
      )
  ) {
    return(tool_calls[[1]]$output)
  }

  build_macro_evidence_packet(
    question = question,
    series_ids = DEFAULT_SERIES,
    start_date = DEFAULT_START_DATE,
    end_date = TODAY,
    limit = DEFAULT_LIMIT,
    top_k = DEFAULT_TOP_K,
    allow_demo_fallback = TRUE
  )
}


# 5. Rendering helpers for user-readable terminal output #######################

render_key_data_snapshot = function(packet) {
  snapshot_df = packet$live_snapshot
  display_df = data.frame(
    Series = snapshot_df$series_id,
    `Latest Date` = snapshot_df$latest_date,
    `Latest Value` = vapply(snapshot_df$latest_value, format_num, character(1)),
    `Previous Value` = vapply(snapshot_df$previous_value, format_num, character(1)),
    `Absolute Change` = vapply(snapshot_df$absolute_change, format_num, character(1)),
    `Percent Change` = vapply(snapshot_df$percent_change, format_pct, character(1)),
    Direction = snapshot_df$direction,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  df_as_text(display_df)
}

render_derived_macro_signals = function(packet) {
  derived_df = packet$derived_metrics
  if (nrow(derived_df) == 0) {
    return("| Metric | Value | Formula | Meaning |\n| --- | --- | --- | --- |\n| No derived macro signals | NA | NA | The available evidence did not support additional calculations. |")
  }

  display_df = data.frame(
    Metric = derived_df$label,
    Value = vapply(derived_df$value, format_num, character(1)),
    Formula = derived_df$formula,
    Meaning = derived_df$meaning,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  df_as_text(display_df)
}

render_risk_snapshot = function(packet) {
  market_risk = packet$market_risk
  if (is.null(market_risk) || !isTRUE(market_risk$available) || nrow(market_risk$metrics) == 0) {
    return(paste(
      "- Risk note:", if (!is.null(market_risk)) market_risk$note else "Market risk appendix unavailable.",
      "\n| Metric | Value | Interpretation |\n| --- | --- | --- |\n| Market risk appendix unavailable | NA | The macro workflow still ran successfully. |",
      sep = ""
    ))
  }

  metric_table = df_as_text(data.frame(
    Metric = market_risk$metrics$label,
    Value = market_risk$metrics$value,
    Interpretation = market_risk$metrics$meaning,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))

  worst_loss_table = if (nrow(market_risk$worst_losses) > 0) {
    df_as_text(market_risk$worst_losses)
  } else {
    "| Date | SP500 Return | Loss Magnitude |\n| --- | --- | --- |\n| No daily losses available | NA | NA |"
  }

  paste(
    paste0("- Risk note: ", market_risk$note),
    paste0("- ", market_risk$formula_note),
    metric_table,
    "### Worst 5 SP500 Daily Losses",
    worst_loss_table,
    sep = "\n"
  )
}

render_retrieval_snapshot = function(packet) {
  knowledge_matches = packet$knowledge_matches
  if (length(knowledge_matches) == 0) {
    return("| Topic | Matched Terms | Related Series | Retrieved Formula |\n| --- | --- | --- | --- |\n| No retrieval match | NA | NA | NA |")
  }

  display_df = data.frame(
    Topic = vapply(knowledge_matches, function(item) item$entry_id, character(1)),
    `Matched Terms` = vapply(knowledge_matches, function(item) paste(item$matched_terms, collapse = ", "), character(1)),
    `Related Series` = vapply(knowledge_matches, function(item) item$primary_fred_series, character(1)),
    `Retrieved Formula` = vapply(
      knowledge_matches,
      function(item) {
        if (length(item$formulas) == 0) "No explicit formula extracted" else item$formulas[[1]]
      },
      character(1)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  df_as_text(display_df)
}

summarize_stance_note = function(packet) {
  derived_df = packet$derived_metrics
  if (!is.null(derived_df) && nrow(derived_df) > 0 && any(derived_df$metric == "real_policy_rate")) {
    row = derived_df[derived_df$metric == "real_policy_rate", , drop = FALSE][1, ]
    stance = if (row$value > 1) {
      "looks restrictive on an inflation-adjusted basis"
    } else if (row$value < 0) {
      "looks accommodative on an inflation-adjusted basis"
    } else {
      "looks close to neutral on an inflation-adjusted basis"
    }
    return(paste0("Real policy rate = ", format_num(row$value), ", so policy ", stance, "."))
  }

  "Use the sign and size of the real-rate metrics before making a policy-stance claim."
}

render_evidence_packet = function(packet) {
  data_source_lines = c(
    paste0("- Source: `", packet$data_source, "`."),
    paste0("- Note: ", packet$source_note),
    paste0(
      "- Date window: ",
      packet$date_window$start_date,
      " to ",
      packet$date_window$end_date,
      " with limit ",
      packet$date_window$limit,
      " rows per series."
    ),
    paste0("- Requested series: ", paste(packet$series_requested, collapse = ", "), ".")
  )

  if ("auxiliary_series" %in% names(packet) && length(packet$auxiliary_series) > 0) {
    data_source_lines = c(
      data_source_lines,
      paste0("- Auxiliary series used for derived signals and risk context: ", paste(packet$auxiliary_series, collapse = ", "), ".")
    )
  }

  note_lines = c(
    paste0("- ", summarize_stance_note(packet)),
    "- The key data snapshot shows headline levels and recent moves.",
    "- The derived macro signals section is the main quantitative interpretation layer.",
    "- The risk snapshot is an index-based market proxy, not portfolio risk.",
    paste0("- Knowledge retrieval came from `", packet$knowledge_base, "`.")
  )

  paste(
    "# Evidence Packet",
    "## Data Source",
    paste(data_source_lines, collapse = "\n"),
    "## Key Data Snapshot",
    render_key_data_snapshot(packet),
    "## Derived Macro Signals",
    render_derived_macro_signals(packet),
    "## Risk Snapshot",
    render_risk_snapshot(packet),
    "## Retrieved Context",
    render_retrieval_snapshot(packet),
    "## Interpretation Notes",
    paste(note_lines, collapse = "\n"),
    sep = "\n"
  )
}

build_market_risk_brief_section = function(packet) {
  if (!("market_risk" %in% names(packet))) {
    return("")
  }

  market_risk = packet$market_risk
  if (is.null(market_risk) || !isTRUE(market_risk$available) || nrow(market_risk$metrics) == 0) {
    return("")
  }

  metric_value = function(metric_id) {
    rows = market_risk$metrics[market_risk$metrics$metric == metric_id, , drop = FALSE]
    if (nrow(rows) == 0) "NA" else as.character(rows$value[[1]])
  }

  var95 = metric_value("sp500_hist_var_95")
  var99 = metric_value("sp500_hist_var_99")
  latest_vix = metric_value("latest_vix")
  regime = metric_value("vix_regime")

  bullets = c(
    paste0(
      "- SP500 1-day historical VaR is ",
      var95,
      " at 95% and ",
      var99,
      " at 99%, so the appendix adds an index-based downside-risk proxy rather than portfolio VaR."
    )
  )

  if (!identical(latest_vix, "NA") && !identical(regime, "NA")) {
    bullets = c(
      bullets,
      paste0(
        "- Latest VIX is ",
        latest_vix,
        ", which suggests a ",
        regime,
        " implied-volatility backdrop for the broader market."
      )
    )
  } else {
    bullets = c(
      bullets,
      "- The worst-loss table complements the VaR estimate by showing the largest recent SP500 down days directly."
    )
  }

  paste(c("## Market Risk Appendix", bullets), collapse = "\n")
}

insert_section_before_heading = function(text, section_text, heading = "## Limits") {
  if (!nzchar(section_text)) {
    return(text)
  }

  lines = unlist(strsplit(text, "\n", fixed = TRUE))
  target_idx = which(trimws(lines) == heading)
  if (length(target_idx) == 0) {
    return(paste(text, section_text, sep = "\n"))
  }

  idx = target_idx[[1]]
  before = if (idx > 1) paste(lines[seq_len(idx - 1)], collapse = "\n") else ""
  after = paste(lines[idx:length(lines)], collapse = "\n")

  if (!nzchar(before)) {
    paste(section_text, after, sep = "\n")
  } else {
    paste(before, section_text, after, sep = "\n")
  }
}

replace_section_block = function(text, heading, section_text) {
  lines = unlist(strsplit(text, "\n", fixed = TRUE))
  start_idx = which(trimws(lines) == heading)

  if (length(start_idx) == 0) {
    return(paste(text, section_text, sep = "\n"))
  }

  start_idx = start_idx[[1]]
  later_headers = which(grepl("^##\\s+", trimws(lines)) & seq_along(lines) > start_idx)
  end_idx = if (length(later_headers) > 0) later_headers[1] - 1 else length(lines)

  before = if (start_idx > 1) paste(lines[seq_len(start_idx - 1)], collapse = "\n") else ""
  after = if (end_idx < length(lines)) paste(lines[(end_idx + 1):length(lines)], collapse = "\n") else ""

  pieces = Filter(nzchar, c(before, section_text, after))
  paste(pieces, collapse = "\n")
}

extract_section_lines = function(text, heading) {
  lines = unlist(strsplit(text, "\n", fixed = TRUE))
  start_idx = which(trimws(lines) == heading)
  if (length(start_idx) == 0) {
    return(character(0))
  }

  later_headers = which(grepl("^##\\s+", trimws(lines)) & seq_along(lines) > start_idx[1])
  end_idx = if (length(later_headers) > 0) later_headers[1] - 1 else length(lines)

  if (end_idx <= start_idx[1]) {
    return(character(0))
  }

  lines[(start_idx[1] + 1):end_idx]
}

count_bullets = function(lines) {
  sum(grepl("^\\s*-\\s+", lines))
}

build_limits_section = function(packet) {
  paste(
    "## Limits",
    paste0("- This brief relies on `", packet$data_source, "` rather than guaranteed live FRED results."),
    paste0("- The evidence window is ", packet$date_window$start_date, " to ", packet$date_window$end_date, "."),
    sep = "\n"
  )
}

clean_macro_brief_output = function(text) {
  lines = unlist(strsplit(text, "\n", fixed = TRUE))
  lines = lines[!grepl("^\\s*Tools called:", lines)]
  trimws(paste(lines, collapse = "\n"))
}

truncate_after_limits = function(text) {
  lines = unlist(strsplit(text, "\n", fixed = TRUE))
  start_idx = which(trimws(lines) == "## Limits")
  if (length(start_idx) == 0) {
    return(trimws(text))
  }

  start_idx = start_idx[[1]]
  bullet_idx = which(grepl("^\\s*-\\s+", lines) & seq_along(lines) > start_idx)
  bullet_idx = bullet_idx[bullet_idx > start_idx]
  if (length(bullet_idx) >= 2) {
    return(trimws(paste(lines[seq_len(bullet_idx[[2]])], collapse = "\n")))
  }

  trimws(text)
}

is_valid_macro_brief = function(text) {
  required_headers = c("# Macro Brief", "## Key Signals", "## Why It Matters", "## Bottom Line", "## Limits")
  if (!all(vapply(required_headers, function(h) grepl(paste0("(?m)^", gsub("([#])", "\\\\\\1", h), "$"), text, perl = TRUE), logical(1)))) {
    return(FALSE)
  }

  key_lines = extract_section_lines(text, "## Key Signals")
  why_lines = extract_section_lines(text, "## Why It Matters")
  bottom_lines = extract_section_lines(text, "## Bottom Line")
  limits_lines = extract_section_lines(text, "## Limits")

  count_bullets(key_lines) >= 4 &&
    count_bullets(why_lines) >= 3 &&
    nzchar(trimws(paste(bottom_lines, collapse = " "))) &&
    count_bullets(limits_lines) >= 2
}

ensure_macro_brief_sections = function(brief_text, packet) {
  output = clean_macro_brief_output(brief_text)
  market_section = build_market_risk_brief_section(packet)
  limits_section = build_limits_section(packet)

  if (grepl("(?m)^## Limits$", output, perl = TRUE)) {
    output = replace_section_block(output, "## Limits", limits_section)
  } else {
    output = paste(output, limits_section, sep = "\n")
  }

  if (nzchar(market_section)) {
    if (grepl("(?m)^## Market Risk Appendix$", output, perl = TRUE)) {
      output = replace_section_block(output, "## Market Risk Appendix", market_section)
    } else {
      output = insert_section_before_heading(output, market_section, heading = "## Limits")
    }
  }

  truncate_after_limits(output)
}

build_macro_brief_fallback = function(packet) {
  snapshot_df = packet$live_snapshot
  derived_df = packet$derived_metrics

  key_lines = apply(snapshot_df, 1, function(row) {
    paste0(
      "- `", row[["series_id"]], "` is ",
      format_num(as.numeric(row[["latest_value"]])),
      " on ",
      row[["latest_date"]],
      ", versus ",
      format_num(as.numeric(row[["previous_value"]])),
      " previously; change = ",
      format_num(as.numeric(row[["absolute_change"]])),
      " (",
      format_pct(as.numeric(row[["percent_change"]])),
      ")."
    )
  })

  why_lines = character(0)
  if (nrow(derived_df) > 0 && any(derived_df$metric == "cpi_yoy_inflation")) {
    row = derived_df[derived_df$metric == "cpi_yoy_inflation", , drop = FALSE][1, ]
    why_lines = c(why_lines, paste0("- CPI year-over-year inflation is ", format_num(row$value), ", so price pressure is still relevant."))
  }
  if (nrow(derived_df) > 0 && any(derived_df$metric == "real_policy_rate")) {
    row = derived_df[derived_df$metric == "real_policy_rate", , drop = FALSE][1, ]
    why_lines = c(why_lines, paste0("- Real policy rate is ", format_num(row$value), ", which points to restrictive inflation-adjusted policy conditions."))
  }
  if (any(snapshot_df$series_id == "UNRATE")) {
    row = snapshot_df[snapshot_df$series_id == "UNRATE", , drop = FALSE][1, ]
    labor_line = if (row$absolute_change > 0) {
      "- Unemployment moved up, which suggests some softening in the labor market."
    } else if (row$absolute_change < 0) {
      "- Unemployment moved down, which suggests a firmer labor market."
    } else {
      "- Unemployment was stable, so labor conditions do not materially change the interpretation."
    }
    why_lines = c(why_lines, labor_line)
  }
  if (length(why_lines) < 3 && nrow(derived_df) > 0 && any(derived_df$metric == "real_10y_yield")) {
    row = derived_df[derived_df$metric == "real_10y_yield", , drop = FALSE][1, ]
    why_lines = c(why_lines, paste0("- Real 10-year yield is ", format_num(row$value), ", reinforcing the signal from long-term financing conditions."))
  }
  why_lines = why_lines[seq_len(min(3, length(why_lines)))]

  bottom_line = if (packet$data_source == "live_fred") {
    "The combined evidence points to a relatively restrictive macro stance, especially when viewed through the positive real policy rate. Inflation and labor-market signals should still be monitored together before making stronger claims."
  } else {
    "The combined evidence points to a relatively restrictive macro stance, but this run used the demo fallback packet because live FRED access was unavailable. The logic of the workflow is still valid, while the numeric values should be treated as placeholders."
  }

  market_section = build_market_risk_brief_section(packet)

  output = paste(
    "# Macro Brief",
    "## Key Signals",
    paste(key_lines, collapse = "\n"),
    "## Why It Matters",
    paste(why_lines, collapse = "\n"),
    "## Bottom Line",
    bottom_line,
    "## Limits",
    paste0("- This brief relies on `", packet$data_source, "` rather than guaranteed live FRED results."),
    paste0("- The evidence window is ", packet$date_window$start_date, " to ", packet$date_window$end_date, "."),
    sep = "\n"
  )

  if (nzchar(market_section)) {
    output = insert_section_before_heading(output, market_section, heading = "## Limits")
  }

  output
}


# 6. Agent prompts #############################################################

agent1_role = paste(
  "You are Agent 1, the macro evidence agent for Homework 2.",
  "This app compiles work from previous labs on multi-agent orchestration, RAG, and function calling.",
  "You must call the build_macro_evidence_packet tool exactly once.",
  "Use the same user question in the tool call.",
  "If the user does not specify series, use FEDFUNDS, UNRATE, CPIAUCSL, and DGS10.",
  paste("Use start_date 2021-01-01 and end_date", TODAY, "."),
  "Use limit 240, top_k 2, and allow_demo_fallback true.",
  "Return the tool call only.",
  "Do not answer in prose.",
  sep = "\n"
)

agent2_role = paste(
  "You are Agent 2, the macro briefing agent for Homework 2.",
  "This app compiles work from previous labs on multi-agent orchestration, RAG, and function calling.",
  "You will receive an evidence packet from Agent 1.",
  "Use only that evidence.",
  "Do not repeat raw tables line by line, and do not add forecasts or investment advice.",
  "Return only markdown using this exact structure:",
  "# Macro Brief",
  "## Key Signals",
  "- exactly 4 bullets, with one bullet each for FEDFUNDS, UNRATE, CPIAUCSL, and DGS10 when they are present",
  "## Why It Matters",
  "- exactly 3 bullets",
  "## Bottom Line",
  "Write exactly 2 sentences.",
  "## Market Risk Appendix",
  "- when the evidence packet includes a market-risk appendix, add exactly 2 bullets and state clearly that it is an index-based market proxy rather than portfolio VaR",
  "## Limits",
  "- exactly 2 bullets",
  "Keep the tone concise, clear, and factual.",
  sep = "\n"
)


# 7. Terminal output helpers ###################################################

section_break = function(title) {
  paste0("=== ", title, " ===\n")
}


# 8. Run the Homework 2 app ####################################################

available_models = tryCatch(ollamar::list_models(), error = function(e) NULL)
if (is.null(available_models) || nrow(available_models) == 0) {
  stop("No local Ollama models are installed. Install a model first, then rerun this script.")
}

tool_models = c("smollm2:1.7b", "qwen2.5:latest", "llama3.1:latest")
writer_models = c("gemma3:latest", "qwen2.5:latest", "llama3.1:latest", "smollm2:1.7b")

matching_tool_models = tool_models[tool_models %in% available_models$name]
matching_writer_models = writer_models[writer_models %in% available_models$name]

TOOL_MODEL = if (length(matching_tool_models) > 0) matching_tool_models[[1]] else available_models$name[[1]]
WRITER_MODEL = if (length(matching_writer_models) > 0) matching_writer_models[[1]] else TOOL_MODEL
MODEL = WRITER_MODEL

ensure_ollama_model(TOOL_MODEL, auto_pull = FALSE)
ensure_ollama_model(WRITER_MODEL, auto_pull = FALSE)

tool_metadata_json = jsonlite::toJSON(tool_build_macro_evidence_packet, auto_unbox = TRUE, pretty = TRUE)

agent1_tool_calls = agent_run(
  role = agent1_role,
  task = USER_QUERY,
  tools = list(tool_build_macro_evidence_packet),
  model = TOOL_MODEL,
  output = "tools"
)

agent1_packet = resolve_agent1_packet(agent1_tool_calls, USER_QUERY)
agent1_output = render_evidence_packet(agent1_packet)

agent2_output_raw = agent_run(
  role = agent2_role,
  task = agent1_output,
  model = WRITER_MODEL,
  output = "text"
)

agent2_output = if (is_valid_macro_brief(agent2_output_raw)) {
  ensure_macro_brief_sections(agent2_output_raw, agent1_packet)
} else {
  build_macro_brief_fallback(agent1_packet)
}

cat(section_break("App"))
cat(APP_NAME, "\n")
cat(APP_PURPOSE, "\n\n")

cat(section_break("Compiled From Previous Labs"))
cat("- 06_agents: two-agent orchestration and prompt design\n")
cat("- 07_rag: Mishkin text retrieval and RAG context\n")
cat("- 08_function_calling: tool calling with live FRED data and external data sources\n\n")

cat(section_break("Models"))
cat("Tool model:", TOOL_MODEL, "\n")
cat("Writer model:", WRITER_MODEL, "\n\n")

cat(section_break("User Question"))
cat(USER_QUERY, "\n\n")

cat(section_break("Tool Metadata"))
cat(tool_metadata_json, "\n\n")

cat(section_break("Agent 1 Tool Usage"))
cat(tool_calls_as_text(agent1_tool_calls), "\n\n")

cat(section_break("Agent 1 Evidence Packet"))
cat(agent1_output, "\n\n")

cat(section_break("Agent 2 Final Macro Brief"))
cat(agent2_output, "\n")
