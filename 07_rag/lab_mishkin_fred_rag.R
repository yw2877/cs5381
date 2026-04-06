# lab_mishkin_fred_rag.R
# Complete text-file RAG workflow for the Mishkin + FRED knowledge base
# Covers Task 2 search + Task 3 JSON-to-LLM workflow in one submit-ready script.

# install.packages(c("httr2", "jsonlite"), repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))
library(httr2)
library(jsonlite)


# 0. SETUP ###################################

args_all = commandArgs(trailingOnly = FALSE)
file_arg = grep("^--file=", args_all, value = TRUE)
SCRIPT_PATH = if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  normalizePath(".")
}
SCRIPT_DIR = dirname(SCRIPT_PATH)


# 0.1 Configuration ###################################

MODEL = "smollm2:1.7b"
PORT = 11434
OLLAMA_HOST = paste0("http://localhost:", PORT)
CHAT_URL = paste0(OLLAMA_HOST, "/api/chat")
DOCUMENT = file.path(SCRIPT_DIR, "mishkin_fred_explained.txt")
TOP_K = 1
MAX_SECTION_CHARS = 2500

QUERIES = c(
  "What does an inverted yield curve usually mean?",
  "How can I estimate a real policy rate using FRED data?",
  "Does money growth still matter for inflation?"
)

Sys.setenv(OLLAMA_HOST = OLLAMA_HOST)


# 1. SEARCH FUNCTION ###################################

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
    "estimate", "still", "matter", "data"
  )

  clean = tolower(text)
  clean = gsub("[^a-z0-9 ]", " ", clean)
  pieces = unlist(strsplit(clean, "\\s+"))
  pieces = pieces[nchar(pieces) >= 3]
  pieces = pieces[!(pieces %in% stopwords)]
  unique(pieces)
}


make_preview = function(section_text, max_chars = 420) {
  compact = gsub("\\s+", " ", section_text)
  compact = trimws(compact)

  if (nchar(compact) <= max_chars) {
    return(compact)
  }

  paste0(substr(compact, 1, max_chars), "...")
}


trim_text = function(text, max_chars = MAX_SECTION_CHARS) {
  compact = trimws(text)

  if (nchar(compact) <= max_chars) {
    return(compact)
  }

  paste0(substr(compact, 1, max_chars), "\n\n[truncated for prompt length]")
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
      cross_references = extract_field(chunk, "Cross-References:"),
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


search_text = function(query, document_path, top_k = TOP_K) {
  document_text = read_text(document_path)
  sections = split_into_sections(document_text)

  if (length(sections) == 0) {
    stop("No retrievable sections were found in the text file.")
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
      matching_content = section$content
    )
  })

  scores = vapply(scored_results, function(x) x$score, numeric(1))
  order_idx = order(scores, decreasing = TRUE)
  ranked_results = scored_results[order_idx]
  top_results = ranked_results[seq_len(min(top_k, length(ranked_results)))]

  list(
    query = query,
    document = basename(document_path),
    retrieval_unit = "topic_section",
    num_sections_considered = length(sections),
    num_sections_returned = length(top_results),
    results = top_results
  )
}


# 2. LLM WORKFLOW ###################################

check_ollama = function(ollama_host) {
  tryCatch({
    request(paste0(ollama_host, "/api/tags")) |>
      req_perform()
    TRUE
  }, error = function(e) FALSE)
}


list_models = function(ollama_host) {
  resp = request(paste0(ollama_host, "/api/tags")) |>
    req_perform() |>
    resp_body_json()

  vapply(resp$models, function(x) x$name, character(1))
}


build_payload = function(query, retrieval_result) {
  list(
    query = query,
    document = retrieval_result$document,
    retrieval_unit = retrieval_result$retrieval_unit,
    num_sections_returned = retrieval_result$num_sections_returned,
    retrieved_sections = lapply(retrieval_result$results, function(item) {
      list(
        title = item$title,
        entry_id = item$entry_id,
        retrieval_label = item$retrieval_label,
        primary_fred_series = item$primary_fred_series,
        score = item$score,
        matched_terms = item$matched_terms,
        preview = item$preview,
        matching_content = trim_text(item$matching_content)
      )
    })
  )
}


ROLE = paste(
  "You are a macroeconomics teaching assistant using a Mishkin + FRED text knowledge base.",
  "You will receive a JSON object with a user query and retrieved textbook sections.",
  "Use only the retrieved content in that JSON.",
  "Do not invent current FRED values, dates, forecasts, or outside facts.",
  "Explain the concept clearly in plain language.",
  "If the retrieved content gives only an approximate relationship, state that it is approximate.",
  "If live values are not provided, say which FRED series would be needed rather than making them up.",
  "Do not write about stocks, portfolios, or investment strategy unless those exact ideas appear in the retrieved content.",
  "If the retrieved text does not fully answer the question, say that directly in the Limits section.",
  "Return only valid JSON with this exact structure:",
  "{\"answer\": \"string\", \"interpretation\": [\"bullet1\", \"bullet2\"], \"relevant_formulas\": [\"formula1\"], \"relevant_fred_series\": [\"SERIES1\", \"SERIES2\"], \"limits\": \"string\"}",
  "The answer value should be 2 to 3 sentences.",
  "The interpretation array must contain exactly 2 bullet strings.",
  "The relevant_formulas array must contain 1 to 3 formula or equation strings from the retrieved content.",
  "The relevant_fred_series array should contain only FRED series codes.",
  "The limits value must be exactly 1 sentence.",
  "Do not include markdown, explanations, or any text outside the JSON object.",
  sep = "\n"
)


ask_llm = function(role, payload_json, chat_url, model) {
  body = list(
    model = model,
    messages = list(
      list(role = "system", content = role),
      list(role = "user", content = payload_json)
    ),
    format = "json",
    options = list(
      temperature = 0.1,
      num_predict = 380
    ),
    stream = FALSE
  )

  resp = request(chat_url) |>
    req_timeout(180) |>
    req_body_json(body) |>
    req_perform() |>
    resp_body_json()

  resp$message$content
}


render_markdown = function(parsed_json) {
  interpretation_lines = paste0("- ", parsed_json$interpretation, collapse = "\n")
  formula_lines = paste0("- ", parsed_json$relevant_formulas, collapse = "\n")
  fred_series_line = paste(parsed_json$relevant_fred_series, collapse = ", ")

  paste(
    "# Answer",
    parsed_json$answer,
    "## Interpretation",
    interpretation_lines,
    "## Relevant Formulas",
    formula_lines,
    "## Relevant FRED Series",
    fred_series_line,
    "## Limits",
    parsed_json$limits,
    sep = "\n"
  )
}


default_formula_for_entry_id = function(entry_id) {
  formula_map = list(
    topic_1_understanding_interest_rates = "real_policy_rate_t approx FEDFUNDS_t - pi_t",
    topic_2_behaviour_of_interest_rates = "i_t = alpha + beta1*pi_t + beta2*UNRATE_t + beta3*payroll_growth_t + error_t",
    topic_3_risk_and_term_structure = "term_spread_t = DGS10_t - DGS2_t",
    topic_4_central_banks = "policy_gap_t = EFFR_t - target_mid_t",
    topic_5_conduct_of_monetary_policy = "i_Taylor_t = r_star + pi_t + 0.5*(pi_t - pi_star) + 0.5*output_gap_t",
    topic_6_money_and_inflation = "M * V = P * Y",
    topic_7_cross_topic_interpretation_rules = "real_rate approx nominal_rate - inflation"
  )

  if (!is.null(formula_map[[entry_id]])) {
    return(formula_map[[entry_id]])
  }

  "Use the main formula from the top retrieved section."
}


normalize_llm_output = function(parsed_json, retrieval_result) {
  top_entry_id = retrieval_result$results[[1]]$entry_id
  primary_series = retrieval_result$results[[1]]$primary_fred_series
  default_formula = default_formula_for_entry_id(top_entry_id)

  if (is.null(parsed_json$interpretation) || length(parsed_json$interpretation) == 0) {
    parsed_json$interpretation = c(
      "The answer is based only on the top retrieved textbook section.",
      "A live FRED query would be needed for current numerical interpretation."
    )
  }

  if (is.null(parsed_json$relevant_formulas) || length(parsed_json$relevant_formulas) == 0) {
    parsed_json$relevant_formulas = c(default_formula)
  } else {
    parsed_json$relevant_formulas = parsed_json$relevant_formulas[nzchar(trimws(parsed_json$relevant_formulas))]
    if (length(parsed_json$relevant_formulas) == 0 || any(grepl("No specific formula was extracted", parsed_json$relevant_formulas, fixed = TRUE))) {
      parsed_json$relevant_formulas = c(default_formula)
    }
  }

  if (is.null(parsed_json$relevant_fred_series) || length(parsed_json$relevant_fred_series) == 0) {
    parsed_json$relevant_fred_series = strsplit(primary_series, ",\\s*")[[1]]
  } else if (length(parsed_json$relevant_fred_series) == 1 && grepl(",", parsed_json$relevant_fred_series[[1]], fixed = TRUE)) {
    parsed_json$relevant_fred_series = strsplit(parsed_json$relevant_fred_series[[1]], ",\\s*")[[1]]
  }

  if (is.null(parsed_json$limits) || !nzchar(trimws(parsed_json$limits))) {
    parsed_json$limits = "This answer uses only the retrieved textbook section and does not include live FRED values."
  }

  parsed_json$interpretation = parsed_json$interpretation[seq_len(min(2, length(parsed_json$interpretation)))]
  parsed_json$relevant_formulas = parsed_json$relevant_formulas[seq_len(min(3, length(parsed_json$relevant_formulas)))]

  parsed_json
}


print_retrieval_summary = function(retrieval_result) {
  cat("Retrieval summary:\n")
  cat("- Document:", retrieval_result$document, "\n")
  cat("- Retrieval unit:", retrieval_result$retrieval_unit, "\n")
  cat("- Sections considered:", retrieval_result$num_sections_considered, "\n")
  cat("- Sections returned:", retrieval_result$num_sections_returned, "\n")
  cat("- Top entry id:", retrieval_result$results[[1]]$entry_id, "\n")
  cat("- Top series:", retrieval_result$results[[1]]$primary_fred_series, "\n\n")
}


safe_parse_llm_json = function(llm_raw, retrieval_result) {
  tryCatch(
    fromJSON(llm_raw),
    error = function(e) {
      list(
        answer = "The model returned incomplete structured output, so this response uses a fallback summary based on the retrieved section.",
        interpretation = c(
          "The top retrieved section should still be reviewed directly for the most reliable wording.",
          "Relevant FRED series are inferred from the retrieved section metadata."
        ),
        relevant_formulas = c(default_formula_for_entry_id(retrieval_result$results[[1]]$entry_id)),
        relevant_fred_series = strsplit(retrieval_result$results[[1]]$primary_fred_series, ",\\s*")[[1]],
        limits = "The local model response was truncated before valid JSON could be fully parsed."
      )
    }
  )
}


run_rag_query = function(query, document_path, chat_url, model, top_k = TOP_K) {
  retrieval_result = search_text(query, document_path, top_k = top_k)
  payload = build_payload(query, retrieval_result)
  payload_json = toJSON(payload, auto_unbox = TRUE, pretty = TRUE)
  llm_raw = ask_llm(ROLE, payload_json, chat_url, model)
  llm_parsed = safe_parse_llm_json(llm_raw, retrieval_result)
  llm_parsed = normalize_llm_output(llm_parsed, retrieval_result)
  llm_output = render_markdown(llm_parsed)

  list(
    query = query,
    retrieval_result = retrieval_result,
    payload_json = payload_json,
    llm_raw = llm_raw,
    llm_output = llm_output
  )
}


# 3. TEST THE COMPLETE WORKFLOW ###################################

cat("=== Configuration ===\n")
cat("MODEL:", MODEL, "\n")
cat("PORT:", PORT, "\n")
cat("OLLAMA_HOST:", OLLAMA_HOST, "\n")
cat("DOCUMENT:", DOCUMENT, "\n")
cat("TOP_K:", TOP_K, "\n\n")
cat("MAX_SECTION_CHARS:", MAX_SECTION_CHARS, "\n\n")

if (!check_ollama(OLLAMA_HOST)) {
  stop("Ollama is not reachable at the configured host and port.")
}

available_models = list_models(OLLAMA_HOST)
cat("=== Available Models ===\n")
cat(paste(available_models, collapse = ", "), "\n\n")

if (!(MODEL %in% available_models)) {
  stop(paste0("Configured model not found: ", MODEL))
}

for (query in QUERIES) {
  result = tryCatch(
    run_rag_query(
      query = query,
      document_path = DOCUMENT,
      chat_url = CHAT_URL,
      model = MODEL,
      top_k = TOP_K
    ),
    error = function(e) e
  )

  cat("============================================================\n")
  cat("Query:\n")
  cat(query, "\n\n")

  if (inherits(result, "error")) {
    cat("Workflow error:\n")
    cat(conditionMessage(result), "\n\n")
    next
  }

  cat("Retrieved titles:\n")
  cat(paste0("- ", vapply(result$retrieval_result$results, function(x) x$title, character(1)), collapse = "\n"), "\n\n")
  print_retrieval_summary(result$retrieval_result)

  cat("LLM output:\n")
  cat(result$llm_output, "\n\n")
}
