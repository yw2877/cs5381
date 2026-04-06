# 02_txt_mishkin_fred_task2.R
# Task 2 custom search workflow for the Mishkin + FRED text knowledge base
# Adapted from 02_txt.R, but retrieves whole topic sections instead of single lines.

args_all = commandArgs(trailingOnly = FALSE)
file_arg = grep("^--file=", args_all, value = TRUE)
SCRIPT_PATH = if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  normalizePath(".")
}
SCRIPT_DIR = dirname(SCRIPT_PATH)

DOCUMENT = file.path(SCRIPT_DIR, "mishkin_fred_explained.txt")
TOP_K = 2


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
    "have", "has", "had", "using", "used", "your", "their"
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


# Test search function with a sample query
sample_query = "What does an inverted yield curve usually mean?"
test_result = search_text(sample_query, DOCUMENT, top_k = TOP_K)

cat("=== Task 2 Search Function Test ===\n")
cat("Sample query:", sample_query, "\n\n")
cat("Document:", test_result$document, "\n")
cat("Retrieval unit:", test_result$retrieval_unit, "\n")
cat("Sections considered:", test_result$num_sections_considered, "\n")
cat("Sections returned:", test_result$num_sections_returned, "\n\n")

for (i in seq_along(test_result$results)) {
  result = test_result$results[[i]]
  cat("--- Result", i, "---\n")
  cat("Title:", result$title, "\n")
  cat("Entry ID:", result$entry_id, "\n")
  cat("Score:", result$score, "\n")
  cat("Primary FRED Series:", result$primary_fred_series, "\n")
  cat("Matched Terms:", paste(result$matched_terms, collapse = ", "), "\n")
  cat("Preview:", result$preview, "\n\n")
}
