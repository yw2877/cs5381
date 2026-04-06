# 05_fred_mishkin_multi_agent_lab.R
#
# A modular 08_function_calling lab that builds on:
# - 06-style agent orchestration via functions.R
# - 07-style Mishkin + FRED retrieval helpers
# - 08 custom tool calling and multi-agent chaining

suppressWarnings(suppressPackageStartupMessages(library(httr2)))
suppressWarnings(suppressPackageStartupMessages(library(jsonlite)))
suppressWarnings(suppressPackageStartupMessages(library(ollamar)))


# 0. Paths and configuration ###################################################

args_all = commandArgs(trailingOnly = FALSE)
file_arg = grep("^--file=", args_all, value = TRUE)
SCRIPT_PATH = if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg[1]))
} else if (!is.null(sys.frames()[[1]]$ofile)) {
  normalizePath(sys.frames()[[1]]$ofile)
} else {
  normalizePath(file.path(getwd(), "08_function_calling", "05_fred_mishkin_multi_agent_lab.R"), mustWork = FALSE)
}
SCRIPT_DIR = dirname(SCRIPT_PATH)
REPO_ROOT = normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = TRUE)

if (Sys.getenv("OLLAMA_HOST") == "") {
  Sys.setenv(OLLAMA_HOST = "http://127.0.0.1:11434")
}

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
  "Use live FRED data and the Mishkin knowledge base to explain whether current",
  "U.S. macro conditions look restrictive. Include formulas and discuss",
  paste(DEFAULT_SERIES, collapse = ", "),
  "in a way that a student or stakeholder can understand."
)

user_args = commandArgs(trailingOnly = TRUE)
USER_QUERY = if (length(user_args) > 0) {
  paste(user_args, collapse = " ")
} else {
  DEFAULT_QUERY
}


# 1. Source course layers ######################################################

source(file.path(SCRIPT_DIR, "functions.R"))
source(file.path(SCRIPT_DIR, "05a_fred_macro_helpers.R"))
source(file.path(SCRIPT_DIR, "05b_mishkin_rag_helpers.R"))
source(file.path(SCRIPT_DIR, "05c_tool_helpers.R"))
source(file.path(SCRIPT_DIR, "05d_render_helpers.R"))


# 2. Agent prompts #############################################################

agent1_role = paste(
  "You are a macro formula and evidence agent.",
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
  "You are a macro brief writer for non-technical stakeholders.",
  "You will receive an evidence packet from another agent.",
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


# 3. Run workflow ##############################################################

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

if (length(agent1_tool_calls) == 0) {
  stop("Agent 1 did not produce any tool calls. Try rerunning with a different tool-capable model.")
}

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

cat("=== Tool Model ===\n")
cat(TOOL_MODEL, "\n\n")

cat("=== Writer Model ===\n")
cat(WRITER_MODEL, "\n\n")

cat("=== User Query ===\n")
cat(USER_QUERY, "\n\n")

cat("=== Custom Tool Metadata ===\n")
cat(tool_metadata_json, "\n\n")

cat("=== Agent 1 Output ===\n")
cat(agent1_output, "\n\n")

cat("=== Agent 2 Output ===\n")
cat(agent2_output, "\n")
