# lab_fred_multi_agent.R
#
# Multi-agent workflow for a simple FRED-style macro brief.
# R computes the metric table from simple mock input.
# Agent 1 analyzes the prepared macro table.
# Agent 2 turns Agent 1's analysis into a formatted macro brief.

if (Sys.getenv("OLLAMA_HOST") == "") {
  Sys.setenv(OLLAMA_HOST = "http://127.0.0.1:11434")
}

source("functions.R")

available_models = tryCatch(ollamar::list_models(), error = function(e) NULL)
if (is.null(available_models) || nrow(available_models) == 0) {
  stop("No local Ollama models are installed. Install a model first, then rerun this script.")
}

preferred_model = "gemma3:latest"
MODEL = if (preferred_model %in% available_models$name) preferred_model else available_models$name[[1]]
ensure_ollama_model(MODEL, auto_pull = FALSE)

# Simple mock FRED-style user input
raw_input = data.frame(
  Indicator = c("FEDFUNDS", "UNRATE", "CPIAUCSL", "DGS10"),
  Previous = c(5.00, 4.00, 300.00, 4.00),
  Current = c(4.50, 4.20, 303.00, 3.80)
)

# Deterministic calculations are done in R so the workflow stays numerically reliable.
prepared_input = raw_input
prepared_input$Absolute_Change = round(prepared_input$Current - prepared_input$Previous, 2)
prepared_input$Percent_Change_Value = round((prepared_input$Absolute_Change / prepared_input$Previous) * 100, 2)
prepared_input$Ratio = round(prepared_input$Current / prepared_input$Previous, 3)
prepared_input$Direction = ifelse(
  prepared_input$Absolute_Change > 0,
  "up",
  ifelse(prepared_input$Absolute_Change < 0, "down", "stable")
)
prepared_input$Absolute_Formula = sprintf(
  "%.2f - %.2f = %.2f",
  prepared_input$Current,
  prepared_input$Previous,
  prepared_input$Absolute_Change
)
prepared_input$Percent_Formula = sprintf(
  "(%.2f / %.2f) x 100 = %.2f%%",
  prepared_input$Absolute_Change,
  prepared_input$Previous,
  prepared_input$Percent_Change_Value
)
prepared_input$Ratio_Formula = sprintf(
  "%.2f / %.2f = %.3f",
  prepared_input$Current,
  prepared_input$Previous,
  prepared_input$Ratio
)
prepared_input$Percent_Change = sprintf("%.2f%%", prepared_input$Percent_Change_Value)

agent1_input = prepared_input[, c(
  "Indicator",
  "Previous",
  "Current",
  "Absolute_Change",
  "Percent_Change",
  "Ratio",
  "Direction",
  "Absolute_Formula",
  "Percent_Formula",
  "Ratio_Formula"
)]

input_text = df_as_text(agent1_input)

agent1_role = paste(c(
  "You are a macro calculation agent.",
  "You will receive a markdown table of FRED-style indicators with precomputed values and formulas.",
  "Use only the information in that table.",
  "Return only markdown using this exact structure:",
  "## Formulas Used",
  "- Absolute Change = Current - Previous",
  "- Percent Change = (Absolute Change / Previous) x 100",
  "- Ratio = Current / Previous",
  "- Direction rule = up, down, or stable based on the sign of Absolute Change",
  "## Calculation Table",
  "| Indicator | Previous | Current | Absolute Formula | Percent Formula | Ratio Formula | Absolute Change | Percent Change | Ratio | Direction |",
  "| ... |",
  "Under ## Calculation Table, include exactly one row per indicator and preserve the provided values.",
  "Do not add commentary, interpretation, takeaways, or extra sections."
), collapse = "\n")

agent2_role = paste(c(
  "You are a macro brief writer.",
  "You will receive analysis results from another agent.",
  "Use only that information.",
  "Do not introduce any new data, calculations, forecasts, or extra assumptions.",
  "Do not repeat formula strings or reproduce the calculation table.",
  "Interpret indicators using these rules:",
  "- Higher UNRATE suggests a softer labor market.",
  "- Higher CPIAUCSL suggests stronger inflation pressure.",
  "- Lower FEDFUNDS suggests easier monetary policy.",
  "- Lower DGS10 suggests lower long-term yields.",
  "Return only markdown using this exact structure:",
  "# Macro Brief",
  "## Key Signals",
  "- bullet 1",
  "- bullet 2",
  "- bullet 3",
  "- bullet 4",
  "## Interpretation",
  "- bullet 1",
  "- bullet 2",
  "## Takeaway",
  "one sentence",
  "Under ## Key Signals, write exactly 4 markdown bullet points, one for each indicator, in plain language.",
  "Under ## Interpretation, write exactly 2 markdown bullet points focused on the combined macro meaning.",
  "Under ## Takeaway, write exactly 1 sentence and do not use a bullet there.",
  "Keep the tone concise, professional, and factual.",
  "Do not include code fences or any extra sections."
), collapse = "\n")

agent1_output = agent_run(
  role = agent1_role,
  task = input_text,
  model = MODEL,
  output = "text"
)

agent2_output = agent_run(
  role = agent2_role,
  task = agent1_output,
  model = MODEL,
  output = "text"
)

cat("=== Model Used ===\n")
cat(MODEL, "\n\n")

cat("=== Raw Input ===\n")
cat(df_as_text(raw_input), "\n\n")

cat("=== Agent 1 Output ===\n")
cat(agent1_output, "\n\n")

cat("=== Agent 2 Output ===\n")
cat(agent2_output, "\n")
