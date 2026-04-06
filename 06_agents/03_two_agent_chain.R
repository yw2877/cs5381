# 03_two_agent_chain.R

# Simple 2-agent workflow:
# 1. Agent 1 summarizes raw data
# 2. Agent 2 turns the summary into formatted output

library(tibble)
library(ollamar)

if (Sys.getenv("OLLAMA_HOST") == "") {
  Sys.setenv(OLLAMA_HOST = "http://127.0.0.1:11434")
}

source("functions.R")

MODEL = "gemma3:latest"
ensure_ollama_model(MODEL, auto_pull = FALSE)

# Raw input for Agent 1
sales_data = tibble::tibble(
  day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
  sales = c(120, 95, 140, 110, 180),
  orders = c(12, 10, 15, 11, 20)
)

raw_data = df_as_text(sales_data)

# Agent 1: summarize the raw data
role1 = paste(
  "You summarize raw sales tables.",
  "Return 3 short bullet points covering trend, highest day, and lowest day.",
  "Finish with one sentence recommendation."
)

agent1_output = agent_run(
  role = role1,
  task = raw_data,
  model = MODEL,
  output = "text"
)

# Agent 2: format the summary for presentation
role2 = paste(
  "You turn a summary into a polished markdown update.",
  "Return exactly these sections: # Weekly Sales Update, ## Key Points, ## Recommendation.",
  "Use only the information provided by the user."
)

agent2_output = agent_run(
  role = role2,
  task = agent1_output,
  model = MODEL,
  output = "text"
)

cat("=== Raw Data ===\n")
cat(raw_data, "\n\n")
cat("=== Agent 1 Summary ===\n")
cat(agent1_output, "\n\n")
cat("=== Agent 2 Formatted Output ===\n")
cat(agent2_output, "\n")
