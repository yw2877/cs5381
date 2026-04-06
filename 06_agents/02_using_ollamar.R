# 02_using_ollamar.R

# This script demonstrates how to use the ollamar package in R to interact with an LLM.

# Load packages
require(ollamar)
require(dplyr)
require(stringr)

# Ensure Ollama uses the default local host unless configured otherwise.
if (Sys.getenv("OLLAMA_HOST") == "") {
    Sys.setenv(OLLAMA_HOST = "http://127.0.0.1:11434")
}

# Select model of interest
MODEL = "gemma3:latest"


# Check if model is currently loaded
has_model = ollamar::list_models() |> 
    dplyr::filter(stringr::str_detect(name, MODEL)) %>%
    nrow() > 0

# If model is not loaded, pull it
if(!has_model) { ollamar::pull(MODEL) }

# Create a list of messages
messages = ollamar::create_messages(
    # Start with a system prompt
    ollamar::create_message(role = "system", content = "You are a helpful data analyst. Summarize patterns clearly, use plain language, and give concise recommendations."),
    # Add user prompt
    ollamar::create_message(role = "user", content = "Here is weekly sales data in dollars: Monday 120, Tuesday 95, Wednesday 140, Thursday 110, Friday 180. Identify the main trend, the highest and lowest day, and give one recommendation.")
)

system.time({
    resp = ollamar::chat(model = MODEL, messages = messages, output = "text", stream = FALSE)
    # append result to chat history
    messages = ollamar::append_message(x = messages, role = "assistant", content = resp)
})

# View the response
resp

# View the chat history in entirety
messages

# View the chat history in a more readable format
dplyr::bind_rows(messages)

## Convert dataframe chat history back to a list of messages
purrr::transpose(dplyr::bind_rows(messages))

# Clean up shop
rm(list = ls())
