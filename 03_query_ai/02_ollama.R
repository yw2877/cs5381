# 02_ollama.R
# Query Ollama LLM
# Pairs with 02_ollama.py
# Tim Fraser

# This script demonstrates how to query a local Ollama LLM instance using R.
# Students will learn how to make HTTP POST requests to interact with language models
# running locally on their machine.

# Set CRAN mirror (required to avoid errors)
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

# Starting message
cat("\n🚀 Sending LLM query in R...\n")

# If you haven't already, install these packages...
# install.packages(c("httr2", "jsonlite"))

# Load libraries
library(httr2)    # For HTTP requests
library(jsonlite) # For working with JSON

PORT = 11434
OLLAMA_HOST = paste0("http://localhost:", PORT)
url = paste0("http://localhost:", PORT, "/api/generate")

# Construct the request body as a list
body <- list(
  model = "gemma3:latest", # Model name
  prompt = "Is model working?",        # User prompt
  stream = FALSE       # Non-streaming response
)

# Build and send the POST request to the Ollama REST API
res <- request(url) %>%
  req_body_json(body) %>%   # Attach the JSON body
  req_method("POST") %>%   # Set HTTP method
  req_perform()             # Execute the request

# Parse the response JSON
response <- resp_body_json(res)

# Extract the model's reply as a character string
# Ollama's /api/generate endpoint returns the response directly in the "response" field
output = response$response

# Print the model's reply
cat("\n📝 Model Response:\n")
cat(output)
cat("\n")

# Closing message
cat("✅ LLM query complete. Exiting R...\n")

# Close out of R, and don't save the environment.
q(save = "no")