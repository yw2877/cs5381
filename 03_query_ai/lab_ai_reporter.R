# lab_ai_reporter.R
# Build an AI-Powered Data Reporter
# Task 1: Prepare Data Pipeline
# Tim Fraser

# This script:
# 1. Queries MBTA API to get route data
# 2. Processes and aggregates the data
# 3. Formats data for AI consumption

# 0. SETUP ###################################

## 0.1 CRAN Mirror Configuration #################################
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

## 0.2 Load Packages #################################
library(httr2)    # For HTTP requests
library(jsonlite) # For working with JSON
# library(dplyr)    # For data manipulation (optional - using base R instead)

cat("\n🚀 Starting AI-Powered Data Reporter Pipeline...\n\n")

# 1. QUERY API ###################################

## 1.1 Load API Key #################################
# Load environment variables from .env file
if (file.exists(".env")) {
  readRenviron(".env")
} else {
  # Try parent directory
  if (file.exists("../.env")) {
    readRenviron("../.env")
  } else {
    stop("Error: .env file not found. Please create it in the project root.")
  }
}

key = Sys.getenv("MBTA_API_KEY")
cat("MBTA_API_KEY found:", nzchar(key), "\n")
if (!nzchar(key)) {
  stop("Error: MBTA_API_KEY not found in .env file!\nPlease add: MBTA_API_KEY=your_key_here")
}

## 1.2 Make API Request #################################
cat("\n📡 Making API request to MBTA...\n")
resp = request("https://api-v3.mbta.com/routes") |>
  req_headers(`x-api-key` = key, accept = "application/vnd.api+json") |>
  req_url_query(`page[limit]` = 50) |>  # Get more data for better analysis
  req_perform()

cat("Status:", resp$status_code, "\n")
if (resp$status_code != 200) {
  cat("Response body:", resp_body_string(resp), "\n")
  stop("API request failed with status code: ", resp$status_code)
}

## 1.3 Parse Response #################################
cat("Parsing JSON response...\n")
obj = resp_body_json(resp)
cat("Number of routes in response:", length(obj$data), "\n")

## 1.4 Create Data Frame #################################
cat("Creating data frame...\n")
df = do.call(rbind, lapply(obj$data, function(x) {
  data.frame(
    route_id = x$id,
    long_name = x$attributes$long_name,
    description = ifelse(is.null(x$attributes$description), "", x$attributes$description),
    type = x$attributes$type,
    stringsAsFactors = FALSE
  )
}))

cat("Total records:", nrow(df), "\n")
cat("\nFirst few records:\n")
print(df[1:min(10, nrow(df)), ], row.names = FALSE)

# 2. PROCESS DATA ###################################

cat("\n📊 Processing data...\n")

## 2.1 Data Cleaning #################################
# Remove any rows with missing critical data
df_clean = df[!is.na(df$type) & nchar(df$long_name) > 0, ]
cat("Records after cleaning:", nrow(df_clean), "\n")

## 2.2 Aggregate Statistics #################################
# Count routes by type
# Type codes: 0=Light Rail, 1=Heavy Rail, 2=Commuter Rail, 3=Bus, 4=Ferry
type_labels = c("0" = "Light Rail", "1" = "Heavy Rail", "2" = "Commuter Rail", "3" = "Bus", "4" = "Ferry")
df_clean$type_label = type_labels[as.character(df_clean$type)]

# Count routes by type using base R
route_summary = aggregate(count ~ type_label, 
  data = data.frame(
    type_label = df_clean$type_label,
    count = 1
  ), 
  FUN = length
)
route_summary = route_summary[order(route_summary$count, decreasing = TRUE), ]

cat("\n📈 Route Summary by Type:\n")
print(route_summary, row.names = FALSE)

# Overall statistics
total_routes = nrow(df_clean)
unique_types = length(unique(df_clean$type))

cat("\n📊 Overall Statistics:\n")
cat("  Total routes:", total_routes, "\n")
cat("  Unique route types:", unique_types, "\n")
cat("  Most common type:", route_summary$type_label[1], "(", route_summary$count[1], "routes)\n")

# 3. FORMAT DATA FOR AI ###################################

cat("\n📝 Formatting data for AI consumption...\n")

## 3.1 Create Structured Summary #################################
# Format as structured text that AI can easily understand
data_summary = paste0(
  "MBTA Route Data Summary:\n",
  "Total Routes: ", total_routes, "\n",
  "Route Types: ", unique_types, "\n\n",
  "Breakdown by Type:\n",
  paste(
    paste0("  - ", route_summary$type_label, ": ", route_summary$count, " routes"),
    collapse = "\n"
  ),
  "\n\n",
  "Sample Routes (first 10):\n",
  paste(
    paste0("  - ", df_clean$long_name[1:min(10, nrow(df_clean))], " (Type: ", df_clean$type_label[1:min(10, nrow(df_clean))], ")"),
    collapse = "\n"
  )
)

cat("\n📄 Formatted Data Summary:\n")
cat(data_summary)
cat("\n\n")

## 3.2 Save as JSON (for structured AI input) #################################
# Create a structured JSON object for AI
json_data = list(
  summary = list(
    total_routes = total_routes,
    unique_types = unique_types,
    most_common_type = route_summary$type_label[1]
  ),
  breakdown = setNames(as.list(route_summary$count), route_summary$type_label),
  sample_routes = df_clean[1:min(10, nrow(df_clean)), c("long_name", "type_label")]
)

json_string = toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
writeLines(json_string, "mbta_data.json")
cat("✅ Saved formatted data as JSON: mbta_data.json\n")

## 3.3 Save as Text (for AI prompt) #################################
writeLines(data_summary, "mbta_data_summary.txt")
cat("✅ Saved data summary as text: mbta_data_summary.txt\n")

cat("\n✅ Task 1 Complete: Data pipeline prepared!\n")
cat("   - API data queried and processed\n")
cat("   - Data aggregated and summarized\n")
cat("   - Data formatted for AI (JSON and text formats)\n\n")

# Keep data in environment for Task 2
cat("💡 Data is ready for AI analysis in Task 2!\n")
cat("   Use 'data_summary' variable or load from 'mbta_data_summary.txt'\n\n")

# ============================================================================
# TASK 2: DESIGN YOUR AI PROMPT
# ============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("TASK 2: Design Your AI Prompt\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 4. DESIGN AI PROMPT ###################################

cat("🤖 Designing AI prompt for MBTA route analysis...\n\n")

## 4.1 Define What We Want AI to Return #################################
# Based on your project plan:
# 1. Reliability-first Route Recommendation
# 2. Bunching Detection and Warning  
# 3. "Leave-by window" Departure Time Reminder

# We want AI to provide:
# - Insights about route reliability patterns
# - Recommendations for route selection based on stability
# - Analysis of route types and their characteristics
# - Suggestions for improving user experience

## 4.2 Create Initial Prompt #################################
# Build a comprehensive prompt that includes:
# - The processed data
# - Clear instructions for analysis
# - Specific format requirements

ai_prompt = paste0(
  "You are analyzing MBTA (Massachusetts Bay Transportation Authority) route data to help build a transit app that prioritizes reliability over speed. ",
  "The app focuses on three key features:\n\n",
  "1. **Reliability-first Route Recommendation**: Recommends routes that are most stable and less prone to delays, ",
  "not necessarily the fastest routes.\n",
  "2. **Bunching Detection and Warning**: Identifies when vehicles arrive too close together and warns users about potential crowding.\n",
  "3. **Leave-by Window Departure Time Reminder**: Provides reliable departure time windows instead of single ETAs.\n\n",
  "Here is the MBTA route data:\n\n",
  data_summary,
  "\n\n",
  "Please analyze this data and provide:\n",
  "1. **Summary Statistics**: Key insights about the route distribution and types\n",
  "2. **Reliability Insights**: Which route types (Bus, Light Rail, Heavy Rail, Commuter Rail, Ferry) might be more reliable ",
  "based on their characteristics. Consider factors like:\n",
  "   - Fixed infrastructure (rails) vs flexible routes (buses)\n",
  "   - Frequency and service patterns\n",
  "   - Historical reliability patterns\n",
  "3. **Recommendations**: Specific recommendations for the app features:\n",
  "   - Which route types to prioritize for reliability-first recommendations\n",
  "   - How to detect potential bunching issues\n",
  "   - How to calculate reliable departure time windows\n",
  "4. **Format**: Present your analysis in a clear, structured format with:\n",
  "   - Bullet points for key insights\n",
  "   - Short paragraphs for explanations\n",
  "   - Specific actionable recommendations\n\n",
  "Keep the response focused, practical, and relevant to building a transit app that helps users make reliable travel decisions."
)

cat("📝 AI Prompt Created:\n")
cat("   Length:", nchar(ai_prompt), "characters\n")
cat("   Includes: Data summary + Analysis instructions + Format requirements\n\n")

# Save prompt for reference
writeLines(ai_prompt, "ai_prompt.txt")
cat("✅ Saved AI prompt to: ai_prompt.txt\n\n")

## 4.3 Test with Ollama #################################
cat("🧪 Testing AI prompt with Ollama...\n")
cat("   Make sure Ollama server is running (bash 01_ollama.sh)\n\n")

PORT = 11434
OLLAMA_HOST = paste0("http://localhost:", PORT)
url = paste0(OLLAMA_HOST, "/api/generate")

# Check if Ollama server is running
cat("Checking Ollama server connection...\n")
test_connection = tryCatch({
  test_resp = request(paste0(OLLAMA_HOST, "/api/tags")) |>
    req_method("GET") |>
    req_perform()
  TRUE
}, error = function(e) {
  FALSE
})

if (!test_connection) {
  cat("⚠️  Warning: Cannot connect to Ollama server at", OLLAMA_HOST, "\n")
  cat("   Please start Ollama server first: bash 01_ollama.sh\n")
  cat("   Or the server may not be running on port", PORT, "\n\n")
  cat("💡 You can test the prompt later by:\n")
  cat("   1. Starting Ollama: bash 01_ollama.sh\n")
  cat("   2. Running this script again\n")
  cat("   3. Or manually testing with the prompt in ai_prompt.txt\n\n")
} else {
  cat("✅ Ollama server is running!\n\n")
  
  # Construct the request body
  body = list(
    model = "gemma3:latest",  # Model name
    prompt = ai_prompt,        # Our designed prompt
    stream = FALSE            # Non-streaming response
  )
  
  cat("📤 Sending prompt to Ollama...\n")
  cat("   Model: gemma3:latest\n")
  cat("   This may take 30-60 seconds...\n\n")
  
  # Send request to Ollama
  ai_res = request(url) |>
    req_body_json(body) |>
    req_method("POST") |>
    req_perform()
  
  # Parse response
  ai_response = resp_body_json(ai_res)
  ai_output = ai_response$response
  
  # Display AI response
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("📝 AI-GENERATED REPORT\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
  cat(ai_output)
  cat("\n\n")
  
  # Save AI response
  writeLines(ai_output, "ai_report.txt")
  cat("✅ Saved AI report to: ai_report.txt\n\n")
  
  cat("✅ Task 2 Complete: AI prompt designed and tested!\n")
  cat("   - Prompt created with clear instructions\n")
  cat("   - Tested with Ollama successfully\n")
  cat("   - Report generated and saved\n\n")
}

cat("💡 Next Steps for Task 3:\n")
cat("   1. Review the AI output in ai_report.txt\n")
cat("   2. Refine the prompt based on results\n")
cat("   3. Test 2-3 iterations to improve output quality\n")
cat("   4. Document your prompt design choices\n\n")

