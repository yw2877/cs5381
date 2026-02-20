# 05_reporting.R
# Save AI Report in Multiple Formats
# Pairs with 05_reporting.py
# Tim Fraser

# This script demonstrates how to save AI-generated reports in different formats:
# .txt, .md, .html, and .docx. Students will learn how to format and write
# LLM output to various file types for different use cases.

# 0. SETUP ###################################

## 0.1 CRAN Mirror Configuration #################################
# Set CRAN mirror (required to avoid errors)
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

## 0.2 Load Packages #################################

# If you haven't already, install required packages:
# install.packages(c("readr", "rmarkdown", "officer"))

library(readr)     # for writing text files
# library(rmarkdown) # for generating HTML reports (requires pandoc)
# library(officer)   # for creating Word documents

## 0.2 Mock LLM Output #########################

# Simulate an AI response object
# In a real script, this would come from your LLM API call
mock_llm_response = list(
  response = "# Data Analysis Report

## Summary
The dataset contains 150 records with 3 key metrics showing positive trends.

## Key Findings
- Metric A increased by 15% over the period
- Metric B remained stable at 42 units
- Metric C showed significant variation

## Recommendations
Consider further investigation into Metric C variations."
)

# Extract the text content
report_text = mock_llm_response$response

# 1. SAVE AS PLAIN TEXT (.txt) ###################################

# Use readr::write_lines for simple text file writing
# Simple and universal format
write_lines(report_text, "report.txt")

cat("✅ Saved report.txt\n")

# Optional: Save as Markdown (.md) - uncomment if needed
# write_lines(report_text, "report.md")
# cat("✅ Saved report.md\n")

# Optional: Save as HTML (.html) - requires pandoc, uncomment if needed
# library(rmarkdown)
# temp_rmd = "temp_report.Rmd"
# write_lines(c("---", "output: html_document", "---", "", report_text), temp_rmd)
# render(temp_rmd, output_file = "report.html", quiet = TRUE)
# file.remove(temp_rmd)
# cat("✅ Saved report.html\n")

# Optional: Save as Word Document (.docx) - uncomment if needed
# library(officer)
# doc = read_docx()
# lines = strsplit(report_text, "\n")[[1]]
# for (line in lines) {
#   if (startsWith(line, "# ")) {
#     doc = body_add_par(doc, substring(line, 3), style = "heading 1")
#   } else if (startsWith(line, "## ")) {
#     doc = body_add_par(doc, substring(line, 4), style = "heading 2")
#   } else if (startsWith(line, "- ")) {
#     doc = body_add_par(doc, substring(line, 3), style = "List Bullet")
#   } else if (nchar(trimws(line)) > 0) {
#     doc = body_add_par(doc, line)
#   }
# }
# print(doc, target = "report.docx")
# cat("✅ Saved report.docx\n")

cat("\n✅ Report saved successfully as report.txt!\n")
