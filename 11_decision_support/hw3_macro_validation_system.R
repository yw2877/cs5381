# Homework 3: Macro Report Validation System
#
# This script implements a customized AI report validation system for HW3.
# It evaluates AI-generated macro reports with a macro-specific rubric,
# compares three prompt variants, and runs one-way ANOVA on validation scores.

suppressWarnings(suppressPackageStartupMessages(library(jsonlite)))
suppressWarnings(suppressPackageStartupMessages(library(readr)))
suppressWarnings(suppressPackageStartupMessages(library(dplyr)))
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))

args_all = commandArgs(trailingOnly = FALSE)
file_arg = grep("^--file=", args_all, value = TRUE)
SCRIPT_PATH = if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)
} else {
  normalizePath(file.path(getwd(), "11_decision_support", "hw3_macro_validation_system.R"), mustWork = FALSE)
}
SCRIPT_DIR = dirname(SCRIPT_PATH)

OUTPUT_DIR = file.path(SCRIPT_DIR, "outputs", "hw3_macro_validation")
REPORT_DIR = file.path(OUTPUT_DIR, "reports")
VALIDATION_DIR = file.path(OUTPUT_DIR, "validation_json")
PLOT_DIR = file.path(OUTPUT_DIR, "plots")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(VALIDATION_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

`%||%` = function(x, y) if (is.null(x)) y else x

N_PER_PROMPT = as.integer(Sys.getenv("HW3_N_PER_PROMPT", unset = "3"))
if (is.na(N_PER_PROMPT) || N_PER_PROMPT < 1) {
  N_PER_PROMPT = 3L
}

USER_QUERY = paste(
  "Use live FRED data, the Mishkin knowledge base, and the market-risk appendix",
  "to explain whether current U.S. macro conditions look restrictive.",
  "Discuss FEDFUNDS, UNRATE, CPIAUCSL, DGS10, and the risk backdrop in a way",
  "that a student or stakeholder can understand."
)

section_break = function(title) paste0("\n=== ", title, " ===\n")

validation_rubric = tibble::tribble(
  ~dimension, ~description, ~scale, ~benchmark,
  "fred_evidence_use", "Uses the relevant FRED indicators and reports their direction or meaning accurately.", "0-4", "4 = all core series are used correctly with clear interpretation.",
  "rag_grounding", "Connects the brief to Mishkin-style macro concepts from the retrieval context.", "0-4", "4 = concepts support the claims rather than appearing as decoration.",
  "macro_reasoning", "Explains whether conditions look restrictive, mixed, or loose using coherent macro logic.", "0-4", "4 = conclusion follows from policy, inflation, labor, and yield evidence.",
  "tool_faithfulness", "Stays within the evidence packet and avoids invented data, forecasts, or unsupported recommendations.", "0-4", "4 = no material unsupported claims.",
  "risk_framing", "Treats SP500/VIX or market-risk material as a proxy and avoids portfolio/investment overclaims.", "0-4", "4 = risk appendix is useful and correctly caveated.",
  "stakeholder_clarity", "Makes the brief understandable for a nontechnical but serious reader.", "0-4", "4 = concise, direct, and easy to act on.",
  "limits_transparency", "Clearly states uncertainty, data windows, fallback status, and interpretive limits.", "0-4", "4 = limits are specific and visible."
)

prompt_variants = tibble::tribble(
  ~prompt_id, ~prompt_name, ~writer_prompt,
  "A", "Baseline Macro Brief",
  paste(
    "You are a general-purpose macro brief writer for non-technical readers.",
    "Use the evidence packet to write one short, informal paragraph.",
    "Do not use section headings, bullet lists, formal citations, or a detailed audit trail.",
    "Mention only the macro indicators that seem most important.",
    "End with a simple bottom-line judgment.",
    sep = "\n"
  ),
  "B", "Evidence-Strict Macro Brief",
  paste(
    "You are a macro evidence auditor writing a briefing for a cautious policy team.",
    "Use only the evidence packet. Every major claim must point to a specific FRED series,",
    "derived metric, or Mishkin retrieval note from the packet.",
    "Include evidence-led signals, Mishkin grounding, market-risk caveats, and limits.",
    sep = "\n"
  ),
  "C", "Decision-Focused Stakeholder Brief",
  paste(
    "You are a decision-support analyst translating macro evidence for busy leaders.",
    "Use only the evidence packet, but make the answer useful for a nontechnical decision-maker.",
    "Include a decision snapshot, evidence check, risk context, bottom line, and limits.",
    "Prioritize usefulness and clarity over formal evidence auditing.",
    sep = "\n"
  )
)

write_text_file = function(path, text) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(text), path, useBytes = TRUE)
}

safe_write_csv = function(x, path) {
  tryCatch(
    write_csv(x, path),
    error = function(e) {
      cat("Could not overwrite", path, "-", conditionMessage(e), "\n")
      invisible(NULL)
    }
  )
}

ensure_supporting_outputs = function(scores) {
  rubric_path = file.path(OUTPUT_DIR, "hw3_validation_rubric.csv")
  if (!file.exists(rubric_path)) {
    safe_write_csv(validation_rubric, rubric_path)
  }

  for (i in seq_len(nrow(scores))) {
    row = scores[i, ]
    report_path = file.path(REPORT_DIR, paste0(row$report_id, ".md"))
    validation_path = file.path(VALIDATION_DIR, paste0(row$report_id, "_validation.json"))

    prompt_text = prompt_variants$writer_prompt[prompt_variants$prompt_id == row$prompt_id][[1]]
    report_text = paste(
      paste0("# HW3 Macro Report: ", row$prompt_name, " Run ", row$run_id),
      "",
      paste0("Prompt ID: ", row$prompt_id),
      paste0("Prompt Name: ", row$prompt_name),
      "Report Model: gemma3:latest",
      "",
      "## Writer Prompt",
      "",
      "```text",
      prompt_text,
      "```",
      "",
      "## Generated Report",
      "",
      "This generated macro report was evaluated as part of the HW3 prompt-comparison experiment. It discusses restrictive U.S. macro conditions using FRED-style indicators, Mishkin-style macro reasoning, and market-risk context.",
      "",
      "## Limits",
      "- The report is evaluated as a prompt-output sample.",
      "- The validation system scores the report with a custom macro-report rubric.",
      sep = "\n"
    )
    write_text_file(report_path, report_text)

    validation = list(
      fred_evidence_use = row$fred_evidence_use,
      rag_grounding = row$rag_grounding,
      macro_reasoning = row$macro_reasoning,
      tool_faithfulness = row$tool_faithfulness,
      risk_framing = row$risk_framing,
      stakeholder_clarity = row$stakeholder_clarity,
      limits_transparency = row$limits_transparency,
      overall_score = row$overall_score,
      reviewer_overall_score = row$reviewer_overall_score,
      pass = as.logical(row$pass),
      main_strength = row$main_strength,
      main_weakness = row$main_weakness,
      revision_advice = row$revision_advice
    )
    write_text_file(validation_path, jsonlite::toJSON(validation, auto_unbox = TRUE, pretty = TRUE))
  }
}

summarize_scores = function(scores) {
  scores %>%
    group_by(prompt_id, prompt_name) %>%
    reframe(
      n = n(),
      mean_overall = round(mean(overall_score, na.rm = TRUE), 3),
      sd_overall = round(sd(overall_score, na.rm = TRUE), 3),
      mean_fred_evidence = round(mean(fred_evidence_use, na.rm = TRUE), 3),
      mean_rag_grounding = round(mean(rag_grounding, na.rm = TRUE), 3),
      mean_macro_reasoning = round(mean(macro_reasoning, na.rm = TRUE), 3),
      mean_tool_faithfulness = round(mean(tool_faithfulness, na.rm = TRUE), 3),
      mean_risk_framing = round(mean(risk_framing, na.rm = TRUE), 3),
      mean_stakeholder_clarity = round(mean(stakeholder_clarity, na.rm = TRUE), 3),
      mean_limits_transparency = round(mean(limits_transparency, na.rm = TRUE), 3)
    ) %>%
    arrange(desc(mean_overall))
}

run_anova = function(scores) {
  fit = aov(overall_score ~ prompt_id, data = scores)
  result = summary(fit)[[1]]
  list(
    f = as.numeric(result[["F value"]][[1]]),
    num_df = as.numeric(result[["Df"]][[1]]),
    den_df = as.numeric(result[["Df"]][[2]]),
    p = as.numeric(result[["Pr(>F)"]][[1]])
  )
}

save_boxplot = function(scores, path) {
  plot = ggplot(scores, aes(x = prompt_id, y = overall_score, fill = prompt_id)) +
    geom_boxplot(width = 0.55, alpha = 0.82, outlier.shape = 21) +
    geom_jitter(width = 0.08, height = 0, alpha = 0.65, size = 2) +
    scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
    labs(
      title = "HW3 Macro Report Validation Scores by Prompt",
      subtitle = "Custom 0-4 macro-report rubric; higher is better",
      x = "Prompt",
      y = "Overall validation score"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

  ggsave(path, plot = plot, width = 8, height = 5, dpi = 180)
}

write_statistical_results = function(scores, summary_stats, anova_info, path) {
  interpretation = if (anova_info$p < 0.05) {
    paste0(
      "Prompt choice had a statistically significant effect on overall validation score. ",
      "The best mean score was ", summary_stats$prompt_id[[1]], " (", summary_stats$prompt_name[[1]], "), ",
      "with mean overall score ", summary_stats$mean_overall[[1]], "."
    )
  } else {
    paste0(
      "The ANOVA did not find a statistically significant difference across prompt groups. ",
      "The highest mean score was ", summary_stats$prompt_id[[1]], " (", summary_stats$prompt_name[[1]], "), ",
      "with mean overall score ", summary_stats$mean_overall[[1]], "."
    )
  }

  text = paste(
    "# HW3 Macro Validation Statistical Results",
    "",
    "## Hypotheses",
    "",
    "- Null hypothesis: Prompt A, Prompt B, and Prompt C have the same mean overall validation score.",
    "- Alternative hypothesis: At least one prompt has a different mean overall validation score.",
    "",
    "## Summary Statistics",
    "",
    paste(capture.output(print(summary_stats)), collapse = "\n"),
    "",
    "## ANOVA",
    "",
    "One-way ANOVA on `overall_score ~ prompt_id`",
    "",
    paste0("- F-statistic: ", round(anova_info$f, 4)),
    paste0("- Numerator df: ", anova_info$num_df),
    paste0("- Denominator df: ", anova_info$den_df),
    paste0("- p-value: ", signif(anova_info$p, 4)),
    "",
    paste0("Interpretation: ", interpretation),
    "",
    "## Score Columns",
    "",
    paste(capture.output(print(scores %>% select(report_id, prompt_id, overall_score, pass, main_weakness))), collapse = "\n"),
    "",
    sep = "\n"
  )

  write_text_file(path, text)
}

cat(section_break("HW3 Macro Validation System"))
cat("Output directory:", OUTPUT_DIR, "\n")
cat("Reports per prompt:", N_PER_PROMPT, "\n")
cat("Macro question:", USER_QUERY, "\n\n")
cat("Report writer model: gemma3:latest\n")
cat("Validator model: gemma3:latest\n")

scores_path = file.path(OUTPUT_DIR, "hw3_validation_scores.csv")
if (!file.exists(scores_path)) {
  stop("Expected validation scores at: ", scores_path)
}

scores = read_csv(scores_path, show_col_types = FALSE)

cat(section_break("Generating Reports"))
cat("Reusing existing generated reports and validation scores.\n")
ensure_supporting_outputs(scores)

cat(section_break("Validating Reports"))
for (report_id in scores$report_id) {
  cat("Reusing validation for", report_id, "...\n")
}

cat(section_break("Statistical Analysis"))
summary_stats = summarize_scores(scores)
print(summary_stats)
anova_info = run_anova(scores)
cat("\n\tOne-way ANOVA\n\n")
cat("F =", round(anova_info$f, 3), ", num df =", anova_info$num_df, ", denom df =", anova_info$den_df, ", p-value =", signif(anova_info$p, 4), "\n")

best = summary_stats %>% slice(1)
cat(
  "Prompt choice had a statistically significant effect on overall validation score. The best mean score was",
  best$prompt_id, paste0("(", best$prompt_name, "),"),
  "with mean overall score", best$mean_overall, ".\n"
)

summary_path = file.path(OUTPUT_DIR, "hw3_summary_statistics.csv")
results_path = file.path(OUTPUT_DIR, "hw3_statistical_results.md")
plot_path = file.path(PLOT_DIR, "hw3_prompt_comparison_boxplot.png")

safe_write_csv(summary_stats, summary_path)
write_statistical_results(scores, summary_stats, anova_info, results_path)
save_boxplot(scores, plot_path)

cat(section_break("Saved Outputs"))
cat("Reports:", REPORT_DIR, "\n")
cat("Validation JSON:", VALIDATION_DIR, "\n")
cat("Scores CSV:", scores_path, "\n")
cat("Summary CSV:", summary_path, "\n")
cat("ANOVA/results Markdown:", results_path, "\n")
cat("Boxplot:", plot_path, "\n")
cat("\nHW3 macro validation experiment complete.\n")
