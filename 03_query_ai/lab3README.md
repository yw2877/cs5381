# Lab 3 - AI Reporting Pipeline

This folder now covers:
- **Task 1**: data pipeline preparation
- **Task 2**: prompt testing with Ollama/OpenAI
- **Task 3**: prompt iteration/refinement with final report artifacts

## Files
- `01_prepare_data_pipeline.R` - pulls/cleans/summarizes FRED series and writes AI-ready data artifacts.
- `02_run_ai_report.R` - fills the Task 2 prompt with summary values and calls Ollama/OpenAI.
- `reports/prompt_v1.md` - Task 2 prompt template.
- `03_iterate_and_refine.R` - runs 3 prompt iterations and writes final Task 3 deliverables.

## Task 1 Output Artifacts
- `data/fred_raw_long.csv`
- `data/fred_processed_wide.csv`
- `data/fred_summary.csv`
- `data/fred_ai_payload.json`
- `reports/prompt_input.txt`

## Run Task 1
```bash
Rscript dsai/03_ai_reporting/01_prepare_data_pipeline.R
```

## Run Task 2 (Ollama/OpenAI test)
```bash
Rscript dsai/03_ai_reporting/02_run_ai_report.R
```

### Provider selection
- Default: `AI_PROVIDER=auto` (tries Ollama first, then OpenAI)
- Force Ollama:
  ```bash
  AI_PROVIDER=ollama OLLAMA_HOST=http://127.0.0.1:11434 OLLAMA_MODEL=smollm2:1.7b Rscript dsai/03_ai_reporting/02_run_ai_report.R
  ```
- Force OpenAI:
  ```bash
  AI_PROVIDER=openai OPENAI_API_KEY=your_key OPENAI_MODEL=gpt-4o-mini Rscript dsai/03_ai_reporting/02_run_ai_report.R
  ```

## Task 2 Outputs
- `reports/prompt_v1_filled.md` (prompt with actual values)
- `reports/ai_report_latest.md` (latest successful model output)
- `reports/ai_report_<provider>_<timestamp>.md` (versioned output)
- `reports/ai_report_error.log` (if both providers fail)

## Notes
- If `data/fred_summary.csv` is missing and `FRED_API_KEY` is unavailable, the Task 2 script falls back to demo summary values so you can still test model connectivity and prompt format.


## Run Task 3 (Iterate and Refine)
```bash
Rscript dsai/03_ai_reporting/03_iterate_and_refine.R
```

### Task 3 Outputs
- `reports/task3_prompt_v1.md`, `reports/task3_prompt_v2.md`, `reports/task3_prompt_v3.md`
- `reports/task3_output_v1_<provider>_<timestamp>.md`
- `reports/task3_output_v2_<provider>_<timestamp>.md`
- `reports/task3_output_v3_<provider>_<timestamp>.md`
- `reports/final_ai_report.md` (copied from v3 output for easy screenshot)
- `reports/task3_iteration_notes.md` (2-3 sentence explanation of iteration choices)

## Submission Mapping (Lab requirement)
- Complete script: `01_prepare_data_pipeline.R` + `03_iterate_and_refine.R`
- Final AI report screenshot: use `reports/final_ai_report.md`
- Brief explanation: use `reports/task3_iteration_notes.md`