This folder contains the **Task 1 data pipeline** for Lab 3.
It reuses the Lab 1 FRED query approach and prepares cleaned, structured data for AI analysis.

## Script
- `01_prepare_data_pipeline.R`

## What it does
1. Loads `FRED_API_KEY` from `.env`
2. Pulls 4 FRED series: `FEDFUNDS`, `UNRATE`, `CPIAUCSL`, `DGS10`
3. Cleans missing/non-numeric values
4. Builds summary stats + trend labels
5. Writes AI-ready outputs:
   - `data/fred_raw_long.csv`
   - `data/fred_processed_wide.csv`
   - `data/fred_summary.csv`
   - `data/fred_ai_payload.json`
   - `reports/prompt_input.txt`

## Run
```bash
Rscript dsai/03_ai_reporting/01_prepare_data_pipeline.R
```

## Next step (Task 2)
Use `reports/prompt_input.txt` as your initial prompt input for Ollama or OpenAI scripts.
