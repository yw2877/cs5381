```mermaid
flowchart LR
  A[INPUTS: api_key series_id limit sort_order] --> B[REQUEST: GET observations from FRED]
  B --> C[CLEAN: convert value to numeric handle missing]
  C --> D[OUTPUT: latest 20 rows table date value]