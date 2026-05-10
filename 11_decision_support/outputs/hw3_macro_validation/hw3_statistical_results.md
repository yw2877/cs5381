# HW3 Macro Validation Statistical Results

## Hypotheses

- Null hypothesis: Prompt A, Prompt B, and Prompt C have the same mean overall validation score.
- Alternative hypothesis: At least one prompt has a different mean overall validation score.

## Summary Statistics

# A tibble: 3 × 12
  prompt_id prompt_name             n mean_overall sd_overall mean_fred_evidence
  <chr>     <chr>               <int>        <dbl>      <dbl>              <dbl>
1 C         Decision-Focused S…     3         2.29      0                   1.33
2 B         Evidence-Strict Ma…     3         2.24      0.083               2   
3 A         Baseline Macro Bri…     3         1.38      0.165               1   
# ℹ 6 more variables: mean_rag_grounding <dbl>, mean_macro_reasoning <dbl>,
#   mean_tool_faithfulness <dbl>, mean_risk_framing <dbl>,
#   mean_stakeholder_clarity <dbl>, mean_limits_transparency <dbl>

## ANOVA

One-way ANOVA on `overall_score ~ prompt_id`

- F-statistic: 68.8812
- Numerator df: 2
- Denominator df: 6
- p-value: 7.27e-05

Interpretation: Prompt choice had a statistically significant effect on overall validation score. The best mean score was C (Decision-Focused Stakeholder Brief), with mean overall score 2.286.

## Score Columns

# A tibble: 9 × 5
  report_id       prompt_id overall_score pass  main_weakness                   
  <chr>           <chr>             <dbl> <lgl> <chr>                           
1 prompt_A_run_01 A                  1.57 FALSE Overly simplistic interpretatio…
2 prompt_A_run_02 A                  1.29 TRUE  The interpretation of the risk …
3 prompt_A_run_03 A                  1.29 TRUE  The interpretation is overly si…
4 prompt_B_run_01 B                  2.14 TRUE  The explanation of the derived …
5 prompt_B_run_02 B                  2.29 TRUE  The reasoning lacks depth and d…
6 prompt_B_run_03 B                  2.29 TRUE  The explanation of the derived …
7 prompt_C_run_01 C                  2.29 FALSE The reasoning lacks depth and d…
8 prompt_C_run_02 C                  2.29 FALSE The interpretation of the data …
9 prompt_C_run_03 C                  2.29 TRUE  The reasoning lacks depth and d…

