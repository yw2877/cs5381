# 05a_fred_macro_helpers.R
# Deterministic FRED data helpers, metric calculations, and demo fallback logic.
# This module keeps the quantitative layer separate from the 08 lab orchestration.

lookup_series_field = function(series_id, field) {
  idx = match(series_id, SERIES_LIBRARY$series_id)
  if (is.na(idx)) {
    if (identical(field, "title")) {
      return(series_id)
    }
    return("")
  }
  SERIES_LIBRARY[[field]][idx]
}

format_num = function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("NA")
  }
  format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE, scientific = FALSE)
}

format_pct = function(x, digits = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("NA")
  }
  paste0(format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE, scientific = FALSE), "%")
}

normalize_series_ids = function(series_ids) {
  if (is.null(series_ids) || length(series_ids) == 0) {
    return(DEFAULT_SERIES)
  }

  if (is.list(series_ids)) {
    series_ids = unlist(series_ids, use.names = FALSE)
  }

  series_ids = as.character(series_ids)
  if (length(series_ids) == 1 && grepl(",", series_ids, fixed = TRUE)) {
    series_ids = unlist(strsplit(series_ids, ",", fixed = TRUE))
  }

  series_ids = toupper(trimws(series_ids))
  series_ids = series_ids[nzchar(series_ids)]

  if (length(series_ids) == 0) {
    DEFAULT_SERIES
  } else {
    unique(series_ids)
  }
}

normalize_flag = function(x, default = TRUE) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  if (is.logical(x)) {
    return(isTRUE(x[[1]]))
  }
  tolower(as.character(x[[1]])) %in% c("true", "1", "yes", "y")
}

load_fred_key = function() {
  env_candidates = c(
    file.path(SCRIPT_DIR, ".env"),
    file.path(REPO_ROOT, ".env"),
    file.path(REPO_ROOT, "01_query_api", ".env"),
    file.path(REPO_ROOT, "02_productivity", "shiny_app", ".env"),
    file.path(REPO_ROOT, "07_rag", ".env")
  )

  for (candidate in env_candidates) {
    if (file.exists(candidate)) {
      readRenviron(candidate)
      key = Sys.getenv("FRED_API_KEY")
      if (nzchar(key)) {
        return(key)
      }
    }
  }

  Sys.getenv("FRED_API_KEY")
}

fetch_fred_observations = function(series_id, start_date, end_date, limit, api_key) {
  resp = request("https://api.stlouisfed.org/fred/series/observations") |>
    req_url_query(
      series_id = series_id,
      observation_start = start_date,
      observation_end = end_date,
      sort_order = "desc",
      limit = limit,
      file_type = "json",
      api_key = api_key
    ) |>
    req_timeout(25) |>
    req_perform()

  obj = resp_body_json(resp)
  if (is.null(obj$observations) || length(obj$observations) == 0) {
    stop(paste("No observations returned for", series_id))
  }

  df = data.frame(
    date = as.Date(vapply(obj$observations, function(x) x$date, character(1))),
    value_raw = vapply(obj$observations, function(x) x$value, character(1)),
    stringsAsFactors = FALSE
  )

  df$value = suppressWarnings(as.numeric(df$value_raw))
  df = df[!is.na(df$value), c("date", "value")]
  df = df[order(df$date), , drop = FALSE]

  if (nrow(df) == 0) {
    stop(paste("Returned observations are non-numeric for", series_id))
  }

  df
}

summarize_series = function(series_id, df) {
  latest_row = df[nrow(df), , drop = FALSE]
  previous_row = if (nrow(df) >= 2) df[nrow(df) - 1, , drop = FALSE] else latest_row

  latest_value = latest_row$value[[1]]
  previous_value = previous_row$value[[1]]
  absolute_change = latest_value - previous_value
  percent_change = if (!is.na(previous_value) && previous_value != 0) {
    100 * (latest_value / previous_value - 1)
  } else {
    NA_real_
  }

  direction = if (is.na(absolute_change) || abs(absolute_change) < 1e-12) {
    "stable"
  } else if (absolute_change > 0) {
    "up"
  } else {
    "down"
  }

  data.frame(
    series_id = series_id,
    title = lookup_series_field(series_id, "title"),
    latest_date = as.character(latest_row$date[[1]]),
    latest_value = round(latest_value, 3),
    previous_date = as.character(previous_row$date[[1]]),
    previous_value = round(previous_value, 3),
    absolute_change = round(absolute_change, 3),
    percent_change = round(percent_change, 2),
    direction = direction,
    absolute_formula = paste(
      format_num(latest_value),
      "-",
      format_num(previous_value),
      "=",
      format_num(absolute_change)
    ),
    percent_formula = if (is.na(percent_change)) {
      "Percent change undefined because previous value is zero or missing."
    } else {
      paste(
        "100 * (",
        format_num(latest_value),
        "/",
        format_num(previous_value),
        "- 1) =",
        format_pct(percent_change),
        sep = " "
      )
    },
    context_note = lookup_series_field(series_id, "context_note"),
    observation_count = nrow(df),
    stringsAsFactors = FALSE
  )
}

compute_yoy_metric = function(df, source_series, metric_name, label) {
  if (nrow(df) < 13) {
    return(NULL)
  }

  latest_row = df[nrow(df), , drop = FALSE]
  ref_idx = which(df$date <= latest_row$date[[1]] - 364)
  if (length(ref_idx) == 0) {
    return(NULL)
  }

  ref_row = df[max(ref_idx), , drop = FALSE]
  yoy = 100 * (latest_row$value[[1]] / ref_row$value[[1]] - 1)

  data.frame(
    metric = metric_name,
    label = label,
    latest_date = as.character(latest_row$date[[1]]),
    value = round(yoy, 3),
    formula = paste(
      "100 * (",
      format_num(latest_row$value[[1]]),
      "/",
      format_num(ref_row$value[[1]]),
      "- 1) =",
      format_pct(yoy),
      sep = " "
    ),
    meaning = "Year-over-year inflation proxy from the price index.",
    source_series = source_series,
    stringsAsFactors = FALSE
  )
}

compute_monthly_annualized_inflation = function(df, source_series, metric_name, label) {
  if (nrow(df) < 2) {
    return(NULL)
  }

  latest_row = df[nrow(df), , drop = FALSE]
  previous_row = df[nrow(df) - 1, , drop = FALSE]
  annualized_inflation = 1200 * log(latest_row$value[[1]] / previous_row$value[[1]])

  data.frame(
    metric = metric_name,
    label = label,
    latest_date = as.character(latest_row$date[[1]]),
    value = round(annualized_inflation, 3),
    formula = paste(
      "1200 * ln(",
      format_num(latest_row$value[[1]]),
      "/",
      format_num(previous_row$value[[1]]),
      ") =",
      format_pct(annualized_inflation),
      sep = " "
    ),
    meaning = "Short-run annualized inflation momentum from the latest monthly price change.",
    source_series = source_series,
    stringsAsFactors = FALSE
  )
}

compute_derived_metrics = function(series_frames, summary_df) {
  rows = list()

  if ("CPIAUCSL" %in% names(series_frames)) {
    rows[[length(rows) + 1]] = compute_yoy_metric(
      series_frames[["CPIAUCSL"]],
      source_series = "CPIAUCSL",
      metric_name = "cpi_yoy_inflation",
      label = "CPI year-over-year inflation"
    )
    rows[[length(rows) + 1]] = compute_monthly_annualized_inflation(
      series_frames[["CPIAUCSL"]],
      source_series = "CPIAUCSL",
      metric_name = "cpi_monthly_annualized_inflation",
      label = "CPI monthly annualized inflation"
    )
  }

  if ("PCEPI" %in% names(series_frames)) {
    rows[[length(rows) + 1]] = compute_yoy_metric(
      series_frames[["PCEPI"]],
      source_series = "PCEPI",
      metric_name = "pce_yoy_inflation",
      label = "PCE year-over-year inflation"
    )
    rows[[length(rows) + 1]] = compute_monthly_annualized_inflation(
      series_frames[["PCEPI"]],
      source_series = "PCEPI",
      metric_name = "pce_monthly_annualized_inflation",
      label = "PCE monthly annualized inflation"
    )
  }

  rows = Filter(Negate(is.null), rows)
  derived_df = if (length(rows) > 0) do.call(rbind, rows) else NULL

  inflation_row = NULL
  if (!is.null(derived_df) && any(derived_df$metric == "cpi_yoy_inflation")) {
    inflation_row = derived_df[derived_df$metric == "cpi_yoy_inflation", , drop = FALSE][1, ]
  } else if (!is.null(derived_df) && any(derived_df$metric == "pce_yoy_inflation")) {
    inflation_row = derived_df[derived_df$metric == "pce_yoy_inflation", , drop = FALSE][1, ]
  }

  if ("FEDFUNDS" %in% summary_df$series_id && !is.null(inflation_row)) {
    fedfunds = summary_df[summary_df$series_id == "FEDFUNDS", , drop = FALSE][1, ]
    real_policy_rate = fedfunds$latest_value - inflation_row$value
    new_row = data.frame(
      metric = "real_policy_rate",
      label = "Ex post real policy rate",
      latest_date = fedfunds$latest_date,
      value = round(real_policy_rate, 3),
      formula = paste(
        format_num(fedfunds$latest_value),
        "-",
        format_num(inflation_row$value),
        "=",
        format_num(real_policy_rate)
      ),
      meaning = "Positive values suggest more restrictive inflation-adjusted policy conditions.",
      source_series = paste("FEDFUNDS +", inflation_row$source_series),
      stringsAsFactors = FALSE
    )
    derived_df = if (is.null(derived_df)) new_row else rbind(derived_df, new_row)
  }

  if ("DGS10" %in% summary_df$series_id && !is.null(inflation_row)) {
    dgs10 = summary_df[summary_df$series_id == "DGS10", , drop = FALSE][1, ]
    real_10y = dgs10$latest_value - inflation_row$value
    new_row = data.frame(
      metric = "real_10y_yield",
      label = "Ex post real 10-year Treasury yield",
      latest_date = dgs10$latest_date,
      value = round(real_10y, 3),
      formula = paste(
        format_num(dgs10$latest_value),
        "-",
        format_num(inflation_row$value),
        "=",
        format_num(real_10y)
      ),
      meaning = "Higher real long yields imply tighter long-term financing conditions.",
      source_series = paste("DGS10 +", inflation_row$source_series),
      stringsAsFactors = FALSE
    )
    derived_df = if (is.null(derived_df)) new_row else rbind(derived_df, new_row)
  }

  if (all(c("DGS10", "DGS2") %in% summary_df$series_id)) {
    dgs10 = summary_df[summary_df$series_id == "DGS10", , drop = FALSE][1, ]
    dgs2 = summary_df[summary_df$series_id == "DGS2", , drop = FALSE][1, ]
    term_spread = dgs10$latest_value - dgs2$latest_value
    new_row = data.frame(
      metric = "term_spread_10y_2y",
      label = "10-year minus 2-year Treasury spread",
      latest_date = dgs10$latest_date,
      value = round(term_spread, 3),
      formula = paste(
        format_num(dgs10$latest_value),
        "-",
        format_num(dgs2$latest_value),
        "=",
        format_num(term_spread)
      ),
      meaning = "Positive values imply a normal upward-sloping yield curve; negative values imply inversion.",
      source_series = "DGS10 + DGS2",
      stringsAsFactors = FALSE
    )
    derived_df = if (is.null(derived_df)) new_row else rbind(derived_df, new_row)
  }

  if (is.null(derived_df)) {
    data.frame(
      metric = character(0),
      label = character(0),
      latest_date = character(0),
      value = numeric(0),
      formula = character(0),
      meaning = character(0),
      source_series = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    derived_df
  }
}

compute_market_risk_appendix = function(series_frames) {
  unavailable = list(
    available = FALSE,
    note = "Market risk appendix unavailable because the auxiliary SP500 or VIXCLS data could not be prepared.",
    formula_note = "Formula trace unavailable because market-risk inputs could not be prepared.",
    lookback_observations = 0L,
    sample_start = NA_character_,
    sample_end = NA_character_,
    metrics = data.frame(
      metric = character(0),
      label = character(0),
      value = character(0),
      formula = character(0),
      meaning = character(0),
      stringsAsFactors = FALSE
    ),
    worst_losses = data.frame(
      Date = character(0),
      `SP500 Return` = character(0),
      `Loss Magnitude` = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )

  if (!("SP500" %in% names(series_frames))) {
    unavailable$note = "Market risk appendix unavailable because SP500 observations were not available."
    return(unavailable)
  }

  sp500_df = series_frames[["SP500"]]
  if (nrow(sp500_df) < 21) {
    unavailable$note = "Market risk appendix unavailable because SP500 returned too few daily observations for a meaningful historical VaR sample."
    return(unavailable)
  }

  return_dates = as.Date(sp500_df$date[-1])
  daily_returns = 100 * (sp500_df$value[-1] / sp500_df$value[-nrow(sp500_df)] - 1)
  valid_idx = which(!is.na(daily_returns) & is.finite(daily_returns))

  if (length(valid_idx) < 20) {
    unavailable$note = "Market risk appendix unavailable because SP500 daily returns could not be computed cleanly."
    return(unavailable)
  }

  return_dates = return_dates[valid_idx]
  daily_returns = daily_returns[valid_idx]
  var_95 = max(0, -as.numeric(stats::quantile(daily_returns, probs = 0.05, na.rm = TRUE, names = FALSE)))
  var_99 = max(0, -as.numeric(stats::quantile(daily_returns, probs = 0.01, na.rm = TRUE, names = FALSE)))

  loss_order = order(daily_returns)
  worst_n = min(5, length(loss_order))
  worst_idx = loss_order[seq_len(worst_n)]
  worst_losses = data.frame(
    Date = as.character(return_dates[worst_idx]),
    `SP500 Return` = vapply(daily_returns[worst_idx], format_pct, character(1)),
    `Loss Magnitude` = vapply(-daily_returns[worst_idx], function(x) format_pct(max(0, x)), character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  metrics = list(
    data.frame(
      metric = "sp500_hist_var_95",
      label = "SP500 1-day historical VaR (95%)",
      value = format_pct(var_95),
      formula = "VaR_95 = max(0, -quantile(r_t, 5%))",
      meaning = "Approximate one-day S&P 500 loss threshold from the recent historical return sample.",
      stringsAsFactors = FALSE
    ),
    data.frame(
      metric = "sp500_hist_var_99",
      label = "SP500 1-day historical VaR (99%)",
      value = format_pct(var_99),
      formula = "VaR_99 = max(0, -quantile(r_t, 1%))",
      meaning = "More extreme tail-loss threshold from the same recent S&P 500 return sample.",
      stringsAsFactors = FALSE
    )
  )

  if ("VIXCLS" %in% names(series_frames) && nrow(series_frames[["VIXCLS"]]) > 0) {
    vix_df = series_frames[["VIXCLS"]]
    latest_vix = vix_df$value[[nrow(vix_df)]]
    regime = if (is.na(latest_vix)) {
      "unavailable"
    } else if (latest_vix < 15) {
      "low"
    } else if (latest_vix < 25) {
      "moderate"
    } else {
      "high"
    }

    metrics[[length(metrics) + 1]] = data.frame(
      metric = "latest_vix",
      label = "Latest VIX close",
      value = format_num(latest_vix),
      formula = paste0("Latest VIXCLS close on ", as.character(vix_df$date[[nrow(vix_df)]])),
      meaning = "Options-implied near-term equity volatility from the CBOE VIX index.",
      stringsAsFactors = FALSE
    )
    metrics[[length(metrics) + 1]] = data.frame(
      metric = "vix_regime",
      label = "Volatility regime",
      value = regime,
      formula = "Heuristic: low < 15, moderate 15-25, high >= 25",
      meaning = "Simple interpretation layer for the latest VIX level; this is a market proxy, not portfolio VaR.",
      stringsAsFactors = FALSE
    )
  }

  list(
    available = TRUE,
    note = paste(
      "This appendix uses recent SP500 daily returns as an index-based market risk proxy, not a portfolio VaR.",
      "The sample window spans",
      as.character(return_dates[[1]]),
      "to",
      as.character(return_dates[[length(return_dates)]]),
      "with",
      length(daily_returns),
      "daily returns."
    ),
    formula_note = "Formula trace: VaR_95 = max(0, -quantile(r_t, 5%)); VaR_99 = max(0, -quantile(r_t, 1%)); VIX regime heuristic = low < 15, moderate 15-25, high >= 25.",
    lookback_observations = length(daily_returns),
    sample_start = as.character(return_dates[[1]]),
    sample_end = as.character(return_dates[[length(return_dates)]]),
    metrics = do.call(rbind, metrics),
    worst_losses = worst_losses
  )
}

formula_reference_for_metric = function(metric) {
  reference_map = list(
    cpi_yoy_inflation = list(
      topic = "topic_1_understanding_interest_rates",
      formula_label = "FRED-Linked Formula 10",
      textbook_formula = "pi_CPI_yoy_t = 100 * (CPIAUCSL_t / CPIAUCSL_t-12 - 1)"
    ),
    cpi_monthly_annualized_inflation = list(
      topic = "topic_6_money_and_inflation",
      formula_label = "FRED-Linked Formula 7",
      textbook_formula = "pi_ann_monthly_t = 1200 * ln(P_t / P_t-1)"
    ),
    pce_yoy_inflation = list(
      topic = "topic_1_understanding_interest_rates",
      formula_label = "FRED-Linked Formula 11",
      textbook_formula = "pi_PCE_yoy_t = 100 * (PCEPI_t / PCEPI_t-12 - 1)"
    ),
    pce_monthly_annualized_inflation = list(
      topic = "topic_6_money_and_inflation",
      formula_label = "FRED-Linked Formula 7",
      textbook_formula = "pi_ann_monthly_t = 1200 * ln(P_t / P_t-1)"
    ),
    real_policy_rate = list(
      topic = "topic_1_understanding_interest_rates",
      formula_label = "FRED-Linked Formula 12",
      textbook_formula = "real_policy_rate_t approx FEDFUNDS_t - pi_t"
    ),
    real_10y_yield = list(
      topic = "topic_1_understanding_interest_rates",
      formula_label = "FRED-Linked Formula 13",
      textbook_formula = "real_10y_t approx DGS10_t - pi_t"
    ),
    term_spread_10y_2y = list(
      topic = "topic_3_risk_and_term_structure",
      formula_label = "FRED-Linked Formula 5",
      textbook_formula = "term_spread_t = DGS10_t - DGS2_t"
    )
  )

  if (!is.null(reference_map[[metric]])) {
    return(reference_map[[metric]])
  }

  list(
    topic = "custom_metric",
    formula_label = "Custom Derived Metric",
    textbook_formula = "No textbook reference mapped."
  )
}

build_demo_evidence_packet = function(question, series_ids, start_date, end_date, limit, top_k, knowledge_result, failure_reason) {
  failure_parts = trimws(unlist(strsplit(failure_reason, "|", fixed = TRUE)))
  failed_series = unique(trimws(sub("\\s*-.*$", "", failure_parts)))
  failed_series = failed_series[nzchar(failed_series)]
  if (length(failed_series) > 0) {
    compact_failure_reason = paste(
      "Network or API access failed for",
      paste(failed_series, collapse = ", "),
      "in this environment."
    )
  } else {
    compact_failure_reason = "Network or API access failed in this environment."
  }

  demo_rows = data.frame(
    series_id = c("FEDFUNDS", "UNRATE", "CPIAUCSL", "DGS10"),
    title = c(
      "Federal Funds Rate",
      "Unemployment Rate",
      "Consumer Price Index for All Urban Consumers",
      "10-Year Treasury Constant Maturity Rate"
    ),
    latest_date = c("2026-02-01", "2026-02-01", "2026-02-01", "2026-02-01"),
    latest_value = c(5.000, 4.100, 320.100, 4.050),
    previous_date = c("2026-01-01", "2026-01-01", "2026-01-01", "2026-01-01"),
    previous_value = c(5.250, 3.900, 319.000, 4.180),
    absolute_change = c(-0.250, 0.200, 1.100, -0.130),
    percent_change = c(-4.76, 5.13, 0.34, -3.11),
    direction = c("down", "up", "up", "down"),
    absolute_formula = c(
      "5.000 - 5.250 = -0.250",
      "4.100 - 3.900 = 0.200",
      "320.100 - 319.000 = 1.100",
      "4.050 - 4.180 = -0.130"
    ),
    percent_formula = c(
      "100 * ( 5.000 / 5.250 - 1) = -4.76%",
      "100 * ( 4.100 / 3.900 - 1) = 5.13%",
      "100 * ( 320.100 / 319.000 - 1) = 0.34%",
      "100 * ( 4.050 / 4.180 - 1) = -3.11%"
    ),
    context_note = c(
      lookup_series_field("FEDFUNDS", "context_note"),
      lookup_series_field("UNRATE", "context_note"),
      lookup_series_field("CPIAUCSL", "context_note"),
      lookup_series_field("DGS10", "context_note")
    ),
    observation_count = c(240, 240, 240, 240),
    stringsAsFactors = FALSE
  )

  demo_rows = demo_rows[demo_rows$series_id %in% series_ids, , drop = FALSE]

  derived_metrics = data.frame(
    metric = c("cpi_yoy_inflation", "cpi_monthly_annualized_inflation", "real_policy_rate", "real_10y_yield", "term_spread_10y_2y"),
    label = c(
      "CPI year-over-year inflation",
      "CPI monthly annualized inflation",
      "Ex post real policy rate",
      "Ex post real 10-year Treasury yield",
      "10-year minus 2-year Treasury spread"
    ),
    latest_date = c("2026-02-01", "2026-02-01", "2026-02-01", "2026-02-01", "2026-02-01"),
    value = c(2.820, 4.130, 2.180, 1.230, -0.150),
    formula = c(
      "100 * ( 320.100 / 311.321 - 1) = 2.82%",
      "1200 * ln( 320.100 / 319.000 ) = 4.13%",
      "5.000 - 2.820 = 2.180",
      "4.050 - 2.820 = 1.230",
      "4.050 - 4.200 = -0.150"
    ),
    meaning = c(
      "Year-over-year inflation proxy from the price index.",
      "Short-run annualized inflation momentum from the latest monthly price change.",
      "Positive values suggest more restrictive inflation-adjusted policy conditions.",
      "Higher real long yields imply tighter long-term financing conditions.",
      "Positive values imply a normal upward-sloping yield curve; negative values imply inversion."
    ),
    source_series = c("CPIAUCSL", "CPIAUCSL", "FEDFUNDS + CPIAUCSL", "DGS10 + CPIAUCSL", "DGS10 + DGS2"),
    stringsAsFactors = FALSE
  )

  keep_metric = c(
    "cpi_yoy_inflation" = "CPIAUCSL" %in% series_ids,
    "cpi_monthly_annualized_inflation" = "CPIAUCSL" %in% series_ids,
    "real_policy_rate" = all(c("FEDFUNDS", "CPIAUCSL") %in% series_ids),
    "real_10y_yield" = all(c("DGS10", "CPIAUCSL") %in% series_ids),
    "term_spread_10y_2y" = "DGS10" %in% series_ids
  )
  derived_metrics = derived_metrics[keep_metric[derived_metrics$metric], , drop = FALSE]

  market_risk = list(
    available = TRUE,
    note = paste(
      "This appendix uses labeled demo fallback values for SP500/VIX market-risk proxies rather than live market data.",
      "It is still an index-based risk proxy, not a portfolio VaR."
    ),
    formula_note = "Formula trace: VaR_95 = max(0, -quantile(r_t, 5%)); VaR_99 = max(0, -quantile(r_t, 1%)); VIX regime heuristic = low < 15, moderate 15-25, high >= 25.",
    lookback_observations = 239L,
    sample_start = "2025-03-18",
    sample_end = "2026-02-27",
    metrics = data.frame(
      metric = c("sp500_hist_var_95", "sp500_hist_var_99", "latest_vix", "vix_regime"),
      label = c(
        "SP500 1-day historical VaR (95%)",
        "SP500 1-day historical VaR (99%)",
        "Latest VIX close",
        "Volatility regime"
      ),
      value = c("2.14%", "3.86%", "19.800", "moderate"),
      formula = c(
        "VaR_95 = max(0, -quantile(r_t, 5%))",
        "VaR_99 = max(0, -quantile(r_t, 1%))",
        "Latest VIXCLS close on 2026-02-27",
        "Heuristic: low < 15, moderate 15-25, high >= 25"
      ),
      meaning = c(
        "Approximate one-day S&P 500 loss threshold from the recent historical return sample.",
        "More extreme tail-loss threshold from the same recent S&P 500 return sample.",
        "Options-implied near-term equity volatility from the CBOE VIX index.",
        "Simple interpretation layer for the latest VIX level; this is a market proxy, not portfolio VaR."
      ),
      stringsAsFactors = FALSE
    ),
    worst_losses = data.frame(
      Date = c("2025-04-04", "2025-10-13", "2025-08-01", "2025-12-18", "2025-06-17"),
      `SP500 Return` = c("-4.22%", "-3.91%", "-3.47%", "-3.10%", "-2.88%"),
      `Loss Magnitude` = c("4.22%", "3.91%", "3.47%", "3.10%", "2.88%"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )

  list(
    question = question,
    data_source = "demo_fallback",
    source_note = paste(
      "Live FRED data could not be fetched, so the tool returned a labeled demo packet.",
      "Reason:",
      compact_failure_reason
    ),
    knowledge_base = basename(KNOWLEDGE_BASE),
    date_window = list(start_date = start_date, end_date = end_date, limit = limit),
    series_requested = series_ids,
    auxiliary_series = unique(c(if ("DGS10" %in% series_ids) "DGS2" else character(0), "SP500", "VIXCLS")),
    live_snapshot = demo_rows,
    derived_metrics = derived_metrics,
    market_risk = market_risk,
    knowledge_matches = knowledge_result$results,
    notes = c(
      "This packet is safe for prompt-testing when the API key or network is unavailable.",
      "Any stakeholder-facing interpretation should mention that the values are demo fallback numbers."
    ),
    top_k = top_k
  )
}
