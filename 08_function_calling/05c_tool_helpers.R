# 05c_tool_helpers.R
# 08 lab custom tool definition and packet-resolution helpers.

build_macro_evidence_packet = function(
  question,
  series_ids = DEFAULT_SERIES,
  start_date = DEFAULT_START_DATE,
  end_date = as.character(Sys.Date()),
  limit = DEFAULT_LIMIT,
  top_k = DEFAULT_TOP_K,
  allow_demo_fallback = TRUE
) {
  series_ids = normalize_series_ids(series_ids)
  auxiliary_series = character(0)
  if ("DGS10" %in% series_ids && !("DGS2" %in% series_ids)) {
    auxiliary_series = c(auxiliary_series, "DGS2")
  }
  auxiliary_series = unique(c(auxiliary_series, "SP500", "VIXCLS"))
  fetch_series_ids = unique(c(series_ids, auxiliary_series))
  limit = as.integer(limit[[1]])
  top_k = as.integer(top_k[[1]])
  allow_demo_fallback = normalize_flag(allow_demo_fallback, default = TRUE)

  knowledge_result = tryCatch(
    search_mishkin_context(question, document_path = KNOWLEDGE_BASE, top_k = top_k),
    error = function(e) {
      list(
        query = question,
        document = basename(KNOWLEDGE_BASE),
        num_sections_considered = 0,
        num_sections_returned = 0,
        results = list(
          list(
            title = "Knowledge base unavailable",
            entry_id = "knowledge_base_error",
            retrieval_label = "knowledge_base_error",
            primary_fred_series = paste(fetch_series_ids, collapse = ", "),
            score = 0,
            matched_terms = character(0),
            preview = conditionMessage(e),
            formulas = character(0),
            matching_content = ""
          )
        )
      )
    }
  )

  api_key = load_fred_key()
  if (!nzchar(api_key)) {
    if (allow_demo_fallback) {
      return(build_demo_evidence_packet(
        question = question,
        series_ids = series_ids,
        start_date = start_date,
        end_date = end_date,
        limit = limit,
        top_k = top_k,
        knowledge_result = knowledge_result,
        failure_reason = "FRED_API_KEY not found in any expected .env location."
      ))
    }
    stop("FRED_API_KEY not found in any expected .env location.")
  }

  series_frames = list()
  fetch_errors = character(0)

  for (series_id in fetch_series_ids) {
    result = tryCatch(
      fetch_fred_observations(
        series_id = series_id,
        start_date = start_date,
        end_date = end_date,
        limit = limit,
        api_key = api_key
      ),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      fetch_errors = c(fetch_errors, paste(series_id, "-", conditionMessage(result)))
    } else {
      series_frames[[series_id]] = result
    }
  }

  if (length(series_frames) == 0) {
    if (allow_demo_fallback) {
      return(build_demo_evidence_packet(
        question = question,
        series_ids = series_ids,
        start_date = start_date,
        end_date = end_date,
        limit = limit,
        top_k = top_k,
        knowledge_result = knowledge_result,
        failure_reason = paste(fetch_errors, collapse = " | ")
      ))
    }
    stop(paste(fetch_errors, collapse = " | "))
  }

  summary_rows = lapply(series_ids[series_ids %in% names(series_frames)], function(series_id) {
    summarize_series(series_id, series_frames[[series_id]])
  })
  summary_df = do.call(rbind, summary_rows)
  full_summary_rows = lapply(names(series_frames), function(series_id) {
    summarize_series(series_id, series_frames[[series_id]])
  })
  full_summary_df = do.call(rbind, full_summary_rows)
  derived_df = compute_derived_metrics(series_frames, full_summary_df)
  market_risk = compute_market_risk_appendix(series_frames)

  list(
    question = question,
    data_source = "live_fred",
    source_note = if (length(fetch_errors) == 0) {
      "Live FRED data fetched successfully."
    } else {
      paste(
        "Live FRED data fetched for the available series, but some requested series failed:",
        paste(fetch_errors, collapse = " | ")
      )
    },
    knowledge_base = basename(KNOWLEDGE_BASE),
    date_window = list(start_date = start_date, end_date = end_date, limit = limit),
    series_requested = series_ids,
    auxiliary_series = auxiliary_series,
    live_snapshot = summary_df,
    derived_metrics = derived_df,
    market_risk = market_risk,
    knowledge_matches = knowledge_result$results,
    failed_series = setdiff(series_ids, names(series_frames)),
    notes = c(
      if (length(fetch_errors) == 0) {
        "All requested series were fetched successfully."
      } else {
        paste("Some requested series could not be fetched:", paste(fetch_errors, collapse = " | "))
      },
      "All deterministic calculations were computed in R before being handed to the model."
    ),
    top_k = top_k
  )
}

summarize_stance_note = function(packet) {
  derived_df = packet$derived_metrics
  if (!is.null(derived_df) && nrow(derived_df) > 0 && any(derived_df$metric == "real_policy_rate")) {
    row = derived_df[derived_df$metric == "real_policy_rate", , drop = FALSE][1, ]
    stance = if (row$value > 1) {
      "looks restrictive on an inflation-adjusted basis"
    } else if (row$value < 0) {
      "looks accommodative on an inflation-adjusted basis"
    } else {
      "looks close to neutral on an inflation-adjusted basis"
    }
    return(paste0("Real policy rate = ", format_num(row$value), ", so policy ", stance, "."))
  }

  "Use the sign and size of the real-rate metrics before making a policy-stance claim."
}

is_valid_evidence_packet = function(packet) {
  is.list(packet) &&
    !isTRUE(packet$error) &&
    "data_source" %in% names(packet) &&
    "live_snapshot" %in% names(packet) &&
    "derived_metrics" %in% names(packet) &&
    is.data.frame(packet$live_snapshot) &&
    is.data.frame(packet$derived_metrics)
}

matches_requested_configuration = function(packet, series_ids, start_date, end_date) {
  identical(as.character(packet$series_requested), as.character(series_ids)) &&
    identical(as.character(packet$date_window$start_date), as.character(start_date)) &&
    identical(as.character(packet$date_window$end_date), as.character(end_date))
}

resolve_agent1_packet = function(tool_calls, question) {
  if (
    length(tool_calls) > 0 &&
      is_valid_evidence_packet(tool_calls[[1]]$output) &&
      matches_requested_configuration(
        tool_calls[[1]]$output,
        series_ids = DEFAULT_SERIES,
        start_date = DEFAULT_START_DATE,
        end_date = TODAY
      )
  ) {
    return(tool_calls[[1]]$output)
  }

  build_macro_evidence_packet(
    question = question,
    series_ids = DEFAULT_SERIES,
    start_date = DEFAULT_START_DATE,
    end_date = TODAY,
    limit = DEFAULT_LIMIT,
    top_k = DEFAULT_TOP_K,
    allow_demo_fallback = TRUE
  )
}

tool_build_macro_evidence_packet = list(
  type = "function",
  "function" = list(
    name = "build_macro_evidence_packet",
    description = paste(
      "Fetch live U.S. macro data from FRED, compute deterministic change metrics and formula traces,",
      "retrieve relevant Mishkin textbook formula context from the local knowledge base,",
      "and add a small SP500/VIX market-risk appendix before returning a structured evidence packet."
    ),
    parameters = list(
      type = "object",
      required = list("question", "series_ids", "start_date", "end_date", "limit", "top_k"),
      properties = list(
        question = list(
          type = "string",
          description = "The user's macroeconomic question or briefing request."
        ),
        series_ids = list(
          type = "array",
          description = "FRED series codes to include. Recommended macro options: FEDFUNDS, EFFR, UNRATE, CPIAUCSL, PCEPI, DGS10, DGS2. Optional market-risk series include SP500 and VIXCLS.",
          items = list(type = "string")
        ),
        start_date = list(
          type = "string",
          description = "Observation start date in YYYY-MM-DD format."
        ),
        end_date = list(
          type = "string",
          description = "Observation end date in YYYY-MM-DD format."
        ),
        limit = list(
          type = "integer",
          description = "Maximum number of observations per series."
        ),
        top_k = list(
          type = "integer",
          description = "Number of Mishkin knowledge-base sections to retrieve."
        ),
        allow_demo_fallback = list(
          type = "boolean",
          description = "Whether to return a clearly labeled demo packet if live FRED data cannot be fetched."
        )
      )
    )
  )
)
