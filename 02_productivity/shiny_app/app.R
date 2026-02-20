library(shiny)
library(httr2)
library(DT)
library(ggplot2)

load_fred_key <- function() {
  env_candidates <- c(".env", "../.env", "../../.env", "../../../.env", "dsai/01_query_api/.env")
  for (f in env_candidates) {
    if (file.exists(f)) {
      readRenviron(f)
      key <- Sys.getenv("FRED_API_KEY")
      if (nzchar(key)) {
        return(key)
      }
    }
  }
  Sys.getenv("FRED_API_KEY")
}

fetch_fred_observations <- function(series_id, start_date, end_date, limit = 200) {
  key <- load_fred_key()
  if (!nzchar(key)) {
    stop("FRED_API_KEY not found. Add FRED_API_KEY=... to a .env file.")
  }

  resp <- request("https://api.stlouisfed.org/fred/series/observations") |>
    req_url_query(
      series_id = series_id,
      observation_start = start_date,
      observation_end = end_date,
      sort_order = "asc",
      limit = limit,
      file_type = "json",
      api_key = key
    ) |>
    req_timeout(20) |>
    req_perform()

  if (resp$status_code != 200) {
    stop(paste("FRED request failed with status", resp$status_code, "\n", resp_body_string(resp)))
  }

  obj <- resp_body_json(resp)
  if (is.null(obj$observations) || length(obj$observations) == 0) {
    stop("No observations returned for this query.")
  }

  df <- data.frame(
    date = as.Date(vapply(obj$observations, function(x) x$date, character(1))),
    value_raw = vapply(obj$observations, function(x) x$value, character(1)),
    stringsAsFactors = FALSE
  )

  df$value <- suppressWarnings(as.numeric(df$value_raw))
  df <- df[!is.na(df$value), c("date", "value")]
  if (nrow(df) == 0) {
    stop("Returned observations are not numeric.")
  }

  df
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n      body { background: #f5f7fb; }\n      .card { background: white; border-radius: 14px; padding: 16px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); }\n      .title { font-weight: 700; margin-bottom: 12px; }\n      .metric { font-size: 26px; font-weight: 700; color: #0d6efd; }\n      .subtle { color: #6c757d; }\n    "))
  ),
  titlePanel("FRED Macro Dashboard (Lab 2)"),
  fluidRow(
    column(
      width = 4,
      div(
        class = "card",
        h4("Query Controls", class = "title"),
        selectInput(
          "series_id",
          "FRED Series",
          choices = c(
            "FEDFUNDS - Federal Funds Rate" = "FEDFUNDS",
            "UNRATE - Unemployment Rate" = "UNRATE",
            "CPIAUCSL - CPI" = "CPIAUCSL",
            "DGS10 - 10Y Treasury Yield" = "DGS10"
          ),
          selected = "FEDFUNDS"
        ),
        dateInput("start_date", "Start Date", value = Sys.Date() - 3650),
        dateInput("end_date", "End Date", value = Sys.Date()),
        numericInput("limit", "Max rows", value = 300, min = 20, max = 1000, step = 10),
        numericInput("shock_bp", "Scenario Shock (bp)", value = 50, min = -500, max = 500, step = 5),
        actionButton("run", "Run Query", class = "btn-primary", style = "width:100%;"),
        br(), br(),
        p("This app fetches macro data from FRED and runs a simple scenario shock on the latest value.", class = "subtle")
      )
    ),
    column(
      width = 8,
      div(
        class = "card",
        h4("Status", class = "title"),
        verbatimTextOutput("status"),
        fluidRow(
          column(6, h5("Latest Value"), div(class = "metric", textOutput("latest_value"))),
          column(6, h5("Shocked Value"), div(class = "metric", textOutput("shocked_value")))
        ),
        br(),
        plotOutput("series_plot", height = "300px"),
        br(),
        DTOutput("series_table")
      )
    )
  )
)

server <- function(input, output, session) {
  queried <- eventReactive(input$run, {
    tryCatch(
      {
        df <- fetch_fred_observations(
          series_id = input$series_id,
          start_date = as.character(input$start_date),
          end_date = as.character(input$end_date),
          limit = input$limit
        )
        list(ok = TRUE, data = df, error = NULL)
      },
      error = function(e) list(ok = FALSE, data = NULL, error = conditionMessage(e))
    )
  }, ignoreNULL = FALSE)

  output$status <- renderText({
    res <- queried()
    if (!res$ok) {
      paste("Error:\n", res$error)
    } else {
      paste("Success. Rows:", nrow(res$data), "Series:", input$series_id)
    }
  })

  output$latest_value <- renderText({
    res <- queried()
    if (!res$ok) return("-")
    sprintf("%.3f", tail(res$data$value, 1))
  })

  output$shocked_value <- renderText({
    res <- queried()
    if (!res$ok) return("-")
    latest <- tail(res$data$value, 1)
    shocked <- latest + input$shock_bp / 100
    sprintf("%.3f", shocked)
  })

  output$series_plot <- renderPlot({
    res <- queried()
    req(res$ok)
    ggplot(res$data, aes(x = date, y = value)) +
      geom_line(color = "#0d6efd", linewidth = 1) +
      geom_point(color = "#0d6efd", size = 1, alpha = 0.7) +
      theme_minimal(base_size = 12) +
      labs(x = "Date", y = "Value", title = paste("FRED Series:", input$series_id))
  })

  output$series_table <- renderDT({
    res <- queried()
    req(res$ok)
    datatable(
      res$data,
      options = list(pageLength = 10, scrollX = TRUE, order = list(list(0, "desc"))),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)