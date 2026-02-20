library(httr2)
library(jsonlite)

# 1) Read API key from .env
readRenviron('.env')
api_key <- Sys.getenv('FRED_API_KEY')

cat('FRED_API_KEY found:', nzchar(api_key), '\n')
if (!nzchar(api_key)) {
  stop('Error: FRED_API_KEY not found in .env file!\nPlease add: FRED_API_KEY=your_key_here')
}

# 2) Build and run request
cat('Making FRED API request...\n')
resp <- request('https://api.stlouisfed.org/fred/series/observations') |>
  req_url_query(
    series_id = 'FEDFUNDS',
    api_key = api_key,
    file_type = 'json',
    limit = 20,
    sort_order = 'desc'
  ) |>
  req_perform()

cat('Status:', resp$status_code, '\n')
if (resp$status_code != 200) {
  cat('Response body:', resp_body_string(resp), '\n')
  stop('FRED API request failed with status code: ', resp$status_code)
}

# 3) Parse JSON
cat('Parsing JSON response...\n')
obj <- resp_body_json(resp)

if (is.null(obj$observations) || length(obj$observations) == 0) {
  stop('No observations returned from FRED API query.')
}

# 4) Convert to data frame
cat('Creating data frame...\n')
df <- data.frame(
  date = vapply(obj$observations, function(x) x$date, character(1)),
  value = vapply(obj$observations, function(x) x$value, character(1)),
  realtime_start = vapply(obj$observations, function(x) x$realtime_start, character(1)),
  realtime_end = vapply(obj$observations, function(x) x$realtime_end, character(1)),
  stringsAsFactors = FALSE
)

df$value <- suppressWarnings(as.numeric(df$value))

cat('Number of records returned:', nrow(df), '\n')
cat('Fields:', paste(colnames(df), collapse = ', '), '\n')

# 5) Print first 15 rows for lab evidence
print(df[1:min(15, nrow(df)), ], row.names = FALSE)