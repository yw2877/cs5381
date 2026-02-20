library(httr2)

# 1) Load API key from .env
readRenviron('.env')
API_KEY <- Sys.getenv('FRED_API_KEY')

cat('FRED_API_KEY found:', nzchar(API_KEY), 'Length:', nchar(API_KEY), '\n')
if (!nzchar(API_KEY)) {
  stop('Error: FRED_API_KEY not found!\nPlease create .env file and add: FRED_API_KEY=your_api_key')
}

# 2) GET request to FRED (returns multi-row time series)
resp <- request('https://api.stlouisfed.org/fred/series/observations') |>
  req_url_query(
    series_id = 'FEDFUNDS',
    api_key = API_KEY,
    file_type = 'json',
    limit = 20,
    sort_order = 'desc'
  ) |>
  req_method('GET') |>
  req_perform()

# 3) Verify status
cat('Status:', resp$status_code, '\n')
stopifnot(resp$status_code == 200)

# 4) Parse and extract data
obj <- resp_body_json(resp)
stopifnot(!is.null(obj$observations), length(obj$observations) >= 10)

df <- data.frame(
  date = vapply(obj$observations, function(x) x$date, character(1)),
  value = vapply(obj$observations, function(x) x$value, character(1)),
  stringsAsFactors = FALSE
)

cat('Rows extracted:', nrow(df), '\n')
print(df[1:min(10, nrow(df)), ], row.names = FALSE)
