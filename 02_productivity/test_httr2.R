library(httr2)

resp <- request("https://api.github.com/users/octocat") |>
  req_perform()

# Check status
resp_status(resp)
resp_status_desc(resp)

# Parse JSON body
user_data <- resp |>
  resp_body_json()

# Look at the result
str(user_data)