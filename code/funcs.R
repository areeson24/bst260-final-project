get_cdc_data <- function(url, limit = 10000000) {
  require(httr2)
  raw_data <- request(url) |> 
    req_url_query("$limit" = limit) |>
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
  return(raw_data)
}