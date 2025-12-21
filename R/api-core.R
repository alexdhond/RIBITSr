#' @keywords internal
rb_base_url <- function() {
  "https://ribits.ops.usace.army.mil/ords/RI/public/"
}

#' Make a request to the RIBITS API
#'
#' @param endpoint The API endpoint (e.g., "bank_site_list/")
#' @param params A named list of query parameters
#'
#' @return A httr2 response object
#' @keywords internal
rb_request <- function(endpoint, params = list()) {
  url <- paste0(rb_base_url(), endpoint)
  
  req <- httr2::request(url) |>
    httr2::req_user_agent("RIBITSr R package")
    
  if (length(params) > 0) {
    # The API expects filters in a 'q' parameter as a JSON string
    q_json <- jsonlite::toJSON(params, auto_unbox = TRUE)
    req <- req |> httr2::req_url_query(q = q_json)
  }
    
  httr2::req_perform(req)
}

#' Parse RIBITS API response
#' 
#' @param resp A httr2 response object
#' @return A tibble or list depending on the endpoint
#' @keywords internal
rb_parse_response <- function(resp) {
  httr2::resp_check_status(resp)
  
  # The API seems to return JSON.
  # We use jsonlite to parse it.
  jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE)
}
