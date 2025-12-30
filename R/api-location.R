# R/api-location.R
# Location-based queries for banks and programs
# Split from R/unified-api.R (627 lines → focused modules)

# =============================================================================
# UNIFIED LOCATION-BASED QUERY
# =============================================================================

#' Find banks or programs by location
#'
#' Unified function to find mitigation banks or ILF programs near a location.
#'
#' @param lat Latitude (decimal degrees)
#' @param lon Longitude (decimal degrees)
#' @param type What to search for: "banks" or "programs". Default "banks".
#' @return A tibble of results near the location
#' @keywords internal
#' @examples
#' \dontrun{
#' # Find banks near Portland, OR
#' banks <- rb_near(lat = 45.5152, lon = -122.6784)
#'
#' # Find ILF programs near same location
#' programs <- rb_near(lat = 45.5152, lon = -122.6784, type = "programs")
#' }
rb_near <- function(lat, lon, type = c("banks", "programs")) {
  type <- match.arg(type)
  
  if (type == "banks") {
    rb_banks_by_location(lat = lat, lon = lon)
  } else {
    rb_programs_by_location(lat = lat, lon = lon)
  }
}

#' Find banks by location
#'
#' Queries the RIBITS API to find mitigation banks near a given latitude/longitude.
#'
#' @param lat Latitude (decimal degrees)
#' @param lon Longitude (decimal degrees)
#' @return A tibble of banks near the location
#' @keywords internal
rb_banks_by_location <- function(lat, lon) {
  if (!is.numeric(lat) || !is.numeric(lon)) {
    cli::cli_abort("lat and lon must be numeric")
  }
  
  url <- paste0(
    "https://ribits.ops.usace.army.mil/ords/RI/public/getmitbanksbylocation_47/",
    "?param0=", lon, "&param1=", lat
 )
  
  cli::cli_alert_info("Querying banks near ({lat}, {lon})...")
  
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()
    
    data <- httr2::resp_body_json(resp)
    
    if (length(data) == 0) {
      cli::cli_alert_info("No banks found near this location")
      return(tibble::tibble())
    }
    
    result <- purrr::map_dfr(data, tibble::as_tibble)
    # Note: We preserve original column names here - clean_names() is applied at final output
    
    cli::cli_alert_success("Found {nrow(result)} banks")
    result
  }, error = function(e) {
    cli::cli_alert_warning("Query failed: {e$message}")
    tibble::tibble()
  })
}


#' Find ILF programs by location
#'
#' Queries the RIBITS API to find ILF programs near a given latitude/longitude.
#'
#' @param lat Latitude (decimal degrees)
#' @param lon Longitude (decimal degrees)
#' @return A tibble of ILF programs near the location
#' @keywords internal
rb_programs_by_location <- function(lat, lon) {
  if (!is.numeric(lat) || !is.numeric(lon)) {
    cli::cli_abort("lat and lon must be numeric")
  }
  
  url <- paste0(
    "https://ribits.ops.usace.army.mil/ords/RI/public/getprogramsbylocation_47/",
    "?param0=", lon, "&param1=", lat
  )
  
  cli::cli_alert_info("Querying ILF programs near ({lat}, {lon})...")
  
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()
    
    data <- httr2::resp_body_json(resp)
    
    if (length(data) == 0) {
      cli::cli_alert_info("No ILF programs found near this location")
      return(tibble::tibble())
    }
    
    result <- purrr::map_dfr(data, tibble::as_tibble)
    # Note: We preserve original column names here - clean_names() is applied at final output
    
    cli::cli_alert_success("Found {nrow(result)} ILF programs")
    result
  }, error = function(e) {
    cli::cli_alert_warning("Query failed: {e$message}")
    tibble::tibble()
  })
}


