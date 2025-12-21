# Core API functionality for RIBITS
# 
# This file contains the foundational functions for making requests
# to the USACE RIBITS REST API.

#' Base URL for RIBITS API
#' @noRd
.rb_base_url <- function() {
  "https://ribits.ops.usace.army.mil/ords/RI/public/"
}

#' Make a request to the RIBITS API
#'
#' Low-level function for making requests to RIBITS endpoints.
#' Handles URL construction, rate limiting, and response parsing.
#'
#' @param endpoint Character. The API endpoint (e.g., "bank_site_list").
#' @param params Named list. Parameters to include in the query.
#' @param email Character. Optional email for usage tracking.
#'
#' @return Parsed JSON response as a list.
#' @noRd
.rb_request <- function(endpoint, params = list(), email = NULL) {
  # Add email tracking if provided
  if (!is.null(email)) {
    params$webconsumer_email <- email
  }
  
  # Build URL
  url <- paste0(.rb_base_url(), endpoint, "/")
  
  # Create request
  req <- httr2::request(url)
  
  # Add JSON query parameters if any
  if (length(params) > 0) {
    query_json <- jsonlite::toJSON(params, auto_unbox = TRUE)
    req <- httr2::req_url_query(req, q = query_json)
  }
  
  # Configure request with rate limiting and retries
  req <- req |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503)
    ) |>
    httr2::req_throttle(rate = 2)  # Max 2 requests per second
  
  # Perform request
  resp <- httr2::req_perform(req)
  
  # Check for errors
  if (httr2::resp_status(resp) != 200) {
    rlang::abort(
      c(
        "RIBITS API request failed",
        "x" = paste0("Status: ", httr2::resp_status(resp)),
        "i" = paste0("Endpoint: ", endpoint)
      ),
      class = "ribits_error_api"
    )
  }
  
  # Parse response
  httr2::resp_body_json(resp)
}

#' Extract items from RIBITS API response
#'
#' RIBITS API responses contain an ITEMS array. This function
#' extracts and validates that array.
#'
#' @param response List. Parsed API response from .rb_request().
#' @param expected_single Logical. If TRUE, expect exactly one item.
#'
#' @return List of items, or single item if expected_single = TRUE.
#' @noRd
.rb_extract_items <- function(response, expected_single = FALSE) {
  items <- response$ITEMS
  
  if (is.null(items)) {
    rlang::abort(
      c(
        "Unexpected API response format",
        "x" = "Response does not contain ITEMS array"
      ),
      class = "ribits_error_response"
    )
  }
  
  if (expected_single) {
    if (length(items) == 0) {
      rlang::abort(
        "No data returned from API",
        class = "ribits_error_empty"
      )
    }
    return(items[[1]])
  }
  
  items
}

#' Convert list of items to tibble
#'
#' Converts a list of API response items to a tibble, handling
#' NULL values and nested structures appropriately.
#'
#' @param items List. Items from .rb_extract_items().
#' @param flat_only Logical. If TRUE, only include non-list columns.
#'
#' @return A tibble.
#' @noRd
.rb_items_to_tibble <- function(items, flat_only = TRUE) {
  if (length(items) == 0) {
    return(tibble::tibble())
  }
  
  # Convert each item to a one-row tibble
  rows <- purrr::map(items, function(item) {
    # Filter out list columns if requested
    if (flat_only) {
      item <- purrr::keep(item, \(x) !is.list(x))
    }
    
    # Convert NULLs to NA
    item <- purrr::map(item, \(x) if (is.null(x)) NA else x)
    
    tibble::as_tibble(item)
  })
  
  # Bind all rows
  dplyr::bind_rows(rows)
}

#' Build filter parameters for list endpoints
#'
#' Constructs the parameter list for list endpoints,
#' removing NULL values and validating inputs.
#'
#' @param kind Character. Bank type filter.
#' @param district Character. USACE district filter.
#' @param state Character. State abbreviation filter.
#' @param noaaregion Character. NMFS region filter.
#' @param fieldoffice Character. FWS field office filter.
#'
#' @return Named list of non-NULL parameters.
#' @noRd
.rb_build_list_params <- function(kind = NULL,
                                   district = NULL,
                                   state = NULL,
                                   noaaregion = NULL,
                                   fieldoffice = NULL) {
  params <- list(
    kind_of_bank = kind,
    district = district,
    state = state,
    noaaregion = noaaregion,
    fieldoffice = fieldoffice
  )
  
  # Remove NULLs
  purrr::compact(params)
}

#' Build show parameters for detail endpoints
#'
#' Constructs the show_* parameters for detail endpoints.
#'
#' @param service_area Logical. Include service area?
#' @param footprint Logical. Include footprint?
#' @param contacts Logical. Include contacts?
#' @param ledger Logical. Include ledger?
#'
#' @return Named list of show parameters.
#' @noRd
.rb_build_show_params <- function(service_area = TRUE,
                                   footprint = TRUE,
                                   contacts = TRUE,
                                   ledger = TRUE) {
  params <- list()
  
  if (service_area) params$show_service_area <- "Yes"
  if (footprint) params$show_footprint <- "Yes"
  if (contacts) params$show_contacts <- "Yes"
  if (ledger) params$show_ledger <- "Yes"
  
  params
}

#' Get or set RIBITS email for API tracking
#'
#' RIBITS requests that regular API consumers include their email
#' for usage tracking and notification of updates.
#'
#' @param email Character. Email address to set, or NULL to get current.
#'
#' @return If getting, returns the current email or NULL.
#'   If setting, returns the previous value invisibly.
#'
#' @export
#' @examples
#' # Set email for API tracking
#' rb_email("your.email@example.com")
#'
#' # Get current email
#' rb_email()
#'
#' # Clear email
#' rb_email(NULL)
rb_email <- function(email) {
  if (missing(email)) {
    return(getOption("ribits.email"))
  }
  
  old <- getOption("ribits.email")
  options(ribits.email = email)
  
  if (!is.null(email)) {
    cli::cli_alert_success("RIBITS email set to {.email {email}}")
  } else {
    cli::cli_alert_info("RIBITS email cleared")
  }
  
  invisible(old)
}

#' Check API connection
#'
#' Tests connectivity to the RIBITS API by making a minimal request.
#'
#' @return TRUE if connection successful, FALSE otherwise.
#'   Prints diagnostic information.
#'
#' @export
#' @examples
#' \dontrun{
#' rb_check_connection()
#' }
rb_check_connection <- function() {
  cli::cli_alert_info("Testing RIBITS API connection...")
  
  tryCatch({
    response <- .rb_request("bank_site_list", params = list(state = "DC"))
    items <- response$ITEMS
    
    cli::cli_alert_success(
      "Connection successful! Found {length(items)} banks in DC."
    )
    
    invisible(TRUE)
  }, error = function(e) {
    cli::cli_alert_danger("Connection failed: {e$message}")
    invisible(FALSE)
  })
}
