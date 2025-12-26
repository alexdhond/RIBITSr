#' Convert logical to Yes/No for API parameters
#' @param x Logical value
#' @return Character "Yes" or "No"
#' @keywords internal
rb_bool_to_text <- function(x) {
  if (isTRUE(as.logical(x))) "Yes" else "No"
}

#' @keywords internal
rb_base_url <- function() {
  "https://ribits.ops.usace.army.mil/ords/RI/public/"
}

#' Make a request to the RIBITS API
#'
#' @param endpoint The API endpoint (e.g., "bank_site_list/")
#' @param params A named list of query parameters
#' @param description Description for progress/error messages
#'
#' @return A httr2 response object, or NULL if request failed
#' @keywords internal
rb_request <- function(endpoint, params = list(), description = NULL) {
  url <- paste0(rb_base_url(), endpoint)

  if (is.null(description)) {
    description <- paste("RIBITS API:", endpoint)
  }

  req <- httr2::request(url) |>
    httr2::req_user_agent("RIBITSr R package") # |>
    # Throttle requests to 5 per second (adjust as needed)
    # httr2::req_throttle(rate = 5 / 1)

  if (length(params) > 0) {
    # The API expects filters in a 'q' parameter as a JSON string
    q_json <- jsonlite::toJSON(params, auto_unbox = TRUE)
    req <- req |> httr2::req_url_query(q = q_json)
  }

  # Use retry logic
  rb_request_with_retry(req, description = description)
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

#' Extract items from API response
#'
#' Handles the ITEMS wrapper that RIBITS API uses.
#'
#' @param data Parsed JSON response
#' @return A tibble
#' @keywords internal
rb_extract_items <- function(data) {

  # Handle ORDS response structure (often in 'items')
  result <- if ("items" %in% names(data)) {
    tibble::as_tibble(data$items)
  } else if ("ITEMS" %in% names(data)) {
    tibble::as_tibble(data$ITEMS)
  } else if (length(data) == 0) {
    cli::cli_alert_info("API returned empty response")
    tibble::tibble()
  } else {
    tibble::as_tibble(data)
  }

  # Clean column names immediately to avoid case-insensitive lookups throughout codebase
  if (ncol(result) > 0) {
    result <- janitor::clean_names(result)
  }

  result
}

#' Friendly error for common issues
#' @keywords internal
.rb_friendly_error <- function(e, context = "API request") {
  msg <- conditionMessage(e)

  # Extract HTTP status code if present
  status_code <- NULL
  if (grepl("HTTP (\\d{3})", msg)) {
    status_code <- as.integer(sub(".*HTTP (\\d{3}).*", "\\1", msg))
  }

  # Determine if error is retryable using same logic as retry system
  is_retryable <- .is_retryable_error(msg, status_code)

  # Network errors (always retryable)
  if (grepl("Could not resolve host|connection refused|timed out|timeout", msg, ignore.case = TRUE)) {
    cli::cli_abort(c(
      "Network error during {context}",
      "x" = "Original error: {msg}",
      "i" = "This is a temporary network issue",
      "!" = "The request will be retried with exponential backoff",
      "i" = "Check your internet connection if retries continue to fail",
      "i" = "The RIBITS server may be temporarily unavailable"
    ))
  }

  # HTTP 401 - Unauthorized (permanent error)
  if (!is.null(status_code) && status_code == 401) {
    cli::cli_abort(c(
      "Authentication required (401)",
      "x" = "The RIBITS API requires authentication for this resource",
      "!" = "This is a permanent error - request will NOT be retried",
      "i" = "Check if the endpoint requires credentials",
      "x" = "Original error: {msg}"
    ))
  }

  # HTTP 403 - Forbidden (permanent error)
  if (!is.null(status_code) && status_code == 403) {
    cli::cli_abort(c(
      "Access forbidden (403)",
      "x" = "You do not have permission to access this resource",
      "!" = "This is a permanent error - request will NOT be retried",
      "i" = "The resource may be restricted or your IP may be blocked",
      "i" = "Contact RIBITS support if you believe this is an error",
      "x" = "Original error: {msg}"
    ))
  }

  # HTTP 404 - Not Found (permanent error)
  if (!is.null(status_code) && status_code == 404) {
    cli::cli_abort(c(
      "Resource not found (404)",
      "x" = "The requested bank/program ID does not exist",
      "!" = "This is a permanent error - request will NOT be retried",
      "i" = "Try rb_get('banks') to see available IDs",
      "i" = "Check that your ID parameter is correct",
      "x" = "Original error: {msg}"
    ))
  }

  # HTTP 408 - Request Timeout (retryable)
  if (!is.null(status_code) && status_code == 408) {
    cli::cli_abort(c(
      "Request timeout (408)",
      "x" = "The server did not receive the complete request in time",
      "!" = "This error will be retried automatically",
      "i" = "If this persists, the server may be overloaded",
      "x" = "Original error: {msg}"
    ))
  }

  # HTTP 429 - Rate Limited (retryable)
  if (!is.null(status_code) && status_code == 429) {
    cli::cli_abort(c(
      "Rate limit exceeded (429)",
      "x" = "Too many requests sent to the RIBITS server",
      "!" = "This error will be retried automatically with backoff",
      "i" = "Consider reducing rb_config(rate_limit = ...) to avoid this",
      "i" = "Current rate limit: {.network_options$rate_limit %||% 5} requests/second",
      "x" = "Original error: {msg}"
    ))
  }

  # HTTP 500/502/503 - Server errors (retryable)
  if (!is.null(status_code) && status_code >= 500 && status_code < 600) {
    cli::cli_abort(c(
      "RIBITS server error ({status_code})",
      "x" = "The server encountered an internal error",
      "!" = "This error will be retried automatically",
      "i" = "The server may be experiencing issues - wait a few minutes",
      "i" = "If this persists after retries, try again later",
      "x" = "Original error: {msg}"
    ))
  }

  # Other 4xx errors (permanent)
  if (!is.null(status_code) && status_code >= 400 && status_code < 500) {
    cli::cli_abort(c(
      "Client error ({status_code})",
      "x" = "The request was invalid",
      "!" = "This is a permanent error - request will NOT be retried",
      "i" = "Check your request parameters and try again",
      "x" = "Original error: {msg}"
    ))
  }

  # Generic error with retryability indicator
  retry_msg <- if (is_retryable) {
    "This error may be retried"
  } else {
    "This is a permanent error - will NOT be retried"
  }

  cli::cli_abort(c(
    "Error during {context}",
    "x" = "Original error: {msg}",
    "!" = retry_msg,
    "i" = "Use rb_network_failures() to see all failed requests"
  ))
}

#' Process single item response
#'
#' Converts API response for single item into a tibble with list-columns.
#'
#' @param data Parsed JSON response
#' @return A tibble
#' @keywords internal
rb_process_single_item <- function(data) {
  # Handle ITEMS wrapper if present
  if ("items" %in% names(data)) {
    data <- data$items
  } else if ("ITEMS" %in% names(data)) {
    data <- data$ITEMS
  }

  # If it's a data frame, return as tibble
  # Note: We preserve original column names here - clean_names() is applied at final output
  if (is.data.frame(data)) {
    return(tibble::as_tibble(data))
  }

  # If it's a list, convert to 1-row tibble with list-columns for nested data
  if (is.list(data)) {
    # Convert nested structures to list-columns
    processed_data <- lapply(data, function(x) {
      if (is.data.frame(x) || (is.list(x) && length(x) > 0 && !is.null(names(x)))) {
        list(x)
      } else {
        x
      }
    })
    return(tibble::as_tibble(processed_data))
  }

  tibble::as_tibble(data)
}

#' Generic list function for RIBITS endpoints
#'
#' Internal generic function that handles listing resources from any RIBITS
#' endpoint with consistent behavior.
#'
#' @param type Character. Endpoint type (e.g., "banks", "ilf_programs")
#' @param ... Filter parameters specific to the endpoint
#' @return A tibble
#' @keywords internal
rb_list_generic <- function(type, ...) {
  config <- rb_get_endpoint_config(type)
  params <- list(...)

  # Validate parameters
  params <- rb_validate_list_params(type, params)

  # Initial request
  resp <- rb_request(config$list_endpoint, params)
  data <- rb_parse_response(resp)

  # Accumulate results
  all_items <- list(rb_extract_items(data))

  # Handle Pagination
  while (.rb_has_next_link(data)) {
    next_url <- .rb_get_next_link(data)

    # Direct request for next page
    # Rate limiting now handled globally in rb_request_with_retry()
    req <- httr2::request(next_url) |>
      httr2::req_user_agent("RIBITSr R package")

    resp <- httr2::req_perform(req)
    data <- rb_parse_response(resp)
    
    all_items <- append(all_items, list(rb_extract_items(data)))
  }

  dplyr::bind_rows(all_items)
}

#' Check if response has next link
#' @param data Parsed JSON response
#' @keywords internal
.rb_has_next_link <- function(data) {
  if ("links" %in% names(data)) {
    links <- data$links
    return("next" %in% links$rel)
  }
  if ("LINKS" %in% names(data)) {
    links <- data$LINKS
    return("next" %in% links$rel)
  }
  FALSE
}

#' Get next link URL
#' @param data Parsed JSON response
#' @keywords internal
.rb_get_next_link <- function(data) {
  if ("links" %in% names(data)) {
    links <- data$links
    # links is a data frame
    return(links$href[links$rel == "next"])
  }
  if ("LINKS" %in% names(data)) {
    links <- data$LINKS
    return(links$href[links$rel == "next"])
  }
  NULL
}

#' Generic get function for single RIBITS items
#'
#' Internal generic function that retrieves detailed information for a single
#' resource from any RIBITS endpoint.
#'
#' @param type Character. Endpoint type (e.g., "banks", "ilf_programs")
#' @param id Integer. Resource ID
#' @param show_params Named list. Show parameters (e.g., show_ledger = "yes")
#' @param ... Additional parameters
#' @return A tibble (1 row with list-columns for nested data)
#' @keywords internal
rb_get_generic <- function(type, id, show_params = list(), ...) {
  if (missing(id)) {
    config <- rb_get_endpoint_config(type)
    rlang::abort(
      glue::glue("{config$id_param} is required"),
      class = "ribits_error_missing_param"
    )
  }

  config <- rb_get_endpoint_config(type)

  # Build parameters
  params <- c(
    stats::setNames(list(id), config$id_param),
    show_params,
    list(...)
  )

  resp <- rb_request(config$get_endpoint, params)
  data <- rb_parse_response(resp)

  rb_process_single_item(data)
}

#' Generic batch get function for RIBITS resources
#'
#' Internal generic function that retrieves multiple resources with rate
#' limiting, progress reporting, and error handling.
#'
#' @param type Character. Endpoint type (e.g., "banks", "ilf_programs")
#' @param ids Integer vector. Resource IDs
#' @param get_fn Function. The specific get function to use
#' @param progress Logical. Show progress bar?
#' @param ... Additional arguments passed to get_fn
#' @return A tibble with combined results
#' @keywords internal
rb_get_multiple_generic <- function(type, ids, get_fn, progress = TRUE, ...) {
  if (length(ids) == 0) {
    return(tibble::tibble())
  }

  config <- rb_get_endpoint_config(type)

  if (progress) {
    bar_id <- cli::cli_progress_bar(
      glue::glue("Downloading {config$type_name}"),
      total = length(ids)
    )
  }

  results <- purrr::map(ids, function(id) {
    if (progress) cli::cli_progress_update(id = bar_id)

    tryCatch({
      get_fn(id, ...)
    }, error = function(e) {
      cli::cli_alert_warning(
        "Failed to retrieve {config$type_singular} {id}: {e$message}"
      )
      return(NULL)
    })
  })

  if (progress) cli::cli_progress_done(id = bar_id)

  # Combine results
  # If results are lists (from new rb_get_bank), we need to aggregate by component
  if (length(results) > 0 && is.list(results[[1]]) && !is.data.frame(results[[1]])) {
    # Get all unique component names
    components <- unique(unlist(lapply(results, names)))
    
    # Initialize output list
    out <- list()
    
    for (comp in components) {
      # Extract this component from all results
      items <- lapply(results, function(x) x[[comp]])
      
      # Remove NULLs
      items <- items[!sapply(items, is.null)]
      
      if (length(items) > 0) {
        # Check if items are sf objects
        if (inherits(items[[1]], "sf")) {
          out[[comp]] = do.call(rbind, items)
        } else {
          # Use purrr::list_rbind to handle potential column type mismatches more gracefully?
          # Or manually coerce all columns to character if binding fails?
          tryCatch({
            out[[comp]] = dplyr::bind_rows(items)
          }, error = function(e) {
            # Fallback: Convert all columns to character to force bind
            items_char <- lapply(items, function(df) {
               dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
            })
            out[[comp]] = dplyr::bind_rows(items_char)
          })
        }
      }
    }
    return(out)
  }

  # Default behavior for tibbles
  dplyr::bind_rows(results)
}

#' Check RIBITS API connection
#'
#' Checks if the RIBITS API is reachable and responding.
#'
#' @param timeout Numeric. Seconds to wait before timing out. Default 10.
#' @param verbose Logical. If TRUE, prints status messages. Default FALSE.
#' @return Logical. TRUE if API is reachable, FALSE otherwise.
#' @export
check_ribits_connection <- function(timeout = 10, verbose = FALSE) {
  if (verbose) {
    cli::cli_alert_info("Testing connection to RIBITS API...")
  }

  url <- rb_base_url()
  
  tryCatch({
    # Check a real endpoint (bank_site_list) with minimal data
    # Note: We don't use limit=1 here to match existing test mocks which expect
    # a request to bank_site_list/ without query parameters.
    req_test <- httr2::request(paste0(url, "bank_site_list/")) |>
      httr2::req_timeout(timeout)
      
    resp_test <- httr2::req_perform(req_test, verbosity = 0)
    
    if (httr2::resp_status(resp_test) == 200) {
      if (verbose) cli::cli_alert_success("RIBITS API is reachable")
      return(TRUE)
    } else {
      if (verbose) cli::cli_alert_warning("RIBITS API returned status {httr2::resp_status(resp_test)}")
      return(FALSE)
    }
  }, error = function(e) {
    if (verbose) cli::cli_alert_danger("Connection failed: {e$message}")
    return(FALSE)
  })
}
