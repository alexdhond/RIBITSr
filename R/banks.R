# R/banks.R

#' List mitigation banks
#'
#' Retrieves a list of mitigation banks from RIBITS, optionally filtered by
#' various parameters.
#'
#' @param state Character. Two-letter state abbreviation (e.g., "FL").
#' @param district Character. USACE district code.
#'
#' @return A tibble containing bank information.
#' @export
rb_list_banks <- function(state = NULL, district = NULL) {
  params <- list()
  if (!is.null(state)) params$state <- state
  if (!is.null(district)) params$district <- district
  
  resp <- rb_request("bank_site_list/", params)
  data <- rb_parse_response(resp)
  
  # Handle ORDS response structure (often in 'items')
  # The API seems to return uppercase 'ITEMS' sometimes
  if ("items" %in% names(data)) {
    tibble::as_tibble(data$items) |> janitor::clean_names()
  } else if ("ITEMS" %in% names(data)) {
    tibble::as_tibble(data$ITEMS) |> janitor::clean_names()
  } else {
    tibble::as_tibble(data) |> janitor::clean_names()
  }
}

#' @rdname rb_list_banks
#' @export
get_all_banks <- function() {
  warning("get_all_banks() is deprecated. Please use rb_list_banks() instead.")
  rb_list_banks()
}

#' Get bank details
#'
#' Retrieves detailed information for a specific mitigation bank.
#'
#' @param bank_id Integer. The unique RIBITS bank identifier.
#' @param service_area Logical. Include service area geometries? Default TRUE.
#' @param footprint Logical. Include bank footprint geometry? Default TRUE.
#' @param contacts Logical. Include contact information? Default TRUE.
#' @param ledger Logical. Include transaction ledger? Default TRUE.
#'
#' @return A list containing bank attributes and requested nested data.
#' @export
rb_get_bank <- function(bank_id, service_area = TRUE, footprint = TRUE, contacts = TRUE, ledger = TRUE) {
  if (missing(bank_id)) {
    rlang::abort("bank_id is required", class = "ribits_error_missing_param")
  }
  
  params <- list(
    bank_id = bank_id,
    show_service_area = tolower(as.character(service_area)),
    show_footprint = tolower(as.character(footprint)),
    show_contacts = tolower(as.character(contacts)),
    show_ledger = tolower(as.character(ledger))
  )
  
  resp <- rb_request("bank_site_data/", params)
  data <- rb_parse_response(resp)
  
  # Handle ITEMS wrapper if present
  if ("items" %in% names(data)) {
    data <- data$items
  } else if ("ITEMS" %in% names(data)) {
    data <- data$ITEMS
  }
  
  # If it's a data frame, return as tibble
  if (is.data.frame(data)) {
    return(tibble::as_tibble(data) |> janitor::clean_names())
  }
  
  # If it's a list, convert to 1-row tibble with list-columns for nested data
  if (is.list(data)) {
    # Wrap recursive elements (lists/dfs) or multi-element vectors in a list
    # so they can be stored in a single cell of a tibble
    processed_data <- lapply(data, function(x) {
      if (is.recursive(x) || length(x) != 1) {
        list(x)
      } else {
        x
      }
    })
    return(tibble::as_tibble(processed_data) |> janitor::clean_names())
  }
  
  tibble::as_tibble(data) |> janitor::clean_names()
}

#' Get details for multiple banks
#'
#' Retrieves detailed information for multiple mitigation banks, handling
#' rate limiting and errors.
#'
#' @param bank_ids Integer vector. Unique RIBITS bank identifiers.
#' @param ... Additional arguments passed to `rb_get_bank()` (e.g., `service_area`, `ledger`).
#' @param progress Logical. Show progress bar? Default TRUE.
#'
#' @return A tibble containing combined bank information.
#' @export
rb_get_banks <- function(bank_ids, ..., progress = TRUE) {
  if (length(bank_ids) == 0) {
    return(tibble::tibble())
  }
  
  if (progress) {
    bar_id <- cli::cli_progress_bar("Downloading banks", total = length(bank_ids))
  }
  
  results <- purrr::map(bank_ids, function(id) {
    if (progress) cli::cli_progress_update(id = bar_id)
    
    # Rate limiting
    Sys.sleep(0.5)
    
    tryCatch({
      rb_get_bank(id, ...)
    }, error = function(e) {
      cli::cli_alert_warning("Failed to retrieve bank {id}: {e$message}")
      return(NULL)
    })
  })
  
  if (progress) cli::cli_progress_done(id = bar_id)
  
  # Combine results
  dplyr::bind_rows(results)
}
