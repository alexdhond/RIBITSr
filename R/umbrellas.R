# R/umbrellas.R

#' List umbrella instruments
#'
#' Retrieves a list of umbrella mitigation banking instruments from RIBITS,
#' optionally filtered by various parameters.
#'
#' @param state Character. Two-letter state abbreviation (e.g., "FL").
#' @param district Character. USACE district code.
#' @param field_office Character. FWS field office name.
#' @param noaa_region Character. NOAA/NMFS region name.
#'
#' @return A tibble containing umbrella instrument information.
#' @export
#' @examples
#' \dontrun{
#' # List all umbrella instruments
#' all_umbrellas <- rb_list_umbrellas()
#'
#' # Filter by state
#' ca_umbrellas <- rb_list_umbrellas(state = "CA")
#'
#' # Filter by district
#' district_umbrellas <- rb_list_umbrellas(district = "Sacramento")
#' }
rb_list_umbrellas <- function(state = NULL, district = NULL,
                               field_office = NULL, noaa_region = NULL) {
  params <- list()
  if (!is.null(state)) params$state <- state
  if (!is.null(district)) params$district <- district
  if (!is.null(field_office)) params$fieldoffice <- field_office
  if (!is.null(noaa_region)) params$noaaregion <- noaa_region

  resp <- rb_request("umbrella_instrument_list/", params)
  data <- rb_parse_response(resp)

  # Handle ORDS response structure
  if ("items" %in% names(data)) {
    tibble::as_tibble(data$items) |> janitor::clean_names()
  } else if ("ITEMS" %in% names(data)) {
    tibble::as_tibble(data$ITEMS) |> janitor::clean_names()
  } else {
    tibble::as_tibble(data) |> janitor::clean_names()
  }
}

#' Get umbrella instrument details
#'
#' Retrieves detailed information for a specific umbrella mitigation banking
#' instrument.
#'
#' @param umbrella_id Integer. The unique RIBITS umbrella instrument identifier.
#' @param service_area Logical. Include service area geometries? Default TRUE.
#' @param contacts Logical. Include contact information? Default TRUE.
#' @param ledger Logical. Include transaction ledger? Default TRUE.
#'
#' @return A list containing umbrella instrument attributes and requested nested
#'   data.
#' @export
#' @examples
#' \dontrun{
#' # Get a specific umbrella instrument with all details
#' umbrella <- rb_get_umbrella(1)
#'
#' # Get without ledger
#' umbrella_basic <- rb_get_umbrella(1, ledger = FALSE)
#' }
rb_get_umbrella <- function(umbrella_id, service_area = TRUE,
                             contacts = TRUE, ledger = TRUE) {
  if (missing(umbrella_id)) {
    rlang::abort("umbrella_id is required",
                 class = "ribits_error_missing_param")
  }

  params <- list(
    umbrella_id = umbrella_id,
    show_service_area = tolower(as.character(service_area)),
    show_contacts = tolower(as.character(contacts)),
    show_ledger = tolower(as.character(ledger))
  )

  resp <- rb_request("umbrella_instrument_data/", params)
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

#' Get details for multiple umbrella instruments
#'
#' Retrieves detailed information for multiple umbrella instruments, handling
#' rate limiting and errors.
#'
#' @param umbrella_ids Integer vector. Unique RIBITS umbrella instrument
#'   identifiers.
#' @param ... Additional arguments passed to `rb_get_umbrella()`.
#' @param progress Logical. Show progress bar? Default TRUE.
#'
#' @return A tibble containing combined umbrella instrument information.
#' @export
#' @examples
#' \dontrun{
#' # Get multiple umbrella instruments
#' umbrellas <- rb_get_umbrellas(c(1, 2, 3))
#' }
rb_get_umbrellas <- function(umbrella_ids, ..., progress = TRUE) {
  if (length(umbrella_ids) == 0) {
    return(tibble::tibble())
  }

  if (progress) {
    bar_id <- cli::cli_progress_bar("Downloading umbrella instruments",
                                    total = length(umbrella_ids))
  }

  results <- purrr::map(umbrella_ids, function(id) {
    if (progress) cli::cli_progress_update(id = bar_id)

    # Rate limiting
    Sys.sleep(0.5)

    tryCatch({
      rb_get_umbrella(id, ...)
    }, error = function(e) {
      cli::cli_alert_warning("Failed to retrieve umbrella {id}: {e$message}")
      return(NULL)
    })
  })

  if (progress) cli::cli_progress_done(id = bar_id)

  # Combine results
  dplyr::bind_rows(results)
}

#' Extract umbrella sites from umbrella instrument object
#'
#' @param umbrella A tibble returned by `rb_get_umbrella()`
#' @return A tibble containing umbrella sites.
#' @export
rb_extract_umbrella_sites <- function(umbrella) {
  val <- umbrella$umbrella_sites

  # Handle list-column from tibble
  if (is.list(val) && length(val) == 1 &&
      (tibble::is_tibble(umbrella) || is.data.frame(umbrella))) {
    val <- val[[1]]
  }

  if (is.null(val) || all(is.na(val))) {
    return(tibble::tibble())
  }

  # If it's already a data frame, return it as tibble
  if (is.data.frame(val)) {
    return(tibble::as_tibble(val) |> janitor::clean_names())
  }

  # If it's a list, try to bind rows
  if (is.list(val)) {
    return(dplyr::bind_rows(val) |> janitor::clean_names())
  }

  return(tibble::tibble())
}
