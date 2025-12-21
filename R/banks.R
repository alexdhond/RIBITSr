# Bank and site functions for RIBITS
#
# Functions for listing and retrieving mitigation bank data.

#' List mitigation banks from RIBITS
#'
#' Retrieves a list of mitigation banks with optional filtering by
#' bank type, district, state, NMFS region, or field office.
#'
#' @param kind Character. Filter by bank type. Options: "ILF", "Umbrella",
#'   "NRDA", "Standard". Multiple values can be comma-separated.
#' @param district Character. Filter by USACE district name.
#'   See `rb_districts()` for valid options.
#' @param state Character. Filter by state abbreviation (e.g., "CA", "TX").
#'   See `rb_states()` for valid options.
#' @param noaaregion Character. Filter by NMFS region.
#'   Options: "Alaska", "Northeast", "Pacific Islands", "Southeast", "West Coast".
#' @param fieldoffice Character. Filter by FWS field office.
#'   See `rb_field_offices()` for valid options.
#' @param email Character. Optional email for API tracking. If NULL, uses
#'   value from `rb_email()`.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{BANK_ID}{Integer. Unique bank identifier.}
#'     \item{BANK_NAME}{Character. Name of the bank.}
#'     \item{BANK_SITE_DATA_WS_URL}{Character. API URL for full details.}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # List all banks
#' all_banks <- rb_list_banks()
#'
#' # List only ILF sites
#' ilf_sites <- rb_list_banks(kind = "ILF")
#'
#' # List California banks
#' ca_banks <- rb_list_banks(state = "CA")
#'
#' # Combine filters
#' ca_standard <- rb_list_banks(state = "CA", kind = "Standard")
#' }
rb_list_banks <- function(kind = NULL,
                          district = NULL,
                          state = NULL,
                          noaaregion = NULL,
                          fieldoffice = NULL,
                          email = NULL) {
  # Use stored email if not provided
  if (is.null(email)) {
    email <- getOption("ribits.email")
  }
  
  # Build parameters
  params <- .rb_build_list_params(
    kind = kind,
    district = district,
    state = state,
    noaaregion = noaaregion,
    fieldoffice = fieldoffice
  )
  
  # Add email if provided
  if (!is.null(email)) {
    params$webconsumer_email <- email
  }
  
  # Make request
  cli::cli_alert_info("Fetching bank list from RIBITS...")
  response <- .rb_request("bank_site_list", params)
  
  # Extract and convert to tibble
  items <- .rb_extract_items(response)
  result <- .rb_items_to_tibble(items)
  
  cli::cli_alert_success("Retrieved {nrow(result)} banks")
  
  result
}

#' Get detailed bank data from RIBITS
#'
#' Retrieves comprehensive data for a specific mitigation bank,
#' optionally including service areas, footprint geometry, contacts,
#' and transaction ledger.
#'
#' @param bank_id Integer. The unique RIBITS bank identifier.
#'   Required.
#' @param service_area Logical. Include service area geometries?
#'   Default TRUE.
#' @param footprint Logical. Include bank footprint geometry?
#'   Default TRUE.
#' @param contacts Logical. Include contact information (sponsors,
#'   POCs, managers, IRT members)? Default TRUE.
#' @param ledger Logical. Include transaction ledger? Default TRUE.
#' @param email Character. Optional email for API tracking.
#'
#' @return A list containing bank attributes and requested nested data.
#'   Use `rb_extract_*()` functions to work with specific components:
#'   - `rb_extract_ledger()` for transaction data
#'   - `rb_extract_contacts()` for contact information
#'   - `rb_extract_footprint()` for bank boundary (sf object)
#'   - `rb_extract_service_areas()` for service area polygons (sf object)
#'
#' @export
#' @seealso [rb_extract_ledger()], [rb_extract_footprint()]
#' @examples
#' \dontrun
#' # Get bank with all details
#' bank <- rb_get_bank(17)
#'
#' # Get bank without spatial data (faster)
#' bank <- rb_get_bank(17, service_area = FALSE, footprint = FALSE)
#'
#' # Extract specific components
#' ledger <- rb_extract_ledger(bank)
#' footprint_sf <- rb_extract_footprint(bank)
#' }
rb_get_bank <- function(bank_id,
                        service_area = TRUE,
                        footprint = TRUE,
                        contacts = TRUE,
                        ledger = TRUE,
                        email = NULL) {
  # Validate input
  if (missing(bank_id) || is.null(bank_id)) {
    rlang::abort(
      c(
        "bank_id is required",
        "i" = "Use rb_list_banks() to find available bank IDs"
      ),
      class = "ribits_error_missing_param"
    )
  }
  
  if (!is.numeric(bank_id) || length(bank_id) != 1) {
    rlang::abort(
      c(
        "bank_id must be a single integer",
        "i" = "Use rb_get_banks() to retrieve multiple banks"
      ),
      class = "ribits_error_invalid_param"
    )
  }
  
  # Use stored email if not provided
  if (is.null(email)) {
    email <- getOption("ribits.email")
  }
  
  # Build parameters
  params <- list(bank_id = as.integer(bank_id))
  params <- c(params, .rb_build_show_params(
    service_area = service_area,
    footprint = footprint,
    contacts = contacts,
    ledger = ledger
  ))
  
  if (!is.null(email)) {
    params$webconsumer_email <- email
  }
  
  # Make request
  cli::cli_alert_info("Fetching bank {bank_id}...")
  response <- .rb_request("bank_site_data", params)
  
  # Extract single item
  result <- .rb_extract_items(response, expected_single = TRUE)
  
  cli::cli_alert_success("Retrieved bank: {result$BANK_NAME}")
  
  result
}

#' Get multiple banks from RIBITS
#'
#' Retrieves detailed data for multiple banks. This function handles
#' rate limiting and provides progress feedback.
#'
#' @param bank_ids Integer vector. Bank IDs to retrieve.
#' @param service_area Logical. Include service area geometries? Default TRUE.
#' @param footprint Logical. Include bank footprint geometry? Default TRUE.
#' @param contacts Logical. Include contact information? Default TRUE.
#' @param ledger Logical. Include transaction ledger? Default TRUE.
#' @param email Character. Optional email for API tracking.
#' @param progress Logical. Show progress bar? Default TRUE.
#'
#' @return A list of bank data lists, one per bank_id.
#'   Names correspond to bank_ids.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get details for specific banks
#' banks <- rb_get_banks(c(17, 18, 19))
#'
#' # Extract all ledgers
#' all_ledgers <- purrr::map_dfr(banks, rb_extract_ledger, .id = "bank_id")
#' }
rb_get_banks <- function(bank_ids,
                         service_area = TRUE,
                         footprint = TRUE,
                         contacts = TRUE,
                         ledger = TRUE,
                         email = NULL,
                         progress = TRUE) {
  # Validate input
  if (missing(bank_ids) || length(bank_ids) == 0) {
    rlang::abort(
      "bank_ids is required and must not be empty",
      class = "ribits_error_missing_param"
    )
  }
  
  bank_ids <- as.integer(bank_ids)
  n <- length(bank_ids)
  
  cli::cli_alert_info("Fetching {n} banks from RIBITS...")
  
  # Set up progress
  if (progress && n > 1) {
    cli::cli_progress_bar(
      "Fetching banks",
      total = n,
      format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"
    )
  }
  
  # Fetch each bank
  results <- purrr::map(bank_ids, function(id) {
    tryCatch({
      bank <- rb_get_bank(
        bank_id = id,
        service_area = service_area,
        footprint = footprint,
        contacts = contacts,
        ledger = ledger,
        email = email
      )
      
      if (progress && n > 1) {
        cli::cli_progress_update()
      }
      
      bank
    }, error = function(e) {
      cli::cli_alert_warning("Failed to fetch bank {id}: {e$message}")
      NULL
    })
  })
  
  if (progress && n > 1) {
    cli::cli_progress_done()
  }
  
  # Name results by bank_id
  names(results) <- as.character(bank_ids)
  
  # Remove failed requests
  results <- purrr::compact(results)
  
  cli::cli_alert_success("Retrieved {length(results)} of {n} banks")
  
  results
}

#' Get bank IDs by filter criteria
#'
#' Convenience function that lists banks and returns just the IDs.
#' Useful for piping into `rb_get_banks()`.
#'
#' @inheritParams rb_list_banks
#'
#' @return Integer vector of bank IDs.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all California bank IDs
#' ca_ids <- rb_bank_ids(state = "CA")
#'
#' # Then fetch details
#' ca_banks <- rb_get_banks(ca_ids)
#' }
rb_bank_ids <- function(kind = NULL,
                        district = NULL,
                        state = NULL,
                        noaaregion = NULL,
                        fieldoffice = NULL,
                        email = NULL) {
  banks <- rb_list_banks(
    kind = kind,
    district = district,
    state = state,
    noaaregion = noaaregion,
    fieldoffice = fieldoffice,
    email = email
  )
  
  banks$BANK_ID
}
