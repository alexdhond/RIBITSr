#' Read Bank Summary CSV
#'
#' Reads and cleans the "Bank Summary" CSV file manually downloaded from RIBITS.
#'
#' @param path Path to the CSV file.
#' @return A tibble.
#' @export
rb_read_bank_summary <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste("File not found:", path))
  }
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()
}

#' Read Credit Classification CSV
#'
#' Reads and cleans the "Credit Classification by Jurisdiction" CSV file.
#'
#' @param path Path to the CSV file.
#' @return A tibble.
#' @export
rb_read_credit_classification <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste("File not found:", path))
  }
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()
}

#' Read Credit Tracking CSV
#'
#' Reads and cleans the "Bank and ILF Program Credit Tracking" CSV file.
#'
#' @param path Path to the CSV file.
#' @return A tibble.
#' @export
rb_read_credit_tracking <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste("File not found:", path))
  }
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()
}

#' Read Service Area Comments CSV
#'
#' Reads and cleans the "Service Area Comments" CSV file.
#'
#' @param path Path to the CSV file.
#' @return A tibble.
#' @export
rb_read_service_area_comments <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste("File not found:", path))
  }
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()
}

#' Read Credit Withdrawal CSV
#'
#' Reads and cleans the "Bank and ILF Program Credit Withdrawal" CSV file.
#'
#' @param path Path to the CSV file.
#' @return A tibble.
#' @export
rb_read_credit_withdrawal <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste("File not found:", path))
  }
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()
}

#' Read Potential Credits by Mitigation Type CSV
#'
#' Parses the "Potential Credits by Mitigation Type" CSV file downloaded from
#' RIBITS. This file has a complex hierarchical structure with nested headers
#' and subtotals that require special handling.
#'
#' The function performs a two-pass parse:
#' 1. First pass: Identifies hierarchical context (District, Resource, Method)
#' 2. Second pass: Filters out non-data rows and parses the clean data
#'
#' @param path Path to the CSV file.
#' @return A tibble with columns: district_name, reported_bank_count,
#'   reported_status, resource_type, mitigation_method, bank_name,
#'   credit_classification, potential_credits, init_acres, init_feet.
#' @export
#' @examples
#' \dontrun{
#' # Read and parse potential credits file
#' pot_credits <- rb_read_potential_credits(
#'   "Potential Credits by Mitigation Type 2025_11_19.csv"
#' )
#' }
rb_read_potential_credits <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste("File not found:", path))
  }

  # Read all lines as text
  raw_lines <- readr::read_lines(path)

  # Define expected values
  resources <- c("Wetland", "Stream", "Species", "NRDA")
  methods <- c("Establishment", "Re-establishment", "Rehabilitation",
               "Preservation", "Enhancement", "Uplands (Buffer)",
               "-Unspecified-")

  # PASS 1: Map hierarchical context
  hierarchy_map <- tibble::tibble(raw_line = raw_lines) |>
    dplyr::mutate(
      # Clean the lines
      clean_line = stringr::str_remove_all(.data$raw_line, '"') |>
        stringr::str_trim(),

      # Detect context rows
      is_district = stringr::str_detect(.data$clean_line, "^USACE District"),
      is_resource = .data$clean_line %in% resources,
      is_method = stringr::str_detect(
        .data$clean_line,
        paste0("^(", paste(methods, collapse = "|"), ")")
      ),

      # Extract context values
      district_val = dplyr::if_else(.data$is_district, .data$clean_line, NA_character_),
      resource_val = dplyr::if_else(.data$is_resource, .data$clean_line, NA_character_),
      method_val = dplyr::if_else(.data$is_method, .data$clean_line, NA_character_)
    ) |>
    # Fill down context
    tidyr::fill(.data$district_val, .direction = "down") |>
    tidyr::fill(.data$resource_val, .direction = "down") |>
    # Reset method when resource changes
    dplyr::mutate(
      method_val = dplyr::if_else(.data$is_resource, NA_character_, .data$method_val)
    ) |>
    tidyr::fill(.data$method_val, .direction = "down") |>
    # Filter to data rows only
    dplyr::filter(
      # Remove lines with no commas (headers/context)
      stringr::str_count(.data$raw_line, ",") > 0,
      # Remove subtotal lines (start with empty field)
      !stringr::str_detect(.data$raw_line, '^"","'),
      # Remove total lines
      !stringr::str_detect(.data$clean_line, "^Total"),
      # Remove column header row
      !stringr::str_detect(.data$clean_line, "^Bank,Credit Classification")
    )

  # PASS 2: Parse as CSV
  parsed_data <- readr::read_csv(
    I(paste(hierarchy_map$raw_line, collapse = "\n")),
    col_names = c("bank_name", "credit_classification", "potential_credits",
                  "init_acres", "init_feet", "junk"),
    col_types = readr::cols(
      bank_name = readr::col_character(),
      credit_classification = readr::col_character(),
      potential_credits = readr::col_number(),
      init_acres = readr::col_number(),
      init_feet = readr::col_number(),
      junk = readr::col_character()
    ),
    trim_ws = TRUE
  )

  # Combine and clean
  final_data <- dplyr::bind_cols(
    hierarchy_map |>
      dplyr::select(district_raw = .data$district_val,
                    resource_type = .data$resource_val,
                    mitigation_method = .data$method_val),
    parsed_data
  ) |>
    dplyr::select(-.data$junk) |>
    # Parse district string
    tidyr::extract(
      col = .data$district_raw,
      into = c("district_name", "reported_bank_count", "reported_status"),
      regex = "^USACE District (.+) Reporting (\\d+) (.+)$",
      convert = TRUE
    ) |>
    dplyr::mutate(
      reported_status = stringr::str_remove(.data$reported_status, " Banks$")
    )

  # Validation
  n_fails <- sum(is.na(final_data$potential_credits))
  if (n_fails > 0) {
    cli::cli_alert_warning("{n_fails} rows failed to parse credits")
  }

  final_data
}
