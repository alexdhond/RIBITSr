# R/api-registry.R

#' RIBITS API Endpoint Registry
#'
#' Central configuration for all RIBITS API endpoints. This makes it easy to
#' add new endpoints and maintain consistent behavior across all API functions.
#'
#' @keywords internal
#' @noRd
RIBITS_ENDPOINTS <- list(
  banks = list(
    type_name = "banks",
    type_singular = "bank",
    list_endpoint = "bank_site_list/",
    get_endpoint = "bank_site_data/",
    id_param = "bank_id",
    show_params = c("service_area", "footprint", "contacts", "ledger"),
    list_filters = c("state", "district", "fieldoffice", "noaaregion", "kind_of_bank")
  ),

  ilf_programs = list(
    type_name = "ILF programs",
    type_singular = "ILF program",
    list_endpoint = "ilf_program_list/",
    get_endpoint = "ilf_program_data/",
    id_param = "program_id",
    show_params = c("service_area", "contacts", "ledger"),
    list_filters = c("state", "district", "fieldoffice", "noaaregion")
  ),

  umbrellas = list(
    type_name = "umbrella instruments",
    type_singular = "umbrella instrument",
    list_endpoint = "umbrella_instrument_list/",
    get_endpoint = "umbrella_instrument_data/",
    id_param = "umbrella_instrument_id",
    show_params = c("service_area", "contacts", "ledger"),
    list_filters = c("state", "district", "fieldoffice", "noaaregion")
  ),

  wqt_projects = list(
    type_name = "WQT projects",
    type_singular = "WQT project",
    list_endpoint = "wqt_project_list/",
    get_endpoint = "wqt_project_data/",
    id_param = "wqt_project_id",
    show_params = character(0),
    list_filters = c("state", "district")
  )
)

#' Get endpoint configuration
#'
#' @param type Character. Endpoint type (e.g., "banks", "ilf_programs")
#' @return List with endpoint configuration
#' @keywords internal
#' @noRd
rb_get_endpoint_config <- function(type) {
  config <- RIBITS_ENDPOINTS[[type]]

  if (is.null(config)) {
    rlang::abort(
      paste("Unknown endpoint type:", type),
      class = "ribits_error_unknown_endpoint"
    )
  }

  config
}

#' Validate filter parameters against endpoint configuration
#'
#' @param type Character. Endpoint type
#' @param params List. Parameters to validate
#' @return List. Validated parameters (with NULLs removed)
#' @keywords internal
#' @noRd
rb_validate_list_params <- function(type, params) {
  config <- rb_get_endpoint_config(type)

  # Remove NULL parameters
  params <- purrr::compact(params)

  # Get parameter names
  param_names <- names(params)

  # Check for invalid parameters
  invalid <- setdiff(param_names, config$list_filters)

  if (length(invalid) > 0) {
    cli::cli_alert_warning(
      "Unknown filter(s) for {config$type_name}: {paste(invalid, collapse = ', ')}"
    )
    cli::cli_alert_info(
      "Valid filters: {paste(config$list_filters, collapse = ', ')}"
    )
  }

  params
}
