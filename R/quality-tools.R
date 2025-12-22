# R/quality-tools.R
# Data quality and coverage tools

#' Check data coverage for a state or region
#'
#' Quickly see data availability before downloading full datasets.
#' Shows how many banks exist and what percentage have various data types.
#'
#' @param state State filter (e.g., "CA", "OR", "TX")
#' @param district USACE district filter
#' @param quietly Suppress progress messages. Default FALSE.
#'
#' @return A tibble with coverage statistics
#' @export
#' @examples
#' \dontrun{
#' # Check Washington state coverage
#' rb_coverage(state = "WA")
#'
#' # Check Portland district
#' rb_coverage(district = "Portland")
#' }
rb_coverage <- function(state = NULL, district = NULL, quietly = FALSE) {
  
  if (is.null(state) && is.null(district)) {
    cli::cli_abort("Please specify at least one of: state, district")
  }
  
  filter_desc <- paste(c(
    if (!is.null(state)) paste0("state=", state),
    if (!is.null(district)) paste0("district=", district)
  ), collapse = ", ")
  
  if (!quietly) cli::cli_h2("Data Coverage: {filter_desc}")
  
  # Get API bank count
  if (!quietly) cli::cli_progress_step("Checking API...")
  api_banks <- tryCatch({
    rb_get("banks", state = state, district = district)
  }, error = function(e) NULL)
  n_api <- if (!is.null(api_banks)) nrow(api_banks) else 0
  
  # Get EPA bank count
  if (!quietly) cli::cli_progress_step("Checking EPA...")
  epa_banks <- tryCatch({
    rb_epa("banks", state = state, district = district) |> sf::st_drop_geometry()
  }, error = function(e) NULL)
  n_epa <- if (!is.null(epa_banks)) nrow(epa_banks) else 0
  
  # Get footprint count
  if (!quietly) cli::cli_progress_step("Checking footprints...")
  footprints <- tryCatch({
    rb_epa("footprints", state = state, district = district)
  }, error = function(e) NULL)
  n_fp <- if (!is.null(footprints)) nrow(footprints) else 0
  
  # Get service area count
  if (!quietly) cli::cli_progress_step("Checking service areas...")
  service_areas <- tryCatch({
    rb_epa("service_areas", state = state, district = district)
  }, error = function(e) NULL)
  n_sa <- if (!is.null(service_areas)) nrow(service_areas) else 0
  
  # Build result
  n_total <- max(n_api, n_epa)
  
  result <- tibble::tibble(
    metric = c("Total Banks (API)", "Total Banks (EPA)", "With Footprint", "With Service Area"),
    count = c(n_api, n_epa, n_fp, n_sa),
    percent = c(
      100,
      if (n_api > 0) round(100 * n_epa / n_api, 1) else NA,
      if (n_total > 0) round(100 * n_fp / n_total, 1) else NA,
      if (n_total > 0) round(100 * n_sa / n_total, 1) else NA
    )
  )
  
  if (!quietly) {
    cli::cli_h3("Coverage Summary")
    cli::cli_bullets(c(
      "*" = "Banks in API: {.val {n_api}}",
      "*" = "Banks in EPA: {.val {n_epa}} ({result$percent[2]}% of API)",
      "*" = "With footprints: {.val {n_fp}} ({result$percent[3]}%)",
      "*" = "With service areas: {.val {n_sa}} ({result$percent[4]}%)"
    ))
    
    if (n_api > n_epa) {
      cli::cli_alert_info("{n_api - n_epa} banks in API not in EPA (likely pending/newer)")
    }
  }
  
  invisible(result)
}


#' Generate data quality report for RIBITS data
#'
#' Analyzes a ribits_data object and reports on data quality including
#' missing values, source conflicts, and coverage gaps.
#'
#' @param data A ribits_data object returned by rb_banks() or similar
#' @param verbose Show detailed output. Default TRUE.
#'
#' @return A list with quality metrics (invisibly)
#' @export
#' @examples
#' \dontrun{
#' result <- rb_banks(state = "WA")
#' rb_quality_report(result)
#' }
rb_quality_report <- function(data, verbose = TRUE) {
  
  if (!inherits(data, "ribits_data")) {
    cli::cli_abort("Expected a ribits_data object from rb_banks() or similar")
  }
  
  report <- list()
  
  if (verbose) cli::cli_h1("Data Quality Report")
  
  # === Banks ===
  if (!is.null(data$banks) && nrow(data$banks) > 0) {
    banks <- data$banks
    n_banks <- nrow(banks)
    
    if (verbose) cli::cli_h2("Banks ({n_banks} rows)")
    
    # Key field completeness
    key_fields <- c("bank_id", "bank_name", "district", "state_list", 
                    "total_acres", "bank_status", "year_established", 
                    "permit_number", "bank_type")
    
    completeness <- sapply(key_fields, function(f) {
      if (f %in% names(banks)) {
        round(100 * sum(!is.na(banks[[f]])) / n_banks, 1)
      } else {
        NA
      }
    })
    
    report$banks_completeness <- tibble::tibble(
      field = key_fields,
      percent_complete = completeness
    )
    
    if (verbose) {
      cli::cli_h3("Field Completeness")
      for (i in seq_along(key_fields)) {
        pct <- completeness[i]
        status <- if (is.na(pct)) "!" else if (pct >= 90) "v" else if (pct >= 50) "*" else "x"
        pct_str <- if (is.na(pct)) "MISSING" else paste0(pct, "%")
        cli::cli_bullets(stats::setNames(
          paste0("{.field ", key_fields[i], "}: ", pct_str),
          status
        ))
      }
    }
    
    # Source breakdown
    if (!is.null(data$.meta$sources$banks)) {
      report$banks_sources <- data$.meta$sources$banks
      if (verbose) {
        cli::cli_h3("Data Sources")
        cli::cli_text("Sources: {.val {data$.meta$sources$banks}}")
      }
    }
  }
  
  # === Spatial Coverage ===
  if (verbose) cli::cli_h2("Spatial Coverage")
  
  n_banks <- if (!is.null(data$banks)) nrow(data$banks) else 0
  n_fp <- if (!is.null(data$footprints)) nrow(data$footprints) else 0
  n_sa <- if (!is.null(data$service_areas)) nrow(data$service_areas) else 0
  
  report$spatial <- tibble::tibble(
    type = c("Footprints", "Service Areas"),
    count = c(n_fp, n_sa),
    percent = if (n_banks > 0) round(100 * c(n_fp, n_sa) / n_banks, 1) else c(NA, NA)
  )
  
  if (verbose) {
    cli::cli_bullets(c(
      "*" = "Footprints: {.val {n_fp}}/{n_banks} ({report$spatial$percent[1]}%)",
      "*" = "Service Areas: {.val {n_sa}}/{n_banks} ({report$spatial$percent[2]}%)"
    ))
  }
  
  # === Ledger ===
  if (!is.null(data$ledger) && nrow(data$ledger) > 0) {
    if (verbose) {
      cli::cli_h2("Ledger Data")
      cli::cli_bullets(c(
        "*" = "Transactions: {.val {nrow(data$ledger)}}"
      ))
    }
    report$ledger_rows <- nrow(data$ledger)
  }
  
  # === Discrepancies ===
  if (!is.null(data$.meta$discrepancies) && nrow(data$.meta$discrepancies) > 0) {
    n_disc <- nrow(data$.meta$discrepancies)
    if (verbose) {
      cli::cli_h2("Data Discrepancies")
      cli::cli_alert_warning("{n_disc} discrepancies between sources")
      cli::cli_text("Use {.code discrepancies(data)} to view details")
    }
    report$discrepancies <- n_disc
  }
  
  # === Timing ===
  if (!is.null(data$.meta$timing$duration_secs)) {
    if (verbose) {
      cli::cli_h2("Performance")
      cli::cli_text("Fetch time: {.val {round(data$.meta$timing$duration_secs, 1)}} seconds")
    }
    report$fetch_time_secs <- data$.meta$timing$duration_secs
  }
  
  invisible(report)
}
