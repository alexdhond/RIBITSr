# R/harmonization-config.R
# Configuration and priority rules for discrepancy resolution

#' Configure discrepancy resolution priorities
#'
#' Set rules for how conflicts between data sources should be resolved.
#' These settings persist for the R session.
#'
#' @param source_priority Character vector specifying source priority order:
#'   - "api" = RIBITS API (default first - most current)
#'   - "csv" = RIBITS CSV reports (most complete/official)
#'   - "epa" = EPA ArcGIS (best spatial coverage)
#' @param numeric_tolerance Numeric tolerance for considering values "equal".
#'   Default 0.01 (1% difference considered equal).
#' @param date_tolerance_days For date fields, days difference considered equal.
#'   Default 0 (exact match required).
#' @param string_matching How to compare strings:
#'   - "exact" (default): case-sensitive exact match
#'   - "ignore_case": case-insensitive
#'   - "fuzzy": allow minor differences (whitespace, punctuation)
#' @param flag_threshold What percentage difference triggers a flag?
#'   Default 5 (flag differences >5%).
#' @param auto_resolve Automatically resolve conflicts using priority rules?
#'   Default TRUE. If FALSE, keeps all versions for user review.
#' @param auto_harmonize Apply intelligent auto-harmonization rules?
#'   Default TRUE. When enabled, automatically fixes obvious errors like:
#'   - Missing values (backfill from available source)
#'   - Negative values for positive fields (use positive value)
#'   - Future dates for historical fields (use past date)
#'   - Rounding differences <1% (use more precise value)
#'   - String normalization (whitespace, punctuation, case)
#'   Set to FALSE to only use priority rules without smart harmonization.
#'
#' @return Invisibly returns the configuration list
#'
#' @export
#' @examples
#' \dontrun{
#' # Prioritize CSV data (most official)
#' rb_discrepancy_config(source_priority = c("csv", "api", "epa"))
#'
#' # Be more tolerant of numeric differences
#' rb_discrepancy_config(numeric_tolerance = 0.05)  # 5% tolerance
#'
#' # Don't auto-resolve - let user choose
#' rb_discrepancy_config(auto_resolve = FALSE)
#'
#' # Disable smart harmonization (only use source priority)
#' rb_discrepancy_config(auto_harmonize = FALSE)
#'
#' # Reset to defaults
#' rb_discrepancy_config("reset")
#' }
rb_discrepancy_config <- function(source_priority = c("csv", "api", "epa"),
                                   numeric_tolerance = 0.01,
                                   date_tolerance_days = 0,
                                   string_matching = c("exact", "ignore_case", "fuzzy"),
                                   flag_threshold = 5,
                                   auto_resolve = TRUE,
                                   auto_harmonize = TRUE) {

  # Handle reset
  if (length(source_priority) == 1 && source_priority == "reset") {
    options(ribits_discrepancy_config = NULL)
    cli::cli_alert_success("Discrepancy configuration reset to defaults")
    return(invisible(NULL))
  }

  # Validate inputs
  valid_sources <- c("api", "csv", "epa")
  if (!all(source_priority %in% valid_sources)) {
    cli::cli_abort("source_priority must contain only: {paste(valid_sources, collapse = ', ')}")
  }

  string_matching <- match.arg(string_matching)

  # Build configuration
  config <- list(
    source_priority = source_priority,
    numeric_tolerance = numeric_tolerance,
    date_tolerance_days = date_tolerance_days,
    string_matching = string_matching,
    flag_threshold = flag_threshold,
    auto_resolve = auto_resolve,
    auto_harmonize = auto_harmonize,
    timestamp = Sys.time()
  )

  # Store in options
  options(ribits_discrepancy_config = config)

  cli::cli_alert_success("Discrepancy configuration updated")
  cli::cli_bullets(c(
    "i" = "Source priority: {paste(source_priority, collapse = ' > ')}",
    "i" = "Auto-resolve: {auto_resolve}",
    "i" = "Auto-harmonize: {auto_harmonize}",
    "i" = "Flag threshold: {flag_threshold}%"
  ))

  invisible(config)
}


#' Get current discrepancy configuration
#'
#' @return List with current configuration settings
#' @keywords internal
.get_discrepancy_config <- function() {
  config <- getOption("ribits_discrepancy_config")

  # Return defaults if not set
  if (is.null(config)) {
    config <- list(
      source_priority = c("csv", "api", "epa"),  # CSV most official/complete
      numeric_tolerance = 0.01,
      date_tolerance_days = 0,
      string_matching = "exact",
      flag_threshold = 5,
      auto_resolve = TRUE,
      auto_harmonize = TRUE
    )
  }

  config
}
