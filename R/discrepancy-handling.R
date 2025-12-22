# R/discrepancy-handling.R
# Comprehensive discrepancy detection and resolution system

# =============================================================================
# CONFIGURATION & PRIORITY RULES
# =============================================================================

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
#'
#' @return Invisibly returns the configuration list
#'
#' @keywords internal
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
#' # Reset to defaults
#' rb_discrepancy_config("reset")
#' }
rb_discrepancy_config <- function(source_priority = c("csv", "api", "epa"),
                                   numeric_tolerance = 0.01,
                                   date_tolerance_days = 0,
                                   string_matching = c("exact", "ignore_case", "fuzzy"),
                                   flag_threshold = 5,
                                   auto_resolve = TRUE) {

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
    timestamp = Sys.time()
  )

  # Store in options
  options(ribits_discrepancy_config = config)

  cli::cli_alert_success("Discrepancy configuration updated")
  cli::cli_bullets(c(
    "i" = "Source priority: {paste(source_priority, collapse = ' > ')}",
    "i" = "Auto-resolve: {auto_resolve}",
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
      auto_resolve = TRUE
    )
  }

  config
}


# =============================================================================
# DISCREPANCY DETECTION
# =============================================================================

#' Compare two values and detect discrepancies
#'
#' @param value1 First value
#' @param value2 Second value
#' @param field_name Name of the field being compared
#' @param source1 Name of first source (e.g., "api")
#' @param source2 Name of second source (e.g., "csv")
#' @param config Configuration from .get_discrepancy_config()
#'
#' @return NULL if values match, or a tibble with discrepancy details
#' @keywords internal
.compare_values <- function(value1, value2, field_name, source1, source2, config) {

  # Both NA - no discrepancy
  if (is.na(value1) && is.na(value2)) {
    return(NULL)
  }

  # One NA, one not - discrepancy
  if (is.na(value1) || is.na(value2)) {
    return(tibble::tibble(
      field = field_name,
      source1 = source1,
      value1 = as.character(value1),
      source2 = source2,
      value2 = as.character(value2),
      discrepancy_type = "missing_value",
      severity = "medium",
      diff_pct = NA_real_
    ))
  }

  # Numeric comparison
  if (is.numeric(value1) && is.numeric(value2)) {
    # Calculate difference
    diff <- abs(value1 - value2)
    avg <- mean(c(value1, value2))
    diff_pct <- if (avg > 0) (diff / avg) * 100 else 0

    # Check if within tolerance
    if (diff_pct <= (config$numeric_tolerance * 100)) {
      return(NULL)  # Values match within tolerance
    }

    # Determine severity
    severity <- if (diff_pct > config$flag_threshold) "high" else "low"

    return(tibble::tibble(
      field = field_name,
      source1 = source1,
      value1 = value1,
      source2 = source2,
      value2 = value2,
      discrepancy_type = "numeric_difference",
      severity = severity,
      diff_pct = round(diff_pct, 2)
    ))
  }

  # Date comparison
  if (inherits(value1, "Date") && inherits(value2, "Date")) {
    diff_days <- abs(as.numeric(difftime(value1, value2, units = "days")))

    if (diff_days <= config$date_tolerance_days) {
      return(NULL)
    }

    return(tibble::tibble(
      field = field_name,
      source1 = source1,
      value1 = as.character(value1),
      source2 = source2,
      value2 = as.character(value2),
      discrepancy_type = "date_difference",
      severity = if (diff_days > 365) "high" else "medium",
      diff_pct = NA_real_,
      diff_days = diff_days
    ))
  }

  # String comparison
  if (is.character(value1) && is.character(value2)) {
    match <- switch(config$string_matching,
      exact = identical(value1, value2),
      ignore_case = tolower(value1) == tolower(value2),
      fuzzy = .fuzzy_match(value1, value2)
    )

    if (match) {
      return(NULL)
    }

    return(tibble::tibble(
      field = field_name,
      source1 = source1,
      value1 = value1,
      source2 = source2,
      value2 = value2,
      discrepancy_type = "string_mismatch",
      severity = "low",
      diff_pct = NA_real_
    ))
  }

  # Type mismatch
  return(tibble::tibble(
    field = field_name,
    source1 = source1,
    value1 = as.character(value1),
    source2 = source2,
    value2 = as.character(value2),
    discrepancy_type = "type_mismatch",
    severity = "high",
    diff_pct = NA_real_
  ))
}


#' Fuzzy string matching helper
#' @keywords internal
.fuzzy_match <- function(str1, str2) {
  # Normalize strings
  clean1 <- str1 |>
    tolower() |>
    stringr::str_squish() |>
    stringr::str_replace_all("[[:punct:]]", "")

  clean2 <- str2 |>
    tolower() |>
    stringr::str_squish() |>
    stringr::str_replace_all("[[:punct:]]", "")

  identical(clean1, clean2)
}


#' Compare two data frames and find all discrepancies
#'
#' @param df1 First data frame
#' @param df2 Second data frame
#' @param id_col Column name to use as identifier (e.g., "bank_id")
#' @param source1_name Name of first source
#' @param source2_name Name of second source
#' @param fields_to_compare Optional vector of specific fields to compare.
#'   If NULL, compares all common columns.
#'
#' @return A tibble with all detected discrepancies
#' @keywords internal
.compare_dataframes <- function(df1, df2, id_col = "bank_id",
                                source1_name = "source1",
                                source2_name = "source2",
                                fields_to_compare = NULL) {

  config <- .get_discrepancy_config()

  # Find common columns (excluding ID)
  common_cols <- intersect(names(df1), names(df2))
  common_cols <- setdiff(common_cols, id_col)

  # Use specified fields or all common
  if (!is.null(fields_to_compare)) {
    common_cols <- intersect(common_cols, fields_to_compare)
  }

  if (length(common_cols) == 0) {
    return(tibble::tibble())
  }

  # Find common IDs
  common_ids <- intersect(df1[[id_col]], df2[[id_col]])

  if (length(common_ids) == 0) {
    return(tibble::tibble())
  }

  # Filter to common IDs
  df1_common <- df1 |> dplyr::filter(.data[[id_col]] %in% common_ids)
  df2_common <- df2 |> dplyr::filter(.data[[id_col]] %in% common_ids)

  # Compare each row
  all_discrepancies <- list()

  for (id in common_ids) {
    row1 <- df1_common |> dplyr::filter(.data[[id_col]] == id)
    row2 <- df2_common |> dplyr::filter(.data[[id_col]] == id)

    # Compare each field
    for (col in common_cols) {
      val1 <- row1[[col]]
      val2 <- row2[[col]]

      # Handle list-columns (skip for now)
      if (is.list(val1) || is.list(val2)) {
        next
      }

      # Extract single values if needed
      if (length(val1) > 0) val1 <- val1[1]
      if (length(val2) > 0) val2 <- val2[1]

      # Compare
      disc <- .compare_values(val1, val2, col, source1_name, source2_name, config)

      if (!is.null(disc)) {
        disc[[id_col]] <- id
        all_discrepancies[[length(all_discrepancies) + 1]] <- disc
      }
    }
  }

  if (length(all_discrepancies) == 0) {
    return(tibble::tibble())
  }

  # Combine and reorganize
  result <- dplyr::bind_rows(all_discrepancies)

  # Move ID to front
  result <- result |>
    dplyr::select(dplyr::all_of(id_col), dplyr::everything())

  result
}


#' Compare geometries between sources
#'
#' @param geom1 sf object from source 1
#' @param geom2 sf object from source 2
#' @param id Identifier for the geometry
#' @param geom_type Type of geometry (e.g., "footprint", "service_area")
#'
#' @return NULL if geometries match, or a tibble with discrepancy details
#' @keywords internal
.compare_geometries <- function(geom1, geom2, id, geom_type) {

  # Check if geometries exist
  if (is.null(geom1) || is.null(geom2)) {
    return(NULL)
  }

  if (!inherits(geom1, "sf") || !inherits(geom2, "sf")) {
    return(NULL)
  }

  # Calculate areas
  area1 <- tryCatch({
    as.numeric(sf::st_area(geom1))
  }, error = function(e) NA_real_)

  area2 <- tryCatch({
    as.numeric(sf::st_area(geom2))
  }, error = function(e) NA_real_)

  if (is.na(area1) || is.na(area2)) {
    return(NULL)
  }

  # Convert to acres (assuming input is in square meters)
  area1_acres <- area1 / 4046.86
  area2_acres <- area2 / 4046.86

  # Calculate difference
  diff <- abs(area1_acres - area2_acres)
  avg_area <- mean(c(area1_acres, area2_acres))
  diff_pct <- if (avg_area > 0) (diff / avg_area) * 100 else 0

  # Only report if difference > 1%
  if (diff_pct < 1) {
    return(NULL)
  }

  tibble::tibble(
    bank_id = id,
    field = geom_type,
    source1 = "ribits_api",
    value1 = round(area1_acres, 2),
    source2 = "epa_arcgis",
    value2 = round(area2_acres, 2),
    discrepancy_type = "geometry_area",
    severity = if (diff_pct > 10) "high" else "medium",
    diff_pct = round(diff_pct, 2)
  )
}


# =============================================================================
# DISCREPANCY RESOLUTION
# =============================================================================

#' Resolve discrepancies using priority rules
#'
#' @param discrepancies Tibble of discrepancies
#' @param data_list Named list of data frames from different sources
#'
#' @return Tibble with resolved values and resolution metadata
#' @keywords internal
.resolve_discrepancies <- function(discrepancies, data_list) {

  config <- .get_discrepancy_config()

  if (!config$auto_resolve) {
    cli::cli_alert_info("Auto-resolve disabled. Returning unresolved discrepancies.")
    return(discrepancies)
  }

  # Add resolution column based on priority
  discrepancies <- discrepancies |>
    dplyr::mutate(
      resolved_source = dplyr::case_when(
        # Priority order from config
        source1 %in% config$source_priority[1] ~ source1,
        source2 %in% config$source_priority[1] ~ source2,
        source1 %in% config$source_priority[2] ~ source1,
        source2 %in% config$source_priority[2] ~ source2,
        TRUE ~ source1  # Default to source1 if not in priority list
      ),
      resolved_value = dplyr::if_else(
        resolved_source == source1,
        as.character(value1),
        as.character(value2)
      ),
      resolution_method = "priority_rule"
    )

  discrepancies
}


# =============================================================================
# USER-FACING DISCREPANCY REPORTING
# =============================================================================

#' Generate discrepancy report
#'
#' Create a comprehensive report of all data quality issues found across sources.
#'
#' @param data A ribits_data object
#' @param severity_filter Filter by severity: "all", "high", "medium", "low"
#' @param group_by How to group discrepancies: "field", "severity", "bank"
#' @param include_resolved Include resolved discrepancies? Default FALSE.
#'
#' @return A formatted tibble with discrepancy details
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' ca <- rb_banks(state = "CA")
#'
#' # View all discrepancies
#' rb_discrepancy_report(ca)
#'
#' # Only high-severity issues
#' rb_discrepancy_report(ca, severity_filter = "high")
#'
#' # Group by affected field
#' rb_discrepancy_report(ca, group_by = "field")
#' }
rb_discrepancy_report <- function(data,
                                   severity_filter = c("all", "high", "medium", "low"),
                                   group_by = c("severity", "field", "bank"),
                                   include_resolved = FALSE) {

  severity_filter <- match.arg(severity_filter)
  group_by <- match.arg(group_by)

  # Get discrepancies
  disc <- data$.meta$discrepancies

  if (is.null(disc) || nrow(disc) == 0) {
    cli::cli_alert_success("No discrepancies found!")
    return(invisible(tibble::tibble()))
  }

  # Filter by severity
  if (severity_filter != "all") {
    disc <- disc |> dplyr::filter(severity == severity_filter)
  }

  # Filter resolved if requested
  if (!include_resolved && "resolved_value" %in% names(disc)) {
    disc <- disc |> dplyr::filter(is.na(resolved_value))
  }

  # Print summary
  cli::cli_h2("Discrepancy Report")

  cli::cli_bullets(c(
    "i" = "Total discrepancies: {nrow(disc)}",
    "i" = "Severity breakdown:"
  ))

  severity_summary <- disc |>
    dplyr::group_by(severity) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  for (i in seq_len(nrow(severity_summary))) {
    sev <- severity_summary$severity[i]
    cnt <- severity_summary$count[i]
    icon <- dplyr::case_when(
      sev == "high" ~ "x",
      sev == "medium" ~ "!",
      TRUE ~ "i"
    )
    cli::cli_bullets(c("{icon}" = "{sev}: {cnt}"))
  }

  # Group results
  if (group_by == "field") {
    cli::cli_h3("By Field")
    field_summary <- disc |>
      dplyr::group_by(field) |>
      dplyr::summarise(
        count = dplyr::n(),
        high_severity = sum(severity == "high", na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(count))

    print(field_summary)
  } else if (group_by == "bank") {
    cli::cli_h3("By Bank")
    if ("bank_id" %in% names(disc)) {
      bank_summary <- disc |>
        dplyr::group_by(bank_id) |>
        dplyr::summarise(
          count = dplyr::n(),
          high_severity = sum(severity == "high", na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(count))

      print(bank_summary)
    }
  }

  invisible(disc)
}


#' Export discrepancies to CSV for manual review
#'
#' @param data A ribits_data object
#' @param file_path Path to save CSV file
#'
#' @keywords internal
rb_export_discrepancies <- function(data, file_path = "ribits_discrepancies.csv") {
  disc <- data$.meta$discrepancies

  if (is.null(disc) || nrow(disc) == 0) {
    cli::cli_alert_info("No discrepancies to export")
    return(invisible(NULL))
  }

  readr::write_csv(disc, file_path)
  cli::cli_alert_success("Exported {nrow(disc)} discrepancies to {.file {file_path}}")

  invisible(disc)
}


# =============================================================================
# DATA MERGING WITH COLUMN PRESERVATION
# =============================================================================

#' Merge data frames preserving all columns from both sources
#'
#' Uses coalesce logic to combine matching columns and preserves
#' columns unique to each source. Priority source fills values first.
#'
#' @param df1 First data frame (priority source)
#' @param df2 Second data frame (secondary source)
#' @param by Column(s) to join on. Default "bank_id".
#' @param suffix Suffixes for duplicate columns. Default c("_1", "_2").
#'
#' @return A merged data frame with all columns from both sources
#' @keywords internal
.merge_preserving_columns <- function(df1, df2, by = "bank_id", suffix = c("_1", "_2")) {
  if (is.null(df1) || nrow(df1) == 0) return(df2)
  if (is.null(df2) || nrow(df2) == 0) return(df1)
 
  # Normalize column names to lowercase for consistent merging
  # (clean_names will be applied at final output)
  names(df1) <- tolower(names(df1))
  names(df2) <- tolower(names(df2))
  by <- tolower(by)
  
  # Ensure join column exists in both
  if (!all(by %in% names(df1)) || !all(by %in% names(df2))) {
    cli::cli_alert_warning("Join column '{by}' not found in both data frames")
    return(df1)
  }
  
  # Get columns unique to each source (excluding join columns)
  cols1_only <- setdiff(names(df1), c(names(df2), by))
  cols2_only <- setdiff(names(df2), c(names(df1), by))
  common_cols <- setdiff(intersect(names(df1), names(df2)), by)
  
  # Full join to preserve all rows
  merged <- dplyr::full_join(df1, df2, by = by, suffix = suffix)
  
  # Coalesce common columns (df1 takes priority)
  for (col in common_cols) {
    col1 <- paste0(col, suffix[1])
    col2 <- paste0(col, suffix[2])
    
    if (col1 %in% names(merged) && col2 %in% names(merged)) {
      # Handle type mismatches by converting to character if needed
      val1 <- merged[[col1]]
      val2 <- merged[[col2]]
      
      # If types differ, convert both to character
      if (class(val1)[1] != class(val2)[1]) {
        val1 <- as.character(val1)
        val2 <- as.character(val2)
      }
      
      # Coalesce: use df1 value if available, else df2
      merged[[col]] <- dplyr::coalesce(val1, val2)
      merged <- merged |> dplyr::select(-dplyr::all_of(c(col1, col2)))
    }
  }
  
  merged
}

#' Merge multiple data frames preserving all columns
#'
#' @param df_list Named list of data frames to merge
#' @param by Column(s) to join on
#' @param priority_order Vector of names specifying merge priority
#'
#' @return A merged data frame
#' @keywords internal
.merge_multiple_sources <- function(df_list, by = "bank_id", priority_order = NULL) {
  # Filter out NULL/empty data frames
  df_list <- purrr::keep(df_list, ~ !is.null(.) && nrow(.) > 0)
  
  if (length(df_list) == 0) return(NULL)
  if (length(df_list) == 1) return(df_list[[1]])
  
  # Order by priority if specified
  if (!is.null(priority_order)) {
    available <- intersect(priority_order, names(df_list))
    others <- setdiff(names(df_list), priority_order)
    df_list <- df_list[c(available, others)]
  }
  
  # Sequentially merge
  result <- df_list[[1]]
  for (i in 2:length(df_list)) {
    result <- .merge_preserving_columns(result, df_list[[i]], by = by)
  }
  
  result
}
