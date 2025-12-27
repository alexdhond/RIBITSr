# R/discrepancy-compare.R
# Discrepancy detection and comparison logic
# Split from R/discrepancy-handling.R (1,052 lines → focused modules)

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
  area1_acres <- area1 / SQ_METERS_PER_ACRE
  area2_acres <- area2 / SQ_METERS_PER_ACRE

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
