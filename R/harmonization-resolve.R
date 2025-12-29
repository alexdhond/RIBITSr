# R/harmonization-resolve.R
# Discrepancy resolution and auto-harmonization engine
# Split from R/discrepancy-handling.R (1,052 lines → focused modules)

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
# AUTO-HARMONIZATION ENGINE
# =============================================================================

#' Auto-harmonize discrepancies using intelligent rules
#'
#' Automatically resolves discrepancies when the correct value is clear,
#' such as when one source is missing data, values differ by rounding,
#' or obvious errors exist (negative values, future dates, etc.).
#'
#' @param discrepancies Tibble of detected discrepancies
#' @param merged_data The merged data frame (will be updated with harmonized values)
#' @param config Discrepancy configuration
#'
#' @return List with:
#'   - data: harmonized data frame
#'   - discrepancies_remaining: discrepancies that need human review
#'   - resolutions: what was auto-fixed and why
#' @keywords internal
.auto_harmonize <- function(discrepancies, merged_data, config = NULL) {

  if (is.null(config)) {
    config <- .get_discrepancy_config()
  }

  # Initialize tracking
  resolutions <- list()
  remaining_discrepancies <- list()

  if (is.null(discrepancies) || nrow(discrepancies) == 0) {
    return(list(
      data = merged_data,
      discrepancies_remaining = tibble::tibble(),
      resolutions = tibble::tibble()
    ))
  }

  # Process each discrepancy
  for (i in seq_len(nrow(discrepancies))) {
    disc <- discrepancies[i, ]

    # Try auto-harmonization rules in order of confidence
    resolution <- .try_auto_harmonization_rules(disc, config)

    if (!is.null(resolution$harmonized_value)) {
      # Successfully auto-harmonized
      resolutions[[length(resolutions) + 1]] <- tibble::tibble(
        bank_id = disc$bank_id,
        field = disc$field,
        source1 = disc$source1,
        value1 = disc$value1,
        source2 = disc$source2,
        value2 = disc$value2,
        harmonized_value = resolution$harmonized_value,
        harmonized_source = resolution$source,
        resolution_rule = resolution$rule,
        confidence = resolution$confidence,
        discrepancy_type = disc$discrepancy_type
      )

      # Update merged_data with harmonized value
      if (!is.null(disc$bank_id) && disc$field %in% names(merged_data)) {
        bank_idx <- which(merged_data$bank_id == disc$bank_id)
        if (length(bank_idx) > 0) {
          merged_data[bank_idx, disc$field] <- resolution$harmonized_value
        }
      }

    } else {
      # Could not auto-harmonize - needs human review
      remaining_discrepancies[[length(remaining_discrepancies) + 1]] <- disc
    }
  }

  # Convert to tibbles
  resolutions_df <- if (length(resolutions) > 0) {
    dplyr::bind_rows(resolutions)
  } else {
    tibble::tibble()
  }

  remaining_df <- if (length(remaining_discrepancies) > 0) {
    dplyr::bind_rows(remaining_discrepancies)
  } else {
    tibble::tibble()
  }

  list(
    data = merged_data,
    discrepancies_remaining = remaining_df,
    resolutions = resolutions_df
  )
}


#' Try auto-harmonization rules for a single discrepancy
#'
#' @param disc Single row discrepancy tibble
#' @param config Discrepancy configuration
#'
#' @return List with harmonized_value, source, rule, and confidence
#'   Returns NULL values if cannot auto-harmonize
#' @keywords internal
.try_auto_harmonization_rules <- function(disc, config) {

  val1 <- disc$value1
  val2 <- disc$value2

  # Convert to appropriate types for comparison
  if (!is.na(val1) && !is.na(val2)) {
    # Try numeric conversion
    num1 <- suppressWarnings(as.numeric(val1))
    num2 <- suppressWarnings(as.numeric(val2))

    # Try date conversion using smart parser
    date1 <- .smart_date_parse(val1)
    date2 <- .smart_date_parse(val2)
  } else {
    # Set defaults when one or both values are NA
    num1 <- NA_real_
    num2 <- NA_real_
    date1 <- as.Date(NA)
    date2 <- as.Date(NA)
  }

  # =============================================================================
  # RULE 1: Missing Value Backfill (HIGH CONFIDENCE)
  # =============================================================================
  if (is.na(val1) && !is.na(val2)) {
    return(list(
      harmonized_value = val2,
      source = disc$source2,
      rule = "missing_value_backfill",
      confidence = "high"
    ))
  }

  if (!is.na(val1) && is.na(val2)) {
    return(list(
      harmonized_value = val1,
      source = disc$source1,
      rule = "missing_value_backfill",
      confidence = "high"
    ))
  }

  # =============================================================================
  # RULE 2: Obvious Errors - Negative Values for Positive Fields
  # =============================================================================
  positive_fields <- c("total_acres", "credits", "area", "acreage",
                       "total_credits", "available_credits")

  if (disc$field %in% positive_fields && !is.na(num1) && !is.na(num2)) {
    if (num1 < 0 && num2 >= 0) {
      return(list(
        harmonized_value = val2,
        source = disc$source2,
        rule = "negative_value_correction",
        confidence = "high"
      ))
    }

    if (num2 < 0 && num1 >= 0) {
      return(list(
        harmonized_value = val1,
        source = disc$source1,
        rule = "negative_value_correction",
        confidence = "high"
      ))
    }
  }

  # =============================================================================
  # RULE 3: Future Dates (Only for historical fields)
  # =============================================================================
  historical_fields <- c("date_established", "approval_date", "creation_date",
                         "establishment_date")

  if (disc$field %in% historical_fields && !is.na(date1) && !is.na(date2)) {
    today <- Sys.Date()

    if (date1 > today && date2 <= today) {
      return(list(
        harmonized_value = as.character(val2),
        source = disc$source2,
        rule = "future_date_correction",
        confidence = "high"
      ))
    }

    if (date2 > today && date1 <= today) {
      return(list(
        harmonized_value = as.character(val1),
        source = disc$source1,
        rule = "future_date_correction",
        confidence = "high"
      ))
    }
  }

  # =============================================================================
  # RULE 4: Rounding Differences (MEDIUM CONFIDENCE)
  # =============================================================================
  if (!is.na(num1) && !is.na(num2) && num1 > 0 && num2 > 0) {
    diff_pct <- abs(num1 - num2) / mean(c(num1, num2)) * 100

    # If difference is <1%, likely just rounding - use more precise value
    if (diff_pct < 1) {
      # Determine which is more precise (more decimal places)
      decimals1 <- .count_decimals(val1)
      decimals2 <- .count_decimals(val2)

      if (decimals1 > decimals2) {
        return(list(
          harmonized_value = val1,
          source = disc$source1,
          rule = "rounding_precision",
          confidence = "medium"
        ))
      } else if (decimals2 > decimals1) {
        return(list(
          harmonized_value = val2,
          source = disc$source2,
          rule = "rounding_precision",
          confidence = "medium"
        ))
      }
    }
  }

  # =============================================================================
  # RULE 5: Semantically Equivalent Dates (HIGH CONFIDENCE)
  # =============================================================================
  # Dates that are represented differently but parse to the same value
  # (e.g., "05/22/2007" vs Unix timestamp 1179792000000)
  if (!is.na(date1) && !is.na(date2) && date1 == date2) {
    # Dates are equal - prefer human-readable format over Unix timestamp
    # Check if either value looks like a Unix timestamp (large number or scientific notation)
    is_timestamp1 <- grepl("^[0-9]{10,}(\\.0+)?$|^[0-9]+\\.?[0-9]*e[+-]?[0-9]+$", as.character(val1))
    is_timestamp2 <- grepl("^[0-9]{10,}(\\.0+)?$|^[0-9]+\\.?[0-9]*e[+-]?[0-9]+$", as.character(val2))

    if (is_timestamp1 && !is_timestamp2) {
      # val2 is human-readable, val1 is timestamp
      return(list(
        harmonized_value = val2,
        source = disc$source2,
        rule = "date_format_normalization",
        confidence = "high"
      ))
    } else if (is_timestamp2 && !is_timestamp1) {
      # val1 is human-readable, val2 is timestamp
      return(list(
        harmonized_value = val1,
        source = disc$source1,
        rule = "date_format_normalization",
        confidence = "high"
      ))
    } else {
      # Both human-readable or both timestamps - use source priority
      # Prefer ISO format (YYYY-MM-DD) if available
      if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", val1)) {
        return(list(
          harmonized_value = val1,
          source = disc$source1,
          rule = "date_format_iso_preferred",
          confidence = "medium"
        ))
      } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", val2)) {
        return(list(
          harmonized_value = val2,
          source = disc$source2,
          rule = "date_format_iso_preferred",
          confidence = "medium"
        ))
      }
    }
  }

  # =============================================================================
  # RULE 6: Date Proximity (For dates within tolerance)
  # =============================================================================
  if (!is.na(date1) && !is.na(date2)) {
    diff_days <- abs(as.numeric(difftime(date1, date2, units = "days")))

    # If dates differ by 1-7 days, use earlier date (likely approval vs data entry)
    if (diff_days >= 1 && diff_days <= 7) {
      earlier_date <- min(date1, date2)
      earlier_source <- if (date1 == earlier_date) disc$source1 else disc$source2

      return(list(
        harmonized_value = as.character(earlier_date),
        source = earlier_source,
        rule = "date_proximity_earlier",
        confidence = "medium"
      ))
    }
  }

  # =============================================================================
  # RULE 7: String Normalization (MEDIUM CONFIDENCE)
  # =============================================================================
  if (is.character(val1) && is.character(val2)) {
    # Normalize both strings
    norm1 <- .normalize_string(val1)
    norm2 <- .normalize_string(val2)

    if (norm1 == norm2) {
      # Strings are semantically equivalent - use longer/more complete version
      if (nchar(val1) > nchar(val2)) {
        return(list(
          harmonized_value = val1,
          source = disc$source1,
          rule = "string_normalization",
          confidence = "medium"
        ))
      } else {
        return(list(
          harmonized_value = val2,
          source = disc$source2,
          rule = "string_normalization",
          confidence = "medium"
        ))
      }
    }

    # Check for obvious typos: one string contains the other
    if (grepl(norm1, norm2, fixed = TRUE)) {
      return(list(
        harmonized_value = val2,  # Use the longer one
        source = disc$source2,
        rule = "substring_match",
        confidence = "low"
      ))
    }

    if (grepl(norm2, norm1, fixed = TRUE)) {
      return(list(
        harmonized_value = val1,
        source = disc$source1,
        rule = "substring_match",
        confidence = "low"
      ))
    }
  }

  # =============================================================================
  # RULE 8: Source Priority (LOW CONFIDENCE)
  # =============================================================================
  # Only use as last resort - prefer CSV for official data
  if ("csv" %in% c(disc$source1, disc$source2)) {
    csv_value <- if (disc$source1 == "ribits_csv" || disc$source1 == "csv") val1 else val2
    csv_source <- if (disc$source1 == "ribits_csv" || disc$source1 == "csv") disc$source1 else disc$source2

    return(list(
      harmonized_value = csv_value,
      source = csv_source,
      rule = "source_priority_csv",
      confidence = "low"
    ))
  }

  # =============================================================================
  # Cannot auto-harmonize - needs human review
  # =============================================================================
  list(
    harmonized_value = NULL,
    source = NULL,
    rule = NULL,
    confidence = NULL
  )
}


#' Count decimal places in a number
#' @keywords internal
.count_decimals <- function(x) {
  x_char <- as.character(x)
  if (grepl("\\.", x_char)) {
    parts <- strsplit(x_char, "\\.")[[1]]
    nchar(parts[2])
  } else {
    0
  }
}


#' Normalize string for comparison
#' @keywords internal
.normalize_string <- function(str) {
  str |>
    tolower() |>
    stringr::str_squish() |>
    stringr::str_replace_all("[[:punct:]]", "") |>
    stringr::str_replace_all("\\s+", " ")
}


#' Smart date parser that handles multiple formats
#'
#' Handles:
#' - Unix timestamps (milliseconds or seconds)
#' - Standard date strings (YYYY-MM-DD, MM/DD/YYYY, etc.)
#' - Already-parsed Date objects
#'
#' @param x Date value in various formats
#' @return Date object or NA if cannot parse
#' @keywords internal
.smart_date_parse <- function(x) {
  # Already NA
  if (is.na(x)) {
    return(as.Date(NA))
  }

  # Already a Date
  if (inherits(x, "Date")) {
    return(x)
  }

  # Helper to validate date is in reasonable range (1900-2100)
  .is_reasonable_date <- function(d) {
    if (is.na(d)) return(FALSE)
    year <- as.integer(format(d, "%Y"))
    year >= 1900 && year <= 2100
  }

  # Try to convert to numeric (might be Unix timestamp)
  num_val <- suppressWarnings(as.numeric(x))

  if (!is.na(num_val)) {
    # Check if this looks like a Unix timestamp
    # Handle both positive (post-1970) and negative (pre-1970) timestamps
    # Millisecond timestamps: |value| >= 100000000000 (1973 or 1967) and < 10000000000000 (2286 or before)
    # Second timestamps: |value| >= 100000000 (1973 or 1967) and < 10000000000 (2286 or before)
    # Avoid false positives: 4-digit numbers (years) should not be treated as timestamps

    abs_val <- abs(num_val)

    # Milliseconds timestamp (11-13 digits absolute value)
    if (abs_val >= 100000000000 && abs_val < 10000000000000) {
      result <- tryCatch({
        d <- as.Date(as.POSIXct(num_val / 1000, origin = "1970-01-01", tz = "UTC"))
        if (.is_reasonable_date(d)) d else as.Date(NA)
      }, error = function(e) {
        as.Date(NA)
      })
      if (!is.na(result)) return(result)
    }

    # Seconds timestamp (9-10 digits absolute value)
    # Negative values handle dates before 1970 (e.g., -315619200 = Jan 1, 1960)
    if (abs_val >= 100000000 && abs_val < 10000000000) {
      result <- tryCatch({
        d <- as.Date(as.POSIXct(num_val, origin = "1970-01-01", tz = "UTC"))
        if (.is_reasonable_date(d)) d else as.Date(NA)
      }, error = function(e) {
        as.Date(NA)
      })
      if (!is.na(result)) return(result)
    }
  }

  # Try common US format MM/DD/YYYY first (most common in RIBITS data)
  result <- tryCatch({
    d <- as.Date(x, format = "%m/%d/%Y")
    if (!is.na(d)) d else as.Date(NA)
  }, error = function(e) {
    as.Date(NA)
  })
  if (!is.na(result)) {
    return(result)
  }

  # Try DD/MM/YYYY
  result <- tryCatch({
    d <- as.Date(x, format = "%d/%m/%Y")
    if (!is.na(d)) d else as.Date(NA)
  }, error = function(e) {
    as.Date(NA)
  })
  if (!is.na(result)) {
    return(result)
  }

  # Try MM-DD-YYYY
  result <- tryCatch({
    d <- as.Date(x, format = "%m-%d-%Y")
    if (!is.na(d)) d else as.Date(NA)
  }, error = function(e) {
    as.Date(NA)
  })
  if (!is.na(result)) {
    return(result)
  }

  # Try standard date parsing (ISO format, etc.) - LAST, as it can guess incorrectly
  result <- tryCatch({
    d <- as.Date(x)
    if (!is.na(d)) d else as.Date(NA)
  }, error = function(e) {
    as.Date(NA)
  })
  if (!is.na(result)) {
    return(result)
  }

  # Could not parse - return NA
  as.Date(NA)
}
