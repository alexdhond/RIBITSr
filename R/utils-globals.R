# R/utils-globals.R
# Global variable bindings to avoid R CMD check NOTEs
# These variables are used in dplyr/tidyverse NSE contexts

#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom stats na.omit
#' @importFrom tidyselect where
NULL

# Suppress R CMD check NOTEs about undefined global variables
# These are column names used in dplyr operations with .data pronoun
utils::globalVariables(c(
  # Column names used in dplyr operations
  "bank_id",
  "Bank ID",
  "name",
  "Name",
  "State List",
  "name_normalized",
  ".name_normalized",
  "bank_id_exact",
  "bank_id_fuzzy",
  "fuzzy_score",
  "transaction_id",
  "resolved_source",
  "source1",
  "value1",
  "value2",
  "severity",
  "resolved_value",
  "field",
  "count"
))

# ---- Utility Functions ----

#' Get column name case-insensitively
#'
#' @description
#' Find a column in a dataframe regardless of case. This eliminates the need
#' for repetitive `names(df)[tolower(names(df)) == "column_name"]` patterns.
#'
#' @param df A dataframe
#' @param col_name The column name to find (case-insensitive)
#' @param first_only If TRUE, return only the first match. If FALSE, return all matches.
#'
#' @return Character vector of matching column names, or NA_character_ if not found
#'
#' @keywords internal
.get_column_case_insensitive <- function(df, col_name, first_only = TRUE) {
  if (!is.data.frame(df)) return(NA_character_)

  matches <- names(df)[tolower(names(df)) == tolower(col_name)]

  if (length(matches) == 0) {
    return(NA_character_)
  }

  if (first_only) {
    return(matches[1])
  } else {
    return(matches)
  }
}

#' Safe fetch wrapper with error handling
#'
#' @description
#' Wraps a fetch operation in error handling with optional user feedback.
#' This eliminates repetitive tryCatch blocks throughout the codebase.
#'
#' @param fn A function to execute (no arguments)
#' @param description A description of what's being fetched (for error messages)
#' @param quietly If TRUE, suppress error messages
#' @param default_value Value to return on error (default: NULL)
#'
#' @return Result of fn() on success, default_value on error
#'
#' @keywords internal
.safe_fetch <- function(fn, description, quietly = FALSE, default_value = NULL) {
  tryCatch(
    fn(),
    error = function(e) {
      if (!quietly) {
        cli::cli_alert_warning("{description} failed: {e$message}")
      }
      default_value
    }
  )
}

#' Filter data by bank IDs
#'
#' @description
#' Filter a dataframe to specific bank IDs. If the dataframe has a bank_id column,
#' filter directly. If it only has a name column, perform name matching first.
#'
#' @param data A dataframe to filter
#' @param bank_ids A vector of bank IDs to filter to
#' @param quietly If TRUE, suppress messages
#'
#' @return Filtered dataframe
#'
#' @keywords internal
.filter_by_bank_ids <- function(data, bank_ids, quietly = FALSE) {
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }

  # Check if bank_id column exists (case-insensitive)
  id_col <- .get_column_case_insensitive(data, "bank_id")

  if (!is.na(id_col)) {
    # Direct filtering by bank_id
    data <- data |> dplyr::filter(.data[[id_col]] %in% bank_ids)
  } else if ("name" %in% names(data)) {
    # Need to match by name first
    if (!quietly) {
      cli::cli_alert_info("No bank_id column found, matching by name...")
    }

    lookup <- rb_build_name_lookup(include_csv = FALSE, cache = TRUE)
    data <- rb_match_names(data, lookup, name_col = "name", fuzzy = TRUE)

    # Now filter by the matched bank_id
    if ("bank_id" %in% names(data)) {
      data <- data |> dplyr::filter(bank_id %in% bank_ids)
    }
  } else {
    if (!quietly) {
      cli::cli_alert_warning("Cannot filter by bank_id: no 'bank_id' or 'name' column found")
    }
  }

  data
}


#' Get cache directory
#'
#' @description
#' Returns the appropriate cache directory, creating it if needed.
#' Consolidates the repeated pattern of cache directory setup.
#'
#' @param use_cache If TRUE, use persistent cache in tempdir. If FALSE, use tempdir directly.
#'
#' @return Path to cache directory
#'
#' @keywords internal
.get_cache_dir <- function(use_cache = TRUE) {
  cache_dir <- if (use_cache) {
    file.path(tempdir(), "ribits_cache")
  } else {
    tempdir()
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_dir
}


#' Ensure dataframe has bank_id column
#'
#' @description
#' Ensures a dataframe has a valid bank_id column. If it doesn't exist or is mostly NA,
#' attempts to match names to bank IDs using the lookup table.
#' Consolidates the repeated pattern of name matching across the codebase.
#'
#' @param df A dataframe that may need bank_id
#' @param quietly If TRUE, suppress progress messages
#' @param min_valid_pct Minimum percentage of valid bank_ids to consider the column valid (default 0.5)
#'
#' @return Dataframe with bank_id column added (if possible), with unmatched rows removed
#'
#' @keywords internal
.ensure_bank_id <- function(df, quietly = FALSE, min_valid_pct = 0.5) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  # Check if we already have a valid bank_id column
  has_valid_bank_id <- "bank_id" %in% names(df) &&
    (sum(!is.na(df$bank_id)) / nrow(df)) > min_valid_pct

  if (has_valid_bank_id) {
    return(df)
  }

  # Try to find a name column
  name_col <- names(df)[grepl("^name$|bank.?name", names(df), ignore.case = TRUE)][1]

  if (is.na(name_col)) {
    if (!quietly) {
      cli::cli_alert_warning("Cannot match to bank_id: no name column found")
    }
    return(df)
  }

  # Remove existing bank_id column if it's invalid
  if ("bank_id" %in% names(df)) {
    df <- df |> dplyr::select(-bank_id)
  }

  # Match names to IDs
  if (!quietly) {
    cli::cli_alert_info("Matching names to bank IDs...")
  }

  lookup <- rb_build_name_lookup(include_csv = FALSE)
  df <- rb_match_names(df, lookup, name_col = name_col, fuzzy = TRUE)

  # Clean up unmatched rows
  if ("bank_id" %in% names(df)) {
    # Ensure bank_id is integer (not character)
    df$bank_id <- as.integer(df$bank_id)

    n_unmatched <- sum(is.na(df$bank_id))
    if (n_unmatched > 0) {
      if (!quietly) {
        cli::cli_alert_warning("{n_unmatched} rows couldn't be matched to bank IDs (removing)")
      }
      df <- df |> dplyr::filter(!is.na(bank_id))
    }
  }

  df
}


#' Fetch CSV with bank_id
#'
#' @description
#' Downloads a CSV report, reads it, ensures it has bank_id, and optionally filters
#' to specific bank IDs. Consolidates the repeated pattern of CSV fetching.
#'
#' @param report_type Type of CSV report to download (e.g., "credit_classification")
#' @param bank_ids Optional vector of bank IDs to filter to
#' @param cache If TRUE, cache downloaded files
#' @param quietly If TRUE, suppress progress messages
#'
#' @return Dataframe with bank_id column, filtered to requested IDs
#'
#' @keywords internal
.fetch_csv_with_bank_id <- function(report_type, bank_ids = NULL, cache = TRUE, quietly = FALSE) {
  # Get cache directory
  cache_dir <- .get_cache_dir(cache)

  # Download CSV
  csv_file <- rb_download_report(report_type, download_dir = cache_dir)

  # Read CSV
  data <- rb_read(csv_file)

  # Ensure bank_id column exists
  data <- .ensure_bank_id(data, quietly = quietly)

  # Filter to specific bank IDs if requested
  if (!is.null(bank_ids) && "bank_id" %in% names(data)) {
    data <- data |> dplyr::filter(bank_id %in% bank_ids)
  }

  data
}
