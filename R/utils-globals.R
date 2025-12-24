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
