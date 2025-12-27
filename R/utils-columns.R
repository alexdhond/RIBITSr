#' Safe Column Access Utilities
#'
#' These functions provide safe access to dataframe columns with validation
#' and helpful error messages. They wrap the case-insensitive column
#' lookup from utils-globals.R with additional safety checks.
#'
#' @name safe-columns
#' @keywords internal
NULL

#' Check if Column Exists in Dataframe
#'
#' Checks whether a column exists in a dataframe, optionally using
#' case-insensitive matching.
#'
#' @param df Dataframe to check
#' @param col Column name to look for
#' @param case_insensitive If TRUE, use case-insensitive matching (default: TRUE)
#'
#' @return Logical - TRUE if column exists, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(BankID = 1:3, Name = c("A", "B", "C"))
#'
#' .col_exists(df, "bank_id")  # TRUE (case-insensitive)
#' .col_exists(df, "BankID")   # TRUE
#' .col_exists(df, "missing")  # FALSE
#' }
#'
#' @keywords internal
.col_exists <- function(df, col, case_insensitive = TRUE) {
  # Handle NULL or empty inputs
  if (is.null(df) || !is.data.frame(df)) {
    return(FALSE)
  }

  if (is.null(col) || is.na(col) || col == "") {
    return(FALSE)
  }

  if (case_insensitive) {
    # Use existing case-insensitive lookup
    actual_col <- .get_column_case_insensitive(df, col)
    return(!is.na(actual_col))
  } else {
    # Direct name check
    return(col %in% names(df))
  }
}

#' Safely Get Column from Dataframe
#'
#' Retrieves a column from a dataframe with validation and helpful error
#' messages if the column doesn't exist.
#'
#' @param df Dataframe to get column from
#' @param col Column name to retrieve
#' @param case_insensitive If TRUE, use case-insensitive matching (default: TRUE)
#' @param error_if_missing If TRUE, throw error if column missing (default: TRUE)
#' @param default Value to return if column missing and error_if_missing is FALSE
#'
#' @return Column vector if found, default value or error if not found
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(BankID = 1:3, Name = c("A", "B", "C"))
#'
#' # Get column (case-insensitive)
#' ids <- .col_get(df, "bank_id")
#'
#' # Return default if missing
#' status <- .col_get(df, "status", error_if_missing = FALSE, default = NA)
#'
#' # Error if missing
#' .col_get(df, "nonexistent")  # Throws error
#' }
#'
#' @keywords internal
.col_get <- function(df, col, case_insensitive = TRUE,
                     error_if_missing = TRUE, default = NULL) {
  # Handle NULL inputs
  if (is.null(df) || !is.data.frame(df)) {
    if (error_if_missing) {
      rlang::abort(
        "Cannot get column from NULL or non-dataframe object",
        class = "ribits_column_error"
      )
    }
    return(default)
  }

  # Find actual column name
  if (case_insensitive) {
    actual_col <- .get_column_case_insensitive(df, col)

    if (is.na(actual_col)) {
      if (error_if_missing) {
        rlang::abort(
          c(
            glue::glue("Column '{col}' not found in dataframe"),
            i = glue::glue("Available columns: {paste(names(df), collapse = ', ')}"),
            i = "Note: Column matching is case-insensitive"
          ),
          class = "ribits_column_error"
        )
      }
      return(default)
    }

    return(df[[actual_col]])

  } else {
    # Case-sensitive match
    if (!col %in% names(df)) {
      if (error_if_missing) {
        rlang::abort(
          c(
            glue::glue("Column '{col}' not found in dataframe"),
            i = glue::glue("Available columns: {paste(names(df), collapse = ', ')}"),
            i = "Note: Column matching is case-sensitive"
          ),
          class = "ribits_column_error"
        )
      }
      return(default)
    }

    return(df[[col]])
  }
}

#' Safely Set Column in Dataframe
#'
#' Sets a column value in a dataframe, optionally using case-insensitive
#' matching to find existing columns.
#'
#' @param df Dataframe to modify
#' @param col Column name to set
#' @param value Value to set
#' @param case_insensitive If TRUE, match existing columns case-insensitively
#'
#' @return Modified dataframe
#'
#' @details
#' If case_insensitive is TRUE and a column with different case exists,
#' that column will be updated. Otherwise, a new column is created.
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(BankID = 1:3)
#'
#' # Update existing column (case-insensitive)
#' df <- .col_set(df, "bank_id", c(10, 20, 30))  # Updates BankID
#'
#' # Add new column
#' df <- .col_set(df, "status", c("active", "active", "inactive"))
#' }
#'
#' @keywords internal
.col_set <- function(df, col, value, case_insensitive = TRUE) {
  if (is.null(df) || !is.data.frame(df)) {
    rlang::abort(
      "Cannot set column in NULL or non-dataframe object",
      class = "ribits_column_error"
    )
  }

  # Find actual column name if case-insensitive
  if (case_insensitive) {
    actual_col <- .get_column_case_insensitive(df, col)

    if (!is.na(actual_col)) {
      # Update existing column
      df[[actual_col]] <- value
    } else {
      # Create new column
      df[[col]] <- value
    }
  } else {
    # Direct assignment
    df[[col]] <- value
  }

  df
}

#' Get Multiple Columns Safely
#'
#' Retrieves multiple columns from a dataframe, returning only those that exist.
#'
#' @param df Dataframe to get columns from
#' @param cols Character vector of column names
#' @param case_insensitive If TRUE, use case-insensitive matching
#' @param require_all If TRUE, error if any column is missing
#'
#' @return Dataframe with requested columns (or subset if not all found)
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(BankID = 1:3, Name = c("A", "B", "C"), Status = c("A", "A", "I"))
#'
#' # Get existing columns
#' subset <- .col_get_multiple(df, c("bank_id", "name"))
#'
#' # Get only existing columns, skip missing
#' subset <- .col_get_multiple(df, c("bank_id", "missing"), require_all = FALSE)
#' }
#'
#' @keywords internal
.col_get_multiple <- function(df, cols, case_insensitive = TRUE,
                              require_all = FALSE) {
  if (is.null(df) || !is.data.frame(df)) {
    if (require_all) {
      rlang::abort(
        "Cannot get columns from NULL or non-dataframe object",
        class = "ribits_column_error"
      )
    }
    return(tibble::tibble())
  }

  # Find actual column names
  if (case_insensitive) {
    actual_cols <- purrr::map_chr(cols, ~.get_column_case_insensitive(df, .x))
    names(actual_cols) <- cols

    # Check for missing columns
    missing_cols <- cols[is.na(actual_cols)]

    if (length(missing_cols) > 0) {
      if (require_all) {
        rlang::abort(
          c(
            "Required columns not found",
            x = glue::glue("Missing: {paste(missing_cols, collapse = ', ')}"),
            i = glue::glue("Available: {paste(names(df), collapse = ', ')}")
          ),
          class = "ribits_column_error"
        )
      } else {
        # Return only existing columns
        actual_cols <- actual_cols[!is.na(actual_cols)]
      }
    }

    if (length(actual_cols) == 0) {
      return(tibble::tibble())
    }

    return(df[, actual_cols, drop = FALSE])

  } else {
    # Case-sensitive match
    existing_cols <- intersect(cols, names(df))
    missing_cols <- setdiff(cols, names(df))

    if (length(missing_cols) > 0 && require_all) {
      rlang::abort(
        c(
          "Required columns not found",
          x = glue::glue("Missing: {paste(missing_cols, collapse = ', ')}"),
          i = glue::glue("Available: {paste(names(df), collapse = ', ')}")
        ),
        class = "ribits_column_error"
      )
    }

    if (length(existing_cols) == 0) {
      return(tibble::tibble())
    }

    return(df[, existing_cols, drop = FALSE])
  }
}

#' Rename Column Safely
#'
#' Renames a column, optionally using case-insensitive matching to find it.
#'
#' @param df Dataframe to modify
#' @param old_name Current column name
#' @param new_name New column name
#' @param case_insensitive If TRUE, match old_name case-insensitively
#' @param error_if_missing If TRUE, error if old column doesn't exist
#'
#' @return Modified dataframe
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(BankID = 1:3)
#'
#' # Rename with case-insensitive matching
#' df <- .col_rename(df, "bank_id", "id")
#' }
#'
#' @keywords internal
.col_rename <- function(df, old_name, new_name, case_insensitive = TRUE,
                       error_if_missing = TRUE) {
  if (is.null(df) || !is.data.frame(df)) {
    rlang::abort(
      "Cannot rename column in NULL or non-dataframe object",
      class = "ribits_column_error"
    )
  }

  # Find actual column name
  if (case_insensitive) {
    actual_old <- .get_column_case_insensitive(df, old_name)

    if (is.na(actual_old)) {
      if (error_if_missing) {
        rlang::abort(
          c(
            glue::glue("Column '{old_name}' not found for renaming"),
            i = glue::glue("Available: {paste(names(df), collapse = ', ')}")
          ),
          class = "ribits_column_error"
        )
      }
      return(df)
    }

    # Rename
    names(df)[names(df) == actual_old] <- new_name

  } else {
    if (!old_name %in% names(df)) {
      if (error_if_missing) {
        rlang::abort(
          c(
            glue::glue("Column '{old_name}' not found for renaming"),
            i = glue::glue("Available: {paste(names(df), collapse = ', ')}")
          ),
          class = "ribits_column_error"
        )
      }
      return(df)
    }

    # Rename
    names(df)[names(df) == old_name] <- new_name
  }

  df
}

#' Check if All Required Columns Exist
#'
#' Validates that all required columns exist in a dataframe.
#'
#' @param df Dataframe to check
#' @param required_cols Character vector of required column names
#' @param case_insensitive If TRUE, use case-insensitive matching
#'
#' @return Invisible NULL if all exist, otherwise throws error
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(BankID = 1:3, Name = c("A", "B", "C"))
#'
#' # Validate required columns exist
#' .col_require(df, c("bank_id", "name"))  # Passes
#' .col_require(df, c("bank_id", "missing"))  # Errors
#' }
#'
#' @keywords internal
.col_require <- function(df, required_cols, case_insensitive = TRUE) {
  if (is.null(df) || !is.data.frame(df)) {
    rlang::abort(
      "Cannot check columns in NULL or non-dataframe object",
      class = "ribits_column_error"
    )
  }

  if (case_insensitive) {
    actual_cols <- purrr::map_chr(required_cols,
                                   ~.get_column_case_insensitive(df, .x))
    missing_cols <- required_cols[is.na(actual_cols)]
  } else {
    missing_cols <- setdiff(required_cols, names(df))
  }

  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        "Required columns missing from dataframe",
        x = glue::glue("Missing: {paste(missing_cols, collapse = ', ')}"),
        i = glue::glue("Available: {paste(names(df), collapse = ', ')}")
      ),
      class = "ribits_column_error"
    )
  }

  invisible(NULL)
}
