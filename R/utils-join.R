#' Safe Join Operations with Validation
#'
#' These functions wrap dplyr join operations with validation to prevent
#' silent failures from missing columns, type mismatches, or unexpected
#' duplications.
#'
#' @name safe-joins
#' @keywords internal
NULL

#' Safe Full Join with Validation
#'
#' Performs a full join with comprehensive validation of join columns
#' and result integrity.
#'
#' @param df1 First dataframe
#' @param df2 Second dataframe
#' @param by Join columns (character vector or named character vector)
#' @param suffix Suffixes to apply to duplicate columns
#' @param quietly If TRUE, suppress warnings
#'
#' @return Joined dataframe
#'
#' @details
#' This function validates:
#' - Join columns exist in both dataframes
#' - Join completes without errors
#' - Result doesn't have unexpected duplicates
#'
#' @keywords internal
.safe_full_join <- function(df1, df2, by, suffix = c(".x", ".y"),
                            quietly = FALSE) {
  # Handle NULL inputs
  if (is.null(df1) || nrow(df1) == 0) {
    if (!quietly) {
      cli::cli_alert_info("First dataframe is empty, returning second dataframe")
    }
    return(df2)
  }

  if (is.null(df2) || nrow(df2) == 0) {
    if (!quietly) {
      cli::cli_alert_info("Second dataframe is empty, returning first dataframe")
    }
    return(df1)
  }

  # Validate join columns exist in df1
  missing_cols_df1 <- setdiff(by, names(df1))
  if (length(missing_cols_df1) > 0) {
    rlang::abort(
      c(
        "Join columns missing from first dataframe",
        x = glue::glue("Missing: {paste(missing_cols_df1, collapse = ', ')}"),
        i = glue::glue("Available: {paste(names(df1), collapse = ', ')}")
      ),
      class = "ribits_join_error"
    )
  }

  # Validate join columns exist in df2
  missing_cols_df2 <- setdiff(by, names(df2))
  if (length(missing_cols_df2) > 0) {
    rlang::abort(
      c(
        "Join columns missing from second dataframe",
        x = glue::glue("Missing: {paste(missing_cols_df2, collapse = ', ')}"),
        i = glue::glue("Available: {paste(names(df2), collapse = ', ')}")
      ),
      class = "ribits_join_error"
    )
  }

  # Perform join with error handling
  # Suppress dplyr's many-to-many warning since we have our own warning below
  merged <- tryCatch(
    suppressWarnings(
      dplyr::full_join(df1, df2, by = by, suffix = suffix, relationship = "many-to-many")
    ),
    error = function(e) {
      rlang::abort(
        c(
          "Join operation failed",
          x = conditionMessage(e),
          i = "Check that join columns have compatible types"
        ),
        class = "ribits_join_error",
        parent = e
      )
    }
  )

  # Validate result integrity
  # For a full join, if there are duplicate keys, we get Cartesian product
  # which results in more rows than either input dataframe
  max_input_rows <- max(nrow(df1), nrow(df2))

  # If result has significantly more rows than the larger input,
  # we likely have many-to-many relationship
  if (nrow(merged) > max_input_rows) {
    if (!quietly) {
      cli::cli_warn(c(
        "Join produced more rows than expected",
        i = "Larger input: {max_input_rows} rows, result: {nrow(merged)} rows",
        i = "This indicates duplicate keys causing many-to-many joins",
        i = "Check for duplicates in join columns: '{paste(by, collapse = ', ')}'"
      ))
    }
  }

  merged
}

#' Safe Left Join with Validation
#'
#' @inheritParams .safe_full_join
#' @keywords internal
.safe_left_join <- function(df1, df2, by, suffix = c(".x", ".y"),
                            quietly = FALSE) {
  # Handle NULL inputs
  if (is.null(df1) || nrow(df1) == 0) {
    if (!quietly) {
      cli::cli_alert_info("Left dataframe is empty, returning empty result")
    }
    return(tibble::tibble())
  }

  if (is.null(df2) || nrow(df2) == 0) {
    if (!quietly) {
      cli::cli_alert_info("Right dataframe is empty, returning left dataframe")
    }
    return(df1)
  }

  # Validate join columns
  missing_cols_df1 <- setdiff(by, names(df1))
  if (length(missing_cols_df1) > 0) {
    rlang::abort(
      c(
        "Join columns missing from left dataframe",
        x = glue::glue("Missing: {paste(missing_cols_df1, collapse = ', ')}")
      ),
      class = "ribits_join_error"
    )
  }

  missing_cols_df2 <- setdiff(by, names(df2))
  if (length(missing_cols_df2) > 0) {
    rlang::abort(
      c(
        "Join columns missing from right dataframe",
        x = glue::glue("Missing: {paste(missing_cols_df2, collapse = ', ')}")
      ),
      class = "ribits_join_error"
    )
  }

  # Perform join
  merged <- tryCatch(
    dplyr::left_join(df1, df2, by = by, suffix = suffix),
    error = function(e) {
      rlang::abort(
        c(
          "Left join operation failed",
          x = conditionMessage(e)
        ),
        class = "ribits_join_error",
        parent = e
      )
    }
  )

  # Validate result - left join should preserve row count of df1
  if (nrow(merged) != nrow(df1)) {
    if (!quietly) {
      cli::cli_warn(c(
        "Left join changed row count",
        i = "Original: {nrow(df1)}, Result: {nrow(merged)}",
        i = "This indicates duplicate keys in right dataframe"
      ))
    }
  }

  merged
}

#' Safe Inner Join with Validation
#'
#' @inheritParams .safe_full_join
#' @keywords internal
.safe_inner_join <- function(df1, df2, by, suffix = c(".x", ".y"),
                             quietly = FALSE) {
  # Handle NULL inputs
  if (is.null(df1) || nrow(df1) == 0 ||
      is.null(df2) || nrow(df2) == 0) {
    if (!quietly) {
      cli::cli_alert_info("One or both dataframes empty, returning empty result")
    }
    return(tibble::tibble())
  }

  # Validate join columns
  missing_cols_df1 <- setdiff(by, names(df1))
  if (length(missing_cols_df1) > 0) {
    rlang::abort(
      c(
        "Join columns missing from first dataframe",
        x = glue::glue("Missing: {paste(missing_cols_df1, collapse = ', ')}")
      ),
      class = "ribits_join_error"
    )
  }

  missing_cols_df2 <- setdiff(by, names(df2))
  if (length(missing_cols_df2) > 0) {
    rlang::abort(
      c(
        "Join columns missing from second dataframe",
        x = glue::glue("Missing: {paste(missing_cols_df2, collapse = ', ')}")
      ),
      class = "ribits_join_error"
    )
  }

  # Perform join
  merged <- tryCatch(
    dplyr::inner_join(df1, df2, by = by, suffix = suffix),
    error = function(e) {
      rlang::abort(
        c(
          "Inner join operation failed",
          x = conditionMessage(e)
        ),
        class = "ribits_join_error",
        parent = e
      )
    }
  )

  # Validate result
  expected_max_rows <- min(nrow(df1), nrow(df2))
  if (nrow(merged) > expected_max_rows) {
    if (!quietly) {
      cli::cli_warn(c(
        "Inner join produced more rows than smaller input",
        i = "Smaller input: {expected_max_rows}, Result: {nrow(merged)}",
        i = "This indicates duplicate keys in one or both dataframes"
      ))
    }
  }

  merged
}

#' Validate Join Columns Before Joining
#'
#' Helper function to validate join columns exist and are compatible.
#'
#' @param df1 First dataframe
#' @param df2 Second dataframe
#' @param by Join columns
#'
#' @return Invisible NULL if valid, aborts with error if invalid
#'
#' @keywords internal
.validate_join_columns <- function(df1, df2, by) {
  # Check existence
  missing_df1 <- setdiff(by, names(df1))
  missing_df2 <- setdiff(by, names(df2))

  if (length(missing_df1) > 0 || length(missing_df2) > 0) {
    errors <- character()
    if (length(missing_df1) > 0) {
      errors <- c(errors,
                  glue::glue("Missing from df1: {paste(missing_df1, collapse = ', ')}"))
    }
    if (length(missing_df2) > 0) {
      errors <- c(errors,
                  glue::glue("Missing from df2: {paste(missing_df2, collapse = ', ')}"))
    }

    rlang::abort(
      c("Join column validation failed", x = errors),
      class = "ribits_join_error"
    )
  }

  # Check type compatibility
  for (col in by) {
    type1 <- class(df1[[col]])[1]
    type2 <- class(df2[[col]])[1]

    if (type1 != type2) {
      cli::cli_warn(c(
        "Join column '{col}' has different types",
        i = "df1: {type1}, df2: {type2}",
        i = "Join may produce unexpected results"
      ))
    }
  }

  invisible(NULL)
}
