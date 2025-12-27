#' RIBITSr Error Classes
#'
#' Structured error handling for common failure modes in the RIBITSr package.
#' Using specific error classes allows users to catch and handle different
#' error types appropriately.
#'
#' @section Error Hierarchy:
#' All RIBITSr errors inherit from `ribits_error`:
#' * `ribits_network_error` - API/network failures
#' * `ribits_data_error` - Data quality/validation issues
#' * `ribits_config_error` - Configuration problems
#' * `ribits_column_error` - Column access/manipulation issues
#' * `ribits_join_error` - Data merging problems
#' * `ribits_parse_error` - Data parsing failures
#' * `ribits_spatial_error` - Spatial data issues
#'
#' @section Usage:
#' Catch specific error types:
#' ```r
#' tryCatch(
#'   ribits(state = "XX"),
#'   ribits_network_error = function(e) {
#'     message("Network issue - check connection")
#'   },
#'   ribits_data_error = function(e) {
#'     message("Data quality issue - check source")
#'   }
#' )
#' ```
#'
#' @name ribits-errors
#' @keywords internal
NULL

#' Base RIBITSr Error
#'
#' Creates a base ribits_error that all other errors inherit from.
#'
#' @param message Error message (character vector for bullet points)
#' @param class Additional error classes (character vector)
#' @param ... Additional fields to include in error object
#' @param call Calling environment (default: parent frame)
#'
#' @return Nothing - throws an error
#'
#' @keywords internal
.ribits_error <- function(message, class = character(), ..., call = NULL) {
  if (is.null(call)) {
    call <- rlang::caller_env()
  }

  rlang::abort(
    message,
    class = c(class, "ribits_error"),
    ...,
    call = call
  )
}

#' Network Error
#'
#' Thrown when API requests fail, connections timeout, or rate limits are hit.
#'
#' @inheritParams .ribits_error
#' @param status_code HTTP status code if applicable
#' @param url URL that failed if applicable
#'
#' @examples
#' \dontrun{
#' .network_error(
#'   c(
#'     "Failed to connect to RIBITS API",
#'     i = "Check connection with check_ribits_connection()",
#'     i = "Enable verbose mode for details: rb_config(verbose = TRUE)"
#'   ),
#'   status_code = 500,
#'   url = "https://ribits.usace.army.mil/..."
#' )
#' }
#'
#' @keywords internal
.network_error <- function(message, ..., status_code = NULL, url = NULL,
                          call = NULL) {
  if (is.null(call)) {
    call <- rlang::caller_env()
  }

  .ribits_error(
    message,
    class = "ribits_network_error",
    status_code = status_code,
    url = url,
    ...,
    call = call
  )
}

#' Data Quality Error
#'
#' Thrown when data validation fails, discrepancies are detected, or
#' data integrity checks fail.
#'
#' @inheritParams .ribits_error
#' @param data_source Which data source had the issue
#' @param validation_type Type of validation that failed
#'
#' @examples
#' \dontrun{
#' .data_error(
#'   c(
#'     "Transaction data validation failed",
#'     x = "Found 15 transactions with negative credits",
#'     i = "Run rb_diagnose() for details"
#'   ),
#'   data_source = "CSV",
#'   validation_type = "transaction_validation"
#' )
#' }
#'
#' @keywords internal
.data_error <- function(message, ..., data_source = NULL,
                       validation_type = NULL, call = NULL) {
  if (is.null(call)) {
    call <- rlang::caller_env()
  }

  .ribits_error(
    message,
    class = "ribits_data_error",
    data_source = data_source,
    validation_type = validation_type,
    ...,
    call = call
  )
}

#' Configuration Error
#'
#' Thrown when package configuration is invalid or conflicting.
#'
#' @inheritParams .ribits_error
#' @param config_param Which configuration parameter caused the issue
#'
#' @examples
#' \dontrun{
#' .config_error(
#'   c(
#'     "Invalid rate_limit value",
#'     x = "rate_limit must be positive",
#'     i = "Set with rb_config(rate_limit = 5)"
#'   ),
#'   config_param = "rate_limit"
#' )
#' }
#'
#' @keywords internal
.config_error <- function(message, ..., config_param = NULL, call = NULL) {
  if (is.null(call)) {
    call <- rlang::caller_env()
  }

  .ribits_error(
    message,
    class = "ribits_config_error",
    config_param = config_param,
    ...,
    call = call
  )
}

#' Column Access Error
#'
#' Thrown when column access, manipulation, or validation fails.
#'
#' @inheritParams .ribits_error
#' @param column_name Name of the problematic column
#' @param available_columns Available column names
#'
#' @examples
#' \dontrun{
#' .column_error(
#'   c(
#'     "Required column 'bank_id' not found",
#'     i = "Available columns: {paste(names(df), collapse = ', ')}"
#'   ),
#'   column_name = "bank_id",
#'   available_columns = names(df)
#' )
#' }
#'
#' @keywords internal
.column_error <- function(message, ..., column_name = NULL,
                         available_columns = NULL, call = NULL) {
  if (is.null(call)) {
    call <- rlang::caller_env()
  }

  .ribits_error(
    message,
    class = "ribits_column_error",
    column_name = column_name,
    available_columns = available_columns,
    ...,
    call = call
  )
}

#' Join Operation Error
#'
#' Thrown when data joining/merging fails.
#'
#' @inheritParams .ribits_error
#' @param join_type Type of join (full, left, inner, etc.)
#' @param join_columns Columns used for joining
#'
#' @examples
#' \dontrun{
#' .join_error(
#'   c(
#'     "Join operation failed",
#'     x = "Join columns have incompatible types",
#'     i = "df1$id is character, df2$id is numeric"
#'   ),
#'   join_type = "full",
#'   join_columns = "id"
#' )
#' }
#'
#' @keywords internal
.join_error <- function(message, ..., join_type = NULL, join_columns = NULL,
                       call = NULL) {
  if (is.null(call)) {
    call <- rlang::caller_env()
  }

  .ribits_error(
    message,
    class = "ribits_join_error",
    join_type = join_type,
    join_columns = join_columns,
    ...,
    call = call
  )
}

#' Parse Error
#'
#' Thrown when parsing CSV files, API responses, or other data formats fails.
#'
#' @inheritParams .ribits_error
#' @param file_path Path to file that failed to parse
#' @param format Format that failed (CSV, JSON, etc.)
#'
#' @examples
#' \dontrun{
#' .parse_error(
#'   c(
#'     "Failed to parse CSV file",
#'     x = "File contains HTML error page",
#'     i = "This usually indicates API returned an error"
#'   ),
#'   file_path = "data.csv",
#'   format = "CSV"
#' )
#' }
#'
#' @keywords internal
.parse_error <- function(message, ..., file_path = NULL, format = NULL,
                        call = NULL) {
  if (is.null(call)) {
    call <- rlang::caller_env()
  }

  .ribits_error(
    message,
    class = "ribits_parse_error",
    file_path = file_path,
    format = format,
    ...,
    call = call
  )
}

#' Spatial Data Error
#'
#' Thrown when spatial data operations fail (geometry, CRS, etc.).
#'
#' @inheritParams .ribits_error
#' @param geometry_type Type of geometry involved
#' @param crs Coordinate reference system if relevant
#'
#' @examples
#' \dontrun{
#' .spatial_error(
#'   c(
#'     "Invalid geometry detected",
#'     x = "Geometry is not valid: self-intersection",
#'     i = "Try sf::st_make_valid() to fix"
#'   ),
#'   geometry_type = "POLYGON"
#' )
#' }
#'
#' @keywords internal
.spatial_error <- function(message, ..., geometry_type = NULL, crs = NULL,
                          call = NULL) {
  if (is.null(call)) {
    call <- rlang::caller_env()
  }

  .ribits_error(
    message,
    class = "ribits_spatial_error",
    geometry_type = geometry_type,
    crs = crs,
    ...,
    call = call
  )
}

#' Check if Error is a RIBITSr Error
#'
#' @param error Error object to check
#' @return Logical
#'
#' @examples
#' \dontrun{
#' tryCatch(
#'   some_function(),
#'   error = function(e) {
#'     if (is_ribits_error(e)) {
#'       # Handle ribits-specific error
#'     } else {
#'       # Handle other errors
#'     }
#'   }
#' )
#' }
#'
#' @keywords internal
is_ribits_error <- function(error) {
  inherits(error, "ribits_error")
}

#' Get Error Type
#'
#' Returns the specific type of RIBITSr error.
#'
#' @param error Error object
#' @return Character string of error type, or NULL if not a RIBITSr error
#'
#' @keywords internal
get_error_type <- function(error) {
  if (!is_ribits_error(error)) {
    return(NULL)
  }

  error_classes <- c(
    "ribits_network_error",
    "ribits_data_error",
    "ribits_config_error",
    "ribits_column_error",
    "ribits_join_error",
    "ribits_parse_error",
    "ribits_spatial_error"
  )

  for (error_class in error_classes) {
    if (inherits(error, error_class)) {
      return(error_class)
    }
  }

  return("ribits_error")
}

#' Format Error for User Display
#'
#' Formats a RIBITSr error with helpful context for troubleshooting.
#'
#' @param error Error object
#' @return Formatted error message
#'
#' @keywords internal
format_error <- function(error) {
  if (!is_ribits_error(error)) {
    return(conditionMessage(error))
  }

  msg <- conditionMessage(error)
  error_type <- get_error_type(error)

  # Add type-specific troubleshooting tips
  tips <- switch(error_type,
    ribits_network_error = c(
      i = "Check connection: check_ribits_connection()",
      i = "Enable verbose mode: rb_config(verbose = TRUE)"
    ),
    ribits_data_error = c(
      i = "Diagnose data issues: rb_diagnose(data)",
      i = "Check data sources are accessible"
    ),
    ribits_config_error = c(
      i = "View current config: rb_config()",
      i = "Reset to defaults: rb_config(reset = TRUE)"
    ),
    ribits_column_error = c(
      i = "Check column names: names(data)",
      i = "Column matching is case-insensitive by default"
    ),
    ribits_join_error = c(
      i = "Verify join columns exist in both dataframes",
      i = "Check for type mismatches in join columns"
    ),
    ribits_parse_error = c(
      i = "Check if data source returned error page",
      i = "Try downloading file manually to inspect"
    ),
    ribits_spatial_error = c(
      i = "Check geometry validity: sf::st_is_valid(geom)",
      i = "Verify CRS is correct: sf::st_crs(geom)"
    ),
    character()
  )

  if (length(tips) > 0) {
    cli::cli_abort(c(msg, tips))
  } else {
    cli::cli_abort(msg)
  }
}
