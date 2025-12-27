#' Message Wrapper Utilities
#'
#' Consistent user-facing messaging functions that respect verbosity settings
#' and provide a unified interface for all package communications.
#'
#' @name message-wrappers
#' @keywords internal
NULL

#' Get Current Verbosity Setting
#'
#' Checks if messages should be displayed based on package configuration.
#'
#' @param quietly Override for the current call
#' @return Logical - TRUE if messages should be suppressed
#' @keywords internal
.should_be_quiet <- function(quietly = NULL) {
  if (!is.null(quietly)) {
    return(quietly)
  }

  # Check package config if available
  config <- tryCatch(
    .get_config(),
    error = function(e) list(verbose = TRUE)
  )

  !isTRUE(config$verbose)
}

#' Informational Message
#'
#' Display an informational message to the user.
#'
#' @param msg Message text (supports glue syntax)
#' @param ... Additional arguments passed to cli::cli_alert_info
#' @param quietly If TRUE, suppress message
#' @param .envir Environment for glue interpolation
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' .msg_info("Processing {n} banks", n = 100)
#' .msg_info("Starting analysis", quietly = FALSE)
#' }
.msg_info <- function(msg, ..., quietly = NULL, .envir = parent.frame()) {
  if (.should_be_quiet(quietly)) {
    return(invisible(NULL))
  }

  cli::cli_alert_info(msg, ..., .envir = .envir)
}

#' Success Message
#'
#' Display a success message to the user.
#'
#' @param msg Message text (supports glue syntax)
#' @param ... Additional arguments passed to cli::cli_alert_success
#' @param quietly If TRUE, suppress message
#' @param .envir Environment for glue interpolation
#'
#' @keywords internal
.msg_success <- function(msg, ..., quietly = NULL, .envir = parent.frame()) {
  if (.should_be_quiet(quietly)) {
    return(invisible(NULL))
  }

  cli::cli_alert_success(msg, ..., .envir = .envir)
}

#' Warning Message
#'
#' Display a warning message to the user.
#'
#' @param msg Message text (supports glue syntax)
#' @param ... Additional arguments passed to cli::cli_alert_warning
#' @param quietly If TRUE, suppress message
#' @param .envir Environment for glue interpolation
#'
#' @keywords internal
.msg_warn <- function(msg, ..., quietly = NULL, .envir = parent.frame()) {
  if (.should_be_quiet(quietly)) {
    return(invisible(NULL))
  }

  cli::cli_alert_warning(msg, ..., .envir = .envir)
}

#' Danger/Error Message
#'
#' Display a danger/error message to the user (does not throw error).
#'
#' @param msg Message text (supports glue syntax)
#' @param ... Additional arguments passed to cli::cli_alert_danger
#' @param quietly If TRUE, suppress message
#' @param .envir Environment for glue interpolation
#'
#' @keywords internal
.msg_danger <- function(msg, ..., quietly = NULL, .envir = parent.frame()) {
  if (.should_be_quiet(quietly)) {
    return(invisible(NULL))
  }

  cli::cli_alert_danger(msg, ..., .envir = .envir)
}

#' Progress Bar Wrapper
#'
#' Creates a progress bar that respects verbosity settings.
#'
#' @param total Total number of items
#' @param format Progress bar format string
#' @param ... Additional arguments passed to cli::cli_progress_bar
#' @param quietly If TRUE, suppress progress bar
#'
#' @return Progress bar ID (or NULL if quiet)
#' @keywords internal
.msg_progress_bar <- function(total, format = NULL, ..., quietly = NULL) {
  if (.should_be_quiet(quietly)) {
    return(NULL)
  }

  cli::cli_progress_bar(total = total, format = format, ...)
}

#' Update Progress Bar
#'
#' @param id Progress bar ID (from .msg_progress_bar)
#' @param ... Arguments passed to cli::cli_progress_update
#' @param quietly If TRUE, suppress update
#'
#' @keywords internal
.msg_progress_update <- function(id = NULL, ..., quietly = NULL) {
  if (.should_be_quiet(quietly) || is.null(id)) {
    return(invisible(NULL))
  }

  cli::cli_progress_update(id = id, ...)
}

#' Complete Progress Bar
#'
#' @param id Progress bar ID (from .msg_progress_bar)
#' @param quietly If TRUE, suppress completion
#'
#' @keywords internal
.msg_progress_done <- function(id = NULL, quietly = NULL) {
  if (.should_be_quiet(quietly) || is.null(id)) {
    return(invisible(NULL))
  }

  cli::cli_progress_done(id = id)
}

#' Network Request Message
#'
#' Standardized message for network requests (retry notifications).
#'
#' @param description Request description
#' @param attempt Current attempt number
#' @param max_attempts Maximum attempts
#' @param delay Delay in seconds
#' @param quietly If TRUE, suppress message
#'
#' @keywords internal
.msg_retry <- function(description, attempt, max_attempts, delay = NULL,
                       quietly = NULL) {
  if (.should_be_quiet(quietly)) {
    return(invisible(NULL))
  }

  if (!is.null(delay)) {
    .msg_info(
      "Retry {attempt}/{max_attempts} for {description} (waiting {round(delay, 1)}s)...",
      .envir = environment()
    )
  } else {
    .msg_info(
      "Retry {attempt}/{max_attempts} for {description}",
      .envir = environment()
    )
  }
}

#' Network Failure Message
#'
#' Standardized message for network failures.
#'
#' @param description Request description
#' @param error Error message
#' @param max_attempts Maximum attempts made
#' @param quietly If TRUE, suppress message
#'
#' @keywords internal
.msg_network_failure <- function(description, error, max_attempts,
                                 quietly = NULL) {
  if (.should_be_quiet(quietly)) {
    return(invisible(NULL))
  }

  .msg_danger(
    "{description}: Failed after {max_attempts} attempts. Last error: {error}",
    .envir = environment()
  )
}

#' Data Quality Message
#'
#' Standardized message for data quality issues.
#'
#' @param issue_type Type of issue (e.g., "Missing values", "Duplicates")
#' @param count Number of issues found
#' @param total Total number of records
#' @param severity Severity level ("info", "warning", "danger")
#' @param quietly If TRUE, suppress message
#'
#' @keywords internal
.msg_data_quality <- function(issue_type, count, total,
                              severity = "warning", quietly = NULL) {
  if (.should_be_quiet(quietly)) {
    return(invisible(NULL))
  }

  pct <- round(count / total * 100, 1)
  msg <- "{issue_type} in {count} rows ({pct}%)"

  switch(severity,
    info = .msg_info(msg, .envir = environment()),
    warning = .msg_warn(msg, .envir = environment()),
    danger = .msg_danger(msg, .envir = environment()),
    .msg_info(msg, .envir = environment())
  )
}

#' Checkpoint Message
#'
#' Standardized message for checkpoint operations.
#'
#' @param action Action being performed ("saved", "loaded", "cleared")
#' @param n_items Number of items in checkpoint
#' @param quietly If TRUE, suppress message
#'
#' @keywords internal
.msg_checkpoint <- function(action = "saved", n_items = NULL, quietly = NULL) {
  if (.should_be_quiet(quietly)) {
    return(invisible(NULL))
  }

  msg <- switch(action,
    saved = if (!is.null(n_items)) {
      "Checkpoint saved ({n_items} items)"
    } else {
      "Checkpoint saved"
    },
    loaded = if (!is.null(n_items)) {
      "Checkpoint loaded ({n_items} items)"
    } else {
      "Checkpoint loaded"
    },
    cleared = "Checkpoint cleared",
    "Checkpoint {action}"
  )

  .msg_info(msg, .envir = environment())
}

#' API Rate Limit Message
#'
#' Message for API rate limiting delays.
#'
#' @param delay Delay in seconds
#' @param quietly If TRUE, suppress message
#'
#' @keywords internal
.msg_rate_limit <- function(delay, quietly = NULL) {
  if (.should_be_quiet(quietly)) {
    return(invisible(NULL))
  }

  # Only show if delay is significant (>1 second)
  if (delay > 1) {
    .msg_info("Rate limiting: waiting {round(delay, 1)}s", .envir = environment())
  }
}

#' Validation Summary Message
#'
#' Display a summary of validation results.
#'
#' @param n_passed Number of checks passed
#' @param n_warnings Number of warnings
#' @param n_errors Number of errors
#' @param quietly If TRUE, suppress message
#'
#' @keywords internal
.msg_validation_summary <- function(n_passed, n_warnings, n_errors,
                                    quietly = NULL) {
  if (.should_be_quiet(quietly)) {
    return(invisible(NULL))
  }

  total <- n_passed + n_warnings + n_errors

  if (n_errors > 0) {
    .msg_danger(
      "Validation: {n_passed}/{total} passed, {n_warnings} warnings, {n_errors} errors",
      .envir = environment()
    )
  } else if (n_warnings > 0) {
    .msg_warn(
      "Validation: {n_passed}/{total} passed, {n_warnings} warnings",
      .envir = environment()
    )
  } else {
    .msg_success(
      "Validation: All {n_passed} checks passed",
      .envir = environment()
    )
  }
}

#' Batch Operation Summary
#'
#' Display a summary of batch operation results.
#'
#' @param n_success Number of successful operations
#' @param n_failed Number of failed operations
#' @param operation_name Name of the operation (e.g., "banks fetched", "reports downloaded")
#' @param quietly If TRUE, suppress message
#'
#' @keywords internal
.msg_batch_summary <- function(n_success, n_failed, operation_name,
                               quietly = NULL) {
  if (.should_be_quiet(quietly)) {
    return(invisible(NULL))
  }

  total <- n_success + n_failed

  if (n_failed == 0) {
    .msg_success(
      "{operation_name}: {n_success}/{total} successful",
      .envir = environment()
    )
  } else if (n_success == 0) {
    .msg_danger(
      "{operation_name}: {n_failed}/{total} failed",
      .envir = environment()
    )
  } else {
    .msg_warn(
      "{operation_name}: {n_success}/{total} successful, {n_failed} failed",
      .envir = environment()
    )
  }
}

#' Format Number with Commas
#'
#' Helper to format large numbers with thousand separators.
#'
#' @param x Numeric value
#' @return Character string with formatted number
#' @keywords internal
.format_number <- function(x) {
  format(x, big.mark = ",", scientific = FALSE)
}

#' Format File Size
#'
#' Helper to format file sizes in human-readable format.
#'
#' @param bytes File size in bytes
#' @return Character string like "1.5 MB"
#' @keywords internal
.format_file_size <- function(bytes) {
  if (bytes < 1024) {
    return(paste(bytes, "bytes"))
  } else if (bytes < 1024^2) {
    return(sprintf("%.1f KB", bytes / 1024))
  } else if (bytes < 1024^3) {
    return(sprintf("%.1f MB", bytes / 1024^2))
  } else {
    return(sprintf("%.1f GB", bytes / 1024^3))
  }
}

#' Format Duration
#'
#' Helper to format durations in human-readable format.
#'
#' @param seconds Duration in seconds
#' @return Character string like "2.5 minutes"
#' @keywords internal
.format_duration <- function(seconds) {
  if (seconds < 60) {
    return(sprintf("%.1f seconds", seconds))
  } else if (seconds < 3600) {
    return(sprintf("%.1f minutes", seconds / 60))
  } else {
    return(sprintf("%.1f hours", seconds / 3600))
  }
}
