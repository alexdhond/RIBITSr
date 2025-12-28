# R/network-retry.R
# Request retry logic and network failure tracking
# Split from R/network.R (628 lines → focused modules)

rb_request_with_retry <- function(req,
                                   description = "API request",
                                   max_retries = NULL,
                                   timeout = NULL) {

  max_retries <- max_retries %||% .network_options$max_retries
 timeout <- timeout %||% .network_options$timeout
  retry_delay <- .network_options$retry_delay
  backoff <- .network_options$backoff_multiplier
  verbose <- .network_options$verbose

  # Add timeout to request
  req <- req |> httr2::req_timeout(timeout)

  last_error <- NULL
  last_status_code <- NULL
  is_permanent_error <- FALSE

  for (attempt in seq_len(max_retries)) {
    if (attempt > 1 && verbose) {
      cli::cli_alert_info(
        "Retry {attempt}/{max_retries} for {description} (waiting {round(retry_delay, 1)}s)..."
      )
      Sys.sleep(retry_delay)
      retry_delay <- retry_delay * backoff
    }

    # Apply rate limiting before making request
    .apply_rate_limit()

    result_info <- tryCatch({
      resp <- httr2::req_perform(req)

      if (httr2::resp_status(resp) >= 400) {
        status_code <- httr2::resp_status(resp)
        error_msg <- paste("HTTP", status_code)

        # Check if this is a retryable error
        is_perm <- !.is_retryable_error(error_msg, status_code)
        if (is_perm) {
          if (verbose) {
            cli::cli_alert_danger("{description}: {error_msg} (permanent error, not retrying)")
          }
        } else if (verbose) {
          retry_msg <- if (attempt < max_retries) " (will retry)" else ""
          cli::cli_alert_warning("{description}: {error_msg}{retry_msg}")
        }

        list(response = NULL, error = error_msg, status_code = status_code, is_permanent = is_perm)
      } else {
        list(response = resp, error = NULL, status_code = NULL, is_permanent = FALSE)
      }
    },
    httr2_failure = function(e) {
      error_msg <- conditionMessage(e)
      status_code <- NULL

      # Try to extract status code from error message
      if (grepl("HTTP (\\d{3})", error_msg)) {
        status_code <- as.integer(sub(".*HTTP (\\d{3}).*", "\\1", error_msg))
      }

      # Check if retryable
      is_perm <- !.is_retryable_error(error_msg, status_code)
      if (is_perm) {
        if (verbose) {
          cli::cli_alert_danger("{description}: {error_msg} (permanent error, not retrying)")
        }
      } else if (verbose) {
        if (grepl("timed out|timeout", error_msg, ignore.case = TRUE)) {
          retry_msg <- if (attempt < max_retries) " (will retry)" else ""
          cli::cli_alert_warning("{description}: Request timed out after {timeout}s{retry_msg}")
        } else {
          retry_msg <- if (attempt < max_retries) " (will retry)" else ""
          cli::cli_alert_warning("{description}: {error_msg}{retry_msg}")
        }
      }

      list(response = NULL, error = error_msg, status_code = status_code, is_permanent = is_perm)
    },
    error = function(e) {
      error_msg <- conditionMessage(e)

      # Generic errors are usually retryable
      if (verbose) {
        retry_msg <- if (attempt < max_retries) " (will retry)" else ""
        cli::cli_alert_warning("{description}: {error_msg}{retry_msg}")
      }

      list(response = NULL, error = error_msg, status_code = NULL, is_permanent = FALSE)
    })

    # Update state from result_info
    result <- result_info$response
    last_error <- result_info$error
    last_status_code <- result_info$status_code
    is_permanent_error <- result_info$is_permanent

    if (!is.null(result)) {
      if (attempt > 1 && verbose) {
        cli::cli_alert_success("{description}: Succeeded on retry {attempt}")
      }
      return(result)
    }

    # Break early if permanent error
    if (is_permanent_error) {
      break
    }
  }

  # All retries failed or permanent error
  if (verbose) {
    if (is_permanent_error) {
      cli::cli_alert_danger(
        "{description}: Permanent error, cannot retry. Error: {last_error}"
      )
    } else {
      cli::cli_alert_danger(
        "{description}: Failed after {max_retries} attempts. Last error: {last_error}"
      )
    }
  }

  # Log the failure
  .log_network_failure(description, last_error, max_retries)

  NULL
}


#' Network failure log
#' @keywords internal
#' @noRd
.network_failures <- new.env(parent = emptyenv())
.network_failures$log <- NULL

.log_network_failure <- function(description, error, attempts) {
  entry <- tibble::tibble(
    timestamp = Sys.time(),
    description = description,
    error = error,
    attempts = attempts
  )

  if (is.null(.network_failures$log)) {
    .network_failures$log <- entry
  } else {
    .network_failures$log <- dplyr::bind_rows(.network_failures$log, entry)
  }
}


#' View network failure log (Internal)
#'
#' @description
#' This function is internal. Network failures are logged automatically.
#'
#' Shows all network failures that occurred during the session.
#'
#' @param clear If TRUE, clears the log after displaying. Default FALSE.
#' @return A tibble of network failures
#' @keywords internal
#' @examples
#' \dontrun{
#' rb_network_failures()
#' rb_network_failures(clear = TRUE)
#' }
rb_network_failures <- function(clear = FALSE) {
  log <- .network_failures$log

  if (is.null(log) || nrow(log) == 0) {
    cli::cli_alert_success("No network failures logged")
    return(tibble::tibble())
  }

  cli::cli_h2("Network Failures")
  for (i in seq_len(nrow(log))) {
    entry <- log[i, ]
    cli::cli_alert_warning(
      "[{format(entry$timestamp, '%H:%M:%S')}] {entry$description}: {entry$error} ({entry$attempts} attempts)"
    )
  }

  if (clear) {
    .network_failures$log <- NULL
    cli::cli_alert_info("Failure log cleared")
  }

  invisible(log)
}


