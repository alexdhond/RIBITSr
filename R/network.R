# R/network.R
# Network handling with retry logic, progress tracking, and checkpointing

# Package-level network options
.network_options <- new.env(parent = emptyenv())
.network_options$max_retries <- 3
.network_options$retry_delay <- 2
.network_options$backoff_multiplier <- 2
.network_options$timeout <- 30
.network_options$checkpoint_dir <- NULL
.network_options$verbose <- TRUE


#' Configure network settings (Internal)
#'
#' @description
#' This function is internal. Network settings have sensible defaults.
#'
#' Set package-wide options for network behavior including retries,
#' timeouts, and checkpointing.
#'
#' @param max_retries Maximum number of retry attempts. Default 3.
#' @param retry_delay Initial delay between retries in seconds. Default 2.
#' @param backoff_multiplier Multiplier for exponential backoff. Default 2.
#' @param timeout Request timeout in seconds. Default 30.
#' @param checkpoint_dir Directory for saving checkpoints. If NULL, checkpoints
#'   are saved to a temp directory. Set to FALSE to disable checkpointing.
#' @param verbose Show detailed progress messages. Default TRUE.
#'
#' @return Invisibly returns current settings
#' @keywords internal
#' @examples
#' \dontrun{
#' # Increase retries for unreliable connection
#' rb_network_config(max_retries = 5, timeout = 60)
#'
#' # Enable checkpointing to specific directory
#' rb_network_config(checkpoint_dir = "data/checkpoints")
#'
#' # Disable checkpointing
#' rb_network_config(checkpoint_dir = FALSE)
#' }
rb_network_config <- function(max_retries = NULL,
                               retry_delay = NULL,
                               backoff_multiplier = NULL,
                               timeout = NULL,
                               checkpoint_dir = NULL,
                               verbose = NULL) {

  if (!is.null(max_retries)) .network_options$max_retries <- max_retries
  if (!is.null(retry_delay)) .network_options$retry_delay <- retry_delay
  if (!is.null(backoff_multiplier)) .network_options$backoff_multiplier <- backoff_multiplier
  if (!is.null(timeout)) .network_options$timeout <- timeout
  if (!is.null(checkpoint_dir)) .network_options$checkpoint_dir <- checkpoint_dir
  if (!is.null(verbose)) .network_options$verbose <- verbose

  invisible(list(
    max_retries = .network_options$max_retries,
    retry_delay = .network_options$retry_delay,
    backoff_multiplier = .network_options$backoff_multiplier,
    timeout = .network_options$timeout,
    checkpoint_dir = .network_options$checkpoint_dir,
    verbose = .network_options$verbose
  ))
}


#' Perform HTTP request with automatic retry
#'
#' Wraps httr2 requests with automatic retry logic, exponential backoff,
#' and detailed progress messaging.
#'
#' @param req An httr2 request object
#' @param description Description for progress messages
#' @param max_retries Override default max retries
#' @param timeout Override default timeout
#'
#' @return The response object, or NULL if all retries failed
#' @keywords internal
#' @noRd
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

  for (attempt in seq_len(max_retries)) {
    if (attempt > 1 && verbose) {
      cli::cli_alert_info(
        "Retry {attempt}/{max_retries} for {description} (waiting {round(retry_delay, 1)}s)..."
      )
      Sys.sleep(retry_delay)
      retry_delay <- retry_delay * backoff
    }

    result <- tryCatch({
      resp <- httr2::req_perform(req)

      if (httr2::resp_status(resp) >= 400) {
        last_error <- paste("HTTP", httr2::resp_status(resp))
        if (verbose) {
          cli::cli_alert_warning("{description}: {last_error}")
        }
        NULL
      } else {
        resp
      }
    },
    httr2_failure = function(e) {
      last_error <<- conditionMessage(e)
      if (verbose) {
        if (grepl("timed out|timeout", last_error, ignore.case = TRUE)) {
          cli::cli_alert_warning("{description}: Request timed out after {timeout}s")
        } else {
          cli::cli_alert_warning("{description}: {last_error}")
        }
      }
      NULL
    },
    error = function(e) {
      last_error <<- conditionMessage(e)
      if (verbose) {
        cli::cli_alert_warning("{description}: {last_error}")
      }
      NULL
    })

    if (!is.null(result)) {
      if (attempt > 1 && verbose) {
        cli::cli_alert_success("{description}: Succeeded on retry {attempt}")
      }
      return(result)
    }
  }

  # All retries failed
  if (verbose) {
    cli::cli_alert_danger(
      "{description}: Failed after {max_retries} attempts. Last error: {last_error}"
    )
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


# =============================================================================
# Checkpointing for bulk operations
# =============================================================================

#' Get checkpoint directory
#' @keywords internal
#' @noRd
.get_checkpoint_dir <- function() {
  dir <- .network_options$checkpoint_dir

  if (isFALSE(dir)) {
    return(NULL)
  }

  if (is.null(dir)) {
    dir <- file.path(tempdir(), "ribits_checkpoints")
  }

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  dir
}


#' Save checkpoint
#'
#' @param data Data to checkpoint
#' @param operation_id Unique identifier for this operation
#' @param completed_ids IDs that have been processed
#' @keywords internal
#' @noRd
.save_checkpoint <- function(data, operation_id, completed_ids) {
  dir <- .get_checkpoint_dir()
  if (is.null(dir)) return(invisible())

  checkpoint <- list(
    data = data,
    completed_ids = completed_ids,
    timestamp = Sys.time()
  )

  file <- file.path(dir, paste0(operation_id, ".rds"))
  saveRDS(checkpoint, file)

  if (.network_options$verbose) {
    cli::cli_alert_info(
      "Checkpoint saved: {length(completed_ids)} items completed"
    )
  }
}


#' Load checkpoint
#'
#' @param operation_id Unique identifier for this operation
#' @return List with data and completed_ids, or NULL if no checkpoint
#' @keywords internal
#' @noRd
.load_checkpoint <- function(operation_id) {
  dir <- .get_checkpoint_dir()
  if (is.null(dir)) return(NULL)

  file <- file.path(dir, paste0(operation_id, ".rds"))
  if (!file.exists(file)) return(NULL)

  checkpoint <- readRDS(file)

  if (.network_options$verbose) {
    cli::cli_alert_success(
      "Resuming from checkpoint: {length(checkpoint$completed_ids)} items already completed"
    )
  }

  checkpoint
}


#' Clear checkpoint
#'
#' @param operation_id Unique identifier for this operation
#' @keywords internal
#' @noRd
.clear_checkpoint <- function(operation_id) {
  dir <- .get_checkpoint_dir()
  if (is.null(dir)) return(invisible())

  file <- file.path(dir, paste0(operation_id, ".rds"))
  if (file.exists(file)) {
    unlink(file)
  }
}


#' List available checkpoints (Internal)
#'
#' @description
#' This function is internal. Checkpointing is handled automatically.
#'
#' Shows checkpoints from interrupted operations that can be resumed.
#'
#' @return A tibble of available checkpoints
#' @keywords internal
#' @examples
#' \dontrun{
#' rb_checkpoints()
#' }
rb_checkpoints <- function() {
  dir <- .get_checkpoint_dir()

  if (is.null(dir) || !dir.exists(dir)) {
    cli::cli_alert_info("No checkpoints available")
    return(tibble::tibble())
  }

  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(files) == 0) {
    cli::cli_alert_info("No checkpoints available")
    return(tibble::tibble())
  }

  checkpoints <- purrr::map_dfr(files, function(f) {
    cp <- readRDS(f)
    tibble::tibble(
      operation = tools::file_path_sans_ext(basename(f)),
      completed = length(cp$completed_ids),
      timestamp = cp$timestamp,
      file = f
    )
  })

  cli::cli_h2("Available Checkpoints")
  for (i in seq_len(nrow(checkpoints))) {
    cp <- checkpoints[i, ]
    cli::cli_alert_info(
      "{cp$operation}: {cp$completed} items completed at {format(cp$timestamp, '%Y-%m-%d %H:%M')}"
    )
  }

  invisible(checkpoints)
}


#' Clear all checkpoints (Internal)
#'
#' @description
#' This function is internal. Checkpoints are cleared automatically on success.
#'
#' @keywords internal
rb_clear_checkpoints <- function() {
  dir <- .get_checkpoint_dir()

  if (is.null(dir) || !dir.exists(dir)) {
    cli::cli_alert_info("No checkpoints to clear")
    return(invisible())
  }

  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) > 0) {
    unlink(files)
    cli::cli_alert_success("Cleared {length(files)} checkpoints")
  } else {
    cli::cli_alert_info("No checkpoints to clear")
  }

  invisible()
}


# =============================================================================
# Batch processing with checkpointing
# =============================================================================

#' Process items in batch with checkpointing
#'
#' Processes a list of items with automatic checkpointing, allowing
#' resumption if the operation is interrupted.
#'
#' @param items Vector of items to process (e.g., bank IDs)
#' @param process_fn Function to process each item. Should return data or NULL.
#' @param operation_id Unique identifier for checkpointing
#' @param batch_size Number of items to process before checkpointing. Default 10.
#' @param description Description for progress messages
#'
#' @return Combined results from all processed items
#' @keywords internal
#' @noRd
rb_batch_process <- function(items,
                              process_fn,
                              operation_id,
                              batch_size = 10,
                              description = "items") {

  verbose <- .network_options$verbose

  # Check for existing checkpoint
  checkpoint <- .load_checkpoint(operation_id)

  if (!is.null(checkpoint)) {
    results <- checkpoint$data
    completed <- checkpoint$completed_ids
    remaining <- setdiff(items, completed)

    if (length(remaining) == 0) {
      if (verbose) cli::cli_alert_success("All {description} already completed")
      .clear_checkpoint(operation_id)
      return(results)
    }

    if (verbose) {
      cli::cli_alert_info(
        "Resuming: {length(remaining)} {description} remaining"
      )
    }
  } else {
    results <- list()
    completed <- character()
    remaining <- items
  }

  # Progress bar
  n_total <- length(items)
  n_done <- length(completed)

  if (verbose) {
    pb <- cli::cli_progress_bar(
      total = n_total,
      format = paste0(
        "Processing {description} ",
        "[{cli::pb_current}/{cli::pb_total}] ",
        "{cli::pb_bar} {cli::pb_percent} ",
        "| Failures: {n_failures}"
      )
    )
    cli::cli_progress_update(id = pb, set = n_done)
  }

  n_failures <- 0
  batch_count <- 0

  for (item in remaining) {
    # Process item
    result <- tryCatch({
      process_fn(item)
    }, error = function(e) {
      n_failures <<- n_failures + 1
      NULL
    })

    if (!is.null(result)) {
      results[[length(results) + 1]] <- result
    } else {
      n_failures <- n_failures + 1
    }

    completed <- c(completed, item)
    batch_count <- batch_count + 1

    if (verbose) {
      cli::cli_progress_update(id = pb, inc = 1)
    }

    # Checkpoint periodically
    if (batch_count >= batch_size) {
      .save_checkpoint(results, operation_id, completed)
      batch_count <- 0
    }
  }

  if (verbose) {
    cli::cli_progress_done(id = pb)
  }

  # Clear checkpoint on successful completion
  .clear_checkpoint(operation_id)

  # Summary
  if (verbose) {
    n_success <- length(results)
    if (n_failures > 0) {
      cli::cli_alert_warning(
        "Completed: {n_success} succeeded, {n_failures} failed"
      )
      cli::cli_alert_info("Use rb_network_failures() to see details")
    } else {
      cli::cli_alert_success("Completed: {n_success} {description} processed")
    }
  }

  results
}
