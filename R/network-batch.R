# R/network-batch.R
# Checkpointing and batch processing for bulk operations
# Split from R/network.R (628 lines → focused modules)

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
# Step-based checkpointing for engine operations
# =============================================================================

#' Save step checkpoint (for engine operations)
#'
#' @param operation_id Unique identifier for this operation
#' @param result Current result object
#' @param step Step number completed
#' @param description Optional step description
#' @keywords internal
#' @noRd
.save_step_checkpoint <- function(operation_id, result, step, description = NULL) {
  dir <- .get_checkpoint_dir()
  if (is.null(dir)) return(invisible())

  checkpoint <- list(
    result = result,
    step = step,
    description = description,
    timestamp = Sys.time()
  )

  file <- file.path(dir, paste0("engine_", operation_id, ".rds"))
  saveRDS(checkpoint, file)
}


#' Load step checkpoint (for engine operations)
#'
#' @param operation_id Unique identifier for this operation
#' @return List with result, step, and timestamp, or NULL if no checkpoint
#' @keywords internal
#' @noRd
.load_step_checkpoint <- function(operation_id) {
  dir <- .get_checkpoint_dir()
  if (is.null(dir)) return(NULL)

  file <- file.path(dir, paste0("engine_", operation_id, ".rds"))
  if (!file.exists(file)) return(NULL)

  readRDS(file)
}


#' Clear step checkpoint (for engine operations)
#'
#' @param operation_id Unique identifier for this operation
#' @keywords internal
#' @noRd
.clear_step_checkpoint <- function(operation_id) {
  dir <- .get_checkpoint_dir()
  if (is.null(dir)) return(invisible())

  file <- file.path(dir, paste0("engine_", operation_id, ".rds"))
  if (file.exists(file)) {
    unlink(file)
  }
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
    result_info <- tryCatch({
      list(data = process_fn(item), error = FALSE)
    }, error = function(e) {
      list(data = NULL, error = TRUE)
    })

    result <- result_info$data
    if (result_info$error) {
      n_failures <- n_failures + 1
    }

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
