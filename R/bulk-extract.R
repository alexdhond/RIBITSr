# R/bulk-extract.R
# Functions for bulk extraction of RIBITS data via API

#' Extract all ledger/transaction data via API (Internal)
#'
#' @description
#' This function is internal and called by `rb_transactions()` and `ribits()`.
#'
#' Fetches ledger data for all banks (or a subset) directly from the RIBITS API.
#' This provides transaction-level data including permittee, permit numbers,
#' impact HUC, and credit classification.
#'
#' Features automatic retry on network failures and checkpointing to resume
#' interrupted downloads.
#'
#' @param bank_ids Optional vector of bank IDs. If NULL, fetches all banks.
#' @param state Optional state filter (e.g., "CA", "TX")
#' @param district Optional USACE district filter
#' @param progress Show progress bar. Default TRUE.
#' @param checkpoint Enable checkpointing to resume interrupted downloads.
#'   Default TRUE.
#' @param checkpoint_every Save checkpoint every N banks. Default 10.
#'
#' @return A tibble with all ledger transactions across banks
#' @keywords internal
#' @examples
#' \dontrun{
#' # Recommended: Use ribits() instead
#' ca <- ribits(state = "CA", transactions = "comprehensive")
#'
#' # Internal usage
#' ca_ledger <- rb_bulk_ledger(state = "CA")
#' }
rb_bulk_ledger <- function(bank_ids = NULL,
                            state = NULL,
                            district = NULL,
                            progress = TRUE,
                            checkpoint = TRUE,
                            checkpoint_every = 10) {

  # If no bank_ids provided, get list from API
  if (is.null(bank_ids)) {
    cli::cli_alert_info("Fetching bank list...")
    banks <- rb_get("banks", state = state, district = district)

    if (is.null(banks) || nrow(banks) == 0) {
      cli::cli_alert_warning("Could not fetch bank list")
      return(tibble::tibble())
    }

    # Handle different column name conventions
    bank_ids <- if ("bank_id" %in% names(banks)) banks$bank_id else banks$BANK_ID
    cli::cli_alert_success("Found {length(bank_ids)} banks")
  }

  if (length(bank_ids) == 0) {
    cli::cli_alert_warning("No banks to process")
    return(tibble::tibble())
  }

  # Create operation ID for checkpointing
  op_id <- paste0("ledger_", paste(head(sort(bank_ids), 3), collapse = "_"))

  # Check for existing checkpoint
  all_ledger <- list()
  completed_ids <- integer()
  n_failures <- 0

  if (checkpoint) {
    existing <- .load_checkpoint(op_id)
    if (!is.null(existing)) {
      all_ledger <- existing$data
      completed_ids <- existing$completed_ids
      cli::cli_alert_success(
        "Resuming from checkpoint: {length(completed_ids)}/{length(bank_ids)} banks already processed"
      )
    }
  }

  # Filter to remaining banks
  remaining_ids <- setdiff(bank_ids, completed_ids)

  if (length(remaining_ids) == 0) {
    cli::cli_alert_success("All banks already processed")
    if (checkpoint) .clear_checkpoint(op_id)
    return(dplyr::bind_rows(all_ledger))
  }

  cli::cli_alert_info("Processing {length(remaining_ids)} banks...")

  # Progress bar
  if (progress) {
    pb <- cli::cli_progress_bar(
      total = length(bank_ids),
      format = paste0(
        "Extracting ledger data ",
        "[{cli::pb_current}/{cli::pb_total}] ",
        "{cli::pb_bar} {cli::pb_percent} ",
        "| {n_failures} failures"
      )
    )
    cli::cli_progress_update(id = pb, set = length(completed_ids))
  }

  batch_count <- 0

  for (bank_id in remaining_ids) {
    # Get bank data with ledger
    bank_data <- tryCatch({
      rb_get("banks", id = bank_id, ledger = TRUE,
             service_area = FALSE, footprint = FALSE, contacts = FALSE)
    }, error = function(e) {
      n_failures <<- n_failures + 1
      NULL
    })

    if (!is.null(bank_data) && !is.null(bank_data$ledger) &&
        nrow(bank_data$ledger) > 0) {
      # Add bank_id to ledger
      ledger <- bank_data$ledger
      ledger$bank_id <- bank_id

      # Add bank name if available
      if (!is.null(bank_data$summary) && "bank_name" %in% names(bank_data$summary)) {
        ledger$bank_name <- bank_data$summary$bank_name[1]
      }

      all_ledger[[length(all_ledger) + 1]] <- ledger
    }

    completed_ids <- c(completed_ids, bank_id)
    batch_count <- batch_count + 1

    if (progress) cli::cli_progress_update(id = pb, inc = 1)

    # Checkpoint periodically
    if (checkpoint && batch_count >= checkpoint_every) {
      .save_checkpoint(all_ledger, op_id, completed_ids)
      batch_count <- 0
    }

    # Rate limiting
    Sys.sleep(0.1)
  }

  if (progress) cli::cli_progress_done(id = pb)

  # Clear checkpoint on successful completion
  if (checkpoint) .clear_checkpoint(op_id)

  # Combine all ledgers
  if (length(all_ledger) > 0) {
    result <- dplyr::bind_rows(all_ledger)

    if (n_failures > 0) {
      cli::cli_alert_warning(
        "Extracted {nrow(result)} transactions from {length(all_ledger)} banks ({n_failures} failures)"
      )
      cli::cli_alert_info("Use rb_network_failures() to see failed requests")
    } else {
      cli::cli_alert_success(
        "Extracted {nrow(result)} transactions from {length(all_ledger)} banks"
      )
    }

    return(result)
  } else {
    cli::cli_alert_warning("No ledger data found")
    return(tibble::tibble())
  }
}


