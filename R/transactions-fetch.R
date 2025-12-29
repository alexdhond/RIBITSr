# R/transactions-fetch.R
# Transaction data fetching from multiple sources
# Split from R/harmonize-transactions.R (644 lines → focused modules)

# R/harmonize-transactions.R
# Harmonize transaction-level data from multiple sources with transactions_watershed as foundation

#' Get harmonized transaction data (Internal)
#'
#' @description
#' This function is now internal. Use `ribits(transactions = "comprehensive")` instead.
#'
#' Combines transaction data from three sources to maximize coverage and columns:
#' - Transactions by Watershed CSV (foundation - 71 columns, has native bank_id)
#' - RIBITS API ledger (gap-filling, real-time, unique fields like transaction_id)
#' - Ledger Transactions CSV (additional fields like sub_ledger_id, permit_auth_date)
#'
#' This function uses transactions_watershed as the primary source since it has the most
#' comprehensive data (71 columns) and includes bank_id natively (no name matching needed).
#'
#' @param bank_ids Optional vector of bank IDs to filter
#' @param state Optional state filter
#' @param district Optional district filter
#' @param include_detailed If TRUE, returns transactions. If FALSE, returns NULL (for summary integration). Default TRUE.
#' @param cache_dir Directory for CSV cache. Default tempdir().
#'
#' @return If include_detailed=TRUE: A list with transactions and metadata.
#'   If include_detailed=FALSE: NULL (used when only summaries needed in banks dataframe)
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Recommended: Use ribits() instead
#' ca <- ribits(state = "CA", transactions = "comprehensive")
#'
#' # Internal usage (advanced)
#' wa_txns <- rb_transactions(state = "WA")
#' wa_txns$transactions
#' }
rb_transactions <- function(bank_ids = NULL,
                            state = NULL,
                            district = NULL,
                            include_detailed = TRUE,
                            cache_dir = NULL) {

  if (is.null(cache_dir)) {
    cache_dir <- .get_cache_dir(TRUE)
  }

  # If not including detailed, return NULL immediately
  # (Used when only summaries needed in banks dataframe)
  if (!include_detailed) {
    return(NULL)
  }

  cli::cli_h2("Fetching Transaction Data")

  result <- list(
    transactions = NULL,
    .meta = list(
      sources = list(),
      coverage = list(),
      fetch_date = Sys.Date()
    )
  )

  # === Get bank IDs if not provided ===
  if (is.null(bank_ids)) {
    cli::cli_progress_step("Getting bank list...")
    banks <- rb_get("banks", state = state, district = district)
    bank_ids <- .col_get(banks, "bank_id", error_if_missing = FALSE, default = integer())
    cli::cli_alert_info("Found {length(bank_ids)} banks")
  }

  if (length(bank_ids) == 0) {
    cli::cli_alert_warning("No banks found")
    return(result)
  }

  # ===================================================================
  # Source 1: Transactions by Watershed CSV (FOUNDATION - 71 columns!)
  # ===================================================================
  watershed_txns <- NULL
  cli::cli_alert_info("Downloading transactions by watershed CSV (foundation)...")

  watershed_txns <- tryCatch({
    csv_file <- rb_download_report("transactions_watershed", download_dir = cache_dir)
    data <- rb_read(csv_file)

    # Normalize column names
    names(data) <- janitor::make_clean_names(names(data))

    # Ensure bank_id exists and filter
    data <- .ensure_bank_id(data, quietly = TRUE)
    data <- data |> dplyr::filter(.data$bank_id %in% bank_ids)

    data$source <- "csv_watershed"
    data
  }, error = function(e) {
    cli::cli_alert_warning("Transactions by watershed CSV failed: {e$message}")
    NULL
  })

  if (!is.null(watershed_txns) && nrow(watershed_txns) > 0) {
    cli::cli_alert_success("Watershed CSV: {nrow(watershed_txns)} transactions ({ncol(watershed_txns)} columns)")
  }

  # ===================================================================
  # Source 2: API Ledger (GAP-FILLING - adds transaction_id, real-time data)
  # ===================================================================

  api_ledger <- NULL
  cli::cli_alert_info("Fetching API ledger for gap-filling...")

  api_ledger <- tryCatch({
    # Use bulk ledger extraction
    rb_bulk_ledger(bank_ids = bank_ids, progress = FALSE)
  }, error = function(e) {
    cli::cli_alert_warning("API ledger failed: {e$message}")
    NULL
  })

  if (!is.null(api_ledger) && nrow(api_ledger) > 0) {
    # Normalize column names
    names(api_ledger) <- tolower(names(api_ledger))
    api_ledger$source <- "api"
    cli::cli_alert_success("API Ledger: {nrow(api_ledger)} transactions ({ncol(api_ledger)} columns)")
  }

  # ===================================================================
  # Source 3: CSV Ledger Transactions (ADDITIONAL FIELDS - sub_ledger_id, permit_auth_date)
  # ===================================================================
  csv_ledger <- NULL
  cli::cli_alert_info("Downloading CSV ledger for additional fields...")

  csv_ledger <- tryCatch({
    csv_file <- rb_download_report("ledger_transactions", download_dir = cache_dir)
    data <- rb_read(csv_file)

    # Normalize column names
    names(data) <- janitor::make_clean_names(names(data))

    # Ensure bank_id exists and filter
    data <- .ensure_bank_id(data, quietly = TRUE)
    data <- data |> dplyr::filter(.data$bank_id %in% bank_ids)

    data$source <- "csv_ledger"
    data
  }, error = function(e) {
    cli::cli_alert_warning("CSV ledger failed: {e$message}")
    NULL
  })

  if (!is.null(csv_ledger) && nrow(csv_ledger) > 0) {
    cli::cli_alert_success("CSV Ledger: {nrow(csv_ledger)} transactions ({ncol(csv_ledger)} columns)")
  }

  # ===================================================================
  # THREE-WAY MERGE: watershed (foundation) + API (gap-fill) + CSV ledger (extras)
  # ===================================================================
  cli::cli_alert_info("Harmonizing transactions (3-way merge)...")

  transactions <- .harmonize_transactions_threeway(
    watershed_txns,
    api_ledger,
    csv_ledger
  )

  if (!is.null(transactions) && nrow(transactions) > 0) {
    result$transactions <- transactions
    result$.meta$sources$transactions <- paste(
      c(if (!is.null(watershed_txns)) "csv_watershed" else NULL,
        if (!is.null(api_ledger)) "api" else NULL,
        if (!is.null(csv_ledger)) "csv_ledger" else NULL),
      collapse = " + "
    )
    cli::cli_alert_success("Harmonized: {nrow(transactions)} transactions ({ncol(transactions)} columns)")

    # Validate transaction data
    validation <- .validate_transaction_data(transactions)
    result$.meta$validation <- validation
  } else {
    cli::cli_alert_warning("No transactions after harmonization")
  }

  # === Coverage stats ===
  result$.meta$coverage <- list(
    bank_ids_requested = length(bank_ids),
    banks_with_transactions = if (!is.null(result$transactions))
      length(unique(result$transactions$bank_id)) else 0,
    total_columns = if (!is.null(result$transactions))
      ncol(result$transactions) else 0
  )

  cli::cli_h3("Coverage Summary")
  cli::cli_bullets(c(
    "*" = "Banks requested: {result$.meta$coverage$bank_ids_requested}",
    "*" = "With transactions: {result$.meta$coverage$banks_with_transactions}",
    "*" = "Total columns: {result$.meta$coverage$total_columns}"
  ))

  class(result) <- c("ribits_transactions", "list")
  result
}
