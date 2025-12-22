# R/harmonize-transactions.R
# Harmonize transaction-level data (ledger, credit classification) from multiple sources

#' Get harmonized ledger/transaction data
#'
#' Combines transaction data from multiple sources to maximize coverage:
#' - RIBITS API ledger (real-time, per-bank)
#' - Ledger Transactions CSV (bulk download, additional fields)
#' - Credit Classification CSV (aggregated credit summaries)
#'
#' @param bank_ids Optional vector of bank IDs to filter
#' @param state Optional state filter
#' @param district Optional district filter
#' @param sources Which sources to use. Default all: c("api", "csv")
#' @param include_credit_class Include credit classification summary? Default TRUE.
#' @param progress Show progress. Default TRUE.
#' @param cache_dir Directory for CSV cache. Default tempdir().
#'
#' @return A list with:
#'   \item{transactions}{Harmonized transaction-level data}
#'   \item{credit_summary}{Credit classification summary by bank}
#'   \item{.meta}{Metadata about sources and coverage}
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all transactions for Washington banks
#' wa_txns <- rb_transactions(state = "WA")
#'
#' # Access data
#' wa_txns$transactions     # Individual transactions
#' wa_txns$credit_summary   # Credit totals by classification
#' }
rb_transactions <- function(bank_ids = NULL,
                            state = NULL,
                            district = NULL,
                            sources = c("api", "csv"),
                            include_credit_class = TRUE,
                            progress = TRUE,
                            cache_dir = NULL) {
  
  if (is.null(cache_dir)) {
    cache_dir <- file.path(tempdir(), "ribits_cache")
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  sources <- match.arg(sources, c("api", "csv"), several.ok = TRUE)
  use_api <- "api" %in% sources
  use_csv <- "csv" %in% sources
  
  if (progress) cli::cli_h2("Fetching Transaction Data")
  
  result <- list(
    transactions = NULL,
    credit_summary = NULL,
    .meta = list(
      sources = list(),
      coverage = list(),
      fetch_date = Sys.Date()
    )
  )
  
  # === Get bank IDs if not provided ===
  if (is.null(bank_ids)) {
    if (progress) cli::cli_progress_step("Getting bank list...")
    banks <- rb_get("banks", state = state, district = district)
    # Handle case-insensitive column names
    id_col <- names(banks)[tolower(names(banks)) == "bank_id"][1]
    bank_ids <- if (!is.na(id_col)) banks[[id_col]] else integer()
    if (progress) cli::cli_alert_info("Found {length(bank_ids)} banks")
  }
  
  if (length(bank_ids) == 0) {
    if (progress) cli::cli_alert_warning("No banks found")
    return(result)
  }
  
  # === Source 1: API Ledger ===
  api_ledger <- NULL
  if (use_api) {
    if (progress) cli::cli_progress_step("Fetching API ledger data...")
    
    api_ledger <- tryCatch({
      # Use bulk ledger extraction
      rb_bulk_ledger(bank_ids = bank_ids, progress = progress)
    }, error = function(e) {
      if (progress) cli::cli_alert_warning("API ledger failed: {e$message}")
      NULL
    })
    
    if (!is.null(api_ledger) && nrow(api_ledger) > 0) {
      # Normalize column names
      names(api_ledger) <- tolower(names(api_ledger))
      api_ledger$source <- "api"
      if (progress) cli::cli_alert_success("API: {nrow(api_ledger)} transactions")
    }
  }
  
  # === Source 2: CSV Ledger Transactions ===
  csv_ledger <- NULL
  if (use_csv) {
    if (progress) cli::cli_progress_step("Downloading CSV ledger...")
    
    csv_ledger <- tryCatch({
      csv_file <- rb_download_report("ledger_transactions", download_dir = cache_dir)
      data <- rb_read(csv_file)
      
      # Normalize column names
      names(data) <- janitor::make_clean_names(names(data))
      
      # Filter by bank_ids using name matching if needed
      if ("bank_id" %in% names(data)) {
        data <- data |> dplyr::filter(bank_id %in% bank_ids)
      } else if ("name" %in% names(data)) {
        # Build lookup for name -> bank_id matching
        lookup <- rb_build_name_lookup(include_csv = FALSE, cache = TRUE)
        data <- rb_match_names(data, lookup, name_col = "name", fuzzy = TRUE)
        data <- data |> dplyr::filter(bank_id %in% bank_ids)
      }
      
      data$source <- "csv_ledger"
      data
    }, error = function(e) {
      if (progress) cli::cli_alert_warning("CSV ledger failed: {e$message}")
      NULL
    })
    
    if (!is.null(csv_ledger) && nrow(csv_ledger) > 0) {
      if (progress) cli::cli_alert_success("CSV Ledger: {nrow(csv_ledger)} transactions")
    }
  }
  
  # === Merge Transaction Data ===
  if (progress) cli::cli_progress_step("Harmonizing transaction data...")
  
  transactions <- .harmonize_transactions(api_ledger, csv_ledger)
  
  if (!is.null(transactions) && nrow(transactions) > 0) {
    result$transactions <- transactions
    result$.meta$sources$transactions <- paste(
      c(if (!is.null(api_ledger)) "api" else NULL,
        if (!is.null(csv_ledger)) "csv_ledger" else NULL),
      collapse = " + "
    )
    if (progress) cli::cli_alert_success("Harmonized: {nrow(transactions)} transactions")
  }
  
  # === Source 3: Credit Classification Summary ===
  if (include_credit_class && use_csv) {
    if (progress) cli::cli_progress_step("Downloading credit classification...")
    
    credit_summary <- tryCatch({
      cc_file <- rb_download_report("credit_classification", download_dir = cache_dir)
      data <- rb_read(cc_file)
      
      # Normalize column names
      names(data) <- janitor::make_clean_names(names(data))
      
      # Match to bank_ids
      if (!"bank_id" %in% names(data) && "bank_name" %in% names(data)) {
        lookup <- rb_build_name_lookup(include_csv = FALSE, cache = TRUE)
        data <- rb_match_names(data, lookup, name_col = "bank_name", fuzzy = TRUE)
      }
      
      if ("bank_id" %in% names(data)) {
        data <- data |> dplyr::filter(bank_id %in% bank_ids)
      }
      
      data
    }, error = function(e) {
      if (progress) cli::cli_alert_warning("Credit classification failed: {e$message}")
      NULL
    })
    
    if (!is.null(credit_summary) && nrow(credit_summary) > 0) {
      result$credit_summary <- credit_summary
      result$.meta$sources$credit_summary <- "csv"
      if (progress) cli::cli_alert_success("Credit Summary: {nrow(credit_summary)} rows")
    }
  }
  
  # === Coverage stats ===
  result$.meta$coverage <- list(
    bank_ids_requested = length(bank_ids),
    banks_with_transactions = if (!is.null(result$transactions)) 
      length(unique(result$transactions$bank_id)) else 0,
    banks_with_credit_summary = if (!is.null(result$credit_summary))
      length(unique(result$credit_summary$bank_id)) else 0
  )
  
  if (progress) {
    cli::cli_h3("Coverage Summary")
    cli::cli_bullets(c(
      "*" = "Banks requested: {result$.meta$coverage$bank_ids_requested}",
      "*" = "With transactions: {result$.meta$coverage$banks_with_transactions}",
      "*" = "With credit summary: {result$.meta$coverage$banks_with_credit_summary}"
    ))
  }
  
  class(result) <- c("ribits_transactions", "list")
  result
}


#' Harmonize transaction data from multiple sources
#' @keywords internal
.harmonize_transactions <- function(api_ledger, csv_ledger) {
  
  if (is.null(api_ledger) && is.null(csv_ledger)) {
    return(tibble::tibble())
  }
  
  if (is.null(api_ledger) || nrow(api_ledger) == 0) return(csv_ledger)
  if (is.null(csv_ledger) || nrow(csv_ledger) == 0) return(api_ledger)
  
  # Standardize API ledger columns
  api_std <- api_ledger |>
    dplyr::rename_with(~ gsub("_list$", "", .x))  # credit_type_list -> credit_type
  
  # Standardize CSV ledger columns
  csv_std <- csv_ledger |>
    dplyr::rename_with(~ dplyr::case_when(
      .x == "name" ~ "bank_name",
      .x == "type" ~ "transaction_type", 
      .x == "comment" ~ "notes",
      TRUE ~ .x
    ))
  
  # Convert all columns to character to avoid type conflicts
  api_char <- api_std |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  csv_char <- csv_std |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  
  # Combine
  combined <- dplyr::bind_rows(api_char, csv_char)
  
  # Deduplicate by transaction_id if present, keeping API version
  if ("transaction_id" %in% names(combined) && any(!is.na(combined$transaction_id))) {
    combined <- combined |>
      dplyr::arrange(transaction_id, dplyr::desc(source == "api")) |>
      dplyr::distinct(transaction_id, .keep_all = TRUE)
  }
  
  combined
}


#' Print method for ribits_transactions
#' @param x A ribits_transactions object
#' @param ... Additional arguments passed to methods
#' @export
print.ribits_transactions <- function(x, ...) {
  cli::cli_h1("RIBITS Transaction Data")
  
  if (!is.null(x$.meta$fetch_date)) {
    cli::cli_text("Fetched: {.val {x$.meta$fetch_date}}")
  }
  
  cli::cli_h2("Data")
  
  # Transactions
  if (!is.null(x$transactions) && nrow(x$transactions) > 0) {
    src <- x$.meta$sources$transactions %||% "?"
    n_banks <- length(unique(x$transactions$bank_id))
    cli::cli_alert_success("transactions: {nrow(x$transactions)} rows ({n_banks} banks) [{src}]")
  } else {
    cli::cli_alert_warning("transactions: none")
  }
  
  # Credit summary
  if (!is.null(x$credit_summary) && nrow(x$credit_summary) > 0) {
    src <- x$.meta$sources$credit_summary %||% "?"
    n_banks <- length(unique(x$credit_summary$bank_id))
    cli::cli_alert_success("credit_summary: {nrow(x$credit_summary)} rows ({n_banks} banks) [{src}]")
  } else {
    cli::cli_alert_warning("credit_summary: none")
  }
  
  cli::cli_h2("Coverage")
  cli::cli_bullets(c(
    "*" = "Banks requested: {x$.meta$coverage$bank_ids_requested}",
    "*" = "With transactions: {x$.meta$coverage$banks_with_transactions}",
    "*" = "With credit summary: {x$.meta$coverage$banks_with_credit_summary}"
  ))
  
  invisible(x)
}


#' Get credit breakdown by classification type
#'
#' Summarizes credits across banks by classification type (wetland, stream, etc.)
#'
#' @param data Either a ribits_transactions object or a ribits_data object
#' @param by_bank If TRUE, breaks down by bank. Default FALSE (totals only).
#'
#' @return A tibble with credit summaries
#' @export
rb_credit_breakdown <- function(data, by_bank = FALSE) {
  
  # Extract credit summary from either object type
  credit_data <- if (inherits(data, "ribits_transactions")) {
    data$credit_summary
  } else if (inherits(data, "ribits_data") && !is.null(data$credit_summary)) {
    data$credit_summary
  } else if (is.data.frame(data)) {
    data
  } else {
    cli::cli_abort("Expected ribits_transactions, ribits_data, or data frame")
  }
  
  if (is.null(credit_data) || nrow(credit_data) == 0) {
    cli::cli_alert_warning("No credit classification data available")
    return(tibble::tibble())
  }
  
  # Find credit columns
  credit_cols <- names(credit_data)[grepl("credit", names(credit_data), ignore.case = TRUE)]
  numeric_cols <- credit_cols[sapply(credit_data[credit_cols], is.numeric)]
  
  if (length(numeric_cols) == 0) {
    cli::cli_alert_warning("No numeric credit columns found")
    return(tibble::tibble())
  }
  
  # Summarize
  if (by_bank && "bank_id" %in% names(credit_data)) {
    result <- credit_data |>
      dplyr::group_by(bank_id, 
                      credit_classification = credit_data$credit_classification %||% "unknown") |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  } else {
    result <- credit_data |>
      dplyr::group_by(credit_classification = credit_data$credit_classification %||% "unknown") |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)),
        n_banks = dplyr::n_distinct(bank_id),
        .groups = "drop"
      )
  }
  
  result
}
