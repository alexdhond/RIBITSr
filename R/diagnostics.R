# R/diagnostics.R
# Diagnostic tools for RIBITS data harmonization and quality

#' Diagnose Ledger Data Quality and Harmonization
#'
#' Compares API and CSV ledger data for specific banks to identify
#' discrepancies, gaps, and matching issues.
#'
#' @param bank_ids Vector of bank IDs to diagnose.
#' @param state Optional state filter (if bank_ids not provided).
#' @param csv_path Optional. Path to a local CSV file to use instead of downloading.
#' @param download_csv Force fresh download of CSV? Default FALSE. (Ignored if csv_path provided).
#' @return A list containing diagnostic summaries.
#' @export
rb_diagnose_ledger <- function(bank_ids = NULL, state = NULL, csv_path = NULL, download_csv = FALSE) {
  
  if (is.null(bank_ids) && !is.null(state)) {
    cli::cli_progress_step("Fetching bank list for state {state}...")
    banks <- rb_get("banks", state = state)
    id_col <- names(banks)[tolower(names(banks)) == "bank_id"][1]
    bank_ids <- if (!is.na(id_col)) banks[[id_col]] else integer()
    
    # Limit to first 3 banks for diagnostics unless specified otherwise
    if (length(bank_ids) > 3) {
      cli::cli_alert_info("Diagnosing first 3 banks from {length(bank_ids)} found.")
      bank_ids <- head(bank_ids, 3)
    }
  }
  
  if (length(bank_ids) == 0) {
    cli::cli_abort("No banks provided or found.")
  }
  
  cli::cli_h1("RIBITS Ledger Diagnostics")
  cli::cli_text("Diagnosing banks: {paste(bank_ids, collapse = ', ')}")
  
  # 1. Fetch API Data
  cli::cli_h2("1. API Source")
  api_ledger <- tryCatch({
    rb_bulk_ledger(bank_ids = bank_ids, progress = TRUE)
  }, error = function(e) {
    cli::cli_alert_danger("API fetch failed: {e$message}")
    NULL
  })
  
  if (!is.null(api_ledger)) {
    cli::cli_alert_success("API: {nrow(api_ledger)} transactions found.")
    cli::cli_text("Columns: {paste(head(names(api_ledger), 5), collapse=', ')} ...")
    if ("transaction_id" %in% names(api_ledger)) {
      cli::cli_alert_success("API has 'transaction_id'.")
    } else {
      cli::cli_alert_warning("API MISSING 'transaction_id'. Matching will be difficult.")
    }
  } else {
    cli::cli_alert_warning("API: No data.")
  }
  
  # 2. Fetch CSV Data
  cli::cli_h2("2. CSV Source")
  
  csv_ledger <- tryCatch({
    if (!is.null(csv_path)) {
      if (!file.exists(csv_path)) cli::cli_abort("CSV file not found: {csv_path}")
      cli::cli_alert_info("Reading local CSV: {csv_path}")
      data <- rb_read(csv_path)
    } else {
      cache_dir <- file.path(tempdir(), "ribits_cache")
      if (!dir.exists(cache_dir)) dir.create(cache_dir)
      csv_file <- rb_download_report("ledger_transactions", download_dir = cache_dir)
      data <- rb_read(csv_file)
    }
    
    # Filter for our banks
    names(data) <- janitor::make_clean_names(names(data))
    
    # Try to find bank_id
    if ("bank_id" %in% names(data)) {
      data <- data |> dplyr::filter(bank_id %in% bank_ids)
    } else if ("name" %in% names(data)) {
       lookup <- rb_build_name_lookup(include_csv = FALSE, cache = TRUE)
       data <- rb_match_names(data, lookup, name_col = "name", fuzzy = TRUE)
       data <- data |> dplyr::filter(bank_id %in% bank_ids)
    }
    data
  }, error = function(e) {
    cli::cli_alert_danger("CSV fetch failed: {e$message}")
    NULL
  })
  
  if (!is.null(csv_ledger)) {
    cli::cli_alert_success("CSV: {nrow(csv_ledger)} transactions found for these banks.")
    cli::cli_text("Columns: {paste(head(names(csv_ledger), 5), collapse=', ')} ...")
    
    # Check for linking keys
    potential_keys <- c("transaction_id", "parent_transaction_id", "sub_ledger_id", "permit_number")
    found_keys <- intersect(potential_keys, names(csv_ledger))
    if (length(found_keys) > 0) {
      cli::cli_alert_success("CSV Keys found: {paste(found_keys, collapse=', ')}")
    } else {
      cli::cli_alert_warning("CSV MISSING obvious linking keys (transaction_id, parent_transaction_id).")
    }
  } else {
    cli::cli_alert_warning("CSV: No data.")
  }
  
  # 3. Comparison
  cli::cli_h2("3. Harmonization Check")
  
  if (!is.null(api_ledger) && !is.null(csv_ledger)) {
    # Prepare API
    api_std <- api_ledger |> dplyr::mutate(across(everything(), as.character))
    
    # Prepare CSV (mimic harmonization logic)
    csv_std <- csv_ledger |>
      dplyr::mutate(across(everything(), as.character)) |>
      dplyr::rename_with(~ dplyr::case_when(
        .x == "parent_transaction_id" ~ "transaction_id",
        TRUE ~ .x
      ))
    
    # Check ID Overlap
    if ("transaction_id" %in% names(api_std) && "transaction_id" %in% names(csv_std)) {
      api_ids <- unique(api_std$transaction_id)
      csv_ids <- unique(csv_std$transaction_id)
      common <- intersect(api_ids, csv_ids)
      
      cli::cli_bullets(c(
        "*" = "Unique API IDs: {length(api_ids)}",
        "*" = "Unique CSV IDs: {length(csv_ids)}",
        "v" = "Common IDs (Matchable): {length(common)}",
        "!" = "API only: {length(setdiff(api_ids, csv_ids))}",
        "!" = "CSV only: {length(setdiff(csv_ids, api_ids))}"
      ))
      
      if (length(common) == 0) {
        cli::cli_alert_danger("ZERO OVERLAP in transaction_id. Harmonization will just append rows (duplicates).")
        cli::cli_text("Sample API ID: {head(api_ids, 1)}")
        cli::cli_text("Sample CSV ID: {head(csv_ids, 1)}")
      }
    } else {
      cli::cli_alert_danger("Cannot compare IDs: 'transaction_id' missing in one or both standardized sets.")
    }
    
  } else {
    cli::cli_alert_info("Cannot compare: one source is missing.")
  }
  
  invisible(list(api = api_ledger, csv = csv_ledger))
}
