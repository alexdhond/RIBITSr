# R/diagnostics-ledger.R
# Ledger-specific harmonization diagnostics
# Split from R/data-diagnostics.R (848 lines → focused modules)

#' Internal: Ledger harmonization diagnostics
#' @keywords internal
#' @noRd
.rb_diagnose_ledger_comparison <- function(bank_ids = NULL, state = NULL, csv_path = NULL, verbose = TRUE) {

  if (is.null(bank_ids) && !is.null(state)) {
    if (verbose) cli::cli_progress_step("Fetching bank list for state {state}...")
    banks <- rb_get("banks", state = state)
    id_col <- .get_column_case_insensitive(banks, "bank_id")
    bank_ids <- if (!is.na(id_col)) banks[[id_col]] else integer()

    # Limit to first 3 banks for diagnostics unless specified otherwise
    if (length(bank_ids) > 3) {
      if (verbose) cli::cli_alert_info("Diagnosing first 3 banks from {length(bank_ids)} found.")
      bank_ids <- head(bank_ids, 3)
    }
  }

  if (length(bank_ids) == 0) {
    cli::cli_abort("No banks provided or found.")
  }

  if (verbose) {
    cli::cli_h1("Ledger Harmonization Diagnostics")
    cli::cli_text("Diagnosing banks: {paste(bank_ids, collapse = ', ')}")
  }

  # 1. Fetch API Data
  if (verbose) cli::cli_h2("1. API Source")
  api_ledger <- tryCatch({
    rb_bulk_ledger(bank_ids = bank_ids, progress = verbose)
  }, error = function(e) {
    if (verbose) cli::cli_alert_danger("API fetch failed: {e$message}")
    NULL
  })

  if (!is.null(api_ledger) && verbose) {
    cli::cli_alert_success("API: {nrow(api_ledger)} transactions found.")
    cli::cli_text("Columns: {paste(head(names(api_ledger), 5), collapse=', ')} ...")
    if ("transaction_id" %in% names(api_ledger)) {
      cli::cli_alert_success("API has 'transaction_id'.")
    } else {
      cli::cli_alert_warning("API MISSING 'transaction_id'. Matching will be difficult.")
    }
  } else if (verbose) {
    cli::cli_alert_warning("API: No data.")
  }

  # 2. Fetch CSV Data
  if (verbose) cli::cli_h2("2. CSV Source")

  csv_ledger <- tryCatch({
    if (!is.null(csv_path)) {
      if (!file.exists(csv_path)) cli::cli_abort("CSV file not found: {csv_path}")
      if (verbose) cli::cli_alert_info("Reading local CSV: {csv_path}")
      data <- rb_read(csv_path)
    } else {
      cache_dir <- .get_cache_dir(TRUE)
      csv_file <- rb_download_report("ledger_transactions", download_dir = cache_dir)
      data <- rb_read(csv_file)
    }

    # Filter for our banks
    names(data) <- janitor::make_clean_names(names(data))

    # Ensure bank_id exists and filter
    data <- .ensure_bank_id(data, quietly = !verbose)
    data <- .filter_by_bank_ids(data, bank_ids, quietly = !verbose)

    data
  }, error = function(e) {
    if (verbose) cli::cli_alert_danger("CSV fetch failed: {e$message}")
    NULL
  })

  if (!is.null(csv_ledger) && verbose) {
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
  } else if (verbose) {
    cli::cli_alert_warning("CSV: No data.")
  }

  # 3. Comparison
  if (verbose) cli::cli_h2("3. Harmonization Check")

  if (!is.null(api_ledger) && !is.null(csv_ledger)) {
    # Prepare API
    api_std <- api_ledger |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    # Prepare CSV (mimic harmonization logic)
    csv_std <- csv_ledger |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      dplyr::rename_with(~ dplyr::case_when(
        .x == "parent_transaction_id" ~ "transaction_id",
        TRUE ~ .x
      ))

    # Check ID Overlap
    if ("transaction_id" %in% names(api_std) && "transaction_id" %in% names(csv_std)) {
      api_ids <- unique(api_std$transaction_id)
      csv_ids <- unique(csv_std$transaction_id)
      common <- intersect(api_ids, csv_ids)

      if (verbose) {
        cli::cli_bullets(c(
          "*" = "Unique API IDs: {length(api_ids)}",
          "*" = "Unique CSV IDs: {length(csv_ids)}",
          "v" = "Common IDs (Matchable): {length(common)}",
          "!" = "API only: {length(setdiff(api_ids, csv_ids))}",
          "!" = "CSV only: {length(setdiff(csv_ids, api_ids))}"
        ))
      }

      if (length(common) == 0 && verbose) {
        cli::cli_alert_danger("ZERO OVERLAP in transaction_id. Harmonization will just append rows (duplicates).")
        cli::cli_text("Sample API ID: {head(api_ids, 1)}")
        cli::cli_text("Sample CSV ID: {head(csv_ids, 1)}")
      }
    } else if (verbose) {
      cli::cli_alert_danger("Cannot compare IDs: 'transaction_id' missing in one or both standardized sets.")
    }

  } else if (verbose) {
    cli::cli_alert_info("Cannot compare: one source is missing.")
  }

  invisible(list(api = api_ledger, csv = csv_ledger))
}
