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
    bank_ids <- .col_get(banks, "bank_id", default = integer())
    cli::cli_alert_info("Found {length(bank_ids)} banks")
  }

  if (length(bank_ids) == 0) {
    cli::cli_alert_warning("No banks found")
    return(result)
  }

  # Overall progress bar for the 3 data sources
  pb_sources <- cli::cli_progress_bar(
    "Fetching transaction sources",
    total = 3,
    format = "{cli::pb_bar} {cli::pb_percent} | {cli::pb_current}/{cli::pb_total} sources complete"
  )

  # ===================================================================
  # Source 1: Transactions by Watershed CSV (FOUNDATION - 71 columns!)
  # ===================================================================
  watershed_txns <- NULL
  cli::cli_progress_step("Downloading transactions by watershed CSV (foundation)...")

  watershed_txns <- tryCatch({
    csv_file <- rb_download_report("transactions_watershed", download_dir = cache_dir)
    data <- rb_read(csv_file)

    # Normalize column names
    names(data) <- janitor::make_clean_names(names(data))

    # Ensure bank_id exists and filter
    data <- .ensure_bank_id(data, quietly = TRUE)
    data <- data |> dplyr::filter(bank_id %in% bank_ids)

    data$source <- "csv_watershed"
    data
  }, error = function(e) {
    cli::cli_alert_warning("Transactions by watershed CSV failed: {e$message}")
    NULL
  })

  if (!is.null(watershed_txns) && nrow(watershed_txns) > 0) {
    cli::cli_alert_success("Watershed CSV: {nrow(watershed_txns)} transactions ({ncol(watershed_txns)} columns)")
  }
  cli::cli_progress_update(id = pb_sources, inc = 1)

  # ===================================================================
  # Source 2: API Ledger (GAP-FILLING - adds transaction_id, real-time data)
  # ===================================================================
  api_ledger <- NULL
  cli::cli_progress_step("Fetching API ledger for gap-filling...")

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
  cli::cli_progress_update(id = pb_sources, inc = 1)

  # ===================================================================
  # Source 3: CSV Ledger Transactions (ADDITIONAL FIELDS - sub_ledger_id, permit_auth_date)
  # ===================================================================
  csv_ledger <- NULL
  cli::cli_progress_step("Downloading CSV ledger for additional fields...")

  csv_ledger <- tryCatch({
    csv_file <- rb_download_report("ledger_transactions", download_dir = cache_dir)
    data <- rb_read(csv_file)

    # Normalize column names
    names(data) <- janitor::make_clean_names(names(data))

    # Ensure bank_id exists and filter
    data <- .ensure_bank_id(data, quietly = TRUE)
    data <- data |> dplyr::filter(bank_id %in% bank_ids)

    data$source <- "csv_ledger"
    data
  }, error = function(e) {
    cli::cli_alert_warning("CSV ledger failed: {e$message}")
    NULL
  })

  if (!is.null(csv_ledger) && nrow(csv_ledger) > 0) {
    cli::cli_alert_success("CSV Ledger: {nrow(csv_ledger)} transactions ({ncol(csv_ledger)} columns)")
  }
  cli::cli_progress_update(id = pb_sources, inc = 1)

  cli::cli_progress_done(id = pb_sources)

  # ===================================================================
  # THREE-WAY MERGE: watershed (foundation) + API (gap-fill) + CSV ledger (extras)
  # ===================================================================
  cli::cli_progress_step("Harmonizing transactions (3-way merge)...")

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


#' Three-way harmonization of transaction data
#'
#' Merges transactions from watershed CSV (foundation), API ledger (gap-filling),
#' and CSV ledger (additional fields).
#'
#' @param watershed_txns Transactions by watershed CSV (foundation)
#' @param api_ledger API ledger data
#' @param csv_ledger CSV ledger transactions data
#'
#' @return Harmonized transactions tibble
#' @keywords internal
#' @noRd
.harmonize_transactions_threeway <- function(watershed_txns, api_ledger, csv_ledger) {

  # Start with whichever source has data
  if (is.null(watershed_txns) || nrow(watershed_txns) == 0) {
    if (is.null(api_ledger) || nrow(api_ledger) == 0) {
      return(csv_ledger)
    }
    if (is.null(csv_ledger) || nrow(csv_ledger) == 0) {
      return(api_ledger)
    }
    # Fall back to 2-way merge if no watershed
    return(.harmonize_transactions(api_ledger, csv_ledger, priority = "csv"))
  }

  # STEP 1: Normalize all sources using column registry
  watershed_std <- .normalize_columns(watershed_txns)
  api_std <- if (!is.null(api_ledger) && nrow(api_ledger) > 0) {
    .normalize_columns(api_ledger) |>
      dplyr::rename_with(~ gsub("_list$", "", .x))  # credit_type_list -> credit_type
  } else {
    NULL
  }
  csv_std <- if (!is.null(csv_ledger) && nrow(csv_ledger) > 0) {
    .normalize_columns(csv_ledger) |>
      dplyr::rename_with(~ dplyr::case_when(
        .x == "name" ~ "bank_name",
        .x == "type" ~ "entity_type",
        .x == "parent_transaction_id" ~ "transaction_id",
        .x == "comment" ~ "notes",
        TRUE ~ .x
      ))
  } else {
    NULL
  }

  # STEP 2: Start with watershed as foundation
  unified <- watershed_std

  # STEP 3: Add API ledger fields (if available)
  if (!is.null(api_std)) {
    # Try to merge on both bank_id and transaction_id if available
    # Otherwise fall back to just bank_id
    merge_keys <- c("bank_id")
    if ("transaction_id" %in% names(unified) && "transaction_id" %in% names(api_std)) {
      merge_keys <- c("bank_id", "transaction_id")
    }

    unified <- .merge_preserving_columns(
      unified,
      api_std,
      by = merge_keys,
      suffix = c("", "_api")
    )
  }

  # STEP 4: Add CSV ledger fields (if available)
  if (!is.null(csv_std)) {
    # Try to merge on both bank_id and transaction_id
    merge_keys <- c("bank_id")
    if ("transaction_id" %in% names(unified) && "transaction_id" %in% names(csv_std)) {
      merge_keys <- c("bank_id", "transaction_id")
    }

    unified <- .merge_preserving_columns(
      unified,
      csv_std,
      by = merge_keys,
      suffix = c("", "_csv")
    )
  }

  # STEP 5: Fill bank_name gaps using bank_id lookup
  if ("bank_id" %in% names(unified)) {
    # First try: Build bank_id -> bank_name mapping from rows that have both
    if ("bank_name" %in% names(unified)) {
      known_names <- unified |>
        dplyr::filter(!is.na(bank_id) & !is.na(bank_name)) |>
        dplyr::select(bank_id, bank_name) |>
        dplyr::distinct(bank_id, .keep_all = TRUE)

      if (nrow(known_names) > 0) {
        unified <- unified |>
          dplyr::left_join(known_names, by = "bank_id", suffix = c("", "_lookup")) |>
          dplyr::mutate(bank_name = dplyr::coalesce(bank_name, bank_name_lookup)) |>
          dplyr::select(-dplyr::any_of("bank_name_lookup"))
      }
    }

    # Second try: Use global bank lookup table for remaining gaps
    if ("bank_name" %in% names(unified)) {
      missing_names <- is.na(unified$bank_name)
      if (any(missing_names)) {
        tryCatch({
          lookup <- rb_build_name_lookup(include_csv = FALSE)
          if (!is.null(lookup) && nrow(lookup) > 0) {
            id_to_name <- lookup |>
              dplyr::select(bank_id, name) |>
              dplyr::distinct(bank_id, .keep_all = TRUE) |>
              dplyr::rename(bank_name_global = name)

            unified <- unified |>
              dplyr::left_join(id_to_name, by = "bank_id") |>
              dplyr::mutate(bank_name = dplyr::coalesce(bank_name, bank_name_global)) |>
              dplyr::select(-dplyr::any_of("bank_name_global"))
          }
        }, error = function(e) NULL)
      }
    }
  }

  # STEP 6: Clean up source column
  # Combine sources where multiple contributed
  if ("source" %in% names(unified)) {
    unified <- unified |>
      dplyr::mutate(
        source = dplyr::case_when(
          !is.na(source) ~ source,
          TRUE ~ "unknown"
        )
      )
  }

  unified
}


#' Harmonize transaction data from two sources (LEGACY - for backwards compatibility)
#'
#' @keywords internal
#' @noRd
.harmonize_transactions <- function(api_ledger, csv_ledger, priority = NULL) {

  if (is.null(api_ledger) && is.null(csv_ledger)) {
    return(tibble::tibble())
  }

  if (is.null(api_ledger) || nrow(api_ledger) == 0) return(csv_ledger)
  if (is.null(csv_ledger) || nrow(csv_ledger) == 0) return(api_ledger)

  # Determine priority
  if (is.null(priority)) {
    config <- .get_discrepancy_config()
    p_sources <- config$source_priority
    if ("csv" %in% p_sources && "api" %in% p_sources) {
      priority <- if (match("csv", p_sources) < match("api", p_sources)) "csv" else "api"
    } else {
      priority <- "api"
    }
  }

  # Standardize columns
  api_std <- api_ledger |>
    dplyr::rename_with(~ gsub("_list$", "", .x))

  csv_std <- csv_ledger |>
    dplyr::rename_with(~ dplyr::case_when(
      .x == "name" ~ "bank_name",
      .x == "type" ~ "entity_type",
      .x == "parent_transaction_id" ~ "transaction_id",
      .x == "comment" ~ "notes",
      TRUE ~ .x
    ))

  # Convert to character
  api_char <- api_std |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  csv_char <- csv_std |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Merge on transaction_id if available
  if ("transaction_id" %in% names(api_char) && "transaction_id" %in% names(csv_char)) {
    api_ids <- api_char$transaction_id[!is.na(api_char$transaction_id)]
    csv_ids <- csv_char$transaction_id[!is.na(csv_char$transaction_id)]
    common_ids <- intersect(api_ids, csv_ids)

    if (length(common_ids) > 0) {
      api_common <- api_char |> dplyr::filter(transaction_id %in% common_ids)
      csv_common <- csv_char |> dplyr::filter(transaction_id %in% common_ids)

      all_cols <- union(names(api_common), names(csv_common))

      for (col in setdiff(all_cols, names(api_common))) {
        api_common[[col]] <- NA_character_
      }
      for (col in setdiff(all_cols, names(csv_common))) {
        csv_common[[col]] <- NA_character_
      }

      merged_common <- dplyr::left_join(
        api_common, csv_common,
        by = "transaction_id",
        suffix = c("", "_csv")
      )

      for (col in all_cols) {
        if (col == "transaction_id") next

        csv_col <- paste0(col, "_csv")

        if (csv_col %in% names(merged_common)) {
          val_api <- merged_common[[col]]
          val_csv <- merged_common[[csv_col]]

          if (priority == "csv") {
            merged_common[[col]] <- dplyr::coalesce(val_csv, val_api)
          } else {
            merged_common[[col]] <- dplyr::coalesce(val_api, val_csv)
          }

          merged_common <- merged_common |> dplyr::select(-dplyr::all_of(csv_col))
        }
      }

      merged_common$source <- "api+csv"

      api_only <- api_char |> dplyr::filter(!transaction_id %in% common_ids | is.na(transaction_id))
      csv_only <- csv_char |> dplyr::filter(!transaction_id %in% common_ids | is.na(transaction_id))

      combined <- dplyr::bind_rows(merged_common, api_only, csv_only)
    } else {
      combined <- dplyr::bind_rows(api_char, csv_char)
    }
  } else {
    combined <- dplyr::bind_rows(api_char, csv_char)
  }

  # Deduplication
  if ("transaction_id" %in% names(combined) && any(!is.na(combined$transaction_id))) {
    combined <- combined |>
      dplyr::arrange(transaction_id, dplyr::desc(source == "api+csv"), dplyr::desc(source == priority)) |>
      dplyr::distinct(transaction_id, .keep_all = TRUE)
  }

  # Fill bank_name gaps
  if ("bank_id" %in% names(combined) && "bank_name" %in% names(combined)) {
    known_names <- combined |>
      dplyr::filter(!is.na(bank_id) & !is.na(bank_name)) |>
      dplyr::select(bank_id, bank_name) |>
      dplyr::distinct(bank_id, .keep_all = TRUE)

    if (nrow(known_names) > 0) {
      combined <- combined |>
        dplyr::left_join(known_names, by = "bank_id", suffix = c("", "_lookup")) |>
        dplyr::mutate(bank_name = dplyr::coalesce(bank_name, bank_name_lookup)) |>
        dplyr::select(-dplyr::any_of("bank_name_lookup"))
    }
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
    n_cols <- ncol(x$transactions)
    cli::cli_alert_success("transactions: {nrow(x$transactions)} rows ({n_banks} banks, {n_cols} columns) [{src}]")
  } else {
    cli::cli_alert_warning("transactions: none")
  }

  cli::cli_h2("Coverage")
  cli::cli_bullets(c(
    "*" = "Banks requested: {x$.meta$coverage$bank_ids_requested}",
    "*" = "With transactions: {x$.meta$coverage$banks_with_transactions}",
    "*" = "Total columns: {x$.meta$coverage$total_columns}"
  ))

  invisible(x)
}


#' Validate transaction data quality
#'
#' Checks for common data quality issues in transaction datasets:
#' - Missing bank IDs
#' - Missing or invalid credit values
#' - Negative credits
#' - Unusually large credit values
#' - Source breakdown
#'
#' @param transactions A tibble of transaction data
#' @return A list with validation results and warnings
#' @keywords internal
#' @noRd
.validate_transaction_data <- function(transactions) {
  if (is.null(transactions) || nrow(transactions) == 0) {
    return(list(valid = TRUE, warnings = character()))
  }

  validation <- list(
    valid = TRUE,
    warnings = character(),
    stats = list()
  )

  # Check for bank_id column
  if (!"bank_id" %in% names(transactions)) {
    cli::cli_alert_danger("CRITICAL: Missing 'bank_id' column")
    validation$valid <- FALSE
    validation$warnings <- c(validation$warnings, "Missing required column: bank_id")
    return(validation)
  }

  # Check for missing bank_ids
  n_missing_bank_id <- sum(is.na(transactions$bank_id))
  if (n_missing_bank_id > 0) {
    pct_missing <- round(100 * n_missing_bank_id / nrow(transactions), 1)
    cli::cli_alert_warning("Missing bank_id in {n_missing_bank_id} rows ({pct_missing}%)")
    validation$warnings <- c(
      validation$warnings,
      glue::glue("{n_missing_bank_id} rows missing bank_id ({pct_missing}%)")
    )
  }

  # Find credit column (could be "credit", "credits", "available_credit", etc.)
  credit_col <- NULL
  possible_credit_cols <- c("credit", "credits", "available_credit", "total_credit")
  for (col in possible_credit_cols) {
    if (col %in% names(transactions)) {
      credit_col <- col
      break
    }
  }

  if (!is.null(credit_col)) {
    credit_values <- transactions[[credit_col]]

    # Convert to numeric if needed
    if (!is.numeric(credit_values)) {
      credit_values <- suppressWarnings(as.numeric(credit_values))
    }

    # Check for missing credits
    n_missing_credit <- sum(is.na(credit_values))
    if (n_missing_credit > 0) {
      pct_missing <- round(100 * n_missing_credit / nrow(transactions), 1)
      cli::cli_alert_warning("Missing {credit_col} in {n_missing_credit} rows ({pct_missing}%)")
      validation$warnings <- c(
        validation$warnings,
        glue::glue("{n_missing_credit} rows missing {credit_col} ({pct_missing}%)")
      )
    }

    # Check for negative credits
    valid_credits <- credit_values[!is.na(credit_values)]
    if (length(valid_credits) > 0) {
      n_negative <- sum(valid_credits < 0)
      if (n_negative > 0) {
        cli::cli_alert_warning("Found {n_negative} negative credit values")
        validation$warnings <- c(
          validation$warnings,
          glue::glue("{n_negative} rows have negative {credit_col}")
        )
        validation$stats$negative_credits <- n_negative
      }

      # Check for unusually large credits (>10,000 is suspicious for most mitigation banks)
      n_large <- sum(valid_credits > 10000)
      if (n_large > 0) {
        max_credit <- max(valid_credits, na.rm = TRUE)
        cli::cli_alert_warning(
          "Found {n_large} unusually large credit values (max: {round(max_credit, 2)})"
        )
        validation$warnings <- c(
          validation$warnings,
          glue::glue("{n_large} rows have {credit_col} > 10,000 (max: {round(max_credit, 2)})")
        )
        validation$stats$large_credits <- n_large
      }

      # Credit statistics
      validation$stats$credit_col <- credit_col
      validation$stats$credit_range <- c(min = min(valid_credits, na.rm = TRUE),
                                          max = max(valid_credits, na.rm = TRUE))
      validation$stats$credit_mean <- mean(valid_credits, na.rm = TRUE)
      validation$stats$credit_median <- median(valid_credits, na.rm = TRUE)
    }
  } else {
    cli::cli_alert_warning("No credit column found (expected: {paste(possible_credit_cols, collapse = ', ')})")
    validation$warnings <- c(validation$warnings, "No credit column found")
  }

  # Source breakdown
  if ("source" %in% names(transactions)) {
    source_counts <- table(transactions$source, useNA = "ifany")
    validation$stats$source_breakdown <- as.list(source_counts)

    cli::cli_h3("Data Source Breakdown")
    for (src in names(source_counts)) {
      src_label <- if (is.na(src)) "unknown" else src
      pct <- round(100 * source_counts[[src]] / nrow(transactions), 1)
      cli::cli_bullets(c(
        "*" = "{src_label}: {source_counts[[src]]} rows ({pct}%)"
      ))
    }
  }

  # Overall validation summary
  if (length(validation$warnings) == 0) {
    cli::cli_alert_success("Data validation: No issues found")
  } else {
    cli::cli_alert_info("Data validation: {length(validation$warnings)} warning(s)")
  }

  validation
}
