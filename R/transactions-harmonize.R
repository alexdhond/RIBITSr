# R/transactions-harmonize.R
# Transaction harmonization and merging logic
# Split from R/harmonize-transactions.R (644 lines → focused modules)

#' Convert transaction field types to proper classes
#'
#' Ensures dates are Date objects, numeric fields are numeric, etc.
#'
#' @param transactions Transaction data frame
#' @return Transactions with proper field types
#' @keywords internal
#' @noRd
.convert_transaction_types <- function(transactions) {
  if (is.null(transactions) || nrow(transactions) == 0) {
    return(transactions)
  }

  # Convert transaction_date to Date (if present)
  if ("transaction_date" %in% names(transactions)) {
    if (is.character(transactions$transaction_date)) {
      # Parse character dates (MM/DD/YYYY format most common from CSV)
      transactions$transaction_date <- lubridate::mdy(transactions$transaction_date)
    } else if (inherits(transactions$transaction_date, c("POSIXct", "POSIXlt"))) {
      # Convert POSIX to Date
      transactions$transaction_date <- as.Date(transactions$transaction_date)
    }
  }

  # Convert permit_auth_date to Date (if present)
  if ("permit_auth_date" %in% names(transactions)) {
    if (is.character(transactions$permit_auth_date)) {
      transactions$permit_auth_date <- lubridate::mdy(transactions$permit_auth_date)
    } else if (inherits(transactions$permit_auth_date, c("POSIXct", "POSIXlt"))) {
      transactions$permit_auth_date <- as.Date(transactions$permit_auth_date)
    }
  }

  # Convert numeric fields to proper numeric type
  if ("credits" %in% names(transactions)) {
    if (is.character(transactions$credits)) {
      transactions$credits <- as.numeric(transactions$credits)
    }
  }

  # Standardize bank_id to character for consistency
  if ("bank_id" %in% names(transactions)) {
    transactions$bank_id <- as.character(transactions$bank_id)
  }

  transactions
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
      return(.convert_transaction_types(csv_ledger))
    }
    if (is.null(csv_ledger) || nrow(csv_ledger) == 0) {
      return(.convert_transaction_types(api_ledger))
    }
    # Fall back to 2-way merge if no watershed
    merged <- .harmonize_transactions(api_ledger, csv_ledger, priority = "csv")
    return(.convert_transaction_types(merged))
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
  # API has unique fields: is_transferred, is_purchased, transaction_id
  if (!is.null(api_std)) {
    # Try to merge on both bank_id and transaction_id if available
    # Otherwise fall back to just bank_id
    merge_keys <- c("bank_id")
    if ("transaction_id" %in% names(unified) && "transaction_id" %in% names(api_std)) {
      merge_keys <- c("bank_id", "transaction_id")
    }

    # Preserve API-unique fields (these are authoritative from API)
    api_unique_fields <- c("is_transferred", "is_purchased", "transaction_id")

    unified <- .merge_preserving_columns(
      unified,
      api_std,
      by = merge_keys,
      suffix = c("", "_api"),
      preserve_from_second = api_unique_fields
    )
  }

  # STEP 4: Add CSV ledger fields (if available)
  # CSV ledger has unique fields: sub_ledger_id, permit_auth_date, coordinates, parent_transaction_id
  if (!is.null(csv_std)) {
    # Try to merge on both bank_id and transaction_id
    merge_keys <- c("bank_id")
    if ("transaction_id" %in% names(unified) && "transaction_id" %in% names(csv_std)) {
      merge_keys <- c("bank_id", "transaction_id")
    }

    # Preserve CSV ledger-unique fields (these are authoritative from CSV ledger)
    csv_unique_fields <- c("sub_ledger_id", "permit_auth_date",
                            "impact_latitude", "impact_longitude",
                            "parent_transaction_id", "sub_ledger_project_name")

    unified <- .merge_preserving_columns(
      unified,
      csv_std,
      by = merge_keys,
      suffix = c("", "_csv"),
      preserve_from_second = csv_unique_fields
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
            # Handle both old (name) and new (canonical_name) lookup formats
            name_col <- if ("canonical_name" %in% names(lookup)) "canonical_name" else "name"
            id_to_name <- lookup |>
              dplyr::select(bank_id, !!rlang::sym(name_col)) |>
              dplyr::distinct(bank_id, .keep_all = TRUE) |>
              dplyr::rename(bank_name_global = !!rlang::sym(name_col))

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

  # STEP 7: Deduplicate transactions
  unified <- .deduplicate_transactions(unified)

  # STEP 8: Convert field types to proper classes
  unified <- .convert_transaction_types(unified)

  unified
}


#' Deduplicate transaction records (internal)
#'
#' Removes duplicate transactions based on key fields. Uses a multi-step approach:
#' 1. Exact duplicates (same bank_id, transaction_date, credits, permit_list)
#' 2. Near-duplicates with same bank_id and credits within a small date window
#'
#' @param transactions A data frame of transactions
#' @return Deduplicated transactions with duplicate count in metadata
#' @keywords internal
#' @noRd
.deduplicate_transactions <- function(transactions) {
  if (is.null(transactions) || nrow(transactions) == 0) {
    return(transactions)
  }

  original_count <- nrow(transactions)

  # Key columns for duplicate detection (use what's available)
  key_cols <- intersect(
    c("bank_id", "transaction_date", "credits", "permit_list", "credit_action"),
    names(transactions)
  )

  if (length(key_cols) < 2) {
    # Not enough key columns for meaningful deduplication
    return(transactions)
  }

  # Step 1: Remove exact duplicates based on key columns
  # Keep the row with the most non-NA values
  transactions <- transactions |>
    dplyr::mutate(.row_completeness = rowSums(!is.na(dplyr::pick(dplyr::everything())))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(key_cols)), dplyr::desc(.row_completeness)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(key_cols)), .keep_all = TRUE) |>
    dplyr::select(-.row_completeness)

  # Step 2: Handle near-duplicates (same bank_id/credits but slightly different dates)
  # This catches cases where the same transaction appears with date variations
  if ("transaction_date" %in% names(transactions) && "bank_id" %in% names(transactions)) {
    # Parse dates if character
    if (is.character(transactions$transaction_date)) {
      transactions <- transactions |>
        dplyr::mutate(
          .parsed_date = as.Date(transaction_date, format = "%m/%d/%Y")
        )
    } else if (inherits(transactions$transaction_date, "Date")) {
      transactions <- transactions |>
        dplyr::mutate(.parsed_date = transaction_date)
    }

    if (".parsed_date" %in% names(transactions)) {
      # Group by bank_id and credits, check for date clusters within 7 days
      if ("credits" %in% names(transactions)) {
        transactions <- transactions |>
          dplyr::group_by(bank_id, credits) |>
          dplyr::arrange(.parsed_date) |>
          dplyr::mutate(
            .date_diff = as.numeric(difftime(.parsed_date, dplyr::lag(.parsed_date), units = "days")),
            .is_near_dupe = !is.na(.date_diff) & .date_diff <= 7 & .date_diff >= 0
          ) |>
          dplyr::filter(!.is_near_dupe) |>
          dplyr::ungroup() |>
          dplyr::select(-dplyr::any_of(c(".parsed_date", ".date_diff", ".is_near_dupe")))
      } else {
        transactions <- transactions |>
          dplyr::select(-dplyr::any_of(".parsed_date"))
      }
    }
  }

  final_count <- nrow(transactions)
  removed_count <- original_count - final_count

  if (removed_count > 0) {
    cli::cli_alert_info("Removed {removed_count} duplicate transaction{?s}")
  }

  transactions
}


#' Harmonize transaction data from two sources (LEGACY - for backwards compatibility)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Use `.harmonize_transactions_threeway()` instead.
#'
#' @keywords internal
#' @noRd
.harmonize_transactions <- function(api_ledger, csv_ledger, priority = NULL) {
  lifecycle::deprecate_soft(
    when = "0.1.0",
    what = ".harmonize_transactions()",
    with = ".harmonize_transactions_threeway()"
  )

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
      dplyr::arrange(.data$transaction_id, dplyr::desc(source == "api+csv"), dplyr::desc(source == priority)) |>
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
