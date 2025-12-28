# R/transactions-harmonize.R
# Transaction harmonization and merging logic
# Split from R/harmonize-transactions.R (644 lines → focused modules)

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
