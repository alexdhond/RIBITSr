# R/enriched-tables.R
# Enriched data tables that harmonize RIBITS data from multiple sources
# Provides separate tables at different grains: bank-level, transaction-level, credit-level, HUC-level

# ==============================================================================
# PUBLIC API
# ==============================================================================

#' Get Enriched Data Tables
#'
#' @description
#' Unified interface for fetching enriched RIBITS data at different grains.
#' Returns harmonized data from multiple sources with consistent structure.
#'
#' @param type Character: type of enriched data to retrieve. One of:
#'   \itemize{
#'     \item \code{"banks"} - Bank-level summary with credits and transactions
#'     \item \code{"credits"} - Credit classification data by bank
#'     \item \code{"transactions"} - Transaction-level detail
#'     \item \code{"huc"} - HUC/watershed-level credit availability
#'   }
#' @param state State abbreviation or name to filter by (optional)
#' @param district USACE district to filter by (optional)
#' @param bank_ids Specific bank IDs to include (optional)
#' @param huc8 HUC8 codes to filter by (only for type="huc")
#' @param ... Type-specific options (see details)
#' @param cache If TRUE (default), cache downloaded files
#' @param quietly If TRUE, suppress progress messages
#'
#' @details
#' Type-specific options passed via \code{...}:
#'
#' \strong{For type="banks":}
#' \itemize{
#'   \item \code{include_credits} - Include credit summary (default TRUE)
#'   \item \code{include_transaction_summary} - Include transaction summary (default TRUE)
#'   \item \code{include_geometry} - Include spatial geometry (default FALSE)
#'   \item \code{sources} - Data sources: c("csv", "epa") (default both)
#' }
#'
#' \strong{For type="credits":}
#' \itemize{
#'   \item \code{include_releases} - Include anticipated releases (default TRUE)
#'   \item \code{pivot_wide} - Return wide format (default TRUE)
#' }
#'
#' \strong{For type="transactions":}
#' \itemize{
#'   \item \code{sources} - Data sources: c("csv_watershed", "csv_ledger", "api")
#' }
#'
#' @return A tibble or sf object with enriched data and class attributes.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Get enriched bank summary
#' banks <- rb_enriched("banks")
#'
#' # Get credit data for California
#' ca_credits <- rb_enriched("credits", state = "CA")
#'
#' # Get transactions for specific banks
#' txn <- rb_enriched("transactions", bank_ids = c(100, 200, 300))
#'
#' # Get HUC data
#' huc <- rb_enriched("huc", state = "FL")
#'
#' # Banks with geometry
#' banks_sf <- rb_enriched("banks", state = "CA", include_geometry = TRUE)
#' }
rb_enriched <- function(type = c("banks", "credits", "transactions", "huc"),
                        state = NULL,
                        district = NULL,
                        bank_ids = NULL,
                        huc8 = NULL,
                        ...,
                        cache = TRUE,
                        quietly = FALSE) {

  type <- match.arg(type)

  switch(type,
    banks = .enrich_banks_impl(
      state = state, district = district, bank_ids = bank_ids,
      ..., cache = cache, quietly = quietly
    ),
    credits = .enrich_credits_impl(
      state = state, district = district, bank_ids = bank_ids,
      ..., cache = cache, quietly = quietly
    ),
    transactions = .enrich_transactions_impl(
      state = state, district = district, bank_ids = bank_ids,
      ..., cache = cache, quietly = quietly
    ),
    huc = .enrich_huc_impl(
      huc8 = huc8, state = state,
      ..., cache = cache, quietly = quietly
    )
  )
}


# ==============================================================================
# Internal Helpers
# ==============================================================================

#' Get bank IDs matching state/district filter
#'
#' @description
#' Resolves state and/or district filters to a vector of bank_ids.
#' Uses CSV banks_sites as the authoritative source for all banks.
#'
#' @param state State abbreviation or name to filter by
#' @param district USACE district to filter by
#' @param cache If TRUE, cache downloaded files
#' @param quietly If TRUE, suppress progress messages
#'
#' @return Integer vector of bank_ids, or NULL if no filter specified
#'
#' @keywords internal
.get_bank_ids_for_filter <- function(state = NULL, district = NULL, cache = TRUE, quietly = FALSE) {
  # If no filter specified, return NULL (meaning "all banks")
  if (is.null(state) && is.null(district)) {
    return(NULL)
  }

  # Use banks_sites CSV as the complete source

cache_dir <- .get_cache_dir(cache)

  banks <- tryCatch({
    csv_file <- rb_download_report("banks_sites", download_dir = cache_dir)
    rb_read(csv_file)
  }, error = function(e) {
    if (!quietly) cli::cli_alert_warning("Failed to fetch banks_sites: {e$message}")
    return(NULL)
  })

  if (is.null(banks) || nrow(banks) == 0) {
    return(integer())
  }

  # Normalize columns
  banks <- .normalize_columns(banks)

  # Ensure bank_id exists
  banks <- .ensure_bank_id(banks, quietly = quietly)

  # Filter by state
  if (!is.null(state)) {
    state <- .standardize_state(state)
    state_col <- .get_column_case_insensitive(banks, "state_list")
    if (!is.na(state_col)) {
      banks <- banks |> dplyr::filter(grepl(!!state, .data[[state_col]], ignore.case = TRUE))
    }
  }

  # Filter by district
  if (!is.null(district)) {
    district_col <- .get_column_case_insensitive(banks, "district")
    if (!is.na(district_col)) {
      banks <- banks |> dplyr::filter(grepl(!!district, .data[[district_col]], ignore.case = TRUE))
    }
  }

  # Return unique bank_ids
  unique(banks$bank_id[!is.na(banks$bank_id)])
}


# ==============================================================================
# Implementation Functions
# ==============================================================================

#' Enriched credits implementation
#' @keywords internal
#' @noRd
.enrich_credits_impl <- function(state = NULL,
                                 district = NULL,
                                 bank_ids = NULL,
                                 include_releases = TRUE,
                                 pivot_wide = TRUE,
                                 cache = TRUE,
                                 quietly = FALSE) {

  if (!quietly) cli::cli_h2("Fetching Enriched Credit Data")

  # Resolve bank_ids from state/district if not provided
  if (is.null(bank_ids) && (!is.null(state) || !is.null(district))) {
    if (!quietly) cli::cli_progress_step("Resolving bank IDs for filter...")
    bank_ids <- .get_bank_ids_for_filter(state, district, cache, quietly)

    if (length(bank_ids) == 0) {
      if (!quietly) cli::cli_alert_warning("No banks found matching filter criteria")
      return(tibble::tibble(bank_id = integer()))
    }
    if (!quietly) cli::cli_alert_info("Found {length(bank_ids)} banks matching filter")
  }

  # Fetch credit classification data
  if (!quietly) cli::cli_progress_step("Downloading credit classification data...")

  credit_data <- .safe_fetch(
    fn = function() .fetch_csv_with_bank_id("credit_classification", bank_ids, cache, quietly),
    description = "credit_classification CSV",
    quietly = quietly,
    default_value = NULL
  )

  if (is.null(credit_data) || nrow(credit_data) == 0) {
    if (!quietly) cli::cli_alert_warning("No credit classification data found")
    return(tibble::tibble(bank_id = integer()))
  }

  if (!quietly) cli::cli_alert_success("Retrieved {nrow(credit_data)} credit classification records")

  if (pivot_wide) {
    # Aggregate to one row per bank using existing summarizer
    if (!quietly) cli::cli_progress_step("Aggregating credits by bank...")

    result <- .summarize_credits(credit_data)

    if (!quietly) cli::cli_alert_success("Summarized to {nrow(result)} banks")

    # Optionally add anticipated releases
    if (include_releases) {
      if (!quietly) cli::cli_progress_step("Fetching anticipated credit releases...")

      releases_data <- .safe_fetch(
        fn = function() .fetch_csv_with_bank_id("credit_releases", bank_ids, cache, quietly),
        description = "credit_releases CSV",
        quietly = quietly,
        default_value = NULL
      )

      if (!is.null(releases_data) && nrow(releases_data) > 0) {
        if (!quietly) cli::cli_alert_success("Retrieved {nrow(releases_data)} release records")

        releases_summary <- .summarize_credit_releases(releases_data)

        if (nrow(releases_summary) > 0) {
          result <- dplyr::left_join(result, releases_summary, by = "bank_id")
          if (!quietly) cli::cli_alert_info("Added release data for {sum(!is.na(result$future_anticipated_credits))} banks")
        }
      } else {
        if (!quietly) cli::cli_alert_info("No anticipated credit releases found")
      }
    }

  } else {
    # Return long format (one row per bank + classification)
    if (!quietly) cli::cli_progress_step("Preparing long format...")

    result <- credit_data |>
      dplyr::select(
        dplyr::any_of(c(
          "bank_id", "bank_name", "jurisdiction",
          "credit_classification", "credit_classification_type",
          "available_credits", "released_credits",
          "potential_credits", "withdrawn_credits"
        ))
      ) |>
      dplyr::arrange(.data$bank_id, .data$credit_classification)
  }

  # Add class and metadata
  result <- structure(
    result,
    class = c("ribits_enriched_credits", class(result)),
    fetch_date = Sys.Date(),
    format = if (pivot_wide) "wide" else "long",
    n_banks = length(unique(result$bank_id)),
    include_releases = include_releases
  )

  if (!quietly) {
    cli::cli_alert_success(
      "Enriched credits: {nrow(result)} rows, {ncol(result)} columns, {attr(result, 'n_banks')} banks"
    )
  }

  result
}


#' Enriched banks implementation
#' @keywords internal
#' @noRd
.enrich_banks_impl <- function(state = NULL,
                               district = NULL,
                               bank_ids = NULL,
                               include_credits = TRUE,
                               include_transaction_summary = TRUE,
                               include_geometry = FALSE,
                               sources = c("csv", "epa"),
                               cache = TRUE,
                               quietly = FALSE) {

  if (!quietly) cli::cli_h2("Fetching Enriched Bank Data")

  sources <- match.arg(sources, c("csv", "epa"), several.ok = TRUE)
  cache_dir <- .get_cache_dir(cache)

  # Step 1: Resolve bank_ids from state/district if not provided
  filter_ids <- NULL
  if (!is.null(state) || !is.null(district)) {
    if (!quietly) cli::cli_progress_step("Resolving bank IDs for filter...")
    filter_ids <- .get_bank_ids_for_filter(state, district, cache, quietly)

    if (length(filter_ids) == 0) {
      if (!quietly) cli::cli_alert_warning("No banks found matching filter criteria")
      return(tibble::tibble(bank_id = integer()))
    }
    if (!quietly) cli::cli_alert_info("Found {length(filter_ids)} banks matching filter")
  }

  # Use provided bank_ids or filter_ids
  query_ids <- bank_ids %||% filter_ids

  # Step 2: Fetch base bank data from CSV (banks_sites - has ALL banks)
  if (!quietly) cli::cli_progress_step("Fetching CSV banks_sites (base data)...")

  banks_csv <- NULL
  if ("csv" %in% sources) {
    banks_csv <- .safe_fetch(
      fn = function() .fetch_csv_with_bank_id("banks_sites", query_ids, cache, quietly),
      description = "banks_sites CSV",
      quietly = quietly,
      default_value = NULL
    )

    if (!is.null(banks_csv) && nrow(banks_csv) > 0) {
      if (!quietly) cli::cli_alert_success("Retrieved {nrow(banks_csv)} banks from CSV")
    }
  }

  # Step 3: Fetch EPA data (Approved banks only, has geometry + secondary districts)
  banks_epa <- NULL
  if ("epa" %in% sources) {
    if (!quietly) cli::cli_progress_step("Fetching EPA ArcGIS data...")

    banks_epa <- .safe_fetch(
      fn = function() {
        epa_data <- rb_epa("banks", bank_ids = query_ids, return_geometry = include_geometry)
        if (!is.null(epa_data) && nrow(epa_data) > 0) {
          # Drop geometry for merge if not requested (will add back later)
          if (!include_geometry && inherits(epa_data, "sf")) {
            epa_data <- sf::st_drop_geometry(epa_data)
          }
          .normalize_columns(epa_data)
        } else {
          NULL
        }
      },
      description = "EPA ArcGIS banks",
      quietly = quietly,
      default_value = NULL
    )

    if (!is.null(banks_epa) && nrow(banks_epa) > 0) {
      if (!quietly) cli::cli_alert_success("Retrieved {nrow(banks_epa)} banks from EPA")
    }
  }

  # Step 4: Determine base and merge
  if (is.null(banks_csv) && is.null(banks_epa)) {
    if (!quietly) cli::cli_alert_danger("No bank data retrieved from any source")
    return(tibble::tibble(bank_id = integer()))
  }

  # Use CSV as base (has all statuses), enrich with EPA
  if (!is.null(banks_csv) && !is.null(banks_epa)) {
    if (!quietly) cli::cli_progress_step("Merging CSV and EPA data...")

    # For geometry, we need to handle sf objects specially
    if (include_geometry && inherits(banks_epa, "sf")) {
      # Keep EPA as sf, join CSV attributes
      result <- banks_epa |>
        dplyr::left_join(
          banks_csv |> dplyr::select(-dplyr::any_of(names(banks_epa)[names(banks_epa) != "bank_id"])),
          by = "bank_id"
        )
      # Add banks from CSV that aren't in EPA (without geometry)
      csv_only_ids <- setdiff(banks_csv$bank_id, banks_epa$bank_id)
      if (length(csv_only_ids) > 0) {
        csv_only <- banks_csv |> dplyr::filter(.data$bank_id %in% csv_only_ids)
        # Convert to sf with empty geometry
        csv_only_sf <- sf::st_as_sf(csv_only, coords = c("lon", "lat"), crs = 4326, na.fail = FALSE)
        result <- dplyr::bind_rows(result, csv_only_sf)
      }
    } else {
      # Standard merge
      result <- .merge_preserving_columns(
        banks_csv, banks_epa,
        by = "bank_id",
        preserve_from_second = c("bank_status_date", "secondary_district_list", "secondary_office_list")
      )
    }

    if (!quietly) cli::cli_alert_success("Merged to {nrow(result)} banks")

  } else if (!is.null(banks_csv)) {
    result <- banks_csv
  } else {
    result <- banks_epa
  }

  # Step 5: Add credit summary
  if (include_credits) {
    if (!quietly) cli::cli_progress_step("Adding credit summary...")

    credit_data <- .safe_fetch(
      fn = function() .fetch_csv_with_bank_id("credit_classification", result$bank_id, cache, quietly),
      description = "credit_classification CSV",
      quietly = quietly,
      default_value = NULL
    )

    if (!is.null(credit_data) && nrow(credit_data) > 0) {
      credit_summary <- .summarize_credits(credit_data)
      if (nrow(credit_summary) > 0) {
        result <- dplyr::left_join(result, credit_summary, by = "bank_id")
        if (!quietly) cli::cli_alert_info("Added credit data for {sum(!is.na(result$total_available_credits))} banks")
      }
    } else {
      if (!quietly) cli::cli_alert_info("No credit data available")
    }
  }

  # Step 6: Add transaction summary
  if (include_transaction_summary) {
    if (!quietly) cli::cli_progress_step("Adding transaction summary...")

    txn_data <- .safe_fetch(
      fn = function() .fetch_csv_with_bank_id("transactions_watershed", result$bank_id, cache, quietly),
      description = "transactions_watershed CSV",
      quietly = quietly,
      default_value = NULL
    )

    if (!is.null(txn_data) && nrow(txn_data) > 0) {
      txn_summary <- .summarize_transactions(txn_data)
      if (nrow(txn_summary) > 0) {
        result <- dplyr::left_join(result, txn_summary, by = "bank_id")
        if (!quietly) cli::cli_alert_info("Added transaction data for {sum(!is.na(result$n_transactions))} banks")
      }
    } else {
      if (!quietly) cli::cli_alert_info("No transaction data available")
    }
  }

  # Step 7: Finalize
  if (!quietly) cli::cli_progress_step("Finalizing...")

  # Don't use .finalize_df if we have geometry (it might break sf)
  if (!include_geometry || !inherits(result, "sf")) {
    result <- .finalize_df(result, type = "banks")
  }

  # Add class and metadata
  result <- structure(
    result,
    class = c("ribits_enriched_banks", class(result)),
    fetch_date = Sys.Date(),
    sources = sources,
    include_credits = include_credits,
    include_transaction_summary = include_transaction_summary,
    include_geometry = include_geometry
  )

  if (!quietly) {
    cli::cli_alert_success(
      "Enriched banks: {nrow(result)} banks, {ncol(result)} columns"
    )
  }

  result
}


#' Enriched transactions implementation
#' @keywords internal
#' @noRd
.enrich_transactions_impl <- function(state = NULL,
                                       district = NULL,
                                       bank_ids = NULL,
                                       sources = c("csv_watershed", "csv_ledger"),
                                       cache = TRUE,
                                       quietly = FALSE) {

  if (!quietly) cli::cli_h2("Fetching Enriched Transaction Data")

  sources <- match.arg(sources, c("csv_watershed", "csv_ledger", "api"), several.ok = TRUE)
  cache_dir <- .get_cache_dir(cache)

  # Resolve bank_ids from state/district if not provided
  if (is.null(bank_ids) && (!is.null(state) || !is.null(district))) {
    if (!quietly) cli::cli_progress_step("Resolving bank IDs for filter...")
    bank_ids <- .get_bank_ids_for_filter(state, district, cache, quietly)

    if (length(bank_ids) == 0) {
      if (!quietly) cli::cli_alert_warning("No banks found matching filter criteria")
      return(tibble::tibble(bank_id = integer()))
    }
    if (!quietly) cli::cli_alert_info("Found {length(bank_ids)} banks matching filter")
  }

  # Fetch transactions_watershed (foundation - 71 cols, has bank_id)
  txn_watershed <- NULL
  if ("csv_watershed" %in% sources) {
    if (!quietly) cli::cli_progress_step("Fetching transactions_watershed CSV...")

    txn_watershed <- .safe_fetch(
      fn = function() .fetch_csv_with_bank_id("transactions_watershed", bank_ids, cache, quietly),
      description = "transactions_watershed CSV",
      quietly = quietly,
      default_value = NULL
    )

    if (!is.null(txn_watershed) && nrow(txn_watershed) > 0) {
      if (!quietly) cli::cli_alert_success("Retrieved {nrow(txn_watershed)} transactions ({ncol(txn_watershed)} cols)")
    }
  }

  # Fetch ledger_transactions (sub-ledger detail, permit_auth_date)
  txn_ledger <- NULL
  if ("csv_ledger" %in% sources) {
    if (!quietly) cli::cli_progress_step("Fetching ledger_transactions CSV...")

    txn_ledger <- .safe_fetch(
      fn = function() .fetch_csv_with_bank_id("ledger_transactions", bank_ids, cache, quietly),
      description = "ledger_transactions CSV",
      quietly = quietly,
      default_value = NULL
    )

    if (!is.null(txn_ledger) && nrow(txn_ledger) > 0) {
      if (!quietly) cli::cli_alert_success("Retrieved {nrow(txn_ledger)} ledger records")
    }
  }

  # Determine what we have
  if (is.null(txn_watershed) && is.null(txn_ledger)) {
    if (!quietly) cli::cli_alert_danger("No transaction data retrieved from any source")
    return(tibble::tibble(bank_id = integer()))
  }

  # Merge sources if we have multiple
  if (!is.null(txn_watershed) && !is.null(txn_ledger)) {
    if (!quietly) cli::cli_progress_step("Merging transaction sources...")

    # Use the 3-way merge pattern from transactions-harmonize.R
    # Watershed is foundation, ledger adds unique fields
    result <- .merge_preserving_columns(
      txn_watershed, txn_ledger,
      by = "bank_id",  # Will match on bank_id; transaction_id may not align perfectly
      preserve_from_second = c("sub_ledger_id", "permit_auth_date", "parent_transaction_id")
    )

    if (!quietly) cli::cli_alert_success("Merged to {nrow(result)} transaction records")

  } else if (!is.null(txn_watershed)) {
    result <- txn_watershed
  } else {
    result <- txn_ledger
  }

  # Finalize
  if (!quietly) cli::cli_progress_step("Finalizing...")

  result <- .finalize_df(result, type = "transactions")

  # Add class and metadata
  result <- structure(
    result,
    class = c("ribits_enriched_transactions", class(result)),
    fetch_date = Sys.Date(),
    sources = sources,
    n_banks = length(unique(result$bank_id))
  )

  if (!quietly) {
    cli::cli_alert_success(
      "Enriched transactions: {nrow(result)} records, {ncol(result)} columns, {attr(result, 'n_banks')} banks"
    )
  }

  result
}


#' Enriched HUC implementation
#' @keywords internal
#' @noRd
.enrich_huc_impl <- function(huc8 = NULL,
                             state = NULL,
                             cache = TRUE,
                             quietly = FALSE) {

  if (!quietly) cli::cli_h2("Fetching Enriched HUC Data")

  cache_dir <- .get_cache_dir(cache)

  # Download HUC data
  if (!quietly) cli::cli_progress_step("Downloading available_credits_huc CSV...")

  huc_data <- .safe_fetch(
    fn = function() {
      csv_file <- rb_download_report("available_credits_huc", download_dir = cache_dir)
      rb_read(csv_file)
    },
    description = "available_credits_huc CSV",
    quietly = quietly,
    default_value = NULL
  )

  if (is.null(huc_data) || nrow(huc_data) == 0) {
    if (!quietly) cli::cli_alert_danger("No HUC data retrieved")
    return(tibble::tibble())
  }

  if (!quietly) cli::cli_alert_success("Retrieved {nrow(huc_data)} HUC records ({ncol(huc_data)} cols)")

  # Normalize columns
  huc_data <- .normalize_columns(huc_data)

  # Filter by HUC8 if specified
  if (!is.null(huc8)) {
    if (!quietly) cli::cli_progress_step("Filtering to specified HUC8 codes...")

    # Find the HUC8 column (may have different names)
    huc_col <- names(huc_data)[grepl("huc.*8|huc8|huc_8", names(huc_data), ignore.case = TRUE)][1]

    if (!is.na(huc_col)) {
      huc_data <- huc_data |> dplyr::filter(.data[[huc_col]] %in% huc8)
      if (!quietly) cli::cli_alert_info("Filtered to {nrow(huc_data)} records")
    } else {
      if (!quietly) cli::cli_alert_warning("No HUC8 column found for filtering")
    }
  }

  # Filter by state if specified
  if (!is.null(state)) {
    if (!quietly) cli::cli_progress_step("Filtering to state: {state}...")

    state <- .standardize_state(state)
    state_col <- names(huc_data)[grepl("state", names(huc_data), ignore.case = TRUE)][1]

    if (!is.na(state_col)) {
      huc_data <- huc_data |> dplyr::filter(grepl(!!state, .data[[state_col]], ignore.case = TRUE))
      if (!quietly) cli::cli_alert_info("Filtered to {nrow(huc_data)} records")
    } else {
      if (!quietly) cli::cli_alert_warning("No state column found for filtering")
    }
  }

  # Add class and metadata
  result <- structure(
    huc_data,
    class = c("ribits_enriched_huc", class(huc_data)),
    fetch_date = Sys.Date()
  )

  if (!quietly) {
    cli::cli_alert_success(
      "Enriched HUC: {nrow(result)} records, {ncol(result)} columns"
    )
  }

  result
}


# ---- Print Methods ----

#' @export
print.ribits_enriched_credits <- function(x, ...) {
  cli::cli_h1("Enriched Credit Summary")
  cli::cli_alert_info("{nrow(x)} rows, {ncol(x)} columns")
  cli::cli_alert_info("Banks: {attr(x, 'n_banks')}")
  cli::cli_alert_info("Format: {attr(x, 'format')}")
  cli::cli_alert_info("Fetched: {attr(x, 'fetch_date')}")
  if (attr(x, "include_releases")) {
    cli::cli_alert_info("Includes: anticipated releases")
  }
  cat("\n")
  NextMethod()
}

#' @export
print.ribits_enriched_banks <- function(x, ...) {
  cli::cli_h1("Enriched Bank Summary")
  cli::cli_alert_info("{nrow(x)} banks, {ncol(x)} columns")
  cli::cli_alert_info("Sources: {paste(attr(x, 'sources'), collapse = ' + ')}")
  cli::cli_alert_info("Fetched: {attr(x, 'fetch_date')}")

  includes <- c()
  if (attr(x, "include_credits")) includes <- c(includes, "credits")
  if (attr(x, "include_transaction_summary")) includes <- c(includes, "transactions")
  if (attr(x, "include_geometry")) includes <- c(includes, "geometry")
  if (length(includes) > 0) {
    cli::cli_alert_info("Includes: {paste(includes, collapse = ', ')}")
  }
  cat("\n")
  NextMethod()
}

#' @export
print.ribits_enriched_transactions <- function(x, ...) {
  cli::cli_h1("Enriched Transactions")
  cli::cli_alert_info("{nrow(x)} transactions, {ncol(x)} columns")
  cli::cli_alert_info("Banks: {attr(x, 'n_banks')}")
  cli::cli_alert_info("Sources: {paste(attr(x, 'sources'), collapse = ' + ')}")
  cli::cli_alert_info("Fetched: {attr(x, 'fetch_date')}")
  cat("\n")
  NextMethod()
}

#' @export
print.ribits_enriched_huc <- function(x, ...) {
  cli::cli_h1("Enriched HUC Data")
  cli::cli_alert_info("{nrow(x)} HUC records, {ncol(x)} columns")
  cli::cli_alert_info("Fetched: {attr(x, 'fetch_date')}")
  cat("\n")
  NextMethod()
}
