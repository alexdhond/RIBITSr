# R/name-lookup.R
# Name-to-ID lookup functions for harmonizing CSV data with API/EPA

#' Build a name-to-ID lookup table
#'
#' Creates a lookup table mapping bank/program names to their IDs using
#' data from EPA ArcGIS and the RIBITS API. This function implements a
#' smart caching strategy to minimize API calls:
#'
#' 1. Checks persistent user cache first (default: 30 day refresh)
#' 2. If cache is stale/missing, fetches fresh data from APIs
#' 3. Falls back to bundled package data if APIs are unavailable
#'
#' @param include_csv Logical. Also include mappings from transactions_watershed CSV
#'   when fetching fresh data. Default TRUE.
#' @param max_age_days Integer. Maximum age in days for cached data before
#'   auto-refresh. Default 30. Set to 0 to always fetch fresh data.
#' @param force_refresh Logical. Force refresh from APIs even if cache is fresh.
#'   Default FALSE.
#'
#' @return A tibble with columns: name, bank_id, state, district, year_established,
#'   source (data source), and name_normalized (for fuzzy matching)
#'
#' @details
#' The persistent cache is stored in the user's cache directory
#' (see [rappdirs::user_cache_dir()]). This means:
#' - Cache persists across R sessions
#' - First run fetches from API (~10-30 seconds)
#' - Subsequent runs are near-instant (reads from disk)
#' - Auto-refreshes every 30 days to capture new banks
#' - Works offline using bundled fallback data
#'
#' @seealso [rb_match_names()] to use the lookup table for name matching
#' @export
#' @examples
#' \dontrun{
#' # Build lookup table (uses cache if available)
#' lookup <- rb_build_name_lookup()
#'
#' # Force refresh from APIs
#' lookup <- rb_build_name_lookup(force_refresh = TRUE)
#'
#' # Use shorter cache lifetime (7 days)
#' lookup <- rb_build_name_lookup(max_age_days = 7)
#'
#' # Check when bundled data was generated
#' data(banks_lookup)
#' attr(banks_lookup, "generated_date")
#' }
rb_build_name_lookup <- function(include_csv = TRUE,
                                  max_age_days = 30,
                                  force_refresh = FALSE) {

  # Step 1: Try persistent user cache first
  cache_file <- file.path(rappdirs::user_cache_dir("ribits"), "banks_lookup.rds")

  if (!force_refresh && file.exists(cache_file)) {
    cache_age <- difftime(Sys.time(), file.mtime(cache_file), units = "days")

    if (cache_age < max_age_days) {
      cli::cli_alert_info("Using cached lookup ({round(cache_age, 1)} days old)")
      lookup <- readRDS(cache_file)
      return(lookup)
    } else {
      cli::cli_alert_info("Cache is stale ({round(cache_age, 1)} days old), refreshing...")
    }
  }

  # Step 2: Cache is stale/missing or force refresh - try to fetch fresh data
  cli::cli_h2("Fetching fresh bank lookup data")

  lookup <- tryCatch({
    .fetch_fresh_bank_lookup(include_csv = include_csv)
  }, error = function(e) {
    # Step 3: API failed - fall back to bundled package data
    cli::cli_alert_warning("Could not fetch fresh data: {e$message}")
    cli::cli_alert_info("Using bundled package data as fallback")

    # Load bundled data (ships with package)
    data("banks_lookup", package = "RIBITSr", envir = environment())

    # Check age of bundled data
    gen_date <- attr(banks_lookup, "generated_date")
    if (!is.null(gen_date)) {
      age_days <- difftime(Sys.Date(), gen_date, units = "days")
      cli::cli_alert_warning("Bundled data is {round(age_days, 0)} days old (generated {gen_date})")
    }

    banks_lookup
  })

  # Step 4: Save to user cache (if we got fresh data successfully)
  if (!inherits(lookup, "error") && !is.null(attr(lookup, "fetch_date"))) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(lookup, cache_file)
    cli::cli_alert_success("Cached to: {cache_file}")
  }

  lookup
}


#' Fetch fresh bank lookup data from APIs
#' @keywords internal
#' @noRd
.fetch_fresh_bank_lookup <- function(include_csv = TRUE) {

  lookup_sources <- list()

  # Source 1: EPA ArcGIS (most reliable)
  cli::cli_progress_step("Fetching EPA bank data...")
  epa_banks <- tryCatch({
    rb_epa_query("approved_banks",
                 out_fields = "BANK_ID,BANK_NAME,STATE_LIST,DISTRICT,YEAR_ESTABLISHED",
                 return_geometry = FALSE)
  }, error = function(e) NULL)

  if (!is.null(epa_banks) && nrow(epa_banks) > 0) {
    # Normalize column names for consistent access (original may be UPPERCASE)
    names(epa_banks) <- tolower(names(epa_banks))
    lookup_sources$epa <- tibble::tibble(
      name = epa_banks$bank_name,
      bank_id = epa_banks$bank_id,
      state = epa_banks$state_list,
      district = epa_banks$district,
      year_established = epa_banks$year_established,
      source = "epa"
    )
    cli::cli_alert_success("EPA: {nrow(lookup_sources$epa)} banks")
  }

  # Source 2: RIBITS API list
  cli::cli_progress_step("Fetching API bank list...")
  api_banks <- tryCatch({
    rb_list_generic("banks")
  }, error = function(e) NULL)

  if (!is.null(api_banks) && nrow(api_banks) > 0) {
    # Normalize column names for consistent access (original may be UPPERCASE)
    names(api_banks) <- tolower(names(api_banks))
    lookup_sources$api <- tibble::tibble(
      name = api_banks$name,
      bank_id = api_banks$bank_id,
      state = NA_character_,
      district = NA_character_,
      year_established = NA_integer_,
      source = "api"
    )
    cli::cli_alert_success("API: {nrow(lookup_sources$api)} banks")
  }

  # Source 3: Transactions by Watershed CSV (has name + bank_id pairs)
  if (include_csv) {
    cli::cli_progress_step("Fetching CSV transaction data...")
    csv_lookup <- tryCatch({
      csv_file <- rb_download_report("transactions_watershed", download_dir = tempdir())
      csv_data <- readr::read_csv(csv_file, show_col_types = FALSE)

      # Extract unique name-ID pairs
      csv_data |>
        dplyr::filter(!is.na(`Bank ID`)) |>
        dplyr::select(name = Name, bank_id = `Bank ID`, state = `State List`) |>
        dplyr::distinct() |>
        dplyr::mutate(
          district = NA_character_,
          year_established = NA_integer_,
          source = "csv"
        )
    }, error = function(e) NULL)

    if (!is.null(csv_lookup) && nrow(csv_lookup) > 0) {
      lookup_sources$csv <- csv_lookup
      cli::cli_alert_success("CSV: {nrow(lookup_sources$csv)} banks")
    }
  }

  # Combine all sources (EPA takes priority, then API, then CSV)
  if (length(lookup_sources) == 0) {
    stop("No lookup data available from any source")
  }

  # Stack and deduplicate (keep first occurrence = highest priority source)
  combined <- dplyr::bind_rows(lookup_sources) |>
    dplyr::group_by(.data$bank_id) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  # Also create normalized name for fuzzy matching
  combined <- combined |>
    dplyr::mutate(
      name_normalized = tolower(name) |>
        stringr::str_replace_all("[^a-z0-9]", " ") |>
        stringr::str_squish()
    )

  # Add metadata
  attr(combined, "fetch_date") <- Sys.Date()
  attr(combined, "n_banks") <- nrow(combined)

  cli::cli_alert_success("Built lookup table: {nrow(combined)} unique banks")

  combined
}


#' Match names to bank IDs using fuzzy matching
#'
#' Matches bank names from CSV data to bank IDs using the lookup table.
#' Supports exact matching, fuzzy string matching, and secondary identifiers.
#'
#' @param data Data frame with bank names to match
#' @param lookup Lookup table from `rb_build_name_lookup()`. If NULL, builds one.
#' @param name_col Name of the column containing bank names. Default "bank_name".
#' @param state_col Optional. Column with state for secondary matching.
#' @param year_col Optional. Column with year established for secondary matching.
#' @param fuzzy Logical. Use fuzzy string matching. Default TRUE.
#' @param threshold Minimum similarity score (0-1) for fuzzy matches. Default 0.8.
#'
#' @return The input data with `bank_id` and `match_quality` columns added
#' @keywords internal
#' @examples
#' \dontrun{
#' # Match CSV data to bank IDs
#' csv_data <- rb_read("credit_classification.csv")
#' matched <- rb_match_names(csv_data, name_col = "Bank Name")
#'
#' # Check match quality
#' table(matched$match_quality)
#' }
rb_match_names <- function(data,
                           lookup = NULL,
                           name_col = "bank_name",
                           state_col = NULL,
                           year_col = NULL,
                           fuzzy = TRUE,
                           threshold = 0.8) {
  
  # Build lookup if not provided
  if (is.null(lookup)) {
    lookup <- rb_build_name_lookup()
  }
  
  if (is.null(lookup) || nrow(lookup) == 0) {
    cli::cli_alert_warning("Empty lookup table - cannot match names")
    return(data)
  }
  
  # Check name column exists
  if (!name_col %in% names(data)) {
    # Try common variations
    possible_cols <- c("bank_name", "Bank Name", "name", "Name", "bank name")
    found_col <- possible_cols[possible_cols %in% names(data)]
    if (length(found_col) > 0) {
      name_col <- found_col[1]
      cli::cli_alert_info("Using column: {name_col}")
    } else {
      cli::cli_abort("Column '{name_col}' not found. Available: {paste(names(data), collapse = ', ')}")
    }
  }
  
  # Normalize names in data
  data <- data |>
    dplyr::mutate(
      .name_normalized = tolower(.data[[name_col]]) |>
        stringr::str_replace_all("[^a-z0-9]", " ") |>
        stringr::str_squish()
    )
  
  # Step 1: Exact match on normalized names
  cli::cli_progress_step("Matching names...")
  
  exact_matches <- data |>
    dplyr::left_join(
      lookup |> dplyr::select(name_normalized, bank_id_exact = bank_id),
      by = c(".name_normalized" = "name_normalized")
    )
  
  # Initialize fuzzy columns
  exact_matches$bank_id_fuzzy <- NA_integer_
  exact_matches$fuzzy_score <- NA_real_
  
  n_exact <- sum(!is.na(exact_matches$bank_id_exact))
  cli::cli_alert_success("Exact matches: {n_exact}/{nrow(data)}")
  
  # Step 2: Fuzzy matching for unmatched rows (VECTORIZED for 10-50x speedup)
  if (fuzzy && n_exact < nrow(data)) {
    unmatched_idx <- which(is.na(exact_matches$bank_id_exact))

    if (length(unmatched_idx) > 0) {
      # Get unmatched names and filter out empty/NA
      unmatched_names <- exact_matches$.name_normalized[unmatched_idx]
      valid_mask <- !is.na(unmatched_names) & nchar(unmatched_names) > 0
      valid_idx <- unmatched_idx[valid_mask]
      valid_names <- unmatched_names[valid_mask]

      if (length(valid_names) > 0) {
        # Vectorized similarity computation
        # stringdist::stringsimmatrix computes ALL similarities at once in C code
        # Much faster than sapply loop: O(n*m) in C vs O(n*m) in R
        sim_matrix <- stringdist::stringsimmatrix(
          valid_names,
          lookup$name_normalized,
          method = "jw"
        )

        # Find best match for each row
        best_indices <- apply(sim_matrix, 1, which.max)
        best_scores <- apply(sim_matrix, 1, max)

        # Apply threshold and assign matches
        matches <- ifelse(best_scores >= threshold, lookup$bank_id[best_indices], NA)

        exact_matches$bank_id_fuzzy[valid_idx] <- matches
        exact_matches$fuzzy_score[valid_idx] <- best_scores
      }

      n_fuzzy <- sum(!is.na(exact_matches$bank_id_fuzzy))
      cli::cli_alert_success("Fuzzy matches: {n_fuzzy}")

      # Show performance info if verbose
      if (.network_options$verbose && length(valid_names) > 0) {
        cli::cli_alert_info(
          "Vectorized matching processed {length(valid_names)} names against {nrow(lookup)} banks"
        )
      }
    }
  }
  
  # Combine results
  result <- exact_matches |>
    dplyr::mutate(
      bank_id = dplyr::coalesce(bank_id_exact, bank_id_fuzzy),
      match_quality = dplyr::case_when(
        !is.na(bank_id_exact) ~ "exact",
        !is.na(bank_id_fuzzy) ~ paste0("fuzzy_", round(fuzzy_score, 2)),
        TRUE ~ "unmatched"
      )
    ) |>
    dplyr::select(-bank_id_exact, -bank_id_fuzzy, -fuzzy_score, -.name_normalized)
  
  # Summary
  match_summary <- table(result$match_quality)
  cli::cli_h3("Match Summary")
  for (nm in names(match_summary)) {
    cli::cli_alert_info("{nm}: {match_summary[nm]}")
  }
  
  result
}


#' Clear the name lookup cache
#'
#' Removes the persistent user cache file. Use this to force a fresh
#' fetch on the next call to [rb_build_name_lookup()].
#'
#' @return Invisibly returns TRUE if cache was deleted, FALSE if no cache existed
#' @export
#' @examples
#' \dontrun{
#' # Clear cache to force fresh fetch
#' rb_clear_name_cache()
#'
#' # Next call will fetch from APIs
#' lookup <- rb_build_name_lookup()
#' }
rb_clear_name_cache <- function() {
  cache_file <- file.path(rappdirs::user_cache_dir("ribits"), "banks_lookup.rds")

  if (file.exists(cache_file)) {
    unlink(cache_file)
    cli::cli_alert_success("Persistent cache cleared: {cache_file}")
    invisible(TRUE)
  } else {
    cli::cli_alert_info("No cache file found")
    invisible(FALSE)
  }
}
