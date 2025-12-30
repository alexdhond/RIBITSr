# R/name-lookup.R
# Name-to-ID lookup functions for harmonizing CSV data with API/EPA

# Source priority weights for confidence scoring (higher = more reliable)
LOOKUP_SOURCE_WEIGHTS <- c(
  epa = 1.0,
  api = 0.9,
  csv_watershed = 0.8,
  csv_banks_sites = 0.7,
  csv_credit_class = 0.6,
  csv_ledger = 0.5,
  csv_notices = 0.4
)

#' Build a name-to-ID lookup table
#'
#' Creates a lookup table mapping bank/program names to their IDs using
#' data from EPA ArcGIS, RIBITS API, and multiple CSV sources. This function
#' implements a smart caching strategy to minimize API calls:
#'
#' 1. Checks persistent user cache first (default: 30 day refresh)
#' 2. If cache is stale/missing, fetches fresh data from APIs
#' 3. Falls back to bundled package data if APIs are unavailable
#'
#' @param include_csv Logical. Include mappings from CSV sources when fetching
#'   fresh data. Default TRUE.
#' @param comprehensive Logical. When TRUE (default), fetches from all available
#'   CSV sources (banks_sites, credit_classification, ledger_transactions,
#'   public_notices) in addition to EPA, API, and transactions_watershed.
#'   When FALSE, only uses EPA, API, and transactions_watershed.
#' @param track_aliases Logical. When TRUE (default), tracks all observed name
#'   variants for each bank_id in a list-column. Useful for understanding
#'   naming inconsistencies across sources.
#' @param max_age_days Integer. Maximum age in days for cached data before
#'   auto-refresh. Default 30. Set to 0 to always fetch fresh data.
#' @param force_refresh Logical. Force refresh from APIs even if cache is fresh.
#'   Default FALSE.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{bank_id}{Numeric bank identifier (primary key)}
#'   \item{canonical_name}{Most authoritative name (from highest priority source)}
#'   \item{name_variants}{List of all observed name spellings (if track_aliases=TRUE)}
#'   \item{state}{State(s) where bank operates}
#'   \item{district}{USACE district}
#'   \item{year_established}{Year bank was established}
#'   \item{bank_status}{Bank status (Approved, Pending, etc.)}
#'   \item{bank_type}{Bank type (Private Commercial, etc.)}
#'   \item{sources}{List of sources that confirmed this bank}
#'   \item{source_count}{Number of sources confirming this bank}
#'   \item{confidence_score}{0-1 score based on source agreement (higher = more reliable)}
#'   \item{name_normalized}{Normalized name for fuzzy matching}
#' }
#'
#' @details
#' The persistent cache is stored in the user's cache directory
#' (see [rappdirs::user_cache_dir()]). This means:
#' - Cache persists across R sessions
#' - First run fetches from APIs (~10-60 seconds depending on sources)
#' - Subsequent runs are near-instant (reads from disk)
#' - Auto-refreshes every 30 days to capture new banks
#' - Works offline using bundled fallback data
#'
#' ## Data Sources (in priority order)
#' 1. **EPA ArcGIS** - Most reliable, but only Approved banks
#' 2. **RIBITS API** - Real-time, all banks
#' 3. **CSV transactions_watershed** - 100% bank_id coverage
#' 4. **CSV banks_sites** - All statuses, adds bank_status/type
#' 5. **CSV credit_classification** - Additional name variants
#' 6. **CSV ledger_transactions** - Additional name variants
#' 7. **CSV public_notices** - Additional name variants
#'
#' @seealso [rb_match_names()] to use the lookup table for name matching
#' @keywords internal
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
#' # Basic lookup without comprehensive sources
#' lookup <- rb_build_name_lookup(comprehensive = FALSE)
#'
#' # Check confidence scores
#' lookup |> dplyr::filter(confidence_score < 0.5)
#'
#' # Check when bundled data was generated
#' data(banks_lookup)
#' attr(banks_lookup, "generated_date")
#' }
rb_build_name_lookup <- function(include_csv = TRUE,
                                  comprehensive = TRUE,
                                  track_aliases = TRUE,
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
    .fetch_fresh_bank_lookup(
      include_csv = include_csv,
      comprehensive = comprehensive,
      track_aliases = track_aliases
    )
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
.fetch_fresh_bank_lookup <- function(include_csv = TRUE,
                                      comprehensive = TRUE,
                                      track_aliases = TRUE) {

  all_records <- list()

  # Source 1: EPA ArcGIS (most reliable, Approved banks only)
  cli::cli_progress_step("Fetching EPA bank data...")
  epa_banks <- tryCatch({
    rb_epa_query("approved_banks",
                 out_fields = "BANK_ID,BANK_NAME,STATE_LIST,DISTRICT,YEAR_ESTABLISHED",
                 return_geometry = FALSE)
  }, error = function(e) NULL)

  if (!is.null(epa_banks) && nrow(epa_banks) > 0) {
    names(epa_banks) <- tolower(names(epa_banks))
    all_records$epa <- tibble::tibble(
      name = epa_banks$bank_name,
      bank_id = as.integer(epa_banks$bank_id),
      state = epa_banks$state_list,
      district = epa_banks$district,
      year_established = as.integer(epa_banks$year_established),
      bank_status = NA_character_,
      bank_type = NA_character_,
      source = "epa"
    )
    cli::cli_alert_success("EPA: {nrow(all_records$epa)} banks")
  }

  # Source 2: RIBITS API list
  cli::cli_progress_step("Fetching API bank list...")
  api_banks <- tryCatch({
    rb_list_generic("banks")
  }, error = function(e) NULL)

  if (!is.null(api_banks) && nrow(api_banks) > 0) {
    names(api_banks) <- tolower(names(api_banks))
    all_records$api <- tibble::tibble(
      name = api_banks$name,
      bank_id = as.integer(api_banks$bank_id),
      state = NA_character_,
      district = NA_character_,
      year_established = NA_integer_,
      bank_status = NA_character_,
      bank_type = NA_character_,
      source = "api"
    )
    cli::cli_alert_success("API: {nrow(all_records$api)} banks")
  }

  # Source 3: Transactions by Watershed CSV (100% bank_id coverage)
  if (include_csv) {
    cli::cli_progress_step("Fetching CSV transactions_watershed...")
    csv_watershed <- tryCatch({
      csv_file <- rb_download_report("transactions_watershed", download_dir = tempdir())
      csv_data <- readr::read_csv(csv_file, show_col_types = FALSE)
      csv_data |>
        dplyr::filter(!is.na(`Bank ID`)) |>
        dplyr::select(name = Name, bank_id = `Bank ID`, state = `State List`) |>
        dplyr::distinct() |>
        dplyr::mutate(
          bank_id = as.integer(bank_id),
          district = NA_character_,
          year_established = NA_integer_,
          bank_status = NA_character_,
          bank_type = NA_character_,
          source = "csv_watershed"
        )
    }, error = function(e) NULL)

    if (!is.null(csv_watershed) && nrow(csv_watershed) > 0) {
      all_records$csv_watershed <- csv_watershed
      cli::cli_alert_success("CSV watershed: {nrow(csv_watershed)} name-ID pairs")
    }
  }

  # Sources 4-7: Additional CSV sources (comprehensive mode)
  if (include_csv && comprehensive) {

    # Source 4: Banks & Sites CSV (has bank_status and bank_type!)
    cli::cli_progress_step("Fetching CSV banks_sites...")
    csv_banks <- tryCatch({
      csv_file <- rb_download_report("banks_sites", download_dir = tempdir())
      csv_data <- readr::read_csv(csv_file, show_col_types = FALSE)
      csv_data |>
        dplyr::select(
          name = Name,
          state = `State Abbrev List`,
          bank_status = `Bank Status`,
          bank_type = `Bank Type`
        ) |>
        dplyr::distinct() |>
        dplyr::mutate(
          bank_id = NA_integer_,  # banks_sites doesn't have bank_id
          district = NA_character_,
          year_established = NA_integer_,
          source = "csv_banks_sites"
        )
    }, error = function(e) NULL)

    if (!is.null(csv_banks) && nrow(csv_banks) > 0) {
      all_records$csv_banks_sites <- csv_banks
      cli::cli_alert_success("CSV banks_sites: {nrow(csv_banks)} banks (with status/type)")
    }

    # Source 5: Credit Classification CSV
    cli::cli_progress_step("Fetching CSV credit_classification...")
    csv_credit <- tryCatch({
      csv_file <- rb_download_report("credit_classification", download_dir = tempdir())
      csv_data <- readr::read_csv(csv_file, show_col_types = FALSE)
      # Find the bank name column (may vary)
      name_col <- names(csv_data)[grepl("bank.*name|name", names(csv_data), ignore.case = TRUE)][1]
      if (!is.null(name_col) && !is.na(name_col)) {
        csv_data |>
          dplyr::select(name = dplyr::all_of(name_col)) |>
          dplyr::distinct() |>
          dplyr::mutate(
            bank_id = NA_integer_,
            state = NA_character_,
            district = NA_character_,
            year_established = NA_integer_,
            bank_status = NA_character_,
            bank_type = NA_character_,
            source = "csv_credit_class"
          )
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (!is.null(csv_credit) && nrow(csv_credit) > 0) {
      all_records$csv_credit_class <- csv_credit
      cli::cli_alert_success("CSV credit_classification: {nrow(csv_credit)} unique names")
    }

    # Source 6: Ledger Transactions CSV
    cli::cli_progress_step("Fetching CSV ledger_transactions...")
    csv_ledger <- tryCatch({
      csv_file <- rb_download_report("ledger_transactions", download_dir = tempdir())
      csv_data <- readr::read_csv(csv_file, show_col_types = FALSE)
      name_col <- names(csv_data)[grepl("^name$|bank.*name", names(csv_data), ignore.case = TRUE)][1]
      if (!is.null(name_col) && !is.na(name_col)) {
        csv_data |>
          dplyr::select(name = dplyr::all_of(name_col)) |>
          dplyr::distinct() |>
          dplyr::mutate(
            bank_id = NA_integer_,
            state = NA_character_,
            district = NA_character_,
            year_established = NA_integer_,
            bank_status = NA_character_,
            bank_type = NA_character_,
            source = "csv_ledger"
          )
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (!is.null(csv_ledger) && nrow(csv_ledger) > 0) {
      all_records$csv_ledger <- csv_ledger
      cli::cli_alert_success("CSV ledger: {nrow(csv_ledger)} unique names")
    }

    # Source 7: Public Notices CSV
    cli::cli_progress_step("Fetching CSV public_notices...")
    csv_notices <- tryCatch({
      csv_file <- rb_download_report("public_notices", download_dir = tempdir())
      csv_data <- readr::read_csv(csv_file, show_col_types = FALSE)
      name_col <- names(csv_data)[grepl("bank.*name|name", names(csv_data), ignore.case = TRUE)][1]
      if (!is.null(name_col) && !is.na(name_col)) {
        csv_data |>
          dplyr::select(name = dplyr::all_of(name_col)) |>
          dplyr::distinct() |>
          dplyr::mutate(
            bank_id = NA_integer_,
            state = NA_character_,
            district = NA_character_,
            year_established = NA_integer_,
            bank_status = NA_character_,
            bank_type = NA_character_,
            source = "csv_notices"
          )
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (!is.null(csv_notices) && nrow(csv_notices) > 0) {
      all_records$csv_notices <- csv_notices
      cli::cli_alert_success("CSV notices: {nrow(csv_notices)} unique names")
    }
  }

  # Combine all sources
  if (length(all_records) == 0) {
    stop("No lookup data available from any source")
  }

  cli::cli_progress_step("Building comprehensive lookup table...")

  # Stack all records
  stacked <- dplyr::bind_rows(all_records)

  # Normalize names for matching
  stacked <- stacked |>
    dplyr::mutate(
      name_normalized = tolower(name) |>
        stringr::str_replace_all("[^a-z0-9]", " ") |>
        stringr::str_squish()
    )

  # Build the enhanced lookup table
  combined <- .build_enhanced_lookup(stacked, track_aliases = track_aliases)

  # Add metadata
  attr(combined, "fetch_date") <- Sys.Date()
  attr(combined, "n_banks") <- nrow(combined)
  attr(combined, "sources_used") <- names(all_records)
  attr(combined, "comprehensive") <- comprehensive
  attr(combined, "track_aliases") <- track_aliases

  cli::cli_alert_success("Built lookup table: {nrow(combined)} unique banks from {length(all_records)} sources")

  combined
}


#' Build enhanced lookup table with aliases and confidence
#' @keywords internal
#' @noRd
.build_enhanced_lookup <- function(stacked, track_aliases = TRUE) {

  # First, propagate bank_id from sources that have it to those that don't
 # This uses name matching to fill in bank_id for csv_banks_sites etc.

  # Get records with bank_id
  with_id <- stacked |>
    dplyr::filter(!is.na(bank_id)) |>
    dplyr::select(name_normalized, bank_id) |>
    dplyr::distinct()

  # Join back to fill missing bank_ids
  stacked <- stacked |>
    dplyr::left_join(
      with_id |> dplyr::rename(bank_id_from_match = bank_id),
      by = "name_normalized"
    ) |>
    dplyr::mutate(
      bank_id = dplyr::coalesce(bank_id, bank_id_from_match)
    ) |>
    dplyr::select(-bank_id_from_match)

  # Now group by bank_id to aggregate
  # For records still without bank_id, group by name_normalized
  with_bank_id <- stacked |> dplyr::filter(!is.na(bank_id))
  without_bank_id <- stacked |> dplyr::filter(is.na(bank_id))

  # Process records WITH bank_id
  if (nrow(with_bank_id) > 0) {
    grouped_with_id <- with_bank_id |>
      dplyr::group_by(bank_id) |>
      dplyr::summarise(
        # Take canonical name from highest priority source
        canonical_name = name[which.min(match(source, names(LOOKUP_SOURCE_WEIGHTS)))],
        # Collect all name variants
        name_variants = if (track_aliases) list(unique(name)) else list(character(0)),
        # Coalesce metadata from all sources (first non-NA)
        state = dplyr::first(stats::na.omit(state)),
        district = dplyr::first(stats::na.omit(district)),
        year_established = dplyr::first(stats::na.omit(year_established)),
        bank_status = dplyr::first(stats::na.omit(bank_status)),
        bank_type = dplyr::first(stats::na.omit(bank_type)),
        # Track which sources confirmed this bank
        sources = list(unique(source)),
        source_count = dplyr::n_distinct(source),
        # Get normalized name from canonical
        name_normalized = name_normalized[which.min(match(source, names(LOOKUP_SOURCE_WEIGHTS)))],
        .groups = "drop"
      ) |>
      dplyr::mutate(
        # Calculate confidence score
        confidence_score = purrr::map_dbl(sources, .calculate_confidence)
      )
  } else {
    grouped_with_id <- tibble::tibble()
  }

  # Process records WITHOUT bank_id (will need fuzzy matching later)
  if (nrow(without_bank_id) > 0) {
    grouped_without_id <- without_bank_id |>
      dplyr::group_by(name_normalized) |>
      dplyr::summarise(
        bank_id = NA_integer_,
        canonical_name = name[1],
        name_variants = if (track_aliases) list(unique(name)) else list(character(0)),
        state = dplyr::first(stats::na.omit(state)),
        district = dplyr::first(stats::na.omit(district)),
        year_established = dplyr::first(stats::na.omit(year_established)),
        bank_status = dplyr::first(stats::na.omit(bank_status)),
        bank_type = dplyr::first(stats::na.omit(bank_type)),
        sources = list(unique(source)),
        source_count = dplyr::n_distinct(source),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        confidence_score = purrr::map_dbl(sources, .calculate_confidence)
      )
  } else {
    grouped_without_id <- tibble::tibble()
  }

  # Combine both
  combined <- dplyr::bind_rows(grouped_with_id, grouped_without_id)

  # Reorder columns
  combined <- combined |>
    dplyr::select(
      bank_id, canonical_name, name_variants, state, district, year_established,
      bank_status, bank_type, sources, source_count, confidence_score, name_normalized
    )

  combined
}


#' Calculate confidence score based on source agreement
#' @keywords internal
#' @noRd
.calculate_confidence <- function(sources) {
  if (length(sources) == 0) return(0)

  # Get weights for sources that confirmed this bank
  weights <- LOOKUP_SOURCE_WEIGHTS[sources]
  weights <- weights[!is.na(weights)]

  if (length(weights) == 0) return(0)

  # Score is based on:
  # 1. Presence of high-weight sources (EPA, API)
  # 2. Number of confirming sources
  weighted_sum <- sum(weights)
  max_possible <- sum(LOOKUP_SOURCE_WEIGHTS)

  # Base score from weights
  base_score <- weighted_sum / max_possible

  # Bonus for multiple sources agreeing (up to 20% bonus)
  agreement_bonus <- min(0.2, (length(weights) - 1) * 0.05)

  # Final score capped at 1.0
  min(1.0, base_score + agreement_bonus)
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
#' @keywords internal
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
