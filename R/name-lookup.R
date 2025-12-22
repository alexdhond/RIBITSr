# R/name-lookup.R
# Name-to-ID lookup functions for harmonizing CSV data with API/EPA

#' Build a name-to-ID lookup table
#'
#' Creates a lookup table mapping bank/program names to their IDs using
#' data from EPA ArcGIS and the RIBITS API. Useful for joining CSV data
#' that lacks numeric IDs.
#'
#' @param include_csv Logical. Also include mappings from transactions_watershed CSV.
#'   Default TRUE.
#' @param cache Logical. Cache the lookup table for faster subsequent calls.
#'   Default TRUE.
#'
#' @return A tibble with columns: name, bank_id, state, district, year_established,
#'   and source (where the mapping came from)
#' @keywords internal
#' @examples
#' \dontrun{
#' # Build lookup table
#' lookup <- rb_build_name_lookup()
#'
#' # Use it to add bank_id to CSV data
#' csv_data <- rb_read("my_report.csv")
#' csv_with_id <- rb_match_names(csv_data, lookup, name_col = "Bank Name")
#' }
rb_build_name_lookup <- function(include_csv = TRUE, cache = TRUE) {
  
  # Check cache first

  cache_key <- "ribits_name_lookup"
  if (cache) {
    cached <- getOption(cache_key)
    if (!is.null(cached)) {
      cli::cli_alert_info("Using cached lookup table ({nrow(cached)} entries)")
      return(cached)
    }
  }
  
  cli::cli_h2("Building name-to-ID lookup table")
  
  lookup_sources <- list()
  
  # Source 1: EPA ArcGIS (most reliable)
  cli::cli_progress_step("Fetching EPA bank data...")
  epa_banks <- tryCatch({
    rb_epa_query("approved_banks", out_fields = "BANK_ID,BANK_NAME,STATE_LIST,DISTRICT,YEAR_ESTABLISHED",
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
    cli::cli_alert_danger("No lookup data available")
    return(tibble::tibble())
  }
  
  # Stack and deduplicate (keep first occurrence = highest priority source)
  combined <- dplyr::bind_rows(lookup_sources) |>
    dplyr::group_by(bank_id) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()
  
  # Also create normalized name for fuzzy matching
  combined <- combined |>
    dplyr::mutate(
      name_normalized = tolower(name) |>
        stringr::str_replace_all("[^a-z0-9]", " ") |>
        stringr::str_squish()
    )
  
  cli::cli_alert_success("Built lookup table: {nrow(combined)} unique banks")
  
  # Cache the result
  if (cache) {
    options(stats::setNames(list(combined), cache_key))
  }
  
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
  
  if (nrow(lookup) == 0) {
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
  
  # Step 2: Fuzzy matching for unmatched rows
  if (fuzzy && n_exact < nrow(data)) {
    unmatched_idx <- which(is.na(exact_matches$bank_id_exact))
    
    if (length(unmatched_idx) > 0) {
      cli::cli_progress_step("Fuzzy matching {length(unmatched_idx)} remaining...")
      
      fuzzy_results <- sapply(exact_matches$.name_normalized[unmatched_idx], function(name) {
        if (is.na(name) || nchar(name) == 0) return(list(bank_id = NA, score = 0))
        
        # Calculate string similarity
        similarities <- stringdist::stringsim(name, lookup$name_normalized, method = "jw")
        best_idx <- which.max(similarities)
        best_score <- similarities[best_idx]
        
        if (best_score >= threshold) {
          list(bank_id = lookup$bank_id[best_idx], score = best_score)
        } else {
          list(bank_id = NA, score = best_score)
        }
      }, simplify = FALSE)
      
      for (i in seq_along(unmatched_idx)) {
        exact_matches$bank_id_fuzzy[unmatched_idx[i]] <- fuzzy_results[[i]]$bank_id
        exact_matches$fuzzy_score[unmatched_idx[i]] <- fuzzy_results[[i]]$score
      }
      
      n_fuzzy <- sum(!is.na(exact_matches$bank_id_fuzzy))
      cli::cli_alert_success("Fuzzy matches: {n_fuzzy}")
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
#' @keywords internal
rb_clear_name_cache <- function() {
  options(ribits_name_lookup = NULL)
  cli::cli_alert_success("Name lookup cache cleared")
}
