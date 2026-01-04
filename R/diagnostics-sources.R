# R/diagnostics-sources.R
# Source comparison and analysis functions
# Split from R/data-diagnostics.R (848 lines → focused modules)

#' Data Diagnostics for RIBITS Data
#'
#' Functions to analyze data completeness and source quality.
#' @name data-diagnostics
NULL

#' Compare ledger data completeness across sources (Internal)
#'
#' @description
#' This function is internal. Use `rb_diagnose(data, type = "sources")` instead.
#'
#' Compares data completeness across sources (API, CSV, merged) to help
#' understand which source provides more complete data.
#'
#' @param data A ribits result object or ledger data frame
#' @param verbose Print detailed output. Default TRUE.
#' @return A list with completeness metrics (invisibly)
#' @keywords internal
#' @examples
#' \dontrun{
#' # Recommended: Use rb_diagnose() instead
#' wy <- ribits(state = "WY")
#' rb_diagnose(wy, type = "sources")
#'
#' # Internal usage
#' rb_compare_sources(wy)
#' }
rb_compare_sources <- function(data, verbose = TRUE) {
  # Extract ledger if full result object

ledger <- if (is.data.frame(data)) data else data$ledger
  
  if (is.null(ledger) || nrow(ledger) == 0) {
    cli::cli_alert_warning("No ledger data to diagnose")
    return(invisible(NULL))
  }
  
  # Ensure source column exists
if (!("source" %in% names(ledger))) {
    cli::cli_alert_warning("No 'source' column - cannot compare sources")
    return(invisible(NULL))
  }
  
  sources <- unique(ledger$source)
  
  # Key fields to check
  key_fields <- c(
    # Core transaction info
    "transaction_id", "transaction_date", "transaction_type", "credits", "acres",
    # Classification
    "credit_type", "credit_action", "credit_classification", "resource_type",
    # Project info
    "permit", "permittee", "permit_auth_date", "permit_list",
    "parent_project_name", "sub_ledger_project_name", "sub_ledger_id",
    # Flags
    "is_ilf", "is_purchased", "is_transferred", "is_blm_project_program_site",
    # Geographic
    "jurisdiction", "impact_huc", "impact_latitude", "impact_longitude"
  )
  
  # Filter to fields that exist
  key_fields <- intersect(key_fields, names(ledger))
  
  # Calculate completeness by source
  completeness <- list()
  for (src in sources) {
    sub <- ledger[ledger$source == src, , drop = FALSE]
    n <- nrow(sub)
    
    field_stats <- sapply(key_fields, function(col) {
      vals <- sub[[col]]
      # Convert to character for string comparisons (handles Date/POSIXct safely)
      vals_char <- as.character(vals)
      non_empty <- sum(!is.na(vals) & vals_char != "" & vals_char != "NA")
      round(100 * non_empty / n, 1)
    })
    
    completeness[[src]] <- list(
      n_rows = n,
      field_pct = field_stats,
      avg_completeness = round(mean(field_stats), 1)
    )
  }
  
  # Identify fields unique to merged data
  if ("api+csv" %in% sources && "api" %in% sources) {
    merged <- ledger[ledger$source == "api+csv", , drop = FALSE]
    api_only <- ledger[ledger$source == "api", , drop = FALSE]
    
    # Fields where merged has significantly better coverage
    merged_better <- names(which(
      completeness[["api+csv"]]$field_pct - completeness[["api"]]$field_pct > 20
    ))
    
    # Fields where API-only has better coverage (rare but possible)
    api_better <- names(which(
      completeness[["api"]]$field_pct - completeness[["api+csv"]]$field_pct > 20
    ))
  } else {
    merged_better <- character(0)
    api_better <- character(0)
  }
  
  # Output
  if (verbose) {
    cli::cli_h1("Ledger Data Diagnostics")
    
    cli::cli_h2("Source Summary")
    for (src in sources) {
      cli::cli_alert_info("{src}: {completeness[[src]]$n_rows} transactions, {completeness[[src]]$avg_completeness}% avg field completeness")
    }
    
    cli::cli_h2("Field Completeness by Source")
    
    # Create comparison table
    comp_df <- data.frame(field = key_fields)
    for (src in sources) {
      comp_df[[src]] <- paste0(completeness[[src]]$field_pct, "%")
    }
    print(comp_df, row.names = FALSE)
    
    if (length(merged_better) > 0) {
      cli::cli_h2("Fields Enhanced by CSV Merge")
      cli::cli_alert_success("These fields have >20% better coverage in merged (api+csv) data:")
      cli::cli_ul(merged_better)
    }
    
    if (length(api_better) > 0) {
      cli::cli_h2("Fields Better in API-only")
      cli::cli_alert_info("These fields have better coverage in API-only data:")
      cli::cli_ul(api_better)
    }
    
    cli::cli_h2("Recommendation")
    if ("api+csv" %in% sources) {
      merged_pct <- completeness[["api+csv"]]$avg_completeness
      api_pct <- completeness[["api"]]$avg_completeness
      
      if (merged_pct > api_pct) {
        cli::cli_alert_success(
          "Merging API+CSV increases field completeness by {round(merged_pct - api_pct, 1)}%"
        )
        cli::cli_alert_info("CSV provides: {paste(merged_better, collapse = ', ')}")
      } else {
        cli::cli_alert_info("API data is equally or more complete than merged data")
      }
    }
  }
  
  invisible(completeness)
}
#' Internal: Source comparison diagnostics
#' @keywords internal
#' @noRd
.rb_diagnose_sources <- function(data, verbose = TRUE) {
  # Extract ledger/transactions if full result object
  ledger <- if (is.data.frame(data)) {
    data
  } else if (inherits(data, "ribits_data")) {
    data$transactions %||% data$ledger
  } else {
    NULL
  }

  if (is.null(ledger) || nrow(ledger) == 0) {
    cli::cli_alert_warning("No transaction data to diagnose")
    return(invisible(NULL))
  }

  # Ensure source column exists
  if (!("source" %in% names(ledger))) {
    cli::cli_alert_warning("No 'source' column - cannot compare sources")
    return(invisible(NULL))
  }

  sources <- unique(ledger$source)

  # Key fields to check
  key_fields <- c(
    # Core transaction info
    "transaction_id", "transaction_date", "transaction_type", "credits", "acres",
    # Classification
    "credit_type", "credit_action", "credit_classification", "resource_type",
    # Project info
    "permit", "permittee", "permit_auth_date", "permit_list",
    "parent_project_name", "sub_ledger_project_name", "sub_ledger_id",
    # Flags
    "is_ilf", "is_purchased", "is_transferred", "is_blm_project_program_site",
    # Geographic
    "jurisdiction", "impact_huc", "impact_latitude", "impact_longitude"
  )

  # Filter to fields that exist
  key_fields <- intersect(key_fields, names(ledger))

  # Calculate completeness by source
  completeness <- list()
  for (src in sources) {
    sub <- ledger[ledger$source == src, , drop = FALSE]
    n <- nrow(sub)

    field_stats <- sapply(key_fields, function(col) {
      vals <- sub[[col]]
      # Convert to character for string comparisons (handles Date/POSIXct safely)
      vals_char <- as.character(vals)
      non_empty <- sum(!is.na(vals) & vals_char != "" & vals_char != "NA")
      round(100 * non_empty / n, 1)
    })

    completeness[[src]] <- list(
      n_rows = n,
      field_pct = field_stats,
      avg_completeness = round(mean(field_stats), 1)
    )
  }

  # Identify fields unique to merged data
  if ("api+csv" %in% sources && "api" %in% sources) {
    # Fields where merged has significantly better coverage
    merged_better <- names(which(
      completeness[["api+csv"]]$field_pct - completeness[["api"]]$field_pct > 20
    ))

    # Fields where API-only has better coverage (rare but possible)
    api_better <- names(which(
      completeness[["api"]]$field_pct - completeness[["api+csv"]]$field_pct > 20
    ))
  } else {
    merged_better <- character(0)
    api_better <- character(0)
  }

  # Output
  if (verbose) {
    cli::cli_h1("Source Comparison Diagnostics")

    cli::cli_h2("Source Summary")
    for (src in sources) {
      cli::cli_alert_info("{src}: {completeness[[src]]$n_rows} transactions, {completeness[[src]]$avg_completeness}% avg field completeness")
    }

    cli::cli_h2("Field Completeness by Source")

    # Create comparison table
    comp_df <- data.frame(field = key_fields)
    for (src in sources) {
      comp_df[[src]] <- paste0(completeness[[src]]$field_pct, "%")
    }
    print(comp_df, row.names = FALSE)

    if (length(merged_better) > 0) {
      cli::cli_h2("Fields Enhanced by CSV Merge")
      cli::cli_alert_success("These fields have >20% better coverage in merged (api+csv) data:")
      cli::cli_ul(merged_better)
    }

    if (length(api_better) > 0) {
      cli::cli_h2("Fields Better in API-only")
      cli::cli_alert_info("These fields have better coverage in API-only data:")
      cli::cli_ul(api_better)
    }

    cli::cli_h2("Recommendation")
    if ("api+csv" %in% sources) {
      merged_pct <- completeness[["api+csv"]]$avg_completeness
      api_pct <- completeness[["api"]]$avg_completeness

      if (merged_pct > api_pct) {
        cli::cli_alert_success(
          "Merging API+CSV increases field completeness by {round(merged_pct - api_pct, 1)}%"
        )
        if (length(merged_better) > 0) {
          cli::cli_alert_info("CSV provides: {paste(merged_better, collapse = ', ')}")
        }
      } else {
        cli::cli_alert_info("API data is equally or more complete than merged data")
      }
    }
  }

  invisible(completeness)
}


#' Cross-validate credit data between CSV and API sources
#'
#' Compares credit totals from the credit_classification CSV with API data
#' to identify discrepancies. This helps validate data consistency across
#' the different RIBITS data sources.
#'
#' @param bank_ids Vector of bank IDs to validate. If NULL (default),
#'   validates a random sample.
#' @param tolerance Percentage tolerance for considering values equal.
#'   Default 0.01 (1%). Values differing by less than this percentage
#'   are considered matching.
#' @param sample_size If bank_ids is NULL, validate this many random banks.
#'   Default 50.
#' @param verbose Print detailed output. Default TRUE.
#'
#' @return A tibble with validation results (invisibly):
#' \describe{
#'   \item{bank_id}{Bank identifier}
#'   \item{bank_name}{Bank name from CSV}
#'   \item{csv_available}{Available credits from CSV}
#'   \item{csv_released}{Released credits from CSV}
#'   \item{csv_potential}{Potential credits from CSV}
#'   \item{api_available}{Available credits from API (if available)}
#'   \item{api_released}{Released credits from API (if available)}
#'   \item{api_potential}{Potential credits from API (if available)}
#'   \item{pct_diff_*}{Percentage difference for each credit type}
#'   \item{has_discrepancy}{TRUE if any credit type exceeds tolerance}
#' }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Validate 50 random banks
#' results <- rb_validate_credits()
#'
#' # Validate specific banks
#' results <- rb_validate_credits(bank_ids = c(3646, 3647, 3648))
#'
#' # Use stricter tolerance (0.1%)
#' results <- rb_validate_credits(tolerance = 0.001)
#'
#' # Get banks with discrepancies
#' results |> dplyr::filter(has_discrepancy)
#' }
rb_validate_credits <- function(bank_ids = NULL,
                                 tolerance = 0.01,
                                 sample_size = 50,
                                 verbose = TRUE) {

  cli::cli_h1("Credit Data Cross-Validation")

  # Step 1: Get credit_classification CSV data
  cli::cli_progress_step("Fetching credit classification CSV...")
  csv_credits <- tryCatch({
    csv_file <- rb_download_report("credit_classification", download_dir = tempdir())
    data <- readr::read_csv(csv_file, show_col_types = FALSE)
    names(data) <- janitor::make_clean_names(names(data))
    data <- .ensure_bank_id(data, quietly = TRUE)
    data
  }, error = function(e) {
    cli::cli_alert_danger("Failed to fetch CSV: {e$message}")
    return(NULL)
  })

  if (is.null(csv_credits) || nrow(csv_credits) == 0) {
    cli::cli_alert_danger("No CSV data available for validation")
    return(invisible(NULL))
  }

  cli::cli_alert_success("CSV: {nrow(csv_credits)} credit classification records")

  # Step 2: Select banks to validate
  available_ids <- unique(csv_credits$bank_id[!is.na(csv_credits$bank_id)])

  if (is.null(bank_ids)) {
    bank_ids <- sample(available_ids, min(sample_size, length(available_ids)))
    cli::cli_alert_info("Validating {length(bank_ids)} randomly sampled banks")
  } else {
    bank_ids <- intersect(as.integer(bank_ids), available_ids)
    if (length(bank_ids) == 0) {
      cli::cli_alert_warning("None of the specified bank_ids found in CSV data")
      return(invisible(NULL))
    }
    cli::cli_alert_info("Validating {length(bank_ids)} specified banks")
  }

  # Step 3: Aggregate CSV credits by bank
  cli::cli_progress_step("Aggregating CSV credits by bank...")

  # Find credit columns (may have various names)
  credit_cols <- list(
    available = names(csv_credits)[grepl("available.*credit", names(csv_credits), ignore.case = TRUE)],
    released = names(csv_credits)[grepl("released.*credit|credit.*released", names(csv_credits), ignore.case = TRUE)],
    potential = names(csv_credits)[grepl("potential.*credit|credit.*potential", names(csv_credits), ignore.case = TRUE)]
  )

  csv_summary <- csv_credits |>
    dplyr::filter(bank_id %in% bank_ids) |>
    dplyr::group_by(bank_id) |>
    dplyr::summarise(
      bank_name = dplyr::first(stats::na.omit(
        dplyr::pick(dplyr::any_of(c("bank_name", "name")))[[1]]
      )),
      csv_available = if (length(credit_cols$available) > 0) {
        sum(dplyr::pick(dplyr::all_of(credit_cols$available[1]))[[1]], na.rm = TRUE)
      } else NA_real_,
      csv_released = if (length(credit_cols$released) > 0) {
        sum(dplyr::pick(dplyr::all_of(credit_cols$released[1]))[[1]], na.rm = TRUE)
      } else NA_real_,
      csv_potential = if (length(credit_cols$potential) > 0) {
        sum(dplyr::pick(dplyr::all_of(credit_cols$potential[1]))[[1]], na.rm = TRUE)
      } else NA_real_,
      csv_n_classifications = dplyr::n(),
      .groups = "drop"
    )

  cli::cli_alert_success("Aggregated {nrow(csv_summary)} banks from CSV")

  # Step 4: Fetch API credit data for same banks (sample to avoid rate limits)
  cli::cli_progress_step("Fetching API credit data...")

  api_sample <- if (length(bank_ids) > 20) sample(bank_ids, 20) else bank_ids

  api_credits <- purrr::map_dfr(api_sample, function(id) {
    bank <- tryCatch({
      rb_get("banks", id = id)
    }, error = function(e) NULL)

    if (is.null(bank) || nrow(bank) == 0) {
      return(tibble::tibble(
        bank_id = id,
        api_available = NA_real_,
        api_released = NA_real_,
        api_potential = NA_real_
      ))
    }

    # Extract credit totals from API response
    # Column names vary - try common patterns
    tibble::tibble(
      bank_id = id,
      api_available = .extract_credit_value(bank, "available"),
      api_released = .extract_credit_value(bank, "released"),
      api_potential = .extract_credit_value(bank, "potential")
    )
  })

  cli::cli_alert_success("Fetched API data for {nrow(api_credits)} banks")

  # Step 5: Compare and identify discrepancies
  cli::cli_progress_step("Comparing credit values...")

  comparison <- csv_summary |>
    dplyr::left_join(api_credits, by = "bank_id") |>
    dplyr::mutate(
      # Calculate percentage differences (handle zeros)
      pct_diff_available = .calc_pct_diff(csv_available, api_available),
      pct_diff_released = .calc_pct_diff(csv_released, api_released),
      pct_diff_potential = .calc_pct_diff(csv_potential, api_potential),

      # Flag discrepancies exceeding tolerance
      discrepancy_available = !is.na(pct_diff_available) & pct_diff_available > (tolerance * 100),
      discrepancy_released = !is.na(pct_diff_released) & pct_diff_released > (tolerance * 100),
      discrepancy_potential = !is.na(pct_diff_potential) & pct_diff_potential > (tolerance * 100),

      has_discrepancy = discrepancy_available | discrepancy_released | discrepancy_potential
    )

  # Step 6: Report results
  if (verbose) {
    n_validated <- sum(!is.na(comparison$api_available))
    n_discrepancies <- sum(comparison$has_discrepancy, na.rm = TRUE)

    cli::cli_h2("Validation Results")
    cli::cli_alert_info("Banks in CSV: {nrow(csv_summary)}")
    cli::cli_alert_info("Banks validated against API: {n_validated}")

    if (n_discrepancies == 0) {
      cli::cli_alert_success("No discrepancies found (tolerance: {tolerance*100}%)")
    } else {
      cli::cli_alert_warning("{n_discrepancies} bank{?s} have discrepancies exceeding {tolerance*100}% tolerance")

      # Show summary of discrepancy types
      n_avail <- sum(comparison$discrepancy_available, na.rm = TRUE)
      n_rel <- sum(comparison$discrepancy_released, na.rm = TRUE)
      n_pot <- sum(comparison$discrepancy_potential, na.rm = TRUE)

      if (n_avail > 0) cli::cli_alert_info("  Available credits: {n_avail} discrepancies")
      if (n_rel > 0) cli::cli_alert_info("  Released credits: {n_rel} discrepancies")
      if (n_pot > 0) cli::cli_alert_info("  Potential credits: {n_pot} discrepancies")

      # Show worst discrepancies
      worst <- comparison |>
        dplyr::filter(has_discrepancy) |>
        dplyr::arrange(dplyr::desc(pmax(
          pct_diff_available, pct_diff_released, pct_diff_potential, na.rm = TRUE
        ))) |>
        head(5)

      if (nrow(worst) > 0) {
        cli::cli_h3("Top Discrepancies")
        for (i in seq_len(nrow(worst))) {
          row <- worst[i, ]
          cli::cli_alert_warning(
            "Bank {row$bank_id}: CSV={round(row$csv_available, 0)} vs API={round(row$api_available, 0)} ({round(row$pct_diff_available, 1)}% diff)"
          )
        }
      }
    }

    cli::cli_h2("Summary Statistics")
    if (n_validated > 0) {
      avg_diff <- mean(comparison$pct_diff_available, na.rm = TRUE)
      max_diff <- max(comparison$pct_diff_available, na.rm = TRUE)
      cli::cli_alert_info("Average difference (available): {round(avg_diff, 2)}%")
      cli::cli_alert_info("Maximum difference (available): {round(max_diff, 2)}%")
    }
  }

  invisible(comparison)
}


#' Extract credit value from API bank response
#' @keywords internal
#' @noRd
.extract_credit_value <- function(bank, credit_type) {
  # Try common column name patterns
  patterns <- switch(credit_type,
    available = c("total_available_credits", "available_credits", "credits_available"),
    released = c("total_released_credits", "released_credits", "credits_released"),
    potential = c("total_potential_credits", "potential_credits", "credits_potential")
  )

  for (pattern in patterns) {
    col <- names(bank)[grepl(pattern, names(bank), ignore.case = TRUE)]
    if (length(col) > 0 && !is.na(bank[[col[1]]])) {
      return(as.numeric(bank[[col[1]]]))
    }
  }

  NA_real_
}


#' Calculate percentage difference between two values
#' @keywords internal
#' @noRd
.calc_pct_diff <- function(val1, val2) {
  dplyr::case_when(
    is.na(val1) | is.na(val2) ~ NA_real_,
    val1 == 0 & val2 == 0 ~ 0,
    val1 == 0 | val2 == 0 ~ 100,
    TRUE ~ abs(val1 - val2) / pmax(abs(val1), abs(val2)) * 100
  )
}


# =============================================================================
# STATUS COVERAGE DIAGNOSTICS
# =============================================================================

#' Analyze data coverage by bank status
#'
#' Provides a comprehensive report of data availability across all bank statuses
#' (Approved, Pending, Sold-Out, Suspended, Terminated, Withdrawn). This helps
#' users understand what data to expect for different types of banks.
#'
#' @param state Optional state filter (e.g., "CA", "OR"). If NULL, analyzes
#'   all available data.
#' @param sources Which sources to analyze. Default c("api", "epa", "csv").
#' @param verbose Print detailed output. Default TRUE.
#'
#' @return A list with coverage statistics (invisibly):
#' \describe{
#'   \item{summary}{Tibble with counts and percentages by status}
#'   \item{field_coverage}{Tibble showing field population rates by status}
#'   \item{spatial_coverage}{Tibble showing spatial data availability by status}
#'   \item{transaction_coverage}{Tibble showing transaction data by status}
#'   \item{recommendations}{Character vector of recommendations}
#' }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Full coverage report for California
#' coverage <- rb_status_coverage(state = "CA")
#'
#' # View just the summary
#' coverage$summary
#'
#' # National overview (may take longer)
#' coverage <- rb_status_coverage()
#' }
rb_status_coverage <- function(state = NULL,
                                sources = c("api", "epa", "csv"),
                                verbose = TRUE) {

  if (verbose) {
    cli::cli_h1("Bank Status Coverage Analysis")
    if (!is.null(state)) cli::cli_alert_info("State filter: {state}")
  }

  results <- list()

  # ==========================================================================
  # 1. CSV STATUS DISTRIBUTION (Most complete source for status counts)
  # ==========================================================================
  if (verbose) cli::cli_h2("Step 1: CSV Status Distribution")

  csv_status <- tryCatch({
    csv_file <- rb_download_report("banks_sites", download_dir = tempdir())
    lines <- readLines(csv_file, n = 3)
    skip_rows <- if (nchar(trimws(lines[1])) < 5) 1 else 0

    banks <- readr::read_csv(csv_file, skip = skip_rows, show_col_types = FALSE)
    names(banks) <- janitor::make_clean_names(names(banks))

    # Filter by state if specified
    if (!is.null(state)) {
      state_col <- names(banks)[grepl("state", names(banks), ignore.case = TRUE)][1]
      if (!is.null(state_col) && !is.na(state_col)) {
        banks <- banks[grepl(state, banks[[state_col]], ignore.case = TRUE), ]
      }
    }

    # Get status column
    status_col <- names(banks)[grepl("status", names(banks), ignore.case = TRUE)][1]

    if (!is.null(status_col) && !is.na(status_col)) {
      status_counts <- as.data.frame(table(banks[[status_col]], useNA = "ifany"))
      names(status_counts) <- c("status", "csv_count")
      status_counts$csv_pct <- round(100 * status_counts$csv_count / sum(status_counts$csv_count), 1)
      status_counts
    } else {
      NULL
    }
  }, error = function(e) {
    if (verbose) cli::cli_alert_warning("CSV fetch failed: {e$message}")
    NULL
  })

  if (!is.null(csv_status) && verbose) {
    cli::cli_alert_success("CSV bank status distribution:")
    for (i in seq_len(nrow(csv_status))) {
      row <- csv_status[i, ]
      cli::cli_alert_info("  {row$status}: {row$csv_count} ({row$csv_pct}%)")
    }
  }

  # ==========================================================================
  # 2. EPA STATUS DISTRIBUTION (Centroid layers)
  # ==========================================================================
  if ("epa" %in% sources) {
    if (verbose) cli::cli_h2("Step 2: EPA Centroid Layer Coverage")

    epa_status <- tryCatch({
      where <- if (!is.null(state)) paste0("STATE_LIST LIKE '%", state, "%'") else "1=1"

      # Query all three centroid layers
      approved <- rb_epa_query("approved_banks", where = where,
                                out_fields = "BANK_ID,BANK_STATUS", return_geometry = FALSE)
      pending <- rb_epa_query("pending_banks", where = where,
                               out_fields = "BANK_ID,BANK_STATUS", return_geometry = FALSE)
      terminated <- rb_epa_query("terminated_banks", where = where,
                                  out_fields = "BANK_ID,BANK_STATUS", return_geometry = FALSE)

      # Combine and count
      all_epa <- dplyr::bind_rows(
        if (!is.null(approved) && nrow(approved) > 0) {
          names(approved) <- tolower(names(approved))
          approved
        },
        if (!is.null(pending) && nrow(pending) > 0) {
          names(pending) <- tolower(names(pending))
          pending
        },
        if (!is.null(terminated) && nrow(terminated) > 0) {
          names(terminated) <- tolower(names(terminated))
          terminated
        }
      )

      if (nrow(all_epa) > 0 && "bank_status" %in% names(all_epa)) {
        status_counts <- as.data.frame(table(all_epa$bank_status, useNA = "ifany"))
        names(status_counts) <- c("status", "epa_count")
        status_counts$epa_pct <- round(100 * status_counts$epa_count / sum(status_counts$epa_count), 1)
        status_counts
      } else {
        NULL
      }
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning("EPA query failed: {e$message}")
      NULL
    })

    if (!is.null(epa_status) && verbose) {
      cli::cli_alert_success("EPA centroid coverage by status:")
      for (i in seq_len(nrow(epa_status))) {
        row <- epa_status[i, ]
        cli::cli_alert_info("  {row$status}: {row$epa_count} ({row$epa_pct}%)")
      }
    }
  } else {
    epa_status <- NULL
  }

  # ==========================================================================
  # 3. SPATIAL COVERAGE BY STATUS
  # ==========================================================================
  if ("epa" %in% sources) {
    if (verbose) cli::cli_h2("Step 3: Spatial Data Coverage by Status")

    spatial_coverage <- tryCatch({
      # Use the enhanced rb_spatial_availability function
      spatial <- rb_spatial_availability(state = state, quietly = TRUE)

      if (!is.null(spatial) && nrow(spatial) > 0 && "bank_status" %in% names(spatial)) {
        spatial |>
          dplyr::group_by(bank_status) |>
          dplyr::summarise(
            n_banks = dplyr::n(),
            n_centroids = sum(has_centroid, na.rm = TRUE),
            n_footprints = sum(has_footprint, na.rm = TRUE),
            n_service_areas = sum(has_service_area, na.rm = TRUE),
            pct_footprints = round(100 * sum(has_footprint, na.rm = TRUE) / dplyr::n(), 1),
            pct_service_areas = round(100 * sum(has_service_area, na.rm = TRUE) / dplyr::n(), 1),
            .groups = "drop"
          )
      } else {
        NULL
      }
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning("Spatial coverage check failed: {e$message}")
      NULL
    })

    if (!is.null(spatial_coverage) && verbose) {
      cli::cli_alert_success("Spatial coverage by status:")
      for (i in seq_len(nrow(spatial_coverage))) {
        row <- spatial_coverage[i, ]
        cli::cli_alert_info(
          "  {row$bank_status}: {row$n_banks} banks, {row$pct_footprints}% footprints, {row$pct_service_areas}% service areas"
        )
      }
    }
  } else {
    spatial_coverage <- NULL
  }

  # ==========================================================================
  # 4. TRANSACTION DATA BY STATUS
  # ==========================================================================
  if ("csv" %in% sources) {
    if (verbose) cli::cli_h2("Step 4: Transaction Data by Status")

    transaction_coverage <- tryCatch({
      csv_file <- rb_download_report("transactions_watershed", download_dir = tempdir())
      lines <- readLines(csv_file, n = 3)
      skip_rows <- if (nchar(trimws(lines[1])) < 5) 1 else 0

      txns <- readr::read_csv(csv_file, skip = skip_rows, show_col_types = FALSE)
      names(txns) <- janitor::make_clean_names(names(txns))

      # Find status column
      status_col <- names(txns)[grepl("status", names(txns), ignore.case = TRUE)][1]

      if (!is.null(status_col) && !is.na(status_col)) {
        txns |>
          dplyr::group_by(status = .data[[status_col]]) |>
          dplyr::summarise(
            n_transactions = dplyr::n(),
            pct_of_total = round(100 * dplyr::n() / nrow(txns), 1),
            .groups = "drop"
          )
      } else {
        NULL
      }
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning("Transaction data fetch failed: {e$message}")
      NULL
    })

    if (!is.null(transaction_coverage) && verbose) {
      cli::cli_alert_success("Transactions by bank status:")
      for (i in seq_len(nrow(transaction_coverage))) {
        row <- transaction_coverage[i, ]
        cli::cli_alert_info("  {row$status}: {row$n_transactions} transactions ({row$pct_of_total}%)")
      }
    }
  } else {
    transaction_coverage <- NULL
  }

  # ==========================================================================
  # 5. COMBINE AND GENERATE SUMMARY
  # ==========================================================================
  if (verbose) cli::cli_h2("Summary")

  # Combine status counts from CSV and EPA
  summary_df <- if (!is.null(csv_status)) {
    csv_status |>
      dplyr::rename(csv_banks = csv_count)
  } else {
    tibble::tibble(status = character())
  }

  if (!is.null(epa_status)) {
    summary_df <- summary_df |>
      dplyr::full_join(
        epa_status |> dplyr::rename(epa_centroids = epa_count),
        by = "status"
      )
  }

  if (!is.null(spatial_coverage)) {
    summary_df <- summary_df |>
      dplyr::left_join(
        spatial_coverage |> dplyr::select(status = bank_status, pct_footprints, pct_service_areas),
        by = "status"
      )
  }

  if (!is.null(transaction_coverage)) {
    summary_df <- summary_df |>
      dplyr::left_join(
        transaction_coverage |> dplyr::select(status, n_transactions),
        by = "status"
      )
  }

  # Add expected field info from constants
  summary_df <- summary_df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      expected_ledger = if (status %in% names(BANK_STATUS_FIELD_EXPECTATIONS)) {
        BANK_STATUS_FIELD_EXPECTATIONS[[status]]$has_ledger
      } else {
        NA
      },
      expected_transactions = if (status %in% names(BANK_STATUS_FIELD_EXPECTATIONS)) {
        BANK_STATUS_FIELD_EXPECTATIONS[[status]]$has_transactions
      } else {
        NA
      },
      description = if (status %in% names(BANK_STATUS_FIELD_EXPECTATIONS)) {
        BANK_STATUS_FIELD_EXPECTATIONS[[status]]$description
      } else {
        NA_character_
      }
    ) |>
    dplyr::ungroup()

  # Generate recommendations
  recommendations <- c()

  if (!is.null(csv_status) && !is.null(epa_status)) {
    csv_total <- sum(csv_status$csv_count, na.rm = TRUE)
    epa_total <- sum(epa_status$epa_count, na.rm = TRUE)
    gap <- csv_total - epa_total

    if (gap > 0) {
      recommendations <- c(recommendations,
        paste0("EPA is missing ", gap, " banks that exist in CSV. ",
               "Use API coordinates as fallback for missing EPA centroids.")
      )
    }
  }

  # Check for Pending banks recommendations
  if (!is.null(summary_df) && "Pending" %in% summary_df$status) {
    pending_row <- summary_df[summary_df$status == "Pending", ]
    if (!is.null(pending_row$pct_footprints) && !is.na(pending_row$pct_footprints)) {
      if (pending_row$pct_footprints < 30) {
        recommendations <- c(recommendations,
          "Pending banks have limited spatial data (expected - they're not yet approved). Consider using API centroids."
        )
      }
    }
  }

  # Check for Sold-Out banks
  if (!is.null(transaction_coverage) && "Sold-Out" %in% transaction_coverage$status) {
    soldout_txns <- transaction_coverage[transaction_coverage$status == "Sold-Out", "n_transactions"]
    if (!is.null(soldout_txns) && soldout_txns > 1000) {
      recommendations <- c(recommendations,
        paste0("Sold-Out banks have ", soldout_txns, " historical transactions. ",
               "Include them for comprehensive market analysis.")
      )
    }
  }

  if (length(recommendations) == 0) {
    recommendations <- "Data coverage looks good across all statuses."
  }

  if (verbose) {
    cli::cli_h3("Recommendations")
    for (rec in recommendations) {
      cli::cli_alert_info(rec)
    }
  }

  # Compile results
  results <- list(
    summary = summary_df,
    csv_status = csv_status,
    epa_status = epa_status,
    spatial_coverage = spatial_coverage,
    transaction_coverage = transaction_coverage,
    recommendations = recommendations
  )

  if (verbose) {
    cli::cli_alert_success("Coverage analysis complete. Access results with $summary, $spatial_coverage, etc.")
  }

  invisible(results)
}
