#' Check data coverage or quality
#'
#' Unified function for checking data availability (before download) or
#' data quality (after download).
#'
#' @param data Either NULL (for coverage check) or a ribits_data object (for quality check)
#' @param state State filter for coverage check
#' @param district District filter for coverage check
#' @param quietly Suppress progress messages. Default FALSE.
#'
#' @return Coverage stats or quality report (invisibly)
#' @export
#' @examples
#' \dontrun{
#' # Check coverage before downloading
#' rb_check(state = "WA")
#'
#' # Check quality after downloading
#' data <- ribits(state = "WA")
#' rb_check(data)
#' }
rb_check <- function(data = NULL, state = NULL, district = NULL, quietly = FALSE) {
  
 if (is.null(data)) {
    # Coverage check (pre-download)
    return(rb_coverage(state = state, district = district, quietly = quietly))
  } else if (inherits(data, "ribits_data")) {
    # Quality check (post-download)
    return(rb_quality_report(data, verbose = !quietly))
  } else {
    cli::cli_abort("data must be NULL (for coverage) or a ribits_data object (for quality)")
  }
}


#' @rdname rb_check
#' @keywords internal
rb_coverage <- function(state = NULL, district = NULL, quietly = FALSE) {
  
  if (is.null(state) && is.null(district)) {
    cli::cli_abort("Please specify at least one of: state, district")
  }
  
  filter_desc <- paste(c(
    if (!is.null(state)) paste0("state=", state),
    if (!is.null(district)) paste0("district=", district)
  ), collapse = ", ")
  
  if (!quietly) cli::cli_h2("Data Coverage: {filter_desc}")
  
  # Get API bank count
  if (!quietly) cli::cli_progress_step("Checking API...")
  api_banks <- tryCatch({
    rb_get("banks", state = state, district = district)
  }, error = function(e) NULL)
  n_api <- if (!is.null(api_banks)) nrow(api_banks) else 0
  
  # Get EPA bank count
  if (!quietly) cli::cli_progress_step("Checking EPA...")
  epa_banks <- tryCatch({
    rb_epa("banks", state = state, district = district) |> sf::st_drop_geometry()
  }, error = function(e) NULL)
  n_epa <- if (!is.null(epa_banks)) nrow(epa_banks) else 0
  
  # Get footprint count
  if (!quietly) cli::cli_progress_step("Checking footprints...")
  footprints <- tryCatch({
    rb_epa("footprints", state = state, district = district)
  }, error = function(e) NULL)
  n_fp <- if (!is.null(footprints)) nrow(footprints) else 0
  
  # Get service area count
  if (!quietly) cli::cli_progress_step("Checking service areas...")
  service_areas <- tryCatch({
    rb_epa("service_areas", state = state, district = district)
  }, error = function(e) NULL)
  n_sa <- if (!is.null(service_areas)) nrow(service_areas) else 0
  
  # Build result
  n_total <- max(n_api, n_epa)
  
  result <- tibble::tibble(
    metric = c("Total Banks (API)", "Total Banks (EPA)", "With Footprint", "With Service Area"),
    count = c(n_api, n_epa, n_fp, n_sa),
    percent = c(
      100,
      if (n_api > 0) round(100 * n_epa / n_api, 1) else NA,
      if (n_total > 0) round(100 * n_fp / n_total, 1) else NA,
      if (n_total > 0) round(100 * n_sa / n_total, 1) else NA
    )
  )
  
  if (!quietly) {
    cli::cli_h3("Coverage Summary")
    cli::cli_bullets(c(
      "*" = "Banks in API: {.val {n_api}}",
      "*" = "Banks in EPA: {.val {n_epa}} ({result$percent[2]}% of API)",
      "*" = "With footprints: {.val {n_fp}} ({result$percent[3]}%)",
      "*" = "With service areas: {.val {n_sa}} ({result$percent[4]}%)"
    ))
    
    if (n_api > n_epa) {
      cli::cli_alert_info("{n_api - n_epa} banks in API not in EPA (likely pending/newer)")
    }
  }
  
  invisible(result)
}


#' @rdname rb_check
#' @keywords internal
rb_quality_report <- function(data, verbose = TRUE) {
  
  if (!inherits(data, "ribits_data")) {
    cli::cli_abort("Expected a ribits_data object from ribits() or similar")
  }
  
  report <- list()
  
  if (verbose) cli::cli_h1("Data Quality Report")
  
  # === Banks ===
  if (!is.null(data$banks) && nrow(data$banks) > 0) {
    banks <- data$banks
    n_banks <- nrow(banks)
    
    if (verbose) cli::cli_h2("Banks ({n_banks} rows)")
    
    # Key field completeness
    key_fields <- c("bank_id", "bank_name", "district", "state_list", 
                    "total_acres", "bank_status", "year_established", 
                    "permit_number", "bank_type")
    
    completeness <- sapply(key_fields, function(f) {
      if (f %in% names(banks)) {
        round(100 * sum(!is.na(banks[[f]])) / n_banks, 1)
      } else {
        NA
      }
    })
    
    report$banks_completeness <- tibble::tibble(
      field = key_fields,
      percent_complete = completeness
    )
    
    if (verbose) {
      cli::cli_h3("Field Completeness")
      for (i in seq_along(key_fields)) {
        pct <- completeness[i]
        status <- if (is.na(pct)) "!" else if (pct >= 90) "v" else if (pct >= 50) "*" else "x"
        pct_str <- if (is.na(pct)) "MISSING" else paste0(pct, "%")
        cli::cli_bullets(stats::setNames(
          paste0("{.field ", key_fields[i], "}: ", pct_str),
          status
        ))
      }
    }
    
    # Source breakdown
    if (!is.null(data$.meta$sources$banks)) {
      report$banks_sources <- data$.meta$sources$banks
      if (verbose) {
        cli::cli_h3("Data Sources")
        cli::cli_text("Sources: {.val {data$.meta$sources$banks}}")
      }
    }
  }
  
  # === Spatial Coverage ===
  if (verbose) cli::cli_h2("Spatial Coverage")
  
  n_banks <- if (!is.null(data$banks)) nrow(data$banks) else 0
  n_fp <- if (!is.null(data$geometry) && "footprint" %in% names(data$geometry)) {
    sum(!sf::st_is_empty(data$geometry$footprint))
  } else 0
  n_sa <- if (!is.null(data$geometry) && "service_area" %in% names(data$geometry)) {
    sum(!sf::st_is_empty(data$geometry$service_area))
  } else 0
  
  report$spatial <- tibble::tibble(
    type = c("Footprints", "Service Areas"),
    count = c(n_fp, n_sa),
    percent = if (n_banks > 0) round(100 * c(n_fp, n_sa) / n_banks, 1) else c(NA, NA)
  )
  
  if (verbose) {
    cli::cli_bullets(c(
      "*" = "Footprints: {.val {n_fp}}/{n_banks} ({report$spatial$percent[1]}%)",
      "*" = "Service Areas: {.val {n_sa}}/{n_banks} ({report$spatial$percent[2]}%)"
    ))
  }
  
  # === Ledger ===
  txn_data <- data$transactions %||% data$ledger
  if (!is.null(txn_data) && nrow(txn_data) > 0) {
    if (verbose) {
      cli::cli_h2("Ledger Data")
      cli::cli_bullets(c(
        "*" = "Transactions: {.val {nrow(txn_data)}}"
      ))
    }
    report$ledger_rows <- nrow(txn_data)
  }
  
  # === Discrepancies ===
  if (!is.null(data$.meta$discrepancies) && nrow(data$.meta$discrepancies) > 0) {
    n_disc <- nrow(data$.meta$discrepancies)
    if (verbose) {
      cli::cli_h2("Data Discrepancies")
      cli::cli_alert_warning("{n_disc} discrepancies between sources")
      cli::cli_text("Use {.code discrepancies(data)} to view details")
    }
    report$discrepancies <- n_disc
  }
  
  # === Timing ===
  if (!is.null(data$.meta$timing$duration_secs)) {
    if (verbose) {
      cli::cli_h2("Performance")
      cli::cli_text("Fetch time: {.val {round(data$.meta$timing$duration_secs, 1)}} seconds")
    }
    report$fetch_time_secs <- data$.meta$timing$duration_secs
  }
  
  invisible(report)
}

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
      non_empty <- sum(!is.na(vals) & vals != "" & vals != "NA")
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
  
  invisible(list(
    completeness = completeness,
    merged_better_fields = merged_better,
    api_better_fields = api_better,
    n_total = nrow(ledger)
  ))
}

#' Diagnose RIBITS data quality and harmonization
#'
#' Comprehensive diagnostic tool for RIBITS data. Can analyze:
#' - Overall data quality from a ribits() result
#' - Ledger-specific harmonization issues (API vs CSV comparison)
#' - Source completeness comparison
#'
#' @param data One of:
#'   - A ribits_data object (from ribits())
#'   - A data frame (interpreted as ledger/transaction data)
#'   - NULL (if providing bank_ids or state to fetch fresh data)
#' @param type Type of diagnostics to run:
#'   - "auto" (default): Auto-detect based on input
#'   - "overview": General data quality summary
#'   - "ledger": Detailed ledger harmonization diagnostics
#'   - "sources": Compare data completeness across sources
#' @param bank_ids Optional vector of bank IDs for ledger diagnostics
#' @param state Optional state filter for ledger diagnostics
#' @param verbose Print detailed output? Default TRUE
#' @return A list with diagnostic metrics (invisibly)
#' @export
#' @examples
#' \dontrun{
#' # Overall data quality
#' ca <- ribits(state = "CA")
#' rb_diagnose(ca)
#'
#' # Ledger-specific diagnostics
#' rb_diagnose(ca, type = "ledger")
#'
#' # Compare sources
#' rb_diagnose(ca, type = "sources")
#'
#' # Fetch and diagnose specific banks
#' rb_diagnose(bank_ids = c(17, 100), type = "ledger")
#' }
rb_diagnose <- function(data = NULL,
                        type = c("auto", "overview", "ledger", "sources"),
                        bank_ids = NULL,
                        state = NULL,
                        verbose = TRUE) {

  type <- match.arg(type)

  # Auto-detect type based on input
  if (type == "auto") {
    if (inherits(data, "ribits_data")) {
      type <- "overview"
    } else if (is.data.frame(data)) {
      type <- "sources"
    } else if (!is.null(bank_ids) || !is.null(state)) {
      type <- "ledger"
    } else {
      cli::cli_abort("Cannot auto-detect diagnostic type. Please specify 'type' parameter.")
    }
  }

  # Route to appropriate diagnostic
  if (type == "overview") {
    .rb_diagnose_overview(data, verbose)
  } else if (type == "sources") {
    .rb_diagnose_sources(data, verbose)
  } else if (type == "ledger") {
    .rb_diagnose_ledger_comparison(bank_ids, state, data, verbose)
  }
}

#' Internal: Overview diagnostics
#' @keywords internal
#' @noRd
.rb_diagnose_overview <- function(data, verbose = TRUE) {
  if (verbose) cli::cli_h1("RIBITS Data Quality Report")

  results <- list()

  # Banks
  if (!is.null(data$banks) && nrow(data$banks) > 0) {
    if (verbose) cli::cli_h2("Banks")
    if (verbose) cli::cli_alert_info("{nrow(data$banks)} banks")

    # Key field completeness
    key <- c("bank_name", "bank_status", "bank_type", "total_acres", "establishment_date")
    key <- intersect(key, names(data$banks))
    field_completeness <- list()

    for (col in key) {
      non_na <- sum(!is.na(data$banks[[col]]) & data$banks[[col]] != "")
      pct <- round(100 * non_na / nrow(data$banks), 0)
      field_completeness[[col]] <- pct
      if (verbose) cli::cli_alert("{col}: {pct}% complete")
    }

    results$banks <- list(
      n_rows = nrow(data$banks),
      field_completeness = field_completeness
    )
  }

  # Transactions (check both $transactions and $ledger for backwards compat)
  txn_data <- data$transactions %||% data$ledger
  if (!is.null(txn_data) && nrow(txn_data) > 0) {
    if (verbose) cli::cli_h2("Transactions")
    if (verbose) cli::cli_alert_info("{nrow(txn_data)} transactions")

    if ("source" %in% names(txn_data)) {
      src_tbl <- table(txn_data$source)
      if (verbose) {
        for (s in names(src_tbl)) {
          cli::cli_alert("  {s}: {src_tbl[s]}")
        }
      }
      results$transactions <- list(
        n_rows = nrow(txn_data),
        sources = as.list(src_tbl)
      )
    }

    # If source column exists, offer to run detailed source comparison
    if ("source" %in% names(txn_data) && verbose) {
      cli::cli_alert_info("Run rb_diagnose(data, type='sources') for detailed source comparison")
    }
  }

  # Contacts
  if (!is.null(data$contacts) && nrow(data$contacts) > 0) {
    if (verbose) cli::cli_h2("Contacts")
    if (verbose) cli::cli_alert_info("{nrow(data$contacts)} contacts")

    if ("contact_type" %in% names(data$contacts)) {
      type_tbl <- table(data$contacts$contact_type)
      if (verbose) {
        for (t in names(type_tbl)) {
          cli::cli_alert("  {t}: {type_tbl[t]}")
        }
      }
      results$contacts <- list(
        n_rows = nrow(data$contacts),
        types = as.list(type_tbl)
      )
    }
  }

  # Geometry
  if (!is.null(data$geometry) && nrow(data$geometry) > 0) {
    if (verbose) cli::cli_h2("Geometry")
    if (verbose) cli::cli_alert_info("{nrow(data$geometry)} banks with spatial data")

    geom_stats <- list(n_rows = nrow(data$geometry))

    # Check which geometry types are populated
    if ("centroid" %in% names(data$geometry)) {
      has_centroid <- sum(!sf::st_is_empty(data$geometry$centroid))
      if (verbose) cli::cli_alert("  centroids: {has_centroid}/{nrow(data$geometry)}")
      geom_stats$centroids <- has_centroid
    }
    if ("footprint" %in% names(data$geometry)) {
      has_fp <- sum(!sf::st_is_empty(data$geometry$footprint))
      if (verbose) cli::cli_alert("  footprints: {has_fp}/{nrow(data$geometry)}")
      geom_stats$footprints <- has_fp
    }
    if ("service_area" %in% names(data$geometry)) {
      has_sa <- sum(!sf::st_is_empty(data$geometry$service_area))
      if (verbose) cli::cli_alert("  service_areas: {has_sa}/{nrow(data$geometry)}")
      geom_stats$service_areas <- has_sa
    }

    results$geometry <- geom_stats
  }

  # Meta
  if (!is.null(data$.meta)) {
    if (verbose) cli::cli_h2("Metadata")

    meta_info <- list()

    if (!is.null(data$.meta$timing$duration_secs)) {
      duration <- round(data$.meta$timing$duration_secs, 1)
      if (verbose) cli::cli_alert_info("Fetch time: {duration}s")
      meta_info$duration_secs <- duration
    }
    if (!is.null(data$.meta$sources)) {
      sources_used <- paste(names(data$.meta$sources), collapse = ", ")
      if (verbose) cli::cli_alert_info("Sources used: {sources_used}")
      meta_info$sources <- names(data$.meta$sources)
    }

    results$meta <- meta_info
  }

  invisible(results)
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
      non_empty <- sum(!is.na(vals) & vals != "" & vals != "NA")
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

  invisible(list(
    completeness = completeness,
    merged_better_fields = merged_better,
    api_better_fields = api_better,
    n_total = nrow(ledger)
  ))
}

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
