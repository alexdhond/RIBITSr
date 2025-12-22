# R/ribits.R
# Main user-facing functions with automatic harmonization
# Users should primarily use these - all complexity is hidden

#' Get RIBITS data (auto-harmonized from all sources)
#'
#' The main entry point for fetching RIBITS data. Automatically queries
#' all available sources (RIBITS API, EPA ArcGIS, direct CSV downloads),
#' harmonizes the data, detects discrepancies, and returns the best available data.
#'
#' **Note:** Most users should use the simpler wrapper functions instead:
#' - `rb_banks()` - Get bank data
#' - `rb_ilf_programs()` - Get ILF program data
#' - `rb_umbrellas()` - Get umbrella instrument data
#' - `rb_credits()` - Get credit tracking/classification data
#'
#' @param bank_ids Optional vector of bank IDs. If NULL, fetches all banks
#'   matching the filter criteria.
#' @param state Optional state filter (e.g., "CA", "TX")
#' @param district Optional USACE district filter
#' @param what What data to return. Options:
#'   - "all" (default): banks, ledger, footprints, service_areas
#'   - "banks": just bank summary data
#'   - "ledger": transaction/credit data
#'   - "spatial": footprints and service areas
#' @param type Type of resource to fetch:
#'   - "banks" (default): mitigation banks
#'   - "ilf": In-Lieu Fee programs
#'   - "umbrellas": Umbrella instruments
#' @param sources Which data sources to use:
#'   - c("api", "epa", "csv") (default): all sources
#'   - "api": RIBITS API only
#'   - "epa": EPA ArcGIS only
#'   - "csv": Direct CSV downloads only
#' @param cache Cache downloaded CSV files? Default TRUE.
#' @param quietly Suppress progress messages. Default FALSE.
#'
#' @return A `ribits_data` object containing harmonized data with:
#'   \item{banks}{Bank summary tibble}
#'   \item{ledger}{Transaction/ledger tibble}
#'   \item{footprints}{sf object with footprint polygons}
#'   \item{service_areas}{sf object with service area polygons}
#'   \item{.meta}{Metadata including sources used and any discrepancies}
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Recommended: Use simple wrapper functions
#' ca <- rb_banks(state = "CA")
#'
#' # Advanced: Direct ribits() usage
#' ca <- ribits(state = "CA")
#'
#' # Access the data
#' ca$banks
#' ca$ledger
#' ca$footprints
#'
#' # Check for data quality issues
#' ca$.meta$discrepancies
#'
#' # Get specific banks
#' data <- ribits(bank_ids = c(17, 100, 345))
#'
#' # Just get spatial data
#' spatial <- ribits(state = "OR", what = "spatial")
#'
#' # Control data sources
#' api_only <- ribits(state = "TX", sources = "api")
#' }
ribits <- function(bank_ids = NULL,
                    state = NULL,
                    district = NULL,
                    what = "all",
                    type = "banks",
                    sources = c("api", "epa", "csv"),
                    cache = TRUE,
                    quietly = FALSE) {

  start_time <- Sys.time()

  # Normalize sources parameter
  sources <- match.arg(sources, c("api", "epa", "csv"), several.ok = TRUE)
  use_api <- "api" %in% sources
  use_epa <- "epa" %in% sources
  use_csv <- "csv" %in% sources

  # Show header
  if (!quietly) {
    cli::cli_h2("Fetching RIBITS Data")
    cli::cli_alert_info("Sources: {.field {paste(sources, collapse = ', ')}}")
    if (!is.null(state)) cli::cli_alert_info("State filter: {.val {state}}")
    if (!is.null(district)) cli::cli_alert_info("District filter: {.val {district}}")
  }

  # Initialize result
  result <- structure(
    list(
      banks = NULL,
      ledger = NULL,
      footprints = NULL,
      service_areas = NULL,
      .meta = list(
        fetch_date = Sys.Date(),
        fetch_timestamp = start_time,
        query = list(
          bank_ids = bank_ids,
          state = state,
          district = district,
          what = what,
          type = type,
          sources_requested = sources
        ),
        sources = list(),
        discrepancies = tibble::tibble(),
        timing = list(started = start_time)
      )
    ),
    class = c("ribits_data", "list")
  )

  include_banks <- what %in% c("all", "banks")
  include_ledger <- what %in% c("all", "ledger")
  include_spatial <- what %in% c("all", "spatial")

  # ==========================================================================
  # Step 1: Get resource list (banks/ILF/umbrellas/WQT)
  # ==========================================================================
  resource_name <- switch(type,
    banks = "bank",
    ilf = "ILF program",
    umbrellas = "umbrella",
    wqt = "WQT project",
    "resource"
  )

  if (!quietly) cli::cli_h3("Step 1: Fetching {resource_name} list")

  # Try RIBITS API first (if enabled)
  # Note: API list only returns 3 cols (bank_id, name, url)
  # We need to fetch detailed data for each bank to get full info (30+ cols)
  banks_ribits <- if (use_api) {
    if (!quietly) cli::cli_progress_step("Querying RIBITS API...")
    tryCatch({
      # First get list of bank IDs
      bank_list <- rb_get(type, state = state, district = district)
      
      if (is.null(bank_list) || nrow(bank_list) == 0) {
        NULL
      } else {
        # Find bank_id column (case-insensitive since we removed early clean_names)
        id_col <- names(bank_list)[tolower(names(bank_list)) == "bank_id"]
        if (length(id_col) == 0) {
          cli::cli_alert_warning("No bank_id column found in API list")
          NULL
        } else {
          list_ids <- bank_list[[id_col[1]]]
          
          # Filter to specific IDs if provided
          ids_to_fetch <- if (!is.null(bank_ids)) {
            intersect(list_ids, bank_ids)
          } else {
            list_ids
          }
        
        if (length(ids_to_fetch) == 0) {
          NULL
        } else {
          # Fetch detailed data for each bank (has 30+ columns vs 3 in list)
          if (!quietly) cli::cli_alert_info("Fetching detailed API data for {length(ids_to_fetch)} banks...")
          
          detailed <- rb_get(type, id = ids_to_fetch, ledger = FALSE, 
                            footprint = FALSE, service_area = FALSE, contacts = FALSE)
          
          # Extract summary component (the main bank attributes)
          if (is.list(detailed) && "summary" %in% names(detailed)) {
            detailed$summary
          } else {
            # Fallback to list data if detail fetch fails
            bank_list
          }
        }
        }
      }
    }, error = function(e) {
      if (!quietly) cli::cli_alert_warning("API query failed: {e$message}")
      NULL
    })
  } else {
    NULL
  }

  if (!is.null(banks_ribits) && nrow(banks_ribits) > 0) {
    if (!quietly) cli::cli_alert_success("Found {nrow(banks_ribits)} {resource_name}s from API ({ncol(banks_ribits)} columns)")
  }

  # Also fetch from EPA ArcGIS (if enabled) - not just as fallback
  banks_epa <- NULL
  if (use_epa) {
    if (!quietly) cli::cli_progress_step("Querying EPA ArcGIS...")
    banks_epa <- tryCatch({
      epa <- rb_epa("banks", state = state, district = district)
      if (!is.null(epa) && nrow(epa) > 0) sf::st_drop_geometry(epa) else NULL
    }, error = function(e) {
      if (!quietly) cli::cli_alert_warning("EPA query failed: {e$message}")
      NULL
    })

    if (!is.null(banks_epa) && nrow(banks_epa) > 0) {
      if (!quietly) cli::cli_alert_success("Found {nrow(banks_epa)} {resource_name}s from EPA")
      if (!is.null(bank_ids)) {
        # Find bank_id column (case-insensitive)
        id_col <- names(banks_epa)[tolower(names(banks_epa)) == "bank_id"][1]
        if (!is.na(id_col)) {
          banks_epa <- banks_epa |> dplyr::filter(.data[[id_col]] %in% bank_ids)
        }
      }
    }
  }

  # Try CSV downloads (if enabled)
  banks_csv <- NULL
  if (use_csv) {
    if (!quietly) cli::cli_progress_step("Downloading CSV reports...")

    banks_csv <- tryCatch({
      # Use temporary cache directory if caching enabled
      cache_dir <- if (cache) file.path(tempdir(), "ribits_cache") else tempdir()
      dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

      # Download bank summary CSV directly
      csv_file <- rb_download_report("banks_sites", download_dir = cache_dir)

      # Read CSV
      banks <- rb_read(csv_file)
      
      # CSV column names differ from API/EPA - handle variations
      # state_abbrev_list -> filter by state
      if (!is.null(state)) {
        state_col <- names(banks)[grepl("state", names(banks), ignore.case = TRUE)][1]
        if (!is.na(state_col)) {
          banks <- banks |> dplyr::filter(grepl(!!state, .data[[state_col]]))
        }
      }
      
      # Add bank_id via name matching (CSV doesn't have bank_id natively)
      # Check if bank_id exists AND has MOSTLY valid values (rb_read may extract wrong IDs from names)
      # Require >50% valid bank_ids to skip name matching
      has_valid_bank_id <- "bank_id" %in% names(banks) && 
        (sum(!is.na(banks$bank_id)) / nrow(banks)) > 0.5
      
      if (!has_valid_bank_id) {
        name_col <- names(banks)[grepl("^name$|bank.?name", names(banks), ignore.case = TRUE)][1]
        if (!is.na(name_col)) {
          if (!quietly) cli::cli_alert_info("Matching CSV names to bank IDs...")
          # Remove any existing empty bank_id column before matching
          if ("bank_id" %in% names(banks)) {
            banks <- banks |> dplyr::select(-bank_id)
          }
          lookup <- rb_build_name_lookup(include_csv = FALSE, cache = TRUE)
          banks <- rb_match_names(banks, lookup, name_col = name_col, fuzzy = TRUE)
          
          # Remove unmatched rows for harmonization (find bank_id column case-insensitive)
          id_col <- names(banks)[tolower(names(banks)) == "bank_id"][1]
          n_unmatched <- if (!is.na(id_col)) sum(is.na(banks[[id_col]])) else 0
          if (n_unmatched > 0 && !quietly) {
            cli::cli_alert_warning("{n_unmatched} CSV rows couldn't be matched to bank_id")
          }
          if (!is.na(id_col)) {
            banks <- banks |> dplyr::filter(!is.na(.data[[id_col]]))
          }
        }
      }
      
      # Filter by bank_ids if specified (case-insensitive column lookup)
      id_col <- names(banks)[tolower(names(banks)) == "bank_id"][1]
      if (!is.null(bank_ids) && !is.na(id_col)) {
        banks <- banks |> dplyr::filter(.data[[id_col]] %in% !!bank_ids)
      }

      if (!quietly && nrow(banks) > 0) {
        cli::cli_alert_success("{nrow(banks)} banks from CSV")
      }
      
      banks
    }, error = function(e) {
      if (!quietly) cli::cli_alert_warning("CSV fetch failed: {e$message}")
      NULL
    })
  }

  # Merge and detect discrepancies if we have multiple sources
  # Collect all available bank data sources
  bank_sources <- list(
    ribits_api = banks_ribits,
    epa_arcgis = banks_epa,
    ribits_csv = banks_csv
  )
  
  # Filter to non-empty sources
  available_sources <- purrr::keep(bank_sources, ~ !is.null(.) && nrow(.) > 0)
  
  if (length(available_sources) > 1) {
    # Detect discrepancies between sources
    source_names <- names(available_sources)
    for (i in 1:(length(available_sources) - 1)) {
      for (j in (i + 1):length(available_sources)) {
        disc <- .compare_dataframes(
          available_sources[[i]], available_sources[[j]],
          id_col = "bank_id",
          source1_name = source_names[i],
          source2_name = source_names[j]
        )
        if (!is.null(disc) && nrow(disc) > 0) {
          result$.meta$discrepancies <- dplyr::bind_rows(result$.meta$discrepancies, disc)
        }
      }
    }
    
    # Merge all sources preserving columns (CSV priority, then API, then EPA)
    result$banks <- .merge_multiple_sources(
      available_sources,
      by = "bank_id",
      priority_order = c("ribits_csv", "ribits_api", "epa_arcgis")
    )
    result$.meta$sources$banks <- paste(names(available_sources), collapse = " + ")
    
    if (!quietly) {
      cli::cli_alert_info("Merged {length(available_sources)} sources: {result$.meta$sources$banks}")
      cli::cli_alert_info("Result has {ncol(result$banks)} columns (preserving all from each source)")
    }
  } else if (length(available_sources) == 1) {
    result$banks <- available_sources[[1]]
    result$.meta$sources$banks <- names(available_sources)[1]
  }

  # Get bank IDs for subsequent queries (handle both column naming conventions)
  if (!is.null(result$banks) && nrow(result$banks) > 0) {
    # Find bank_id column (case-insensitive)
    id_col <- names(result$banks)[tolower(names(result$banks)) == "bank_id"]
    if (length(id_col) > 0) {
      query_ids <- result$banks[[id_col[1]]]
    } else {
      query_ids <- bank_ids
    }
  } else {
    query_ids <- bank_ids
  }

  if (is.null(query_ids) || length(query_ids) == 0) {
    if (!quietly) cli::cli_alert_warning("No banks found matching criteria")
    return(result)
  }

  if (!quietly) cli::cli_alert_success("Found {length(query_ids)} banks")

  # ==========================================================================
  # Step 2: Get ledger/transaction data (harmonized from multiple sources)
  # ==========================================================================
  if (include_ledger) {
    if (!quietly) cli::cli_h3("Step 2: Fetching transaction/ledger data")

    # Use harmonized transaction fetching
    txn_sources <- c(
      if (use_api) "api" else NULL,
      if (use_csv) "csv" else NULL
    )
    
    if (length(txn_sources) > 0) {
      txn_result <- tryCatch({
        cache_dir <- if (cache) file.path(tempdir(), "ribits_cache") else tempdir()
        rb_transactions(
          bank_ids = query_ids,
          sources = txn_sources,
          include_credit_class = use_csv,
          progress = !quietly,
          cache_dir = cache_dir
        )
      }, error = function(e) {
        if (!quietly) cli::cli_alert_warning("Transaction fetch failed: {e$message}")
        NULL
      })
      
      if (!is.null(txn_result)) {
        if (!is.null(txn_result$transactions) && nrow(txn_result$transactions) > 0) {
          result$ledger <- txn_result$transactions
          result$.meta$sources$ledger <- txn_result$.meta$sources$transactions
        }
        if (!is.null(txn_result$credit_summary) && nrow(txn_result$credit_summary) > 0) {
          result$credit_summary <- txn_result$credit_summary
          result$.meta$sources$credit_summary <- "csv"
        }
      }
    }
    
    # Legacy fallback if harmonized fetch returned nothing
    if ((is.null(result$ledger) || nrow(result$ledger) == 0) && use_csv) {
      if (!quietly) cli::cli_progress_step("Downloading ledger CSV (fallback)...")
      ledger_csv <- tryCatch({
        cache_dir <- if (cache) file.path(tempdir(), "ribits_cache") else tempdir()
        dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

        csv_file <- rb_download_report("ledger_transactions", download_dir = cache_dir)
        ledger <- rb_read(csv_file)

        # Filter by query_ids if available (case-insensitive)
        id_col <- names(ledger)[tolower(names(ledger)) == "bank_id"][1]
        if (!is.null(query_ids) && !is.na(id_col)) {
          ledger <- ledger |> dplyr::filter(.data[[id_col]] %in% !!query_ids)
        }

        ledger
      }, error = function(e) {
        if (!quietly) cli::cli_alert_warning("CSV ledger download failed: {e$message}")
        NULL
      })

      if (!is.null(ledger_csv) && nrow(ledger_csv) > 0) {
        result$ledger <- ledger_csv
        result$.meta$sources$ledger <- "ribits_csv"
        if (!quietly) cli::cli_alert_success("{nrow(result$ledger)} transactions from CSV")
      }
    }
  }

  # ==========================================================================
  # Step 3: Get spatial data (harmonized from both sources)
  # ==========================================================================
  if (include_spatial) {
    if (!quietly) cli::cli_h3("Step 3: Fetching spatial data")

    # Get from EPA ArcGIS (faster for bulk) - if enabled
    epa_footprints <- if (use_epa) {
      if (!quietly) cli::cli_progress_step("Querying EPA footprints...")
      tryCatch({
        fp <- rb_epa("footprints", bank_ids = query_ids)
        if (!is.null(fp) && nrow(fp) > 0) {
          fp$source <- "epa_arcgis"
          if (!quietly) cli::cli_alert_success("{nrow(fp)} footprints from EPA")
        }
        fp
      }, error = function(e) {
        if (!quietly) cli::cli_alert_warning("EPA footprints failed: {e$message}")
        NULL
      })
    } else {
      NULL
    }

    epa_service_areas <- if (use_epa) {
      if (!quietly) cli::cli_progress_step("Querying EPA service areas...")
      tryCatch({
        sa <- rb_epa("service_areas", bank_ids = query_ids)
        if (!is.null(sa) && nrow(sa) > 0) {
          sa$source <- "epa_arcgis"
          if (!quietly) cli::cli_alert_success("{nrow(sa)} service areas from EPA")
        }
        sa
      }, error = function(e) {
        if (!quietly) cli::cli_alert_warning("EPA service areas failed: {e$message}")
        NULL
      })
    } else {
      NULL
    }

    # Get from RIBITS API for banks missing from EPA (sample if many) - if enabled
    # Find bank_id column (case-insensitive)
    epa_fp_ids <- if (!is.null(epa_footprints)) {
      fp_id_col <- names(epa_footprints)[tolower(names(epa_footprints)) == "bank_id"][1]
      if (!is.na(fp_id_col)) epa_footprints[[fp_id_col]] else integer()
    } else integer()
    missing_fp_ids <- setdiff(query_ids, epa_fp_ids)

    ribits_footprints <- NULL
    if (use_api && length(missing_fp_ids) > 0 && length(missing_fp_ids) <= 30) {
      if (!quietly) cli::cli_progress_step("Fetching {length(missing_fp_ids)} missing footprints from API...")
      ribits_fp_list <- list()
      for (bid in missing_fp_ids) {
        tryCatch({
          bd <- rb_get("banks", id = bid, footprint = TRUE, ledger = FALSE,
                       service_area = FALSE, contacts = FALSE)
          if (!is.null(bd$footprint) && nrow(bd$footprint) > 0) {
            fp <- bd$footprint
            fp$source <- "ribits_api"
            fp$bank_id <- bid
            ribits_fp_list[[length(ribits_fp_list) + 1]] <- fp
          }
        }, error = function(e) NULL)
        Sys.sleep(0.05)
      }
      if (length(ribits_fp_list) > 0) {
        ribits_footprints <- dplyr::bind_rows(ribits_fp_list)
        if (!quietly) cli::cli_alert_success("{nrow(ribits_footprints)} additional footprints from API")
      }
    }

    # Combine footprints
    if (!is.null(epa_footprints) || !is.null(ribits_footprints)) {
      all_fp <- list()
      if (!is.null(epa_footprints) && nrow(epa_footprints) > 0) {
        all_fp[[1]] <- epa_footprints
      }
      if (!is.null(ribits_footprints) && nrow(ribits_footprints) > 0) {
        all_fp[[length(all_fp) + 1]] <- ribits_footprints
      }
      result$footprints <- tryCatch(dplyr::bind_rows(all_fp), error = function(e) all_fp[[1]])

      # Check for discrepancies in overlapping data
      if (!is.null(epa_footprints) && !is.null(ribits_footprints)) {
        # Find bank_id columns (case-insensitive)
        epa_fp_col <- names(epa_footprints)[tolower(names(epa_footprints)) == "bank_id"][1]
        rib_fp_col <- names(ribits_footprints)[tolower(names(ribits_footprints)) == "bank_id"][1]
        if (!is.na(epa_fp_col) && !is.na(rib_fp_col)) {
          overlap_ids <- intersect(epa_footprints[[epa_fp_col]], ribits_footprints[[rib_fp_col]])
          for (bid in overlap_ids) {
            disc <- .compare_geometries(
              ribits_footprints[ribits_footprints[[rib_fp_col]] == bid, ],
              epa_footprints[epa_footprints[[epa_fp_col]] == bid, ],
              bid, "footprint"
            )
            if (!is.null(disc)) {
              result$.meta$discrepancies <- dplyr::bind_rows(
                result$.meta$discrepancies, disc
              )
            }
          }
        }
      }

      result$.meta$sources$footprints <- "epa_arcgis + ribits_api"
    }

    # Service areas (EPA ArcGIS is primary)
    if (!is.null(epa_service_areas) && nrow(epa_service_areas) > 0) {
      result$service_areas <- epa_service_areas
      result$.meta$sources$service_areas <- "epa_arcgis"
    }

    if (!quietly) {
      n_fp <- if (!is.null(result$footprints)) nrow(result$footprints) else 0
      n_sa <- if (!is.null(result$service_areas)) nrow(result$service_areas) else 0
      cli::cli_alert_success("{n_fp} footprints, {n_sa} service areas")
    }
  }

  # ==========================================================================
  # Finalize - Apply clean_names() to all data frames for consistent output
  # ==========================================================================
  
  # Standardize column names across all data components (single point of normalization)
  if (!is.null(result$banks) && nrow(result$banks) > 0) {
    result$banks <- janitor::clean_names(result$banks)
  }
  if (!is.null(result$ledger) && nrow(result$ledger) > 0) {
    result$ledger <- janitor::clean_names(result$ledger)
  }
  if (!is.null(result$footprints) && nrow(result$footprints) > 0) {
    result$footprints <- janitor::clean_names(result$footprints)
  }
  if (!is.null(result$service_areas) && nrow(result$service_areas) > 0) {
    result$service_areas <- janitor::clean_names(result$service_areas)
  }
  
  result$.meta$timing$completed <- Sys.time()
  result$.meta$timing$duration_secs <- as.numeric(
    difftime(result$.meta$timing$completed, start_time, units = "secs")
  )

  if (!quietly) {
    cli::cli_h3("Summary")
    
    # Summary of what was fetched
    n_banks <- if (!is.null(result$banks)) nrow(result$banks) else 0
    n_ledger <- if (!is.null(result$ledger)) nrow(result$ledger) else 0
    n_fp <- if (!is.null(result$footprints)) nrow(result$footprints) else 0
    n_sa <- if (!is.null(result$service_areas)) nrow(result$service_areas) else 0
    
    cli::cli_bullets(c(
      "v" = "{n_banks} {resource_name}s",
      "v" = "{n_ledger} transactions",
      "v" = "{n_fp} footprints",
      "v" = "{n_sa} service areas"
    ))
    
    # Report discrepancies
    n_disc <- nrow(result$.meta$discrepancies)
    if (n_disc > 0) {
      cli::cli_alert_warning("{n_disc} discrepancies detected - use {.code discrepancies(result)} to view")
    }
    
    cli::cli_alert_success("Completed in {round(result$.meta$timing$duration_secs, 1)}s")
  }

  result
}


#' Print method for ribits_data
#' 
#' @param x A ribits_data object
#' @param ... Additional arguments passed to print
#' @export
print.ribits_data <- function(x, ...) {
  cli::cli_h1("RIBITS Data")

  # Fetch date
  if (!is.null(x$.meta$fetch_date)) {
    cli::cli_text("Fetched: {.val {x$.meta$fetch_date}}")
  }

  # Query info
  q <- x$.meta$query
  filters <- c()
  if (!is.null(q$state)) filters <- c(filters, paste0("state=", q$state))
  if (!is.null(q$district)) filters <- c(filters, paste0("district=", q$district))
  if (!is.null(q$bank_ids)) filters <- c(filters, paste0(length(q$bank_ids), " bank_ids"))
  if (length(filters) > 0) {
    cli::cli_text("Query: {paste(filters, collapse = ', ')}")
  }

  cli::cli_h2("Data")

  # Banks
  if (!is.null(x$banks) && nrow(x$banks) > 0) {
    src <- x$.meta$sources$banks %||% "?"
    cli::cli_alert_success("banks: {nrow(x$banks)} rows [{src}]")
  } else {
    cli::cli_alert_warning("banks: none")
  }

  # Ledger
  if (!is.null(x$ledger) && nrow(x$ledger) > 0) {
    src <- x$.meta$sources$ledger %||% "?"
    cli::cli_alert_success("ledger: {nrow(x$ledger)} transactions [{src}]")
  } else {
    cli::cli_alert_warning("ledger: none")
  }

  # Footprints
  if (!is.null(x$footprints) && nrow(x$footprints) > 0) {
    src <- x$.meta$sources$footprints %||% "?"
    cli::cli_alert_success("footprints: {nrow(x$footprints)} polygons [{src}]")
  } else {
    cli::cli_alert_warning("footprints: none")
  }

  # Service areas
  if (!is.null(x$service_areas) && nrow(x$service_areas) > 0) {
    src <- x$.meta$sources$service_areas %||% "?"
    cli::cli_alert_success("service_areas: {nrow(x$service_areas)} polygons [{src}]")
  } else {
    cli::cli_alert_warning("service_areas: none")
  }

  # Discrepancies
  n_disc <- nrow(x$.meta$discrepancies)
  if (n_disc > 0) {
    cli::cli_h2("Data Quality")
    cli::cli_alert_warning("{n_disc} discrepancies between sources")
    cli::cli_text("Use {.code $discrepancies()} to view details")
  }

  invisible(x)
}


#' Access discrepancies from ribits_data
#'
#' @param x A ribits_data object
#' @param ... Additional arguments passed to methods
#' @return A tibble of discrepancies
#' @export
discrepancies <- function(x, ...) {
  UseMethod("discrepancies")
}

#' @rdname discrepancies
#' @export
discrepancies.ribits_data <- function(x, ...) {
  disc <- x$.meta$discrepancies

  if (is.null(disc) || nrow(disc) == 0) {
    cli::cli_alert_success("No discrepancies found")
    return(tibble::tibble())
  }

  cli::cli_h2("Data Discrepancies")
  cli::cli_text("These banks have different values in RIBITS API vs EPA ArcGIS:")
  cli::cli_text("")

  for (i in seq_len(nrow(disc))) {
    d <- disc[i, ]
    cli::cli_alert_warning(
      "Bank {d$bank_id} {d$data_type}: {d$value1_acres} acres (RIBITS) vs {d$value2_acres} acres (EPA) [{d$diff_pct}% diff]"
    )
  }

  invisible(disc)
}


#' Subset ribits_data by bank IDs
#'
#' @param x A ribits_data object
#' @param bank_ids Bank IDs to keep
#' @param ... Additional arguments ignored
#' @return Filtered ribits_data object
#' @export
subset.ribits_data <- function(x, bank_ids, ...) {
  result <- x

  if (!is.null(result$banks)) {
    result$banks <- result$banks |> dplyr::filter(.data$bank_id %in% bank_ids)
  }

  if (!is.null(result$ledger)) {
    result$ledger <- result$ledger |> dplyr::filter(.data$bank_id %in% bank_ids)
  }

  if (!is.null(result$footprints)) {
    id_col <- "bank_id"  # Standardized column name
    result$footprints <- result$footprints |>
      dplyr::filter(.data[[id_col]] %in% bank_ids)
  }

  if (!is.null(result$service_areas)) {
    id_col <- "bank_id"  # Standardized column name
    result$service_areas <- result$service_areas |>
      dplyr::filter(.data[[id_col]] %in% bank_ids)
  }

  result
}
