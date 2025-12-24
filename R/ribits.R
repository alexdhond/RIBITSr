# R/ribits.R
# Main user-facing functions with automatic harmonization
# Users should primarily use these - all complexity is hidden

#' Internal harmonization engine
#' @keywords internal
#' @noRd
.ribits_engine <- function(bank_ids = NULL,
                    state = NULL,
                    district = NULL,
                    what = "all",
                    type = "banks",
                    sources = c("api", "epa", "csv"),
                    include_detailed_contacts = FALSE,
                    include_detailed_transactions = FALSE,
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
  # Architecture: Query ALL sources, combine/harmonize, fill gaps, detect discrepancies
  # NEW STRUCTURE: 3 dataframes (banks, transactions, geometry) instead of 5
  # - banks: includes contact/credit summaries
  # - transactions: unified from watershed CSV + API + CSV ledger (if include_detailed_transactions=TRUE)
  # - geometry: unified spatial data
  # - .contacts: detailed contacts (if include_detailed_contacts=TRUE)
  result <- structure(
    list(
      banks = NULL,           # Summary with contact/credit columns
      transactions = NULL,    # Unified transactions (if include_detailed_transactions=TRUE)
      geometry = NULL,        # Unified spatial: centroids + footprints + service_areas
      .contacts = NULL,       # Detailed contacts (if include_detailed_contacts=TRUE)
      .meta = list(
        fetch_date = Sys.Date(),
        fetch_timestamp = start_time,
        query = list(
          bank_ids = bank_ids,
          state = state,
          district = district,
          what = what,
          type = type,
          sources_requested = sources,
          include_detailed_contacts = include_detailed_contacts,
          include_detailed_transactions = include_detailed_transactions
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
  
  banks_ribits <- NULL
  if (use_api) {
    if (!quietly) cli::cli_progress_step("Querying RIBITS API...")
    api_result <- tryCatch({
      # First get list of bank IDs
      bank_list <- rb_get(type, state = state, district = district)
      
      if (is.null(bank_list) || nrow(bank_list) == 0) {
        list(banks = NULL, contacts = NULL)
      } else {
        # Find bank_id column (case-insensitive)
        id_col <- .get_column_case_insensitive(bank_list, "bank_id")
        if (is.na(id_col)) {
          cli::cli_alert_warning("No bank_id column found in API list")
          list(banks = NULL, contacts = NULL)
        } else {
          list_ids <- bank_list[[id_col]]
          
          # Filter to specific IDs if provided
          ids_to_fetch <- if (!is.null(bank_ids)) {
            intersect(list_ids, bank_ids)
          } else {
            list_ids
          }
        
          if (length(ids_to_fetch) == 0) {
            list(banks = NULL, contacts = NULL)
          } else {
            # Fetch detailed data for each bank (has 30+ columns vs 3 in list)
            # Enable contacts to get sponsors/POCs/managers in the same call
            if (!quietly) cli::cli_alert_info("Fetching detailed API data for {length(ids_to_fetch)} banks...")
            
            detailed <- rb_get(type, id = ids_to_fetch, ledger = FALSE, 
                              footprint = FALSE, service_area = FALSE, contacts = TRUE)
            
            # Extract summary and contacts
            banks_data <- if (is.list(detailed) && "summary" %in% names(detailed)) {
              detailed$summary
            } else {
              bank_list
            }
            
            contacts_data <- if (is.list(detailed) && !is.null(detailed$contacts) && 
                                 nrow(detailed$contacts) > 0) {
              detailed$contacts
            } else {
              NULL
            }
            
            list(banks = banks_data, contacts = contacts_data)
          }
        }
      }
    }, error = function(e) {
      if (!quietly) cli::cli_alert_warning("API query failed: {e$message}")
      list(banks = NULL, contacts = NULL)
    })
    
    banks_ribits <- api_result$banks
    
    # Store contacts from API
    if (!is.null(api_result$contacts) && nrow(api_result$contacts) > 0) {
      result$contacts <- api_result$contacts
      result$.meta$sources$contacts <- "ribits_api"
    }
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
        banks_epa <- .filter_by_bank_ids(banks_epa, bank_ids, quietly = quietly)
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

          # Remove unmatched rows for harmonization
          id_col <- .get_column_case_insensitive(banks, "bank_id")
          n_unmatched <- if (!is.na(id_col)) sum(is.na(banks[[id_col]])) else 0
          if (n_unmatched > 0 && !quietly) {
            cli::cli_alert_warning("{n_unmatched} CSV rows couldn't be matched to bank_id")
          }
          if (!is.na(id_col)) {
            banks <- banks |> dplyr::filter(!is.na(.data[[id_col]]))
          }
        }
      }

      # Filter by bank_ids if specified
      if (!is.null(bank_ids)) {
        banks <- .filter_by_bank_ids(banks, bank_ids, quietly = quietly)
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
    # Find bank_id column
    id_col <- .get_column_case_insensitive(result$banks, "bank_id")
    if (!is.na(id_col)) {
      query_ids <- result$banks[[id_col]]
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
  # Step 2: Extract contacts for summarization (ALWAYS needed)
  # ==========================================================================
  if (!quietly) cli::cli_h3("Step 2: Extracting contacts and credit data")

  # Extract contacts from API data (needed for summary even if not returning detailed)
  contacts_detailed <- NULL
  if (!is.null(banks_ribits) && nrow(banks_ribits) > 0) {
    contacts_detailed <- rb_extract_contacts(banks_ribits)
    if (!quietly && !is.null(contacts_detailed) && nrow(contacts_detailed) > 0) {
      cli::cli_alert_success("{nrow(contacts_detailed)} contacts extracted from API")
    }
  }

  # Store detailed contacts if requested
  if (include_detailed_contacts && !is.null(contacts_detailed)) {
    result$.contacts <- contacts_detailed
    result$.meta$sources$contacts <- "ribits_api"
  }

  # ==========================================================================
  # Step 3: Fetch and summarize credit classification data
  # ==========================================================================
  credit_class_detailed <- NULL
  if (use_csv) {
    credit_class_detailed <- tryCatch({
      cache_dir <- if (cache) file.path(tempdir(), "ribits_cache") else tempdir()
      dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

      cc_file <- rb_download_report("credit_classification", download_dir = cache_dir)
      data <- rb_read(cc_file)

      # Match to bank_ids if needed (names already cleaned by rb_read)
      if (!"bank_id" %in% names(data) && "bank_name" %in% names(data)) {
        lookup <- rb_build_name_lookup(include_csv = FALSE, cache = TRUE)
        data <- rb_match_names(data, lookup, name_col = "bank_name", fuzzy = TRUE)
      }

      if ("bank_id" %in% names(data)) {
        data <- data |> dplyr::filter(bank_id %in% query_ids)
      }

      data
    }, error = function(e) {
      if (!quietly) cli::cli_alert_warning("Credit classification failed: {e$message}")
      NULL
    })

    if (!quietly && !is.null(credit_class_detailed) && nrow(credit_class_detailed) > 0) {
      cli::cli_alert_success("{nrow(credit_class_detailed)} credit classifications")
    }
  }

  # ==========================================================================
  # Step 4: Create summaries and merge into banks
  # ==========================================================================
  if (!quietly) cli::cli_progress_step("Creating contact and credit summaries...")

  # Summarize contacts
  contact_summary <- .summarize_contacts(contacts_detailed)

  # Summarize credits
  credit_summary <- .summarize_credits(credit_class_detailed)

  # Merge summaries into banks
  if (!is.null(result$banks) && nrow(result$banks) > 0) {
    if (!is.null(contact_summary) && nrow(contact_summary) > 0) {
      result$banks <- result$banks |>
        dplyr::left_join(contact_summary, by = "bank_id")
      if (!quietly) cli::cli_alert_success("Added contact summaries to banks")
    }

    if (!is.null(credit_summary) && nrow(credit_summary) > 0) {
      result$banks <- result$banks |>
        dplyr::left_join(credit_summary, by = "bank_id")
      if (!quietly) cli::cli_alert_success("Added credit summaries to banks")
    }
  }

  # ==========================================================================
  # Step 5: Fetch detailed transactions (if requested)
  # ==========================================================================
  if (include_detailed_transactions) {
    if (!quietly) cli::cli_h3("Step 5: Fetching unified transaction data")

    txn_result <- tryCatch({
      cache_dir <- if (cache) file.path(tempdir(), "ribits_cache") else tempdir()
      rb_transactions(
        bank_ids = query_ids,
        include_detailed = TRUE,
        cache_dir = cache_dir
      )
    }, error = function(e) {
      if (!quietly) cli::cli_alert_warning("Transaction fetch failed: {e$message}")
      NULL
    })

    if (!is.null(txn_result) && !is.null(txn_result$transactions)) {
      result$transactions <- txn_result$transactions
      result$.meta$sources$transactions <- txn_result$.meta$sources$transactions
    }
  }

  # ==========================================================================
  # Step 6: Get spatial data (harmonized from both sources)
  # ==========================================================================
  if (include_spatial) {
    if (!quietly) cli::cli_h3("Step 6: Fetching spatial data")

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
      fp_id_col <- .get_column_case_insensitive(epa_footprints, "bank_id")
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

    # Combine footprints into temporary variable (will be added to unified geometry)
    footprints_data <- NULL
    if (!is.null(epa_footprints) || !is.null(ribits_footprints)) {
      all_fp <- list()
      if (!is.null(epa_footprints) && nrow(epa_footprints) > 0) {
        all_fp[[1]] <- epa_footprints
      }
      if (!is.null(ribits_footprints) && nrow(ribits_footprints) > 0) {
        all_fp[[length(all_fp) + 1]] <- ribits_footprints
      }
      footprints_data <- tryCatch(dplyr::bind_rows(all_fp), error = function(e) all_fp[[1]])

      # Check for discrepancies in overlapping data
      if (!is.null(epa_footprints) && !is.null(ribits_footprints)) {
        epa_fp_col <- .get_column_case_insensitive(epa_footprints, "bank_id")
        rib_fp_col <- .get_column_case_insensitive(ribits_footprints, "bank_id")
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
      result$.meta$sources$geometry <- "epa_arcgis + ribits_api"
    }

    # Service areas (EPA ArcGIS + API fallback)
    # Get API service areas for banks missing from EPA
    epa_sa_ids <- if (!is.null(epa_service_areas) && nrow(epa_service_areas) > 0) {
      sa_id_col <- .get_column_case_insensitive(epa_service_areas, "bank_id")
      if (!is.na(sa_id_col)) epa_service_areas[[sa_id_col]] else integer()
    } else integer()
    missing_sa_ids <- setdiff(query_ids, epa_sa_ids)
    
    ribits_service_areas <- NULL
    if (use_api && length(missing_sa_ids) > 0 && length(missing_sa_ids) <= 30) {
      if (!quietly) cli::cli_progress_step("Fetching {length(missing_sa_ids)} service areas from API...")
      ribits_sa_list <- list()
      for (bid in missing_sa_ids) {
        tryCatch({
          bd <- rb_get("banks", id = bid, service_area = TRUE, footprint = FALSE,
                       ledger = FALSE, contacts = FALSE)
          if (!is.null(bd$service_area) && nrow(bd$service_area) > 0) {
            sa <- bd$service_area
            sa$source <- "ribits_api"
            sa$bank_id <- bid
            ribits_sa_list[[length(ribits_sa_list) + 1]] <- sa
          }
        }, error = function(e) NULL)
        Sys.sleep(0.05)
      }
      if (length(ribits_sa_list) > 0) {
        ribits_service_areas <- dplyr::bind_rows(ribits_sa_list)
        if (!quietly) cli::cli_alert_success("{nrow(ribits_service_areas)} service areas from API")
      }
    }
    
    # Combine service areas into temporary variable (will be added to unified geometry)
    service_areas_data <- NULL
    if (!is.null(epa_service_areas) || !is.null(ribits_service_areas)) {
      all_sa <- list()
      if (!is.null(epa_service_areas) && nrow(epa_service_areas) > 0) {
        all_sa[[1]] <- epa_service_areas
      }
      if (!is.null(ribits_service_areas) && nrow(ribits_service_areas) > 0) {
        all_sa[[length(all_sa) + 1]] <- ribits_service_areas
      }
      if (length(all_sa) > 0) {
        service_areas_data <- tryCatch(dplyr::bind_rows(all_sa), error = function(e) all_sa[[1]])
      }
    }

    if (!quietly) {
      n_fp <- if (!is.null(footprints_data)) nrow(footprints_data) else 0
      n_sa <- if (!is.null(service_areas_data)) nrow(service_areas_data) else 0
      cli::cli_alert_success("{n_fp} footprints, {n_sa} service areas")
    }
  } else {
    # Initialize to NULL if not fetching spatial
    footprints_data <- NULL
    service_areas_data <- NULL
  }

  # ==========================================================================
  # Finalize - Clean, dedupe, and order columns for all data components
  # ==========================================================================
  
  if (!is.null(result$banks) && nrow(result$banks) > 0) {
    result$banks <- .finalize_df(result$banks, type = "banks")
  }
  if (!is.null(result$transactions) && nrow(result$transactions) > 0) {
    result$transactions <- .finalize_df(result$transactions, type = "transactions")
  }
  if (!is.null(result$.contacts) && nrow(result$.contacts) > 0) {
    result$.contacts <- .finalize_df(result$.contacts, type = "contacts")
  }
  
  # ==========================================================================
  # Create unified geometry layer: wide format (one row per bank)
  # Columns: bank_id, bank_name, bank_status, centroid, footprint, service_area
  # ==========================================================================
  if (include_spatial) {
    
    # Helper to coalesce name columns into bank_name
    .coalesce_name <- function(df) {
      if ("bank_name" %in% names(df) && "name" %in% names(df)) {
        df$bank_name <- dplyr::coalesce(df$bank_name, df$name)
        df <- df |> dplyr::select(-name)
      } else if ("name" %in% names(df) && !("bank_name" %in% names(df))) {
        names(df)[names(df) == "name"] <- "bank_name"
      }
      df
    }
    
    # Start with banks as base (one row per bank)
    if (!is.null(result$banks) && nrow(result$banks) > 0) {
      geom_base <- result$banks |>
        dplyr::select(dplyr::any_of(c("bank_id", "bank_name", "bank_status"))) |>
        dplyr::distinct()
      
      # 1. Extract centroids from bank_location_centroid (GeoJSON strings)
      centroids <- NULL
      if ("bank_location_centroid" %in% names(result$banks)) {
        centroids_list <- list()
        for (i in seq_len(nrow(result$banks))) {
          centroid_json <- result$banks$bank_location_centroid[i]
          bid <- result$banks$bank_id[i]
          if (!is.na(centroid_json) && nchar(centroid_json) > 10) {
            tryCatch({
              pt <- sf::st_read(centroid_json, quiet = TRUE, drivers = "GeoJSON")
              if (nrow(pt) > 0) {
                centroids_list[[as.character(bid)]] <- sf::st_geometry(pt)[[1]]
              }
            }, error = function(e) NULL)
          }
        }
        if (length(centroids_list) > 0) {
          centroids <- tibble::tibble(
            bank_id = as.integer(names(centroids_list)),
            centroid = sf::st_sfc(centroids_list, crs = 4326)
          )
        }
      }
      
      # 2. Extract footprints (one per bank_id)
      footprints <- NULL
      if (!is.null(footprints_data) && nrow(footprints_data) > 0 &&
          inherits(footprints_data, "sf")) {
        # Names already cleaned by rb_epa_query()
        if ("bank_id" %in% names(footprints_data)) {
          footprints <- footprints_data |>
            dplyr::group_by(bank_id) |>
            dplyr::summarise(footprint = sf::st_union(geometry), .groups = "drop") |>
            sf::st_as_sf()
        }
      }

      # 3. Extract service areas (one per bank_id)
      service_areas <- NULL
      if (!is.null(service_areas_data) && nrow(service_areas_data) > 0 &&
          inherits(service_areas_data, "sf")) {
        # Names already cleaned by rb_epa_query()
        if ("bank_id" %in% names(service_areas_data)) {
          service_areas <- service_areas_data |>
            dplyr::group_by(bank_id) |>
            dplyr::summarise(service_area = sf::st_union(geometry), .groups = "drop") |>
            sf::st_as_sf()
        }
      }
      
      # Join all geometries to base (wide format)
      result$geometry <- tryCatch({
        geom_wide <- geom_base
        
        # Add centroids
        if (!is.null(centroids)) {
          geom_wide <- dplyr::left_join(geom_wide, centroids, by = "bank_id")
        } else {
          geom_wide$centroid <- sf::st_sfc(lapply(seq_len(nrow(geom_wide)), 
                                                   function(x) sf::st_point()), crs = 4326)
        }
        
        # Add footprints
        if (!is.null(footprints)) {
          fp_df <- sf::st_drop_geometry(footprints)
          fp_df$footprint <- sf::st_geometry(footprints)
          geom_wide <- dplyr::left_join(geom_wide, fp_df, by = "bank_id")
        } else {
          geom_wide$footprint <- sf::st_sfc(lapply(seq_len(nrow(geom_wide)), 
                                                    function(x) sf::st_polygon()), crs = 4326)
        }
        
        # Add service areas
        if (!is.null(service_areas)) {
          sa_df <- sf::st_drop_geometry(service_areas)
          sa_df$service_area <- sf::st_geometry(service_areas)
          geom_wide <- dplyr::left_join(geom_wide, sa_df, by = "bank_id")
        } else {
          geom_wide$service_area <- sf::st_sfc(lapply(seq_len(nrow(geom_wide)), 
                                                       function(x) sf::st_polygon()), crs = 4326)
        }
        
        # Convert to sf with centroid as active geometry (can switch as needed)
        geom_wide <- sf::st_as_sf(geom_wide, sf_column_name = "centroid")
        
        geom_wide
      }, error = function(e) {
        # Fallback: just return footprints if available
        if (!is.null(footprints_data)) footprints_data else NULL
      })
    }
  }
  
  result$.meta$timing$completed <- Sys.time()
  result$.meta$timing$duration_secs <- as.numeric(
    difftime(result$.meta$timing$completed, start_time, units = "secs")
  )

  if (!quietly) {
    cli::cli_h3("Summary")

    # Summary of what was fetched
    n_banks <- if (!is.null(result$banks)) nrow(result$banks) else 0
    n_transactions <- if (!is.null(result$transactions)) nrow(result$transactions) else 0
    n_contacts <- if (!is.null(result$.contacts)) nrow(result$.contacts) else 0
    n_geom <- if (!is.null(result$geometry)) nrow(result$geometry) else 0

    bullets <- c(
      "v" = "{n_banks} {resource_name}s (with contact/credit summaries)"
    )

    if (n_transactions > 0) {
      bullets <- c(bullets, "v" = "{n_transactions} transactions (unified)")
    }

    if (n_contacts > 0) {
      bullets <- c(bullets, "v" = "{n_contacts} detailed contacts")
    }

    if (n_geom > 0) {
      bullets <- c(bullets, "v" = "{n_geom} geometries (centroids + footprints + service areas)")
    }

    cli::cli_bullets(bullets)
    
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

  # Banks (now with summaries)
  if (!is.null(x$banks) && nrow(x$banks) > 0) {
    src <- x$.meta$sources$banks %||% "?"
    n_cols <- ncol(x$banks)
    cli::cli_alert_success("banks: {nrow(x$banks)} rows, {n_cols} columns (includes contact/credit summaries) [{src}]")
  } else {
    cli::cli_alert_warning("banks: none")
  }

  # Transactions (unified)
  if (!is.null(x$transactions) && nrow(x$transactions) > 0) {
    src <- x$.meta$sources$transactions %||% "?"
    n_cols <- ncol(x$transactions)
    cli::cli_alert_success("transactions: {nrow(x$transactions)} rows, {n_cols} columns (unified from watershed CSV + API + CSV ledger) [{src}]")
  } else {
    cli::cli_text("transactions: not requested (use include_detailed_transactions=TRUE)")
  }

  # Detailed contacts (optional)
  if (!is.null(x$.contacts) && nrow(x$.contacts) > 0) {
    src <- x$.meta$sources$contacts %||% "?"
    cli::cli_alert_success("detailed contacts: {nrow(x$.contacts)} rows [{src}]")
  } else {
    cli::cli_text("detailed contacts: not requested (use include_detailed_contacts=TRUE)")
  }

  # Geometry (unified)
  if (!is.null(x$geometry) && nrow(x$geometry) > 0) {
    src <- x$.meta$sources$geometry %||% "?"
    cli::cli_alert_success("geometry: {nrow(x$geometry)} features (centroids + footprints + service areas) [{src}]")
  } else {
    cli::cli_alert_warning("geometry: none")
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

# =============================================================================
# S3 Methods for ribits_data
# =============================================================================

#' Filter ribits_data objects
#'
#' @description
#' Apply dplyr::filter() to the banks dataframe within a ribits_data object.
#' This allows you to filter banks without accessing `$banks` directly.
#'
#' @param .data A ribits_data object
#' @param ... Filter expressions passed to dplyr::filter()
#'
#' @return A filtered ribits_data object
#' @export
#'
#' @examples
#' \dontrun{
#' ca_banks <- ribits(state = "CA")
#' large_banks <- ca_banks |> filter(total_acres > 100)
#' }
filter.ribits_data <- function(.data, ...) {
  .data$banks <- dplyr::filter(.data$banks, ...)

  # Also filter related data by bank_id if present
  if (!is.null(.data$banks) && nrow(.data$banks) > 0 && "bank_id" %in% names(.data$banks)) {
    remaining_ids <- .data$banks$bank_id

    if (!is.null(.data$transactions) && "bank_id" %in% names(.data$transactions)) {
      .data$transactions <- .data$transactions |>
        dplyr::filter(bank_id %in% remaining_ids)
    }

    if (!is.null(.data$.contacts) && "bank_id" %in% names(.data$.contacts)) {
      .data$.contacts <- .data$.contacts |>
        dplyr::filter(bank_id %in% remaining_ids)
    }

    if (!is.null(.data$geometry) && "bank_id" %in% names(.data$geometry)) {
      .data$geometry <- .data$geometry |>
        dplyr::filter(bank_id %in% remaining_ids)
    }
  }

  .data
}

#' Arrange ribits_data objects
#'
#' @description
#' Apply dplyr::arrange() to the banks dataframe within a ribits_data object.
#'
#' @param .data A ribits_data object
#' @param ... Arrange expressions passed to dplyr::arrange()
#'
#' @return An arranged ribits_data object
#' @export
#'
#' @examples
#' \dontrun{
#' ca_banks <- ribits(state = "CA")
#' sorted_banks <- ca_banks |> arrange(desc(total_acres))
#' }
arrange.ribits_data <- function(.data, ...) {
  .data$banks <- dplyr::arrange(.data$banks, ...)
  .data
}

#' Select columns from ribits_data objects
#'
#' @description
#' Apply dplyr::select() to the banks dataframe within a ribits_data object.
#'
#' @param .data A ribits_data object
#' @param ... Select expressions passed to dplyr::select()
#'
#' @return A ribits_data object with selected columns
#' @export
#'
#' @examples
#' \dontrun{
#' ca_banks <- ribits(state = "CA")
#' simplified <- ca_banks |> select(bank_id, bank_name, total_acres)
#' }
select.ribits_data <- function(.data, ...) {
  .data$banks <- dplyr::select(.data$banks, ...)
  .data
}

#' Mutate ribits_data objects
#'
#' @description
#' Apply dplyr::mutate() to the banks dataframe within a ribits_data object.
#'
#' @param .data A ribits_data object
#' @param ... Mutate expressions passed to dplyr::mutate()
#'
#' @return A ribits_data object with mutated columns
#' @export
#'
#' @examples
#' \dontrun{
#' ca_banks <- ribits(state = "CA")
#' with_hectares <- ca_banks |> mutate(total_hectares = total_acres * 0.404686)
#' }
mutate.ribits_data <- function(.data, ...) {
  .data$banks <- dplyr::mutate(.data$banks, ...)
  .data
}
