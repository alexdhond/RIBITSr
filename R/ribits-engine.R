# R/ribits-engine.R
# Internal harmonization engine and orchestration logic
# Handles multi-source data fetching, merging, and harmonization

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
                    include_summaries = TRUE,
                    cache = TRUE,
                    use_checkpoints = TRUE,
                    quietly = FALSE) {

  start_time <- Sys.time()

  # Generate unique operation ID for checkpointing
  operation_id <- if (use_checkpoints) {
    paste0(
      type, "_",
      if (!is.null(state)) state else "all", "_",
      substr(digest::digest(list(bank_ids, district, what, sources)), 1, 8)
    )
  } else NULL

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

  resource_name <- switch(type,
    banks = "bank",
    ilf = "ILF program",
    umbrellas = "umbrella",
    wqt = "WQT project",
    "resource"
  )

  # Initialize result
  result <- structure(
    list(
      banks = NULL,           # Summary with contact/credit columns
      transactions = NULL,    # Unified transactions
      credits = NULL,         # Credit classifications
      notices = NULL,         # Public notices
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
        source_freshness = NULL,  # Will be populated with .collect_source_freshness()
        discrepancies = tibble::tibble(),
        timing = list(started = start_time)
      )
    ),
    class = c("ribits_data", "list")
  )

  include_spatial <- what %in% c("all", "spatial")

  # ==========================================================================
  # Step 1: Fetch Data from Sources
  # ==========================================================================
  if (!quietly) cli::cli_h3("Step 1: Fetching {resource_name} list")

  # 1. API
  banks_ribits_data <- if (use_api) {
      .fetch_ribits_api_data(type, bank_ids, state, district, include_spatial, quietly)
  } else {
      list(banks = NULL, contacts = NULL, geometry = NULL)
  }
  banks_ribits <- banks_ribits_data$banks
  
  # Store API geometry (merged later)
  # geometry is now a list with 'footprints' and 'service_areas' elements
  if (!is.null(banks_ribits_data$geometry) && is.list(banks_ribits_data$geometry)) {
      result$geometry <- banks_ribits_data$geometry
  }

  # Store API contacts (temporary, will be finalized later)
  if (!is.null(banks_ribits_data$contacts) && nrow(banks_ribits_data$contacts) > 0) {
    result$.contacts <- banks_ribits_data$contacts
    if (include_detailed_contacts) {
        result$.meta$sources$contacts <- "ribits_api"
    }
  }

  if (!is.null(banks_ribits) && nrow(banks_ribits) > 0) {
    if (!quietly) cli::cli_alert_success("Found {nrow(banks_ribits)} {resource_name}s from API")
  }

  # 2. EPA
  banks_epa <- if (use_epa) {
      .fetch_epa_data(type, bank_ids, state, district, quietly)
  } else NULL

  if (!is.null(banks_epa) && nrow(banks_epa) > 0) {
    if (!quietly) cli::cli_alert_success("Found {nrow(banks_epa)} {resource_name}s from EPA")
  }

  # 3. CSV
  banks_csv <- if (use_csv) {
      .fetch_csv_data(type, bank_ids, state, cache, quietly)
  } else NULL

  if (!is.null(banks_csv) && nrow(banks_csv) > 0) {
    if (!quietly) cli::cli_alert_success("Found {nrow(banks_csv)} {resource_name}s from CSV")
  }

  # ==========================================================================
  # Step 2: Merge Sources
  # ==========================================================================

  available_sources <- list(
    ribits_api = banks_ribits,
    epa_arcgis = banks_epa,
    ribits_csv = banks_csv
  )
  available_sources <- purrr::keep(available_sources, ~ !is.null(.) && nrow(.) > 0)

  if (length(available_sources) > 0) {
    # Detect discrepancies
    if (length(available_sources) > 1) {
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
    }

    # Merge all sources preserving columns (CSV priority, then API, then EPA)
    result$banks <- .merge_multiple_sources(
      available_sources,
      by = "bank_id",
      priority_order = c("ribits_csv", "ribits_api", "epa_arcgis")
    )
    result$.meta$sources$banks <- paste(names(available_sources), collapse = " + ")

    # Apply auto-harmonization if enabled
    config <- .get_discrepancy_config()
    if (config$auto_harmonize && nrow(result$.meta$discrepancies) > 0) {
      if (!quietly) cli::cli_alert_info("Applying auto-harmonization rules...")

      harmonization_result <- .auto_harmonize(
        result$.meta$discrepancies,
        result$banks,
        config
      )

      # Update with harmonized data
      result$banks <- harmonization_result$data
      result$.meta$discrepancies <- harmonization_result$discrepancies_remaining
      result$.meta$harmonization_resolutions <- harmonization_result$resolutions

      # Report results
      if (!quietly && nrow(harmonization_result$resolutions) > 0) {
        n_resolved <- nrow(harmonization_result$resolutions)
        n_remaining <- nrow(harmonization_result$discrepancies_remaining)
        cli::cli_alert_success("Auto-harmonized {n_resolved} discrepancies")
        if (n_remaining > 0) {
          cli::cli_alert_warning("{n_remaining} discrepancies need manual review")
        }
      }
    }

    if (!quietly && length(available_sources) > 1) {
      cli::cli_alert_info("Merged {length(available_sources)} sources: {result$.meta$sources$banks}")
      cli::cli_alert_info("Result has {ncol(result$banks)} columns (preserving all from each source)")
    }
  } else {
     # If no data found at all
     if (!quietly) cli::cli_alert_warning("No banks found matching criteria")
     return(result)
  }

  # Get bank IDs for subsequent queries
  query_ids <- if (!is.null(result$banks)) .col_get(result$banks, "bank_id", error_if_missing = FALSE) else NULL

  if (is.null(query_ids) || length(query_ids) == 0) {
    if (!quietly) cli::cli_alert_warning("No valid bank IDs found after merge")
    return(result)
  }

  if (!quietly) cli::cli_alert_success("Processing {length(query_ids)} banks...")

  # Step 2b: API Hydration (REMOVED - Optimised into Step 1)
  # Original hydration logic is now handled by .fetch_ribits_api_data
  # which fetches detailed data including spatial attributes in the first pass.

  # ==========================================================================
  # Step 3: Summaries (Contacts, Credits, etc.)
  # ==========================================================================
  if (!quietly) cli::cli_h3("Step 2: Creating Summaries")

  # Contacts Summary
  contacts_detailed <- result$.contacts
  contact_summary <- .summarize_contacts(contacts_detailed)

  # Credit Summary
  credit_class_detailed <- NULL
  if (use_csv) {
    credit_class_detailed <- .safe_fetch(
      fn = function() .fetch_csv_with_bank_id("credit_classification", query_ids, cache, quietly),
      description = "Credit classification",
      quietly = quietly
    )
    if (!quietly && !is.null(credit_class_detailed) && nrow(credit_class_detailed) > 0) {
      cli::cli_alert_success("{nrow(credit_class_detailed)} credit classifications")
    }
  }
  credit_summary <- .summarize_credits(credit_class_detailed)

  # Pivot wide for Master Summary (StreamCat-style metrics)
  if (!is.null(credit_class_detailed) && nrow(credit_class_detailed) > 0) {
    credit_wide <- .pivot_credits_wide(credit_class_detailed)
    if (nrow(credit_wide) > 0) {
      credit_summary <- dplyr::left_join(credit_summary, credit_wide, by = "bank_id")
      if (!quietly) cli::cli_alert_success("Added wide credit metrics")
    }
  }

  # Releases & Notices
  credit_releases_summary <- NULL
  public_notices_summary <- NULL

  if (include_summaries && use_csv) {
    # Credit releases
    cr_detailed <- .safe_fetch(
      fn = function() .fetch_csv_with_bank_id("credit_releases", query_ids, cache, quietly),
      description = "Credit releases",
      quietly = TRUE
    )
    if (!is.null(cr_detailed)) credit_releases_summary <- .summarize_credit_releases(cr_detailed)

    # Public notices
    pn_detailed <- .safe_fetch(
      fn = function() .fetch_csv_with_bank_id("public_notices", query_ids, cache, quietly),
      description = "Public notices",
      quietly = TRUE
    )
    if (!is.null(pn_detailed)) public_notices_summary <- .summarize_public_notices(pn_detailed)
  }

  # Merge summaries into banks
  if (!is.null(result$banks) && nrow(result$banks) > 0) {
    if (!is.null(contact_summary) && nrow(contact_summary) > 0) {
      result$banks <- dplyr::left_join(result$banks, contact_summary, by = "bank_id")
      if (!quietly) cli::cli_alert_success("Added contact summaries")
    }
    if (!is.null(credit_summary) && nrow(credit_summary) > 0) {
      result$banks <- dplyr::left_join(result$banks, credit_summary, by = "bank_id")
      if (!quietly) cli::cli_alert_success("Added credit summaries")
    }
    if (!is.null(credit_releases_summary) && nrow(credit_releases_summary) > 0) {
      result$banks <- dplyr::left_join(result$banks, credit_releases_summary, by = "bank_id")
      if (!quietly) cli::cli_alert_success("Added anticipated release summaries")
    }
    if (!is.null(public_notices_summary) && nrow(public_notices_summary) > 0) {
      result$banks <- dplyr::left_join(result$banks, public_notices_summary, by = "bank_id")
      if (!quietly) cli::cli_alert_success("Added public notice summaries")
    }
  }

  # Finalize contacts
  if (!include_detailed_contacts) {
      result$.contacts <- NULL
  } else if (!is.null(result$.contacts)) {
      result$.contacts <- .finalize_df(result$.contacts, type = "contacts")
  }

  # ==========================================================================
  # Step 4: Transactions
  # ==========================================================================
  if (include_detailed_transactions) {
    if (!quietly) cli::cli_h3("Step 3: Fetching unified transaction data")

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

      # Create transaction summary and merge into banks
      if (include_summaries && !is.null(result$banks) && nrow(result$banks) > 0) {
        transaction_summary <- .summarize_transactions(result$transactions)
        if (!is.null(transaction_summary) && nrow(transaction_summary) > 0) {
          result$banks <- dplyr::left_join(result$banks, transaction_summary, by = "bank_id")
          if (!quietly) cli::cli_alert_success("Added transaction summaries")
        }
      }

      result$transactions <- .finalize_df(result$transactions, type = "transactions")
    }
  }

  # Save checkpoint before expensive spatial fetch (most valuable checkpoint)
  if (use_checkpoints && include_spatial) {
    .save_step_checkpoint(operation_id, result, step = 4, description = "pre_spatial")
  }
  
  # ==========================================================================
  # Step 4b: Credits and Notices (always fetch for banks)
  # ==========================================================================
  if (use_csv && type == "banks") {
    cache_dir <- if (cache) file.path(tempdir(), "ribits_cache") else tempdir()
    
    # Fetch credit classifications
    if (!quietly) cli::cli_progress_step("Fetching credit classifications...")
    result$credits <- tryCatch({
      credit_data <- .fetch_csv_with_bank_id("credit_classification", query_ids, cache, quietly)
      if (!is.null(credit_data) && nrow(credit_data) > 0) {
        # Normalize and return as-is (long format, one row per bank x classification)
        credit_data <- .normalize_columns(credit_data)
        if (!quietly) cli::cli_alert_success("Retrieved {nrow(credit_data)} credit classification records")
        credit_data
      } else NULL
    }, error = function(e) {
      if (!quietly) cli::cli_alert_warning("Credit classifications fetch failed: {e$message}")
      NULL
    })
    
    # Fetch public notices
    if (!quietly) cli::cli_progress_step("Fetching public notices...")
    result$notices <- tryCatch({
      notice_data <- .fetch_csv_with_bank_id("public_notices", query_ids, cache, quietly)
      if (!is.null(notice_data) && nrow(notice_data) > 0) {
        notice_data <- .normalize_columns(notice_data)
        if (!quietly) cli::cli_alert_success("Retrieved {nrow(notice_data)} public notice records")
        
        # Add notice summary to banks if include_summaries
        if (include_summaries && !is.null(result$banks) && nrow(result$banks) > 0) {
          notice_summary <- .summarize_public_notices(notice_data)
          if (!is.null(notice_summary) && nrow(notice_summary) > 0) {
            result$banks <- dplyr::left_join(result$banks, notice_summary, by = "bank_id")
            if (!quietly) cli::cli_alert_success("Added notice summaries")
          }
        }
        
        notice_data
      } else NULL
    }, error = function(e) {
      if (!quietly) cli::cli_alert_warning("Public notices fetch failed: {e$message}")
      NULL
    })
    
    result$.meta$sources$credits <- if (!is.null(result$credits)) "credit_classification_csv" else NULL
    result$.meta$sources$notices <- if (!is.null(result$notices)) "public_notices_csv" else NULL
  }

  # ==========================================================================
  # Step 5: Spatial Data
  # ==========================================================================
  if (include_spatial) {
    spatial_res <- .fetch_harmonized_spatial(
        query_ids, use_epa, use_api, 
        geometry_api = result$geometry, # Pass pre-fetched API geometry
        quietly
    )

    if (nrow(spatial_res$discrepancies) > 0) {
      result$.meta$discrepancies <- dplyr::bind_rows(result$.meta$discrepancies, spatial_res$discrepancies)
    }

    footprints <- spatial_res$footprints
    service_areas <- spatial_res$service_areas

    # Build wide geometry (one row per bank with centroid, footprint, service_area)
    if (!is.null(result$banks) && nrow(result$banks) > 0) {
      result$geometry <- .build_wide_geometry(
        banks = result$banks,
        footprints = footprints,
        service_areas = service_areas,
        quietly = quietly
      )
      result$.meta$sources$geometry <- "epa_arcgis + ribits_api"

      if (!quietly) {
        n_fp <- if (!is.null(footprints)) nrow(footprints) else 0
        n_sa <- if (!is.null(service_areas)) nrow(service_areas) else 0
        cli::cli_alert_success("{n_fp} footprints, {n_sa} service areas")
      }
    }
  }

  # ==========================================================================
  # Finalize - Clean, dedupe, and order columns for all data components
  # ==========================================================================

  if (!is.null(result$banks) && nrow(result$banks) > 0) {
    # Ensure lat/lon columns exist by extracting from geometry if needed
    if (!is.null(result$geometry) && "centroid" %in% names(result$geometry)) {
        # The geometry object has columns: bank_id, bank_name, bank_status, centroid, footprint, service_area
        # Extract coordinates from the centroid column (which is already a geometry column)

        # First, set centroid as the active geometry to extract coordinates
        geom_with_centroids <- result$geometry |>
          dplyr::select(dplyr::any_of("bank_id"), "centroid")

        # Extract coordinates from the centroid geometries
        # Use st_set_geometry to make centroid the active geometry column
        if (nrow(geom_with_centroids) > 0) {
          geom_with_centroids <- sf::st_set_geometry(geom_with_centroids, "centroid")
          coords_matrix <- sf::st_coordinates(geom_with_centroids)

          coords <- geom_with_centroids |>
            sf::st_drop_geometry() |>
            dplyr::mutate(
              longitude = coords_matrix[,1],
              latitude = coords_matrix[,2]
            ) |>
            dplyr::select("bank_id", "latitude", "longitude")
        } else {
          coords <- tibble::tibble(bank_id = integer(), latitude = numeric(), longitude = numeric())
        }
        
        # Merge into banks, filling gaps or overwriting if missing
        # We use explicit suffixes to handle both cases (column exists or not)
        result$banks <- result$banks |>
          dplyr::left_join(coords, by = "bank_id", suffix = c(".old", ".new"))
          
        # Coalesce latitude
        if ("latitude.new" %in% names(result$banks)) {
            if ("latitude.old" %in% names(result$banks)) {
                result$banks$latitude <- dplyr::coalesce(as.numeric(result$banks$latitude.new), as.numeric(result$banks$latitude.old))
            } else {
                result$banks$latitude <- as.numeric(result$banks$latitude.new)
            }
        }
        
        # Coalesce longitude
        if ("longitude.new" %in% names(result$banks)) {
            if ("longitude.old" %in% names(result$banks)) {
                result$banks$longitude <- dplyr::coalesce(as.numeric(result$banks$longitude.new), as.numeric(result$banks$longitude.old))
            } else {
                result$banks$longitude <- as.numeric(result$banks$longitude.new)
            }
        }
        
        # Cleanup
        result$banks <- result$banks |>
          dplyr::select(-dplyr::matches("\\.old$|\\.new$"))
    }

    result$banks <- .finalize_df(result$banks, type = "banks")
  }
  if (!is.null(result$transactions) && nrow(result$transactions) > 0) {
    result$transactions <- .finalize_df(result$transactions, type = "transactions")
  }
  if (!is.null(result$.contacts) && nrow(result$.contacts) > 0) {
    result$.contacts <- .finalize_df(result$.contacts, type = "contacts")
  }

  result$.meta$timing$completed <- Sys.time()
  result$.meta$timing$duration_secs <- as.numeric(
    difftime(result$.meta$timing$completed, start_time, units = "secs")
  )
  
  # Collect source freshness metadata (internal, for debugging data staleness)
  result$.meta$source_freshness <- tryCatch(
    .collect_source_freshness(sources = sources),
    error = function(e) NULL
  )

  if (!quietly) {
    cli::cli_h3("Summary")

    # Summary of what was fetched
    n_banks <- if (!is.null(result$banks)) nrow(result$banks) else 0
    n_transactions <- if (!is.null(result$transactions)) nrow(result$transactions) else 0
    n_credits <- if (!is.null(result$credits)) nrow(result$credits) else 0
    n_notices <- if (!is.null(result$notices)) nrow(result$notices) else 0
    n_contacts <- if (!is.null(result$.contacts)) nrow(result$.contacts) else 0
    n_geom <- if (!is.null(result$geometry)) nrow(result$geometry) else 0

    bullets <- c(
      "v" = "{n_banks} {resource_name}s (with contact/credit summaries)"
    )

    if (n_transactions > 0) {
      bullets <- c(bullets, "v" = "{n_transactions} transactions")
    }
    
    if (n_credits > 0) {
      bullets <- c(bullets, "v" = "{n_credits} credit classifications")
    }
    
    if (n_notices > 0) {
      bullets <- c(bullets, "v" = "{n_notices} public notices")
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
      cli::cli_h2("Data Quality")
      cli::cli_alert_warning("{n_disc} discrepancies between sources")
      cli::cli_text("Use {.code $discrepancies()} to view details")
    }

    cli::cli_alert_success("Completed in {round(result$.meta$timing$duration_secs, 1)}s")
  }

  # Clear checkpoint on successful completion
  if (use_checkpoints && !is.null(operation_id)) {
    .clear_step_checkpoint(operation_id)
  }

  result
}


# =============================================================================
# Helper Functions
# =============================================================================

#' Parse GeoJSON centroids from bank data (vectorized)
#'
#' Converts GeoJSON strings in bank_location_centroid column to sf geometries.
#' Uses purrr for vectorized parsing instead of row-by-row loops.
#'
#' @param banks Data frame with bank_id and bank_location_centroid columns
#' @return Tibble with bank_id and centroid geometry, or NULL if no valid centroids
#' @keywords internal
#' @noRd
.parse_geojson_centroids <- function(banks) {
  if (is.null(banks) || nrow(banks) == 0) {
    return(NULL)
  }

  # Check for required columns
  if (!"bank_location_centroid" %in% names(banks) || !"bank_id" %in% names(banks)) {
    return(NULL)
  }

  # Parse each GeoJSON string to geometry using purrr::map2
  parsed <- purrr::map2(
    banks$bank_location_centroid,
    banks$bank_id,
    function(geojson, bid) {
      if (is.na(geojson) || nchar(geojson) <= 10) {
        return(NULL)
      }
      tryCatch({
        pt <- sf::st_read(geojson, quiet = TRUE, drivers = "GeoJSON")
        if (nrow(pt) > 0) {
          list(bank_id = bid, geom = sf::st_geometry(pt)[[1]])
        } else {
          NULL
        }
      }, error = function(e) NULL)
    }
  )

  # Filter out NULLs and build result

  valid <- purrr::compact(parsed)

  if (length(valid) == 0) {
    return(NULL)
  }

  tibble::tibble(
    bank_id = purrr::map_int(valid, "bank_id"),
    centroid = sf::st_sfc(purrr::map(valid, "geom"), crs = 4326)
  )
}


#' Build Wide Geometry Layer
#'
#' Constructs a wide-format sf object with one row per bank containing:
#' bank_id, bank_name, bank_status, centroid, footprint, service_area
#'
#' @param banks Data frame with bank_id, bank_name, bank_status, and bank_location_centroid
#' @param footprints sf object with bank_id and geometry (can be NULL)
#' @param service_areas sf object with bank_id and geometry (can be NULL)
#' @param quietly If TRUE, suppress warning messages
#' @return sf object with wide geometry format, or NULL if no valid data
#' @keywords internal
#' @noRd
.build_wide_geometry <- function(banks, footprints, service_areas, quietly = FALSE) {
  if (is.null(banks) || nrow(banks) == 0) {
    return(NULL)
  }

  # Build base with identifying columns

  geom_base <- banks |>
    dplyr::select(dplyr::any_of(c("bank_id", "bank_name", "bank_status"))) |>
    dplyr::distinct()

  # 1. Parse centroids from GeoJSON (vectorized)
  centroids <- .parse_geojson_centroids(banks)

  # 2. Union footprints by bank_id (with error handling)
  fp_wide <- NULL
  if (!is.null(footprints) && nrow(footprints) > 0 && inherits(footprints, "sf")) {
    if ("bank_id" %in% names(footprints)) {
      fp_wide <- tryCatch({
        footprints |>
          dplyr::group_by(.data$bank_id) |>
          dplyr::summarise(footprint = sf::st_union(geometry), .groups = "drop") |>
          sf::st_as_sf()
      }, error = function(e) {
        if (!quietly) {
          cli::cli_alert_warning("Failed to process footprints: {conditionMessage(e)}")
        }
        NULL
      })
    }
  }

  # 3. Union service areas by bank_id (with error handling)
  sa_wide <- NULL
  if (!is.null(service_areas) && nrow(service_areas) > 0 && inherits(service_areas, "sf")) {
    if ("bank_id" %in% names(service_areas)) {
      sa_wide <- tryCatch({
        service_areas |>
          dplyr::group_by(.data$bank_id) |>
          dplyr::summarise(service_area = sf::st_union(geometry), .groups = "drop") |>
          sf::st_as_sf()
      }, error = function(e) {
        if (!quietly) {
          cli::cli_alert_warning("Failed to process service areas: {conditionMessage(e)}")
        }
        NULL
      })
    }
  }

  # 4. Join all geometries to base (wide format)
  tryCatch({
    geom_wide <- geom_base

    # Add centroids
    if (!is.null(centroids)) {
      geom_wide <- dplyr::left_join(geom_wide, centroids, by = "bank_id")
    } else {
      geom_wide$centroid <- sf::st_sfc(
        purrr::map(seq_len(nrow(geom_wide)), ~ sf::st_point()),
        crs = 4326
      )
    }

    # Add footprints
    if (!is.null(fp_wide)) {
      fp_df <- sf::st_drop_geometry(fp_wide)
      fp_df$footprint <- sf::st_geometry(fp_wide)
      geom_wide <- dplyr::left_join(geom_wide, fp_df, by = "bank_id")
    } else {
      geom_wide$footprint <- sf::st_sfc(
        purrr::map(seq_len(nrow(geom_wide)), ~ sf::st_polygon()),
        crs = 4326
      )
    }

    # Add service areas
    if (!is.null(sa_wide)) {
      sa_df <- sf::st_drop_geometry(sa_wide)
      sa_df$service_area <- sf::st_geometry(sa_wide)
      geom_wide <- dplyr::left_join(geom_wide, sa_df, by = "bank_id")
    } else {
      geom_wide$service_area <- sf::st_sfc(
        purrr::map(seq_len(nrow(geom_wide)), ~ sf::st_polygon()),
        crs = 4326
      )
    }

    # Convert to sf with centroid as active geometry
    sf::st_as_sf(geom_wide, sf_column_name = "centroid")

  }, error = function(e) {
    # Fallback: return footprints if available
    if (!is.null(footprints)) footprints else NULL
  })
}
