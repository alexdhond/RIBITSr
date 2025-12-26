# R/ribits.R
# Main user-facing functions with automatic harmonization
# Users should primarily use these - all complexity is hidden

#' Get RIBITS data (fully harmonized)
#'
#' The main function for getting mitigation bank, ILF program, or umbrella data.
#' Automatically fetches and harmonizes data from all available sources:
#' - RIBITS API (real-time data)
#' - EPA ArcGIS MapServer (spatial data)
#' - RIBITS CSV reports (official records, fetched directly)
#'
#' No manual downloads required - everything is automatic!
#'
#' @param type Type of data to fetch:
#'   - "banks" (default): Mitigation banks
#'   - "ilf": In-Lieu Fee programs
#'   - "umbrellas": Umbrella mitigation instruments
#' @param state State filter (e.g., "CA", "OR", "TX")
#' @param district USACE district filter (e.g., "Portland", "Sacramento")
#' @param ids Optional vector of specific IDs to retrieve (bank_ids, program_ids, etc.)
#' @param transactions Level of transaction data to include (banks only):
#'   - "none": No transaction data (fastest)
#'   - "basic" (default): API ledger only (~20 columns, fast, real-time data)
#'   - "comprehensive": Full 3-source harmonization (~85 columns, slower but most complete)
#'
#'   The "comprehensive" option merges data from watershed CSV, API ledger, and CSV reports.
#'   Use "basic" for quick queries, "comprehensive" for data analysis requiring maximum detail.
#'
#' @param spatial Include spatial data (footprints and service area geometries)? Default TRUE.
#'   Set to FALSE for faster queries when you only need bank attributes.
#'
#' @param sources Which data sources to use. Default: c("api", "epa", "csv") (all sources).
#'   - "api" = RIBITS API only (fastest, real-time, ~30 seconds)
#'   - "epa" = EPA ArcGIS only (spatial data)
#'   - "csv" = Direct CSV downloads (official records, slow)
#'   - c("api", "epa", "csv") = All sources (recommended for accuracy, ~60-90 seconds)
#'
#'   Using all sources provides the most complete and accurate data. If a source fails,
#'   the package automatically falls back to other sources.
#'
#' @param cache Cache downloaded CSV files to temp directory? Default TRUE.
#'   Significantly speeds up repeated queries. Cache is cleared when R session ends.
#'
#' @param include_summaries Include comprehensive summaries in banks dataframe? Default TRUE.
#'   When TRUE, adds summary metrics from:
#'   - Transactions (volume, temporal patterns, geography, permittees)
#'   - Anticipated credit releases (upcoming releases in next 5 years)
#'   - Public notices (document counts and recency)
#'   Set to FALSE for minimal banks dataframe with just core attributes.
#'
#' @param quietly Suppress progress messages? Default FALSE.
#'
#' @return A `ribits_data` object containing:
#'   \item{banks/programs/umbrellas}{Summary data (tibble)}
#'   \item{transactions}{Transaction/credit tracking data (tibble, banks only)}
#'   \item{geometry}{Spatial data including footprints and service areas (sf object)}
#'   \item{.meta}{Metadata including sources used and any discrepancies}
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all California banks (fully harmonized, automatic!)
#' ca <- ribits(state = "CA")
#'
#' # Access the data
#' ca$banks           # Summary data
#' ca$transactions    # Transaction history
#' ca$geometry        # Spatial polygons
#'
#' # Get comprehensive transaction data (3-source harmonization, ~85 columns)
#' ca_full <- ribits(state = "CA", transactions = "comprehensive")
#'
#' # Get ILF programs
#' ilf <- ribits(type = "ilf", state = "TX")
#'
#' # Get umbrella instruments
#' umb <- ribits(type = "umbrellas", state = "FL")
#'
#' # Get specific banks by ID
#' my_banks <- ribits(ids = c(17, 100, 345))
#'
#' # Just summary data, no spatial or transactions (fastest)
#' summary <- ribits(state = "OR", spatial = FALSE, transactions = "none")
#'
#' # Only use API source (fastest, real-time)
#' api_only <- ribits(state = "TX", sources = "api")
#' }
ribits <- function(type = "banks",
                   state = NULL,
                   district = NULL,
                   ids = NULL,
                   transactions = c("basic", "comprehensive", "none"),
                   spatial = TRUE,
                   sources = c("api", "epa", "csv"),
                   cache = TRUE,
                   include_summaries = TRUE,
                   quietly = FALSE) {

  type <- match.arg(type, c("banks", "ilf", "umbrellas"))
  transactions <- match.arg(transactions)

  # Determine what data to fetch
  # Transactions only apply to banks
  include_ledger <- transactions != "none" && type == "banks"
  include_comprehensive <- transactions == "comprehensive" && type == "banks"

  what <- if (include_ledger && spatial) {
    "all"
  } else if (spatial) {
    "spatial"
  } else if (include_ledger) {
    "ledger"
  } else {
    "banks"
  }

  # Call the auto-harmonization engine
  .ribits_engine(
    bank_ids = ids,
    state = state,
    district = district,
    what = what,
    type = type,
    sources = sources,
    cache = cache,
    quietly = quietly,
    include_detailed_transactions = include_comprehensive,
    include_summaries = include_summaries
  )
}

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

  include_spatial <- what %in% c("all", "spatial")

  # ==========================================================================
  # Step 1: Fetch Data from Sources
  # ==========================================================================
  if (!quietly) cli::cli_h3("Step 1: Fetching {resource_name} list")

  # 1. API
  banks_ribits_data <- if (use_api) {
      .fetch_ribits_api_data(type, bank_ids, state, district, quietly)
  } else {
      list(banks = NULL, contacts = NULL)
  }
  banks_ribits <- banks_ribits_data$banks
  
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
  query_ids <- if (!is.null(result$banks)) .col_get(result$banks, "bank_id") else NULL

  if (is.null(query_ids) || length(query_ids) == 0) {
    if (!quietly) cli::cli_alert_warning("No valid bank IDs found after merge")
    return(result)
  }

  if (!quietly) cli::cli_alert_success("Processing {length(query_ids)} banks...")

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

  # ==========================================================================
  # Step 5: Spatial Data
  # ==========================================================================
  if (include_spatial) {
    spatial_res <- .fetch_harmonized_spatial(query_ids, use_epa, use_api, quietly)
    
    if (nrow(spatial_res$discrepancies) > 0) {
      result$.meta$discrepancies <- dplyr::bind_rows(result$.meta$discrepancies, spatial_res$discrepancies)
    }
    
    footprints <- spatial_res$footprints
    service_areas <- spatial_res$service_areas

    # Construct wide geometry (one row per bank)
    if (!is.null(result$banks) && nrow(result$banks) > 0) {
      geom_base <- result$banks |>
        dplyr::select(dplyr::any_of(c("bank_id", "bank_name", "bank_status"))) |>
        dplyr::distinct()
      
      # 1. Centroids
      centroids <- NULL
      if (.col_exists(result$banks, "bank_location_centroid")) {
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
      
      # 2. Footprints
      fp_wide <- NULL
      if (!is.null(footprints) && nrow(footprints) > 0 && inherits(footprints, "sf")) {
          fp_wide <- footprints |>
            dplyr::group_by(bank_id) |>
            dplyr::summarise(footprint = sf::st_union(geometry), .groups = "drop") |>
            sf::st_as_sf()
      }

      # 3. Service Areas
      sa_wide <- NULL
      if (!is.null(service_areas) && nrow(service_areas) > 0 && inherits(service_areas, "sf")) {
          sa_wide <- service_areas |>
            dplyr::group_by(bank_id) |>
            dplyr::summarise(service_area = sf::st_union(geometry), .groups = "drop") |>
            sf::st_as_sf()
      }
      
      # Join all
      geom_wide <- geom_base
      
      if (!is.null(centroids)) {
        geom_wide <- dplyr::left_join(geom_wide, centroids, by = "bank_id")
      } else {
        geom_wide$centroid <- sf::st_sfc(lapply(seq_len(nrow(geom_wide)), function(x) sf::st_point()), crs = 4326)
      }
      
      if (!is.null(fp_wide)) {
        df_fp <- sf::st_drop_geometry(fp_wide)
        df_fp$footprint <- sf::st_geometry(fp_wide)
        geom_wide <- dplyr::left_join(geom_wide, df_fp, by = "bank_id")
      } else {
         geom_wide$footprint <- sf::st_sfc(lapply(seq_len(nrow(geom_wide)), function(x) sf::st_polygon()), crs = 4326)
      }
      
      if (!is.null(sa_wide)) {
        df_sa <- sf::st_drop_geometry(sa_wide)
        df_sa$service_area <- sf::st_geometry(sa_wide)
        geom_wide <- dplyr::left_join(geom_wide, df_sa, by = "bank_id")
      } else {
         geom_wide$service_area <- sf::st_sfc(lapply(seq_len(nrow(geom_wide)), function(x) sf::st_polygon()), crs = 4326)
      }
      
      result$geometry <- sf::st_as_sf(geom_wide, sf_column_name = "centroid")
      result$.meta$sources$geometry <- "epa_arcgis + ribits_api"
      
      if (!quietly) {
        n_fp <- if (!is.null(footprints)) nrow(footprints) else 0
        n_sa <- if (!is.null(service_areas)) nrow(service_areas) else 0
        cli::cli_alert_success("{n_fp} footprints, {n_sa} service areas")
      }
    }
  } else {
     # Initialize to NULL if not fetching spatial
     # (Already done in struct init)
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
      fp_wide <- NULL
      if (!is.null(footprints) && nrow(footprints) > 0 &&
          inherits(footprints, "sf")) {
        # Names already cleaned by rb_epa_query()
        if ("bank_id" %in% names(footprints)) {
          fp_wide <- footprints |>
            dplyr::group_by(bank_id) |>
            dplyr::summarise(footprint = sf::st_union(geometry), .groups = "drop") |>
            sf::st_as_sf()
        }
      }

      # 3. Extract service areas (one per bank_id)
      sa_wide <- NULL
      if (!is.null(service_areas) && nrow(service_areas) > 0 &&
          inherits(service_areas, "sf")) {
        # Names already cleaned by rb_epa_query()
        if ("bank_id" %in% names(service_areas)) {
          sa_wide <- service_areas |>
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
        if (!is.null(fp_wide)) {
          fp_df <- sf::st_drop_geometry(fp_wide)
          fp_df$footprint <- sf::st_geometry(fp_wide)
          geom_wide <- dplyr::left_join(geom_wide, fp_df, by = "bank_id")
        } else {
          geom_wide$footprint <- sf::st_sfc(lapply(seq_len(nrow(geom_wide)), 
                                                    function(x) sf::st_polygon()), crs = 4326)
        }
        
        # Add service areas
        if (!is.null(sa_wide)) {
          sa_df <- sf::st_drop_geometry(sa_wide)
          sa_df$service_area <- sf::st_geometry(sa_wide)
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
        if (!is.null(footprints)) footprints else NULL
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
      cli::cli_h2("Data Quality")
      cli::cli_alert_warning("{n_disc} discrepancies between sources")
      cli::cli_text("Use {.code $discrepancies()} to view details")
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
#' @exportS3Method dplyr::filter
#' @importFrom dplyr filter
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
#' @exportS3Method dplyr::arrange
#' @importFrom dplyr arrange
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
#' @exportS3Method dplyr::select
#' @importFrom dplyr select
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
#' @exportS3Method dplyr::mutate
#' @importFrom dplyr mutate
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