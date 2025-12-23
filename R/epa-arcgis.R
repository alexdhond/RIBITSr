# R/epa-arcgis.R
# Functions for querying EPA ArcGIS MapServer for RIBITS spatial data

# EPA ArcGIS MapServer base URL
EPA_ARCGIS_BASE <- "https://geopub.epa.gov/arcgis/rest/services/NEPAssist/RIBITS/MapServer"

# Layer IDs
EPA_LAYERS <- list(
  banks = 0,
  bank_status = 1,
  approved_banks = 2,
  pending_banks = 3,
  terminated_banks = 4,
  bank_footprints = 5,
  bank_service_areas = 6,
  ilf_programs = 7,
  ilf_service_areas = 8,
  ilf_approved = 9,
  ilf_pending = 10,
  ilf_terminated = 11,
  usace_districts = 12
)

#' Query EPA ArcGIS MapServer for RIBITS data
#'
#' Fetches spatial data from EPA's ArcGIS MapServer, which mirrors RIBITS
#' bank and service area geometries. This can be used as an alternative or
#' supplement to the RIBITS API for spatial data.
#'
#' @param layer Character. Layer to query. One of: "approved_banks",
#'   "pending_banks", "terminated_banks", "bank_footprints",
#'   "bank_service_areas", "ilf_service_areas", "usace_districts"
#' @param where SQL WHERE clause for filtering. Default "1=1" (all records).
#' @param bank_ids Optional vector of bank IDs to filter by.
#' @param out_fields Fields to return. Default "*" (all).
#' @param return_geometry Include geometry? Default TRUE.
#'
#' @return An sf object with the queried features
#' @keywords internal
#' @examples
#' \dontrun{
#' # Get all approved bank locations
#' banks <- rb_epa_query("approved_banks")
#'
#' # Get footprints for specific banks
#' footprints <- rb_epa_query("bank_footprints", bank_ids = c(17, 100, 500))
#'
#' # Get all service areas in California
#' ca_areas <- rb_epa_query("bank_service_areas", where = "STATE_LIST LIKE '%CA%'")
#' }
rb_epa_query <- function(layer,
                          where = "1=1",
                          bank_ids = NULL,
                          out_fields = "*",
                          return_geometry = TRUE) {

  # Validate layer
 if (!layer %in% names(EPA_LAYERS)) {
    cli::cli_abort(c(
      "Invalid layer: {layer}",
      "i" = "Valid layers: {paste(names(EPA_LAYERS), collapse = ', ')}"
    ))
  }

  layer_id <- EPA_LAYERS[[layer]]

  # Handle large bank_id lists by chunking (EPA has URL length limits)
  # If more than 50 bank_ids, chunk and combine results
  if (!is.null(bank_ids) && length(bank_ids) > 50) {
    chunks <- split(bank_ids, ceiling(seq_along(bank_ids) / 50))
    all_results <- list()
    
    for (i in seq_along(chunks)) {
      chunk_result <- rb_epa_query(
        layer = layer,
        where = where,
        bank_ids = chunks[[i]],
        out_fields = out_fields,
        return_geometry = return_geometry
      )
      if (!is.null(chunk_result) && nrow(chunk_result) > 0) {
        all_results[[i]] <- chunk_result
      }
      Sys.sleep(0.1)  # Be polite to the server
    }
    
    if (length(all_results) > 0) {
      combined <- do.call(rbind, all_results)
      return(combined)
    } else {
      return(sf::st_sf(geometry = sf::st_sfc()))
    }
  }

  # Build WHERE clause for normal-sized requests
  if (!is.null(bank_ids)) {
    bank_filter <- paste0("BANK_ID IN (", paste(bank_ids, collapse = ","), ")")
    if (where == "1=1") {
      where <- bank_filter
    } else {
      where <- paste0("(", where, ") AND ", bank_filter)
    }
  }

  # Build query URL - use httr2 for proper URL encoding
  base_url <- paste0(EPA_ARCGIS_BASE, "/", layer_id, "/query")
  
  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      where = where,
      outFields = out_fields,
      returnGeometry = tolower(as.character(return_geometry)),
      f = "geojson"
    )

  cli::cli_alert_info("Querying EPA ArcGIS: {layer}...")

  # Make request
  resp <- req |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200) {
    cli::cli_alert_danger("EPA ArcGIS query failed")
    return(sf::st_sf(geometry = sf::st_sfc()))
  }

  # Parse GeoJSON
  geojson <- httr2::resp_body_string(resp)

  tryCatch({
    result <- sf::st_read(geojson, quiet = TRUE)

    # Note: We preserve original column names here - clean_names() is applied at final output

    cli::cli_alert_success("Retrieved {nrow(result)} features from EPA ArcGIS")
    return(result)
  }, error = function(e) {
    cli::cli_alert_warning("Failed to parse GeoJSON: {e$message}")
    return(sf::st_sf(geometry = sf::st_sfc()))
  })
}


# =============================================================================
# SPATIAL AVAILABILITY FLAGS
# =============================================================================

#' Get spatial data availability flags for banks
#'
#' Quickly checks which banks have footprints and/or service areas available
#' WITHOUT downloading the actual geometries. Useful for filtering before
#' downloading large spatial datasets.
#'
#' @param bank_ids Optional vector of bank IDs to check. If NULL, checks all.
#' @param state Optional state filter (e.g., "CA")
#' @param quietly Suppress progress messages. Default FALSE.
#'
#' @return A tibble with bank_id and logical flags: has_centroid, has_footprint, has_service_area
#' @keywords internal
rb_spatial_availability <- function(bank_ids = NULL, state = NULL, quietly = FALSE) {
  
  # Build WHERE clause for banks layer
  where <- "1=1"
  if (!is.null(state)) {
    where <- paste0("STATE_LIST LIKE '%", state, "%'")
  }
  
  # Get bank IDs from approved_banks layer (this layer has point/centroid geometry)
  if (!quietly) cli::cli_progress_step("Fetching bank list...")
  
  banks <- rb_epa_query("approved_banks", where = where, bank_ids = bank_ids,
                         out_fields = "BANK_ID", return_geometry = FALSE)
  
  if (nrow(banks) == 0) {
    if (!quietly) cli::cli_alert_warning("No banks found")
    return(tibble::tibble(
      bank_id = integer(),
      has_centroid = logical(),
      has_footprint = logical(),
      has_service_area = logical()
    ))
  }
  
  # Get bank_id column (case-insensitive)
  id_col <- names(banks)[tolower(names(banks)) == "bank_id"][1]
  all_bank_ids <- banks[[id_col]]
  if (!quietly) cli::cli_alert_success("Found {length(all_bank_ids)} banks (all have centroids)")
  
  # Get bank IDs that have footprints (query by bank_id, not state)
  if (!quietly) cli::cli_progress_step("Checking footprint availability...")
  fp_banks <- tryCatch({
    fp <- rb_epa_query("bank_footprints", bank_ids = all_bank_ids,
                        out_fields = "BANK_ID", return_geometry = FALSE)
    fp_id_col <- names(fp)[tolower(names(fp)) == "bank_id"][1]
    unique(fp[[fp_id_col]])
  }, error = function(e) integer())
  
  if (!quietly) cli::cli_alert_success("{length(fp_banks)} banks have footprints")
  
  # Get bank IDs that have service areas (query by bank_id, not state)
  if (!quietly) cli::cli_progress_step("Checking service area availability...")
  sa_banks <- tryCatch({
    sa <- rb_epa_query("bank_service_areas", bank_ids = all_bank_ids,
                        out_fields = "BANK_ID", return_geometry = FALSE)
    sa_id_col <- names(sa)[tolower(names(sa)) == "bank_id"][1]
    unique(sa[[sa_id_col]])
  }, error = function(e) integer())
  
  if (!quietly) cli::cli_alert_success("{length(sa_banks)} banks have service areas")
  
  # Build result - all banks in approved_banks have centroids (point geometry)
  result <- tibble::tibble(
    bank_id = all_bank_ids,
    has_centroid = TRUE,  # All banks in approved_banks layer have point/centroid
    has_footprint = all_bank_ids %in% fp_banks,
    has_service_area = all_bank_ids %in% sa_banks
  )
  
  if (!quietly) {
    n_fp <- sum(result$has_footprint)
    n_sa <- sum(result$has_service_area)
    n_both <- sum(result$has_footprint & result$has_service_area)
    cli::cli_alert_info("Summary: {nrow(result)} centroids, {n_fp} footprints, {n_sa} service areas")
  }
  
  result
}


# DELETED: rb_banks_summary() - use ribits() + rb_check() instead
# DELETED: rb_spatial_availability() - use rb_coverage() instead  
# DELETED: Deprecated EPA wrapper functions - use rb_epa() instead
