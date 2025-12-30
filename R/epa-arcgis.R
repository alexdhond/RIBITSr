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

    # Add progress bar for chunked queries
    pb <- cli::cli_progress_bar(
      paste("Fetching", layer, "geometry"),
      total = length(chunks),
      format = "{cli::pb_bar} {cli::pb_percent} | {cli::pb_current}/{cli::pb_total} chunks"
    )

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
      cli::cli_progress_update(id = pb)
      # Rate limiting now handled globally in rb_request_with_retry()
    }

    cli::cli_progress_done(id = pb)
    
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

  # Make request with retry logic
  # Spatial queries can be slow for large geometries
  resp <- rb_request_with_retry(
    req,
    description = paste("EPA ArcGIS:", layer),
    timeout = 60  # Spatial queries can take longer
  )

  if (is.null(resp)) {
    cli::cli_alert_danger("EPA ArcGIS query failed after retries")
    return(sf::st_sf(geometry = sf::st_sfc()))
  }

  if (httr2::resp_status(resp) != 200) {
    cli::cli_alert_danger("EPA ArcGIS query returned HTTP {httr2::resp_status(resp)}")
    return(sf::st_sf(geometry = sf::st_sfc()))
  }

  # Parse GeoJSON
  geojson <- httr2::resp_body_string(resp)

  tryCatch({
    result <- sf::st_read(geojson, quiet = TRUE)

    # Clean column names immediately to avoid case-insensitive lookups
    if (ncol(result) > 0) {
      result <- janitor::clean_names(result)
    }

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
#' Quickly checks which banks have centroids, footprints and/or service areas
#' available WITHOUT downloading the actual geometries. Queries all EPA centroid
#' layers (approved, pending, terminated) to provide complete coverage.
#'
#' @param bank_ids Optional vector of bank IDs to check. If NULL, checks all.
#' @param state Optional state filter (e.g., "CA")
#' @param quietly Suppress progress messages. Default FALSE.
#'
#' @return A tibble with bank_id, bank_status, epa_layer, and logical flags:
#'   has_centroid, has_footprint, has_service_area
#' @keywords internal
rb_spatial_availability <- function(bank_ids = NULL, state = NULL, quietly = FALSE) {

  # Build WHERE clause for state filter
  where <- "1=1"
  if (!is.null(state)) {
    where <- paste0("STATE_LIST LIKE '%", state, "%'")
  }

  # Get bank IDs from ALL centroid layers (approved, pending, terminated)
  if (!quietly) cli::cli_progress_step("Fetching bank list from all EPA layers...")

  all_banks <- list()

  # Layer 2: Approved banks
  approved <- tryCatch({
    epa <- rb_epa_query("approved_banks", where = where, bank_ids = bank_ids,
                         out_fields = "BANK_ID,BANK_STATUS", return_geometry = FALSE)
    if (!is.null(epa) && nrow(epa) > 0) {
      names(epa) <- tolower(names(epa))
      tibble::tibble(
        bank_id = as.integer(epa$bank_id),
        bank_status = epa$bank_status %||% "Approved",
        epa_layer = "approved_banks"
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(approved)) {
    all_banks$approved <- approved
    if (!quietly) cli::cli_alert_success("Approved: {nrow(approved)} banks")
  }

  # Layer 3: Pending banks
  pending <- tryCatch({
    epa <- rb_epa_query("pending_banks", where = where, bank_ids = bank_ids,
                         out_fields = "BANK_ID,BANK_STATUS", return_geometry = FALSE)
    if (!is.null(epa) && nrow(epa) > 0) {
      names(epa) <- tolower(names(epa))
      tibble::tibble(
        bank_id = as.integer(epa$bank_id),
        bank_status = epa$bank_status %||% "Pending",
        epa_layer = "pending_banks"
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(pending)) {
    all_banks$pending <- pending
    if (!quietly) cli::cli_alert_success("Pending: {nrow(pending)} banks")
  }

  # Layer 4: Terminated banks (includes Sold-Out, Suspended, Terminated, Withdrawn)
  terminated <- tryCatch({
    epa <- rb_epa_query("terminated_banks", where = where, bank_ids = bank_ids,
                         out_fields = "BANK_ID,BANK_STATUS", return_geometry = FALSE)
    if (!is.null(epa) && nrow(epa) > 0) {
      names(epa) <- tolower(names(epa))
      tibble::tibble(
        bank_id = as.integer(epa$bank_id),
        bank_status = epa$bank_status %||% "Terminated",
        epa_layer = "terminated_banks"
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(terminated)) {
    all_banks$terminated <- terminated
    if (!quietly) cli::cli_alert_success("Terminated/Sold-Out/etc: {nrow(terminated)} banks")
  }

  # Combine all banks
  if (length(all_banks) == 0) {
    if (!quietly) cli::cli_alert_warning("No banks found in any EPA layer")
    return(tibble::tibble(
      bank_id = integer(),
      bank_status = character(),
      epa_layer = character(),
      has_centroid = logical(),
      has_footprint = logical(),
      has_service_area = logical()
    ))
  }

  banks <- dplyr::bind_rows(all_banks)
  all_bank_ids <- banks$bank_id

  if (!quietly) cli::cli_alert_success("Total: {length(all_bank_ids)} banks with EPA centroids")

  # Get bank IDs that have footprints
  if (!quietly) cli::cli_progress_step("Checking footprint availability...")
  fp_banks <- tryCatch({
    fp <- rb_epa_query("bank_footprints", bank_ids = all_bank_ids,
                        out_fields = "BANK_ID", return_geometry = FALSE)
    if (!is.null(fp) && nrow(fp) > 0) {
      names(fp) <- tolower(names(fp))
      unique(as.integer(fp$bank_id))
    } else {
      integer()
    }
  }, error = function(e) integer())

  if (!quietly) cli::cli_alert_success("{length(fp_banks)} banks have footprints")

  # Get bank IDs that have service areas
  if (!quietly) cli::cli_progress_step("Checking service area availability...")
  sa_banks <- tryCatch({
    sa <- rb_epa_query("bank_service_areas", bank_ids = all_bank_ids,
                        out_fields = "BANK_ID", return_geometry = FALSE)
    if (!is.null(sa) && nrow(sa) > 0) {
      names(sa) <- tolower(names(sa))
      unique(as.integer(sa$bank_id))
    } else {
      integer()
    }
  }, error = function(e) integer())

  if (!quietly) cli::cli_alert_success("{length(sa_banks)} banks have service areas")

  # Build result with status information
  result <- banks |>
    dplyr::mutate(
      has_centroid = TRUE,  # All banks from centroid layers have centroids
      has_footprint = bank_id %in% fp_banks,
      has_service_area = bank_id %in% sa_banks
    )

  if (!quietly) {
    n_fp <- sum(result$has_footprint)
    n_sa <- sum(result$has_service_area)

    # Status breakdown
    status_summary <- result |>
      dplyr::group_by(bank_status) |>
      dplyr::summarise(
        n = dplyr::n(),
        fp_pct = round(100 * sum(has_footprint) / dplyr::n(), 0),
        sa_pct = round(100 * sum(has_service_area) / dplyr::n(), 0),
        .groups = "drop"
      )

    cli::cli_h3("Spatial Availability by Status")
    for (i in seq_len(nrow(status_summary))) {
      row <- status_summary[i, ]
      cli::cli_alert_info("{row$bank_status}: {row$n} banks ({row$fp_pct}% footprints, {row$sa_pct}% service areas)")
    }
  }

  result
}


# DELETED: rb_banks_summary() - use ribits() + rb_check() instead
# DELETED: rb_spatial_availability() - use rb_coverage() instead  
# DELETED: Deprecated EPA wrapper functions - use rb_epa() instead
