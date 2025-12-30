# R/spatial-harmonize.R
# Unified spatial data access layer combining EPA ArcGIS and RIBITS API sources

#' Get bank geometry from best available source
#'
#' Retrieves spatial data for mitigation banks from EPA ArcGIS (preferred for
#' approved banks) or RIBITS API (for non-approved banks or when EPA unavailable).
#' This provides a unified interface that automatically selects the best source
#' based on bank status and data availability.
#'
#' @param bank_ids Vector of bank IDs to fetch geometry for
#' @param geometry_type Type of geometry to retrieve:
#'   - "centroid": Bank location point (fastest)
#'   - "footprint": Bank footprint polygon
#'   - "service_area": Bank service area polygon
#'   - "all": All available geometry types (returns list-columns)
#' @param prefer_epa Logical. If TRUE (default), try EPA ArcGIS first for
#'   approved banks. Set to FALSE to always use API.
#'
#' @return An sf object with bank geometries and metadata:
#' \describe{
#'   \item{bank_id}{Bank identifier}
#'   \item{bank_name}{Bank name (if available)}
#'   \item{geometry_source}{Source of geometry: "epa" or "api"}
#'   \item{epa_available}{Whether EPA had geometry for this bank}
#'   \item{status_limitation}{Note if geometry limited by bank status}
#'   \item{geometry}{The spatial geometry}
#' }
#'
#' @details
#' ## Data Source Behavior
#'
#' **EPA ArcGIS (prefer_epa = TRUE)**
#' - Returns geometry for Approved banks only
#' - Faster for bulk queries (single request)
#' - Has layers: approved_banks (centroids), bank_footprints, bank_service_areas
#'
#' **RIBITS API (prefer_epa = FALSE or non-approved banks)**
#' - Returns geometry for all bank statuses
#' - Requires individual bank queries (slower for bulk)
#' - Extracts from nested GeoJSON fields
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Get footprints for specific banks
#' footprints <- rb_get_geometry(c(3646, 3647), geometry_type = "footprint")
#'
#' # Get service areas, always using API
#' service_areas <- rb_get_geometry(c(3646), geometry_type = "service_area",
#'                                   prefer_epa = FALSE)
#'
#' # Get all geometry types
#' all_geom <- rb_get_geometry(c(3646), geometry_type = "all")
#' }
rb_get_geometry <- function(bank_ids,
                             geometry_type = c("centroid", "footprint", "service_area", "all"),
                             prefer_epa = TRUE) {

  geometry_type <- match.arg(geometry_type)

  if (length(bank_ids) == 0) {
    cli::cli_alert_warning("No bank IDs provided")
    return(.empty_sf())
  }

  bank_ids <- as.integer(unique(bank_ids))

  cli::cli_h2("Fetching geometry for {length(bank_ids)} bank{?s}")

  # Step 1: Check spatial availability by source
  availability <- .check_spatial_availability(bank_ids)

  # Step 2: Partition banks by best source
  if (prefer_epa) {
    epa_banks <- availability |>
      dplyr::filter(epa_available) |>
      dplyr::pull(bank_id)

    api_banks <- availability |>
      dplyr::filter(!epa_available) |>
      dplyr::pull(bank_id)
  } else {
    # User requested API for all
    epa_banks <- integer(0)
    api_banks <- bank_ids
  }

  results <- list()

  # Step 3: Fetch EPA geometry (for approved banks)
  if (length(epa_banks) > 0) {
    cli::cli_progress_step("Fetching EPA geometry for {length(epa_banks)} approved bank{?s}...")
    epa_geom <- .fetch_epa_geometry(epa_banks, geometry_type)
    if (!is.null(epa_geom) && nrow(epa_geom) > 0) {
      epa_geom$geometry_source <- "epa"
      results$epa <- epa_geom
      cli::cli_alert_success("EPA: {nrow(epa_geom)} geometries")
    }
  }

  # Step 4: Fetch API geometry (for non-approved or when EPA not preferred)
  if (length(api_banks) > 0) {
    cli::cli_progress_step("Fetching API geometry for {length(api_banks)} bank{?s}...")
    api_geom <- .fetch_api_geometry(api_banks, geometry_type)
    if (!is.null(api_geom) && nrow(api_geom) > 0) {
      api_geom$geometry_source <- "api"
      results$api <- api_geom
      cli::cli_alert_success("API: {nrow(api_geom)} geometries")
    }
  }

  # Step 5: Combine results
  if (length(results) == 0) {
    cli::cli_alert_warning("No geometry found for requested banks")
    return(.empty_sf())
  }

  # Standardize columns before binding
  combined <- .combine_spatial_results(results)

  # Add availability flags
  combined <- combined |>
    dplyr::left_join(
      availability |> dplyr::select(bank_id, epa_available, status_limitation),
      by = "bank_id"
    )

  cli::cli_alert_success("Total: {nrow(combined)} geometries from {length(results)} source{?s}")

  combined
}


#' Check spatial data availability for banks
#'
#' Queries all EPA centroid layers (approved, pending, terminated) to determine
#' which banks have geometry available. The terminated layer includes Sold-Out,
#' Suspended, Terminated, and Withdrawn banks.
#'
#' @param bank_ids Vector of bank IDs
#' @return Tibble with bank_id, epa_available, epa_layer, bank_status, status_limitation
#' @keywords internal
#' @noRd
.check_spatial_availability <- function(bank_ids) {


  # Query all three EPA centroid layers
  epa_results <- list()

  # Layer 2: Approved banks
  approved_ids <- tryCatch({
    epa <- rb_epa_query("approved_banks",
                        bank_ids = bank_ids,
                        out_fields = "BANK_ID,BANK_STATUS",
                        return_geometry = FALSE)
    if (!is.null(epa) && nrow(epa) > 0) {
      names(epa) <- tolower(names(epa))
      tibble::tibble(
        bank_id = as.integer(epa$bank_id),
        epa_layer = "approved_banks",
        bank_status = epa$bank_status %||% "Approved"
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(approved_ids)) epa_results$approved <- approved_ids

  # Layer 3: Pending banks
  pending_ids <- tryCatch({
    epa <- rb_epa_query("pending_banks",
                        bank_ids = bank_ids,
                        out_fields = "BANK_ID,BANK_STATUS",
                        return_geometry = FALSE)
    if (!is.null(epa) && nrow(epa) > 0) {
      names(epa) <- tolower(names(epa))
      tibble::tibble(
        bank_id = as.integer(epa$bank_id),
        epa_layer = "pending_banks",
        bank_status = epa$bank_status %||% "Pending"
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(pending_ids)) epa_results$pending <- pending_ids

  # Layer 4: Terminated banks (includes Sold-Out, Suspended, Terminated, Withdrawn)
  terminated_ids <- tryCatch({
    epa <- rb_epa_query("terminated_banks",
                        bank_ids = bank_ids,
                        out_fields = "BANK_ID,BANK_STATUS",
                        return_geometry = FALSE)
    if (!is.null(epa) && nrow(epa) > 0) {
      names(epa) <- tolower(names(epa))
      tibble::tibble(
        bank_id = as.integer(epa$bank_id),
        epa_layer = "terminated_banks",
        bank_status = epa$bank_status %||% "Terminated"
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(terminated_ids)) epa_results$terminated <- terminated_ids

  # Combine all EPA results
  all_epa <- if (length(epa_results) > 0) {
    dplyr::bind_rows(epa_results)
  } else {
    tibble::tibble(bank_id = integer(), epa_layer = character(), bank_status = character())
  }

  # Build result for all requested bank_ids
  result <- tibble::tibble(bank_id = bank_ids) |>
    dplyr::left_join(all_epa, by = "bank_id") |>
    dplyr::mutate(
      epa_available = !is.na(epa_layer),
      status_limitation = dplyr::if_else(
        !epa_available,
        "Bank not in any EPA centroid layer - will use API coordinates",
        NA_character_
      )
    )

  result
}


#' Fetch geometry from EPA ArcGIS
#'
#' Queries all relevant EPA layers based on geometry type. For centroids,
#' queries approved_banks, pending_banks, and terminated_banks layers.
#' For footprints and service areas, queries the dedicated layers which
#' include all statuses.
#'
#' @param bank_ids Vector of bank IDs
#' @param geometry_type "centroid", "footprint", "service_area", or "all"
#' @return sf object or NULL
#' @keywords internal
#' @noRd
.fetch_epa_geometry <- function(bank_ids, geometry_type) {

  if (geometry_type == "all") {
    # Fetch all geometry types and combine
    centroid <- .fetch_epa_centroids_all_layers(bank_ids)
    footprint <- tryCatch(
      rb_epa_query("bank_footprints", bank_ids = bank_ids),
      error = function(e) NULL
    )
    service_area <- tryCatch(
      rb_epa_query("bank_service_areas", bank_ids = bank_ids),
      error = function(e) NULL
    )

    # Combine into a single result with geometry type column
    results <- list()

    if (!is.null(centroid) && nrow(centroid) > 0) {
      centroid$geometry_type <- "centroid"
      results$centroid <- centroid |>
        dplyr::select(bank_id, bank_name = dplyr::any_of(c("bank_name", "name")),
                      geometry_type, dplyr::any_of("bank_status"), geometry)
    }

    if (!is.null(footprint) && nrow(footprint) > 0) {
      names(footprint) <- tolower(names(footprint))
      footprint$geometry_type <- "footprint"
      results$footprint <- footprint |>
        dplyr::select(bank_id, bank_name = dplyr::any_of(c("bank_name", "name")),
                      geometry_type, geometry)
    }

    if (!is.null(service_area) && nrow(service_area) > 0) {
      names(service_area) <- tolower(names(service_area))
      service_area$geometry_type <- "service_area"
      results$service_area <- service_area |>
        dplyr::select(bank_id, bank_name = dplyr::any_of(c("bank_name", "name")),
                      geometry_type, geometry)
    }

    if (length(results) == 0) return(NULL)

    do.call(rbind, results)

  } else if (geometry_type == "centroid") {
    # For centroids, query all three status layers
    result <- .fetch_epa_centroids_all_layers(bank_ids)

    if (is.null(result) || nrow(result) == 0) return(NULL)

    result$geometry_type <- "centroid"

    # Select key columns
    result |>
      dplyr::select(
        bank_id,
        bank_name = dplyr::any_of(c("bank_name", "name")),
        geometry_type,
        dplyr::any_of(c("bank_status", "state_list", "district")),
        geometry
      )

  } else {
    # For footprints and service areas, use the dedicated layers
    layer <- switch(geometry_type,
      footprint = "bank_footprints",
      service_area = "bank_service_areas"
    )

    result <- tryCatch(
      rb_epa_query(layer, bank_ids = bank_ids),
      error = function(e) {
        cli::cli_alert_warning("EPA query failed: {e$message}")
        NULL
      }
    )

    if (is.null(result) || nrow(result) == 0) return(NULL)

    names(result) <- tolower(names(result))
    result$geometry_type <- geometry_type

    # Select key columns
    result |>
      dplyr::select(
        bank_id,
        bank_name = dplyr::any_of(c("bank_name", "name")),
        geometry_type,
        dplyr::any_of(c("bank_status", "state_list", "district")),
        geometry
      )
  }
}


#' Fetch centroids from all EPA status layers
#'
#' Queries approved_banks, pending_banks, and terminated_banks layers
#' and combines results. The terminated layer includes Sold-Out, Suspended,
#' Terminated, and Withdrawn banks.
#'
#' @param bank_ids Vector of bank IDs
#' @return sf object with centroids from all layers, or NULL
#' @keywords internal
#' @noRd
.fetch_epa_centroids_all_layers <- function(bank_ids) {

  results <- list()

  # Layer 2: Approved banks
  approved <- tryCatch({
    epa <- rb_epa_query("approved_banks", bank_ids = bank_ids)
    if (!is.null(epa) && nrow(epa) > 0) {
      names(epa) <- tolower(names(epa))
      epa$epa_layer <- "approved_banks"
      epa
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(approved)) results$approved <- approved

 # Layer 3: Pending banks
  pending <- tryCatch({
    epa <- rb_epa_query("pending_banks", bank_ids = bank_ids)
    if (!is.null(epa) && nrow(epa) > 0) {
      names(epa) <- tolower(names(epa))
      epa$epa_layer <- "pending_banks"
      epa
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(pending)) results$pending <- pending

  # Layer 4: Terminated banks (includes Sold-Out, Suspended, Terminated, Withdrawn)
  terminated <- tryCatch({
    epa <- rb_epa_query("terminated_banks", bank_ids = bank_ids)
    if (!is.null(epa) && nrow(epa) > 0) {
      names(epa) <- tolower(names(epa))
      epa$epa_layer <- "terminated_banks"
      epa
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(terminated)) results$terminated <- terminated

  # Combine all results
  if (length(results) == 0) return(NULL)

  combined <- do.call(rbind, results)

  # Ensure we have required columns
  if (!"bank_id" %in% names(combined)) {
    cli::cli_alert_warning("No bank_id column in EPA centroids")
    return(NULL)
  }

  combined
}


#' Fetch geometry from API nested fields
#'
#' @param bank_ids Vector of bank IDs
#' @param geometry_type "centroid", "footprint", "service_area", or "all"
#' @return sf object or NULL
#' @keywords internal
#' @noRd
.fetch_api_geometry <- function(bank_ids, geometry_type) {

  # Determine which API fields to request
  request_footprint <- geometry_type %in% c("all", "footprint")
  request_service_area <- geometry_type %in% c("all", "service_area")

  results <- purrr::map(bank_ids, function(id) {
    bank <- tryCatch({
      rb_get("banks", id = id,
             footprint = request_footprint,
             service_area = request_service_area)
    }, error = function(e) {
      cli::cli_alert_warning("Could not fetch bank {id}: {e$message}")
      NULL
    })

    if (is.null(bank) || nrow(bank) == 0) return(NULL)

    bank_geoms <- list()

    # Extract centroid if requested
    if (geometry_type %in% c("all", "centroid")) {
      if ("bank_location_centroid" %in% names(bank)) {
        centroid <- tryCatch(
          rb_extract(bank, "bank_location_centroid", type = "geojson"),
          error = function(e) NULL
        )
        if (!is.null(centroid) && inherits(centroid, "sf") && nrow(centroid) > 0) {
          centroid$bank_id <- id
          centroid$bank_name <- bank$bank_name[1] %||% bank$name[1] %||% NA_character_
          centroid$geometry_type <- "centroid"
          bank_geoms$centroid <- centroid
        }
      }
    }

    # Extract footprint if requested
    if (geometry_type %in% c("all", "footprint")) {
      footprint <- tryCatch(
        rb_extract_footprint(bank),
        error = function(e) NULL
      )
      if (!is.null(footprint) && inherits(footprint, "sf") && nrow(footprint) > 0) {
        footprint$bank_id <- id
        footprint$bank_name <- bank$bank_name[1] %||% bank$name[1] %||% NA_character_
        footprint$geometry_type <- "footprint"
        bank_geoms$footprint <- footprint
      }
    }

    # Extract service area if requested
    if (geometry_type %in% c("all", "service_area")) {
      service_area <- tryCatch(
        rb_extract_service_area(bank),
        error = function(e) NULL
      )
      if (!is.null(service_area) && inherits(service_area, "sf") && nrow(service_area) > 0) {
        service_area$bank_id <- id
        service_area$bank_name <- bank$bank_name[1] %||% bank$name[1] %||% NA_character_
        service_area$geometry_type <- "service_area"
        bank_geoms$service_area <- service_area
      }
    }

    if (length(bank_geoms) == 0) return(NULL)

    # Combine all geometry types for this bank
    do.call(rbind, bank_geoms)
  })

  # Combine all results
  results <- purrr::compact(results)
  if (length(results) == 0) return(NULL)

  do.call(rbind, results)
}


#' Combine spatial results from multiple sources
#'
#' @param results Named list of sf objects
#' @return Combined sf object with standardized columns
#' @keywords internal
#' @noRd
.combine_spatial_results <- function(results) {

  # Standardize columns across all results
  standardized <- purrr::map(results, function(sf_obj) {
    # Ensure required columns exist
    if (!"bank_id" %in% names(sf_obj)) {
      cli::cli_alert_warning("Missing bank_id column in spatial result")
      return(NULL)
    }

    # Select and order key columns
    sf_obj |>
      dplyr::select(
        bank_id,
        bank_name = dplyr::any_of(c("bank_name", "name")),
        geometry_type = dplyr::any_of("geometry_type"),
        geometry_source = dplyr::any_of("geometry_source"),
        dplyr::any_of(c("bank_status", "state_list", "district")),
        geometry
      )
  })

  standardized <- purrr::compact(standardized)
  if (length(standardized) == 0) return(.empty_sf())

  # Bind all results
  do.call(rbind, standardized)
}


#' Create empty sf object with expected schema
#'
#' @return Empty sf object
#' @keywords internal
#' @noRd
.empty_sf <- function() {
  sf::st_sf(
    bank_id = integer(0),
    bank_name = character(0),
    geometry_type = character(0),
    geometry_source = character(0),
    epa_available = logical(0),
    status_limitation = character(0),
    geometry = sf::st_sfc()
  )
}
