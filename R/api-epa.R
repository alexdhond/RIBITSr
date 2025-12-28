# R/api-epa.R
# EPA ArcGIS spatial data retrieval
# Split from R/unified-api.R (627 lines → focused modules)

# =============================================================================
# UNIFIED EPA ARCGIS FUNCTION  
# =============================================================================

#' Query EPA ArcGIS for RIBITS spatial data (Internal)
#'
#' @description
#' This function is internal and called automatically by `ribits()`.
#'
#' Unified function to query any EPA ArcGIS layer. Replaces the individual
#' rb_epa_* functions with a single flexible interface.
#'
#' @param layer Character. Layer to query. One of:
#'   - "banks" - Bank point locations (approved)
#'   - "footprints" - Bank footprint polygons
#'   - "service_areas" - Bank service area polygons
#'   - "ilf_programs" - ILF program point locations
#'   - "ilf_service_areas" - ILF program service area polygons
#'   - "districts" - USACE district boundaries
#' @param state Optional state filter (e.g., "CA", "OR")
#' @param district Optional USACE district filter
#' @param bank_ids Optional vector of bank IDs to filter by
#' @param program_ids Optional vector of program IDs (for ILF layers)
#' @param status Optional status filter: "Approved", "Pending", "Terminated"
#' @param kind Optional bank type filter: "Standard", "ILF", "Umbrella", "NRDA"
#' @param where Optional custom SQL WHERE clause (overrides other filters)
#' @return An sf object with the queried features
#' @keywords internal
#' @examples
#' \dontrun{
#' # Get Oregon banks
#' banks <- rb_epa("banks", state = "OR")
#'
#' # Get footprints for specific banks
#' fp <- rb_epa("footprints", bank_ids = c(17, 100))
#'
#' # Get all ILF service areas
#' ilf_sa <- rb_epa("ilf_service_areas")
#'
#' # Get approved standard banks in California
#' ca_banks <- rb_epa("banks", state = "CA", kind = "Standard")
#'
#' # List available layers
#' rb_epa()
#' }
rb_epa <- function(layer = NULL,
                   state = NULL,
                   district = NULL,
                   bank_ids = NULL,
                   program_ids = NULL,
                   status = NULL,
                   kind = NULL,
                   where = NULL) {
  
  # Layer mapping to internal layer names
  layer_map <- list(
    banks = "approved_banks",
    footprints = "bank_footprints",
    service_areas = "bank_service_areas",
    ilf_programs = "ilf_approved",
    ilf_service_areas = "ilf_service_areas",
    districts = "usace_districts"
  )
  
  available <- names(layer_map)
  
  # If no layer specified, list available options
  if (is.null(layer)) {
    cli::cli_h2("Available EPA ArcGIS Layers")
    cli::cli_bullets(c(
      "*" = "{.field banks}: Approved bank point locations",
      "*" = "{.field footprints}: Bank footprint polygons",
      "*" = "{.field service_areas}: Bank service area polygons",
      "*" = "{.field ilf_programs}: ILF program point locations",
      "*" = "{.field ilf_service_areas}: ILF program service areas",
      "*" = "{.field districts}: USACE district boundaries"
    ))
    cli::cli_text("")
    cli::cli_alert_info("Usage: {.code rb_epa(\"layer_name\", state = \"CA\")}")
    return(invisible(NULL))
  }
  
  # Validate layer
  layer <- tolower(gsub("[- ]", "_", layer))
  if (!(layer %in% available)) {
    cli::cli_abort(c(
      "Unknown layer: {layer}",
      "i" = "Available: {paste(available, collapse = ', ')}"
    ))
  }
  
  internal_layer <- layer_map[[layer]]
  
  # Build WHERE clause
  if (is.null(where)) {
    where_parts <- c()
    
    if (!is.null(state)) {
      where_parts <- c(where_parts, paste0("STATE_LIST LIKE '%", state, "%'"))
    }
    if (!is.null(district)) {
      where_parts <- c(where_parts, paste0("DISTRICT = '", district, "'"))
    }
    if (!is.null(kind)) {
      where_parts <- c(where_parts, paste0("KIND_OF_BANK = '", kind, "'"))
    }
    if (!is.null(status)) {
      where_parts <- c(where_parts, paste0("BANK_STATUS LIKE '%", status, "%'"))
    }
    
    where <- if (length(where_parts) > 0) {
      paste(where_parts, collapse = " AND ")
    } else {
      "1=1"
    }
  }
  
  # Handle bank_ids or program_ids
  ids <- bank_ids
  if (layer %in% c("ilf_programs", "ilf_service_areas") && !is.null(program_ids)) {
    ids <- program_ids
  }
  
  rb_epa_query(internal_layer, where = where, bank_ids = ids)
}


