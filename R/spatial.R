#' Parse GeoJSON string to sf object
#'
#' @param geojson_string Character string containing GeoJSON.
#' @return An sfc object or NULL if invalid.
#' @keywords internal
.rb_parse_geojson <- function(geojson_string) {
  if (is.null(geojson_string) || !is.character(geojson_string) || 
      length(geojson_string) == 0 || nchar(trimws(geojson_string)) < 10) {
    return(NULL)
  }
  
  trimmed <- trimws(geojson_string)
  if (!grepl("^\\s*\\{", trimmed) || !grepl("}\\s*$", trimmed)) {
    return(NULL)
  }
  
  tryCatch({
    # Suppress warnings from st_read
    # We use quiet = TRUE to avoid printing "Reading layer..."
    geom <- sf::st_read(trimmed, quiet = TRUE, drivers = "GeoJSON")
    if (nrow(geom) == 0) return(NULL)
    
    # Return just the geometry column
    sf::st_geometry(geom)
  }, error = function(e) {
    NULL
  })
}

#' Extract bank footprint
#' 
#' Extracts the bank footprint geometry from a bank object and converts it to
#' an sf object.
#'
#' @param bank A tibble returned by `rb_get_bank()`.
#' @return An sf object containing the footprint, or NULL if not found.
#' @export
rb_extract_footprint <- function(bank) {
  val <- bank$bank_footprint
  
  # Handle list-column extraction
  if (is.list(val) && length(val) == 1 && (tibble::is_tibble(bank) || is.data.frame(bank))) {
    val <- val[[1]]
  }
  
  if (is.null(val) || all(is.na(val))) return(NULL)
  
  # If it's a character string (GeoJSON), parse it
  if (is.character(val)) {
    geom <- .rb_parse_geojson(val)
    if (!is.null(geom)) {
      # Create sf object with bank_id
      return(sf::st_sf(bank_id = bank$bank_id, geometry = geom))
    }
  }
  
  return(NULL)
}

#' Extract service area
#' 
#' Extracts the service area geometry from a bank object and converts it to
#' an sf object.
#'
#' @param bank A tibble returned by `rb_get_bank()`.
#' @return An sf object containing the service area, or NULL if not found.
#' @export
rb_extract_service_area <- function(bank) {
  val <- bank$service_areas
  
  # Handle list-column extraction
  if (is.list(val) && length(val) == 1 && (tibble::is_tibble(bank) || is.data.frame(bank))) {
    val <- val[[1]]
  }
  
  if (is.null(val) || all(is.na(val))) return(NULL)
  
  # Service areas might be a list of items or a single GeoJSON string
  # If it's a character string (GeoJSON), parse it
  if (is.character(val)) {
    geom <- .rb_parse_geojson(val)
    if (!is.null(geom)) {
      return(sf::st_sf(bank_id = bank$bank_id, geometry = geom))
    }
  }
  
  # If it's a list (e.g. multiple service areas), we might need to iterate
  # But for now, let's assume it comes as a GeoJSON string or we need to inspect further.
  # Based on 03 script, it seems to handle "raw service areas" which might be nested.
  
  return(NULL)
}
