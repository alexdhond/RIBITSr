# R/simple-api.R
# Simple, user-friendly API - the recommended interface for most users
# These functions hide all complexity and "just work"

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
#' @param ledger Include transaction/credit ledger data? Default TRUE (banks only).
#' @param spatial Include footprint and service area geometries? Default TRUE.
#' @param sources Which data sources to use. Default: all available
#'   - "api" = RIBITS API only (fast, real-time)
#'   - "epa" = EPA ArcGIS only (spatial data)
#'   - "csv" = Direct CSV downloads (official records)
#'   - c("api", "epa", "csv") = All sources (recommended, default)
#' @param cache Cache downloaded CSV files? Default TRUE.
#' @param quietly Suppress progress messages? Default FALSE.
#'
#' @return A `ribits_data` object containing:
#'   \item{banks/programs/umbrellas}{Summary data (tibble)}
#'   \item{ledger}{Transaction/credit tracking data (tibble, banks only)}
#'   \item{footprints}{Footprint geometries (sf object)}
#'   \item{service_areas}{Service area geometries (sf object)}
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
#' ca$ledger          # Transaction history
#' ca$footprints      # Spatial polygons
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
#' # Just summary data, no spatial (faster)
#' summary <- ribits(state = "OR", spatial = FALSE, ledger = FALSE)
#'
#' # Only use API source (fastest, real-time)
#' api_only <- ribits(state = "TX", sources = "api")
#' }
ribits <- function(type = "banks",
                   state = NULL,
                   district = NULL,
                   ids = NULL,
                   ledger = TRUE,
                   spatial = TRUE,
                   sources = c("api", "epa", "csv"),
                   cache = TRUE,
                   quietly = FALSE) {

  type <- match.arg(type, c("banks", "ilf", "umbrellas"))

  # Determine what data to fetch
  # Ledger only applies to banks
  include_ledger <- ledger && type == "banks"
  
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
    quietly = quietly
  )
}


