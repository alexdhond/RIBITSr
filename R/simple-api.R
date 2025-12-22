# R/simple-api.R
# Simple, user-friendly API - the recommended interface for most users
# These functions hide all complexity and "just work"

#' Get all bank data (fully harmonized)
#'
#' The simplest way to get mitigation bank data. Automatically fetches and
#' harmonizes data from all available sources:
#' - RIBITS API (real-time data)
#' - EPA ArcGIS MapServer (spatial data)
#' - RIBITS CSV reports (official records, fetched directly)
#'
#' No manual downloads required - everything is automatic!
#'
#' @param state State filter (e.g., "CA", "OR", "TX")
#' @param district USACE district filter (e.g., "Portland", "Sacramento")
#' @param bank_ids Optional vector of specific bank IDs to retrieve
#' @param ledger Include transaction/credit ledger data? Default TRUE.
#' @param spatial Include footprint and service area geometries? Default TRUE.
#' @param sources Which data sources to use. Default: all available
#'   - "api" = RIBITS API only (fast, real-time)
#'   - "epa" = EPA ArcGIS only (spatial data)
#'   - "csv" = Direct CSV downloads (official records)
#'   - c("api", "epa", "csv") = All sources (recommended, default)
#' @param cache Cache downloaded CSV files? Default TRUE. Files cached in
#'   session temp directory and reused if called again.
#' @param quietly Suppress progress messages? Default FALSE.
#'
#' @return A `ribits_data` object containing:
#'   \item{banks}{Bank summary data (tibble)}
#'   \item{ledger}{Transaction/credit tracking data (tibble)}
#'   \item{footprints}{Bank footprint geometries (sf object)}
#'   \item{service_areas}{Service area geometries (sf object)}
#'   \item{.meta}{Metadata including sources used and any discrepancies}
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all California banks (fully harmonized, automatic!)
#' ca_banks <- rb_banks(state = "CA")
#'
#' # Access the data
#' ca_banks$banks           # Summary data
#' ca_banks$ledger          # Transaction history
#' ca_banks$footprints      # Spatial polygons
#'
#' # Check data quality
#' ca_banks$.meta$discrepancies  # Any conflicts between sources?
#'
#' # Get specific banks by ID
#' my_banks <- rb_banks(bank_ids = c(17, 100, 345))
#'
#' # Just summary data, no spatial (faster)
#' summary <- rb_banks(state = "OR", spatial = FALSE)
#'
#' # Only use API source (fastest, real-time)
#' api_only <- rb_banks(state = "TX", sources = "api")
#'
#' # Export to CSV for analysis
#' readr::write_csv(ca_banks$banks, "california_banks.csv")
#' }
rb_banks <- function(state = NULL,
                     district = NULL,
                     bank_ids = NULL,
                     ledger = TRUE,
                     spatial = TRUE,
                     sources = c("api", "epa", "csv"),
                     cache = TRUE,
                     quietly = FALSE) {

  # Determine what data to fetch
  what <- if (ledger && spatial) {
    "all"
  } else if (spatial) {
    "spatial"
  } else if (ledger) {
    "ledger"
  } else {
    "banks"
  }

  # Call the auto-harmonization engine
  ribits(
    bank_ids = bank_ids,
    state = state,
    district = district,
    what = what,
    sources = sources,
    cache = cache,
    quietly = quietly
  )
}


#' Get ILF program data (fully harmonized)
#'
#' Simple interface to get In-Lieu Fee (ILF) program data. Automatically
#' fetches and harmonizes data from all available sources.
#'
#' @param state State filter (e.g., "CA", "OR", "TX")
#' @param district USACE district filter
#' @param program_ids Optional vector of specific program IDs
#' @param spatial Include footprint and service area geometries? Default TRUE.
#' @param sources Which data sources to use. Default: all available.
#' @param cache Cache downloaded data? Default TRUE.
#' @param quietly Suppress progress messages? Default FALSE.
#'
#' @return A `ribits_data` object with ILF program information
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all California ILF programs
#' ca_ilf <- rb_ilf_programs(state = "CA")
#'
#' # Access the data
#' ca_ilf$programs      # Program summary
#' ca_ilf$footprints    # Program footprints
#'
#' # Just API data (faster)
#' programs <- rb_ilf_programs(state = "OR", sources = "api")
#' }
rb_ilf_programs <- function(state = NULL,
                             district = NULL,
                             program_ids = NULL,
                             spatial = TRUE,
                             sources = c("api", "epa", "csv"),
                             cache = TRUE,
                             quietly = FALSE) {

  what <- if (spatial) "all" else "programs"

  ribits(
    bank_ids = program_ids,  # ribits() uses bank_ids generically
    state = state,
    district = district,
    what = what,
    type = "ilf",
    sources = sources,
    cache = cache,
    quietly = quietly
  )
}


#' Get umbrella instrument data (fully harmonized)
#'
#' Simple interface to get umbrella mitigation instrument data. Automatically
#' fetches and harmonizes data from all available sources.
#'
#' @param state State filter (e.g., "CA", "OR", "TX")
#' @param district USACE district filter
#' @param umbrella_ids Optional vector of specific umbrella IDs
#' @param spatial Include footprint and service area geometries? Default TRUE.
#' @param sources Which data sources to use. Default: all available.
#' @param cache Cache downloaded data? Default TRUE.
#' @param quietly Suppress progress messages? Default FALSE.
#'
#' @return A `ribits_data` object with umbrella instrument information
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all umbrella instruments
#' umbrellas <- rb_umbrellas(state = "FL")
#'
#' # Access the data
#' umbrellas$umbrellas     # Umbrella summary
#' umbrellas$footprints    # Spatial data
#' }
rb_umbrellas <- function(state = NULL,
                         district = NULL,
                         umbrella_ids = NULL,
                         spatial = TRUE,
                         sources = c("api", "epa", "csv"),
                         cache = TRUE,
                         quietly = FALSE) {

  what <- if (spatial) "all" else "umbrellas"

  ribits(
    bank_ids = umbrella_ids,
    state = state,
    district = district,
    what = what,
    type = "umbrellas",
    sources = sources,
    cache = cache,
    quietly = quietly
  )
}


# DELETED: rb_credits() - use rb_transactions() instead
# DELETED: print.ribits_credits() - no longer needed
