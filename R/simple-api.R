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


