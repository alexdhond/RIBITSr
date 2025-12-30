# R/ribits-user.R
# Main user-facing functions with automatic harmonization
# Users should primarily use these - all complexity is hidden

#' Get RIBITS Data
#'
#' @description
#' The main function for downloading mitigation banking data. Automatically fetches
#' and harmonizes data from all available sources (RIBITS API, EPA ArcGIS, and
#' CSV reports). By default, retrieves the maximum amount of data with all columns.
#'
#' **Philosophy:** Get everything, then filter using tidyverse. This function gives
#' you all the data; use dplyr/tidyr to shape it for your analysis.
#'
#' @param type Type of data to fetch:
#'   - "banks" (default): Mitigation banks
#'   - "ilf": In-Lieu Fee programs
#'   - "umbrellas": Umbrella mitigation instruments
#' @param state State filter (e.g., "CA", "OR", "TX"). Full names also accepted.
#' @param district USACE district filter (e.g., "Portland", "Sacramento")
#' @param ids Optional vector of specific IDs to retrieve (bank_ids, program_ids, etc.).
#'   Alias: `id` can also be used.
#' @param transactions Transaction detail level (banks only). Default "comprehensive".
#'   - "comprehensive": All transaction data (~85 columns) - RECOMMENDED
#'   - "basic": API ledger only (~20 columns, faster)
#'   - "none": No transaction data
#' @param spatial Include spatial geometries? Default TRUE.
#' @param sources Data sources to use. Default c("api", "epa", "csv") - all sources.
#'   Using all sources provides the most complete data. If one fails, others are used.
#' @param cache Cache downloaded files? Default TRUE. Speeds up repeated queries.
#' @param include_summaries Include summary columns? Default TRUE. Adds transaction
#'   volume, temporal patterns, geography, and public notice counts.
#' @param quietly Suppress progress messages? Default FALSE.
#' @param id Alternative for `ids` (for backward compatibility).
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
#' # The simple way - get everything for California
#' ca <- ribits(state = "CA")
#'
#' # Access the data
#' ca$banks           # All bank attributes, all sources merged
#' ca$transactions    # All transaction data (~85 columns)
#' ca$geometry        # Spatial footprints and service areas
#'
#' # Filter with tidyverse
#' library(dplyr)
#' ca$banks %>%
#'   filter(bank_status == "Approved") %>%
#'   select(bank_id, bank_name, total_acres, available_credits)
#'
#' # Get ILF programs
#' ilf <- ribits(type = "ilf", state = "TX")
#'
#' # Get specific banks by ID
#' my_banks <- ribits(ids = c(17, 100, 345))
#'
#' # For speed, reduce data:
#' # - Basic transactions instead of comprehensive
#' # - Skip spatial data
#' fast <- ribits(state = "OR", transactions = "basic", spatial = FALSE)
#' }
ribits <- function(type = "banks",
                   state = NULL,
                   district = NULL,
                   ids = NULL,
                   transactions = c("comprehensive", "basic", "none"),
                   spatial = TRUE,
                   sources = c("api", "epa", "csv"),
                   cache = TRUE,
                   include_summaries = TRUE,
                   quietly = FALSE,
                   id = NULL) {

  type <- match.arg(type, c("banks", "ilf", "umbrellas"))
  transactions <- match.arg(transactions)

  # Standardize state
  state <- .standardize_state(state)

  # Handle id/ids alias
  if (is.null(ids) && !is.null(id)) {
    ids <- id
  }

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
