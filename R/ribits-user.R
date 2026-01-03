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
#'   \item{banks}{Summary data (tibble) - always included}
#'   \item{transactions}{Transaction/credit tracking data (tibble, if detailed=TRUE)}
#'   \item{credits}{Credit classifications by type (tibble, if detailed=TRUE)}
#'   \item{notices}{Public notices (tibble, if detailed=TRUE)}
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
#' ca$credits         # Credit classifications (NEW)
#' ca$notices         # Public notices (NEW)
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

#' List Available Data Parameters
#'
#' @description
#' Lists the available data columns and metrics that can be retrieved, similar to
#' `StreamCatTools::sc_get_params()`. This helps users understand what data is available
#' in the "Master Summary" and "Master Ledger" tables.
#'
#' @param type Data type to list parameters for ("banks", "transactions", "contacts")
#'
#' @return A tibble with column names and descriptions
#' @export
#' @examples
#' rb_params("banks")
#' rb_params("transactions")
rb_params <- function(type = c("banks", "transactions", "contacts")) {
  type <- match.arg(type)

  # Get column order from registry
  cols <- .order_columns(data.frame(), type = type)
  col_names <- names(cols)

  # Create description mapping (this could be moved to registry later)
  descriptions <- c(
    "bank_id" = "Primary unique identifier for the bank",
    "bank_name" = "Official name of the mitigation bank",
    "bank_status" = "Current approval status (Approved, Pending, Sold-Out, etc.)",
    "total_available_credits" = "Total credits currently available for sale",
    "total_released_credits" = "Total credits released by IRT",
    "total_potential_credits" = "Total potential credits including unreleased",
    "state_abbrev_list" = "State(s) where the bank operates",
    "district" = "USACE District with oversight",
    "centroid" = "Spatial point location (sf geometry)",
    "footprint" = "Spatial polygon boundary (sf geometry)",
    "service_area" = "Spatial service area polygon (sf geometry)",
    "transaction_date" = "Date of credit transaction",
    "credit_classification" = "Specific type of credit (e.g., Wetland, Stream)",
    "primary_sponsor" = "Name of the bank sponsor organization"
  )

  tibble::tibble(
    parameter = col_names,
    description = descriptions[col_names] |> 
      ifelse(is.na(.), "Standardized attribute", .),
    type = type
  )
}

#' Get RIBITS Data (StreamCat-style Wrapper)
#'
#' @description
#' A simplified, text-based interface for retrieving data, designed to feel familiar
#' to users of `StreamCatTools`. Uses a generic `aoi` (Area of Interest) argument
#' to intelligently filter data.
#'
#' @param aoi Area of Interest. Can be:
#'   - State abbreviation (e.g., "CA", "OR")
#'   - USACE District name (e.g., "Portland", "Sacramento")
#'   - Numeric Bank ID (e.g., 17)
#' @param type Type of data: "banks" (default), "ilf", "umbrellas"
#' @param x_coord Optional longitude (if searching by location - future feature)
#' @param y_coord Optional latitude (if searching by location - future feature)
#' @param state Optional state filter (explicit)
#' @param ... Additional arguments passed to `ribits()`
#'
#' @return A `ribits_data` object
#' @export
#' @examples
#' \dontrun{
#' # Get data for California (aoi = state)
#' ca <- rb_get_data(aoi = "CA")
#'
#' # Get data for Portland District (aoi = district)
#' pd <- rb_get_data(aoi = "Portland")
#'
#' # Get specific bank (aoi = ID)
#' bank <- rb_get_data(aoi = 17)
#' }
rb_get_data <- function(aoi = NULL, 
                        type = "banks",
                        x_coord = NULL, 
                        y_coord = NULL,
                        state = NULL,
                        ...) {
  
  district <- NULL
  ids <- NULL

  # Intelligent AOI parsing
  if (!is.null(aoi)) {
    if (is.numeric(aoi)) {
      ids <- aoi
    } else if (is.character(aoi)) {
      if (nchar(aoi) == 2) {
        # Likely a state abbreviation
        if (is.null(state)) state <- aoi
      } else {
        # Likely a district name
        district <- aoi
      }
    }
  }

  # Delegate to main function
  ribits(
    type = type,
    state = state,
    district = district,
    ids = ids,
    ...
  )
}

#' Get Master Summary Dataframe
#'
#' @description
#' Convenience function to retrieve the "Master Summary" dataframe (one row per bank).
#' Includes all harmonized attributes, flattened spatial data, and wide-form credit metrics.
#'
#' @param aoi Area of Interest (State, District, or Bank ID)
#' @param ... Additional arguments passed to `ribits()`
#'
#' @return A tibble with one row per bank
#' @export
rb_get_summary <- function(aoi = NULL, ...) {
  res <- rb_get_data(aoi = aoi, include_summaries = TRUE, ...)
  res$banks
}

#' Get Master Ledger Dataframe
#'
#' @description
#' Convenience function to retrieve the "Master Ledger" dataframe.
#' Includes comprehensive transaction history from all sources (Watershed CSV + API + Ledger CSV).
#'
#' @param aoi Area of Interest (State, District, or Bank ID)
#' @param ... Additional arguments passed to `ribits()`
#'
#' @return A tibble of transaction history
#' @export
rb_get_ledger <- function(aoi = NULL, ...) {
  res <- rb_get_data(aoi = aoi, transactions = "comprehensive", ...)
  res$transactions
}


