# R/data.R
# Documentation for package datasets

#' Bank ID to Name Lookup Table
#'
#' A bundled lookup table mapping bank IDs to bank names and metadata.
#' This dataset is generated from EPA ArcGIS and RIBITS API sources and
#' shipped with the package to provide offline fallback and faster lookups.
#'
#' @format A tibble with 4,938 rows and 7 columns:
#' \describe{
#'   \item{name}{Bank name (character)}
#'   \item{bank_id}{Numeric bank identifier (integer)}
#'   \item{state}{State(s) where bank operates (character)}
#'   \item{district}{USACE district (character)}
#'   \item{year_established}{Year the bank was established (integer)}
#'   \item{source}{Data source: "epa" (EPA ArcGIS) or "api" (RIBITS API) (character)}
#'   \item{name_normalized}{Normalized name for fuzzy matching (character)}
#' }
#'
#' @details
#' This dataset is used internally by [rb_build_name_lookup()] and [rb_match_names()]
#' to provide fast name-to-ID matching without requiring API calls.
#'
#' The lookup table is built from three sources in priority order:
#' 1. EPA ArcGIS (highest priority, most reliable)
#' 2. RIBITS API (real-time data)
#' 3. Transactions by Watershed CSV (optional)
#'
#' ## Updating
#'
#' This data is updated periodically before package releases. To generate
#' a fresh version, run:
#'
#' ```r
#' source("data-raw/banks_lookup.R")
#' ```
#'
#' The bundled data includes a `generated_date` attribute showing when it was created.
#'
#' @section Automatic Updates:
#'
#' When you call [rb_build_name_lookup()], it will:
#' - Check a persistent user cache first (refreshed every 30 days by default)
#' - If the cache is stale or missing, fetch fresh data from APIs
#' - Use this bundled data as a fallback if APIs are unavailable
#'
#' This ensures you get up-to-date bank information even if the package hasn't
#' been updated recently, while still providing offline functionality.
#'
#' @source Generated from:
#' - EPA ArcGIS MapServer (https://geopub.epa.gov/arcgis/rest/services/NEPAssist/RIBITS/MapServer)
#' - RIBITS API (https://ribits.ops.usace.army.mil/ords/RI/public/bank_site_list/)
#'
#' @examples
#' # Load the bundled lookup data
#' data(banks_lookup)
#'
#' # View metadata
#' attr(banks_lookup, "generated_date")
#' attr(banks_lookup, "n_banks")
#'
#' # Summary by source
#' table(banks_lookup$source)
#'
#' # Find a specific bank
#' banks_lookup |>
#'   dplyr::filter(grepl("Wetland", name, ignore.case = TRUE)) |>
#'   head()
"banks_lookup"
