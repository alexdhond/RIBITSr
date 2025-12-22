#' @keywords internal
"_PACKAGE"

#' RIBITSr: R Interface to the RIBITS API
#'
#' The RIBITSr package provides a streamlined interface to the RIBITS
#' (Regulatory In-lieu fee and Bank Information Tracking System) API
#' and EPA ArcGIS MapServer for mitigation banking data.
#'
#' Automatically fetches and harmonizes data from multiple sources (RIBITS API,
#' EPA ArcGIS, direct CSV downloads) with comprehensive discrepancy detection.
#'
#' @section Simple API (Recommended):
#' Use these functions for most tasks:
#' \describe{
#'   \item{\code{\link{ribits}}}{Get bank/ILF/umbrella data (auto-harmonized)}
#'   \item{\code{\link{rb_transactions}}}{Get credit tracking/classification data}
#'   \item{\code{\link{rb_info}}}{Explore available data sources}
#'   \item{\code{\link{rb_check}}}{Check data coverage and quality}
#' }

#' @section Quick Start:
#' \preformatted{
#' # Get California banks (all sources, auto-harmonized)
#' ca <- ribits(state = "CA")
#'
#' # Access data
#' ca$banks         # Bank summary
#' ca$ledger        # Transaction history
#' ca$footprints    # Spatial data
#'
#' # Get ILF programs or umbrellas
#' ilf <- ribits(type = "ilf", state = "TX")
#' umb <- ribits(type = "umbrellas", state = "FL")
#'
#' # Check data quality
#' ca$.meta$discrepancies
#' }
#'
#' @section Advanced API:
#' \describe{
#'   \item{\code{\link{rb_get}}}{Fine-grained API queries}
#'   \item{\code{\link{rb_epa}}}{Direct EPA ArcGIS access}
#'   \item{\code{\link{rb_download_report}}}{Direct CSV downloads}
#'   \item{\code{\link{rb_extract}}}{Extract nested data}
#' }
#'
#' @section Discrepancy Handling:
#' \describe{
#'   \item{\code{\link{rb_discrepancy_config}}}{Configure conflict resolution}
#'   \item{\code{\link{rb_discrepancy_report}}}{View data quality issues}
#' }
#'
#' @name RIBITSr-package
NULL

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom utils head
## usethis namespace: end
NULL
