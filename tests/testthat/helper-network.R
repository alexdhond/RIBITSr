# tests/testthat/helper-network.R
# Shared helper functions for network-dependent tests

#' Skip test if RIBITS API is unavailable
#'
#' Use this at the start of any test that requires network access to RIBITS API.
#' @keywords internal
skip_if_ribits_offline <- function() {
  tryCatch({
    resp <- httr2::request("https://ribits.ops.usace.army.mil/ords/RI/public/bank_site_list/") |>
      httr2::req_timeout(10) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200) {
      testthat::skip("RIBITS API unavailable")
    }
  }, error = function(e) {
    testthat::skip("RIBITS API unavailable (network error)")
  })
}

#' Skip test if EPA ArcGIS is unavailable
#' @keywords internal
skip_if_epa_offline <- function() {
  tryCatch({
    resp <- httr2::request("https://geopub.epa.gov/arcgis/rest/services/NEPAssist/RIBITS/MapServer?f=json") |>
      httr2::req_timeout(10) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200) {
      testthat::skip("EPA ArcGIS unavailable")
    }
  }, error = function(e) {
    testthat::skip("EPA ArcGIS unavailable (network error)")
  })
}
