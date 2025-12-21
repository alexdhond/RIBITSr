#' Check RIBITS API Connection
#'
#' Tests if the RIBITS (Regulatory In-lieu fee and Bank Information Tracking
#' System) API is reachable and responding to requests.
#'
#' @param timeout Numeric. Maximum time to wait for response in seconds.
#'   Default is 10.
#' @param verbose Logical. If TRUE, prints status messages. Default is FALSE.
#'
#' @return Logical. TRUE if the RIBITS API is reachable, FALSE otherwise.
#'
#' @export
#' @examples
#' \dontrun{
#' # Check if RIBITS API is available
#' check_ribits_connection()
#'
#' # Check with verbose output
#' check_ribits_connection(verbose = TRUE)
#' }
check_ribits_connection <- function(timeout = 10, verbose = FALSE) {
  # Test a known endpoint that should exist
  test_url <- "https://ribits.ops.usace.army.mil/ords/RI/public/bank_site_list/"

  if (verbose) {
    cli::cli_alert_info("Testing connection to RIBITS API...")
  }

  tryCatch(
    {
      resp <- httr2::request(test_url) |>
        httr2::req_timeout(timeout) |>
        httr2::req_perform()

      status <- httr2::resp_status(resp)

      if (status >= 200 && status < 300) {
        if (verbose) {
          cli::cli_alert_success("RIBITS API is reachable (status: {status})")
        }
        return(TRUE)
      } else {
        if (verbose) {
          cli::cli_alert_warning("RIBITS API returned status: {status}")
        }
        return(FALSE)
      }
    },
    error = function(e) {
      if (verbose) {
        cli::cli_alert_danger("Connection failed: {e$message}")
      }
      return(FALSE)
    }
  )
}
