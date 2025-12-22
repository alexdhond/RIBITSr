# tests/testthat/test-api-core.R

library(testthat)
library(RIBITSr)

# Helper to skip tests if RIBITS API is unavailable
skip_if_ribits_offline <- function() {
  tryCatch({
    resp <- httr2::request("https://ribits.ops.usace.army.mil/ords/RI/public/bank_site_list/") |>
      httr2::req_timeout(10) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200) {
      skip("RIBITS API unavailable")
    }
  }, error = function(e) {
    skip("RIBITS API unavailable (network error)")
  })
}

test_that("check_ribits_connection returns logical value", {
  # This test always passes - just checks function returns logical
 result <- check_ribits_connection()
  expect_type(result, "logical")
})

test_that("check_ribits_connection returns TRUE when API is reachable", {
  skip_if_ribits_offline()
  expect_true(check_ribits_connection())
})
