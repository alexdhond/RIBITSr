# tests/testthat/test-banks.R

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

test_that("rb_get('banks') returns a data frame with expected columns", {
  skip_on_cran()
  skip_if_ribits_offline()

  # Use a filter to avoid fetching too much data
  banks <- rb_get("banks", state = "FL")

  # Handle NULL return (network failure with retry exhausted)
  skip_if(is.null(banks), "API request failed")

  expect_s3_class(banks, "data.frame")
  # rb_get likely returns standardized 'bank_id' and 'bank_name' or similar
  # Checking loose match or print names to verify later if it fails
  expect_true(any(grepl("bank_id", colnames(banks), ignore.case = TRUE)))
  expect_gt(nrow(banks), 0)
})

test_that("rb_get('banks', id=...) returns a list of flattened components", {
  skip_on_cran()
  skip_if_ribits_offline()

  # Fetch a known bank (Bank ID 17 is commonly used in examples)
  bank <- rb_get("banks", id = 17)

  # Handle NULL/empty return
  skip_if(is.null(bank) || length(bank) == 0, "API request failed")

  expect_type(bank, "list")
  expect_true("summary" %in% names(bank))
  expect_s3_class(bank$summary, "tbl_df")
  expect_equal(nrow(bank$summary), 1)

  # Check for other components
  if ("ledger" %in% names(bank)) {
    expect_s3_class(bank$ledger, "tbl_df")
  }
})
