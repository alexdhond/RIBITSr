# tests/testthat/test-banks.R

library(testthat)
library(RIBITSr)

test_that("rb_list_banks returns a data frame with expected columns", {
  skip_on_cran()
  # We'll test rb_list_banks directly as get_all_banks is deprecated
  # Use a filter to avoid fetching too much data
  banks <- rb_list_banks(state = "FL")
  expect_s3_class(banks, "data.frame")
  # Check for actual columns returned by API
  expect_true(all(c("bank_id", "name") %in% colnames(banks)))
  expect_gt(nrow(banks), 0)
})

test_that("rb_get_bank returns a tibble with implicit flattening", {
  skip_on_cran()
  
  # Fetch a known bank (Bank ID 17 is commonly used in examples)
  bank <- rb_get_bank(17)
  
  expect_s3_class(bank, "tbl_df")
  expect_equal(nrow(bank), 1)
  
  # Check for list-columns
  expect_true("ledger" %in% colnames(bank))
  # LEDGER might be a list (if data exists) or NA (if not), but it should be a column.
  
  # Check that we can extract from it
  ledger <- rb_extract_ledger(bank)
  expect_s3_class(ledger, "tbl_df")
})
