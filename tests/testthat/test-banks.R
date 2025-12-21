# tests/testthat/test-banks.R

library(testthat)
library(RIBITSr)

test_that("get_all_banks returns a data frame with expected columns", {
  banks <- get_all_banks()
  expect_s3_class(banks, "data.frame")
  expect_true(all(c("BankName", "State", "ServiceArea") %in% colnames(banks)))
  expect_gt(nrow(banks), 0) # Expect more than 0 rows
})
