# tests/testthat/test-extractors.R

test_that("rb_flatten_bank works on a tibble", {
  bank <- tibble::tibble(
    bank_id = 123,
    name = "Test Bank",
    ledger = list(data.frame(a=1)),
    contacts = list(NULL)
  )
  
  flat <- rb_flatten_bank(bank)
  
  expect_s3_class(flat, "tbl_df")
  expect_equal(nrow(flat), 1)
  expect_equal(ncol(flat), 2) # bank_id and name
  expect_equal(flat$bank_id, 123)
})

test_that("rb_extract_ledger handles missing ledger in tibble", {
  bank <- tibble::tibble(ledger = list(NA))
  expect_equal(nrow(rb_extract_ledger(bank)), 0)
  
  bank <- tibble::tibble(ledger = list(NULL))
  expect_equal(nrow(rb_extract_ledger(bank)), 0)
})

test_that("rb_extract_ledger handles data frame ledger in tibble", {
  bank <- tibble::tibble(ledger = list(data.frame(x = 1, y = 2)))
  res <- rb_extract_ledger(bank)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_equal(res$x, 1)
})

test_that("rb_scan_missing identifies missing fields in tibble", {
  bank <- tibble::tibble(
    A = 1,
    B = list(NA),
    C = list(NULL)
  )
  
  diag <- rb_scan_missing(bank)
  
  expect_equal(nrow(diag), 3)
  expect_equal(diag$status[diag$field == "A"], "present")
  expect_equal(diag$status[diag$field == "B"], "missing_na")
  expect_equal(diag$status[diag$field == "C"], "missing_null")
})
