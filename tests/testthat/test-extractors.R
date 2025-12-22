# tests/testthat/test-extractors.R

test_that("rb_flatten_record works on a tibble", {
  bank <- tibble::tibble(
    bank_id = 123,
    name = "Test Bank",
    ledger = list(data.frame(a=1)),
    contacts = list(NULL)
  )
  
  flat <- rb_flatten_record(bank)
  
  expect_s3_class(flat, "tbl_df")
  expect_equal(nrow(flat), 1)
  # ledger and contacts should be removed (as they are list columns)
  expect_false("ledger" %in% names(flat))
  expect_false("contacts" %in% names(flat))
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
  expect_equal(res$x, "1")
})
