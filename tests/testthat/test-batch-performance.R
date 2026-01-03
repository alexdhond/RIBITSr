test_that("batch performance test works with current API", {
  skip_on_cran()
  skip_if_offline()

  # Get Florida data to test with
  fl <- ribits(state = "FL", transactions = "none", spatial = FALSE, quietly = TRUE)

  expect_s3_class(fl, "ribits_data")
  expect_true("banks" %in% names(fl))
  expect_gt(nrow(fl$banks), 0)

  # Get a subset with full data
  all_ids <- fl$banks$bank_id

  if (length(all_ids) >= 10) {
    # Test getting 10 banks with comprehensive transactions
    ids_subset <- all_ids[1:10]
    res <- ribits(ids = ids_subset, transactions = "comprehensive", quietly = TRUE)

    expect_s3_class(res, "ribits_data")
    expect_equal(nrow(res$banks), 10)
    expect_true("transactions" %in% names(res))
  }
})
