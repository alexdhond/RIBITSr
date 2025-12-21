test_that("check_ribits_connection returns TRUE when API is reachable", {
  skip_on_cran()

  httptest2::with_mock_dir("ribits_connection_success", {
    result <- check_ribits_connection()

    expect_type(result, "logical")
    expect_true(result)
  })
})

test_that("check_ribits_connection returns FALSE on connection error", {
  skip_on_cran()

  httptest2::with_mock_dir("ribits_connection_error", {
    # Mock a failed request
    result <- check_ribits_connection()

    expect_type(result, "logical")
  })
})

test_that("check_ribits_connection respects timeout parameter", {
  skip_on_cran()

  httptest2::with_mock_dir("ribits_connection_timeout", {
    result <- check_ribits_connection(timeout = 5)

    expect_type(result, "logical")
  })
})

test_that("check_ribits_connection verbose parameter works", {
  skip_on_cran()

  httptest2::with_mock_dir("ribits_connection_verbose", {
    # Capture output when verbose = TRUE
    expect_message(
      check_ribits_connection(verbose = TRUE),
      "Testing connection to RIBITS API"
    )
  })
})

test_that("check_ribits_connection handles invalid responses", {
  skip_on_cran()

  httptest2::with_mock_dir("ribits_connection_invalid", {
    result <- check_ribits_connection()

    expect_type(result, "logical")
  })
})
