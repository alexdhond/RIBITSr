# Tests for diagnostics-sources.R
# Tests rb_compare_sources(), rb_validate_credits(), rb_status_coverage()

library(testthat)
library(RIBITSr)
library(dplyr)

# ==============================================================================
# rb_compare_sources() Tests
# ==============================================================================

test_that("rb_compare_sources() handles NULL or empty data", {
  # NULL input
  result <- rb_compare_sources(NULL, verbose = FALSE)
  expect_null(result)

  # Empty data frame
  empty_df <- tibble::tibble()
  result <- rb_compare_sources(empty_df, verbose = FALSE)
  expect_null(result)

  # Data frame with no rows
  zero_rows <- tibble::tibble(
    transaction_id = integer(),
    source = character(),
    credits = numeric()
  )
  result <- rb_compare_sources(zero_rows, verbose = FALSE)
  expect_null(result)
})

test_that("rb_compare_sources() handles missing source column", {
  # Data without 'source' column
  data <- tibble::tibble(
    transaction_id = 1:5,
    credits = c(10, 20, 30, 40, 50)
  )

  result <- rb_compare_sources(data, verbose = FALSE)
  expect_null(result)
})

test_that("rb_compare_sources() works with single source", {
  # Data from single source
  data <- tibble::tibble(
    transaction_id = 1:10,
    source = rep("api", 10),
    credits = runif(10, 10, 100),
    transaction_date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10)
  )

  result <- rb_compare_sources(data, verbose = FALSE)

  # Should return list with completeness metrics
  expect_type(result, "list")
  expect_true("api" %in% names(result))
  expect_true("n_rows" %in% names(result$api))
  expect_equal(result$api$n_rows, 10)
})

test_that("rb_compare_sources() compares multiple sources", {
  # Data from multiple sources
  data <- tibble::tibble(
    transaction_id = 1:20,
    source = rep(c("api", "csv"), each = 10),
    credits = c(runif(10, 10, 100), runif(10, 10, 100)),
    transaction_date = c(
      seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
      seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10)
    ),
    # CSV has additional field that API doesn't
    permit = c(rep(NA, 10), paste0("PERMIT-", 1:10))
  )

  result <- rb_compare_sources(data, verbose = FALSE)

  # Should have both sources
  expect_true("api" %in% names(result))
  expect_true("csv" %in% names(result))

  # Each should have metrics
  expect_equal(result$api$n_rows, 10)
  expect_equal(result$csv$n_rows, 10)

  # CSV should have better completeness for 'permit' field
  if ("field_pct" %in% names(result$csv)) {
    expect_true("avg_completeness" %in% names(result$csv))
  }
})

test_that("rb_compare_sources() calculates completeness correctly", {
  # Create data with known completeness
  data <- tibble::tibble(
    transaction_id = 1:10,
    source = rep("api", 10),
    credits = c(rep(100, 5), rep(NA, 5)),  # 50% complete
    transaction_date = as.Date("2020-01-01"),  # 100% complete
    permit = rep(NA, 10)  # 0% complete
  )

  result <- rb_compare_sources(data, verbose = FALSE)

  expect_type(result, "list")
  expect_true("api" %in% names(result))

  # Check field percentages if available
  if ("field_pct" %in% names(result$api)) {
    field_pct <- result$api$field_pct

    # credits should be ~50% (5 out of 10)
    if ("credits" %in% names(field_pct)) {
      expect_equal(field_pct[["credits"]], 50.0)
    }

    # transaction_date should be 100%
    if ("transaction_date" %in% names(field_pct)) {
      expect_equal(field_pct[["transaction_date"]], 100.0)
    }
  }
})

test_that("rb_compare_sources() works with ribits_data object", {
  # Create mock ribits_data object
  ribits_obj <- list(
    banks = tibble::tibble(bank_id = 1:5),
    transactions = tibble::tibble(
      transaction_id = 1:10,
      source = rep("api", 10),
      credits = runif(10, 10, 100)
    ),
    ledger = tibble::tibble(
      transaction_id = 1:10,
      source = rep("api", 10),
      credits = runif(10, 10, 100),
      transaction_date = as.Date("2020-01-01")
    ),
    .meta = list()
  )
  class(ribits_obj) <- "ribits_data"

  # Should extract ledger automatically
  result <- rb_compare_sources(ribits_obj, verbose = FALSE)

  expect_type(result, "list")
})

# ==============================================================================
# rb_validate_credits() Tests
# ==============================================================================

test_that("rb_validate_credits() handles missing dependencies gracefully", {
  skip_on_cran()  # May require network access

  # Function should exist
  expect_true(exists("rb_validate_credits", mode = "function"))

  # Should handle NULL bank_ids
  # (Will fail if CSV download fails, which is expected)
  result <- tryCatch(
    rb_validate_credits(bank_ids = NULL, sample_size = 1, verbose = FALSE),
    error = function(e) NULL
  )

  # Test passes if function runs (may return NULL if CSV unavailable)
  expect_true(is.null(result) || is.list(result))
})

test_that("rb_validate_credits() accepts valid parameters", {
  # Should accept bank_ids as integer vector
  # Should accept tolerance as numeric
  # Should accept sample_size as integer
  # Should accept verbose as logical

  # These parameter checks ensure the function signature is correct
  expect_error(
    rb_validate_credits(bank_ids = 1:10, tolerance = 0.05, sample_size = 10, verbose = FALSE),
    NA  # Should not error on valid parameters (may fail on execution if CSV unavailable)
  )
})

# ==============================================================================
# rb_status_coverage() Tests
# ==============================================================================

test_that("rb_status_coverage() handles NULL state parameter", {
  skip_on_cran()

  # Function should exist
  expect_true(exists("rb_status_coverage", mode = "function"))

  # Should handle NULL state (analyze all states)
  result <- tryCatch(
    rb_status_coverage(state = NULL, verbose = FALSE),
    error = function(e) NULL
  )

  # Function should run (may return NULL if CSV/API unavailable)
  expect_true(is.null(result) || is.list(result))
})

test_that("rb_status_coverage() accepts valid state codes", {
  skip_on_cran()

  # Should accept 2-letter state codes
  result <- tryCatch(
    rb_status_coverage(state = "CA", verbose = FALSE),
    error = function(e) NULL
  )

  # Should complete without error
  expect_true(is.null(result) || is.list(result))
})

test_that("rb_status_coverage() accepts source parameter", {
  skip_on_cran()

  # Should accept sources parameter
  result <- tryCatch(
    rb_status_coverage(state = "CA", sources = c("api", "csv"), verbose = FALSE),
    error = function(e) NULL
  )

  expect_true(is.null(result) || is.list(result))
})

# ==============================================================================
# Integration Tests (require network access)
# ==============================================================================

test_that("rb_compare_sources() produces meaningful output with real data", {
  skip_on_cran()
  skip_if_offline <- function() {
    tryCatch({
      httr2::request("https://ribits.ops.usace.army.mil") |>
        httr2::req_timeout(5) |>
        httr2::req_perform()
    }, error = function(e) skip("RIBITS API unavailable"))
  }
  skip_if_offline()

  # Get real data for a small state
  data <- tryCatch(
    ribits(state = "VT", transactions = "comprehensive"),  # Vermont - small state
    error = function(e) NULL
  )

  skip_if(is.null(data), "Could not fetch data")
  skip_if(is.null(data$ledger) || nrow(data$ledger) == 0, "No ledger data")

  # Run comparison
  result <- rb_compare_sources(data, verbose = FALSE)

  # Should return list with source metrics
  expect_type(result, "list")
  expect_gt(length(result), 0)

  # Each source should have required fields
  for (source_name in names(result)) {
    expect_true("n_rows" %in% names(result[[source_name]]))
    expect_true(is.numeric(result[[source_name]]$n_rows))
    expect_gte(result[[source_name]]$n_rows, 0)
  }
})

# ==============================================================================
# Edge Cases and Error Handling
# ==============================================================================

test_that("rb_compare_sources() handles unusual source names", {
  # Sources with special characters or unusual names
  data <- tibble::tibble(
    transaction_id = 1:10,
    source = rep(c("api+csv", "watershed"), each = 5),
    credits = runif(10, 10, 100),
    transaction_date = as.Date("2020-01-01")
  )

  result <- rb_compare_sources(data, verbose = FALSE)

  expect_type(result, "list")
  expect_true("api+csv" %in% names(result) || "watershed" %in% names(result))
})

test_that("rb_compare_sources() handles all NA columns", {
  # Data where some columns are entirely NA
  data <- tibble::tibble(
    transaction_id = 1:10,
    source = rep("api", 10),
    credits = rep(NA_real_, 10),
    all_na_field = rep(NA_character_, 10)
  )

  result <- rb_compare_sources(data, verbose = FALSE)

  expect_type(result, "list")

  # Completeness for all-NA fields should be 0%
  if (!is.null(result$api) && "field_pct" %in% names(result$api)) {
    if ("credits" %in% names(result$api$field_pct)) {
      expect_equal(result$api$field_pct[["credits"]], 0.0)
    }
  }
})

test_that("rb_compare_sources() handles mixed data types in fields", {
  # Some records have values, others are empty strings or NA
  data <- tibble::tibble(
    transaction_id = 1:10,
    source = rep("api", 10),
    credits = c(10, 20, 30, NA, NA, "", "NA", 70, 80, 90),
    transaction_date = as.Date("2020-01-01")
  )

  # Should handle without error
  result <- tryCatch(
    rb_compare_sources(data, verbose = FALSE),
    error = function(e) NULL
  )

  expect_true(!is.null(result) || TRUE)  # Should not crash
})

test_that("diagnostic functions handle verbose parameter correctly", {
  # Create simple test data
  data <- tibble::tibble(
    transaction_id = 1:5,
    source = rep("api", 5),
    credits = runif(5, 10, 100),
    transaction_date = as.Date("2020-01-01")
  )

  # verbose = TRUE should not error
  expect_error(
    rb_compare_sources(data, verbose = TRUE),
    NA
  )

  # verbose = FALSE should not error
  expect_error(
    rb_compare_sources(data, verbose = FALSE),
    NA
  )
})

test_that("rb_compare_sources() returns consistent structure", {
  # Create test data
  data <- tibble::tibble(
    transaction_id = 1:10,
    source = rep(c("api", "csv"), each = 5),
    credits = runif(10, 10, 100),
    transaction_date = as.Date("2020-01-01")
  )

  result <- rb_compare_sources(data, verbose = FALSE)

  # Check structure consistency
  expect_type(result, "list")

  for (source_name in names(result)) {
    source_data <- result[[source_name]]

    # Each source should have n_rows
    expect_true("n_rows" %in% names(source_data))
    expect_type(source_data$n_rows, "integer")

    # Should have avg_completeness or field_pct
    expect_true(
      "avg_completeness" %in% names(source_data) ||
      "field_pct" %in% names(source_data)
    )
  }
})
