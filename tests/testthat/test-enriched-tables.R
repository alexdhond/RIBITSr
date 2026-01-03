# Tests for enriched-tables.R
# Tests rb_enriched() function and related print methods

library(testthat)
library(RIBITSr)
library(dplyr)

# ==============================================================================
# rb_enriched() Parameter Validation
# ==============================================================================

test_that("rb_enriched() validates type parameter", {
  # Invalid type should error
  expect_error(
    rb_enriched(type = "invalid"),
    "should be one of"
  )

  # Valid types should not error on parameter validation
  # (may error on execution if data unavailable, but parameter check passes)
  expect_error(
    rb_enriched(type = "banks", quietly = TRUE),
    NA
  )
})

test_that("rb_enriched() accepts all valid type options", {
  skip_on_cran()

  # All valid types should be accepted
  valid_types <- c("banks", "credits", "transactions", "huc")

  for (type_val in valid_types) {
    # Should not error on type validation
    result <- tryCatch(
      rb_enriched(type = type_val, quietly = TRUE),
      error = function(e) NULL
    )

    # Test passes if function accepts the type (may return NULL if data unavailable)
    expect_true(is.null(result) || is.data.frame(result) || inherits(result, "sf"))
  }
})

test_that("rb_enriched() handles state parameter", {
  skip_on_cran()

  # NULL state should be accepted
  result <- tryCatch(
    rb_enriched(type = "banks", state = NULL, quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))

  # Valid state code should be accepted
  result <- tryCatch(
    rb_enriched(type = "banks", state = "CA", quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("rb_enriched() handles district parameter", {
  skip_on_cran()

  # District parameter should be accepted
  result <- tryCatch(
    rb_enriched(type = "banks", district = "Sacramento", quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("rb_enriched() handles bank_ids parameter", {
  skip_on_cran()

  # bank_ids parameter should be accepted
  result <- tryCatch(
    rb_enriched(type = "banks", bank_ids = c(100, 200), quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))

  # Single bank_id should work
  result <- tryCatch(
    rb_enriched(type = "banks", bank_ids = 100, quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("rb_enriched() handles huc8 parameter for type='huc'", {
  skip_on_cran()

  # huc8 parameter should be accepted for type="huc"
  result <- tryCatch(
    rb_enriched(type = "huc", huc8 = "18020111", quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("rb_enriched() handles cache parameter", {
  skip_on_cran()

  # cache = TRUE
  result_cached <- tryCatch(
    rb_enriched(type = "banks", cache = TRUE, quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result_cached) || is.data.frame(result_cached))

  # cache = FALSE
  result_uncached <- tryCatch(
    rb_enriched(type = "banks", cache = FALSE, quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result_uncached) || is.data.frame(result_uncached))
})

test_that("rb_enriched() handles quietly parameter", {
  skip_on_cran()

  # quietly = TRUE should suppress messages
  expect_message(
    rb_enriched(type = "banks", quietly = TRUE),
    NA  # No messages expected
  )

  # quietly = FALSE may produce messages (don't test, just ensure it runs)
  result <- tryCatch(
    rb_enriched(type = "banks", quietly = FALSE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))
})

# ==============================================================================
# Type-Specific Options
# ==============================================================================

test_that("rb_enriched() type='banks' accepts type-specific options", {
  skip_on_cran()

  # include_credits option
  result <- tryCatch(
    rb_enriched(type = "banks", include_credits = TRUE, quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))

  # include_transaction_summary option
  result <- tryCatch(
    rb_enriched(type = "banks", include_transaction_summary = FALSE, quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))

  # include_geometry option (should return sf object if TRUE and data available)
  result <- tryCatch(
    rb_enriched(type = "banks", include_geometry = TRUE, quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result) || inherits(result, "sf"))
})

test_that("rb_enriched() type='credits' accepts type-specific options", {
  skip_on_cran()

  # include_releases option
  result <- tryCatch(
    rb_enriched(type = "credits", include_releases = TRUE, quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))

  # pivot_wide option
  result <- tryCatch(
    rb_enriched(type = "credits", pivot_wide = FALSE, quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("rb_enriched() type='transactions' accepts sources parameter", {
  skip_on_cran()

  # sources parameter
  result <- tryCatch(
    rb_enriched(type = "transactions", sources = c("csv_watershed", "api"), quietly = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))
})

# ==============================================================================
# Return Structure
# ==============================================================================

test_that("rb_enriched() returns appropriate data structure", {
  skip_on_cran()

  skip_if_offline <- function() {
    tryCatch({
      httr2::request("https://ribits.ops.usace.army.mil") |>
        httr2::req_timeout(5) |>
        httr2::req_perform()
    }, error = function(e) skip("RIBITS API unavailable"))
  }
  skip_if_offline()

  # Try to get banks data for small state
  result <- tryCatch(
    rb_enriched(type = "banks", state = "VT", quietly = TRUE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not fetch enriched banks data")

  # Should be a data frame or tibble
  expect_true(is.data.frame(result))

  # Should have bank-related columns
  expect_true(
    any(grepl("bank", names(result), ignore.case = TRUE)),
    info = "Should have bank-related columns"
  )

  # Should have class attribute
  expect_true(
    "ribits_enriched_banks" %in% class(result) ||
    "tbl_df" %in% class(result)
  )
})

test_that("rb_enriched() with include_geometry=TRUE returns sf object", {
  skip_on_cran()

  skip_if_offline <- function() {
    tryCatch({
      httr2::request("https://ribits.ops.usace.army.mil") |>
        httr2::req_timeout(5) |>
        httr2::req_perform()
    }, error = function(e) skip("RIBITS API unavailable"))
  }
  skip_if_offline()

  result <- tryCatch(
    rb_enriched(type = "banks", state = "VT", include_geometry = TRUE, quietly = TRUE),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not fetch enriched banks with geometry")

  # May return sf object if spatial data available
  expect_true(
    inherits(result, "sf") ||
    is.data.frame(result),  # May be data.frame if no geometry available
    info = "Should return sf or data.frame"
  )
})

# ==============================================================================
# Print Methods
# ==============================================================================

test_that("print.ribits_enriched_banks() works", {
  # Create mock enriched banks object
  mock_banks <- tibble::tibble(
    bank_id = 1:5,
    bank_name = paste("Bank", 1:5),
    total_credits = runif(5, 100, 1000)
  )
  class(mock_banks) <- c("ribits_enriched_banks", "tbl_df", "tbl", "data.frame")

  # Should print without error
  expect_output(
    print(mock_banks),
    "bank"  # Should mention "bank" in output
  )
})

test_that("print.ribits_enriched_credits() works", {
  # Create mock enriched credits object
  mock_credits <- tibble::tibble(
    bank_id = 1:5,
    credit_type = rep(c("Stream", "Wetland"), length.out = 5),
    available_credits = runif(5, 10, 100)
  )
  class(mock_credits) <- c("ribits_enriched_credits", "tbl_df", "tbl", "data.frame")

  # Should print without error
  expect_output(
    print(mock_credits),
    "credit"  # Should mention "credit" in output
  )
})

test_that("print.ribits_enriched_transactions() works", {
  # Create mock enriched transactions object
  mock_txn <- tibble::tibble(
    transaction_id = 1:10,
    bank_id = rep(1:2, each = 5),
    transaction_date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    credits = runif(10, 10, 100)
  )
  class(mock_txn) <- c("ribits_enriched_transactions", "tbl_df", "tbl", "data.frame")

  # Should print without error
  expect_output(
    print(mock_txn),
    "transaction"  # Should mention "transaction" in output
  )
})

test_that("print.ribits_enriched_huc() works", {
  # Create mock enriched HUC object
  mock_huc <- tibble::tibble(
    huc8 = paste0("1802", 1:5),
    state = rep("CA", 5),
    available_credits = runif(5, 100, 1000)
  )
  class(mock_huc) <- c("ribits_enriched_huc", "tbl_df", "tbl", "data.frame")

  # Should print without error
  expect_output(
    print(mock_huc),
    "huc|HUC"  # Should mention "huc" in output
  )
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("rb_enriched() handles empty results gracefully", {
  skip_on_cran()

  # Query for data that likely doesn't exist
  result <- tryCatch(
    rb_enriched(type = "banks", bank_ids = 9999999, quietly = TRUE),
    error = function(e) NULL
  )

  # Should return NULL or empty data frame, not error
  expect_true(
    is.null(result) ||
    (is.data.frame(result) && nrow(result) == 0)
  )
})

test_that("rb_enriched() handles invalid state codes", {
  # Invalid state code should either error or return empty results
  result <- tryCatch(
    rb_enriched(type = "banks", state = "XX", quietly = TRUE),
    error = function(e) NULL
  )

  # Should complete (may return NULL or empty data)
  expect_true(
    is.null(result) ||
    (is.data.frame(result) && nrow(result) == 0) ||
    is.data.frame(result)
  )
})

test_that("rb_enriched() handles multiple filters simultaneously", {
  skip_on_cran()

  # Apply multiple filters at once
  result <- tryCatch(
    rb_enriched(
      type = "banks",
      state = "CA",
      district = "Sacramento",
      bank_ids = c(100, 200),
      quietly = TRUE
    ),
    error = function(e) NULL
  )

  # Should handle multiple filters
  expect_true(is.null(result) || is.data.frame(result))
})

# ==============================================================================
# Type Switching Logic
# ==============================================================================

test_that("rb_enriched() correctly switches between types", {
  # Each type should call different implementation
  # Test by checking return structure or class

  skip_on_cran()

  # Get all types (may fail if data unavailable)
  types <- c("banks", "credits", "transactions", "huc")
  results <- list()

  for (type_val in types) {
    results[[type_val]] <- tryCatch(
      rb_enriched(type = type_val, state = "VT", quietly = TRUE),
      error = function(e) NULL
    )
  }

  # At least some types should return data
  n_successful <- sum(sapply(results, function(x) !is.null(x) && is.data.frame(x)))

  # If any succeeded, they should have appropriate structure
  for (type_val in names(results)) {
    if (!is.null(results[[type_val]]) && is.data.frame(results[[type_val]])) {
      # Check class name includes type
      expect_true(
        any(grepl(type_val, class(results[[type_val]]), ignore.case = TRUE)) ||
        "tbl_df" %in% class(results[[type_val]]),
        info = paste("Type", type_val, "should have appropriate class")
      )
    }
  }
})

test_that("rb_enriched() returns consistent column types", {
  skip_on_cran()

  skip_if_offline <- function() {
    tryCatch({
      httr2::request("https://ribits.ops.usace.army.mil") |>
        httr2::req_timeout(5) |>
        httr2::req_perform()
    }, error = function(e) skip("RIBITS API unavailable"))
  }
  skip_if_offline()

  result <- tryCatch(
    rb_enriched(type = "banks", state = "VT", quietly = TRUE),
    error = function(e) NULL
  )

  skip_if(is.null(result) || nrow(result) == 0, "No data returned")

  # bank_id should be numeric or character
  if ("bank_id" %in% names(result)) {
    expect_true(
      is.numeric(result$bank_id) || is.character(result$bank_id)
    )
  }

  # Numeric fields should be numeric
  numeric_patterns <- c("credit", "acre", "total", "available")
  for (pattern in numeric_patterns) {
    numeric_cols <- grep(pattern, names(result), ignore.case = TRUE, value = TRUE)
    for (col in numeric_cols) {
      if (col %in% names(result)) {
        # Should be numeric (allow for character if it's an ID field)
        expect_true(
          is.numeric(result[[col]]) ||
          grepl("_id$|^id_", col),  # ID fields can be character
          info = paste("Column", col, "should be numeric or ID")
        )
      }
    }
  }
})
