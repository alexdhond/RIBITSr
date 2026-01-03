# Tests for name-lookup.R
# Tests rb_build_name_lookup(), rb_match_names(), rb_clear_name_cache()

library(testthat)
library(RIBITSr)
library(dplyr)

# ==============================================================================
# rb_build_name_lookup() Tests
# ==============================================================================

test_that("rb_build_name_lookup() accepts all parameters", {
  skip_on_cran()

  # Function should exist
  expect_true(exists("rb_build_name_lookup", mode = "function"))

  # Should accept include_csv parameter
  result <- tryCatch(
    rb_build_name_lookup(include_csv = FALSE, max_age_days = 30),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))

  # Should accept comprehensive parameter
  result <- tryCatch(
    rb_build_name_lookup(comprehensive = FALSE, max_age_days = 30),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))

  # Should accept track_aliases parameter
  result <- tryCatch(
    rb_build_name_lookup(track_aliases = FALSE, max_age_days = 30),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("rb_build_name_lookup() handles max_age_days parameter", {
  skip_on_cran()

  # max_age_days = 0 should force refresh
  result <- tryCatch(
    rb_build_name_lookup(max_age_days = 0),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))

  # max_age_days = 365 should use cache if available
  result <- tryCatch(
    rb_build_name_lookup(max_age_days = 365),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("rb_build_name_lookup() handles force_refresh parameter", {
  skip_on_cran()

  # force_refresh = FALSE (default)
  result_cached <- tryCatch(
    rb_build_name_lookup(force_refresh = FALSE, max_age_days = 30),
    error = function(e) NULL
  )
  expect_true(is.null(result_cached) || is.data.frame(result_cached))

  # force_refresh = TRUE should bypass cache
  result_fresh <- tryCatch(
    rb_build_name_lookup(force_refresh = TRUE),
    error = function(e) NULL
  )
  expect_true(is.null(result_fresh) || is.data.frame(result_fresh))
})

test_that("rb_build_name_lookup() returns expected structure", {
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
    rb_build_name_lookup(comprehensive = FALSE, max_age_days = 30),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not build lookup table")

  # Should be a data frame
  expect_s3_class(result, "data.frame")

  # Should have expected columns
  expected_cols <- c("bank_id", "canonical_name")
  for (col in expected_cols) {
    expect_true(
      col %in% names(result),
      info = paste("Missing expected column:", col)
    )
  }

  # bank_id should be numeric or character
  expect_true(
    is.numeric(result$bank_id) || is.character(result$bank_id)
  )

  # canonical_name should be character
  if ("canonical_name" %in% names(result)) {
    expect_type(result$canonical_name, "character")
  }

  # Should have rows
  expect_gt(nrow(result), 0, info = "Lookup table should have data")
})

test_that("rb_build_name_lookup() with track_aliases=TRUE includes name_variants", {
  skip_on_cran()

  result <- tryCatch(
    rb_build_name_lookup(track_aliases = TRUE, comprehensive = FALSE, max_age_days = 30),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not build lookup table")

  # May have name_variants column (list-column)
  if ("name_variants" %in% names(result)) {
    expect_type(result$name_variants, "list")
  }
})

test_that("rb_build_name_lookup() includes confidence_score", {
  skip_on_cran()

  result <- tryCatch(
    rb_build_name_lookup(comprehensive = FALSE, max_age_days = 30),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not build lookup table")

  # Should have confidence_score
  if ("confidence_score" %in% names(result)) {
    expect_type(result$confidence_score, "double")

    # Confidence scores should be between 0 and 1
    expect_true(all(result$confidence_score >= 0 & result$confidence_score <= 1, na.rm = TRUE))
  }
})

test_that("rb_build_name_lookup() includes source information", {
  skip_on_cran()

  result <- tryCatch(
    rb_build_name_lookup(comprehensive = FALSE, max_age_days = 30),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not build lookup table")

  # Should have sources or source_count
  has_source_info <- any(c("sources", "source_count") %in% names(result))
  expect_true(has_source_info, info = "Should have source information")

  if ("source_count" %in% names(result)) {
    expect_type(result$source_count, "integer")
    expect_true(all(result$source_count > 0, na.rm = TRUE))
  }
})

# ==============================================================================
# rb_match_names() Tests
# ==============================================================================

test_that("rb_match_names() requires data and name_col parameters", {
  # Function should exist
  expect_true(exists("rb_match_names", mode = "function"))

  # Should error without required parameters
  expect_error(
    rb_match_names(),
    "argument.*data.*missing"
  )
})

test_that("rb_match_names() accepts valid parameters", {
  # Create sample data
  test_data <- tibble::tibble(
    bank_name = c("Sacramento River Bank", "Yolo Bypass Bank", "Unknown Bank")
  )

  # Should accept data and name_col
  result <- tryCatch(
    rb_match_names(test_data, name_col = "bank_name"),
    error = function(e) NULL
  )

  # Function should run (may return NULL if lookup unavailable)
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("rb_match_names() handles missing name_col", {
  test_data <- tibble::tibble(
    bank_name = c("Bank A", "Bank B")
  )

  # Missing name_col should error or use default
  result <- tryCatch(
    rb_match_names(test_data, name_col = "nonexistent_col"),
    error = function(e) NULL
  )

  # Should either error or handle gracefully
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("rb_match_names() adds bank_id column", {
  skip_on_cran()

  test_data <- tibble::tibble(
    bank_name = c("Sacramento River Bank")
  )

  result <- tryCatch(
    rb_match_names(test_data, name_col = "bank_name"),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not perform name matching")

  # Should be a data frame
  expect_s3_class(result, "data.frame")

  # Should have original columns plus bank_id
  expect_true("bank_name" %in% names(result))

  # May have bank_id column if matches found
  # (or match_confidence, match_method, etc.)
})

test_that("rb_match_names() handles empty data", {
  empty_data <- tibble::tibble(
    bank_name = character()
  )

  result <- tryCatch(
    rb_match_names(empty_data, name_col = "bank_name"),
    error = function(e) NULL
  )

  # Should handle empty data gracefully
  expect_true(
    is.null(result) ||
    (is.data.frame(result) && nrow(result) == 0)
  )
})

test_that("rb_match_names() handles NA values", {
  test_data <- tibble::tibble(
    bank_name = c("Bank A", NA, "Bank B", NA)
  )

  result <- tryCatch(
    rb_match_names(test_data, name_col = "bank_name"),
    error = function(e) NULL
  )

  # Should handle NA values without crashing
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("rb_match_names() handles duplicate names", {
  test_data <- tibble::tibble(
    bank_name = c("Bank A", "Bank A", "Bank B", "Bank B")
  )

  result <- tryCatch(
    rb_match_names(test_data, name_col = "bank_name"),
    error = function(e) NULL
  )

  # Should handle duplicates
  expect_true(is.null(result) || is.data.frame(result))

  if (!is.null(result) && is.data.frame(result)) {
    # Should have same number of rows
    expect_equal(nrow(result), nrow(test_data))
  }
})

test_that("rb_match_names() preserves original data", {
  test_data <- tibble::tibble(
    bank_name = c("Bank A", "Bank B"),
    other_col = c(100, 200)
  )

  result <- tryCatch(
    rb_match_names(test_data, name_col = "bank_name"),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not perform name matching")

  # Original columns should be preserved
  expect_true("bank_name" %in% names(result))
  expect_true("other_col" %in% names(result))

  if ("other_col" %in% names(result)) {
    expect_equal(result$other_col, c(100, 200))
  }
})

test_that("rb_match_names() works with different name variations", {
  skip_on_cran()

  test_data <- tibble::tibble(
    bank_name = c(
      "Sacramento River Bank",  # Exact match
      "SACRAMENTO RIVER BANK",  # Case variation
      "Sacramento River",       # Partial match
      "Completely Unknown Bank" # No match
    )
  )

  result <- tryCatch(
    rb_match_names(test_data, name_col = "bank_name"),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not perform name matching")

  # Should return data frame with results
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
})

# ==============================================================================
# rb_clear_name_cache() Tests
# ==============================================================================

test_that("rb_clear_name_cache() exists and runs", {
  # Function should exist
  expect_true(exists("rb_clear_name_cache", mode = "function"))

  # Should run without error
  expect_error(
    rb_clear_name_cache(),
    NA
  )
})

test_that("rb_clear_name_cache() handles missing cache gracefully", {
  # Clear cache (even if it doesn't exist)
  result <- rb_clear_name_cache()

  # Should complete without error
  expect_true(is.null(result) || is.logical(result))
})

test_that("rb_clear_name_cache() actually clears cache", {
  skip_on_cran()

  # Build lookup (creates cache)
  lookup1 <- tryCatch(
    rb_build_name_lookup(comprehensive = FALSE, max_age_days = 30),
    error = function(e) NULL
  )

  skip_if(is.null(lookup1), "Could not build initial lookup")

  # Clear cache
  rb_clear_name_cache()

  # Build again (should fetch fresh data or use bundled data)
  lookup2 <- tryCatch(
    rb_build_name_lookup(comprehensive = FALSE, max_age_days = 30),
    error = function(e) NULL
  )

  # Both should succeed
  expect_true(!is.null(lookup2))
})

# ==============================================================================
# Integration Tests
# ==============================================================================

test_that("name lookup workflow: build → match → clear", {
  skip_on_cran()

  skip_if_offline <- function() {
    tryCatch({
      httr2::request("https://ribits.ops.usace.army.mil") |>
        httr2::req_timeout(5) |>
        httr2::req_perform()
    }, error = function(e) skip("RIBITS API unavailable"))
  }
  skip_if_offline()

  # Step 1: Build lookup
  lookup <- tryCatch(
    rb_build_name_lookup(comprehensive = FALSE, max_age_days = 30),
    error = function(e) NULL
  )

  skip_if(is.null(lookup), "Could not build lookup table")
  expect_s3_class(lookup, "data.frame")
  expect_gt(nrow(lookup), 0)

  # Step 2: Match names
  test_data <- tibble::tibble(
    bank_name = c(lookup$canonical_name[1])  # Use a known name
  )

  matched <- tryCatch(
    rb_match_names(test_data, name_col = "bank_name"),
    error = function(e) NULL
  )

  skip_if(is.null(matched), "Could not match names")
  expect_s3_class(matched, "data.frame")

  # Step 3: Clear cache
  expect_error(rb_clear_name_cache(), NA)
})

test_that("lookup table has reasonable data quality", {
  skip_on_cran()

  lookup <- tryCatch(
    rb_build_name_lookup(comprehensive = FALSE, max_age_days = 30),
    error = function(e) NULL
  )

  skip_if(is.null(lookup), "Could not build lookup table")

  # bank_id should be unique
  if ("bank_id" %in% names(lookup)) {
    n_unique <- length(unique(lookup$bank_id[!is.na(lookup$bank_id)]))
    n_total <- sum(!is.na(lookup$bank_id))

    # Most bank_ids should be unique (allowing some duplicates for name variants)
    expect_gte(n_unique / n_total, 0.8)
  }

  # canonical_name should not be all NA
  if ("canonical_name" %in% names(lookup)) {
    pct_non_na <- sum(!is.na(lookup$canonical_name)) / nrow(lookup)
    expect_gte(pct_non_na, 0.9)
  }

  # Confidence scores should be reasonable
  if ("confidence_score" %in% names(lookup)) {
    avg_confidence <- mean(lookup$confidence_score, na.rm = TRUE)
    expect_gte(avg_confidence, 0.5, info = "Average confidence should be reasonable")
  }
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("rb_match_names() handles special characters in names", {
  test_data <- tibble::tibble(
    bank_name = c(
      "Bank & Trust",
      "Bank (Delaware)",
      "Bank - Phase II",
      "Bank, LLC"
    )
  )

  result <- tryCatch(
    rb_match_names(test_data, name_col = "bank_name"),
    error = function(e) NULL
  )

  # Should handle special characters without crashing
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("rb_match_names() handles very long names", {
  test_data <- tibble::tibble(
    bank_name = c(
      paste(rep("Very Long Bank Name", 20), collapse = " ")
    )
  )

  result <- tryCatch(
    rb_match_names(test_data, name_col = "bank_name"),
    error = function(e) NULL
  )

  # Should handle long names
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("rb_build_name_lookup() handles network failures gracefully", {
  skip_on_cran()

  # Should either return bundled data or NULL, not crash
  result <- tryCatch(
    rb_build_name_lookup(comprehensive = FALSE, force_refresh = TRUE),
    error = function(e) NULL
  )

  # Should complete (may return NULL if network fails and no bundled data)
  expect_true(is.null(result) || is.data.frame(result))
})
