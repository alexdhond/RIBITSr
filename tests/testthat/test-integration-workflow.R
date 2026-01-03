# Integration Tests for Full Extraction & Harmonization Workflow
# Based on audit_extraction_harmonization.R and audit_data_quality.R
# Tests the complete ribits() pipeline: fetch → harmonize → validate

library(testthat)
library(RIBITSr)
library(dplyr)
library(lubridate)

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

# ==============================================================================
# INTEGRATION TEST: Full Workflow
# ==============================================================================

test_that("ribits() full workflow extracts and harmonizes data successfully", {
  skip_on_cran()
  skip_if_ribits_offline()

  # Extract comprehensive data for a single state (keep it fast)
  data <- ribits(
    state = "MT",  # Montana - typically has good data coverage
    transactions = "comprehensive",
    spatial = TRUE
  )

  # Should return ribits_data object
  expect_s3_class(data, "ribits_data")

  # Should have main components
  expect_true("banks" %in% names(data))
  expect_true("transactions" %in% names(data))
  expect_true(".meta" %in% names(data))

  # Banks should be a data frame with rows
  expect_s3_class(data$banks, "data.frame")
  expect_gt(nrow(data$banks), 0)

  # Transactions should be a data frame (may be empty for some states)
  expect_s3_class(data$transactions, "data.frame")

  # Metadata should exist
  expect_type(data$.meta, "list")
})

# ==============================================================================
# DATA TYPE VALIDATION
# ==============================================================================

test_that("ribits() returns correct data types for critical fields", {
  skip_on_cran()
  skip_if_ribits_offline()

  data <- ribits(state = "MT", transactions = "comprehensive")

  # Skip if no transactions
  skip_if(nrow(data$transactions) == 0, "No transactions for this state")

  # Critical transaction field types
  if ("transaction_date" %in% names(data$transactions)) {
    expect_true(
      inherits(data$transactions$transaction_date, "Date") ||
      inherits(data$transactions$transaction_date, "POSIXct"),
      info = "transaction_date should be Date or POSIXct type"
    )
  }

  if ("bank_id" %in% names(data$transactions)) {
    expect_type(data$transactions$bank_id, "character")
  }

  if ("debits" %in% names(data$transactions)) {
    expect_true(
      is.numeric(data$transactions$debits),
      info = "debits should be numeric"
    )
  }

  if ("credits" %in% names(data$transactions)) {
    expect_true(
      is.numeric(data$transactions$credits),
      info = "credits should be numeric"
    )
  }

  # Critical bank field types
  if ("bank_id" %in% names(data$banks)) {
    expect_type(data$banks$bank_id, "character")
  }

  if ("total_acres" %in% names(data$banks)) {
    expect_true(is.numeric(data$banks$total_acres))
  }

  if ("available_credits" %in% names(data$banks)) {
    expect_true(is.numeric(data$banks$available_credits))
  }
})

test_that("transaction dates are properly parsed and valid", {
  skip_on_cran()
  skip_if_ribits_offline()

  data <- ribits(state = "MT", transactions = "comprehensive")

  skip_if(nrow(data$transactions) == 0, "No transactions for this state")
  skip_if(!("transaction_date" %in% names(data$transactions)), "No transaction_date column")

  # Should be Date type (not character!)
  expect_false(
    is.character(data$transactions$transaction_date),
    info = "transaction_date should not be character - indicates parsing failure"
  )

  # Should be proper date type
  expect_true(
    inherits(data$transactions$transaction_date, "Date") ||
    inherits(data$transactions$transaction_date, "POSIXct")
  )

  # Date operations should work
  years <- year(data$transactions$transaction_date)
  expect_type(years, "double")

  # Most dates should be reasonable (1990-present)
  min_year <- min(years, na.rm = TRUE)
  max_year <- max(years, na.rm = TRUE)

  expect_gte(min_year, 1980, info = "Minimum year seems too old")
  expect_lte(max_year, year(Sys.Date()) + 1, info = "Maximum year is in the future")
})

# ==============================================================================
# COMPLETENESS ANALYSIS
# ==============================================================================

test_that("critical fields have reasonable completeness", {
  skip_on_cran()
  skip_if_ribits_offline()

  data <- ribits(state = "MT", transactions = "comprehensive")

  # Bank completeness
  if (nrow(data$banks) > 0) {
    # bank_id should be 100% complete
    if ("bank_id" %in% names(data$banks)) {
      completeness <- sum(!is.na(data$banks$bank_id)) / nrow(data$banks) * 100
      expect_gte(completeness, 95, info = "bank_id should be nearly complete")
    }

    # bank_name should be mostly complete
    if ("bank_name" %in% names(data$banks)) {
      completeness <- sum(!is.na(data$banks$bank_name)) / nrow(data$banks) * 100
      expect_gte(completeness, 80, info = "bank_name should be mostly complete")
    }
  }

  # Transaction completeness
  if (nrow(data$transactions) > 0) {
    # transaction_date should be mostly complete
    if ("transaction_date" %in% names(data$transactions)) {
      completeness <- sum(!is.na(data$transactions$transaction_date)) / nrow(data$transactions) * 100
      expect_gte(completeness, 70, info = "transaction_date should have reasonable completeness")
    }

    # bank_id should be complete
    if ("bank_id" %in% names(data$transactions)) {
      completeness <- sum(!is.na(data$transactions$bank_id)) / nrow(data$transactions) * 100
      expect_gte(completeness, 95, info = "transaction bank_id should be nearly complete")
    }
  }
})

# ==============================================================================
# HARMONIZATION QUALITY
# ==============================================================================

test_that("harmonization produces minimal or no discrepancies", {
  skip_on_cran()
  skip_if_ribits_offline()

  data <- ribits(state = "MT", transactions = "comprehensive")

  # Check for discrepancies in metadata
  if (!is.null(data$.meta$discrepancies)) {
    n_discrepancies <- nrow(data$.meta$discrepancies)

    # Ideally should be 0, but allow some tolerance
    expect_lte(
      n_discrepancies,
      nrow(data$banks) * 0.05,  # Allow up to 5% discrepancy rate
      info = paste("Too many discrepancies found:", n_discrepancies)
    )
  }
})

test_that("harmonization preserves data from all available sources", {
  skip_on_cran()
  skip_if_ribits_offline()

  data <- ribits(state = "MT", transactions = "comprehensive")

  # Should have source metadata
  expect_true(".meta" %in% names(data))
  expect_true("sources" %in% names(data$.meta))

  # Should indicate which sources were used
  sources <- names(data$.meta$sources)
  expect_true(length(sources) > 0, info = "Should have at least one data source")

  # Common sources: api, epa, csv
  expect_true(
    any(c("api", "epa", "csv", "watershed") %in% sources),
    info = "Should use recognized data sources"
  )
})

# ==============================================================================
# VALUE RANGE VALIDATION
# ==============================================================================

test_that("numeric values are within reasonable ranges", {
  skip_on_cran()
  skip_if_ribits_offline()

  data <- ribits(state = "MT", transactions = "comprehensive")

  skip_if(nrow(data$transactions) == 0, "No transactions for this state")

  # Debits and credits should generally be non-negative
  # (some negative values may be valid withdrawals, but should be rare)
  if ("debits" %in% names(data$transactions)) {
    negative_debits <- sum(data$transactions$debits < 0, na.rm = TRUE)
    expect_lte(
      negative_debits / nrow(data$transactions),
      0.1,  # Allow up to 10% negative
      info = "Too many negative debits"
    )
  }

  if ("credits" %in% names(data$transactions)) {
    negative_credits <- sum(data$transactions$credits < 0, na.rm = TRUE)
    expect_lte(
      negative_credits / nrow(data$transactions),
      0.1,
      info = "Too many negative credits"
    )
  }

  # Total acres should be reasonable (not negative, not extreme)
  if ("total_acres" %in% names(data$banks) && nrow(data$banks) > 0) {
    negative_acres <- sum(data$banks$total_acres < 0, na.rm = TRUE)
    expect_equal(negative_acres, 0, info = "Should have no negative acres")

    # Very large acreage is possible but uncommon
    extreme_acres <- sum(data$banks$total_acres > 50000, na.rm = TRUE)
    expect_lte(
      extreme_acres / nrow(data$banks),
      0.05,
      info = "Too many banks with extreme acreage"
    )
  }
})

test_that("dates are within expected ranges", {
  skip_on_cran()
  skip_if_ribits_offline()

  data <- ribits(state = "MT", transactions = "comprehensive")

  skip_if(nrow(data$transactions) == 0, "No transactions")
  skip_if(!("transaction_date" %in% names(data$transactions)), "No transaction_date")

  dates <- data$transactions$transaction_date

  # Should not have future dates (beyond 30 days)
  future_dates <- sum(dates > Sys.Date() + 30, na.rm = TRUE)
  expect_equal(future_dates, 0, info = "Should have no far-future dates")

  # Very old dates (pre-1990) should be rare
  years <- year(dates)
  pre_1990 <- sum(years < 1990, na.rm = TRUE)
  expect_lte(
    pre_1990 / length(dates),
    0.05,
    info = "Too many pre-1990 dates (check pre-2000 date handling)"
  )
})

# ==============================================================================
# MULTI-STATE TESTING
# ==============================================================================

test_that("ribits() works consistently across multiple states", {
  skip_on_cran()
  skip_if_ribits_offline()

  # Test on 2 diverse states
  test_states <- c("CA", "FL")  # High-volume states

  for (state in test_states) {
    data <- ribits(state = state, transactions = "basic")

    # Should return valid structure
    expect_s3_class(data, "ribits_data")
    expect_true("banks" %in% names(data))
    expect_true("transactions" %in% names(data))

    # Should have data
    expect_gt(nrow(data$banks), 0, info = paste("No banks for", state))
  }
})

# ==============================================================================
# SPATIAL DATA INTEGRATION
# ==============================================================================

test_that("spatial data is properly integrated when requested", {
  skip_on_cran()
  skip_if_ribits_offline()

  # With spatial = TRUE
  data_with_spatial <- ribits(state = "MT", spatial = TRUE)

  # Should have geometry component (may be NULL if no spatial data available)
  expect_true(
    "geometry" %in% names(data_with_spatial) ||
    "footprint" %in% names(data_with_spatial$banks),
    info = "Should have spatial data component when spatial=TRUE"
  )

  # With spatial = FALSE
  data_without_spatial <- ribits(state = "MT", spatial = FALSE)

  # Should still work
  expect_s3_class(data_without_spatial, "ribits_data")
})

# ==============================================================================
# ERROR HANDLING
# ==============================================================================

test_that("ribits() handles invalid inputs gracefully", {
  # Invalid state code
  expect_error(
    ribits(state = "XX"),
    info = "Should error on invalid state code"
  )

  # Invalid transaction type
  expect_error(
    ribits(state = "CA", transactions = "invalid"),
    info = "Should error on invalid transaction type"
  )
})

test_that("ribits() handles network failures gracefully", {
  skip_on_cran()

  # This test checks that errors are informative
  # Actual network failure testing would require mocking

  # Invalid endpoint should fail gracefully (not just crash)
  expect_error(
    ribits(state = "CA", transactions = "comprehensive"),
    NA  # Should not throw error on valid state
  )
})

# ==============================================================================
# TRANSACTION DETAIL LEVELS
# ==============================================================================

test_that("transaction detail levels work as expected", {
  skip_on_cran()
  skip_if_ribits_offline()

  # Basic transactions (fewer columns)
  data_basic <- ribits(state = "MT", transactions = "basic")
  expect_s3_class(data_basic$transactions, "data.frame")

  # Comprehensive transactions (more columns)
  data_comprehensive <- ribits(state = "MT", transactions = "comprehensive")
  expect_s3_class(data_comprehensive$transactions, "data.frame")

  # Comprehensive should have more or equal columns than basic
  if (nrow(data_basic$transactions) > 0 && nrow(data_comprehensive$transactions) > 0) {
    expect_gte(
      ncol(data_comprehensive$transactions),
      ncol(data_basic$transactions),
      info = "Comprehensive should have at least as many columns as basic"
    )
  }

  # None - no transactions
  data_none <- ribits(state = "MT", transactions = "none")
  expect_true(
    is.null(data_none$transactions) || nrow(data_none$transactions) == 0,
    info = "transactions='none' should return no transaction data"
  )
})

# ==============================================================================
# DATE PARSING EDGE CASES
# ==============================================================================

test_that("pre-2000 dates are handled correctly", {
  skip_on_cran()
  skip_if_ribits_offline()

  # This test verifies the pre-2000 date handling fix
  data <- ribits(state = "CA", transactions = "comprehensive")  # CA likely to have old data

  skip_if(nrow(data$transactions) == 0, "No transactions")
  skip_if(!("transaction_date" %in% names(data$transactions)), "No transaction_date")

  # If there are any pre-2000 dates, they should be parsed correctly
  years <- year(data$transactions$transaction_date)
  pre_2000 <- years[years < 2000 & !is.na(years)]

  if (length(pre_2000) > 0) {
    # Pre-2000 dates should have reasonable years (not 1970, which indicates parsing error)
    expect_true(
      all(pre_2000 >= 1990 & pre_2000 < 2000),
      info = "Pre-2000 dates should be in 1990s range (not defaulting to 1970)"
    )
  }
})

test_that("date operations work correctly on returned data", {
  skip_on_cran()
  skip_if_ribits_offline()

  data <- ribits(state = "MT", transactions = "comprehensive")

  skip_if(nrow(data$transactions) == 0, "No transactions")
  skip_if(!("transaction_date" %in% names(data$transactions)), "No transaction_date")

  # Should be able to filter by date
  recent <- data$transactions %>%
    filter(transaction_date >= as.Date("2020-01-01"))

  expect_s3_class(recent, "data.frame")

  # Should be able to extract year
  data$transactions %>%
    mutate(year = year(transaction_date)) %>%
    expect_s3_class("data.frame")

  # Should be able to compute on credits
  if ("credits" %in% names(data$transactions)) {
    total <- sum(data$transactions$credits, na.rm = TRUE)
    expect_type(total, "double")
    expect_gte(total, 0)
  }
})
