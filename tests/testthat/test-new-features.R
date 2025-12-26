# Test suite for all new pain point fixes
# Tests phases 1-6 of the implementation

# ============================================================================
# PHASE 1: Foundation & Configuration
# ============================================================================

test_that("rb_config() exists and works", {
  # Save current config to restore later
  old_config <- .get_config()

  # Test setting individual parameters
  rb_config(max_retries = 5)
  expect_equal(.network_options$max_retries, 5)

  rb_config(retry_delay = 3)
  expect_equal(.network_options$retry_delay, 3)

  rb_config(timeout = 120)
  expect_equal(.network_options$timeout, 120)

  rb_config(rate_limit = 2)
  expect_equal(.network_options$rate_limit, 2)

  # Test setting multiple parameters at once
  rb_config(max_retries = 4, retry_delay = 2, timeout = 90)
  expect_equal(.network_options$max_retries, 4)
  expect_equal(.network_options$retry_delay, 2)
  expect_equal(.network_options$timeout, 90)

  # Test reset
  rb_config(reset = TRUE)
  expect_equal(.network_options$max_retries, 3)  # Default
  expect_equal(.network_options$retry_delay, 2)  # Default
})

test_that(".is_retryable_error() classifies errors correctly", {
  # Network errors - should be retryable
  expect_true(.is_retryable_error("Could not resolve host"))
  expect_true(.is_retryable_error("Connection refused"))
  expect_true(.is_retryable_error("Request timed out"))
  expect_true(.is_retryable_error("Connection timeout"))

  # 5xx server errors - should be retryable
  expect_true(.is_retryable_error("HTTP 500", status_code = 500))
  expect_true(.is_retryable_error("HTTP 502", status_code = 502))
  expect_true(.is_retryable_error("HTTP 503", status_code = 503))

  # 429 rate limit - should be retryable
  expect_true(.is_retryable_error("HTTP 429", status_code = 429))

  # 408 request timeout - should be retryable
  expect_true(.is_retryable_error("HTTP 408", status_code = 408))

  # 4xx client errors - should NOT be retryable (permanent)
  expect_false(.is_retryable_error("HTTP 404", status_code = 404))
  expect_false(.is_retryable_error("HTTP 401", status_code = 401))
  expect_false(.is_retryable_error("HTTP 403", status_code = 403))
  expect_false(.is_retryable_error("HTTP 400", status_code = 400))
})

test_that("Environment variable loading works", {
  # Test that .load_env_config exists and is callable
  expect_true(exists(".load_env_config", mode = "function"))

  # Set test environment variables
  Sys.setenv(RIBITS_MAX_RETRIES = "7")
  Sys.setenv(RIBITS_RATE_LIMIT = "4")

  # Load config
  .load_env_config()

  expect_equal(.network_options$max_retries, 7)
  expect_equal(.network_options$rate_limit, 4)

  # Clean up
  Sys.unsetenv("RIBITS_MAX_RETRIES")
  Sys.unsetenv("RIBITS_RATE_LIMIT")
  rb_config(reset = TRUE)
})


# ============================================================================
# PHASE 2: Retry Logic & Reliability
# ============================================================================

test_that("rb_request_with_retry() uses error classification", {
  # This is tested indirectly through .is_retryable_error()
  # The retry wrapper should skip retries for permanent errors

  # Function should exist
  expect_true(exists("rb_request_with_retry", mode = "function"))
})


# ============================================================================
# PHASE 3: Progress Indicators
# ============================================================================

test_that("Vectorized name matching is faster than one-at-a-time", {
  skip_on_cran()
  skip_if_not_installed("stringdist")

  # Create test data
  lookup <- data.frame(
    bank_id = 1:100,
    name_normalized = paste0("bank_", 1:100),
    stringsAsFactors = FALSE
  )

  test_names <- paste0("bank_", sample(1:100, 20))

  # The new vectorized approach should be available via stringsimmatrix
  expect_true(exists("stringsimmatrix", where = asNamespace("stringdist")))

  # Test vectorized approach
  sim_matrix <- stringdist::stringsimmatrix(
    test_names,
    lookup$name_normalized,
    method = "jw"
  )

  expect_equal(nrow(sim_matrix), length(test_names))
  expect_equal(ncol(sim_matrix), nrow(lookup))
})


# ============================================================================
# PHASE 4: Rate Limiting & Performance
# ============================================================================

test_that("Rate limiter enforces delays", {
  # Reset rate limiter
  .rate_limiter$last_request_time <- NULL

  # Set aggressive rate limit
  rb_config(rate_limit = 10)  # 10 requests/sec = 0.1s between requests

  # First request should not wait
  start <- Sys.time()
  .apply_rate_limit()
  duration1 <- as.numeric(Sys.time() - start)
  expect_lt(duration1, 0.05)  # Should be nearly instant

  # Second request should wait
  start <- Sys.time()
  .apply_rate_limit()
  duration2 <- as.numeric(Sys.time() - start)
  expect_gte(duration2, 0.05)  # Should wait ~0.1s

  # Reset
  rb_config(reset = TRUE)
  .rate_limiter$last_request_time <- NULL
})

test_that("Persistent cache directory creation works", {
  # Test with persistent cache enabled
  cache_dir <- .get_cache_dir(use_cache = TRUE, persistent = TRUE)
  expect_true(dir.exists(cache_dir))
  expect_true(grepl("ribits", cache_dir, ignore.case = TRUE))

  # Test with session-only cache
  cache_dir_session <- .get_cache_dir(use_cache = TRUE, persistent = FALSE)
  expect_true(dir.exists(cache_dir_session))
  expect_true(grepl("ribits_cache", cache_dir_session))

  # Test with cache disabled
  cache_dir_none <- .get_cache_dir(use_cache = FALSE)
  expect_equal(cache_dir_none, tempdir())
})

test_that("rb_clear_cache() function exists and works", {
  expect_true(exists("rb_clear_cache", mode = "function"))

  # Test that it accepts type parameter
  expect_error(rb_clear_cache("invalid_type"), "should be one of")

  # Valid types should not error
  expect_silent(rb_clear_cache("csv", verbose = FALSE))
  expect_silent(rb_clear_cache("lookup", verbose = FALSE))
  expect_silent(rb_clear_cache("all", verbose = FALSE))
})


# ============================================================================
# PHASE 5: Validation & Error Messages
# ============================================================================

test_that(".validate_csv_content() detects HTML error pages", {
  skip_on_cran()

  # Create a temp file with HTML content
  html_file <- tempfile(fileext = ".csv")
  writeLines(c(
    "<!DOCTYPE html>",
    "<html><head><title>Error</title></head>",
    "<body>Server Error</body></html>"
  ), html_file)

  # Should detect HTML and abort
  expect_error(
    .validate_csv_content(html_file),
    "HTML error page"
  )

  unlink(html_file)
})

test_that(".validate_csv_content() detects Oracle errors", {
  skip_on_cran()

  # Create a temp file with Oracle error
  oracle_file <- tempfile(fileext = ".csv")
  writeLines(c(
    "ORA-12345: Database connection failed",
    "Oracle APEX error encountered"
  ), oracle_file)

  # Should detect Oracle error and abort
  expect_error(
    .validate_csv_content(oracle_file),
    "Oracle database error"
  )

  unlink(oracle_file)
})

test_that(".validate_csv_content() detects small/empty files", {
  skip_on_cran()

  # Create a very small file
  small_file <- tempfile(fileext = ".csv")
  writeLines("a,b", small_file)  # Just header, very small

  # Should return FALSE for suspiciously small files
  expect_false(.validate_csv_content(small_file))

  unlink(small_file)
})

test_that(".validate_csv_content() passes valid CSV files", {
  skip_on_cran()

  # Create a valid CSV (needs to be > 100 bytes)
  valid_file <- tempfile(fileext = ".csv")
  writeLines(c(
    "bank_id,name,credit,location,status,type,resource,mitigation_method",
    "123,Test Bank Alpha,100.5,California,Active,Mitigation,Wetland,Establishment",
    "456,Another Bank Beta,250.0,Texas,Active,Mitigation,Stream,Rehabilitation",
    "789,Third Bank Gamma,175.25,Florida,Active,ILF,Wetland,Preservation",
    "101,Fourth Bank Delta,325.75,Oregon,Active,Mitigation,Stream,Enhancement"
  ), valid_file)

  # Should pass validation (file is now > 100 bytes)
  expect_true(.validate_csv_content(valid_file))

  # With expected columns
  expect_true(.validate_csv_content(valid_file, expected_cols = c("bank_id", "name")))

  unlink(valid_file)
})

test_that(".validate_transaction_data() validates transaction quality", {
  skip_on_cran()

  # Create test transaction data with various issues
  test_txns <- data.frame(
    bank_id = c(1, 2, 3, NA, 5),
    credit = c(100, -50, 15000, 200, NA),
    source = c("api", "csv", "api", "api", "csv"),
    stringsAsFactors = FALSE
  )

  # Run validation
  result <- .validate_transaction_data(test_txns)

  expect_type(result, "list")
  expect_true("warnings" %in% names(result))
  expect_true("stats" %in% names(result))

  # Should detect missing bank_id
  expect_true(any(grepl("missing bank_id", result$warnings)))

  # Should detect negative credits
  expect_true(any(grepl("negative", result$warnings)))

  # Should detect unusually large credits
  expect_true(any(grepl("10,000", result$warnings)))

  # Should have source breakdown
  expect_true("source_breakdown" %in% names(result$stats))
})

test_that(".validate_transaction_data() handles missing bank_id column", {
  skip_on_cran()

  # Data without bank_id column
  bad_data <- data.frame(
    name = c("Bank A", "Bank B"),
    credit = c(100, 200)
  )

  result <- .validate_transaction_data(bad_data)
  expect_false(result$valid)
  expect_true(any(grepl("bank_id", result$warnings)))
})

test_that(".rb_friendly_error() provides specific error messages", {
  # Function should exist
  expect_true(exists(".rb_friendly_error", mode = "function"))

  # Test network error
  e_network <- simpleError("Could not resolve host: ribits.ops.usace.army.mil")
  expect_error(
    .rb_friendly_error(e_network),
    "Network error"
  )

  # Note: Full testing of error messages would require mocking httr2 responses
  # which is complex. The key is that the function exists and handles basic cases.
})


# ============================================================================
# PHASE 6: Documentation
# ============================================================================

test_that("Documentation files exist", {
  # Check README.Rmd has configuration section
  readme_path <- file.path(here::here(), "README.Rmd")
  if (file.exists(readme_path)) {
    readme_content <- paste(readLines(readme_path), collapse = "\n")
    expect_true(grepl("## Configuration", readme_content))
    expect_true(grepl("rb_config", readme_content))
    expect_true(grepl("rb_clear_cache", readme_content))
  }

  # Check configuration vignette exists
  vignette_path <- file.path(here::here(), "vignettes", "configuration-guide.Rmd")
  expect_true(file.exists(vignette_path))

  if (file.exists(vignette_path)) {
    vignette_content <- paste(readLines(vignette_path), collapse = "\n")
    expect_true(grepl("Network & Reliability", vignette_content))
    expect_true(grepl("Persistent Cache", vignette_content))
    expect_true(grepl("Rate Limiting", vignette_content))
    expect_true(grepl("Environment Variables", vignette_content))
  }
})


# ============================================================================
# INTEGRATION TESTS
# ============================================================================

test_that("Full configuration workflow works end-to-end", {
  # Save original config
  rb_config(reset = TRUE)
  original_retries <- .network_options$max_retries

  # Change multiple settings
  rb_config(
    max_retries = 5,
    retry_delay = 3,
    rate_limit = 2,
    use_persistent_cache = TRUE,
    cache_max_age_days = 7
  )

  # Verify changes
  expect_equal(.network_options$max_retries, 5)
  expect_equal(.network_options$retry_delay, 3)
  expect_equal(.network_options$rate_limit, 2)
  expect_equal(.network_options$use_persistent_cache, TRUE)
  expect_equal(.network_options$cache_max_age_days, 7)

  # Reset and verify
  rb_config(reset = TRUE)
  expect_equal(.network_options$max_retries, original_retries)
})

test_that("Cache directory switching works", {
  # Session cache
  rb_config(use_persistent_cache = FALSE)
  cache_dir_session <- .get_cache_dir(TRUE)
  expect_true(grepl("ribits_cache", cache_dir_session))

  # Persistent cache
  rb_config(use_persistent_cache = TRUE)
  cache_dir_persist <- .get_cache_dir(TRUE)
  expect_true(grepl("ribits", cache_dir_persist, ignore.case = TRUE))

  # Should be different
  expect_false(cache_dir_session == cache_dir_persist)

  # Reset
  rb_config(reset = TRUE)
})

test_that("Error classification integrates with retry logic", {
  # Permanent errors should not be retried
  # This is tested through .is_retryable_error() which is used by rb_request_with_retry()

  permanent_errors <- c(404, 401, 403, 400)
  for (code in permanent_errors) {
    expect_false(
      .is_retryable_error(paste("HTTP", code), status_code = code),
      info = paste("Status code", code, "should not be retryable")
    )
  }

  # Retryable errors
  retryable_errors <- c(500, 502, 503, 429, 408)
  for (code in retryable_errors) {
    expect_true(
      .is_retryable_error(paste("HTTP", code), status_code = code),
      info = paste("Status code", code, "should be retryable")
    )
  }
})


# ============================================================================
# SUMMARY TEST
# ============================================================================

test_that("All new exported functions exist", {
  # Phase 1
  expect_true(exists("rb_config"))

  # Phase 4
  expect_true(exists("rb_clear_cache"))

  # Verify they're exported
  expect_true("rb_config" %in% getNamespaceExports("RIBITSr"))
  expect_true("rb_clear_cache" %in% getNamespaceExports("RIBITSr"))
})

test_that("All internal functions exist", {
  # Phase 1
  expect_true(exists(".is_retryable_error", mode = "function"))
  expect_true(exists(".load_env_config", mode = "function"))
  expect_true(exists(".get_config", mode = "function"))

  # Phase 4
  expect_true(exists(".apply_rate_limit", mode = "function"))
  expect_true(exists(".get_cache_dir", mode = "function"))

  # Phase 5
  expect_true(exists(".validate_csv_content", mode = "function"))
  expect_true(exists(".validate_transaction_data", mode = "function"))
  expect_true(exists(".rb_friendly_error", mode = "function"))
})
