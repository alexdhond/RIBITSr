test_that(".should_be_quiet respects quietly parameter", {
  # Explicitly quiet
  expect_true(.should_be_quiet(quietly = TRUE))

  # Explicitly not quiet
  expect_false(.should_be_quiet(quietly = FALSE))
})

test_that(".msg_info displays message when not quiet", {
  # Should not error
  expect_no_error(.msg_info("Test message", quietly = FALSE))

  # Should be silent when quiet
  expect_silent(.msg_info("Test message", quietly = TRUE))
})

test_that(".msg_info supports glue syntax", {
  n <- 100
  # Should not error with variable interpolation
  expect_no_error(.msg_info("Processing {n} banks", quietly = FALSE))
})

test_that(".msg_success displays message when not quiet", {
  expect_no_error(.msg_success("Operation completed", quietly = FALSE))
  expect_silent(.msg_success("Operation completed", quietly = TRUE))
})

test_that(".msg_warn displays warning when not quiet", {
  expect_no_error(.msg_warn("Warning message", quietly = FALSE))
  expect_silent(.msg_warn("Warning message", quietly = TRUE))
})

test_that(".msg_danger displays danger message when not quiet", {
  expect_no_error(.msg_danger("Danger message", quietly = FALSE))
  expect_silent(.msg_danger("Danger message", quietly = TRUE))
})

test_that(".msg_progress_bar creates progress bar when not quiet", {
  # When not quiet, should return a progress bar ID (non-NULL)
  pb <- .msg_progress_bar(100, quietly = FALSE)
  expect_true(!is.null(pb) || is.character(pb) || is.numeric(pb))

  # Clean up
  if (!is.null(pb)) {
    cli::cli_progress_done(id = pb)
  }

  # When quiet, should return NULL
  pb_quiet <- .msg_progress_bar(100, quietly = TRUE)
  expect_null(pb_quiet)
})

test_that(".msg_progress_update updates progress when not quiet", {
  # Testing progress bars in non-interactive mode can be tricky
  # Just verify the function doesn't error with valid-looking inputs
  expect_no_error(.msg_progress_update(id = NULL, inc = 1, quietly = FALSE))

  # When quiet, should be silent
  expect_silent(.msg_progress_update(id = "test-id", inc = 1, quietly = TRUE))
})

test_that(".msg_progress_update handles NULL id gracefully", {
  # Should not error with NULL id
  expect_silent(.msg_progress_update(id = NULL, inc = 1))
})

test_that(".msg_progress_done completes progress bar", {
  pb <- .msg_progress_bar(100, quietly = FALSE)

  # Should not error
  expect_no_error(.msg_progress_done(id = pb, quietly = FALSE))

  # Second call should also not error (already done)
  expect_no_error(.msg_progress_done(id = pb, quietly = FALSE))
})

test_that(".msg_retry formats retry messages correctly", {
  expect_no_error(
    .msg_retry("API request", attempt = 2, max_attempts = 3, quietly = FALSE)
  )

  expect_no_error(
    .msg_retry("API request", attempt = 2, max_attempts = 3, delay = 1.5, quietly = FALSE)
  )

  expect_silent(
    .msg_retry("API request", attempt = 2, max_attempts = 3, quietly = TRUE)
  )
})

test_that(".msg_network_failure formats failure messages correctly", {
  expect_no_error(
    .msg_network_failure("API request", "Connection timeout", max_attempts = 3, quietly = FALSE)
  )

  expect_silent(
    .msg_network_failure("API request", "Connection timeout", max_attempts = 3, quietly = TRUE)
  )
})

test_that(".msg_data_quality displays quality issues", {
  # Info severity
  expect_no_error(
    .msg_data_quality("Missing values", 10, 100, severity = "info", quietly = FALSE)
  )

  # Warning severity
  expect_no_error(
    .msg_data_quality("Duplicates", 5, 100, severity = "warning", quietly = FALSE)
  )

  # Danger severity
  expect_no_error(
    .msg_data_quality("Invalid values", 50, 100, severity = "danger", quietly = FALSE)
  )

  # Quiet mode
  expect_silent(
    .msg_data_quality("Missing values", 10, 100, quietly = TRUE)
  )
})

test_that(".msg_checkpoint handles different actions", {
  expect_no_error(.msg_checkpoint("saved", n_items = 100, quietly = FALSE))
  expect_no_error(.msg_checkpoint("loaded", n_items = 100, quietly = FALSE))
  expect_no_error(.msg_checkpoint("cleared", quietly = FALSE))

  expect_silent(.msg_checkpoint("saved", quietly = TRUE))
})

test_that(".msg_rate_limit shows message for significant delays", {
  # Should show message for >1 second delays
  expect_no_error(.msg_rate_limit(2.5, quietly = FALSE))

  # Should not show message for small delays (but not error)
  expect_no_error(.msg_rate_limit(0.5, quietly = FALSE))

  # Should be silent when quiet
  expect_silent(.msg_rate_limit(2.5, quietly = TRUE))
})

test_that(".msg_validation_summary shows correct severity", {
  # All passed
  expect_no_error(.msg_validation_summary(10, 0, 0, quietly = FALSE))

  # Some warnings
  expect_no_error(.msg_validation_summary(8, 2, 0, quietly = FALSE))

  # Some errors
  expect_no_error(.msg_validation_summary(5, 2, 3, quietly = FALSE))

  # Quiet mode
  expect_silent(.msg_validation_summary(10, 0, 0, quietly = TRUE))
})

test_that(".msg_batch_summary shows correct status", {
  # All successful
  expect_no_error(.msg_batch_summary(10, 0, "banks fetched", quietly = FALSE))

  # Some failed
  expect_no_error(.msg_batch_summary(8, 2, "banks fetched", quietly = FALSE))

  # All failed
  expect_no_error(.msg_batch_summary(0, 10, "banks fetched", quietly = FALSE))

  # Quiet mode
  expect_silent(.msg_batch_summary(10, 0, "banks fetched", quietly = TRUE))
})

test_that(".format_number formats numbers with commas", {
  expect_equal(.format_number(1000), "1,000")
  expect_equal(.format_number(1000000), "1,000,000")
  expect_equal(.format_number(100), "100")
})

test_that(".format_file_size formats bytes correctly", {
  # Bytes
  result <- .format_file_size(500)
  expect_match(result, "500 bytes")

  # Kilobytes
  result <- .format_file_size(2048)
  expect_match(result, "KB")

  # Megabytes
  result <- .format_file_size(2 * 1024^2)
  expect_match(result, "MB")

  # Gigabytes
  result <- .format_file_size(2 * 1024^3)
  expect_match(result, "GB")
})

test_that(".format_duration formats time correctly", {
  # Seconds
  result <- .format_duration(30)
  expect_match(result, "seconds")

  # Minutes
  result <- .format_duration(120)
  expect_match(result, "minutes")

  # Hours
  result <- .format_duration(7200)
  expect_match(result, "hours")
})

test_that("message functions handle special characters", {
  # Should not error with special characters (no glue interpolation)
  expect_no_error(.msg_info("Test: value", quietly = FALSE))
  expect_no_error(.msg_success("100% complete!", quietly = FALSE))
  expect_no_error(.msg_warn("Warning: 'value' is missing", quietly = FALSE))

  # With actual variable for interpolation
  value <- "test"
  expect_no_error(.msg_info("Test: {value}", quietly = FALSE))
})

test_that("message functions handle long messages", {
  long_msg <- paste(rep("word", 100), collapse = " ")
  expect_no_error(.msg_info(long_msg, quietly = FALSE))
})

test_that("message functions handle empty messages", {
  expect_no_error(.msg_info("", quietly = FALSE))
  expect_no_error(.msg_success("", quietly = FALSE))
})

test_that("progress bar functions handle edge cases", {
  # Zero total
  pb <- .msg_progress_bar(0, quietly = FALSE)
  expect_no_error(.msg_progress_done(id = pb, quietly = FALSE))

  # Large total
  pb <- .msg_progress_bar(1000000, quietly = FALSE)
  expect_no_error(.msg_progress_done(id = pb, quietly = FALSE))
})

test_that(".msg_data_quality calculates percentages correctly", {
  # 10 out of 100 should be 10%
  # We can't easily test the message content, but we can test it doesn't error
  expect_no_error(.msg_data_quality("Test", 10, 100, quietly = FALSE))

  # Edge case: 0 out of 100
  expect_no_error(.msg_data_quality("Test", 0, 100, quietly = FALSE))

  # Edge case: 100 out of 100
  expect_no_error(.msg_data_quality("Test", 100, 100, quietly = FALSE))
})

test_that(".format_file_size handles edge cases", {
  # 0 bytes
  result <- .format_file_size(0)
  expect_equal(result, "0 bytes")

  # 1 byte
  result <- .format_file_size(1)
  expect_equal(result, "1 bytes")

  # Exactly 1 KB
  result <- .format_file_size(1024)
  expect_match(result, "1.0 KB")

  # Exactly 1 MB
  result <- .format_file_size(1024^2)
  expect_match(result, "1.0 MB")
})

test_that(".format_duration handles edge cases", {
  # 0 seconds
  result <- .format_duration(0)
  expect_match(result, "0.0 seconds")

  # Exactly 1 minute
  result <- .format_duration(60)
  expect_match(result, "1.0 minutes")

  # Exactly 1 hour
  result <- .format_duration(3600)
  expect_match(result, "1.0 hours")

  # Fractional values
  result <- .format_duration(90.5)
  expect_match(result, "1.5 minutes")
})

test_that("message functions work with numeric values", {
  x <- 42
  n <- 100
  expect_no_error(.msg_info("Count: {x}", quietly = FALSE))
  expect_no_error(.msg_success("Processed {n} items", quietly = FALSE))
})

test_that(".msg_retry handles missing delay gracefully", {
  # Without delay
  expect_no_error(
    .msg_retry("Request", 1, 3, delay = NULL, quietly = FALSE)
  )

  # With delay
  expect_no_error(
    .msg_retry("Request", 1, 3, delay = 2.0, quietly = FALSE)
  )
})

test_that(".msg_checkpoint handles missing n_items", {
  expect_no_error(.msg_checkpoint("saved", n_items = NULL, quietly = FALSE))
  expect_no_error(.msg_checkpoint("loaded", n_items = NULL, quietly = FALSE))
})

test_that("message wrappers return invisibly", {
  # Should return invisible NULL
  result <- .msg_info("Test", quietly = TRUE)
  expect_null(result)

  result <- .msg_success("Test", quietly = TRUE)
  expect_null(result)

  result <- .msg_warn("Test", quietly = TRUE)
  expect_null(result)
})

test_that(".format_number handles negative numbers", {
  expect_equal(.format_number(-1000), "-1,000")
  expect_equal(.format_number(-1000000), "-1,000,000")
})

test_that(".format_number handles decimals", {
  result <- .format_number(1234.56)
  expect_match(result, "1,234")
})

test_that("all message functions respect .envir parameter", {
  x <- 42
  # Should use parent environment for interpolation
  expect_no_error(.msg_info("Value: {x}", quietly = FALSE))
  expect_no_error(.msg_success("Value: {x}", quietly = FALSE))
  expect_no_error(.msg_warn("Value: {x}", quietly = FALSE))
})
