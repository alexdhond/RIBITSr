# tests/testthat/test-error-classification.R
# Tests for error classification system

test_that(".is_retryable_error correctly classifies network errors", {
  # Network errors should be retryable
  expect_true(RIBITSr:::.is_retryable_error("Connection timed out"))
  expect_true(RIBITSr:::.is_retryable_error("Could not resolve host"))
  expect_true(RIBITSr:::.is_retryable_error("Network is unreachable"))
  expect_true(RIBITSr:::.is_retryable_error("DNS resolution failed"))
})

test_that(".is_retryable_error correctly classifies HTTP 4xx errors", {
  # 4xx errors are permanent (not retryable) except 408 and 429
  expect_false(RIBITSr:::.is_retryable_error("HTTP 400", 400))  # Bad Request
  expect_false(RIBITSr:::.is_retryable_error("HTTP 401", 401))  # Unauthorized
  expect_false(RIBITSr:::.is_retryable_error("HTTP 403", 403))  # Forbidden
  expect_false(RIBITSr:::.is_retryable_error("HTTP 404", 404))  # Not Found

  # Exceptions: 408 and 429 are retryable
  expect_true(RIBITSr:::.is_retryable_error("HTTP 408", 408))  # Request Timeout
  expect_true(RIBITSr:::.is_retryable_error("HTTP 429", 429))  # Rate Limit
})

test_that(".is_retryable_error correctly classifies HTTP 5xx errors", {
  # 5xx errors are retryable (server errors)
  expect_true(RIBITSr:::.is_retryable_error("HTTP 500", 500))  # Internal Server Error
  expect_true(RIBITSr:::.is_retryable_error("HTTP 502", 502))  # Bad Gateway
  expect_true(RIBITSr:::.is_retryable_error("HTTP 503", 503))  # Service Unavailable
  expect_true(RIBITSr:::.is_retryable_error("HTTP 504", 504))  # Gateway Timeout
})

test_that(".is_retryable_error has conservative default for unknown errors", {
  # Unknown errors should be retried (conservative approach)
  expect_true(RIBITSr:::.is_retryable_error("Unknown error occurred"))
  expect_true(RIBITSr:::.is_retryable_error("Something went wrong"))
})

test_that(".is_retryable_error handles mixed case and patterns", {
  # Should be case-insensitive for network patterns
  expect_true(RIBITSr:::.is_retryable_error("CONNECTION FAILED"))
  expect_true(RIBITSr:::.is_retryable_error("Request Timed Out"))
  expect_true(RIBITSr:::.is_retryable_error("network error"))
})
