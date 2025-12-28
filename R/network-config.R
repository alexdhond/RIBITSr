# R/network-config.R
# Network configuration, rate limiting, and error classification
# Split from R/network.R (628 lines → focused modules)

# R/network.R
# Network handling with retry logic, progress tracking, and checkpointing

# Package-level network options
.network_options <- new.env(parent = emptyenv())
.network_options$max_retries <- 3
.network_options$retry_delay <- 2
.network_options$backoff_multiplier <- 2
.network_options$timeout <- 30
.network_options$checkpoint_dir <- NULL
.network_options$verbose <- TRUE

# New options for enhanced functionality
.network_options$rate_limit <- 5  # requests per second (NULL = unlimited)
.network_options$use_persistent_cache <- FALSE  # Use persistent cache across sessions
.network_options$cache_max_age_days <- 30  # Days before cached data expires
.network_options$custom_cache_dir <- NULL  # Custom cache directory (NULL = default)

# Rate limiting state
.rate_limiter <- new.env(parent = emptyenv())
.rate_limiter$last_request_time <- NULL


#' Apply rate limiting before request
#'
#' Enforces a minimum delay between requests based on the configured rate limit.
#' If rate limiting is disabled (rate_limit = NULL), returns immediately.
#'
#' @keywords internal
#' @noRd
.apply_rate_limit <- function() {
  rate_limit <- .network_options$rate_limit

  # If rate limiting disabled, return immediately
  if (is.null(rate_limit) || rate_limit <= 0) {
    return(invisible())
  }

  # Calculate minimum time between requests
  min_interval <- 1 / rate_limit

  # Check if we need to wait
  if (!is.null(.rate_limiter$last_request_time)) {
    elapsed <- as.numeric(Sys.time() - .rate_limiter$last_request_time)

    if (elapsed < min_interval) {
      wait_time <- min_interval - elapsed
      Sys.sleep(wait_time)
    }
  }

  # Update last request time
  .rate_limiter$last_request_time <- Sys.time()

  invisible()
}


#' Network failure log
#' @keywords internal
#' @noRd
.network_failures <- new.env(parent = emptyenv())
.network_failures$log <- NULL


#' Classify error as retryable or permanent
#'
#' Determines whether an error should be retried based on error message
#' and HTTP status code.
#'
#' @param error_msg Error message string
#' @param status_code HTTP status code (if available)
#'
#' @return TRUE if error is retryable, FALSE if permanent
#' @keywords internal
#' @noRd
#'
#' @details
#' Retryable errors:
#' - Network errors (timeout, connection failed, DNS resolution)
#' - HTTP 429 (Rate Limit)
#' - HTTP 5xx (Server errors)
#' - HTTP 408 (Request Timeout)
#'
#' Permanent errors (don't retry):
#' - HTTP 400 (Bad Request)
#' - HTTP 401 (Unauthorized)
#' - HTTP 403 (Forbidden)
#' - HTTP 404 (Not Found)
#' - Other 4xx errors
.is_retryable_error <- function(error_msg, status_code = NULL) {

  # Network errors are always retryable
  network_patterns <- c(
    "timed out", "timeout", "connection", "resolve host",
    "network", "DNS", "could not connect"
  )

  if (any(sapply(network_patterns, function(pattern) {
    grepl(pattern, error_msg, ignore.case = TRUE)
  }))) {
    return(TRUE)
  }

  # HTTP status code classification
  if (!is.null(status_code)) {
    # Rate limiting - retryable
    if (status_code == 429) return(TRUE)

    # Request timeout - retryable
    if (status_code == 408) return(TRUE)

    # Server errors (5xx) - retryable
    if (status_code >= 500 && status_code < 600) return(TRUE)

    # Client errors (4xx) - permanent, don't retry
    if (status_code >= 400 && status_code < 500) return(FALSE)
  }

  # Conservative default: retry unless we know it's permanent
  TRUE
}


#' Perform HTTP request with automatic retry
#'
#' Wraps httr2 requests with automatic retry logic, exponential backoff,
#' and detailed progress messaging. Skips retries for permanent errors
#' (e.g., 404, 401).
#'
#' @param req An httr2 request object
#' @param description Description for progress messages
#' @param max_retries Override default max retries
#' @param timeout Override default timeout
#'
#' @return The response object, or NULL if all retries failed
#' @keywords internal
#' @noRd
