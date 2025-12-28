# R/zzz.R
# Package initialization and environment configuration

#' Package initialization
#'
#' Called when the package is loaded. Initializes network options from
#' environment variables if present.
#'
#' @param libname Library name (automatically provided)
#' @param pkgname Package name (automatically provided)
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  .load_env_config()
  invisible()
}


#' Load configuration from environment variables
#'
#' Reads environment variables and updates .network_options if they exist.
#' Supported environment variables:
#' - RIBITS_MAX_RETRIES: Maximum retry attempts (integer)
#' - RIBITS_RETRY_DELAY: Initial retry delay in seconds (numeric)
#' - RIBITS_TIMEOUT: Request timeout in seconds (numeric)
#' - RIBITS_RATE_LIMIT: Maximum requests per second (numeric, NULL = unlimited)
#' - RIBITS_USE_PERSISTENT_CACHE: Enable persistent caching (true/false)
#' - RIBITS_CACHE_MAX_AGE_DAYS: Days before cache expires (numeric)
#' - RIBITS_CACHE_DIR: Custom cache directory path
#' - RIBITS_VERBOSE: Show verbose messages (true/false)
#'
#' @keywords internal
#' @noRd
.load_env_config <- function() {

  # Helper to safely parse environment variables
  .get_env <- function(var, default, type = "character") {
    val <- Sys.getenv(var, unset = "")
    if (val == "") return(default)

    result <- tryCatch({
      switch(type,
        "integer" = as.integer(val),
        "numeric" = as.numeric(val),
        "logical" = tolower(val) %in% c("true", "yes", "1", "t", "y"),
        "character" = val
      )
    }, error = function(e) {
      warning(sprintf(
        "Invalid value for %s: '%s'. Using default: %s",
        var, val, default
      ), call. = FALSE)
      default
    })

    result
  }

  # Check if .network_options exists (it should be defined in network-config.R)
  # Use hardcoded defaults if not yet available (shouldn't happen in normal package loading)
  if (!exists(".network_options", envir = parent.env(environment()))) {
    return(invisible())
  }

  # Load network configuration
  max_retries <- .get_env("RIBITS_MAX_RETRIES", .network_options$max_retries, "integer")
  retry_delay <- .get_env("RIBITS_RETRY_DELAY", .network_options$retry_delay, "numeric")
  timeout <- .get_env("RIBITS_TIMEOUT", .network_options$timeout, "numeric")
  verbose <- .get_env("RIBITS_VERBOSE", .network_options$verbose, "logical")

  # Load new configuration options (added in this update)
  rate_limit <- .get_env("RIBITS_RATE_LIMIT", 5, "numeric")
  if (rate_limit <= 0) rate_limit <- NULL  # NULL = unlimited

  use_persistent_cache <- .get_env("RIBITS_USE_PERSISTENT_CACHE", FALSE, "logical")
  cache_max_age_days <- .get_env("RIBITS_CACHE_MAX_AGE_DAYS", 30, "numeric")

  cache_dir <- Sys.getenv("RIBITS_CACHE_DIR", unset = "")
  if (cache_dir == "") cache_dir <- NULL

  # Update .network_options
  .network_options$max_retries <- max_retries
  .network_options$retry_delay <- retry_delay
  .network_options$timeout <- timeout
  .network_options$verbose <- verbose

  # Add new options
  .network_options$rate_limit <- rate_limit
  .network_options$use_persistent_cache <- use_persistent_cache
  .network_options$cache_max_age_days <- cache_max_age_days
  .network_options$custom_cache_dir <- cache_dir

  invisible()
}
