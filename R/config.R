# R/config.R
# Unified configuration API for RIBITSr

#' Configure RIBITSr package settings
#'
#' @description
#' Unified configuration function for all RIBITSr settings including network
#' behavior, caching, rate limiting, and data harmonization preferences.
#'
#' Call without arguments to view current configuration. Call with parameters
#' to update settings. Settings persist for the R session and can be overridden
#' with environment variables (see Details).
#'
#' @param max_retries Maximum number of retry attempts for failed requests.
#'   Default: 3
#' @param retry_delay Initial delay between retries in seconds. Increases
#'   exponentially with backoff multiplier. Default: 2
#' @param backoff_multiplier Exponential backoff multiplier for retry delays.
#'   Default: 2 (delays: 2s, 4s, 8s, ...)
#' @param timeout Request timeout in seconds. Default: 30
#' @param rate_limit Maximum requests per second. Set to NULL for unlimited.
#'   Default: 5
#' @param checkpoint_dir Directory for saving operation checkpoints. NULL uses
#'   tempdir(). Set to FALSE to disable. Default: NULL
#' @param verbose Show detailed progress messages. Default: TRUE
#' @param use_persistent_cache Use persistent cache across R sessions (stored
#'   in user cache directory). FALSE uses session-only temp cache. Default: FALSE
#' @param cache_max_age_days Days before cached data expires and is re-downloaded.
#'   Default: 30
#' @param custom_cache_dir Custom cache directory path. NULL uses default
#'   (rappdirs::user_cache_dir for persistent, tempdir for session). Default: NULL
#' @param source_priority Character vector specifying data source priority
#'   for conflict resolution: "api", "csv", "epa". Default: c("csv", "api", "epa")
#' @param auto_resolve Automatically resolve data conflicts using priority rules.
#'   Default: TRUE
#' @param reset Reset all settings to defaults. Default: FALSE
#'
#' @return Invisibly returns current configuration as a list. When called without
#'   arguments, prints formatted configuration and returns invisibly.
#'
#' @details
#' ## Environment Variables
#'
#' Configuration can be set via environment variables (read on package load):
#' - `RIBITS_MAX_RETRIES`: Maximum retry attempts
#' - `RIBITS_RETRY_DELAY`: Initial retry delay (seconds)
#' - `RIBITS_TIMEOUT`: Request timeout (seconds)
#' - `RIBITS_RATE_LIMIT`: Max requests per second
#' - `RIBITS_USE_PERSISTENT_CACHE`: Enable persistent cache (true/false)
#' - `RIBITS_CACHE_MAX_AGE_DAYS`: Cache expiry (days)
#' - `RIBITS_CACHE_DIR`: Custom cache directory path
#' - `RIBITS_VERBOSE`: Show verbose messages (true/false)
#'
#' Environment variables are loaded once when the package is loaded. Use
#' `rb_config()` to change settings during the session.
#'
#' ## Cache Behavior
#'
#' - **Session cache** (default): Fast, cleared when R session ends. Good for
#'   interactive analysis.
#' - **Persistent cache**: Survives R restarts. Good for reproducible workflows
#'   and reducing API load. Enable with `use_persistent_cache = TRUE`.
#'
#' Clear cache with `rb_clear_cache()`.
#'
#' @export
#' @examples
#' # View current configuration
#' rb_config()
#'
#' # Customize network behavior
#' rb_config(
#'   max_retries = 5,        # More retries for unreliable connections
#'   timeout = 60,           # Longer timeout for slow connections
#'   rate_limit = 2          # Slower rate (2 requests/sec)
#' )
#'
#' # Enable persistent caching
#' rb_config(
#'   use_persistent_cache = TRUE,
#'   cache_max_age_days = 7  # Refresh cache weekly
#' )
#'
#' # Disable rate limiting for maximum speed
#' rb_config(rate_limit = NULL)
#'
#' # Prioritize API data over CSV
#' rb_config(source_priority = c("api", "csv", "epa"))
#'
#' # Reset everything to defaults
#' rb_config(reset = TRUE)
rb_config <- function(max_retries = NULL,
                      retry_delay = NULL,
                      backoff_multiplier = NULL,
                      timeout = NULL,
                      rate_limit = NULL,
                      checkpoint_dir = NULL,
                      verbose = NULL,
                      use_persistent_cache = NULL,
                      cache_max_age_days = NULL,
                      custom_cache_dir = NULL,
                      source_priority = NULL,
                      auto_resolve = NULL,
                      reset = FALSE) {

  # Handle reset
  if (reset) {
    .reset_config()
    cli::cli_alert_success("All configuration reset to defaults")
    return(invisible(.get_config()))
  }

  # Check if any parameters were provided using missing() to distinguish
  # between NULL and not provided
  args_provided <- !all(
    missing(max_retries), missing(retry_delay), missing(backoff_multiplier),
    missing(timeout), missing(rate_limit), missing(checkpoint_dir),
    missing(verbose), missing(use_persistent_cache), missing(cache_max_age_days),
    missing(custom_cache_dir), missing(source_priority), missing(auto_resolve)
  )

  # If no arguments, print current config and return
  if (!args_provided) {
    .print_config()
    return(invisible(.get_config()))
  }

  # Update network options
  if (!missing(max_retries) && !is.null(max_retries)) .network_options$max_retries <- max_retries
  if (!missing(retry_delay) && !is.null(retry_delay)) .network_options$retry_delay <- retry_delay
  if (!missing(backoff_multiplier) && !is.null(backoff_multiplier)) .network_options$backoff_multiplier <- backoff_multiplier
  if (!missing(timeout) && !is.null(timeout)) .network_options$timeout <- timeout
  if (!missing(checkpoint_dir) && !is.null(checkpoint_dir)) .network_options$checkpoint_dir <- checkpoint_dir
  if (!missing(verbose) && !is.null(verbose)) .network_options$verbose <- verbose

  # Update rate_limit (allowing NULL to disable rate limiting)
  if (!missing(rate_limit)) {
    .network_options$rate_limit <- rate_limit
  }

  # Update other options
  if (!missing(use_persistent_cache) && !is.null(use_persistent_cache)) .network_options$use_persistent_cache <- use_persistent_cache
  if (!missing(cache_max_age_days) && !is.null(cache_max_age_days)) .network_options$cache_max_age_days <- cache_max_age_days
  if (!missing(custom_cache_dir) && !is.null(custom_cache_dir)) .network_options$custom_cache_dir <- custom_cache_dir

  # Update discrepancy config if parameters provided
  if (!missing(source_priority) || !missing(auto_resolve)) {
    disc_config <- .get_discrepancy_config()

    # Use current values if not specified
    if (missing(source_priority)) source_priority <- disc_config$source_priority
    if (missing(auto_resolve)) auto_resolve <- disc_config$auto_resolve

    # Update discrepancy config
    rb_discrepancy_config(
      source_priority = source_priority,
      numeric_tolerance = disc_config$numeric_tolerance,
      date_tolerance_days = disc_config$date_tolerance_days,
      string_matching = disc_config$string_matching,
      flag_threshold = disc_config$flag_threshold,
      auto_resolve = auto_resolve
    )
  }

  cli::cli_alert_success("Configuration updated")

  invisible(.get_config())
}


#' Get current configuration
#'
#' @return List with current configuration values
#' @keywords internal
#' @noRd
.get_config <- function() {
  disc_config <- .get_discrepancy_config()

  list(
    network = list(
      max_retries = .network_options$max_retries,
      retry_delay = .network_options$retry_delay,
      backoff_multiplier = .network_options$backoff_multiplier,
      timeout = .network_options$timeout,
      checkpoint_dir = .network_options$checkpoint_dir,
      verbose = .network_options$verbose
    ),
    performance = list(
      rate_limit = .network_options$rate_limit,
      use_persistent_cache = .network_options$use_persistent_cache,
      cache_max_age_days = .network_options$cache_max_age_days,
      custom_cache_dir = .network_options$custom_cache_dir
    ),
    data_quality = list(
      source_priority = disc_config$source_priority,
      auto_resolve = disc_config$auto_resolve
    )
  )
}


#' Print formatted configuration
#'
#' @keywords internal
#' @noRd
.print_config <- function() {
  config <- .get_config()

  cli::cli_h1("RIBITSr Configuration")

  cli::cli_h2("Network Settings")
  cli::cli_bullets(c(
    " " = "Max retries: {.val {config$network$max_retries}}",
    " " = "Retry delay: {.val {config$network$retry_delay}}s (exponential backoff: {.val {config$network$backoff_multiplier}}x)",
    " " = "Timeout: {.val {config$network$timeout}}s",
    " " = "Verbose: {.val {config$network$verbose}}",
    " " = "Checkpoint dir: {.val {config$network$checkpoint_dir %||% 'tempdir()'}}"
  ))

  cli::cli_h2("Performance & Caching")
  rate_limit_str <- if (is.null(config$performance$rate_limit)) {
    "unlimited"
  } else {
    paste0(config$performance$rate_limit, " req/sec")
  }
  cache_type <- if (config$performance$use_persistent_cache) {
    "persistent (survives R restart)"
  } else {
    "session-only (cleared on exit)"
  }
  cache_dir <- if (!is.null(config$performance$custom_cache_dir)) {
    config$performance$custom_cache_dir
  } else if (config$performance$use_persistent_cache) {
    "rappdirs::user_cache_dir('ribits')"
  } else {
    "tempdir()"
  }

  cli::cli_bullets(c(
    " " = "Rate limit: {.val {rate_limit_str}}",
    " " = "Cache type: {.val {cache_type}}",
    " " = "Cache max age: {.val {config$performance$cache_max_age_days}} days",
    " " = "Cache location: {.path {cache_dir}}"
  ))

  cli::cli_h2("Data Quality")
  cli::cli_bullets(c(
    " " = "Source priority: {.val {paste(config$data_quality$source_priority, collapse = ' > ')}}",
    " " = "Auto-resolve conflicts: {.val {config$data_quality$auto_resolve}}"
  ))

  cli::cli_alert_info("Use {.code rb_config(<param> = <value>)} to update settings")
  cli::cli_alert_info("Use {.code rb_config(reset = TRUE)} to restore defaults")

  invisible()
}


#' Reset configuration to defaults
#'
#' @keywords internal
#' @noRd
.reset_config <- function() {
  # Reset network options
  .network_options$max_retries <- 3
  .network_options$retry_delay <- 2
  .network_options$backoff_multiplier <- 2
  .network_options$timeout <- 30
  .network_options$checkpoint_dir <- NULL
  .network_options$verbose <- TRUE
  .network_options$rate_limit <- 5
  .network_options$use_persistent_cache <- FALSE
  .network_options$cache_max_age_days <- 30
  .network_options$custom_cache_dir <- NULL

  # Reset discrepancy config
  options(ribits_discrepancy_config = NULL)

  invisible()
}
