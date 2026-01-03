## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
# library(RIBITSr)

## -----------------------------------------------------------------------------
# # View current configuration
# rb_config()
# 
# # Change settings
# rb_config(max_retries = 5, rate_limit = 3)
# 
# # Reset to defaults
# rb_config(reset = TRUE)

## -----------------------------------------------------------------------------
# # Increase retries for unreliable connections
# rb_config(
#   max_retries = 5,      # Default: 3
#   retry_delay = 3       # Initial delay in seconds (default: 2)
# )

## -----------------------------------------------------------------------------
# # Set longer timeout for large CSV downloads
# rb_config(timeout = 300)  # 5 minutes (default: 60 seconds)

## -----------------------------------------------------------------------------
# # More conservative rate limit
# rb_config(rate_limit = 2)  # 2 requests/second (default: 5)
# 
# # More aggressive (use with caution)
# rb_config(rate_limit = 10)  # 10 requests/second
# 
# # Disable rate limiting (not recommended)
# rb_config(rate_limit = 0)

## -----------------------------------------------------------------------------
# # First call downloads from RIBITS
# banks <- ribits(state = "CA")
# 
# # Second call uses cache (fast!)
# banks <- ribits(state = "CA")
# 
# # Cache cleared when R restarts

## -----------------------------------------------------------------------------
# # Enable persistent caching
# rb_config(
#   use_persistent_cache = TRUE,
#   cache_max_age_days = 7     # Auto-delete files older than 7 days
# )

## -----------------------------------------------------------------------------
# # Clear all cache
# rb_clear_cache()
# 
# # Clear only CSV reports
# rb_clear_cache("csv")
# 
# # Clear only name lookup table
# rb_clear_cache("lookup")
# 
# # Silent clearing
# rb_clear_cache(verbose = FALSE)

## -----------------------------------------------------------------------------
# # Set custom cache directory
# Sys.setenv(RIBITS_CACHE_DIR = "/path/to/my/cache")
# 
# # Or in .Renviron:
# # RIBITS_CACHE_DIR=/path/to/my/cache

## -----------------------------------------------------------------------------
# # Prefer API over CSV over EPA (default)
# rb_config(source_priority = c("api", "csv", "epa"))
# 
# # Prefer CSV over API (e.g., if CSV is known to be more accurate)
# rb_config(source_priority = c("csv", "api", "epa"))

## -----------------------------------------------------------------------------
# # Automatically resolve conflicts using source priority (default)
# rb_config(auto_resolve = TRUE)
# 
# # Preserve all conflicting values and let user decide
# rb_config(auto_resolve = FALSE)

## -----------------------------------------------------------------------------
# # See what environment variables are set
# Sys.getenv("RIBITS_MAX_RETRIES")
# Sys.getenv("RIBITS_RATE_LIMIT")

## -----------------------------------------------------------------------------
# # Maximize reliability
# rb_config(
#   max_retries = 5,           # More retries
#   retry_delay = 4,           # Longer delays
#   timeout = 180,             # 3-minute timeout
#   rate_limit = 2             # Slower requests
# )

## -----------------------------------------------------------------------------
# # Enable persistent caching
# rb_config(
#   use_persistent_cache = TRUE,
#   cache_max_age_days = 1      # Refresh daily
# )
# 
# # First run: downloads and caches
# banks <- ribits(state = "CA", transactions = "comprehensive")
# 
# # Subsequent runs same day: uses cache (fast!)
# banks <- ribits(state = "CA", transactions = "comprehensive")

## -----------------------------------------------------------------------------
# # Optimize for bulk operations
# rb_config(
#   max_retries = 3,
#   rate_limit = 3,            # Conservative rate limit
#   use_persistent_cache = TRUE,
#   cache_max_age_days = 7
# )
# 
# # Extract state by state with checkpointing
# all_states <- c("AL", "AK", "AZ", ...)
# results <- list()
# 
# for (state in all_states) {
#   results[[state]] <- ribits(state = state, transactions = "comprehensive")
# 
#   # Save checkpoint
#   saveRDS(results, "ribits_bulk_extraction.rds")
# }

## -----------------------------------------------------------------------------
# # Reliable, conservative settings
# rb_config(
#   max_retries = 5,
#   retry_delay = 2,
#   timeout = 300,
#   rate_limit = 3,
#   use_persistent_cache = TRUE,
#   cache_max_age_days = 1,    # Refresh nightly
#   auto_resolve = TRUE
# )

## -----------------------------------------------------------------------------
# # Speed over reliability
# rb_config(
#   max_retries = 1,           # Fail fast
#   timeout = 30,              # Short timeout
#   rate_limit = 10,           # Fast requests (local testing only!)
#   use_persistent_cache = TRUE  # Avoid re-downloading
# )

## -----------------------------------------------------------------------------
# # Increase timeout
# rb_config(timeout = 300)
# 
# # Check network connection
# check_ribits_connection(verbose = TRUE)

## -----------------------------------------------------------------------------
# # Reduce rate limit
# rb_config(rate_limit = 2)
# 
# # Check current failures
# rb_network_failures()

## -----------------------------------------------------------------------------
# # Clear cache manually
# rb_clear_cache()
# 
# # Or reduce cache age
# rb_config(cache_max_age_days = 1)
# 
# # Or disable cache for this query
# banks <- ribits(state = "CA", cache = FALSE)

## -----------------------------------------------------------------------------
# # Clear and rebuild name lookup cache
# rb_clear_cache("lookup")
# 
# # Rebuild with fresh API data
# lookup <- rb_build_name_lookup(include_csv = FALSE, cache = FALSE)
# 
# # Check for specific bank
# rb_match_names(
#   data.frame(name = "My Bank Name"),
#   lookup,
#   name_col = "name"
# )

## -----------------------------------------------------------------------------
# # Diagnose the data
# banks <- ribits(state = "CA", transactions = "comprehensive")
# rb_diagnose(banks)
# 
# # Adjust source priority if needed
# rb_config(source_priority = c("csv", "api", "epa"))
# 
# # Disable auto-resolution to inspect conflicts
# rb_config(auto_resolve = FALSE)

## -----------------------------------------------------------------------------
# # View all failed requests from current session
# rb_network_failures()
# #> Network failures (last 50):
# #>   timestamp           | description                  | error
# #>   2024-01-15 10:23:45 | CSV download: transactions  | HTTP 503
# #>   2024-01-15 10:24:12 | API request: bank 123       | Timeout

## -----------------------------------------------------------------------------
# # Test RIBITS API connection
# check_ribits_connection(verbose = TRUE)
# #> ✔ RIBITS API is reachable
# #> ✔ Response time: 234ms
# 
# # Test specific bank
# rb_get("banks", id = "SAJ-2009-00123")

## -----------------------------------------------------------------------------
# # See all settings
# rb_config()
# #> RIBITSr Configuration:
# #>   Network & Reliability:
# #>     - max_retries: 3
# #>     - retry_delay: 2 seconds
# #>     - timeout: 60 seconds
# #>     - rate_limit: 5 requests/second
# #>   Caching:
# #>     - use_persistent_cache: FALSE
# #>     - cache_max_age_days: 30
# #>   Data Quality:
# #>     - source_priority: api, csv, epa
# #>     - auto_resolve: TRUE

## -----------------------------------------------------------------------------
# # At start of analysis script
# rb_config(use_persistent_cache = TRUE, cache_max_age_days = 7)

## -----------------------------------------------------------------------------
# # Production environment
# rb_config(rate_limit = 3)  # Be a good API citizen

## -----------------------------------------------------------------------------
# .First <- function() {
#   if (requireNamespace("RIBITSr", quietly = TRUE)) {
#     RIBITSr::rb_config(
#       max_retries = 5,
#       use_persistent_cache = TRUE,
#       cache_max_age_days = 7
#     )
#   }
# }

## -----------------------------------------------------------------------------
# # Weekly maintenance
# rb_clear_cache()

## -----------------------------------------------------------------------------
# rb_config(cache_max_age_days = 7)

## -----------------------------------------------------------------------------
# # Enable verbose mode (internal setting)
# .network_options$verbose <- TRUE
# 
# # Make requests
# banks <- ribits(state = "CA")
# #> → Downloading CSV: transactions_watershed
# #> → Progress: 1.2 MB / 3.4 MB [35%]
# #> ✔ Downloaded in 2.3s

## -----------------------------------------------------------------------------
# result <- ribits(state = "CA", transactions = "comprehensive")
# 
# # Check validation results
# result$.meta$validation
# #> $valid: TRUE
# #> $warnings: character(0)
# #> $stats:
# #>   - credit_col: "credit"
# #>   - credit_range: min=0.01, max=5432.10
# #>   - source_breakdown: csv_watershed=1234, api=456, csv_ledger=789

