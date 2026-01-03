## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE  # Don't actually run API calls in vignette build
)

## ----setup--------------------------------------------------------------------
# library(RIBITSr)

## ----batch-example------------------------------------------------------------
# # OLD approach (slow - sequential API calls)
# bank_ids <- 1:100
# slow_results <- lapply(bank_ids, function(id) {
#   rb_get("banks", id = id)
#   Sys.sleep(0.05)  # Rate limiting
# })
# 
# # NEW approach (fast - batched API calls)
# fast_results <- rb_extract(bank_ids = 1:100)

## ----session-cache------------------------------------------------------------
# # Configure session cache (default)
# rb_config(use_persistent_cache = FALSE)
# 
# # Data fetched and cached for this session
# ca_banks <- ribits(state = "CA")
# 
# # Second call is instant (uses cache)
# ca_banks_again <- ribits(state = "CA")

## ----persistent-cache---------------------------------------------------------
# # Enable persistent caching
# rb_config(use_persistent_cache = TRUE)
# 
# # Data cached to disk
# fl_banks <- ribits(state = "FL")
# 
# # Restart R and load package
# # This will use cached data (no API calls)
# fl_banks <- ribits(state = "FL")
# 
# # Clear cache when needed
# rb_clear_cache()

## ----selective-fetching-------------------------------------------------------
# # Minimal (fastest) - just bank metadata
# basic <- ribits(state = "CA",
#                 transactions = "none",
#                 geometry = FALSE)
# 
# # Moderate - with summary transactions
# moderate <- ribits(state = "CA",
#                    transactions = "basic",  # Summaries only
#                    geometry = TRUE)
# 
# # Comprehensive (slowest) - all available data
# complete <- ribits(state = "CA",
#                    transactions = "comprehensive",  # All sources
#                    geometry = TRUE)

## ----filtering----------------------------------------------------------------
# # GOOD - Filter at API level
# ca_wetland <- ribits(state = "CA") |>
#   dplyr::filter(grepl("Wetland", bank_name, ignore.case = TRUE))
# 
# # BETTER - Use specific IDs if known
# wetland_ids <- c(17, 42, 156)
# wetland_banks <- ribits(bank_ids = wetland_ids)

## ----batching-----------------------------------------------------------------
# # SLOW - Individual queries
# results <- list()
# for (id in bank_ids) {
#   results[[id]] <- rb_get("banks", id = id)
# }
# 
# # FAST - Batch query
# results <- rb_extract(bank_ids = bank_ids)

## ----transaction-modes--------------------------------------------------------
# # For quick overview - use "basic"
# overview <- ribits(state = "OR", transactions = "basic")
# # ✓ Gets: Transaction summaries by bank
# # ✓ Fast: Single CSV download
# 
# # For detailed analysis - use "comprehensive"
# detailed <- ribits(state = "OR", transactions = "comprehensive")
# # ✓ Gets: All transaction records from 3 sources
# # ✓ Slow: Multiple API calls + CSV downloads + harmonization

## ----quiet-mode---------------------------------------------------------------
# # Verbose (default) - shows progress
# verbose_data <- ribits(state = "CA")
# 
# # Quiet - minimal output
# rb_config(verbose = FALSE)
# quiet_data <- ribits(state = "CA")

## ----geometry-----------------------------------------------------------------
# # Fast - no geometry
# banks <- ribits(state = "WA", geometry = FALSE)
# 
# # Slower - with geometry (centroids + footprints + service areas)
# banks_spatial <- ribits(state = "WA", geometry = TRUE)

## ----benchmarking-------------------------------------------------------------
# library(bench)
# 
# # Compare different approaches
# comparison <- bench::mark(
#   minimal = ribits(state = "CA", transactions = "none", geometry = FALSE),
#   moderate = ribits(state = "CA", transactions = "basic", geometry = TRUE),
#   full = ribits(state = "CA", transactions = "comprehensive", geometry = TRUE),
#   iterations = 3,
#   check = FALSE  # Don't compare results
# )
# 
# print(comparison)
# # Shows: median time, memory usage, allocations

## ----quick-explore------------------------------------------------------------
# # Minimal data, fast queries
# banks <- ribits(state = "CA",
#                 transactions = "none",
#                 geometry = FALSE)
# 
# # View summary
# print(banks)
# 
# # Filter to interesting banks
# interesting <- banks |>
#   dplyr::filter(total_acres > 100)

## ----spatial-analysis---------------------------------------------------------
# # Get geometry but skip detailed transactions
# spatial_data <- ribits(state = "OR",
#                        transactions = "basic",  # Just summaries
#                        geometry = TRUE)
# 
# # Plot
# plot(spatial_data)

## ----transaction-analysis-----------------------------------------------------
# # Focus on transaction data
# txn_data <- ribits(state = "FL",
#                    transactions = "comprehensive",
#                    geometry = FALSE)  # Skip geometry
# 
# # Analyze
# library(dplyr)
# summary <- txn_data$transactions |>
#   group_by(transaction_type, year = lubridate::year(transaction_date)) |>
#   summarise(
#     total_credits = sum(credits, na.rm = TRUE),
#     n_transactions = n()
#   )

## ----bulk-extraction----------------------------------------------------------
# # Get all bank IDs for a region
# all_banks <- ribits(state = "CA", transactions = "none", geometry = FALSE)
# bank_ids <- all_banks$banks$bank_id
# 
# # Batch download (uses internal optimization)
# bulk_data <- rb_extract(bank_ids = bank_ids, progress = TRUE)

## ----first-query--------------------------------------------------------------
# # First query downloads everything
# system.time(data1 <- ribits(state = "CA"))  # Slow
# 
# # Second query uses cache
# system.time(data2 <- ribits(state = "CA"))  # Fast

## ----network-timeout----------------------------------------------------------
# # Increase timeout for slow connections
# rb_config(
#   timeout = 60,        # 60 second timeout (default: 30)
#   max_retries = 5      # Retry 5 times (default: 3)
# )

## ----memory-management--------------------------------------------------------
# # PROBLEM - Loading 500 banks at once
# all_data <- ribits(bank_ids = 1:500, transactions = "comprehensive")
# 
# # SOLUTION - Process in chunks
# process_chunk <- function(ids) {
#   ribits(bank_ids = ids, transactions = "comprehensive")
# }
# 
# # Split into chunks of 50
# chunks <- split(1:500, ceiling(seq_along(1:500) / 50))
# results <- lapply(chunks, process_chunk)

## ----disable-harmonization----------------------------------------------------
# # Skip harmonization for speed
# rb_discrepancy_config(auto_resolve = FALSE)
# 
# # Now faster (but may have data quality issues)
# fast_data <- ribits(state = "CA")
# 
# # Check discrepancies manually if needed
# discrepancies(fast_data)

## ----monitoring---------------------------------------------------------------
# # Time a specific operation
# system.time({
#   data <- ribits(state = "CA", transactions = "comprehensive")
# })
# 
# # Detailed profiling with profvis
# profvis::profvis({
#   ribits(state = "CA", transactions = "comprehensive")
# })
# 
# # Memory profiling
# bench::mark(
#   ribits(state = "CA"),
#   memory = TRUE
# )

## ----quick-reference----------------------------------------------------------
# # Fastest configuration
# rb_config(
#   use_persistent_cache = TRUE,
#   verbose = FALSE
# )
# 
# data <- ribits(
#   state = "CA",
#   transactions = "basic",  # Or "none"
#   geometry = FALSE
# )
# 
# # Full-featured configuration
# rb_config(
#   use_persistent_cache = TRUE,
#   max_retries = 5,
#   timeout = 60,
#   verbose = TRUE
# )
# 
# data <- ribits(
#   state = "CA",
#   transactions = "comprehensive",
#   geometry = TRUE
# )

