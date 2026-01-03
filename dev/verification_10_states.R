# dev/verification_10_states.R
# Test script to verify extraction for 10 diverse states and check data quality

message("Loading package...")
devtools::load_all(quiet = TRUE)

states <- c("CA", "FL", "TX", "OR", "WA", "VA", "GA", "IL", "OH", "NC")
results <- list()

message("\n========================================================")
message("Starting 10-State Verification Run")
message("========================================================\n")

for (st in states) {
  message(sprintf("Processing State: %s...", st))
  
  start_time <- Sys.time()
  
  tryCatch({
    # Step 1: Get list of banks (fast)
    # Note: rb_get does not accept 'quietly', it prints by default but we can suppress with suppressMessages if really needed, 
    # or just let it print.
    bank_list <- rb_get("banks", state = st)
    
    if (is.null(bank_list) || nrow(bank_list) == 0) {
      stop("No banks found")
    }
    
    # Step 2: Sample 5 random banks
    sample_size <- min(5, nrow(bank_list))
    if (sample_size > 0) {
      sample_ids <- sample(bank_list$bank_id[!is.na(bank_list$bank_id)], sample_size)
      message(sprintf("  -> Found %d banks, sampling %d...", nrow(bank_list), sample_size))
      
      # Step 3: Fetch detailed data for sample
      # Note: rb_get_data expects IDs to be passed via 'aoi' argument if using the wrapper
      data <- rb_get_data(
        aoi = sample_ids, 
        transactions = "basic",  
        include_summaries = TRUE, 
        quietly = TRUE
      )
      
      # Basic counts
      n_banks <- if (!is.null(data$banks)) nrow(data$banks) else 0
      n_txns <- if (!is.null(data$transactions)) nrow(data$transactions) else 0
      duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      
      # Quality Checks (Banks)
      bank_na_checks <- list()
      if (n_banks > 0) {
        cols_to_check <- c("bank_name", "bank_status", "total_available_credits", 
                           "total_acres", "establishment_date")
        available_cols <- intersect(cols_to_check, names(data$banks))
        
        for (col in available_cols) {
          n_na <- sum(is.na(data$banks[[col]]))
          pct_na <- round(100 * n_na / n_banks, 1)
          bank_na_checks[[col]] <- pct_na
        }
      }
      
      # Store results
      results[[st]] <- list(
        success = TRUE,
        n_banks = n_banks,
        n_txns = n_txns,
        duration = round(duration, 1),
        quality = bank_na_checks
      )
      
      message(sprintf("  -> Success! %d banks, %d txns (%.1fs)", n_banks, n_txns, duration))
      
    } else {
      stop("No valid bank IDs found")
    }
    
    # Small pause to be polite
    Sys.sleep(0.5)
    
  }, error = function(e) {
    message(sprintf("  -> FAILED: %s", e$message))
    results[[st]] <<- list(success = FALSE, error = e$message)
  })
}

message("\n========================================================")
message("Verification Summary")
message("========================================================\n")

# Print nice table
printf <- function(...) cat(sprintf(...))

printf("%-5s | %-7s | %-6s | %-6s | %-6s | %-25s\n", 
       "State", "Status", "Banks", "Txns", "Time", "Quality (NA %)")
printf("%s\n", paste(rep("-", 70), collapse = ""))

for (st in states) {
  res <- results[[st]]
  if (res$success) {
    qual_str <- ""
    if (length(res$quality) > 0) {
      # Format quality string: "acres:5%, date:10%"
      qual_parts <- sapply(names(res$quality), function(n) {
        val <- res$quality[[n]]
        if (val > 0) sprintf("%s:%.0f%%", substr(n, 1, 4), val) else NA
      })
      qual_str <- paste(na.omit(qual_parts), collapse = ", ")
      if (qual_str == "") qual_str <- "Perfect"
    }
    
    printf("%-5s | %-7s | %-6d | %-6d | %-5.1fs | %-25s\n",
           st, "OK", res$n_banks, res$n_txns, res$duration, qual_str)
  } else {
    printf("%-5s | %-7s | %-6s | %-6s | %-6s | %-25s\n",
           st, "FAIL", "-", "-", "-", substr(res$error, 1, 25))
  }
}
