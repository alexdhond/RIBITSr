# Verify redundancy for broken Credit Releases CSV (Fast Version)
library(devtools)
devtools::load_all()
library(dplyr)
library(cli)

cat("\n=== Verifying Release Redundancy (FAST) ===\n")

# Use known FL bank ID from previous checks + a few others if possible
# 7136 was FL bank.
# Let's use fetching by ID which is efficient.
ids_to_check <- c(7136) 

# First, verifying what 7136 is...
# Actually, to be safe, let's fetch bank list for FL and take head(5) ids?
# That requires fetching list first.

cli::cli_alert_info("Fetching bank list for FL to find candidates...")
# This uses the light API call (not detailed)
fl_banks <- rb_get("banks", state = "FL", ledger = FALSE)
candidates <- head(fl_banks$bank_id[1:5], 5)

cli::cli_alert_info("Testing with Bank IDs: {paste(candidates, collapse=', ')}")

# Run ribits() for these specific IDs
res <- ribits(ids = candidates, quietly = TRUE)

cat("\n1. Checking Bank Summaries...\n")
if ("total_credits_released" %in% names(res$banks)) {
    released_count <- sum(res$banks$total_credits_released, na.rm=TRUE)
    cat("   [PASS] Found 'total_credits_released' column.\n")
    cat("   -> Total Credits Released (Subset):", released_count, "\n")
} else {
    cat("   [FAIL] Missing 'total_credits_released' column in bank summary.\n")
}

cat("\n2. Checking Transaction Ledger...\n")
if (!is.null(res$transactions)) {
    cat("   [PASS] Transaction ledger present.\n")
    
    release_txs <- res$transactions |> 
        filter(grepl("release", transaction_type, ignore.case=TRUE))
    
    if (nrow(release_txs) > 0) {
        cat("   [PASS] Found", nrow(release_txs), "release transactions in ledger.\n")
        print(head(as_tibble(release_txs) |> select(bank_id, transaction_date, credits, transaction_type), 3))
    } else {
        cat("   [WARN] No release transactions found for these 5 banks.\n")
    }
} else {
    cat("   [FAIL] Transaction ledger missing.\n")
}

if ("total_credits_released" %in% names(res$banks)) {
    cli::cli_alert_success("Verification Successful: Redundancy Active.")
} else {
    cli::cli_alert_danger("Verification Failed.")
}
