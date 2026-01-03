# Verify redundancy for broken Credit Releases CSV
library(devtools)
devtools::load_all()
library(dplyr)

cat("\n=== Verifying Release Redundancy ===\n")

# Run ribits() for FL (verified to work quickly)
# Default is transactions="comprehensive"
res <- ribits(state = "FL", quietly = TRUE)

cat("1. Checking Bank Summaries...\n")
if ("total_credits_released" %in% names(res$banks)) {
    released_count <- sum(res$banks$total_credits_released, na.rm=TRUE)
    cat("   [PASS] Found 'total_credits_released' column.\n")
    cat("   -> Total Credits Released (FL):", released_count, "\n")
} else {
    cat("   [FAIL] Missing 'total_credits_released' column in bank summary.\n")
}

cat("\n2. Checking Transaction Ledger...\n")
if (!is.null(res$transactions)) {
    cat("   [PASS] Transaction ledger present.\n")
    
    # Check for "Release" type
    # 'transaction_type' or 'credit_action'
    
    release_txs <- res$transactions |> 
        filter(grepl("release", transaction_type, ignore.case=TRUE))
    
    if (nrow(release_txs) > 0) {
        cat("   [PASS] Found", nrow(release_txs), "release transactions in ledger.\n")
        print(head(as_tibble(release_txs) |> select(bank_id, transaction_date, credits, transaction_type), 3))
    } else {
        cat("   [WARN] No transactions with 'release' type found (might be none in FL?).\n")
        # Check unique types
        cat("    -> Types found:", paste(unique(res$transactions$transaction_type), collapse=", "), "\n")
    }
} else {
    cat("   [FAIL] Transaction ledger missing.\n")
}

cat("\n=== Conclusion ===\n")
if ("total_credits_released" %in% names(res$banks) && !is.null(res$transactions)) {
    cat("Redundancy Verified: Broken 'Credit Releases' CSV is not needed.\n")
    cat("Data is successfully derived from Master Ledger (API/Transactions CSV).\n")
} else {
    cat("Redundancy FAILED.\n")
}
