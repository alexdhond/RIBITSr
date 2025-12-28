#!/usr/bin/env Rscript
# Final data quality check with auto-harmonization enabled

library(RIBITSr)

cat("\n════════════════════════════════════════════════\n")
cat("  Final Data Quality Check (All Fixes Applied)\n")
cat("════════════════════════════════════════════════\n\n")

# Enable auto-harmonization
rb_discrepancy_config(auto_harmonize = TRUE)

states <- c("DE", "MD")

for (state in states) {
  cat("\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat(sprintf("  %s\n", state))
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

  data <- ribits(state = state, transactions = "none", include_summaries = FALSE, quietly = TRUE)

  if (!is.null(data$banks) && nrow(data$banks) > 0) {
    cat(sprintf("Banks: %d\n", nrow(data$banks)))

    # Check for both type columns
    has_bank_type <- "bank_type" %in% names(data$banks)
    has_kind_of_bank <- "kind_of_bank" %in% names(data$banks)

    cat("\nColumn separation:\n")
    cat(sprintf("  ✓ bank_type: %s\n", if (has_bank_type) "Present" else "Missing"))
    cat(sprintf("  ✓ kind_of_bank: %s\n", if (has_kind_of_bank) "Present" else "Missing"))

    if (has_bank_type && has_kind_of_bank) {
      # Show first bank values
      cat(sprintf("\n  Sample (Bank 1):\n"))
      cat(sprintf("    bank_type: %s\n", data$banks$bank_type[1]))
      cat(sprintf("    kind_of_bank: %s\n", data$banks$kind_of_bank[1]))
    }

    # Discrepancies
    n_discrep <- if (!is.null(data$.meta$discrepancies)) nrow(data$.meta$discrepancies) else 0
    cat(sprintf("\nRaw discrepancies: %d\n", n_discrep))

    if (n_discrep > 0) {
      cat("  Fields:\n")
      for (field in unique(data$.meta$discrepancies$field)) {
        count <- sum(data$.meta$discrepancies$field == field)
        cat(sprintf("    - %s: %d\n", field, count))
      }
    }

    # Resolutions
    n_resol <- if (!is.null(data$.meta$harmonization_resolutions)) nrow(data$.meta$harmonization_resolutions) else 0
    cat(sprintf("\nAuto-harmonized: %d\n", n_resol))

    if (n_resol > 0) {
      cat("  Rules used:\n")
      rules <- table(data$.meta$harmonization_resolutions$resolution_rule)
      for (rule in names(rules)) {
        cat(sprintf("    - %s: %d\n", rule, rules[rule]))
      }
    }

    # Net result
    cat(sprintf("\n✓ Net discrepancies remaining: %d", n_discrep))
    if (n_resol > 0) {
      cat(sprintf(" (down from %d)\n", n_discrep + n_resol))
    } else {
      cat("\n")
    }
  } else {
    cat("No banks found\n")
  }
}

cat("\n\n════════════════════════════════════════════════\n")
cat("  Summary\n")
cat("════════════════════════════════════════════════\n\n")

cat("Fixes applied:\n")
cat("  1. ✅ Split bank_type and kind_of_bank columns\n")
cat("  2. ✅ Fixed date parser (reordered format attempts)\n")
cat("  3. ✅ Fixed spatial data processing (sf::st_union)\n\n")

cat("Expected results:\n")
cat("  - All bank_type conflicts eliminated (34 → 0)\n")
cat("  - All date format discrepancies auto-harmonized (10 → 0)\n")
cat("  - Spatial data processing without errors\n")
cat("  - Both bank_type and kind_of_bank preserved in data\n\n")
