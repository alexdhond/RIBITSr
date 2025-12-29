#!/usr/bin/env Rscript
# Test different states to find any remaining data quality issues

library(RIBITSr)
library(dplyr)

cat("\n════════════════════════════════════════════════\n")
cat("  Testing New States for Data Quality Issues\n")
cat("════════════════════════════════════════════════\n\n")

# Enable auto-harmonization to see what gets resolved
rb_discrepancy_config(auto_harmonize = TRUE)

# Test different state sizes/characteristics:
# - CA: Large state (likely many banks)
# - VA: Medium state
# - VT: Small state
# - FL: Large coastal state
test_states <- c("CA", "VA", "VT", "FL")

results <- list()

for (state in test_states) {
  cat("\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat(sprintf("  %s\n", state))
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

  start_time <- Sys.time()

  data <- tryCatch({
    ribits(state = state, transactions = "none", include_summaries = FALSE, quietly = TRUE)
  }, error = function(e) {
    cat("✗ ERROR:", conditionMessage(e), "\n")
    return(NULL)
  })

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  if (is.null(data) || is.null(data$banks) || nrow(data$banks) == 0) {
    cat(sprintf("No banks found (%.1fs)\n", elapsed))
    results[[state]] <- list(
      success = TRUE,
      n_banks = 0,
      n_discrep = 0,
      n_resol = 0,
      elapsed = elapsed
    )
    next
  }

  n_banks <- nrow(data$banks)
  cat(sprintf("Banks: %d\n", n_banks))

  # Check column separation
  has_bank_type <- "bank_type" %in% names(data$banks)
  has_kind_of_bank <- "kind_of_bank" %in% names(data$banks)

  cat("\nColumns:\n")
  cat(sprintf("  bank_type: %s\n", if (has_bank_type) "✓" else "✗ MISSING"))
  cat(sprintf("  kind_of_bank: %s\n", if (has_kind_of_bank) "✓" else "✗ MISSING"))

  # Sample values from first bank
  if (has_bank_type && has_kind_of_bank) {
    cat(sprintf("\nSample (Bank 1):\n"))
    cat(sprintf("  bank_type: %s\n", data$banks$bank_type[1]))
    cat(sprintf("  kind_of_bank: %s\n", data$banks$kind_of_bank[1]))
  }

  # Discrepancies
  discrep <- data$.meta$discrepancies
  n_discrep <- if (!is.null(discrep)) nrow(discrep) else 0

  # Resolutions
  resol <- data$.meta$harmonization_resolutions
  n_resol <- if (!is.null(resol)) nrow(resol) else 0

  cat(sprintf("\nDiscrepancies:\n"))
  cat(sprintf("  Remaining: %d\n", n_discrep))
  cat(sprintf("  Auto-harmonized: %d\n", n_resol))

  if (n_discrep > 0) {
    cat("\n⚠️  FOUND ISSUES - Details:\n")
    cat("  Fields:\n")
    field_summary <- discrep %>%
      group_by(field) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))

    for (i in 1:nrow(field_summary)) {
      cat(sprintf("    - %s: %d\n", field_summary$field[i], field_summary$count[i]))
    }

    cat("\n  Source pairs:\n")
    source_summary <- discrep %>%
      mutate(pair = paste(pmin(source1, source2), "vs", pmax(source1, source2))) %>%
      group_by(pair) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))

    for (i in 1:nrow(source_summary)) {
      cat(sprintf("    - %s: %d\n", source_summary$pair[i], source_summary$count[i]))
    }

    # Show examples
    cat("\n  Examples:\n")
    for (i in 1:min(3, nrow(discrep))) {
      row <- discrep[i, ]
      cat(sprintf("    %d. %s (Bank %s)\n", i, row$field, row$bank_id))
      cat(sprintf("       %s: %s\n", row$source1, substr(as.character(row$value1), 1, 40)))
      cat(sprintf("       %s: %s\n", row$source2, substr(as.character(row$value2), 1, 40)))
    }
  } else {
    cat("\n✓ No remaining discrepancies\n")
  }

  if (n_resol > 0) {
    cat("\n  Auto-harmonization rules used:\n")
    rules <- table(resol$resolution_rule)
    for (rule in names(rules)) {
      cat(sprintf("    - %s: %d\n", rule, rules[rule]))
    }
  }

  cat(sprintf("\nFetch time: %.1fs\n", elapsed))

  # Store results
  results[[state]] <- list(
    success = TRUE,
    n_banks = n_banks,
    n_discrep = n_discrep,
    n_resol = n_resol,
    elapsed = elapsed,
    discrep_data = discrep
  )
}

# Summary
cat("\n\n════════════════════════════════════════════════\n")
cat("  Summary\n")
cat("════════════════════════════════════════════════\n\n")

summary_df <- data.frame(
  State = character(),
  Banks = integer(),
  Discrepancies = integer(),
  AutoHarmonized = integer(),
  Time_Sec = numeric(),
  stringsAsFactors = FALSE
)

for (state in names(results)) {
  if (results[[state]]$success) {
    summary_df <- rbind(summary_df, data.frame(
      State = state,
      Banks = results[[state]]$n_banks,
      Discrepancies = results[[state]]$n_discrep,
      AutoHarmonized = results[[state]]$n_resol,
      Time_Sec = round(results[[state]]$elapsed, 1)
    ))
  }
}

print(summary_df)

cat("\n")
total_banks <- sum(summary_df$Banks)
total_discrep <- sum(summary_df$Discrepancies)
total_resol <- sum(summary_df$AutoHarmonized)

cat(sprintf("Total banks tested: %d\n", total_banks))
cat(sprintf("Total discrepancies: %d\n", total_discrep))
cat(sprintf("Total auto-harmonized: %d\n", total_resol))

if (total_discrep > 0) {
  cat(sprintf("\n⚠️  FOUND %d ISSUES ACROSS %d STATE(S)\n",
              total_discrep,
              sum(summary_df$Discrepancies > 0)))
  cat("\nStates with issues:\n")
  for (state in names(results)) {
    if (results[[state]]$n_discrep > 0) {
      cat(sprintf("  - %s: %d discrepancies\n", state, results[[state]]$n_discrep))
    }
  }
} else {
  cat("\n✓ ALL STATES PASSED - NO ISSUES FOUND\n")
}

cat("\n════════════════════════════════════════════════\n")
