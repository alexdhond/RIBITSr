#!/usr/bin/env Rscript
# RIBITSr Data Quality Testing Script
# Tests real-world data extraction and harmonization from multiple states

library(RIBITSr)
library(dplyr)

# Disable auto-harmonization to see raw discrepancies
rb_discrepancy_config(auto_harmonize = FALSE)

cat("==============================================================================\n")
cat("              RIBITSr Data Quality Test Report\n")
cat("==============================================================================\n\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Package version:", as.character(packageVersion("RIBITSr")), "\n\n")

# Test states (small to medium size)
test_states <- c("DE", "RI", "MD")

results <- list()

for (state in test_states) {
  cat("\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("Testing State:", state, "\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

  # Fetch data
  start_time <- Sys.time()

  data <- tryCatch({
    ribits(state = state, transactions = "none", include_summaries = FALSE, quietly = FALSE)
  }, error = function(e) {
    cat("✗ Error fetching data:", conditionMessage(e), "\n")
    return(NULL)
  })

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  if (is.null(data)) {
    results[[state]] <- list(success = FALSE)
    next
  }

  # Extract metrics
  n_banks <- if (!is.null(data$banks)) nrow(data$banks) else 0
  n_trans <- if (!is.null(data$transactions)) nrow(data$transactions) else 0
  sources <- data$.meta$sources

  # Discrepancies
  discrep <- data$.meta$discrepancies
  n_discrep <- if (!is.null(discrep)) nrow(discrep) else 0

  # Resolutions
  resol <- data$.meta$resolutions
  n_resol <- if (!is.null(resol)) nrow(resol) else 0

  cat("\n┌─ Summary ─────────────────────────────────────────────────────────────┐\n")
  cat(sprintf("│ Banks:              %-48s │\n", n_banks))
  cat(sprintf("│ Transactions:       %-48s │\n", n_trans))
  cat(sprintf("│ Fetch time:         %-48s │\n", sprintf("%.1f seconds", elapsed)))
  cat(sprintf("│ Sources:            %-48s │\n", substr(paste(sources, collapse = ", "), 1, 48)))
  cat("└───────────────────────────────────────────────────────────────────────┘\n\n")

  # Data Quality Metrics
  cat("┌─ Data Quality ────────────────────────────────────────────────────────┐\n")
  cat(sprintf("│ Discrepancies:      %-48s │\n", n_discrep))

  if (n_discrep > 0) {
    # Count by field
    discrep_fields <- table(discrep$field)
    cat("│                                                                       │\n")
    cat("│ Discrepancies by field:                                              │\n")
    for (i in seq_along(discrep_fields)) {
      field_name <- names(discrep_fields)[i]
      count <- discrep_fields[i]
      cat(sprintf("│   %-30s %-36s │\n",
                  substr(field_name, 1, 30),
                  sprintf("%d (%.1f%%)", count, 100 * count / n_banks)))
    }

    # Show a few examples
    cat("│                                                                       │\n")
    cat("│ Example discrepancies:                                               │\n")
    for (i in seq_len(min(3, nrow(discrep)))) {
      row <- discrep[i, ]
      cat(sprintf("│   %-30s                                     │\n",
                  substr(row$field, 1, 30)))
      cat(sprintf("│     %s: %-50s │\n", row$source1, substr(as.character(row$value1), 1, 50)))
      cat(sprintf("│     %s: %-50s │\n", row$source2, substr(as.character(row$value2), 1, 50)))
    }
  }

  cat(sprintf("│ Resolutions:        %-48s │\n", n_resol))
  cat("└───────────────────────────────────────────────────────────────────────┘\n\n")

  # Column completeness
  cat("┌─ Column Completeness ─────────────────────────────────────────────────┐\n")
  key_cols <- c("bank_name", "bank_type", "bank_status", "state",
                "primary_contact_name", "acres_total", "credits_approved")

  for (col in key_cols) {
    if (col %in% names(data$banks)) {
      pct_complete <- 100 * sum(!is.na(data$banks[[col]])) / n_banks
      bar_width <- round(pct_complete / 5)
      bar <- paste(rep("█", bar_width), collapse = "")
      cat(sprintf("│ %-25s %5.1f%% %s%-20s │\n",
                  col, pct_complete, bar, ""))
    }
  }
  cat("└───────────────────────────────────────────────────────────────────────┘\n\n")

  # Run diagnostics
  cat("Running rb_diagnose()...\n\n")
  diag <- tryCatch({
    capture.output(rb_diagnose(data))
    "Success"
  }, error = function(e) {
    paste("Error:", conditionMessage(e))
  })

  cat("Diagnostic result:", diag, "\n\n")

  # Store results
  results[[state]] <- list(
    success = TRUE,
    n_banks = n_banks,
    n_trans = n_trans,
    n_discrep = n_discrep,
    n_resol = n_resol,
    sources = sources,
    elapsed = elapsed,
    data = data
  )
}

cat("\n")
cat("==============================================================================\n")
cat("                          Cross-State Comparison\n")
cat("==============================================================================\n\n")

# Create comparison table
comparison <- data.frame(
  State = character(),
  Banks = integer(),
  Transactions = integer(),
  Discrepancies = integer(),
  "Discrep_Rate" = numeric(),
  Time_Sec = numeric(),
  stringsAsFactors = FALSE
)

for (state in names(results)) {
  if (results[[state]]$success) {
    n_banks <- results[[state]]$n_banks
    discrep_rate <- if (n_banks > 0) round(100 * results[[state]]$n_discrep / n_banks, 1) else 0
    comparison <- rbind(comparison, data.frame(
      State = state,
      Banks = n_banks,
      Transactions = results[[state]]$n_trans,
      Discrepancies = results[[state]]$n_discrep,
      Discrep_Rate = discrep_rate,
      Time_Sec = round(results[[state]]$elapsed, 1)
    ))
  }
}

print(comparison)

cat("\n")
cat("==============================================================================\n")
cat("                          Key Findings\n")
cat("==============================================================================\n\n")

if (nrow(comparison) > 0) {
  total_banks <- sum(comparison$Banks)
  total_discrep <- sum(comparison$Discrepancies)
  avg_discrep_rate <- if (total_banks > 0) mean(comparison$Discrep_Rate[comparison$Banks > 0]) else 0

  cat("✓ Successfully tested", nrow(comparison), "states\n")
  cat("✓ Total banks analyzed:", total_banks, "\n")
  cat("✓ Total discrepancies found:", total_discrep, "\n")
  if (total_banks > 0) {
    cat("✓ Average discrepancy rate:", sprintf("%.1f%%", avg_discrep_rate), "\n")
  }
  cat("✓ Average fetch time:", sprintf("%.1f seconds", mean(comparison$Time_Sec)), "\n")
} else {
  cat("✗ No states successfully tested\n")
}

cat("\nData source coverage:\n")
for (state in names(results)) {
  if (results[[state]]$success) {
    cat("  ", state, ":", paste(results[[state]]$sources, collapse = " + "), "\n")
  }
}

cat("\n==============================================================================\n")
cat("                              Test Complete\n")
cat("==============================================================================\n")
