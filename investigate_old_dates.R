#!/usr/bin/env Rscript
# Investigate why old dates (1980s-1990s) aren't auto-harmonizing

library(RIBITSr)

cat("\n════════════════════════════════════════════════\n")
cat("  Investigating Old Date Parsing Issues\n")
cat("════════════════════════════════════════════════\n\n")

# Test the specific problem dates from the output
test_cases <- list(
  list(date_str = "03/16/1998", timestamp = "890006400000", expected = "1998-03-16"),
  list(date_str = "04/13/1994", timestamp = "766195200000", expected = "1994-04-13"),
  list(date_str = "02/19/1999", timestamp = "919382400000", expected = "1999-02-19"),
  list(date_str = "07/27/1982", timestamp = "3.96576e+11", expected = "1982-07-27"),
  list(date_str = "11/28/1996", timestamp = "849139200000", expected = "1996-11-28")
)

cat("Testing date parser with old dates:\n")
cat("─────────────────────────────────────────────\n\n")

for (i in seq_along(test_cases)) {
  test <- test_cases[[i]]

  cat(sprintf("%d. Expected: %s\n", i, test$expected))

  # Parse the formatted date
  parsed_str <- RIBITSr:::.smart_date_parse(test$date_str)
  cat(sprintf("   API (%s) → %s", test$date_str,
              if (is.na(parsed_str)) "FAILED" else as.character(parsed_str)))
  if (!is.na(parsed_str) && as.character(parsed_str) == test$expected) {
    cat(" ✓\n")
  } else {
    cat(" ✗\n")
  }

  # Parse the timestamp
  parsed_ts <- RIBITSr:::.smart_date_parse(test$timestamp)
  cat(sprintf("   EPA (%s) → %s", test$timestamp,
              if (is.na(parsed_ts)) "FAILED" else as.character(parsed_ts)))
  if (!is.na(parsed_ts) && as.character(parsed_ts) == test$expected) {
    cat(" ✓\n")
  } else {
    cat(" ✗\n")
  }

  # Check if they match
  if (!is.na(parsed_str) && !is.na(parsed_ts)) {
    if (parsed_str == parsed_ts) {
      cat("   Result: ✓ MATCH (should auto-harmonize)\n")
    } else {
      diff_days <- abs(as.numeric(difftime(parsed_str, parsed_ts, units = "days")))
      cat(sprintf("   Result: ✗ DIFFER by %d days (won't auto-harmonize)\n", diff_days))
    }
  } else {
    cat("   Result: ✗ PARSING FAILED\n")
  }

  cat("\n")
}

cat("\n════════════════════════════════════════════════\n")
cat("  Diagnosis\n")
cat("════════════════════════════════════════════════\n\n")

cat("If dates parse correctly but still show as discrepancies,\n")
cat("the issue may be in the auto-harmonization comparison logic.\n\n")

cat("Checking auto-harmonization rule for these cases:\n")
cat("─────────────────────────────────────────────\n\n")

# Create a fake discrepancy to test the rule
fake_discrep <- tibble::tibble(
  field = "bank_status_date",
  value1 = "03/16/1998",
  value2 = "890006400000",
  source1 = "ribits_api",
  source2 = "epa_arcgis",
  bank_id = "TEST"
)

config <- RIBITSr:::.get_discrepancy_config()
config$auto_harmonize <- TRUE

cat("Testing rule with: 03/16/1998 vs 890006400000\n")
result <- RIBITSr:::.try_auto_harmonization_rules(fake_discrep[1, ], config)

cat(sprintf("  Harmonized value: %s\n",
            if (!is.null(result$harmonized_value)) result$harmonized_value else "NULL"))
cat(sprintf("  Rule applied: %s\n",
            if (!is.null(result$rule)) result$rule else "NULL"))
cat(sprintf("  Confidence: %s\n",
            if (!is.null(result$confidence)) result$confidence else "NULL"))

if (is.null(result$harmonized_value)) {
  cat("\n⚠️  Auto-harmonization is NOT working for these old dates!\n")
  cat("    Need to investigate why.\n")
}
