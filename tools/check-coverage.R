#!/usr/bin/env Rscript
# tools/check-coverage.R
# Script to check code coverage for RIBITSr package
#
# Usage:
#   Rscript tools/check-coverage.R
#
# Or from R console:
#   source("tools/check-coverage.R")

# Install covr if needed
if (!requireNamespace("covr", quietly = TRUE)) {
  message("Installing covr package...")
  install.packages("covr", repos = "https://cloud.r-project.org")
}

library(covr)

message("Calculating code coverage for RIBITSr...")
message("This may take a few minutes as all tests are run with coverage tracking...\n")

# Calculate package coverage
cov <- package_coverage()

# Print summary
cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("  RIBITSr Code Coverage Summary\n")
cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("  Total Coverage: %.1f%%\n", percent_coverage(cov)))
cat("═══════════════════════════════════════════════════════════\n\n")

# Get coverage by file
cat("Coverage by File:\n")
cat("─────────────────────────────────────────────────────────────\n")

file_coverage <- tally_coverage(cov, by = "line")
by_file <- split(file_coverage, file_coverage$filename)

coverage_summary <- data.frame(
  file = character(),
  coverage = numeric(),
  lines_covered = integer(),
  lines_total = integer(),
  stringsAsFactors = FALSE
)

for (filename in names(by_file)) {
  file_data <- by_file[[filename]]
  total_lines <- nrow(file_data)
  covered_lines <- sum(file_data$value > 0)
  pct <- if (total_lines > 0) (covered_lines / total_lines) * 100 else 0

  coverage_summary <- rbind(
    coverage_summary,
    data.frame(
      file = basename(filename),
      coverage = pct,
      lines_covered = covered_lines,
      lines_total = total_lines,
      stringsAsFactors = FALSE
    )
  )
}

# Sort by coverage percentage
coverage_summary <- coverage_summary[order(coverage_summary$coverage), ]

# Print each file
for (i in seq_len(nrow(coverage_summary))) {
  row <- coverage_summary[i, ]
  icon <- if (row$coverage >= 80) "\u2713" else if (row$coverage >= 50) "~" else "x"
  cat(sprintf("  %s  %-45s  %5.1f%%  (%d/%d lines)\n",
              icon,
              row$file,
              row$coverage,
              row$lines_covered,
              row$lines_total))
}

cat("─────────────────────────────────────────────────────────────\n")
cat("Legend: \u2713 >= 80%  (good)    ~ >= 50% (acceptable)    x < 50% (needs tests)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Show files with zero coverage (if any)
zero_cov <- zero_coverage(cov)
if (length(zero_cov) > 0) {
  cat("\nFunctions with ZERO test coverage:\n")
  cat("─────────────────────────────────────────────────────────────\n")
  for (func in names(zero_cov)) {
    lines <- zero_cov[[func]]
    cat(sprintf("  • %s (lines %s)\n", func, paste(lines, collapse = ", ")))
  }
  cat("\n")
}

# Generate HTML report
report_file <- "coverage_report.html"
cat(sprintf("Generating detailed HTML report: %s\n", report_file))
report(cov, file = report_file)
cat(sprintf("\u2713 Report saved! Open it in your browser:\n"))
cat(sprintf("  file://%s/%s\n\n", getwd(), report_file))

# Return coverage object invisibly
invisible(cov)
