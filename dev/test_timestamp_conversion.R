#!/usr/bin/env Rscript
# Manually test timestamp conversions

cat("\n════════════════════════════════════════════════\n")
cat("  Manual Timestamp Conversion Test\n")
cat("════════════════════════════════════════════════\n\n")

# Test case: 890006400000 should be 1998-03-16
timestamp_str <- "890006400000"
num_val <- as.numeric(timestamp_str)

cat(sprintf("Input: %s\n", timestamp_str))
cat(sprintf("As numeric: %s\n", format(num_val, scientific = FALSE)))
cat(sprintf("Scientific: %e\n\n", num_val))

# Check thresholds
cat("Threshold checks:\n")
cat(sprintf("  > 946684800000? %s (milliseconds path)\n",
            if (num_val > 946684800000) "YES" else "NO"))
cat(sprintf("  > 946684800? %s (seconds path)\n",
            if (num_val > 946684800) "YES" else "NO"))
cat("\n")

# Try milliseconds conversion (what the code does if > 946684800000)
cat("Milliseconds conversion (num_val / 1000):\n")
seconds_val <- num_val / 1000
cat(sprintf("  Seconds: %s\n", format(seconds_val, scientific = FALSE)))

result_ms <- as.Date(as.POSIXct(seconds_val, origin = "1970-01-01", tz = "UTC"))
cat(sprintf("  Result: %s\n\n", as.character(result_ms)))

# Try seconds conversion (what happens if we treat the whole number as seconds)
cat("Seconds conversion (num_val as-is):\n")
result_sec <- as.Date(as.POSIXct(num_val, origin = "1970-01-01", tz = "UTC"))
cat(sprintf("  Result: %s\n\n", as.character(result_sec)))

# What SHOULD happen for 1998-03-16
cat("Expected value:\n")
expected_date <- as.Date("1998-03-16")
cat(sprintf("  Date: %s\n", as.character(expected_date)))

# What is the correct timestamp?
expected_posix <- as.POSIXct(expected_date, tz = "UTC")
expected_seconds <- as.numeric(expected_posix)
expected_ms <- expected_seconds * 1000

cat(sprintf("  As seconds: %s\n", format(expected_seconds, scientific = FALSE)))
cat(sprintf("  As milliseconds: %s\n\n", format(expected_ms, scientific = FALSE)))

# Compare
cat("Comparison:\n")
cat(sprintf("  EPA gave us: %s\n", timestamp_str))
cat(sprintf("  Correct milliseconds: %s\n", format(expected_ms, scientific = FALSE)))
cat(sprintf("  Correct seconds: %s\n", format(expected_seconds, scientific = FALSE)))
cat("\n")

if (timestamp_str == format(expected_ms, scientific = FALSE)) {
  cat("✓ EPA timestamp is in milliseconds\n")
} else if (timestamp_str == format(expected_seconds, scientific = FALSE)) {
  cat("✓ EPA timestamp is in seconds\n")
} else {
  cat("✗ EPA timestamp doesn't match either format!\n")
  cat("  Checking if it's off by a factor...\n")

  # Maybe it's the wrong unit
  test_as_sec <- as.Date(as.POSIXct(num_val, origin = "1970-01-01", tz = "UTC"))
  cat(sprintf("  If treated as seconds: %s\n", as.character(test_as_sec)))

  test_as_ms <- as.Date(as.POSIXct(num_val / 1000, origin = "1970-01-01", tz = "UTC"))
  cat(sprintf("  If treated as milliseconds: %s\n", as.character(test_as_ms)))
}
