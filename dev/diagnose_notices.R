# Quick diagnostic for notices NULL issue
library(devtools)
devtools::load_all()

cat("\n=== Diagnosing Notices NULL Issue ===\n")

# Step 1: Download and read the public_notices CSV
cache_dir <- file.path(tempdir(), "ribits_cache")
dir.create(cache_dir, showWarnings = FALSE)

cat("\n1. Downloading public_notices CSV...\n")
csv_file <- rb_download_report("public_notices", download_dir = cache_dir)
cat("File:", csv_file, "\n")

cat("\n2. Reading CSV...\n")
data <- rb_read(csv_file)
cat("Rows:", nrow(data), "\n")
cat("Columns:", paste(head(names(data), 10), collapse = ", "), "...\n")

cat("\n3. Checking for bank_name column...\n")
name_cols <- names(data)[grepl("bank|name", names(data), ignore.case = TRUE)]
cat("Name columns found:", paste(name_cols, collapse = ", "), "\n")

cat("\n4. Sample bank names:\n")
if ("bank_name" %in% names(data)) {
  print(head(unique(data$bank_name), 10))
} else if ("name" %in% names(data)) {
  print(head(unique(data$name), 10))
}

cat("\n5. After .ensure_bank_id()...\n")
data_with_id <- .ensure_bank_id(data, quietly = FALSE)
cat("Rows after matching:", nrow(data_with_id), "\n")
cat("Has bank_id:", "bank_id" %in% names(data_with_id), "\n")
if ("bank_id" %in% names(data_with_id)) {
  cat("Valid bank_ids:", sum(!is.na(data_with_id$bank_id)), "\n")
}

# Check for Oregon banks
cat("\n6. Checking for Oregon banks...\n")
or_banks <- ribits(state = "OR", spatial = FALSE, transactions = "none", quietly = TRUE)
cat("Oregon bank_ids:", paste(head(or_banks$banks$bank_id), collapse = ", "), "...\n")

# Filter
cat("\n7. Filtering notices to Oregon...\n")
or_notices <- data_with_id |> dplyr::filter(bank_id %in% or_banks$banks$bank_id)
cat("Oregon notices:", nrow(or_notices), "\n")

cat("\n=== DIAGNOSIS COMPLETE ===\n")
