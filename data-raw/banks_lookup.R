# Generate bundled bank lookup data
# This script creates banks_lookup.rda for package distribution
# Run this periodically (e.g., before CRAN releases) to update bundled data

# Load package functions from development
devtools::load_all()

# Build lookup from live sources (EPA + API + CSV)
# This will take ~10-30 seconds depending on API response times
cli::cli_h1("Generating bundled bank lookup data")

banks_lookup <- rb_build_name_lookup(include_csv = TRUE, cache = FALSE)

# Add metadata about when this was generated
attr(banks_lookup, "generated_date") <- Sys.Date()
attr(banks_lookup, "n_banks") <- nrow(banks_lookup)

# Summary
cli::cli_alert_success("Generated lookup with {nrow(banks_lookup)} banks")
cli::cli_alert_info("Generated on: {Sys.Date()}")

# Count by source
source_counts <- table(banks_lookup$source)
cli::cli_h2("Breakdown by source")
for (src in names(source_counts)) {
  cli::cli_alert_info("{src}: {source_counts[src]} banks")
}

# Save as internal package data (will be available as banks_lookup)
usethis::use_data(banks_lookup, overwrite = TRUE)

cli::cli_alert_success("Saved to data/banks_lookup.rda")
cli::cli_alert_info("Users can access via: data('banks_lookup', package = 'RIBITSr')")
