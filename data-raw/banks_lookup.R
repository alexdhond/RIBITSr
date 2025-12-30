# Generate bundled bank lookup data
# This script creates banks_lookup.rda for package distribution
# Run this periodically (e.g., before CRAN releases) to update bundled data

# Load package functions from development
devtools::load_all()

# Build comprehensive lookup from all sources (EPA + API + 5 CSV sources)
# This will take ~30-60 seconds depending on API response times
cli::cli_h1("Generating bundled bank lookup data")

banks_lookup <- rb_build_name_lookup(
  include_csv = TRUE,
  comprehensive = TRUE,
  track_aliases = TRUE,
  force_refresh = TRUE
)

# Add metadata about when this was generated
attr(banks_lookup, "generated_date") <- Sys.Date()
attr(banks_lookup, "n_banks") <- nrow(banks_lookup)

# Summary
cli::cli_alert_success("Generated lookup with {nrow(banks_lookup)} banks")
cli::cli_alert_info("Generated on: {Sys.Date()}")

# Show sources used
sources_used <- attr(banks_lookup, "sources_used")
cli::cli_h2("Sources used")
for (src in sources_used) {
  cli::cli_alert_info("- {src}")
}

# Confidence score distribution
cli::cli_h2("Confidence score distribution")
score_summary <- summary(banks_lookup$confidence_score)
cli::cli_alert_info("Min: {round(score_summary['Min.'], 2)}")
cli::cli_alert_info("Median: {round(score_summary['Median'], 2)}")
cli::cli_alert_info("Mean: {round(score_summary['Mean'], 2)}")
cli::cli_alert_info("Max: {round(score_summary['Max.'], 2)}")

# Source count distribution
cli::cli_h2("Source coverage")
source_count_dist <- table(banks_lookup$source_count)
for (n in names(source_count_dist)) {
  cli::cli_alert_info("{n} source(s): {source_count_dist[n]} banks")
}

# Banks with/without bank_id
n_with_id <- sum(!is.na(banks_lookup$bank_id))
n_without_id <- sum(is.na(banks_lookup$bank_id))
cli::cli_h2("Bank ID coverage")
cli::cli_alert_info("With bank_id: {n_with_id} ({round(n_with_id/nrow(banks_lookup)*100, 1)}%)")
cli::cli_alert_info("Without bank_id: {n_without_id} ({round(n_without_id/nrow(banks_lookup)*100, 1)}%)")

# Save as internal package data (will be available as banks_lookup)
usethis::use_data(banks_lookup, overwrite = TRUE)

cli::cli_alert_success("Saved to data/banks_lookup.rda")
cli::cli_alert_info("Users can access via: data('banks_lookup', package = 'RIBITSr')")
