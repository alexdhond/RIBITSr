# Verify that hydration populates lat/lon and full columns
library(devtools)
devtools::load_all()

cat("\n=== Verifying API Hydration for FL ===\n\n")

# Run ribits() for FL
# This should trigger the new hydration step
start_time <- Sys.time()
fl_data <- ribits(state = "FL", transactions = "none", cache = FALSE, quietly = FALSE)
end_time <- Sys.time()

cat("\n=== Verification Results ===\n")
cat("Duration:", round(difftime(end_time, start_time, units = "secs"), 1), "s\n")

if (!is.null(fl_data$banks)) {
  cat("Banks found:", nrow(fl_data$banks), "\n")
  cat("Columns:", ncol(fl_data$banks), "\n")
  
  # Check for lat/lon
  coord_cols <- grep("lat|lon|coord|x|y", names(fl_data$banks), ignore.case = TRUE, value = TRUE)
  cat("Coordinate columns:", paste(coord_cols, collapse = ", "), "\n")
  
  if (length(coord_cols) > 0) {
    # Check completeness
    lat_col <- grep("lat", coord_cols, ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(lat_col)) {
      n_valid <- sum(!is.na(fl_data$banks[[lat_col]]))
      cat("Valid Latitudes:", n_valid, "/", nrow(fl_data$banks), 
          sprintf("(%.1f%%)", 100 * n_valid / nrow(fl_data$banks)), "\n")
    }
  }
}

if (!is.null(fl_data$geometry)) {
  cat("\nGeometry Features:", nrow(fl_data$geometry), "\n")
  cat("Geometry Types:", paste(unique(sf::st_geometry_type(fl_data$geometry)), collapse = ", "), "\n")
}
