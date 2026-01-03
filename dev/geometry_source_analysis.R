# Investigate geometry completeness across all data sources
library(devtools)
devtools::load_all()
library(dplyr)

cat("\n╔═══════════════════════════════════════════════════════════════════════════╗\n")
cat("║           GEOMETRY SOURCE ANALYSIS                                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════╝\n\n")

# Use Florida for consistency
test_state <- "FL"
cat("Analyzing geometry sources for", test_state, "\n\n")

# Step 1: Check EPA sources directly
cat("=== STEP 1: EPA ArcGIS Sources ===\n")

# Centroid source - approved_bank_points
cat("\n1a. EPA Approved Banks (centroids):\n")
epa_centroids <- tryCatch({
  rb_epa_query("approved_banks", state = test_state, return_geometry = TRUE)
}, error = function(e) { cat("  Error:", e$message, "\n"); NULL })

if (!is.null(epa_centroids)) {
  cat("  Total records:", nrow(epa_centroids), "\n")
  cat("  Has geometry:", sum(!sf::st_is_empty(epa_centroids$geometry)), "\n")
}

# Footprint source
cat("\n1b. EPA Bank Footprints:\n")
epa_footprints <- tryCatch({
  rb_epa_query("bank_footprints", state = test_state, return_geometry = TRUE)
}, error = function(e) { cat("  Error:", e$message, "\n"); NULL })

if (!is.null(epa_footprints)) {
  cat("  Total records:", nrow(epa_footprints), "\n")
  cat("  Has geometry:", sum(!sf::st_is_empty(epa_footprints$geometry)), "\n")
}

# Service area source
cat("\n1c. EPA Bank Service Areas:\n")
epa_service_areas <- tryCatch({
  rb_epa_query("bank_service_areas", state = test_state, return_geometry = TRUE)
}, error = function(e) { cat("  Error:", e$message, "\n"); NULL })

if (!is.null(epa_service_areas)) {
  cat("  Total records:", nrow(epa_service_areas), "\n")
  cat("  Has geometry:", sum(!sf::st_is_empty(epa_service_areas$geometry)), "\n")
}

# Step 2: Check what our current function returns
cat("\n=== STEP 2: What ribits() Returns ===\n")
data <- ribits(state = test_state, transactions = "none", quietly = TRUE)

if (!is.null(data$geometry)) {
  geom <- data$geometry
  cat("Total geometry records:", nrow(geom), "\n\n")
  
  # Check each geometry column
  if ("centroid" %in% names(geom)) {
    n_valid <- sum(!sf::st_is_empty(geom$centroid))
    cat("Centroids: ", n_valid, "/", nrow(geom), " (", round(100*n_valid/nrow(geom), 1), "%)\n", sep = "")
  }
  
  if ("footprint" %in% names(geom)) {
    n_valid <- sum(!sf::st_is_empty(geom$footprint))
    cat("Footprints: ", n_valid, "/", nrow(geom), " (", round(100*n_valid/nrow(geom), 1), "%)\n", sep = "")
  }
  
  if ("service_area" %in% names(geom)) {
    n_valid <- sum(!sf::st_is_empty(geom$service_area))
    cat("Service Areas: ", n_valid, "/", nrow(geom), " (", round(100*n_valid/nrow(geom), 1), "%)\n", sep = "")
  }
}

# Step 3: Compare EPA availability vs ribits() completeness  
cat("\n=== STEP 3: Gap Analysis ===\n")

if (!is.null(epa_centroids) && !is.null(data$geometry)) {
  # How many EPA centroids are in our data?
  epa_ids <- epa_centroids$BANK_ID
  our_ids <- data$geometry$bank_id
  
  epa_in_ours <- sum(epa_ids %in% our_ids)
  cat("EPA approved banks matched to our banks:", epa_in_ours, "/", length(epa_ids), "\n")
  
  # How many of OUR banks have centroids? 
  our_with_centroid <- sum(!sf::st_is_empty(data$geometry$centroid))
  cat("Our banks with centroids:", our_with_centroid, "/", length(our_ids), "\n")
  
  # GAP: banks in our list without EPA centroids
  gap <- length(our_ids) - our_with_centroid
  cat("\nGAP (banks without centroids):", gap, "\n")
}

# Step 4: Check bank status of those missing geometry
cat("\n=== STEP 4: Missing Geometry by Bank Status ===\n")
banks <- data$banks
geom <- data$geometry

if (!is.null(geom) && "centroid" %in% names(geom)) {
  missing_centroid_ids <- geom$bank_id[sf::st_is_empty(geom$centroid)]
  missing_banks <- banks |> dplyr::filter(bank_id %in% missing_centroid_ids)
  
  cat("\nBanks missing centroids by status:\n")
  print(table(missing_banks$bank_status, useNA = "ifany"))
  
  # These are likely NOT Approved banks
  cat("\nNote: EPA only has geometry for 'Approved' status banks\n")
}

# Step 5: Verify by checking Approved-only banks
cat("\n=== STEP 5: Geometry for APPROVED Banks Only ===\n")
approved_banks <- banks |> dplyr::filter(bank_status == "Approved")
approved_ids <- approved_banks$bank_id

approved_geom <- geom |> dplyr::filter(bank_id %in% approved_ids)

if (nrow(approved_geom) > 0) {
  cat("Total approved banks:", nrow(approved_geom), "\n")
  
  if ("centroid" %in% names(approved_geom)) {
    n_valid <- sum(!sf::st_is_empty(approved_geom$centroid))
    cat("With centroids: ", n_valid, "/", nrow(approved_geom), " (", round(100*n_valid/nrow(approved_geom), 1), "%)\n", sep = "")
  }
  
  if ("footprint" %in% names(approved_geom)) {
    n_valid <- sum(!sf::st_is_empty(approved_geom$footprint))
    cat("With footprints: ", n_valid, "/", nrow(approved_geom), " (", round(100*n_valid/nrow(approved_geom), 1), "%)\n", sep = "")
  }
  
  if ("service_area" %in% names(approved_geom)) {
    n_valid <- sum(!sf::st_is_empty(approved_geom$service_area))
    cat("With service areas: ", n_valid, "/", nrow(approved_geom), " (", round(100*n_valid/nrow(approved_geom), 1), "%)\n", sep = "")
  }
}

cat("\n╔═══════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    ANALYSIS COMPLETE                                     ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════╝\n")
