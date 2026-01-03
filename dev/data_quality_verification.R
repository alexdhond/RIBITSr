# Comprehensive Data Quality Verification for ribits()
# Tests: missing data, date parsing, data completeness, basic sanity checks

library(devtools)
devtools::load_all()
library(dplyr)

cat("\n╔═══════════════════════════════════════════════════════════════════════════╗\n")
cat("║           RIBITS DATA QUALITY VERIFICATION                               ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════╝\n\n")

# Test with a state that has good data coverage
test_state <- "FL"  # Florida has lots of banks and notices

cat("=== Fetching data for", test_state, "===\n\n")
data <- tryCatch({
  ribits(state = test_state, quietly = FALSE)
}, error = function(e) {
  cat("ERROR fetching data:", e$message, "\n")
  NULL
})

if (is.null(data)) {
  cat("❌ FATAL: Could not fetch data. Exiting.\n")
  quit(status = 1)
}

cat("\n\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("1. DATA STRUCTURE CHECK\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

expected_components <- c("banks", "transactions", "credits", "notices", "geometry")
for (comp in expected_components) {
  if (comp %in% names(data)) {
    n <- if (!is.null(data[[comp]])) nrow(data[[comp]]) else 0
    cat("✓", comp, ":", n, "rows\n")
  } else {
    cat("✗", comp, ": MISSING\n")
  }
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("2. BANKS DATA QUALITY\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

banks <- data$banks
if (!is.null(banks) && nrow(banks) > 0) {
  cat("Total banks:", nrow(banks), "\n\n")
  
  # Key columns that should have minimal NAs
  key_cols <- c("bank_id", "bank_name", "bank_status", "state_list")
  cat("Key Column Completeness:\n")
  for (col in key_cols) {
    if (col %in% names(banks)) {
      na_pct <- round(100 * sum(is.na(banks[[col]])) / nrow(banks), 1)
      status <- if (na_pct < 5) "✓" else if (na_pct < 20) "⚠" else "✗"
      cat(status, col, ":", na_pct, "% NA\n")
    } else {
      cat("✗", col, ": COLUMN MISSING\n")
    }
  }
  
  # Check bank_id is unique
  cat("\nUniqueness:\n")
  n_unique_ids <- length(unique(banks$bank_id))
  if (n_unique_ids == nrow(banks)) {
    cat("✓ bank_id is unique (", n_unique_ids, " unique IDs)\n")
  } else {
    cat("✗ bank_id has duplicates (", n_unique_ids, " unique out of", nrow(banks), ")\n")
  }
  
  # Check bank_status values
  cat("\nBank Status Distribution:\n")
  status_table <- table(banks$bank_status, useNA = "ifany")
  for (i in seq_along(status_table)) {
    cat("  ", names(status_table)[i], ":", status_table[i], "\n")
  }
  
} else {
  cat("✗ No banks data\n")
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("3. DATE PARSING CHECK\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

# Check transactions for date parsing
txn <- data$transactions
if (!is.null(txn) && nrow(txn) > 0) {
  date_cols <- names(txn)[grepl("date", names(txn), ignore.case = TRUE)]
  cat("Found date columns in transactions:", paste(date_cols, collapse = ", "), "\n\n")
  
  for (col in date_cols) {
    if (col %in% names(txn)) {
      vals <- txn[[col]]
      col_class <- class(vals)[1]
      n_valid <- sum(!is.na(vals))
      n_total <- length(vals)
      
      # Check if it's actually a date type
      is_date_type <- inherits(vals, "Date") || inherits(vals, "POSIXct")
      status <- if (is_date_type) "✓" else "⚠"
      
      cat(status, col, "\n")
      cat("   Type:", col_class, "\n")
      cat("   Valid:", n_valid, "/", n_total, "(", round(100*n_valid/n_total, 1), "%)\n")
      
      if (n_valid > 0 && is_date_type) {
        min_date <- min(vals, na.rm = TRUE)
        max_date <- max(vals, na.rm = TRUE)
        cat("   Range:", as.character(min_date), "to", as.character(max_date), "\n")
        
        # Sanity check: dates should be reasonable (1980-2030)
        if (min_date < as.Date("1980-01-01") || max_date > as.Date("2030-01-01")) {
          cat("   ⚠ WARNING: Dates outside expected range (1980-2030)\n")
        }
      }
      cat("\n")
    }
  }
} else {
  cat("No transactions data to check\n")
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("4. MISSING DATA PATTERN CHECK\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

# Check if missing data patterns make sense
if (!is.null(banks) && nrow(banks) > 0) {
  cat("Banks - Top 10 columns by % missing:\n")
  na_pcts <- sapply(banks, function(x) 100 * sum(is.na(x)) / length(x))
  na_sorted <- sort(na_pcts, decreasing = TRUE)[1:min(10, length(na_pcts))]
  for (i in seq_along(na_sorted)) {
    cat("  ", names(na_sorted)[i], ":", round(na_sorted[i], 1), "%\n")
  }
}

cat("\n")
if (!is.null(txn) && nrow(txn) > 0) {
  cat("Transactions - Top 10 columns by % missing:\n")
  na_pcts <- sapply(txn, function(x) 100 * sum(is.na(x)) / length(x))
  na_sorted <- sort(na_pcts, decreasing = TRUE)[1:min(10, length(na_pcts))]
  for (i in seq_along(na_sorted)) {
    cat("  ", names(na_sorted)[i], ":", round(na_sorted[i], 1), "%\n")
  }
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("5. DATA COMPLETENESS SANITY CHECK\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

# Compare counts to make sure we're not dropping things
cat("Cross-component consistency:\n")

if (!is.null(banks) && !is.null(txn)) {
  banks_with_txn <- length(unique(txn$bank_id[txn$bank_id %in% banks$bank_id]))
  cat("Banks with transactions:", banks_with_txn, "/", nrow(banks), "\n")
}

if (!is.null(banks) && !is.null(data$credits)) {
  credits <- data$credits
  banks_with_credits <- length(unique(credits$bank_id[credits$bank_id %in% banks$bank_id]))
  cat("Banks with credits:", banks_with_credits, "/", nrow(banks), "\n")
}

if (!is.null(banks) && !is.null(data$geometry)) {
  geom <- data$geometry
  banks_with_geom <- length(unique(geom$bank_id[geom$bank_id %in% banks$bank_id]))
  cat("Banks with geometry:", banks_with_geom, "/", nrow(banks), "\n")
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("6. CREDITS DATA QUALITY\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

credits <- data$credits
if (!is.null(credits) && nrow(credits) > 0) {
  cat("Total credit records:", nrow(credits), "\n")
  cat("Unique banks:", length(unique(credits$bank_id)), "\n\n")
  
  # Check credit values are reasonable
  credit_cols <- c("available_credits", "released_credits", "potential_credits")
  for (col in credit_cols) {
    if (col %in% names(credits)) {
      vals <- credits[[col]]
      n_valid <- sum(!is.na(vals))
      n_negative <- sum(vals < 0, na.rm = TRUE)
      status <- if (n_negative == 0) "✓" else "⚠"
      cat(status, col, ": ", n_valid, " valid,", n_negative, "negative values\n")
    }
  }
} else {
  cat("No credits data\n")
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("7. NOTICES DATA QUALITY\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

notices <- data$notices
if (!is.null(notices) && nrow(notices) > 0) {
  cat("Total notices:", nrow(notices), "\n")
  cat("Unique banks:", length(unique(notices$bank_id)), "\n\n")
  
  # Check for key columns
  notice_cols <- c("bank_id", "bank_name", "create_date", "filename", "description")
  for (col in notice_cols) {
    if (col %in% names(notices)) {
      na_pct <- round(100 * sum(is.na(notices[[col]])) / nrow(notices), 1)
      cat("✓", col, ":", na_pct, "% NA\n")
    } else {
      cat("✗", col, ": MISSING\n")
    }
  }
} else {
  cat("No notices for", test_state, "(may be expected - check raw CSV)\n")
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("8. GEOMETRY SANITY CHECK\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

geom <- data$geometry
if (!is.null(geom) && nrow(geom) > 0) {
  cat("Total geometry records:", nrow(geom), "\n")
  
  # Check CRS
  cat("CRS:", sf::st_crs(geom)$epsg, "\n")
  
  # Check geometry types
  if ("centroid" %in% names(geom)) {
    n_valid_centroids <- sum(!sf::st_is_empty(geom$centroid))
    cat("Valid centroids:", n_valid_centroids, "/", nrow(geom), "\n")
  }
  if ("footprint" %in% names(geom)) {
    n_valid_footprints <- sum(!sf::st_is_empty(geom$footprint))
    cat("Valid footprints:", n_valid_footprints, "/", nrow(geom), "\n")
  }
  if ("service_area" %in% names(geom)) {
    n_valid_sa <- sum(!sf::st_is_empty(geom$service_area))
    cat("Valid service areas:", n_valid_sa, "/", nrow(geom), "\n")
  }
} else {
  cat("No geometry data\n")
}

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    VERIFICATION COMPLETE                                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════╝\n")
