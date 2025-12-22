# =============================================================================
# Script:       clean_bank_spatial_data.R
# ============================================================================

# ============================================================================
# Description
# ============================================================================

# The previous script (unpack_ribits_banks_sites.R) extracted the raw tabular
# data from "combined_bank_details.rds", unpacked it into separate tables,
# and saved them as RDS files. To use the data, we need to now process the
# spatial components of the data.

# This script takes the tabular data extracted from the raw RIBITS download
# and processes its spatial components. This version processes data in chunks
# to handle large service areas without running out of memory.

# Workflow:
# 1. Loads the main bank data, raw footprints, and raw service areas.
# 2. Defines robust helper functions to parse and validate GeoJSON strings.
# 3. Processes each of the three geometry types, handling errors and
#    aggregating multi-part geometries (like service areas).
# 4. Joins the three cleaned geometry types into a single table.
# 5. Converts the final table into a true 'sf' object and saves it as
#    a multi-layer GeoPackage file with separate layers for each geometry type.


# =============================================================================
# 1. Load Packages
# =============================================================================

library(dplyr)      # for data manipulation
library(purrr)      # for functional programming
library(here)       # for file path construction
library(sf)         # for spatial data processing
library(glue)       # for string interpolation
library(fs)         # for file system operations

# Attempt to disable s2 backend (more memory efficient)
# The s2 backend requires additional memory for processing, so we try to
# disable it. If it fails, we display a message but continue without it.
# This helps with processing the large service areas
tryCatch({
  # Disable s2 backend
  sf::sf_use_s2(FALSE)
}, error = function(e) {
  # If we can't disable s2 backend, we display a message and continue
  message("Note: Could not disable s2 backend, will use default")
})


# =============================================================================
# 2. Helper Functions (Memory Optimized)
# =============================================================================

# Function to parse GeoJSON strings. Handles invalid GeoJSON, trims whitespace,
# and combines multiple geometries into a single sfg object. Returns NULL if the
# input is invalid or if parsing fails.
parse_sfg <- possibly(function(geojson_string) {

  # First, check if the input is invalid
  if (is.null(geojson_string) || !is.character(geojson_string) ||
        length(geojson_string) == 0 || nchar(trimws(geojson_string)) < 10) {
    return(NULL) # Invalid input, return NULL
  }

  # Trim whitespace from the input
  trimmed_geojson <- trimws(geojson_string)

  # Check if the input starts and ends with curly braces
  if (!grepl("^\\s*\\{", trimmed_geojson) ||
        !grepl("}\\s*$", trimmed_geojson)) {
    return(NULL) # Invalid input, return NULL
  }

  # Try to parse the GeoJSON string into an sfc object
  tryCatch({
    # Use st_read to parse the GeoJSON string into an sfc object
    # The quiet argument suppresses warnings from st_read
    geometry_sfc <- st_read(trimmed_geojson, quiet = TRUE)

    # Check if we got any geometry
    if (nrow(geometry_sfc) == 0) {
      return(NULL) # No geometry, return NULL
    }

    # Extract the geometry column from the sfc object
    geometry_sfc <- st_geometry(geometry_sfc)

    # If there are multiple geometries, combine them into a single sfg object
    if (length(geometry_sfc) > 1) {
      st_union(geometry_sfc)
    } else {
      geometry_sfc[[1]] # There is only one geometry, return it
    }
  }, error = function(e) {
    return(NULL) # Parsing failed, return NULL
  })
}, otherwise = NULL)


# Function to process spatial data in chunks to manage memory. It takes a data
# frame, column names for the bank ID and geometry, and a chunk size as input.
# It filters the data frame to rows where the geometry column is not NULL or
# empty, parses the geometry column using the parse_sfg function, and returns a
# data frame with the BANK_ID column and a geometry column containing sfg
# objects. The function processes the data in chunks to manage memory.
process_spatial_data_chunked <- function(df,
                                         id_col,
                                         geo_col,
                                         chunk_size = 1000) {

  # Filter valid data first
  # Select the BANK_ID column and the geometry column,
  # and filter out rows where the geometry column is NULL or empty
  valid_data <- df %>%
    select(BANK_ID = {{id_col}}, geojson_string = {{geo_col}}) %>%
    filter(!is.na(geojson_string) & geojson_string != "")

  # If there are no valid data rows, return an empty data frame
  if (nrow(valid_data) == 0) {
    return(tibble())
  }

  # Process in chunks to manage memory
  # Split the valid data into chunks of the specified size
  chunks <- split(valid_data,
                  ceiling(seq_len(nrow(valid_data)) / chunk_size))

  # Process each chunk separately
  results <- map_dfr(chunks, function(chunk) {
    chunk %>%
      # Parse the geometry column using the parse_sfg function
      mutate(geometry = map(geojson_string, parse_sfg)) %>%
      # Filter out rows where the geometry column contains NULL values
      filter(!map_lgl(geometry, is.null)) %>%
      select(BANK_ID, geometry)
  })

  # Clean up memory
  gc()

  # Return the results
  return(results)
}

# =============================================================================
# 3. Load and Process Data
# =============================================================================

# Helpful to have messages along the way to monitor code progress :)
message("Starting spatial data processing...")

# Load data with memory monitoring
message("Loading data files...")

# Main bank file
banks_main <- readRDS(here("data",
                           "data_intermediate",
                           "banks_main.rds"))

# Footprint file
footprints_raw <- readRDS(here("data",
                               "data_intermediate",
                               "bank_footprint.rds"))

# Service area file
service_areas_raw <- readRDS(here("data",
                                  "data_intermediate",
                                  "service_areas.rds"))

message(sprintf("Loaded: %d banks, %d footprints, %d service areas",
                nrow(banks_main),
                nrow(footprints_raw),
                nrow(service_areas_raw)))

# Process centroids
message("Processing centroids...")
centroids <- process_spatial_data_chunked(banks_main,
                                          BANK_ID,
                                          BANK_LOCATION_CENTROID,
                                          chunk_size = 2000)
message(sprintf("Processed %d valid centroids", nrow(centroids)))
gc()  # Clean memory

# Process footprints
footprints <- process_spatial_data_chunked(footprints_raw,
                                           BANK_ID,
                                           raw_geojson,
                                           chunk_size = 1000)
message(sprintf("Processed %d valid footprints", nrow(footprints)))
gc()  # Clean memory

# Process service areas
message("Processing service areas (this may take a while)...")
service_areas <- process_spatial_data_chunked(service_areas_raw,
                                              BANK_ID,
                                              GEOM,
                                              chunk_size = 500)
message(sprintf("Processed %d valid service areas", nrow(service_areas)))
gc()  # Clean memory

# =============================================================================
# 4. Aggregate Service Areas
# =============================================================================

# Step 4.1: Aggregate service areas
#    - This step is memory efficient and designed to handle large datasets
#    - It provides detailed reporting on the variability of service areas
#    - It also reports banks with no service areas

message("Aggregating service areas...")

# Check if there are any service areas
if (nrow(service_areas) > 0) {

  # Get banks with multiple service areas
  bank_counts <- service_areas %>%
    group_by(BANK_ID) %>%
    summarise(count = n(), .groups = "drop")
  # count > 1 means the bank has multiple service areas
  multi_area_banks <- bank_counts %>%
    filter(count > 1)
  message(sprintf("Banks with multiple service areas: %d",
                  nrow(multi_area_banks)))

  # Simple aggregation - keep first geometry for single areas,
  # create simple list for multiple areas
  service_areas_aggregated <- service_areas %>%
    group_by(BANK_ID) %>%
    summarise(
      geometry = list(geometry),
      original_count = n(),
      .groups = "drop"
    ) %>%
    mutate(
      # For single geometries, extract them
      geometry = map(geometry, function(geom_list) {
        if (length(geom_list) == 1) {
          return(geom_list[[1]])
        } else {
          # For multiple geometries, try union but fallback to first if fails
          tryCatch({
            st_union(st_sfc(geom_list, crs = 4326))
          }, error = function(e) {
            return(geom_list[[1]])  # Return first geometry as fallback
          })
        }
      })
    ) %>%
    filter(!map_lgl(geometry, is.null))

  # Detailed service area variability reporting
  service_area_counts <- service_areas_aggregated %>%
    count(original_count, name = "num_banks") %>%
    arrange(original_count)

  message("Service area variability breakdown:")
  for(i in seq_along(service_area_counts$original_count)) {
    count <- service_area_counts$original_count[i]
    banks <- service_area_counts$num_banks[i]
    if (count == 1) {
      message(sprintf("   - Banks with 1 service area: %d", banks))
    } else {
      message(sprintf("   - Banks with %d service areas: %d", count, banks))
    }
  }

  # Report banks with NO service areas
  banks_with_no_service <- setdiff(banks_main$BANK_ID,
                                   service_areas_aggregated$BANK_ID)
  message(sprintf("Banks with NO service areas: %d",
                  length(banks_with_no_service)))

  message(sprintf("Aggregated service areas for %d unique banks",
                  nrow(service_areas_aggregated)))

  # Keep individual service areas
  service_areas_individual <- service_areas

} else {
  service_areas_aggregated <- tibble()
  service_areas_individual <- tibble()
}

gc()  # Clean memory

# =============================================================================
# 5. Merge and Create Final Dataset
# =============================================================================

message("Merging spatial data...")

# Merge all geometries
bank_geometries <- banks_main %>%

  # Select and rename centroid
  select(BANK_ID, BANK_NAME, STATE_LIST, DISTRICT) %>%
  left_join(centroids, by = "BANK_ID") %>%
  rename(centroid = geometry) %>%

  # Select and rename footprint
  left_join(footprints, by = "BANK_ID") %>%
  rename(footprint = geometry) %>%

  # Select and rename service area
  left_join(service_areas_aggregated, by = "BANK_ID") %>%
  rename(service_area = geometry) %>%
  filter(!map_lgl(centroid, is.null))

# Print a message with the number of banks with valid centroids
message(sprintf("Merged data for %d banks with valid centroids",
                nrow(bank_geometries)))

# Report completeness
# Count the number of valid geometries, map_lgl returns a logical vector
centroid_count <- sum(!map_lgl(bank_geometries$centroid, is.null))
footprint_count <- sum(!map_lgl(bank_geometries$footprint, is.null))
service_area_count <- sum(!map_lgl(bank_geometries$service_area, is.null))

message("Geometry completeness summary:")
message(sprintf("   - Centroids: %d/%d (%.1f%%)",
                centroid_count, nrow(bank_geometries),
                100 * centroid_count / nrow(bank_geometries)))
message(sprintf("   - Footprints: %d/%d (%.1f%%)",
                footprint_count, nrow(bank_geometries),
                100 * footprint_count / nrow(bank_geometries)))
message(sprintf("   - Service areas: %d/%d (%.1f%%)",
                service_area_count, nrow(bank_geometries),
                100 * service_area_count / nrow(bank_geometries)))

# =============================================================================
# 6. Save GeoPackage (Layer by Layer)
# =============================================================================

# Set output directory and file paths
output_dir <- here("data", "data_intermediate")
output_file <- file.path(output_dir, "bank_geometries.gpkg")
fs::dir_create(output_dir)


# Function to create a valid sf object from a list of sfg objects
# - Returns NULL if the list is empty or all objects are NULL
# - Returns NULL if the list is not empty but has invalid geometries
# - Returns sf object with valid geometries if the list is not empty and
# all objects are valid
# - Prints a warning message if the list has invalid geometries
# - Returns sf object with valid geometries if the list is not empty but
# has invalid geometries
# Note: geometry list can have multiple geometries

create_sfc_safe <- function(sfg_list, crs = 4326) {

  # Filter out NULL geometries
  valid_sfg <- compact(sfg_list)

  # Return NULL if the list is empty or all objects are NULL
  if (length(valid_sfg) == 0) {
    return(NULL)
  }
  # Try to create sf object with valid geometries
  tryCatch({
    # Create sf object with valid geometries
    st_sfc(valid_sfg, crs = crs)
  }, error = function(e) {
    # Print a warning message if the list has invalid geometries
    message(sprintf("Warning: sfc creation failed: %s", e$message))
    return(NULL)
  })
}

message("Saving GeoPackage layers...")

# Layer 1: Centroids
if (nrow(bank_geometries) > 0) {

  # Get valid centroids data
  valid_centroids_data <- bank_geometries %>%
    filter(!map_lgl(centroid, is.null)) %>%
    select(BANK_ID, BANK_NAME, STATE_LIST, DISTRICT)

  # Extract geometry list separately
  centroid_geometries <- bank_geometries %>%
    filter(!map_lgl(centroid, is.null)) %>%
    pull(centroid)

  # Create sf object by combining data and geometry
  # Combine data and geometry into sf object
  # Delete the centroids_sf object after saving to free memory
  centroids_sf <- st_sf(valid_centroids_data,
                        geometry = st_sfc(centroid_geometries, crs = 4326))
  if (!is.null(centroids_sf) && nrow(centroids_sf) > 0) {
    st_write(centroids_sf, output_file, layer = "bank_centroids",
             delete_layer = TRUE, quiet = FALSE)
    message(sprintf("Saved %d centroids", nrow(centroids_sf)))
    # Delete the sf object and force garbage collection
    # This helps free memory during the script execution
    rm(centroids_sf)
    gc()
  }
}

# Layer 2: Footprints
footprint_count <- sum(!map_lgl(bank_geometries$footprint, is.null))
if (footprint_count > 0) {

  # Get valid footprints data
  valid_footprints_data <- bank_geometries %>%
    filter(!map_lgl(footprint, is.null)) %>%
    select(BANK_ID, BANK_NAME)

  # Extract geometry list separately
  footprint_geometries <- bank_geometries %>%
    filter(!map_lgl(footprint, is.null)) %>%
    pull(footprint)

  # Create sf object by combining data and geometry
  footprints_sf <- st_sf(valid_footprints_data,
                         geometry = st_sfc(footprint_geometries, crs = 4326))
  if (!is.null(footprints_sf) && nrow(footprints_sf) > 0) {
    st_write(footprints_sf, output_file, layer = "bank_footprints",
             delete_layer = TRUE, quiet = FALSE)
    message(sprintf("Saved %d footprints", nrow(footprints_sf)))
    rm(footprints_sf)
    gc()
  }
}

# Layer 3: Service Areas (aggregated)
# The goal here is to validate the service areas geometries and create a single
# sf object to save to the GeoPackage file. However, the geometry list can
# contain multiple geometries, some of which may be invalid. We need to filter
# out the invalid geometries, and if there are any, we need to save the valid
# ones to the GeoPackage file.
# Count the number of service areas (geometries) in the data
service_area_count <- sum(!map_lgl(bank_geometries$service_area, is.null))

# Only process if there are valid service areas
if (service_area_count > 0) {

  # Get valid service areas data with better NULL filtering
  # - Filter out any NULL service areas
  # - Filter out any service areas that are not of type sfg
  # - Select only the necessary columns
  valid_service_areas_data <- bank_geometries %>%
    filter(!map_lgl(service_area, is.null)) %>%
    filter(!map_lgl(service_area, ~is.null(.x) | !inherits(.x, "sfg"))) %>%
    select(BANK_ID, BANK_NAME, original_count)

  # Extract only valid geometries
  # - Filter out any NULL service areas
  # - Filter out any service areas that are not of type sfg
  # - Extract the service area geometries
  service_area_geometries <- bank_geometries %>%
    filter(!map_lgl(service_area, is.null)) %>%
    filter(!map_lgl(service_area, ~is.null(.x) | !inherits(.x, "sfg"))) %>%
    pull(service_area)

  # Count the number of valid geometries
  cat(sprintf("Found %d valid service area geometries\n",
              length(service_area_geometries)))

  # Create sf object with extra validation
  # - Attempt to create sf object, validating the geometries
  # - If there is an error, try to save individual geometries that work
  tryCatch({

    # Create sf object with validated geometries
    service_areas_sf <- st_sf(valid_service_areas_data,
                              geometry = st_sfc(service_area_geometries,
                                                crs = 4326))

    # Only save if the sf object is not NULL and has rows
    if (!is.null(service_areas_sf) && nrow(service_areas_sf) > 0) {
      st_write(service_areas_sf, output_file, layer = "bank_service_areas",
               delete_layer = TRUE, quiet = FALSE)
      message(sprintf("Saved %d aggregated service areas",
                      nrow(service_areas_sf)))

      # Remove the sf object and force garbage collection
      rm(service_areas_sf)
      gc()
    }

  }, error = function(e) {
    message(sprintf("Service areas save failed: %s", e$message))

    # Fallback: try to save individual geometries that work
    valid_indices <- which(!map_lgl(service_area_geometries,
                                    ~is.null(.x) | !inherits(.x, "sfg")))

    # Only save if there are valid geometries
    if (length(valid_indices) > 0) {

      # Extract the valid data and geometries
      fallback_data <- valid_service_areas_data[valid_indices, ]
      fallback_geoms <- service_area_geometries[valid_indices]

      # Create sf object with validated geometries
      service_areas_sf_fallback <- st_sf(fallback_data,
                                         geometry = st_sfc(fallback_geoms,
                                                           crs = 4326))

      # Save the fallback sf object
      st_write(service_areas_sf_fallback,
               output_file,
               layer = "bank_service_areas",
               delete_layer = TRUE,
               quiet = FALSE)

      message(sprintf("Saved fallback service areas: %d geometries",
                      nrow(service_areas_sf_fallback)))

      # Remove the sf object and force garbage collection
      rm(service_areas_sf_fallback)
      gc()
    }
  })
}

# Layer 4: Individual Service Areas
if (nrow(service_areas_individual) > 0) {
  # Get valid individual service areas data
  valid_individual_data <- service_areas_individual %>%
    left_join(banks_main %>%
                select(BANK_ID, BANK_NAME),
              by = "BANK_ID") %>%
    select(BANK_ID, BANK_NAME)

  # Extract geometry list separately
  individual_geometries <- service_areas_individual %>%
    pull(geometry)

  # Create sf object by combining data and geometry
  individual_with_names <- st_sf(valid_individual_data,
                                 geometry = st_sfc(individual_geometries,
                                                   crs = 4326))

  # Only save if the sf object is not NULL and has rows
  if (!is.null(individual_with_names) && nrow(individual_with_names) > 0) {
    st_write(individual_with_names,
             output_file,
             layer = "bank_service_areas_individual",
             delete_layer = TRUE,
             quiet = FALSE)
    message(sprintf("Saved %d individual service areas",
                    nrow(individual_with_names)))

    # Remove the sf object and force garbage collection
    rm(individual_with_names)
    gc()
  }
}

# Final verification
if (file.exists(output_file)) {
  final_size <- file.size(output_file)
  message(sprintf("\nSUCCESS! Multi-layer GeoPackage created: %s (%.2f MB)",
                  basename(output_file), final_size / 1024^2))
} else {
  message("ERROR: GeoPackage file was not created!")
}

message("\nProcessing completed!")
