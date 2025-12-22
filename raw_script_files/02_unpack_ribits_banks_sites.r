# =============================================================================
# Script:     unpack_ribits_banks_sites.R
# =============================================================================

# =============================================================================
# Description
# =============================================================================

# The previous script (extract_ribits_banks_sites.R) downloaded the RIBITS data
# and saved it as a single RDS file. However, that file was extremely large,
# and contained nested elements that were not easily accessible.

# This script takes the raw, nested list data downloaded from the RIBITS API
# (the `combined_bank_details.rds` file) and transforms it into a series of
# clean, relational tables. The output is a set of `.rds` files, each
# corresponding to a different component of the bank data, making it easy
# to use for downstream analysis.

# Workflow:
# 1. Loads the raw `combined_bank_details.rds` file.
# 2. Separates top-level bank attributes (e.g., name, status) from complex
#    nested fields (e.g., contacts, ledger).
# 3. Processes and saves all flat attributes into a single primary table
#    (`banks_main.rds`).
# 4. Iterates through each nested field (contacts, ledger, service areas, etc.)
#    and processes each one into its own separate, tidy table.
# 5. Uses a specialized handler to extract raw GeoJSON strings from the
#    geospatial fields.
# 6. Identifies and logs any records with malformed or inconsistent data into
#    a final summary CSV (`all_extraction_issues.csv`) for quality control.


# =============================================================================
# 1. Load Packages
# =============================================================================

library(dplyr)   # For data manipulation
library(purrr)   # For functional programming
library(tibble)  # For tibble data structures
library(readr)   # For reading and writing CSV files
library(here)    # For constructing file paths
library(glue)    # For string interpolation
library(fs)      # For file system operations


# =============================================================================
# 2. Configure Paths
# =============================================================================

# Define the source file and output directory for processed tables.
input_file <- here("data", "data_raw", "ribits", "combined_bank_details.rds")
output_dir <- here("data", "data_intermediate")

# Ensure the output directory exists before saving files to it.
dir_create(output_dir)


# =============================================================================
# 3. Load and Prepare Data
# =============================================================================

# Load the raw, nested list data downloaded from RIBITS.
raw_data <- readRDS(input_file)

# Extract the main content from the nested 'ITEMS' list for each record.
# Then, filter out any records that are missing a BANK_ID.
flattened <- raw_data %>%
  map(~ .x$ITEMS[[1]]) %>%
  keep(~ !is.null(.x$BANK_ID))

# Stop the script if no valid data was loaded.
if (length(flattened) == 0) {
  stop("No valid records with BANK_IDs found in the input file.")
}


# =============================================================================
# 4. Process Flat Attributes
# =============================================================================

# Create the main bank table by extracting all simple, non-list attributes.
# This loop processes each bank record independently to handle inconsistencies.
banks_main <- map_dfr(flattened, function(bank) {
  # Identify fields that are single, atomic values (not lists).
  is_atomic <- map_lgl(bank, ~is.atomic(.x) && length(.x) == 1)
  flat_subset <- bank[is_atomic]

  # Filter out any fields that have missing or empty names.
  valid_names <- names(flat_subset)
  final_subset <- flat_subset[!is.na(valid_names) & valid_names != ""]

  # Replace any NULLs with NA for consistency before creating the table.
  final_subset[sapply(final_subset, is.null)] <- NA

  # Convert the cleaned list of attributes into a one-row table.
  as_tibble(final_subset)
})

# Save the final main table of bank attributes.
saveRDS(banks_main, file.path(output_dir, "banks_main.rds"))
message(glue("Extracted: banks_main ({nrow(banks_main)} rows)"))


#=============================================================================
# 5. Define Reusable Functions for Nested Data
#=============================================================================

# A function to process standard nested lists (e.g., contacts, ledger).
handle_nested_list <- function(field_content) {
  # Return early if the field is empty.
  if (is.null(field_content) || length(field_content) == 0) return(NULL)

  # Normalize the structure to a consistent list-of-lists.
  if (!is.list(field_content[[1]])) {
    field_content <- list(field_content)
  }

  # Keep only valid rows that have proper names, filtering out malformed data.
  valid_rows <- field_content %>%
    keep(~ is.list(.x) && !is.null(names(.x)))

  if (length(valid_rows) == 0) return(NULL)

  # Convert the list of valid rows into a data frame.
  map_dfr(valid_rows, function(row_list) {
    row_list[sapply(row_list, is.null)] <- NA
    # Coerce all columns to character to prevent data type conflicts.
    as_tibble(row_list) %>%
      mutate(across(everything(), as.character))
  })
}

# A specialized function to extract raw GeoJSON strings from geospatial fields.
# Works for bank footprints, centroids, and service areas.
# This version has better error handling and debugging.
handle_geojson <- function(field_content) {
  # Return early if the field is empty.
  if (is.null(field_content) || length(field_content) == 0) return(NULL)
  # Helper function to find any GeoJSON-like field in nested lists.
  find_geojson_field <- function(x, depth = 0) {
    # Prevent infinite recursion
    if (depth > 10) return(NULL)
    # Stop digging if x is not a list.
    if (!is.list(x)) return(NULL)
    # Look for common GeoJSON field names.
    geojson_fields <- c("GEOM", "CENTROID", "SERVICE_AREA",
                        "FOOTPRINT", "GEOMETRY", "SHAPE", "POLYGON")
    # Check current level for GeoJSON fields
    for (field_name in geojson_fields) {
      if (!is.null(x[[field_name]])) {
        field_value <- x[[field_name]]
        # Check if it's a valid character string and not "null"
        if (is.character(field_value) && length(field_value) > 0 &&
              field_value != "null" && !is.na(field_value) &&
              nchar(trimws(field_value)) > 4) {  # Must be longer than "null"
          return(field_value)
        }
      }
    }
    # If no GeoJSON field found, try digging deeper into all elements
    for (i in seq_along(x)) {
      result <- find_geojson_field(x[[i]], depth + 1)
      if (!is.null(result)) return(result)
    }
    return(NULL)
  }
  # Try to find the GeoJSON string using the helper.
  geojson_string <- find_geojson_field(field_content)
  # Return NULL if no geometry was found.
  if (is.null(geojson_string)) {
    return(NULL)
  }
  # Return the valid GeoJSON string in a clean, one-row table.
  tibble(raw_geojson = geojson_string)
}


#=============================================================================
# 7. Create a General-Purpose Extraction Function
#=============================================================================

# Discover all unique field names that exist across all bank records.
all_field_names <- map(flattened, names) %>% unlist() %>% unique()

# Determine which fields are nested lists.
is_list_field <- map_lgl(all_field_names, function(name) {
  # For each field name, find the first record that actually contains it.
  first_instance <- purrr::detect(flattened, ~ !is.null(.x[[name]]))
  # Then, check if the content of that field is a list.
  is.list(first_instance[[name]])
})

# Create the final vector of nested field names to be processed,
# filtering out invalid names and the special BANK_FOOTPRINT case.
nested_fields <- all_field_names[is_list_field]
nested_fields <- nested_fields[!is.na(nested_fields) & nested_fields != ""]
nested_fields <- setdiff(nested_fields, "BANK_FOOTPRINT")


# This is the main engine of the script. It's a function that processes
# all bank records for a given field using a specific "handler" function.
process_field <- function(data, field_name, handler_fn, output_dir) {
  # A list to keep track of any issues found for this field.
  issues <- list()

  # Iterate through each bank, apply the handler, and row-bind the results.
  processed_data <- map_dfr(data, function(bank) {
    # Skip if the current bank record doesn't contain this field at all.
    if (!field_name %in% names(bank)) return(NULL)
    bank_id <- bank$BANK_ID
    field_content <- bank[[field_name]]

    # Skip if the field exists but is empty (NULL).
    if (is.null(field_content)) {
      return(NULL)
    }
    # Safely apply the specific handler (e.g., handle_nested_list),
    # logging any errors without stopping the entire script.
    result_tibble <- tryCatch({
      handler_fn(field_content)
    }, error = function(e) {
      issues[[as.character(bank_id)]] <-
        paste("Handler failed:", e$message)
      return(NULL) #nolint
    })

    # If the handler failed or returned no valid data, log it as an issue.
    if (is.null(result_tibble) || nrow(result_tibble) == 0) {
      if (is.null(issues[[as.character(bank_id)]])) {
        issues[[as.character(bank_id)]] <-
          "Handler returned NULL or empty tibble (likely malformed data)"
      }
      return(NULL)
    }

    # Add the bank's ID to the processed rows for easy joining later.
    result_tibble %>% mutate(BANK_ID = bank_id)
  })

  # Save the processed data for this field to its own .rds file.
  output_name <- tolower(field_name)
  saveRDS(processed_data, file.path(output_dir, glue("{output_name}.rds")))

  # If any issues were logged, save them to a separate .csv file for review.
  if (length(issues) > 0) {
    issues_df <- tibble(BANK_ID = names(issues),
                        issue = unlist(issues))
    write_csv(issues_df,
              file.path(output_dir,
                        glue("{output_name}_issues.csv")))
  }

  # Print a summary message to the console.
  message(glue("Extracted: {output_name} ({nrow(processed_data)} rows)"))
}


# =============================================================================
# 8. Run Extraction for All Nested Fields
# =============================================================================

# Process all standard nested lists using the `handle_nested_list` function.
walk(nested_fields,
     ~ process_field(flattened, .x, handle_nested_list, output_dir))

# Process the special BANK_FOOTPRINT field separately using its specific
# `handle_geojson` function.
process_field(flattened, "BANK_FOOTPRINT", handle_geojson, output_dir)


# =============================================================================
# 9. Add Diagnostics for BANK_FOOTPRINT
# =============================================================================

# Bank footprint gave a bunch of errors, so here we will
# generate a small diagnostic summary to help interpret them.
analyze_footprint_results <- function(output_dir, flattened_data) {
  # Check if bank_footprint data was created
  footprint_file <- file.path(output_dir, "bank_footprint.rds")
  issues_file <- file.path(output_dir, "bank_footprint_issues.csv")
  # Check if the footprint file exists
  if (file.exists(footprint_file)) {
    footprint_data <- readRDS(footprint_file)
    successful_extractions <- nrow(footprint_data)
    # Report the number of successful extractions
    cat("\n")
    cat(rep("=", 60), "\n")
    cat("BANK_FOOTPRINT EXTRACTION SUMMARY\n")
    cat(rep("=", 60), "\n")
    # Check if the issues file exists
    if (file.exists(issues_file)) {
      issues_data <- read.csv(issues_file)
      failed_extractions <- nrow(issues_data)
      # Report the number of failed extractions
      cat("Successful extractions (valid GeoJSON):",
          successful_extractions, "\n")
      cat("Failed extractions (null/missing data):",
          failed_extractions, "\n")
      cat("Total banks with BANK_FOOTPRINT field:",
          successful_extractions + failed_extractions, "\n")
      # Report the success rate
      if (successful_extractions + failed_extractions > 0) {
        success_rate <- round(
                              successful_extractions /
                                (successful_extractions + failed_extractions)
                              * 100, 1)
        cat("Success rate:", success_rate, "%\n")
      }
      # Explain the meaning of failed extractions
      cat("\n'Failed extractions' typically contain 'null' values, which\n")
      cat("indicate banks without spatial footprint data. This is expected\n")
      cat("behavior and not an error in the extraction process.\n")
    } else {
      cat("All", successful_extractions, "banks had valid footprint data.\n")
    }
    # Add detailed breakdown of footprint data structure
    cat("\n--- DETAILED FOOTPRINT DATA BREAKDOWN ---\n")
    # Get banks with BANK_FOOTPRINT for detailed analysis
    banks_with_footprint <- flattened_data %>%
      keep(~ "BANK_FOOTPRINT" %in% names(.x) && !is.null(.x$BANK_FOOTPRINT))
    # Count different scenarios
    valid_geojson <- 0
    null_string <- 0
    empty_field <- 0
    other_structure <- 0
    # Loop through each bank with footprint data
    # For each bank, check the structure of the BANK_FOOTPRINT field
    for (bank in banks_with_footprint) {
      # Access the BANK_FOOTPRINT field of the current bank
      footprint <- bank$BANK_FOOTPRINT
      # Check if the footprint is a list and has at least one element
      # If it is, proceed to examine the structure of the first element
      if (is.list(footprint) && length(footprint) > 0) {
        # Access the first element of the BANK_FOOTPRINT list
        elem <- footprint[[1]]
        # Check if the element is a list and has a GEOM field
        # If it does, proceed to examine the value of the GEOM field
        if (is.list(elem) && "GEOM" %in% names(elem)) {
          # Access the value of the GEOM field in the first element
          geom <- elem$GEOM
          # Check if the value of GEOM is a character string
          # If it is, proceed to categorize it based on its contents
          if (is.character(geom) && length(geom) > 0) {
            # Check if the string is "null"
            if (geom == "null") {
              null_string <- null_string + 1
              # Check if the string starts with whitespace,
              # followed by an opening brace, and has a length greater than 4
            } else if (grepl("^\\s*\\{", geom) && nchar(trimws(geom)) > 4) {
              valid_geojson <- valid_geojson + 1
              # If none of the above conditions are met,
              # categorize as having other structure
            } else {
              other_structure <- other_structure + 1
            }
            # If the value of GEOM is not a character string,
            # categorize as having other structure
          } else {
            other_structure <- other_structure + 1
          }
          # If the first element of the BANK_FOOTPRINT list is not a list
          # or does not have a GEOM field, categorize as having other structure
        } else {
          other_structure <- other_structure + 1
        }
        # If the BANK_FOOTPRINT field is not a list or does not have
        # any elements, categorize as having an empty field
      } else {
        empty_field <- empty_field + 1
      }
    }
    # Print a summary of the results
    cat("Banks with valid GeoJSON polygons:", valid_geojson, "\n")
    cat("Banks with 'null' string values:", null_string, "\n")
    cat("Banks with empty/missing fields:", empty_field, "\n")
    cat("Banks with other data structures:", other_structure, "\n")
    cat("Total banks analyzed:",
        valid_geojson + null_string + empty_field + other_structure, "\n")
    # Calculate the percentage of valid GeoJSON polygons
    if (valid_geojson + null_string + empty_field + other_structure > 0) {
      valid_percent <- round(valid_geojson /
                               (valid_geojson
                                + null_string
                                + empty_field
                                + other_structure) * 100, 1)
      null_percent <- round(null_string /
                              (valid_geojson
                               + null_string
                               + empty_field
                               + other_structure) * 100, 1)
      cat("\nPercentage with actual spatial data:", valid_percent, "%\n")
      cat("Percentage without spatial data:", null_percent, "%\n")
    }
    # Print a separator line
    cat(rep("=", 60), "\n\n")
  }
}

# Run the diagnostic analysis
analyze_footprint_results(output_dir, flattened)


# =============================================================================
# 9. Aggregate and Log All Extraction Issues
# =============================================================================

# This helper function combines all individual issue logs into one master file
# for easy review.
log_all_extraction_issues <- function(output_dir) {
  # Find all the separate issue logs that were created during the run.
  issue_files <- list.files(output_dir,
                            pattern = "_issues\\.csv$",
                            full.names = TRUE)
  if (length(issue_files) == 0) {
    message("No extraction issue files found.")
    return()
  }
  # Read each issue CSV and stack them into a single master table.
  all_issues <- map_dfr(issue_files, read_csv)

  # Save the final aggregated report.
  if (nrow(all_issues) > 0) {
    write_csv(all_issues,
              file.path(output_dir,
                        "all_extraction_issues.csv"))
    message(glue("Aggregated {nrow(all_issues)} extraction issues into ",
                 "'all_extraction_issues.csv'"))
  } else {
    message("No extraction issues found across all fields.")
  }
}

# Run the aggregation function to create the final report.
log_all_extraction_issues(output_dir)

# Manual inspection of the error logs shows that some banks do not have any
# bank footprint data; it simply has a text value of "null" which is why
# we get the error.