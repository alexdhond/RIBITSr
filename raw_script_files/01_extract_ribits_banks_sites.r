# =============================================================================
# Script:     extract_ribits_banks_sites.R
# =============================================================================

# =============================================================================
# Description
# =============================================================================

# This script automates the download of Bank and Site data from the RIBITS
# GeoJSON web service. For more information, go to the RIBITS homepage,
# navigate to "Webservices", then "Banks or Sites" GeoJson.
# Can also see the RIBITS documentation at
# https://ribits.ops.usace.army.mil/ords/RI/public/ or here:
# https://ribits.ops.usace.army.mil/ords/f?p=107:367:::::P367_SECTION:BANK

# Workflow:
# 1. Fetches a master list of all available BANK_IDs.
# 2. Checks a local directory for data that has already been downloaded.
# 3. Identifies which BANK_IDs are still missing.
# 4. Downloads the missing data in manageable batches, pausing between requests.
# 5. Saves each batch incrementally and logs errors.
# 6. After downloading, combines all batches into a single, deduplicated file.
# 7. Provides a final verification step to check for any remaining missing data.

# It takes ~ 2 to 3 hours to download the data. This approach allows for
# stopping and restarting the script at any time.


# =============================================================================
# 1: Setup - Load Packages and Define Parameters
# =============================================================================

# Load necessary R packages
library(httr)      # For making HTTP requests to web APIs.
library(jsonlite)  # To work with JSON data from the API.
library(purrr)     # For functional programming tools (like `map` and `safely`).
library(fs)        # Interacting with the file system (creating directories).
library(here)      # Create file paths relative to the project root.

# Define script parameters

# Email for API usage tracking (Change to your email).
email <- "alexander.k.dhond@gmail.com"

# The directory to save all raw data files.
# `here()` builds a path from the project's top-level folder.
output_dir <- here("data", "data_raw", "ribits")

# Create the output directory if it doesn't already exist.
dir_create(output_dir)

# Set the number of records to download in each batch before saving.
batch_size <- 100

# Set a brief pause (in seconds) between API calls to be polite to the server.
delay_between_calls <- 0.5

# Define the base URLs for the RIBITS API endpoints.
ribits_base_url <- "https://ribits.ops.usace.army.mil/ords/RI/public/"
ribits_list_url <- paste0(ribits_base_url, "bank_site_list/")
ribits_detail_url <- paste0(ribits_base_url, "bank_site_data/")

# Define paths for log files.
error_log_file <- here(output_dir, "error_log.txt")
download_log_file <- here(output_dir, "download_log.txt")


# =============================================================================
# 2: Helper Functions
# =============================================================================

# Function to get detailed data for a single BANK_ID.
# Parameters:
#   bank_id: The numeric ID of the bank to retrieve
#   email: The user's email for API tracking
# Returns: A list with the parsed JSON response, or NULL if the request fails
get_bank_details <- function(bank_id, email = NULL) {

  # The API expects parameters to be wrapped in a JSON query object.
  query_list <- list(
    bank_id = bank_id, # The numeric ID of the bank to retrieve.
    show_service_area = "Yes", # Retrieve the service area polygon.
    show_footprint = "Yes", # Retrieve the footprint polygon.
    show_contacts = "Yes", # Retrieve contact information.
    show_ledger = "Yes" # Retrieve ledger information.
  )
  if (!is.null(email)) {
    query_list$webconsumer_email <- email # The user's email for API tracking.
  }

  # Convert the R list into a single-line JSON string.
  query_json <- toJSON(query_list, auto_unbox = TRUE)

  # Make the GET request to the API. The `q` parameter holds our JSON query.
  res <- GET(ribits_detail_url, query = list(q = query_json))

  # Check the HTTP status code. 200 means success.
  if (status_code(res) == 200) {
    # If successful, parse the JSON content and return it.
    return(content(res, as = "parsed", type = "application/json"))
  } else {
    # If it fails, raise a warning and return nothing.
    warning(paste("Failed to retrieve BANK_ID:",
                  bank_id, "Status:", status_code(res)))
    return(NULL)
  }
}

# Function to extract BANK_IDs from previously saved files.
# Use this to figure out what data we already have. It scans a list of
# .rds files and extracts all unique BANK_IDs contained within them.

# Parameters: files: A character vector of file paths to .rds files
# Returns: A numeric vector of unique BANK_IDs found in the files
extract_bank_ids_from_files <- function(files) {

  # `map` iterates through each file path.
  all_ids <- map(files, function(file) {

    # `tryCatch` handles cases where a file might be empty or corrupt.
    tryCatch({
      # Read the saved R data object.
      raw_data <- readRDS(file)

      # Extract the BANK_ID from each record in the file.
      # The `~` creates a shorthand anonymous function.
      map_int(raw_data, ~ .x$ITEMS[[1]]$BANK_ID)
    }, error = function(e) {

      # If a file can't be read, return an empty vector and continue.
      integer(0)
    })
  })
  # `unlist` converts the list of vectors into a single vector,
  # and `unique` removes any duplicate IDs.
  unique(unlist(all_ids))
}

# Function to log errors to a text file. Appends a new entry to the error
# log file with a timestamp and error details.
# Parameters:
# - bank_id: The ID that caused the error
# - error_msg: The error message to log

log_error <- function(bank_id, error_msg) {
  entry <- paste0(Sys.time(), # The timestamp of the error.
                  " | BANK_ID: ", bank_id, # The ID that caused the error.
                  " | ERROR: ", error_msg, # The error message to log.
                  "\n") # Newline for the next entry.
  cat(entry,
      file = error_log_file, # The error log file.
      append = TRUE) # Append the entry to the error log file.
}


# =============================================================================
# 3: Download Data
# =============================================================================

# Get the master list of all BANK_IDs from RIBITS
master_list_res <- GET(ribits_list_url) # Make the GET request to the API.
stop_for_status(master_list_res,
                "fetch the master bank list") # Stop if the API request fails.

# Parse the JSON response and extract just the numeric BANK_IDs.
response_content <- content(master_list_res)

# Check if the response contains the expected structure; if yes, extract
# the numeric BANK_IDS, if not, stop the script
if (!is.null(response_content$ITEMS) && length(response_content$ITEMS) > 0) {
  # Extract the numeric BANK_IDs from the response.
  # The `map_int` function extracts the `BANK_ID` from each item.
  master_bank_ids <- map_int(response_content$ITEMS, "BANK_ID")
  # Print the number of BANK_IDs found in the response.
  cat("Found", length(master_bank_ids), "total BANK_IDs.\n\n")
} else {
  # If the response does not contain the expected structure, stop the script.
  stop("API response does not contain expected ITEMS structure")
}

# --- Step 3.2: Identify which BANK_IDs have already been downloaded ---
# Find all existing batch files in our output directory.
existing_files <- list.files(output_dir,
                             pattern = "^batch_\\d+\\.rds$",
                             full.names = TRUE)

# Use helper function to get the IDs from these files.
downloaded_ids <- extract_bank_ids_from_files(existing_files)

# Find missing IDs by checking difference between master list and what we have.
pending_ids <- setdiff(master_bank_ids, downloaded_ids)

# Print a status update.
cat("Total IDs:        ", length(master_bank_ids), "\n")
cat("Already downloaded: ", length(downloaded_ids), "\n")
cat("Remaining to fetch: ", length(pending_ids), "\n\n")

# --- Step 3.3: Download pending BANK_IDs in batches ---
if (length(pending_ids) > 0) {
  # Split the list of missing IDs into smaller chunks (batches).
  batches <- split(pending_ids, ceiling(seq_along(pending_ids) / batch_size))
  # To avoid overwriting old files, find highest batch number
  # and start from the next one.
  existing_batch_nums <- as.integer(
                                    gsub("^batch_(\\d+)\\.rds$", "\\1",
                                         basename(existing_files)))
  next_batch_num <- if (
                        length(
                               existing_batch_nums) > 0)
    max(existing_batch_nums) + 1 else 1
  # `safely()` is a wrapper from `purrr`. Instead of stopping on an error,
  # it returns a list with two parts: `$result` and `$error`. This allows
  # the loop to continue even if some API calls fail.
  safe_get_details <- safely(get_bank_details, otherwise = NULL)
  # Loop through each batch of IDs.
  for (i in seq_along(batches)) {
    batch_ids <- batches[[i]] # Get the IDs for this batch.
    current_batch_num <- next_batch_num + i - 1 # Get the next batch number.
    cat(sprintf("Downloading batch %d of %d (%d IDs)...\n",
                i, length(batches), length(batch_ids)))
    # Pre-allocate a list to store results for this batch.
    batch_results <- vector("list", length(batch_ids))
    # Loop through each ID in the current batch.
    for (j in seq_along(batch_ids)) {
      id <- batch_ids[j] # Get the ID for this record.
      # Pause execution briefly to avoid hammering the server.
      Sys.sleep(delay_between_calls)
      # Call the API using our "safe" function.
      result <- safe_get_details(id, email = email)
      batch_results[[j]] <- result # Store the result in the batch list.
      # Log any errors that occurred.
      if (!is.null(result$error)) {
        log_error(id, as.character(result$error)) # Log the error.
      }
      # Print progress within the batch.
      if (j %% 10 == 0 || j == length(batch_ids)) {
        cat(sprintf("  ...progress: %d/%d\n",
                    j, length(batch_ids))) # Print progress.
      }
    }
    # After the batch is done, extract only the successful results.
    # `map` gets the `$result` from each item, and `compact` removes any NULLs.
    successful_results <- compact(map(batch_results, "result"))
    # Save the successful results to an RDS file.
    if (length(successful_results) > 0) {
      # Create the batch file name.
      batch_file <- here(output_dir,
                         paste0("batch_", current_batch_num, ".rds"))
      # Save the successful results to an RDS file.
      saveRDS(successful_results, batch_file)
      # Print a message indicating the number of records saved.
      cat("Saved",
          length(successful_results),
          "records to:",
          basename(batch_file),
          "\n\n")
      # Log the download event.
      log_entry <- paste0(Sys.time(),
                          " | ",
                          basename(batch_file),
                          " | ",
                          length(successful_results),
                          " records\n")
      cat(log_entry, file = download_log_file, append = TRUE)
    } else {
      cat("No records were successfully downloaded for this batch.\n\n")
    }
  }
} else {
  cat("No new data to download. All BANK_IDs are already present.\n")
}


# =============================================================================
# 4: Finalization - Combine, Verify, and Clean Up
# =============================================================================

# Combine all downloaded batches into a single file
cat("Combining all downloaded batch files...\n")

# Get a fresh list of ALL batch files.
all_batch_files <- list.files(output_dir,
                              pattern = "^batch_\\d+\\.rds$",
                              full.names = TRUE)

if (length(all_batch_files) > 0) {
  # Read all .rds files into one giant list.
  # `flatten` removes one level of nesting, creating a simple list of records.
  combined_data <- flatten(map(all_batch_files, readRDS))
  # Extract all bank IDs from the combined data.
  # Add error handling for malformed data
  tryCatch({
    all_downloaded_ids <- map_int(combined_data, ~ {
      if (!is.null(.x$ITEMS) && length(.x$ITEMS) > 0) {
        .x$ITEMS[[1]]$BANK_ID
      } else {
        stop("Record missing ITEMS structure")
      }
    })
  }, error = function(e) {
    stop("Error extracting BANK_IDs from combined data: ", e$message)
  })
  # Remove duplicate records, keeping only the first occurrence of each BANK_ID.
  deduped_data <- combined_data[!duplicated(all_downloaded_ids)]
  # Save the final, combined file.
  final_file <- here(output_dir, "combined_bank_details.rds")
  saveRDS(deduped_data, final_file)
  # Print a message indicating the number of records saved.
  cat("Successfully combined", length(all_batch_files), "batch files.\n")
  cat("Final combined file saved with",
      length(deduped_data),
      "unique records to:", basename(final_file), "\n\n")
} else {
  cat("No batch files found to combine.\n")
}

# Final verification
cat("Verifying final dataset...\n")

# Check if we have any data to verify
if (exists("deduped_data") && length(deduped_data) > 0) {
  final_ids <- map_int(deduped_data, ~ .x$ITEMS[[1]]$BANK_ID)
  still_missing_ids <- setdiff(master_bank_ids, final_ids)

  if (length(still_missing_ids) == 0) {
    cat("Success! All",
        length(master_bank_ids),
        "BANK_IDs have been downloaded and are in the final combined file.\n")
  } else {
    cat("Warning:",
        length(still_missing_ids),
        "BANK_IDs are still missing from the final file.\n")
    cat("Re-run the script to attempt downloading them again.\n")
    cat("Missing IDs:",
        paste(still_missing_ids, collapse = ", "),
        "\n")
  }
} else {
  cat("No data available, check if downloads were successful.\n")
}

# Optional cleanup
# After confirming the `combined_bank_details.rds` file is complete and correct,
# you can uncomment the code below to delete the individual batch files.

# cat("\nCleaning up individual batch files...\n")
# file_delete(all_batch_files) # Delete all batch files.
# cat("Deleted",
#     length(all_batch_files),
#     "batch files.\n")
