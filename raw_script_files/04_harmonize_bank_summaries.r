# =============================================================================
# Script:     harmonize_bank_summaries.R
# =============================================================================

# =============================================================================
# Description
# =============================================================================

# This script harmonizes and consolidates multiple RIBITS datasets that were
# extracted from the GeoJson API or manually downloaded. The goal is to try and
# get as much available information from RIBITS into a single, usable dataset.

# Workflow:
# 1. Loads the main bank data (banks_main.rds) and supplementary CSV files
# 2. Joins datasets using bank_id as the primary key
# 3. Automatically merges duplicate columns using coalesce() logic
# 4. Manually handles specific column mappings and standardizations
# 5. Cleans and standardizes date fields
# 6. Outputs a final, harmonized dataset ready for analysis

# Input files:
# - banks_main.rds: Main bank data from unpack_ribits_banks_sites.R
# - Bank Summary_10_11_2025.csv: Bank summary information
# - Bank Credit Classification by Jurisdiction_10_11_2025.csv:
#   Credit classifications
# - Bank and ILF Program Credit Tracking 2025_11_10.csv: Credit tracking data
# - Bank & ILF Program Service Area Comments_10_11_2025.csv:
#   Service area comments

# Output:
# - A harmonized dataset with clean, consistent column names and data types


# =============================================================================
# 1. Load Packages
# =============================================================================

library(tidyverse)       # Data manipulation and analysis (dplyr, tidyr, etc.)
library(janitor)         # Data cleaning utilities
library(data.table)      # Fast data reading with fread()
library(here)            # File path handling
library(lubridate)       # Date handling and parsing
library(stringr)         # String operations and manipulation


# =============================================================================
# 2. Configure Paths
# =============================================================================

# Define paths for raw data and processed intermediate files
raw_data_path <- here("data", "data_raw", "ribits")
banks_main_path <- here("data",
                        "data_intermediate",
                        "banks_main.rds")

# Load banks_main.rds
banks_main <- readRDS(banks_main_path)

# List all CSV files in raw data directory
csv_files <- list.files(raw_data_path, pattern = "\\.csv$", full.names = TRUE)

# Load in all the CSV files. All we need is the bank_summary and credit_class

# Bank Summary
bank_summary_file <- file.path(raw_data_path,
                               "Bank Summary_10_11_2025.csv")
bank_summary <- fread(bank_summary_file)

# Credit Classification
credit_class_file <- file.path(raw_data_path,
                               "Bank Credit Classification by Jurisdiction_10_11_2025.csv") #nolint
credit_class <- fread(credit_class_file)

# =============================================================================
# BANK SUMMARY PROCESSING
# =============================================================================

# Here I clean the bank summary data and join it to the main bank data.

# -----------------------------------------------------------------------------
# A: Clean names, remove columns, join
# -----------------------------------------------------------------------------

# Clean names of banks_main and remove columns that we don't need
banks_main <- banks_main %>%
  clean_names() %>%
  select(-c(ilf_program_data_ws_url,
            secondary_district_list,
            secondary_office_list,
            umbrella_instrument_data_ws_url,
            closure_date,
            ribits_url_to_bank,
            bank_geometry_obscured,
            comments,
            website))

# Clean names of bank summary as well
bank_summary <- bank_summary %>% clean_names()

# Inner Join: Automatically keeps only banks present in both
banks_joined <- banks_main %>%
  inner_join(bank_summary, by = "bank_id", suffix = c("_main", "_summary"))


# -----------------------------------------------------------------------------
# B: Standardize Text (Pre-Merge)
# -----------------------------------------------------------------------------

# Fix spacing issues now so they match during the coalesce step
banks_joined <- banks_joined %>%
  mutate(across(
    matches("^(state|county)_list_(main|summary)$"),
    ~ .x %>%
      str_replace_all(",\\s*", ", ") %>%  # Force "comma-space"
      str_squish()                        # Kill extra whitespace
  ))


# -----------------------------------------------------------------------------
# C: Automatic merge
# -----------------------------------------------------------------------------

# Identify columns that got suffixed (the duplicates)
dupe_cols <- names(banks_joined) %>%
  str_subset("_main$") %>%
  str_remove("_main$")

# Loop through duplicates, coalesce them, and remove the old columns
for (col in dupe_cols) {
  col_main <- paste0(col, "_main")
  col_summ <- paste0(col, "_summary")

  # Coalesce and remove old columns
  banks_joined <- banks_joined %>%
    mutate(!!col := coalesce(.data[[col_main]], .data[[col_summ]])) %>%
    select(-all_of(c(col_main, col_summ)))
}


# -----------------------------------------------------------------------------
# D: Manual merge (Specific Columns)
# -----------------------------------------------------------------------------

# We do this in one big block for efficiency
banks_joined <- banks_joined %>%
  mutate(
    # 1. Acres
    total_acres = coalesce(total_acres, bank_acres),

    # 2. Names (Prioritize bank_name, fall back to name)
    bank_name = coalesce(bank_name, name),

    # 3. Years (Combine abbreviation with full name)
    year_established = coalesce(year_established, year_est),

    # 4. NMFS (Combine all 3 variations)
    nmfs_region = coalesce(nmfs_region, nmfs, nmfs_region_list),

    # 5. Field Office (Prioritize LIST first to capture multiple offices)
    field_office = coalesce(field_office_list, field_office)
  ) %>%
  # Remove all the redundant source columns in one go
  select(-bank_acres,
         -name,
         -year_est,
         -nmfs,
         -nmfs_region_list,
         -field_office_list)


# -----------------------------------------------------------------------------
# E: Date cleaning & final polish
# -----------------------------------------------------------------------------

banks_joined <- banks_joined %>%
  # Parse all date columns
  mutate(across(
    ends_with("date"),
    ~ .x %>%
      as.character() %>%   # Force to text to handle logicals/NAs
      na_if("") %>%        # Convert empty strings to NA
      mdy()                # Parse as Date
  )) %>%
  # Fill missing 'year_established' using the newly parsed 'establishment_date'
  mutate(
    year_established = coalesce(year_established, year(establishment_date))
  )

# -----------------------------------------------------------------------------
# F: Final check
# -----------------------------------------------------------------------------

cat("Banks Joined dimensions:",
    dim(banks_joined)[1], "rows,",
    dim(banks_joined)[2], "columns.\n")

glimpse(banks_joined)

# =============================================================================
# CREDIT CLASSIFICATION PROCESSING
# =============================================================================

# Here I clean the credit classification data and join it to the main bank data

# -----------------------------------------------------------------------------
# A: Clean, Trim, and Rename
# -----------------------------------------------------------------------------

# Clean names
credit_class <- credit_class %>% clean_names()

# Trim to relevant columns and rename to avoid collision
# We rename specific credit amounts so they don't clash with bank totals later
credits_cleaned <- credit_class %>%
  select(
    bank_id,
    credit_classification_type,
    credit_classification,
    # Rename these to be specific
    credit_specific_available  = available_credits,
    credit_specific_withdrawn  = withdrawn_credits,
    credit_specific_released   = released_credits,
    credit_specific_potential  = potential_credits,
    credit_jurisdiction        = jurisdiction
  )


# -----------------------------------------------------------------------------
# B: Validation
# -----------------------------------------------------------------------------

# Check if the sum of specific credits matches the bank total in banks_joined
math_check <- credits_cleaned %>%
  group_by(bank_id) %>%
  summarize(
    sum_specific_available = sum(credit_specific_available, na.rm = TRUE)
  ) %>%
  inner_join(banks_joined, by = "bank_id") %>%
  mutate(
    difference = sum_specific_available - available_credits,
    is_match = near(difference, 0)
  )

# Print summary of the math check
cat("\nDifference between Sum of Credits and Bank Total:\n")
summary(math_check$difference)

# Inspect major outliers (diff > 10)
big_mismatches <- math_check %>%
  filter(abs(difference) > 1) %>%
  select(bank_id,
         bank_name,
         sum_specific_available,
         available_credits,
         difference) %>%
  arrange(desc(abs(difference)))

if (nrow(big_mismatches) > 0) {
  cat("\nFound",
      nrow(big_mismatches),
      "banks with significant credit data mismatches.\n")
  print(head(big_mismatches))
}


# Define a threshold for what counts as a "bad" mismatch
# (e.g., more than 1 credit difference)
threshold <- 1

# Create a list of IDs that are suspicious
suspicious_ids <- math_check %>%
  filter(abs(difference) > threshold) %>%
  pull(bank_id)

# Update credits_cleaned dataset to include a warning flag
credits_cleaned <- credits_cleaned %>%
  mutate(
    data_quality_flag = case_when(
      bank_id %in% suspicious_ids ~ "Mismatch between Summary and Details",
      TRUE ~ "Clean"
    )
  )


# ------------------------------------------------------------------------------
# C. Finalize Datasets
# ------------------------------------------------------------------------------

# --- Option A: The Master Credit List (Long Format) ---
# Useful for analyzing specific credit types (e.g., "How many wetland credits?")
# 6,000+ rows (One row per Credit Type) - some banks wont have any info

credits_final <- credits_cleaned %>%
  # Use LEFT JOIN to attach bank details to every credit row
  left_join(banks_joined, by = "bank_id") %>%
  select(
    bank_id,
    bank_name,
    credit_classification,
    credit_specific_available, # Specific to this row
    available_credits,         # Total for the bank
    everything()
  )

cat("\nCREATED 'credits_final': Detailed credit list linked to bank info.\n")
glimpse(credits_final)


# --- Option B: The Bank List with Aggregates (Wide Format) ---
# Useful if we want to keep the 4,868 rows but add credit summary stats.

bank_credit_summaries <- credits_cleaned %>%
  group_by(bank_id) %>%
  summarize(
    total_pot_credits_from_details = sum(credit_specific_potential,
                                         na.rm = TRUE),
    total_pot_credit_types = n()
  )

# Create a version of banks_joined that includes these new summaries
banks_extended <- banks_joined %>%
  left_join(bank_credit_summaries, by = "bank_id")

cat("\nCREATED 'banks_extended': Original bank list + calculated summaries.\n")
glimpse(banks_extended)


# =============================================================================
# FINAL DATA CLEANUP
# =============================================================================

# save banks_extended
saveRDS(banks_extended, here("data",
                             "data_intermediate",
                             "merged_bank_summary.rds"))

# save credits_final
saveRDS(credits_final, here("data",
                            "data_intermediate",
                            "merged_bank_summary_w_cred_class.rds"))
