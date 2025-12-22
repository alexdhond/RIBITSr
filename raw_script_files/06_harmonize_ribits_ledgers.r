# =============================================================================
# Script:     06_harmonize_ribits_ledgers_new.R
# =============================================================================

# =============================================================================
# Description
# =============================================================================

# This script harmonizes multiple RIBITS datasets (ledger, credit tracking,
# credit withdrawal) into one clean final dataframe.
# The goal is to maximize available data while minimizing redundancy.

# Workflow:
# 1. Load and standardize all datasets
# 2. Comprehensive data profiling (columns, rows, content overlap)
# 3. Systematic merging strategy with conflict resolution rules
# 4. Execute harmonization with validation and logging
# 5. Save final harmonized dataset

# Input files:
# - ledger.rds: Main ledger data from unpack_ribits_banks_sites.R
# - Bank and ILF Program Credit Tracking 2025_11_10.csv
# - Bank and ILF Program Credit Withdrawal 2025_11_10_full_export.csv

# Output:
# - harmonized_ribits_ledgers.rds: Final clean dataset


# =============================================================================
# 1. Load Packages
# =============================================================================

library(dplyr)           # Data manipulation
library(tidyr)           # Data tidying
library(janitor)         # Data cleaning
library(data.table)      # Fast data reading
library(here)            # File path handling
library(scales)          # Percentage formatting
library(lubridate)       # Date handling
library(stringr)         # String operations
library(purrr)           # Functional programming


# =============================================================================
# 2. Configure Paths & Load Data
# =============================================================================

# Define paths
raw_data_path <- here("data", "data_raw", "ribits")
intermediate_path <- here("data", "data_intermediate")

# Load all datasets

# Ledger
ledger <- readRDS(file.path(intermediate_path, "ledger.rds"))

# Credit Tracking
credit_tracking <- fread(file.path(raw_data_path,
                                   "Bank and ILF Program Credit Tracking 2025_11_10.csv")) #nolint

# Credit Withdrawal
credit_withdrawal <- fread(file.path(raw_data_path,
                                     "Bank and ILF Program Credit Withdrawal 2025_11_10_full_export.csv")) #nolint


# =============================================================================
# 3. Standardize Column Names and Dates
# =============================================================================

# Clean names across all datasets
datasets <- list(
  ledger = ledger,
  credit_tracking = credit_tracking,
  credit_withdrawal = credit_withdrawal
)

# Apply cleaning across all datasets
datasets <- datasets %>%
  map(~ .x %>% clean_names())

# Fix dates across all datasets - only apply to date-like columns
fix_dates <- function(df) {
  # Find columns that look like dates (contain 'date')
  date_cols <- names(df)[grepl("date", names(df), ignore.case = TRUE)]

  # Try to convert each date column
  for (col in date_cols) {
    if (col %in% names(df)) {
      df[[col]] <- tryCatch({
        # Handle MM/DD/YYYY format specifically
        if (is.character(df[[col]])) {
          # Remove any time components and convert
          clean_dates <- gsub("\\s.*", "", df[[col]])  # Remove time
          lubridate::mdy(clean_dates)  # Parse MM/DD/YYYY format
        } else {
          lubridate::as_date(df[[col]])
        }
      }, error = function(e) {
        # If conversion fails, return original column
        warning("Could not convert column ", col, " to date: ", e$message)
        df[[col]]
      })
    }
  }
  return(df) #nolint
}

# Apply to all datasets
datasets <- datasets %>%
  map(fix_dates)

# Fix specific concatenation issues in tracking and withdrawal
datasets$credit_tracking <- datasets$credit_tracking %>%
  rename(
    purchased_from_bank = purchasedfrom_bank,
    fulfilled_at_site = fulfilledat_site,
    credits_purchased_from_bank = credits_purchasedfrom_bank,
    credits_fulfilled_at_site = credits_fulfilledat_site,
    date_purchased_from_bank = date_purchasedfrom_bank,
    date_fulfilled_at_site = date_fulfilledat_site,
    is_blm_program_or_blm_project_site = is_blm_programor_blm_project_site,
    accepted_in_settlement = acceptedin_settlement
  )

datasets$credit_withdrawal <- datasets$credit_withdrawal %>%
  rename(
    purchased_from_bank = purchasedfrom_bank,
    fulfilled_at_site = fulfilledat_site,
    credits_purchased_from_bank = credits_purchasedfrom_bank,
    credits_fulfilled_at_site = credits_fulfilledat_site,
    date_purchased_from_bank = date_purchasedfrom_bank,
    date_fulfilled_at_site = date_fulfilledat_site,
    is_blm_program_or_blm_project_site = is_blm_programor_blm_project_site,
    accepted_in_settlement = acceptedin_settlement
  )

# Extract back to individual variables
ledger <- datasets$ledger
credit_class <- datasets$credit_class
credit_tracking <- datasets$credit_tracking
credit_withdrawal <- datasets$credit_withdrawal

# =============================================================================
# 4. Data Profiling
# =============================================================================

# Here we just check the basic structure of all the data.

# -----------------------------------------------------------------------------
# Data Profiling Functions
# -----------------------------------------------------------------------------

# Function to calculate column fill rates (i.e. percent of non-NA values) for
# each column in a dataframe. It then reshapes the dataframe, sorts the columns
# by fill rate (descending), and adds a dataset column.
analyze_column_richness <- function(df, name) {

  richness <- df %>%
    # Apply the summarise function to each column
    summarise(across(everything(), ~ sum(!is.na(.x) & .x != "") / n())) %>%
    # Reshape the dataframe to a long format
    pivot_longer(everything(), names_to = "column", values_to = "fill_rate") %>%
    # Sort the dataframe in descending order based on the 'fill_rate' column
    arrange(desc(.data$fill_rate)) %>%
    # Add a dataset column to the dataframe
    mutate(dataset = name)

  return(richness) #nolint
}

# Function to find content matches between datasets. It aligns rows of two
# datasets based on a common key, compares values in each column pair, and
# returns a grid of column matches that exceed a certain percentage match.
find_content_matches <- function(df_master,
                                 df_supp,
                                 id_master,
                                 id_supp,
                                 name_supp) {

  # Align rows via inner join
  joined <- df_master %>%
    rename(join_id = all_of(id_master)) %>%
    inner_join(
      df_supp %>% rename(join_id = all_of(id_supp)),
      by = "join_id",
      suffix = c(".m", ".s")
    )

  # Get columns to compare (exclude IDs)
  cols_master <- setdiff(names(df_master), id_master)
  cols_supp <- setdiff(names(df_supp), id_supp)

  # Create comparison grid
  combo_grid <- expand.grid(
    master_col = cols_master,
    supp_col = cols_supp,
    stringsAsFactors = FALSE
  )

  # Calculate match percentages with column checking
  results <- combo_grid %>%
    rowwise() %>%
    mutate(
      match_pct = {
        master_col_name <- paste0(.data$master_col, ".m")
        supp_col_name <- paste0(.data$supp_col, ".s")

        # Check if columns exist
        if (!master_col_name %in% names(joined) ||
              !supp_col_name %in% names(joined)) {
          0  # Return 0 directly instead of return(0)
        } else {
          master_vals <- joined[[master_col_name]]
          supp_vals <- joined[[supp_col_name]]

          # Handle date columns - convert to Date for comparison
          tryCatch({
            # Try to convert to dates if they look like dates
            if (grepl("date", .data$master_col, ignore.case = TRUE)) {
              master_vals <- as.Date(master_vals)
              supp_vals <- as.Date(supp_vals)
            }

            mean(master_vals == supp_vals, na.rm = TRUE)
          }, error = function(e) {
            # If date conversion fails, compare as character
            mean(as.character(master_vals) ==
                   as.character(supp_vals),
                 na.rm = TRUE)
          })
        }
      }
    ) %>%
    ungroup() %>%
    filter(.data$match_pct > 0.80) %>%
    arrange(desc(.data$match_pct))

  return(results) #nolint
}

# Function to identify redundant columns. It identifies columns in df_supp that
# are identical to corresponding columns in df_master. It returns a character
# vector of the redundant column names.
identify_redundant_columns <- function(df_master,
                                       df_supp,
                                       id_master,
                                       id_supp) {

  # Find common columns (excluding IDs)
  # Get column names that are in both df_master and df_supp, excluding IDs
  common_cols <- setdiff(intersect(names(df_master),
                                   names(df_supp)),
                         c(id_master, id_supp))

  # If there are no common columns, return an empty character vector.
  if (length(common_cols) == 0) return(character(0))

  # Join datasets
  # Join df_master and df_supp based on the common columns and IDs,
  # and select only the join ID and the common columns.
  check_df <- df_master %>%
    rename(join_id = all_of(id_master)) %>%
    select(.data$join_id, all_of(common_cols)) %>%
    inner_join(
      df_supp %>%
        rename(join_id = all_of(id_supp)) %>%
        select(.data$join_id, all_of(common_cols)),
      by = "join_id",
      suffix = c(".m", ".s")
    )

  # Check for identical columns
  # For each common column, compare the values in df_master and df_supp.
  # If all values are identical, add column name to the list of identical_cols.
  identical_cols <- c()

  for (col in common_cols) {
    val_m <- check_df[[paste0(col, ".m")]]
    val_s <- check_df[[paste0(col, ".s")]]

    # Robust comparison
    # Compare the values in df_master and df_supp, treating NA as equal.
    # If both values are NA, they are considered equal.
    # If one value is NA and the other is not, they are considered not equal.
    comp_result <- (val_m == val_s)
    comp_result[is.na(val_m) & is.na(val_s)] <- TRUE
    comp_result[is.na(comp_result)] <- FALSE

    if (all(comp_result)) {
      identical_cols <- c(identical_cols, col)
    }
  }

  # Return the list of redundant column names.
  return(identical_cols) #nolint
}

# -----------------------------------------------------------------------------
# Main Data Profiling
# -----------------------------------------------------------------------------

# Basic dataset information
dataset_info <- tibble(
  dataset = names(datasets),
  rows = map_dbl(datasets, nrow),
  columns = map_dbl(datasets, ncol)
)

print(dataset_info)

# Column richness analysis
column_richness <- map2_dfr(datasets, names(datasets), analyze_column_richness)

# Print the 10 columns with highest fill rate for each dataset
column_richness %>%
  group_by(dataset) %>%
  slice_max(fill_rate, n = 10) %>%
  print()

# Row overlap analysis
# make ledger numeric
ledger <- ledger %>% mutate(transaction_id = as.integer(transaction_id))

# Check overlaps
ledger_ids <- unique(ledger$transaction_id)
tracking_ids <- unique(credit_tracking$bank_transaction_id)
withdrawal_ids <- unique(credit_withdrawal$bank_transaction_id)

cat("Ledger rows in Tracking:",
    scales::percent(mean(ledger_ids %in% tracking_ids)), "\n")
cat("Withdrawal rows in Tracking:",
    scales::percent(mean(withdrawal_ids %in% tracking_ids)), "\n")
cat("Unique Ledger rows (not in Tracking):",
    length(setdiff(ledger_ids, tracking_ids)), "\n")
cat("Unique Withdrawal rows (not in Tracking):",
    length(setdiff(withdrawal_ids, tracking_ids)), "\n")

# Content matching analysis

# Withdrawal vs Tracking
wdr_matches <- find_content_matches(
  credit_tracking, credit_withdrawal,
  "bank_transaction_id", "bank_transaction_id",
  "Withdrawal"
)

# Ledger vs Tracking
ledger_matches <- find_content_matches(
  credit_tracking, ledger,
  "bank_transaction_id", "transaction_id",
  "Ledger"
)

cat("High-content matches in Withdrawal:", nrow(wdr_matches), "\n")
cat("High-content matches in Ledger:", nrow(ledger_matches), "\n")

# 5e. Redundant column identification
cat("\n--- Redundant Column Analysis ---\n")

redundant_wdr_cols <- identify_redundant_columns(
  credit_tracking, credit_withdrawal,
  "bank_transaction_id", "bank_transaction_id"
)

redundant_ledger_cols <- identify_redundant_columns(
  credit_tracking, ledger,
  "bank_transaction_id", "transaction_id"
)

cat("Redundant columns in Withdrawal:", length(redundant_wdr_cols), "\n")
if (length(redundant_wdr_cols) > 0) cat("  ",
                                        paste(redundant_wdr_cols,
                                              collapse = ", "),
                                        "\n")

cat("Redundant columns in Ledger:", length(redundant_ledger_cols), "\n")
if (length(redundant_ledger_cols) > 0) cat("  ",
                                           paste(redundant_ledger_cols,
                                                 collapse = ", "),
                                           "\n")


# =============================================================================
# 5. Internal Deduplication
# =============================================================================

# Manual inspection of the data shows that some rows might have duplicate
# entries, but because of the various columns it is not simple enough to just
# remove duplicates. Below we look through which datasets might have duplicates
# and handle them accordingly.

# -----------------------------------------------------------------------------
# 5a. Define Helper Functions
# -----------------------------------------------------------------------------

# Function to check duplicate rates
report_dupes <- function(df, id_col, name) {
  # Count rows, unique IDs, and duplicates
  n_rows <- nrow(df)
  n_unique <- n_distinct(df[[id_col]])
  n_dupes <- n_rows - n_unique

  # Print results
  cat(sprintf("--- %s ---\n", name))
  cat(sprintf("Total Rows:   %s\n", comma(n_rows)))
  cat(sprintf("Unique IDs:   %s\n", comma(n_unique)))
  cat(sprintf("Duplicates:   %s (%.1f%%)\n\n",
              comma(n_dupes), (n_dupes / n_rows) * 100))
}

# -----------------------------------------------------------------------------
# 5b. Inspect Current State
# -----------------------------------------------------------------------------

# Check which datasets have duplicate IDs
report_dupes(credit_tracking, "bank_transaction_id", "Credit Tracking")
report_dupes(credit_withdrawal, "bank_transaction_id", "Credit Withdrawal")
# Note: Ledger uses 'transaction_id'
report_dupes(ledger, "transaction_id", "Ledger")

# Only credit_tracking has duplicate rows (same transaction ID)

# Let's understand exactly what we have
transaction_analysis <- credit_tracking %>%

  # Group by bank transaction id
  group_by(bank_transaction_id) %>%

  # Count how many rows have bank and program
  summarise(
    has_bank = any(row_type == "Bank", na.rm = TRUE),
    has_program = any(row_type == "Program", na.rm = TRUE),
    n_rows = n(),
    .groups = "drop"
  ) %>%

  # Create a transaction type
  mutate(
    transaction_type = case_when(
      has_bank & has_program ~ "Bank + Program",
      has_bank & !has_program ~ "Bank Only",
      !has_bank & has_program ~ "Program Only",
      TRUE ~ "Unknown"
    )
  )

cat("\n--- Transaction Type Breakdown ---\n")
print(transaction_analysis %>%
        group_by(transaction_type) %>%
        summarise(count = n(), total_rows = sum(n_rows), .groups = "drop"))

# -----------------------------------------------------------------------------
# 5c. Handle Each Type Separately
# -----------------------------------------------------------------------------

# Bank + Program transactions (need merging)
bank_program_ids <- transaction_analysis %>%
  filter(transaction_type == "Bank + Program") %>%
  pull(bank_transaction_id)

cat("Found", length(bank_program_ids), "Bank + Program transactions to merge\n")

# Bank Only transactions (keep as-is)
bank_only_ids <- transaction_analysis %>%
  filter(transaction_type == "Bank Only") %>%
  pull(bank_transaction_id)

cat("Found", length(bank_only_ids), "Bank Only transactions to keep\n")

# Program Only transactions (keep as-is, but handle missing bank_transaction_id)
program_only <- credit_tracking %>%
  filter(row_type == "Program", is.na(bank_transaction_id))

cat("Found", nrow(program_only), "Program Only transactions to keep\n")


# -----------------------------------------------------------------------------
# 5d. Merge Bank + Program Transactions
# -----------------------------------------------------------------------------

if (length(bank_program_ids) > 0) {
  cat("Merging Bank + Program transactions...\n")

  # A. Get Primary Rows (Bank)
  bank_rows <- credit_tracking %>%
    filter(bank_transaction_id %in% bank_program_ids, row_type == "Bank") %>%
    select(-row_type)

  # B. Get Rescue Rows (Program)
  # We explicitly select columns known to contain distinct regulatory info
  program_rows <- credit_tracking %>%
    filter(bank_transaction_id %in% bank_program_ids, row_type == "Program") %>%
    select(
      bank_transaction_id,
      program_transaction_id,
      program_id,
      manager_list,
      jurisdiction,
      permittee
      # Add any other specific columns you identified in your analysis
    ) %>%
    # Safety: Deduplicate program rows per ID to prevent row explosion
    group_by(bank_transaction_id) %>%
    slice(1) %>%
    ungroup()

  # C. Execute Merge
  bank_program_merged <- bank_rows %>%
    left_join(program_rows,
              by = "bank_transaction_id",
              suffix = c("", "_program")) %>%
    mutate(
      # Coalesce Logic: Prioritize Bank, fill gaps with Program
      program_transaction_id = coalesce(
        program_transaction_id,
        program_transaction_id_program
      ),
      program_id = coalesce(
        program_id,
        program_id_program
      ),
      manager_list = coalesce(
        manager_list,
        manager_list_program
      ),
      jurisdiction = coalesce(
        jurisdiction,
        jurisdiction_program
      ),
      permittee = coalesce(permittee, permittee_program)
    ) %>%
    # Drop the temp columns
    select(-ends_with("_program"))

} else {
  bank_program_merged <- tibble()
}
# -----------------------------------------------------------------------------
# 5e. Process Bank Only Transactions (Keep as-is)
# -----------------------------------------------------------------------------

if (length(bank_only_ids) > 0) {
  cat("Keeping Bank Only transactions...\n")
  bank_only <- credit_tracking %>%
    filter(bank_transaction_id %in% bank_only_ids) %>%
    select(-row_type)
} else {
  bank_only <- tibble()
}

# -----------------------------------------------------------------------------
# 5f. Process Program Only Transactions (Handle missing IDs)
# -----------------------------------------------------------------------------

if (nrow(program_only) > 0) {
  cat("Processing Program Only transactions...\n")
  # Create synthetic bank_transaction_id to avoid conflicts
  program_only_clean <- program_only %>%
    mutate(
      bank_transaction_id = -1 * as.numeric(program_transaction_id),
      row_type = "Program Only"
    ) %>%
    select(-row_type)
} else {
  program_only_clean <- tibble()
}

# -----------------------------------------------------------------------------
# 5g. Combine Everything
# -----------------------------------------------------------------------------

# Combine all processed transactions
credit_tracking_clean <- bind_rows(bank_program_merged,
                                   bank_only,
                                   program_only_clean)

cat("\n--- Final Results ---\n")
cat("Original rows:", nrow(credit_tracking), "\n")
cat("Cleaned rows:", nrow(credit_tracking_clean), "\n")
cat("Unique bank_transaction_id:",
    n_distinct(credit_tracking_clean$bank_transaction_id), "\n")

# Overwrite the main variable
credit_tracking <- credit_tracking_clean

# -----------------------------------------------------------------------------
# 5h. Clean Other Datasets (Simple - no duplicates)
# -----------------------------------------------------------------------------

cat("Cleaning other datasets...\n")
credit_withdrawal <- credit_withdrawal %>%
  distinct(bank_transaction_id, .keep_all = TRUE)

ledger <- ledger %>%
  distinct(transaction_id, .keep_all = TRUE)

# -----------------------------------------------------------------------------
# 5i. Validation
# -----------------------------------------------------------------------------

# validate
report_dupes(credit_tracking,
             "bank_transaction_id",
             "Credit Tracking (Clean)")
report_dupes(credit_withdrawal,
             "bank_transaction_id",
             "Credit Withdrawal (Clean)")
report_dupes(ledger,
             "transaction_id",
             "Ledger (Clean)")

# =============================================================================
# 6. Harmonization Strategy
# =============================================================================

# Even though credit_tracking has most rows, it may have "holes" (NAs) or lack
# specific columns found in the ledger or credit_withdrawal

# To maximize data retention, the strategy is as follows:
# 1. PATCHING: If credit_tracking has NA, fill it with data from
#    ledger/credit_withdrawal. #nolint
# 2. EXPANSION: If ledger/credit_withdrawal has a unique column, add it to
#    credit_tracking. #nolint
# 3. CONFLICTS: If both have data for the same column, keep credit_tracking,
#    but save ledger/credit_withdrawal data in a renamed column (e.g., _ledger)
#    for reference. #nolint

# 6a. Define Merge Priority
# We trust Credit Tracking the most. (Master)
merge_priority <- list(
  master = "credit_tracking",
  supplements = c("credit_withdrawal", "ledger")
)

# 6b. Define conflict resolution rules
conflict_rules <- list(

  # Rule 1: Coalesce Targets
  # These are columns we KNOW exist in multiple files and want to merge.
  # We trust Master first, then Supplements.
  coalesce_cols = c("transaction_year",
                    "transaction_date",
                    "permittee",
                    "jurisdiction"),

  # Rule 2: Rename Conflicts
  # These are columns we want to PRESERVE the difference rather than merge.
  # NOTE: We do NOT list 'watershed_basin' here because it is unique to
  # withdrawal. We want it to come over as 'watershed_basin', not '_wdr'.
  rename_conflicts = list(
    # Ledger conflicts: 'comment' is generic, so we rename to avoid overwriting
    ledger = c("comment", "credit_classification", "date_transferred"),

    # Withdrawal conflicts: Only rename if they overlap with master
    # (leaving this empty means unique columns stay unique)
    withdrawal = c()
  )
)

# =============================================================================
# 7. Prepare Datasets for Merging
# =============================================================================

# Helper to ensure consistent ID types (Crucial for joins)
clean_id <- function(x) as.numeric(x)

# -----------------------------------------------------------------------------
# 7a. Define the "Safe List" (Columns we MUST keep for patching/auditing)
# -----------------------------------------------------------------------------

# We look at the conflict rules to see what we planned to use.
keep_and_rename_cols <- unique(c(
  conflict_rules$coalesce_cols, # e.g. transaction_date, permittee
  conflict_rules$rename_conflicts$ledger,  # e.g. comment
  conflict_rules$rename_conflicts$withdrawal
))

# -----------------------------------------------------------------------------
# 7b. Prepare Withdrawal
# -----------------------------------------------------------------------------

# Overlaps: Columns in both datasets
wdr_overlap <- intersect(names(credit_withdrawal), names(credit_tracking))
wdr_overlap <- setdiff(wdr_overlap, "bank_transaction_id")

# Redundant: Overlaps that are NOT in our "Safe List" (Safe to drop)
wdr_drop <- setdiff(wdr_overlap, keep_and_rename_cols)

# Targets: Overlaps that ARE in our "Safe List" (Must rename to survive)
wdr_rename <- intersect(wdr_overlap, keep_and_rename_cols)

wdr_rescue <- credit_withdrawal %>%
  mutate(bank_transaction_id = clean_id(bank_transaction_id)) %>%

  # 1. Drop truly redundant columns (Identical info we don't need)
  select(-any_of(wdr_drop)) %>%

  # 2. Rename the columns we want to use for patching so they don't collide
  #    (e.g., permittee -> permittee_wdr)
  {if (length(wdr_rename) > 0) {
    rename_with(.,
                .fn = ~ paste0(., "_wdr"),
                .cols = any_of(wdr_rename))
  } else {
    .
  }
  } %>%

  # 3. Rename the explicit conflicts (if they haven't been renamed yet)
  {if (length(conflict_rules$rename_conflicts$withdrawal) > 0) {
    rename_with(.,
                .fn = ~ paste0(., "_wdr"),
                .cols = any_of(conflict_rules$rename_conflicts$withdrawal))
  } else {
    .
  }
  } %>%

  # 4. Deduplicate
  distinct(bank_transaction_id, .keep_all = TRUE)

# -----------------------------------------------------------------------------
# 7c. Prepare Ledger
# -----------------------------------------------------------------------------

# Overlaps: Columns in both datasets
ledger_overlap <- intersect(names(ledger), names(credit_tracking))
ledger_overlap <- setdiff(ledger_overlap,
                          c("bank_transaction_id", "transaction_id"))

ledger_drop   <- setdiff(ledger_overlap, keep_and_rename_cols)
ledger_rename <- intersect(ledger_overlap, keep_and_rename_cols)

ledger_rescue <- ledger %>%
  mutate(bank_transaction_id = clean_id(transaction_id)) %>%

  # 1. Drop truly redundant columns
  select(-any_of(ledger_drop), -transaction_id) %>%

  # 2. Rename columns needed for patching
  {if (length(ledger_rename) > 0) {
    rename_with(.,
                .fn = ~ paste0(., "_ledger"),
                .cols = any_of(ledger_rename))
  } else {
    .
  }
  } %>%

  # 3. Rename explicit conflicts
  {if (length(conflict_rules$rename_conflicts$ledger) > 0) {
    rename_with(.,
                .fn = ~ paste0(., "_ledger"),
                .cols = any_of(conflict_rules$rename_conflicts$ledger))
  } else {
    .
  }
  } %>%

  # 4. Deduplicate
  distinct(bank_transaction_id, .keep_all = TRUE)

# Validation of Prep
cat("Withdrawal Prep: Dropped", length(wdr_drop),
    "redundant cols. Renamed", length(wdr_rename), "patching cols.\n")
cat("Ledger Prep:     Dropped", length(ledger_drop),
    "redundant cols. Renamed", length(ledger_rename), "patching cols.\n")


# =============================================================================
# 8. Execute Harmonization
# =============================================================================

# 1. The Merge. We use full_join to ensure we capture all rows
harmonized_data <- credit_tracking %>%
  full_join(wdr_rescue, by = "bank_transaction_id") %>%
  full_join(ledger_rescue, by = "bank_transaction_id")

# 2. The Patching (Conflict Resolution)
# We use 'if' checks to see if the rescue columns exist before trying to use
# them. This prevents "Column not found" errors.

# Priority: Master then Withdrawal then Ledger
cols_year <- names(harmonized_data)
if ("transaction_year_wdr" %in% cols_year ||
      "transaction_year_ledger" %in% cols_year) {
  harmonized_data <- harmonized_data %>%
    mutate(transaction_year = coalesce(
      transaction_year,
      if ("transaction_year_wdr" %in% cols_year)
        transaction_year_wdr else NA,
      if ("transaction_year_ledger" %in% cols_year)
        transaction_year_ledger else NA
    ))
}

# Priority: Master then Ledger then Withdrawal
cols_date <- names(harmonized_data)
# We use 'date_transferred_ledger' because we renamed it in the previous step
if ("date_transferred_ledger" %in% cols_date ||
      "transaction_date_wdr" %in% cols_date) {
  harmonized_data <- harmonized_data %>%
    mutate(transaction_date = coalesce(
      transaction_date,
      if ("date_transferred_ledger" %in% cols_date)
        date_transferred_ledger else NA,
      if ("transaction_date_wdr" %in% cols_date)
        transaction_date_wdr else NA
    ))
}

# Priority: Master then Ledger then Withdrawal
if ("permittee_ledger" %in% names(harmonized_data) ||
      "permittee_wdr" %in% names(harmonized_data)) {
  harmonized_data <- harmonized_data %>%
    mutate(permittee = coalesce(
      permittee,
      if ("permittee_ledger" %in% names(harmonized_data))
        permittee_ledger else NA,
      if ("permittee_wdr" %in% names(harmonized_data)) permittee_wdr else NA
    ))
}

# Priority: Master then Ledger then Withdrawal
if ("jurisdiction_ledger" %in% names(harmonized_data) ||
      "jurisdiction_wdr" %in% names(harmonized_data)) {
  harmonized_data <- harmonized_data %>%
    mutate(jurisdiction = coalesce(
      jurisdiction,
      if ("jurisdiction_ledger" %in% names(harmonized_data))
        jurisdiction_ledger else NA,
      if ("jurisdiction_wdr" %in% names(harmonized_data))
        jurisdiction_wdr else NA
    ))
}

# =============================================================================
# 9. Final Cleanup and Validation
# =============================================================================

cat("Performing final cleanup and validation...\n")

# 1. Remove temporary columns used for conflict resolution
#    (We check existence first to avoid errors)
temp_cols_to_remove <- c("date_transferred_ledger",
                         "permittee_ledger",
                         "jurisdiction_ledger")
existing_temp_cols <- intersect(temp_cols_to_remove, names(harmonized_data))

if (length(existing_temp_cols) > 0) {
  harmonized_data <- harmonized_data %>% select(-any_of(existing_temp_cols))
}

# -----------------------------------------------------------------------------
# 9a. Basic Counts & Completeness
# -----------------------------------------------------------------------------

# Helper to safely count non-NA IDs
count_ids <- function(x) sum(!is.na(x))

final_validation <- tibble(
  metric = c(
    "Original Tracking Rows",
    "Original Withdrawal Rows",
    "Original Ledger Rows",
    "Final Harmonized Rows",
    "Final Unique IDs",
    "Final Completeness (Date)"
  ),
  value = c(
    nrow(credit_tracking),
    nrow(credit_withdrawal),
    nrow(ledger),
    nrow(harmonized_data),
    n_distinct(harmonized_data$bank_transaction_id, na.rm = TRUE),
    scales::percent(mean(!is.na(harmonized_data$transaction_date)))
  )
)

cat("\n--- Final Validation Metrics ---\n")
print(final_validation)


# -----------------------------------------------------------------------------
# 9b. Comprehensive Join Validation
# -----------------------------------------------------------------------------

cat("\nPerforming comprehensive join validation...\n")

# 1. ID Preservation (Did we lose anyone?)
#    We filter out NAs before checking to ensure accurate percentages
safe_track_ids <- na.omit(credit_tracking$bank_transaction_id)
safe_wdr_ids   <- na.omit(credit_withdrawal$bank_transaction_id)
safe_led_ids   <- na.omit(ledger$transaction_id) # Note: uses 'transaction_id'

track_kept <- sum(safe_track_ids %in% harmonized_data$bank_transaction_id)
wdr_kept   <- sum(safe_wdr_ids %in% harmonized_data$bank_transaction_id)
led_kept   <- sum(safe_led_ids %in% harmonized_data$bank_transaction_id)

cat("\n--- ID Preservation Check ---\n")
cat("Tracking IDs preserved:  ",
    scales::percent(track_kept / length(safe_track_ids)), "\n")
cat("Withdrawal IDs preserved:",
    scales::percent(wdr_kept / length(safe_wdr_ids)), "\n")
cat("Ledger IDs preserved:    ",
    scales::percent(led_kept / length(safe_led_ids)), "\n")

# 2. Unique Contributions (Who brought new rows?)
#    (Rows in X that are NOT in the other two)
track_unique <- sum(!safe_track_ids %in% safe_wdr_ids &
                      !safe_track_ids %in% safe_led_ids)
wdr_unique   <- sum(!safe_wdr_ids %in% safe_track_ids &
                      !safe_wdr_ids %in% safe_led_ids)
led_unique   <- sum(!safe_led_ids %in% safe_track_ids &
                      !safe_led_ids %in% safe_wdr_ids)

cat("\n--- Unique Row Contributions ---\n")
cat("Rows unique to Tracking:  ", comma(track_unique), "\n")
cat("Rows unique to Withdrawal:", comma(wdr_unique), "\n")
cat("Rows unique to Ledger:    ", comma(led_unique), "\n")


# -----------------------------------------------------------------------------
# 9c. Duplicate Investigation
# -----------------------------------------------------------------------------

duplicate_ids <- harmonized_data %>%
  filter(!is.na(bank_transaction_id)) %>%
  count(bank_transaction_id) %>%
  filter(n > 1)

cat("\n--- Duplicate Transaction ID Investigation ---\n")
cat("Number of duplicate IDs:", nrow(duplicate_ids), "\n")

if (nrow(duplicate_ids) > 0) {
  cat("[WARNING] Duplicates found. Deep diving...\n")

  # Grab the first duplicate ID to analyze
  sample_id <- duplicate_ids$bank_transaction_id[1]
  cat("\nDeep Dive on ID:", sample_id, "\n")

  # Show the conflicting rows in the final dataset
  dup_rows <- harmonized_data %>% filter(bank_transaction_id == sample_id)
  print(dup_rows)

  # Check where this ID existed originally
  cat("Exists in Tracking?  ", sample_id %in% safe_track_ids, "\n")
  cat("Exists in Withdrawal?", sample_id %in% safe_wdr_ids, "\n")
  cat("Exists in Ledger?    ", sample_id %in% safe_led_ids, "\n")

} else {
  cat("[SUCCESS] No duplicates found in final dataset.\n")
}


# -----------------------------------------------------------------------------
# 9d. Final Integrity Logic Check
# -----------------------------------------------------------------------------

integrity_check <- tibble(
  check = c(
    "No duplicate transaction IDs",
    "All Master IDs preserved",
    "No Join Suffixes (.x/.y)",
    "Row Count Logic Holds"
  ),
  status = c(
    nrow(duplicate_ids) == 0,
    track_kept == length(safe_track_ids),
    !any(grepl("\\.x$|\\.y$", names(harmonized_data))),
    nrow(harmonized_data) >= nrow(credit_tracking)
  ),
  result = c("PASS", "PASS", "PASS", "PASS")
)

# Flag failures
integrity_check$result[!integrity_check$status] <- "FAIL"
integrity_check$status <- NULL

cat("\n--- Final System Integrity Check ---\n")
print(integrity_check)

# =============================================================================
# 10. Save Output
# =============================================================================

if (all(integrity_check$result == "PASS")) {
  cat("\nAll checks passed. Saving harmonized dataset...\n")
  saveRDS(harmonized_data, here("data",
                                "data_intermediate",
                                "harmonized_ribits_ledgers.rds"))
} else {
  cat("\n[WARNING] Integrity checks FAILED. Review errors above.\n")
}