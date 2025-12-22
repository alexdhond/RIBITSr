# =============================================================================
# Script:     parse_potential_credits.R
# =============================================================================

# =============================================================================
# Description
# =============================================================================

# This script parses the RIBITS Potential Credits by Mitigation Type file:
# ("Potential Credits by Mitigation Type 2025_11_19.csv").

# Annoyingly, when downloaded, the file is not a clean CSV. It is a
# human-readable file with nested headers, subtotals, and other non-data rows.

# The strategy is to perform a two-pass parse:
# 1. First Pass: Read all lines as text, identify the hierarchical context
#    (District, Resource, Method) for each line, and tag junk rows.
# 2. Second Pass: Filter out the junk, leaving only clean data lines. Then,
#    use readr::read_csv for robust parsing.

# ==============================================================================
# 1. Load Packages
# ==============================================================================

library(tidyverse)    # For data manipulation and parsing
library(readr)        # Reading files
library(here)         # For file path management


# ==============================================================================
# 2. Load Raw Data
# ==============================================================================

# Load the file path
raw_file_path <- here("data",
                      "data_raw",
                      "ribits",
                      "Potential Credits by Mitigation Type 2025_11_19.csv")

# read in the data as raw lines (from the CSV file)
raw_lines     <- read_lines(raw_file_path)

# ==============================================================================
# Set up variables
# ==============================================================================

# Individual headers (these are the types of credits)
resources <- c("Wetland",
               "Stream",
               "Species",
               "NRDA")

# Methods of mitigation
methods   <- c("Establishment",
               "Re-establishment",
               "Rehabilitation",
               "Preservation",
               "Enhancement",
               "Uplands (Buffer)",
               "-Unspecified-")

# ==============================================================================
# PASS 1: MAPPING THE CONTEXT
# ==============================================================================

# Below, we have to map out what the "true" dataframe might look like, based on
# what the raw data looks like.

# Create a dataframe with the following columns:
hierarchy_map <- tibble(raw_line = raw_lines) %>% 
  mutate(
    # Clean the lines
    clean_line = str_remove_all(raw_line, '"') %>% str_trim(),

    # District detection
    is_district = str_detect(clean_line, "^USACE District"),

    # Resource detection
    is_resource = clean_line %in% resources,

    # Robust method detection (matches start of line)
    is_method   = str_detect(clean_line,
                             paste0("^(", paste(methods, collapse="|"), ")")),

    # Extract Context
    district_val = if_else(is_district, clean_line, NA_character_),
    resource_val = if_else(is_resource, clean_line, NA_character_),
    method_val   = if_else(is_method, clean_line, NA_character_)
  ) %>%

  # Fill Down
  fill(district_val, .direction = "down") %>%
  fill(resource_val, .direction = "down") %>%

  # Reset Method when Resource changes
  mutate(method_val = if_else(is_resource, NA_character_, method_val)) %>%
  fill(method_val, .direction = "down") %>%

  # ROBUST FILTERING
  filter(
    # 1. Remove Lines with NO Commas (These are Headers/Context)
    str_count(raw_line, ",") > 0,

    # 2. Remove Subtotal lines (Start with empty field)
    !str_detect(raw_line, '^"","'),

    # 3. Remove Total lines
    !str_detect(clean_line, "^Total"),

    # 4. Remove the specific Column Header row
    !str_detect(clean_line, "^Bank,Credit Classification")
  )

# ==============================================================================
# PASS 2: PARSING
# ==============================================================================

# We read it as a CSV to parse it easier
parsed_data <- read_csv(
  I(paste(hierarchy_map$raw_line, collapse = "\n")),
  col_names = c("bank_name",
                "credit_classification",
                "potential_credits",
                "init_acres",
                "init_feet",
                "junk"),
  col_types = cols(
    bank_name = col_character(),
    credit_classification = col_character(),
    # Use col_number to strip commas from "1,200"
    potential_credits = col_number(),
    init_acres = col_number(),
    init_feet = col_number(),
    junk = col_character()
  ),

  # Trim whitespace
  trim_ws = TRUE
)

# ==============================================================================
# ASSEMBLY
# ==============================================================================

final_credits_hierarchy <- bind_cols(
  hierarchy_map %>% select(district_raw = district_val,
                           resource_type = resource_val,
                           mitigation_method = method_val),
  parsed_data
) %>%
  select(-junk) %>%
  # Parse District String
  extract(
    col = district_raw,
    into = c("district_name", "reported_bank_count", "reported_status"),
    regex = "^USACE District (.+) Reporting (\\d+) (.+)$",
    convert = TRUE
  ) %>%
  mutate(reported_status = str_remove(reported_status, " Banks$"))

# Final Check
cat("Rows:", nrow(final_credits_hierarchy), "\n")
# Check for parsing errors in the numbers
n_fails <- sum(is.na(final_credits_hierarchy$potential_credits))
if (n_fails > 0) warning(paste(n_fails,
                               "rows failed to parse credits.")) else
  cat("Parsing Successful!\n")

glimpse(final_credits_hierarchy)

# =============================================================================
# Save the data
# =============================================================================

saveRDS(final_credits_hierarchy,
        here("data",
             "data_intermediate",
             "pot_creds_approved_banks.rds"))
