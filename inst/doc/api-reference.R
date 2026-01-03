## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----rb-get-------------------------------------------------------------------
# rb_get(
#   type = NULL,           # Data type: "banks", "ilf", "umbrellas", "wqt"
#   id = NULL,             # Specific ID(s) for detailed data
#   state = NULL,          # State filter: "CA", "OR", "TX"
#   district = NULL,       # USACE district: "Portland", "Sacramento"
#   field_office = NULL,   # FWS field office
#   noaa_region = NULL,    # NOAA region
#   kind = NULL,           # Bank type: "Standard", "ILF", "Umbrella", "NRDA"
#  status = NULL,         # Status: "Approved", "Pending", "Terminated"
#   ledger = FALSE,        # Include transaction history
#   footprint = FALSE,     # Include footprint geometry
#   service_area = FALSE,  # Include service area geometry
#   contacts = FALSE       # Include contact information
# )
# 
# # Examples
# rb_get()                                    # Show help
# rb_get("banks", state = "OR")               # List Oregon banks
# rb_get("banks", id = 17, ledger = TRUE)     # Get bank with transactions
# rb_get("ilf", district = "Portland")        # List Portland ILF programs

## ----rb-epa-------------------------------------------------------------------
# rb_epa(
#   layer = NULL,          # Layer: "banks", "footprints", "service_areas", etc.
#   state = NULL,          # State filter
#   district = NULL,       # USACE district filter
#   bank_ids = NULL,       # Vector of bank IDs
#   program_ids = NULL,    # Vector of program IDs (for ILF layers)
#   status = NULL,         # Status filter
#   kind = NULL,           # Bank type filter
#   where = NULL           # Custom SQL WHERE clause
# )
# 
# # Examples
# rb_epa()                                     # Show available layers
# rb_epa("banks", state = "OR")                # Oregon banks
# rb_epa("footprints", bank_ids = c(17, 100))  # Specific footprints
# rb_epa("ilf_service_areas")                  # All ILF service areas

## ----rb-download--------------------------------------------------------------
# rb_download_report(
#   report_type,           # Report type (see rb_reports())
#   download_dir = "data/ribits_reports",
#   filename = NULL,       # Auto-generated if NULL
#   reset_filters = TRUE   # Reset report filters to get all data
# )
# 
# # See available reports
# rb_reports()
# 
# # Examples
# rb_download_report("banks_sites", download_dir = "data/")
# rb_download_report("credit_classification", download_dir = "data/")
# rb_download_report("ledger_transactions", download_dir = "data/")

## ----rb-read------------------------------------------------------------------
# rb_read(
#   path,                  # Path to CSV file
#   type = NULL            # Optional: force specific parser (usually auto-detected)
# )
# 
# # Examples
# data <- rb_read("data/banks_sites.csv")
# credits <- rb_read("data/potential_credits.csv")  # Auto-detects special parser

## ----rb-near------------------------------------------------------------------
# rb_near(
#   lat,                   # Latitude (decimal degrees)
#   lon,                   # Longitude (decimal degrees)
#   type = "banks"         # "banks" or "programs"
# )
# 
# # Example
# nearby <- rb_near(lat = 45.5152, lon = -122.6784)

## ----rb-extract---------------------------------------------------------------
# rb_extract(
#   data,                  # Bank/program object from rb_get()
#   what                   # What to extract (see below)
# )
# 
# # Available extractions:
# # - "ledger" - Transaction history
# # - "contacts" - All contacts
# # - "sponsors" - Bank sponsors
# # - "pocs" - Points of contact
# # - "managers" - Bank managers
# # - "footprint" - Footprint geometry
# # - "service_area" - Service area geometry
# 
# # Example
# bank <- rb_get("banks", id = 17, ledger = TRUE)
# ledger <- rb_extract(bank, "ledger")

## ----rb-bulk------------------------------------------------------------------
# rb_bulk_ledger(
#   bank_ids,              # Vector of bank IDs
#   progress = TRUE        # Show progress bar
# )
# 
# # Example
# ledgers <- rb_bulk_ledger(bank_ids = 1:100)

