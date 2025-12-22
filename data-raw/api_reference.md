# RIBITSr API Reference

Complete reference for all RIBITS API endpoints, fields, and package functions.

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Unified API Functions](#unified-api-functions)
3. [RIBITS API Endpoints](#ribits-api-endpoints)
4. [EPA ArcGIS Layers](#epa-arcgis-layers)
5. [Available Filters](#available-filters)
6. [Data Fields Reference](#data-fields-reference)
7. [Manual Download Reports](#manual-download-reports)
8. [Complete Workflow](#complete-workflow)

---

## Quick Start

```r
library(RIBITSr)

# Get Oregon banks
banks <- rb_get("banks", state = "OR")

# Get specific bank with full data
bank <- rb_get("banks", id = 17, ledger = TRUE, footprint = TRUE)

# Query EPA spatial data
footprints <- rb_epa("footprints", state = "CA")

# Download manual report directly
file <- rb_download_report("potential_credits")

# Read downloaded CSV
data <- rb_read(file)
```

---

## Unified API Functions

### `rb_get()` - Main Data Retrieval

Retrieve banks, ILF programs, umbrellas, or WQT projects with filtering.

```r
rb_get(
  type = NULL,           # "banks", "ilf", "umbrellas", "wqt"
  id = NULL,             # Specific ID(s) for detailed data
  state = NULL,          # State filter: "CA", "OR", etc.
  district = NULL,       # USACE district: "Portland", "Sacramento"
  field_office = NULL,   # FWS field office
  noaa_region = NULL,    # NOAA region
  kind = NULL,           # Bank type: "Standard", "ILF", "Umbrella", "NRDA"
  status = NULL,         # Status: "Approved", "Pending", "Terminated"
  ledger = FALSE,        # Include transaction history
  footprint = FALSE,     # Include footprint geometry
  service_area = FALSE,  # Include service area geometry
  contacts = FALSE       # Include contact information
)
```

**Examples:**
```r
rb_get()                                    # Show help
rb_get("banks", state = "OR")               # Oregon banks
rb_get("banks", id = 17, ledger = TRUE)     # Bank with transactions
rb_get("ilf", district = "Portland")        # Portland ILF programs
rb_get("banks", kind = "Standard", status = "Approved")
```

### `rb_epa()` - EPA ArcGIS Spatial Data

Query EPA ArcGIS MapServer for spatial data.

```r
rb_epa(
  layer = NULL,          # Layer name (see below)
  state = NULL,          # State filter
  district = NULL,       # USACE district
  bank_ids = NULL,       # Vector of bank IDs
  program_ids = NULL,    # Vector of program IDs
  status = NULL,         # Status filter
  kind = NULL,           # Bank type filter
  where = NULL           # Custom SQL WHERE clause
)
```

**Available Layers:**
| Layer | Description |
|-------|-------------|
| `banks` | Bank point locations (approved) |
| `footprints` | Bank footprint polygons |
| `service_areas` | Bank service area polygons |
| `ilf_programs` | ILF program point locations |
| `ilf_service_areas` | ILF program service areas |
| `districts` | USACE district boundaries |

### `rb_download_report()` - Download Reports via Browser

```r
rb_download_report(
  report = NULL,         # Report type (see below)
  download_dir = "data/ribits_manual"
)
```

**Available Reports:**
| Report | Description |
|--------|-------------|
| `credit_classification` | Credit Classification by Jurisdiction |
| `potential_credits` | Potential Credits by Mitigation Type |
| `credit_tracking` | Credit Tracking (Ledgers) |
| `credit_withdrawal` | Credit Withdrawal details |
| `bank_summary` | Bank Summary report |
| `service_area_comments` | Service Area Comments |

### `rb_read()` - Read Downloaded CSVs

```r
rb_read(
  type = NULL,           # Report type
  file = NULL,           # Path to file (or auto-detect)
  download_dir = "data/ribits_manual"
)
```

### `rb_near()` - Location-Based Search

```r
rb_near(
  lat,                   # Latitude (decimal degrees)
  lon,                   # Longitude (decimal degrees)
  type = "banks"         # "banks" or "programs"
)
```

---

## RIBITS API Endpoints

Base URL: `https://ribits.ops.usace.army.mil/ords/RI/public/`

### List Endpoints

| Endpoint | Function | Description |
|----------|----------|-------------|
| `bank_site_list/` | `rb_list_banks()` | List all banks |
| `ilf_program_list/` | `rb_list_ilf_programs()` | List all ILF programs |
| `umbrella_instrument_list/` | `rb_list_umbrellas()` | List all umbrellas |
| `wqt_project_list/` | `rb_list_wqt_projects()` | List WQT projects |

### Data Endpoints

| Endpoint | Function | Description |
|----------|----------|-------------|
| `bank_site_data/` | `rb_get_bank()` | Get bank details |
| `ilf_program_data/` | `rb_get_ilf_program()` | Get ILF program details |
| `umbrella_instrument_data/` | `rb_get_umbrella()` | Get umbrella details |

### Location Endpoints

| Endpoint | Function | Description |
|----------|----------|-------------|
| `getmitbanksbylocation_47/` | `rb_banks_by_location()` | Find banks near lat/lon |
| `getprogramsbylocation_47/` | `rb_programs_by_location()` | Find programs near lat/lon |

### Other Endpoints

| Endpoint | Function | Description |
|----------|----------|-------------|
| `find_credits_excel/{id}` | `rb_find_credits_excel()` | Download credits as Excel |

---

## EPA ArcGIS Layers

Base URL: `https://geopub.epa.gov/arcgis/rest/services/NEPAssist/RIBITS/MapServer`

| Layer ID | Name | Function |
|----------|------|----------|
| 0 | USACE RIBITS Banks | `rb_epa("banks")` |
| 1 | Bank Status | - |
| 2 | Approved Banks | `rb_epa("banks")` |
| 3 | Pending Banks | - |
| 4 | Terminated Banks | - |
| 5 | Bank Footprint | `rb_epa("footprints")` |
| 6 | Bank Service Area | `rb_epa("service_areas")` |
| 7 | USACE RIBITS ILFs | `rb_epa("ilf_programs")` |
| 8 | ILF Program Service Areas | `rb_epa("ilf_service_areas")` |
| 9 | ILF Approved | - |
| 10 | ILF Pending | - |
| 11 | ILF Terminated | - |
| 12 | USACE Districts | `rb_epa("districts")` |

---

## Available Filters

### RIBITS API Filters

| Filter | Banks | ILF | Umbrellas | WQT |
|--------|-------|-----|-----------|-----|
| `state` | ✓ | ✓ | ✓ | ✓ |
| `district` | ✓ | ✓ | ✓ | ✓ |
| `field_office` | ✓ | ✓ | ✓ | - |
| `noaa_region` | ✓ | ✓ | ✓ | - |
| `kind_of_bank` | ✓ | - | - | - |

### Data Options (for detailed queries)

| Option | Banks | ILF | Umbrellas | WQT |
|--------|-------|-----|-----------|-----|
| `ledger` | ✓ | ✓ | ✓ | - |
| `footprint` | ✓ | - | - | - |
| `service_area` | ✓ | ✓ | ✓ | - |
| `contacts` | ✓ | ✓ | ✓ | - |

---

## Data Fields Reference

### Bank List Fields (`rb_list_banks()`)

| Field | Description |
|-------|-------------|
| `bank_id` | Unique bank identifier |
| `bank_name` | Bank name |
| `bank_status_name` | Status (Approved, Pending, etc.) |
| `kind_of_bank_name` | Type (Standard, ILF, Umbrella, NRDA) |
| `state_abbrev_list` | States where bank operates |
| `district` | USACE district |
| `field_office` | FWS field office |
| `total_potential_credits` | Total potential credits |
| `total_available_credits` | Available credits |
| `longitude`, `latitude` | Bank location |

### Bank Detail Fields (`rb_get_bank()`)

| Field | Description |
|-------|-------------|
| `bank_id` | Unique identifier |
| `bank_name` | Full bank name |
| `bank_status_name` | Current status |
| `site_name` | Site name |
| `approval_date` | Date approved |
| `establishment_date` | Date established |
| `comments` | Service area comments (nested) |
| `permittee_list` | Permittees (nested) |
| `ledger` | Transaction history (if requested) |
| `footprint` | Footprint geometry (if requested) |
| `service_area` | Service area geometry (if requested) |
| `contacts` | Contact information (if requested) |

### Ledger Transaction Fields

| Field | Description |
|-------|-------------|
| `transaction_id` | Unique transaction ID |
| `transaction_type` | Type: Init, Sell, Transfer, etc. |
| `transaction_date` | Date of transaction |
| `credits` | Number of credits |
| `acres` | Acreage |
| `credit_classification` | Classification (Wetlands, Streams) |
| `credit_type_list` | Credit type (Wetland, Stream, Species) |
| `jurisdiction` | Federal or State |
| `is_purchased` | Yes/No |
| `is_transferred` | Yes/No |
| `permittee_name` | Permittee name (if sold) |
| `permit_list` | Permit numbers (if sold) |
| `impact_huc_list` | Impact HUC codes |
| `comment` | Transaction comment |

### WQT Project Fields

| Field | Description |
|-------|-------------|
| `wqt_project_id` | Unique ID |
| `wqt_project_name` | Project name |
| `project_status_name` | Status |
| `project_cat_name` | Category |
| `state_abbrev_list` | States |
| `total_avail_phos` | Available phosphorus credits |
| `total_pend_of_phos` | Pending phosphorus credits |
| `total_potential_of_phos` | Potential phosphorus credits |

---

## Manual Download Reports

These reports require manual download (not available via API):

### Potential Credits by Mitigation Type
- **What**: Breakdown of credits by mitigation method (Establishment, Preservation, Enhancement, etc.)
- **Download**: `rb_download_report("potential_credits")`
- **Read**: `rb_read("potential_credits")`

### Credit Classification (also available via API)
- **What**: Credits by type and jurisdiction
- **API**: `rb_transactions()` or extract from ledger
- **Download**: `rb_download_report("credit_classification")`

### Credit Tracking
- **What**: Detailed transaction records with permittee info
- **API**: Most data available via `rb_get("banks", id = X, ledger = TRUE)`
- **Download**: `rb_download_report("credit_tracking")`

---

## Complete Workflow

### 1. Explore Available Data

```r
# See what's available
rb_get()
rb_epa()
rb_download_report()
```

### 2. Query Banks with Filters

```r
# List banks by state
or_banks <- rb_get("banks", state = "OR")

# Get specific bank with all data
bank <- rb_get("banks", id = 17, 
               ledger = TRUE, 
               footprint = TRUE, 
               service_area = TRUE)
```

### 3. Extract Nested Data

```r
# Extract ledger from bank
ledger <- rb_extract(bank, "ledger")

# Extract credit classifications
classifications <- rb_transactions(bank_ids = c(17, 100))

# Extract service area comments
comments <- rb_service_area_comments(bank_ids = c(17, 100))
```

### 4. Get Spatial Data

```r
# From API
footprints <- rb_epa("footprints", state = "OR")
service_areas <- rb_epa("service_areas", bank_ids = c(17, 100))

# ILF program areas
ilf_areas <- rb_epa("ilf_service_areas")
```
### 5. Download Manual Reports

```r
# Open browser for Potential Credits (mitigation type breakdown)
b <- rb_download_report("potential_credits")
# Click: Actions > Download > CSV
b$close()

# Read the downloaded file
potential <- rb_read("potential_credits")
```

### 6. Bulk Operations

```r
# Get all ledgers for multiple banks
ledgers <- rb_bulk_ledger(bank_ids = 1:100)

# Get multiple banks at once
banks <- rb_get("banks", id = c(17, 100, 345, 500), ledger = TRUE)
```

---

## Function Reference Summary

### Unified Functions (Recommended)

| Function | Purpose |
|----------|---------|
| `rb_get()` | Get any RIBITS data with filters |
| `rb_epa()` | Get EPA ArcGIS spatial data |
| `rb_download_report()` | Download manual reports |
| `rb_read()` | Read downloaded CSVs |
| `rb_near()` | Find banks/programs by location |

### Extraction Functions

| Function | Purpose |
|----------|---------|
| `rb_extract()` | Extract nested data from bank |
| `rb_transactions()` | Get credit classifications |
| `rb_service_area_comments()` | Get service area comments |
| `rb_bulk_ledger()` | Get ledgers for multiple banks |

### Legacy Functions (Still Work)

| Function | Unified Equivalent |
|----------|-------------------|
| `rb_list_banks()` | `rb_get("banks")` |
| `rb_get_bank()` | `rb_get("banks", id = X)` |
| `rb_epa_footprints()` | `rb_epa("footprints")` |
| `rb_download_potential_credits()` | `rb_download_report("potential_credits")` |

---

## Data Availability Summary

| Data Category | API | Manual |
|---------------|-----|--------|
| Bank list & metadata | ✓ | |
| Bank locations | ✓ | |
| Ledger transactions | ✓ | |
| Credit classification | ✓ | |
| Permittee/permit info | ✓ | |
| Service area comments | ✓ | |
| Footprint geometry | ✓ | |
| Service area geometry | ✓ | |
| **Mitigation type breakdown** | **✗** | **✓** |
| Transfer tracking details | | ✓ |

---

*Last updated: December 2025*
