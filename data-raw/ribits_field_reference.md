# RIBITS API Field Reference

## Complete field inventory based on API exploration (December 2025)

---

## Summary of Available Endpoints

| Endpoint | Status | Items | Description |
|----------|--------|-------|-------------|
| `bank_site_list` | ✅ Working | ~4,700+ | List all banks/sites with filters |
| `bank_site_data` | ✅ Working | 1 per call | Full details for one bank |
| `ilf_program_list` | ✅ Working | ~170+ | List all ILF programs |
| `ilf_program_data` | ✅ Working | 1 per call | Full details for one ILF program |
| `umbrella_instrument_list` | ✅ Working | ~400+ | List all umbrella instruments |
| `umbrella_instrument_data` | ✅ Working | 1 per call | Full details for one umbrella |
| `wqt_project_list` | ✅ Working | ? | List WQT projects |
| `wqt_project_data` | ❌ Failed | - | May need different parameters |

---

## Bank Site List

**Endpoint:** `https://ribits.ops.usace.army.mil/ords/RI/public/bank_site_list/`

### Filter Parameters

| Parameter | Type | Options |
|-----------|------|---------|
| `kind_of_bank` | string | `ILF`, `Umbrella`, `NRDA`, `Standard` (comma-separated) |
| `district` | string | 38 USACE districts |
| `fieldoffice` | string | 68 FWS field offices |
| `state` | string | 57 state/territory codes |
| `noaaregion` | string | `Alaska`, `Northeast`, `Pacific Islands`, `Southeast`, `West Coast` |
| `webconsumer_email` | string | Your email for tracking |

### Bank Counts by Type (as of Dec 2025)

| Kind of Bank | Count |
|--------------|-------|
| Standard | 2,777 |
| ILF | 1,741 |
| Umbrella | 414 |
| NRDA | 5 |

### Fields Returned

| Field | Type | Description |
|-------|------|-------------|
| `BANK_ID` | integer | Unique identifier |
| `BANK_NAME` | string | Bank name |
| `BANK_SITE_DATA_WS_URL` | string | API URL for full details |

---

## Bank Site Data

**Endpoint:** `https://ribits.ops.usace.army.mil/ords/RI/public/bank_site_data/`

### Parameters

| Parameter | Required | Options | Default |
|-----------|----------|---------|---------|
| `bank_id` | **Yes** | integer | - |
| `show_service_area` | No | `Yes`, `No` | `No` |
| `show_footprint` | No | `Yes`, `No` | `No` |
| `show_contacts` | No | `Yes`, `No` | `No` |
| `show_ledger` | No | `Yes`, `No` | `No` |
| `webconsumer_email` | No | string | - |

### Flat Fields (20 fields)

| # | Field | Type | Sample | Description |
|---|-------|------|--------|-------------|
| 1 | `BANK_ID` | integer | 3646 | Unique identifier |
| 2 | `BANK_NAME` | string | "Smith Property..." | Bank name |
| 3 | `KIND_OF_BANK` | string | "Standard" | ILF, Umbrella, NRDA, or Standard |
| 4 | `IS_404` | string | "Yes" | Is this a 404 permit bank? |
| 5 | `IS_CONSERVATION` | string | "No" | Is this a conservation bank? |
| 6 | `CHAIR` | string | "USACE" | Overseeing agency |
| 7 | `DISTRICT` | string | "Louisville" | Primary USACE district |
| 8 | `FIELD_OFFICE` | string | "Kentucky" | Primary FWS field office |
| 9 | `NMFS_REGION` | string | "Southeast" | NMFS region |
| 10 | `STATE_LIST` | string | "KY" | Comma-separated state codes |
| 11 | `COUNTY_LIST` | string | "Elliott [KY]" | Comma-separated counties |
| 12 | `PERMIT_NUMBER` | string | "LRL-2013-01013" | Permit number(s) |
| 13 | `YEAR_ESTABLISHED` | integer | 2021 | Year established (YYYY) |
| 14 | `ESTABLISHMENT_DATE` | string | "06/01/2021" | Full establishment date |
| 15 | `BANK_STATUS` | string | "Approved" | Current status |
| 16 | `BANK_STATUS_DATE` | string | "06/01/2021" | Date of status change |
| 17 | `BANK_TYPE` | string | "Public Commercial" | Type of bank |
| 18 | `RIBITS_URL_TO_BANK` | string | "https://ribits..." | Link to RIBITS website |
| 19 | `BANK_GEOMETRY_OBSCURED` | integer | 0 | 1 if location hidden |
| 20 | `BANK_LOCATION_CENTROID` | string | GeoJSON Point | Centroid coordinates |

### Nested Fields (8 fields)

| Field | Type | Triggered By | Description |
|-------|------|--------------|-------------|
| `BANK_FOOTPRINT` | list/GeoJSON | `show_footprint=Yes` | Bank boundary polygon |
| `SERVICE_AREAS` | list/GeoJSON | `show_service_area=Yes` | Service area polygons |
| `BANK_SPONSORS` | list | `show_contacts=Yes` | Sponsor organizations |
| `BANK_POCS` | list | `show_contacts=Yes` | Points of contact |
| `BANK_MANAGERS` | list | `show_contacts=Yes` | Regulatory managers |
| `BANK_IRT_MEMBERS` | list | `show_contacts=Yes` | IRT members |
| `BANK_OTHER_CONTACTS` | list | `show_contacts=Yes` | Other contacts |
| `LEDGER` | list | `show_ledger=Yes` | Transaction records |

### Ledger Fields (11 fields per transaction)

| Field | Type | Description |
|-------|------|-------------|
| `TRANSACTION_ID` | integer | Unique transaction identifier |
| `TRANSACTION_TYPE` | string | Type of transaction |
| `JURISDICTION` | string | Regulatory jurisdiction |
| `TRANSACTION_DATE` | string | Date of transaction |
| `CREDITS` | numeric | Number of credits |
| `ACRES` | numeric | Acres involved |
| `CREDIT_TYPE_LIST` | string | Types of credits involved |
| `CREDIT_CLASSIFICATION` | string | Credit classification |
| `COMMENT` | string | Transaction notes |
| `IS_TRANSFERRED` | string | Was this a transfer? |
| `IS_PURCHASED` | string | Was this a purchase? |

---

## ILF Program Data

**Endpoint:** `https://ribits.ops.usace.army.mil/ords/RI/public/ilf_program_data/`

### Parameters

| Parameter | Required | Options |
|-----------|----------|---------|
| `program_id` | **Yes** | integer |
| `show_service_area` | No | `Yes`, `No` |
| `show_contacts` | No | `Yes`, `No` |
| `show_ledger` | No | `Yes`, `No` |
| `webconsumer_email` | No | string |

### Fields Returned (observed)

| Field | Type | Description |
|-------|------|-------------|
| `PROGRAM_SITES` | list | Array of all sites in the program (can be 100+) |
| `PROGRAM_SPONSORS` | list | Sponsor organizations |
| `PROGRAM_POCS` | list | Points of contact |
| `PROGRAM_MANAGERS` | list | Program managers |
| `PROGRAM_IRT_MEMBERS` | list | IRT members |
| `PROGRAM_OTHER_CONTACTS` | list | Other contacts |
| `LEDGER` | list | Program-level transactions |
| `RIBITS_URL_TO_PROGRAM` | string | Link to RIBITS website |

### Program Sites Sub-fields

| Field | Type | Description |
|-------|------|-------------|
| `BANK_ID` | integer | Links to bank_site_data |
| `BANK_NAME` | string | Site name |
| `BANK_SITE_DATA_WS_URL` | string | API URL for site details |

---

## Data Available via API (Updated December 2025)

**IMPORTANT:** The API returns MORE data than previously documented. Many fields
listed as "manual-only" are actually available via the API.

### Ledger Fields (15 fields - ALL available via API)

| Field | Description | Previously Thought |
|-------|-------------|-------------------|
| `TRANSACTION_ID` | Unique transaction ID | ✅ Known |
| `TRANSACTION_TYPE` | Init/Rel/Wdr | ✅ Known |
| `TRANSACTION_DATE` | Date of transaction | ✅ Known |
| `JURISDICTION` | Federal/State | ✅ Known |
| `CREDITS` | Number of credits | ✅ Known |
| `ACRES` | Acres involved | ✅ Known |
| `CREDIT_TYPE_LIST` | Wetland/Stream/etc | ✅ Known |
| `CREDIT_CLASSIFICATION` | Classification type | ❌ Was "manual-only" |
| `COMMENT` | Transaction notes | ✅ Known |
| `IS_TRANSFERRED` | Transfer flag | ✅ Known |
| `IS_PURCHASED` | Purchase flag | ✅ Known |
| `PERMITTEE` | Who purchased credits | ❌ Was "manual-only" |
| `PERMIT_LIST` | Associated permits | ❌ Was "manual-only" |
| `IMPACT_HUC` | Impact watershed | ❌ Was "manual-only" |
| `IMPACT_QUANTITY` | Impact amount | ❌ Was "manual-only" |

### Service Area Comments (Available via API)

The `COMMENTS` field at the bank level contains service area comments!
Use `rb_service_area_comments()` to extract.

---

## Data Still Requiring Manual Download

### 1. Potential Credits by Mitigation Type
**Available via:** Manual download → "Potential Credits by Mitigation Type" report

Fields not in API:
- Credits by mitigation method (Establishment, Re-establishment, Rehabilitation, 
  Preservation, Enhancement, Uplands/Buffer)
- Initial acres/feet by method

### 2. Transfer Tracking Details
**Available via:** Manual download → "Credit Tracking" report

Fields not in API:
- `credits_purchased_from_bank` vs `credits_fulfilled_at_site`
- `date_purchased_from_bank` vs `date_fulfilled_at_site`
- `is_blm_program_or_blm_project_site`
- `accepted_in_settlement`

---

## API vs Manual Download: When to Use Each (Updated Dec 2025)

| Data Need | Use API | Use Manual Download |
|-----------|---------|---------------------|
| Bank list with filters | ✅ | |
| Bank attributes (status, type, dates, etc.) | ✅ | |
| Bank footprint geometries | ✅ | |
| Service area geometries | ✅ | |
| Contact information | ✅ | |
| **Full ledger with all fields** | ✅ | |
| **Credit classification** | ✅ | |
| **Permittee/permit details** | ✅ | |
| **Impact HUC** | ✅ | |
| **Service area comments** | ✅ | |
| ILF program structure | ✅ | |
| Umbrella instrument structure | ✅ | |
| Credits by mitigation method | | ✅ |
| Transfer tracking details | | ✅ |

---

## R Package Function Mapping

```r
# ══════════════════════════════════════════════════════════════════════════════
# TIER 1: Full API Access
# ══════════════════════════════════════════════════════════════════════════════

# Bank/Site functions
rb_list_banks(kind = NULL, district = NULL, state = NULL, 
              noaaregion = NULL, fieldoffice = NULL)
rb_get_bank(bank_id, service_area = TRUE, footprint = TRUE, 
            contacts = TRUE, ledger = TRUE)
rb_get_banks(bank_ids, ...)

# ILF Program functions
rb_list_ilf_programs(...)
rb_get_ilf_program(program_id, service_area = TRUE, contacts = TRUE, ledger = TRUE)

# Umbrella Instrument functions
rb_list_umbrellas(...)
rb_get_umbrella(umbrella_id, ...)

# WQT Project functions
rb_list_wqt_projects(...)
# rb_get_wqt_project() - needs investigation

# ══════════════════════════════════════════════════════════════════════════════
# TIER 2: Data Extraction Helpers
# ══════════════════════════════════════════════════════════════════════════════

rb_extract_ledger(bank_data)       # Extract ledger as tibble
rb_extract_contacts(bank_data)     # Extract contacts as tibble
rb_extract_footprint(bank_data)    # Extract as sf object
rb_extract_service_areas(bank_data) # Extract as sf object

rb_banks_to_sf(banks_data)         # Convert bank centroids to sf
rb_combine_geometries(banks_list)  # Combine multiple banks' geometries

# ══════════════════════════════════════════════════════════════════════════════
# TIER 3: Manual Download Processors
# ══════════════════════════════════════════════════════════════════════════════

# These process CSVs downloaded manually from RIBITS reports
rb_read_credit_classification(file)  # Credit type breakdowns
rb_read_potential_credits(file)      # Mitigation method breakdowns
rb_read_credit_tracking(file)        # Permittee/permit transaction details
rb_read_credit_withdrawal(file)      # Withdrawal details
rb_read_service_area_comments(file)  # Service area text comments

# ══════════════════════════════════════════════════════════════════════════════
# TIER 4: Reference Data
# ══════════════════════════════════════════════════════════════════════════════

rb_districts()        # List of 38 USACE districts
rb_field_offices()    # List of 68 FWS field offices
rb_states()           # List of 57 state/territory codes
rb_nmfs_regions()     # List of 5 NMFS regions
rb_bank_kinds()       # ILF, Umbrella, NRDA, Standard
rb_bank_statuses()    # Valid bank status values
```

---

## Example Workflows

### Get all California wetland banks with spatial data

```r
# List CA banks
ca_banks <- rb_list_banks(state = "CA", kind = "Standard")

# Get full details with geometry for each
ca_details <- rb_get_banks(ca_banks$BANK_ID, 
                           footprint = TRUE, 
                           service_area = TRUE)

# Convert to spatial
ca_sf <- rb_banks_to_sf(ca_details)
```

### Analyze ILF program performance

```r
# Get program details
vartf <- rb_get_ilf_program(1, ledger = TRUE)

# Extract sites
sites <- vartf$PROGRAM_SITES

# Get details for each site
site_details <- rb_get_banks(sites$BANK_ID, ledger = TRUE)

# Combine ledgers
all_transactions <- map_dfr(site_details, rb_extract_ledger)
```

### Complete credit analysis (requires manual download)

```r
# API data
banks <- rb_list_banks() %>% 
  rb_get_banks(ledger = TRUE)

# Manual download data (richer transaction detail)
credit_tracking <- rb_read_credit_tracking("Credit_Tracking_Report.csv")
credit_class <- rb_read_credit_classification("Credit_Classification_Report.csv")

# Join for complete picture
full_analysis <- banks %>%
  left_join(credit_tracking, by = "BANK_ID") %>%
  left_join(credit_class, by = "BANK_ID")
```
