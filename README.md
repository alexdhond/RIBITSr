# RIBITSr

> **Simple R interface to RIBITS (Regulatory In-lieu fee and Bank Information Tracking System)**

Fetch mitigation banking data from multiple sources with a single function call. No manual downloads, no complexity—just clean, harmonized data ready for analysis.

## Installation

```r
# Install from GitHub
devtools::install_github("alexdhond/RIBITSr")
```

## Quick Start

```r
library(RIBITSr)

# Get all California banks (fully harmonized from all sources)
ca_banks <- rb_banks(state = "CA")

# Access the data
ca_banks$banks           # Bank summary data
ca_banks$ledger          # Transaction history
ca_banks$footprints      # Spatial polygons (sf object)

# That's it! Export for analysis
readr::write_csv(ca_banks$banks, "california_banks.csv")
```

## Why RIBITSr?

**Before:**

```r
# Manual downloads, confusing APIs, data inconsistencies...
rb_download("bank_summary")  # Manual browser download
rb_download("credit_tracking")  # Another manual download
data <- rb_harmonize_dir("downloads/")  # Reconcile differences
```

**After:**

```r
# One function call - everything automatic
banks <- rb_banks(state = "CA")
```

### What RIBITSr Does Automatically

✅ Fetches from **3 data sources** (RIBITS API, EPA ArcGIS, direct CSV downloads)
✅ **Harmonizes** conflicting data between sources
✅ **Detects discrepancies** and flags data quality issues
✅ Returns **clean tibbles** ready for analysis
✅ **No manual downloads** - everything is programmatic
✅ **Caches** data to avoid redundant fetches

---

## Core Functions (Simple API)

### `rb_banks()` - Get Bank Data

Get all mitigation bank data with one call:

```r
# Get California banks (all data sources, fully harmonized)
ca <- rb_banks(state = "CA")

# Just summary data (faster)
summary <- rb_banks(state = "OR", spatial = FALSE)

# Specific banks by ID
my_banks <- rb_banks(bank_ids = c(17, 100, 345))

# Only API data (real-time, fastest)
api_data <- rb_banks(state = "TX", sources = "api")

# Only CSV data (official records)
csv_data <- rb_banks(state = "FL", sources = "csv")

# Check data quality
ca$.meta$discrepancies  # View conflicts between sources
```

**Returns:**

- `$banks` - Bank summary data (tibble)
- `$ledger` - Transaction/credit tracking (tibble)
- `$footprints` - Bank footprint geometries (sf object)
- `$service_areas` - Service area geometries (sf object)
- `$.meta` - Metadata (sources used, discrepancies, timing)

### `rb_credits()` - Get Credit Tracking Data

Get comprehensive credit data directly from RIBITS reports:

```r
# Get all credit tracking data for California
credits <- rb_credits(state = "CA")

# Access specific reports
credits$tracking          # Credit ledger/transactions
credits$classification    # Credits by type
credits$potential         # Potential credits
credits$withdrawal        # Credit withdrawals

# Just tracking data
tracking <- rb_credits(state = "OR", reports = "tracking")

# Specific banks
bank_credits <- rb_credits(bank_ids = c(17, 100))
```

**Available reports:** `"tracking"`, `"classification"`, `"potential"`, `"withdrawal"`, `"all"`

### `rb_ilf_programs()` - Get ILF Program Data

```r
# Get all California ILF programs
ca_ilf <- rb_ilf_programs(state = "CA")

# Access the data
ca_ilf$programs      # Program summary
ca_ilf$footprints    # Program footprints
```

### `rb_umbrellas()` - Get Umbrella Instrument Data

```r
# Get umbrella instruments
umbrellas <- rb_umbrellas(state = "FL")

# Access the data
umbrellas$umbrellas     # Umbrella summary
umbrellas$footprints    # Spatial data
```

---

## Advanced API (Power Users)

For fine-grained control, use the advanced API:

### `rb_get()` - Direct API Queries

```r
# Show all options
rb_get()

# List banks with filters
rb_get("banks", state = "OR")
rb_get("banks", district = "Sacramento", status = "Approved")

# Get detailed data for specific bank(s)
rb_get("banks", id = 17, ledger = TRUE, footprint = TRUE)

# Other resource types
rb_get("ilf", state = "CA")
rb_get("umbrellas", district = "Portland")
```

**Available filters:** `state`, `district`, `field_office`, `noaa_region`, `kind`, `status`

**Data options:** `ledger`, `footprint`, `service_area`, `contacts`

### `rb_epa()` - EPA ArcGIS Queries

```r
# Show available layers
rb_epa()

# Query specific layers
rb_epa("banks", state = "OR")
rb_epa("footprints", bank_ids = c(17, 100))
rb_epa("service_areas", district = "Portland")
```

**Layers:** `banks`, `footprints`, `service_areas`, `ilf_programs`, `ilf_service_areas`

### `rb_download_report()` - Direct CSV Downloads

Download RIBITS reports programmatically (no browser needed!):

```r
# Download directly to disk
rb_download_report("credit_classification", download_dir = "data/")
rb_download_report("bank_summary", download_dir = "data/")

# Available reports
rb_report_types()
```

**Reports:** `credit_classification`, `credit_tracking`, `potential_credits`, `credit_withdrawal`, `bank_summary`, `service_area_comments`

### `rb_read()` - Read Downloaded CSVs

```r
# Read any RIBITS CSV file (auto-detects type)
data <- rb_read("data/credit_classification.csv")
data <- rb_read("data/potential_credits.csv")  # Special parser for this report
```

---

## Data Extraction

Extract nested data from API responses:

```r
# Get bank with nested data
bank <- rb_get("banks", id = 17, ledger = TRUE, contacts = TRUE)

# Extract specific fields
ledger <- rb_extract(bank, "ledger")
contacts <- rb_extract(bank, "bank_pocs")
footprint <- rb_extract(bank, "bank_footprint")  # Returns sf object

# Convenience extractors
rb_extract_ledger(bank)
rb_extract_contacts(bank)
rb_extract_footprint(bank)
```

---

## Data Sources & Harmonization

RIBITSr automatically fetches and harmonizes data from three sources:

### 1. **RIBITS API** (Real-time)

- Most current data
- Detailed transaction records
- Contact information
- Programmatic access

### 2. **EPA ArcGIS MapServer** (Spatial)

- Comprehensive spatial coverage
- Fast bulk queries
- Footprints and service areas
- Official EPA data

### 3. **RIBITS CSV Reports** (Official Records)

- Direct from Oracle APEX
- Complete datasets
- Official source of truth
- Downloaded programmatically (no browser!)

### Harmonization Process

When multiple sources return data for the same bank:

1. **Primary source**: RIBITS API (most current)
2. **Spatial fallback**: EPA ArcGIS for missing geometries
3. **Official records**: CSV reports when API unavailable
4. **Discrepancy detection**: Flags conflicts between sources
5. **Best value selection**: Intelligent merging of complementary data

```r
# View discrepancies
ca <- rb_banks(state = "CA")
ca$.meta$discrepancies  # Tibble showing conflicts

# Control which sources to use
api_only <- rb_banks(state = "CA", sources = "api")
csv_only <- rb_banks(state = "CA", sources = "csv")
harmonized <- rb_banks(state = "CA", sources = c("api", "epa", "csv"))  # Default
```

---

## Common Workflows

### Statewide Analysis

```r
library(RIBITSr)
library(dplyr)
library(sf)

# Get all California banks
ca <- rb_banks(state = "CA", ledger = TRUE, spatial = TRUE)

# Analyze credit availability
ca$banks %>%
  group_by(bank_type) %>%
  summarise(
    n_banks = n(),
    total_credits = sum(available_credits, na.rm = TRUE)
  )

# Map bank locations
library(ggplot2)
ggplot() +
  geom_sf(data = ca$footprints, aes(fill = bank_type)) +
  theme_minimal() +
  labs(title = "California Mitigation Banks")
```

### Credit Tracking Over Time

```r
# Get credit tracking data
credits <- rb_credits(state = "OR", reports = "tracking")

# Analyze trends
credits$tracking %>%
  mutate(year = lubridate::year(transaction_date)) %>%
  group_by(year, transaction_type) %>%
  summarise(total_credits = sum(credit_amount, na.rm = TRUE))
```

### Multi-State Comparison

```r
# Get data for multiple states
states <- c("CA", "OR", "WA")
data <- lapply(states, rb_banks)
names(data) <- states

# Combine and compare
all_banks <- bind_rows(
  lapply(names(data), function(st) {
    data[[st]]$banks %>% mutate(state = st)
  })
)
```

### Export for GIS

```r
# Get spatial data
ca <- rb_banks(state = "CA", spatial = TRUE)

# Export to shapefile
sf::st_write(ca$footprints, "ca_bank_footprints.shp")
sf::st_write(ca$service_areas, "ca_service_areas.shp")

# Export to GeoJSON
sf::st_write(ca$footprints, "ca_banks.geojson")
```

---

## Package Architecture

```text
User-Facing (Simple API)
├── rb_banks()          ← Recommended for most users
├── rb_credits()
├── rb_ilf_programs()
└── rb_umbrellas()

Advanced API
├── rb_get()            ← Fine-grained API queries
├── rb_epa()            ← Direct EPA ArcGIS access
├── rb_download_report() ← Direct CSV downloads
└── rb_read()           ← Read CSV files

Internal Infrastructure
├── ribits()                    ← Auto-harmonization engine
├── discrepancy-handling.R      ← Multi-source conflict resolution
├── rb_bulk_ledger()            ← Bulk extraction with retry
└── Network layer               ← Retry logic, checkpointing
```

---

## Data Quality

RIBITSr helps you identify and handle data quality issues:

```r
ca <- rb_banks(state = "CA")

# Check for discrepancies between sources
ca$.meta$discrepancies

# Example discrepancy output:
# # A tibble: 3 × 6
#   bank_id data_type  value1_acres value2_acres diff_pct source1    source2
#     <int> <chr>             <dbl>        <dbl>    <dbl> <chr>      <chr>
# 1      17 footprint          45.2         46.1      2.0 ribits_api epa_arcgis
# 2     100 footprint         120.5        118.3     -1.8 ribits_api epa_arcgis
# 3     345 service_area     5234.2       5301.8      1.3 ribits_api epa_arcgis

# View metadata
ca$.meta$sources        # Which sources were used
ca$.meta$timing         # How long the query took
```

---

## Network Resilience

RIBITSr is designed for reliability:

- **Automatic retries** with exponential backoff
- **Rate limiting** to respect API limits
- **Checkpointing** for long-running bulk operations
- **Session caching** to avoid redundant fetches
- **Graceful degradation** when sources are unavailable

```r
# Configure network behavior
rb_network_config(
  max_retries = 5,
  retry_delay = 2,
  timeout = 30
)

# View network failures
rb_network_failures()
```

---

## Contributing

Contributions welcome! Please file issues at:
<https://github.com/alexdhond/RIBITSr/issues>

## License

MIT License

## Citation

```r
citation("RIBITSr")
```

---

## Troubleshooting

### "No data returned"

```r
# Check your filters
rb_get("banks", state = "ZZ")  # Invalid state

# Try different sources
rb_banks(state = "CA", sources = "epa")  # Fallback to EPA
```

### "Connection timeout"

```r
# Increase timeout
rb_network_config(timeout = 60)

# Or use CSV source (more reliable)
rb_banks(state = "CA", sources = "csv")
```

### "Discrepancies detected"

This is normal! Different sources may have slightly different values. RIBITSr flags these so you can decide which to trust.

```r
# View discrepancies
data$.meta$discrepancies

# Choose preferred source
api_only <- rb_banks(state = "CA", sources = "api")
```

---

**Questions?** Open an issue or check the full documentation:

```r
help(package = "RIBITSr")
?rb_banks
?ribits
```
