
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RIBITSr

<!-- badges: start -->

<!-- badges: end -->

**RIBITSr** provides streamlined access to the USACE Regulatory In-lieu
fee and Bank Information Tracking System (RIBITS). The package
automatically handles data retrieval, harmonization, and spatial
processing from multiple sources (RIBITS API, EPA ArcGIS services, and
CSV reports).

## Key Features

- **Simple, unified interface** - One main function (`ribits()`) for
  most use cases
- **Automatic data harmonization** - Combines data from 3 sources into a
  single, clean dataset
- **Spatial data included** - Bank locations, service areas, and
  footprints with sf geometry
- **Transaction tracking** - Optional basic or comprehensive transaction
  history
- **Smart caching** - Reduces API calls and speeds up repeated queries
- **Contact management** - Detailed sponsor and POC information

## Installation

You can install the development version of RIBITSr from GitHub:

``` r
# install.packages("pak")
pak::pak("alexanderdhond/RIBITSr")
```

## Quick Start

### Get all banks in a state

The simplest way to get RIBITS data:

``` r
library(RIBITSr)

# Get all mitigation banks in Florida with basic transaction data
fl_banks <- ribits(type = "banks", state = "FL")

# View the data
fl_banks
#> <ribits_data: 45 banks>
#> ✔ 45 banks (with contact/credit summaries)
#> ✔ 1,234 transactions (unified)
#> ✔ 89 detailed contacts
#> ✔ 135 geometries (centroids + footprints + service areas)
```

### Choose transaction detail level

Control how much transaction data you want:

``` r
# No transactions (fastest)
banks_only <- ribits(state = "CA", transactions = "none")

# Basic transactions from API ledger
banks_basic <- ribits(state = "CA", transactions = "basic")

# Comprehensive transactions harmonized from 3 sources (~85 columns)
banks_comprehensive <- ribits(state = "CA", transactions = "comprehensive")
```

### Get specific banks by ID

``` r
# Get specific banks
my_banks <- ribits(ids = c("SAJ-2009-00123", "SAJ-2010-00456"))
```

### Work with ILF programs or umbrella banks

``` r
# Get all ILF programs in a district
ilf <- ribits(type = "ilf", district = "Jacksonville")

# Get umbrella mitigation banks
umbrellas <- ribits(type = "umbrellas", state = "TX")
```

## Core Functions

RIBITSr has a streamlined API with just a handful of user-facing
functions:

### Main Functions

| Function | Purpose |
|----|----|
| `ribits()` | **Main entry point** - Get banks/ILF/umbrellas with automatic harmonization |
| `rb_get()` | Advanced API for power users who need fine-grained control |

### Data Access & Utilities

| Function       | Purpose                                                    |
|----------------|------------------------------------------------------------|
| `rb_check()`   | Check if RIBITS API is accessible                          |
| `rb_info()`    | Get summary statistics about RIBITS data availability      |
| `rb_read()`    | Read previously saved RIBITS data from disk                |
| `rb_extract()` | Extract specific data components (contacts, credits, etc.) |

### Diagnostics & Analysis

| Function        | Purpose                                        |
|-----------------|------------------------------------------------|
| `rb_diagnose()` | Diagnose data quality and source discrepancies |
| `rb_near()`     | Find banks near a location or within a radius  |

### Connection Management

| Function | Purpose |
|----|----|
| `check_ribits_connection()` | Test connection to RIBITS and related services |

## Understanding the Data Structure

The `ribits()` function returns a `ribits_data` object with several
components:

``` r
# Get data
data <- ribits(state = "FL", transactions = "comprehensive")

# Access main bank information
data$banks              # Primary bank details

# Access spatial data
data$geometries         # Centroids, footprints, service areas (sf object)

# Access transactions
data$transactions       # Harmonized transaction data

# Access contact details
data$contacts          # Detailed sponsor and POC information

# Extract specific components easily
contacts <- rb_extract(data, "contacts")
credits <- rb_extract(data, "credits")
```

## Working with Spatial Data

All geometry is returned as sf objects, ready for mapping:

``` r
library(sf)
library(ggplot2)

# Get banks with spatial data
banks <- ribits(state = "NC")

# Map service areas
ggplot(banks$geometries) +
  geom_sf(aes(fill = geometry_type), alpha = 0.5) +
  theme_minimal()

# Find banks near a location
nearby <- rb_near(
  longitude = -80.843,
  latitude = 35.227,
  radius = 50,  # 50 miles
  state = "NC"
)
```

## Diagnosing Data Quality

Check for discrepancies between data sources:

``` r
# Diagnose a dataset
rb_diagnose(banks)
#> ── RIBITS Data Diagnostics ──
#> ✔ 45 banks analyzed
#> ✔ 3 data sources compared
#> ⚠ 12 discrepancies found
#>
#> Common issues:
#> • 8 banks have different credit values across sources
#> • 4 banks missing from EPA ArcGIS layer

# Compare ledger data across sources
rb_diagnose(type = "ledger", state = "FL")
```

## Advanced Usage

For power users who need more control, use `rb_get()`:

``` r
# Get specific data components without harmonization
raw_data <- rb_get(
  type = "banks",
  state = "FL",
  what = "spatial",           # Just spatial data, no transactions
  field_office = "Jacksonville",
  kind = "Mitigation",
  include_inactive = TRUE
)

# Get WQT projects (not available in simple API)
wqt <- rb_get(type = "wqt", state = "CA")
```

## Caching

RIBITSr caches API responses to improve performance:

``` r
# First call hits the API
banks1 <- ribits(state = "FL")

# Second call uses cache (much faster)
banks2 <- ribits(state = "FL")

# Force fresh data
banks3 <- ribits(state = "FL", cache = FALSE)
```

## Data Sources

RIBITSr harmonizes data from three sources:

1.  **RIBITS API** - Primary source, most up-to-date
2.  **EPA ArcGIS** - Spatial data and some attributes
3.  **CSV Reports** - Detailed transaction history

The `ribits()` function automatically combines these sources, resolving
conflicts and filling gaps.

## Getting Help

- Check function documentation: `?ribits`
- View package vignettes: `browseVignettes("RIBITSr")`
- Report issues: [GitHub
  Issues](https://github.com/alexanderdhond/RIBITSr/issues)

## Connection Issues?

If you’re having trouble connecting to RIBITS:

``` r
# Test all connections
check_ribits_connection()

# Check API status
rb_check()
```
