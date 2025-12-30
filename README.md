
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RIBITSr

<!-- badges: start -->

<!-- badges: end -->

**RIBITSr** provides streamlined access to the USACE Regulatory In-lieu
fee and Bank Information Tracking System (RIBITS). Download all the data
you need with one simple function, then filter and analyze using
tidyverse tools.

## Philosophy: Get Everything, Filter with Tidyverse

RIBITSr follows a simple principle: **download the maximum amount of
data by default**, then let you shape it using familiar tidyverse tools.
No complex options to configure—just get the data and start analyzing.

``` r
library(RIBITSr)
library(dplyr)

# Step 1: Get everything
ca <- ribits(state = "CA")  # Downloads all data, all columns, all sources

# Step 2: Filter with tidyverse
ca$banks %>%
  filter(bank_status == "Approved", total_acres > 100) %>%
  select(bank_id, bank_name, available_credits, total_acres)
```

## Key Features

- **One main function** - `ribits()` gets you everything you need
- **Maximum data by default** - All columns, all sources (~85
  transaction columns!)
- **Automatic harmonization** - Combines RIBITS API, EPA ArcGIS, and CSV
  reports
- **Spatial data included** - Bank locations, footprints, and service
  areas (sf geometry)
- **Smart caching** - Fast repeated queries
- **Works with tidyverse** - Filter, mutate, and analyze with
  dplyr/tidyr

## Installation

Install from GitHub:

``` r
# install.packages("pak")
pak::pak("alexanderdhond/RIBITSr")
```

## Quick Start

### The Simple Way

Get all data for a state:

``` r
library(RIBITSr)

# Get everything for California (comprehensive transactions by default!)
ca <- ribits(state = "CA")

# View the structure
ca
#> <ribits_data: 128 banks>
#> ✔ 128 banks with summaries
#> ✔ 4,521 transactions (~85 columns, harmonized from 3 sources)
#> ✔ 342 detailed contacts
#> ✔ 256 spatial geometries

# Access the data
ca$banks          # All bank attributes (1 row per bank)
ca$transactions   # All transaction data (~85 columns)
ca$geometry       # Spatial footprints and service areas
```

### Filter with Tidyverse

``` r
library(dplyr)

# Get approved banks with high credit availability
high_credit_banks <- ca$banks %>%
  filter(
    bank_status == "Approved",
    available_credits > 100
  ) %>%
  select(bank_id, bank_name, bank_type, total_acres, available_credits) %>%
  arrange(desc(available_credits))

# Analyze transaction patterns
recent_transactions <- ca$transactions %>%
  filter(transaction_date >= as.Date("2020-01-01")) %>%
  group_by(bank_id, bank_name) %>%
  summarize(
    n_transactions = n(),
    total_debits = sum(debits, na.rm = TRUE),
    avg_transaction_size = mean(debits, na.rm = TRUE)
  )

# Find banks in specific watershed
huc8_banks <- ca$banks %>%
  filter(grepl("18010201", huc8_list))
```

### Map the Data

All geometry is ready for mapping with sf:

``` r
library(sf)
library(ggplot2)

# Map bank footprints
ggplot(ca$geometry) +
  geom_sf(data = filter(ca$geometry, !is.na(footprint)),
          aes(geometry = footprint),
          fill = "lightblue", alpha = 0.6) +
  theme_minimal() +
  labs(title = "California Mitigation Bank Footprints")

# Find banks near a point
ca_banks_sf <- st_as_sf(ca$banks,
                        coords = c("longitude", "latitude"),
                        crs = 4326)

# Filter to banks within 50km of Sacramento
sacramento <- st_point(c(-121.4944, 38.5816)) %>% st_sfc(crs = 4326)
nearby <- ca_banks_sf %>%
  filter(st_distance(geometry, sacramento, by_element = TRUE) < units::set_units(50, "km"))
```

## Core Functions (Simple API!)

RIBITSr has a streamlined API with just 6 core functions:

| Function           | Purpose                                                |
|--------------------|--------------------------------------------------------|
| **`ribits()`**     | **Main function** - Get all data (banks/ILF/umbrellas) |
| `rb_info()`        | Explore what data is available                         |
| `rb_config()`      | Configure settings (caching, network, etc.)            |
| `rb_check()`       | Check data coverage or quality                         |
| `rb_read()`        | Read saved RIBITS CSV files                            |
| `rb_clear_cache()` | Clear download cache                                   |

That’s it! Most users will only need `ribits()`.

## Understanding the Data Structure

The `ribits()` function returns a `ribits_data` object:

``` r
data <- ribits(state = "FL")

# Main components
data$banks          # Bank summary (1 row per bank, all attributes)
data$transactions   # Transaction data (many rows per bank, ~85 columns)
data$geometry       # Spatial data (footprints, service areas as sf object)
data$.meta          # Metadata (sources used, fetch time, discrepancies)

# Check for data quality issues
discrepancies(data)  # Any conflicts between sources?
resolutions(data)    # How were conflicts resolved?
```

## Options for Different Needs

By default, `ribits()` gives you **everything**. For specific needs:

``` r
# Just need basic info (no transactions, no spatial)
fast <- ribits(state = "OR", transactions = "none", spatial = FALSE)

# Want basic transactions instead of comprehensive
basic <- ribits(state = "TX", transactions = "basic")  # ~20 columns instead of ~85

# ILF programs
ilf <- ribits(type = "ilf", state = "CA")

# Umbrella banks
umbrellas <- ribits(type = "umbrellas", state = "FL")

# Specific banks by ID
my_banks <- ribits(ids = c(17, 100, 345))
```

## Discover Available Data

Before downloading, see what’s available:

``` r
# Overview of all data sources
rb_info()

# Check coverage for a state
rb_check(state = "WA")
#> Data Coverage: state=WA
#> ✔ Banks in API: 45
#> ✔ Banks in EPA: 38 (84% of API)
#> ✔ With footprints: 35 (78%)
#> ✔ With service areas: 42 (93%)

# After downloading, check quality
wa <- ribits(state = "WA")
rb_check(wa)
#> Data Quality Report
#> ✔ 45 banks
#> ✔ bank_name: 100% complete
#> ✔ total_acres: 96% complete
#> ⚠ 3 discrepancies found between sources
```

## Configuration

Adjust settings with `rb_config()`:

``` r
# View current settings
rb_config()

# Enable persistent caching (survives R restart)
rb_config(use_persistent_cache = TRUE, cache_max_age_days = 7)

# For slow/unreliable networks
rb_config(max_retries = 5, timeout = 120, rate_limit = 2)

# Reset to defaults
rb_config(reset = TRUE)
```

You can also set environment variables in `.Renviron`:

    RIBITS_MAX_RETRIES=5
    RIBITS_USE_PERSISTENT_CACHE=true
    RIBITS_CACHE_MAX_AGE_DAYS=7

## Real-World Examples

### Analyze credit availability by district

``` r
library(dplyr)
library(ggplot2)

# Get all California banks
ca <- ribits(state = "CA")

# Summarize by district
district_summary <- ca$banks %>%
  group_by(district) %>%
  summarize(
    n_banks = n(),
    total_available = sum(available_credits, na.rm = TRUE),
    avg_available = mean(available_credits, na.rm = TRUE),
    n_approved = sum(bank_status == "Approved", na.rm = TRUE)
  ) %>%
  arrange(desc(total_available))

# Visualize
ggplot(district_summary, aes(x = reorder(district, total_available), y = total_available)) +
  geom_col() +
  coord_flip() +
  labs(title = "Available Credits by USACE District",
       x = "District", y = "Total Available Credits")
```

### Track transaction patterns over time

``` r
# Analyze transaction volume by year
transaction_trends <- ca$transactions %>%
  mutate(year = lubridate::year(transaction_date)) %>%
  filter(!is.na(year), year >= 2015) %>%
  group_by(year) %>%
  summarize(
    n_transactions = n(),
    total_debits = sum(debits, na.rm = TRUE),
    n_banks_active = n_distinct(bank_id)
  )

# Plot trends
ggplot(transaction_trends, aes(x = year, y = total_debits)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "California Mitigation Banking: Annual Credit Usage",
       x = "Year", y = "Total Credits Debited")
```

### Compare service areas

``` r
library(sf)

# Get banks with service areas
banks_with_sa <- ca$geometry %>%
  filter(!is.na(service_area))

# Calculate service area sizes
service_area_stats <- banks_with_sa %>%
  st_drop_geometry() %>%
  left_join(ca$banks, by = "bank_id") %>%
  mutate(service_area_sqkm = as.numeric(st_area(service_area)) / 1e6) %>%
  select(bank_id, bank_name, service_area_sqkm, available_credits) %>%
  arrange(desc(service_area_sqkm))
```

## Data Sources

RIBITSr automatically harmonizes data from three sources:

1.  **RIBITS API** - Real-time data, all bank statuses
2.  **EPA ArcGIS** - Spatial data (centroids, footprints, service areas)
3.  **CSV Reports** - Official transaction records with maximum detail

The `ribits()` function merges these sources, resolves conflicts, and
fills gaps automatically.

## Getting Help

- Function documentation: `?ribits`
- Package vignettes: `browseVignettes("RIBITSr")`
- Issues: [GitHub](https://github.com/alexanderdhond/RIBITSr/issues)

## Troubleshooting

The package handles connection issues automatically with retries, but if
you’re having persistent problems:

``` r
# Check connection
rb_check()

# Try with reduced rate limiting
rb_config(rate_limit = 2, timeout = 120)

# Clear cache and try again
rb_clear_cache()
```
