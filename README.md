# RIBITSr

R package for accessing RIBITS (Regulatory In-lieu fee and Bank Information Tracking System) data.

## Installation

```r
devtools::install_github("alexdhond/RIBITSr")
```

## Usage

Get mitigation bank data for a state:

```r
library(RIBITSr)

# Get California banks
ca <- ribits(state = "CA")

# Access components
ca$banks          # Bank info
ca$ledger         # Transaction history
ca$footprints     # Spatial data (sf)
```

Query specific banks:

```r
# List Oregon banks
or_banks <- rb_get("banks", state = "OR")

# Get detailed data for specific bank
bank <- rb_get("banks", id = 17, ledger = TRUE)
```

Get spatial data from EPA ArcGIS:

```r
# Bank locations
banks <- rb_epa("banks", state = "TX")

# Footprints
footprints <- rb_epa("footprints", state = "CA")
```

Other data types:

```r
# ILF programs
ilf <- rb_get("ilf", state = "CA")

# Umbrella instruments
umbrellas <- rb_get("umbrellas", district = "Portland")
```

## Functions

- `ribits()` - Main function, fetches and harmonizes data from multiple sources
- `rb_get()` - Query RIBITS API directly
- `rb_epa()` - Query EPA ArcGIS layers
- `rb_download_report()` - Download CSV reports
- `rb_transactions()` - Get transaction data with harmonization

## License

MIT
