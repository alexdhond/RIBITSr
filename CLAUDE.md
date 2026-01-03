# RIBITSr Package Development Guide

## Project Overview

**RIBITSr** provides streamlined access to the USACE Regulatory In-lieu fee and Bank Information Tracking System (RIBITS). This R package enables users to download, harmonize, and analyze mitigation banking data from multiple sources.

- **Type**: R package
- **Main Dependencies**: tidyverse (dplyr, purrr, tidyr), httr2, jsonlite, sf, cli
- **Package Philosophy**: "Get everything, filter with tidyverse" - download maximum data by default, then let users filter with familiar tidyverse tools
- **Version**: 0.0.0.9000 (development)

## Core Architecture

### Entry Points
- `ribits()` - Main user-facing function (get all data by default)
- `rb_info()` - Explore available data
- `rb_config()` - Configure caching, network, harmonization settings
- `rb_check()` - Check data coverage and quality
- `rb_read()` - Read saved RIBITS CSV files
- `rb_clear_cache()` - Clear download cache

### File Organization
```
R/
├── api-*.R           # API client functions (one domain per file)
│   ├── api-ribits.R  # Main RIBITS API
│   ├── api-epa.R     # EPA ArcGIS services
│   ├── api-location.R, api-registry.R, etc.
│
├── ribits-*.R        # Main user interface
│   ├── ribits-user.R    # User-facing functions
│   ├── ribits-engine.R  # Internal coordination
│   ├── ribits-internal.R
│   └── ribits-methods.R
│
├── transactions-*.R     # Transaction data pipeline
│   ├── transactions-fetch.R      # Fetch from sources
│   ├── transactions-harmonize.R  # Harmonization logic
│   └── transactions-utils.R      # Validation, printing
│
├── harmonization-*.R    # Data harmonization modules
│   ├── harmonization-config.R   # Configuration
│   ├── harmonization-compare.R  # Conflict detection
│   ├── harmonization-resolve.R  # Conflict resolution
│   ├── harmonization-merge.R    # Merging logic
│   ├── harmonization-handling.R # Special cases (dates, etc.)
│   └── harmonization-report.R   # Reporting
│
├── data-*.R            # Data quality and diagnostics
│   ├── data-diagnostics.R
│   ├── diagnostics-coverage.R
│   ├── diagnostics-ledger.R
│   ├── diagnostics-main.R
│   └── diagnostics-sources.R
│
├── network-*.R         # Network operations
│   ├── network.R       # Core HTTP functions
│   ├── network-batch.R # Batch operations
│   ├── network-retry.R # Retry logic
│   └── network-config.R
│
├── utils-*.R           # Utilities by category
│   ├── utils-columns.R
│   ├── utils-join.R
│   ├── utils-messages.R
│   └── utils-globals.R
│
├── config.R            # Main configuration
├── constants.R         # Package constants
├── errors.R            # Error handling
└── parsers.R           # Data parsing utilities
```

## Build & Testing Commands

### Development Workflow
```r
# Quick reload during development
devtools::load_all()

# Run tests
devtools::test()

# Run tests for specific file
testthat::test_file("tests/testthat/test-transactions.R")

# Interactive debugging - add browser() to function, then:
devtools::load_all()
my_function()  # Will pause at browser() for inspection
```

### Package Build & Check
```bash
# Build package
R CMD build . --keep-empty-dirs

# Install locally
R CMD INSTALL . --no-multiarch

# Run CRAN checks
R CMD check . --as-cran

# Quick check without examples/vignettes
R CMD check . --no-examples --no-vignettes --no-manual
```

### Installation from Source
```r
# Install from local source
devtools::install()

# Install with vignettes
devtools::install(build_vignettes = TRUE)

# Install from GitHub (for testing)
pak::pak("alexanderdhond/RIBITSr")
```

## Code Style & Standards

### R Package Conventions

#### Documentation with roxygen2
- Use `#'` comments above all exported functions
- Required tags: `@description`, `@param`, `@return`, `@export` (if public)
- Recommended: `@examples`, `@details`, `@seealso`
- Use markdown syntax (enabled in DESCRIPTION: `Roxygen: list(markdown = TRUE)`)

**Example:**
```r
#' Fetch RIBITS data for specific states
#'
#' @description
#' Main interface to download mitigation banking data. Downloads all data by
#' default; filter results with tidyverse tools.
#'
#' @param state Character vector of 2-letter state codes (e.g., "CA", "FL")
#' @param type Type of entity: "banks" (default), "ilf", or "umbrellas"
#' @param transactions Transaction detail level: "comprehensive" (default,
#'   ~85 columns), "basic" (~20 columns), or "none"
#' @param spatial Include spatial data (footprints, service areas). Default: TRUE
#'
#' @return A `ribits_data` object with components:
#'   - `banks`: Data frame with bank summaries (1 row per bank)
#'   - `transactions`: Data frame with transaction details
#'   - `geometry`: sf object with spatial features
#'   - `.meta`: Metadata about sources and harmonization
#'
#' @examples
#' \dontrun{
#'   # Get everything for California
#'   ca <- ribits(state = "CA")
#'
#'   # Filter with tidyverse
#'   ca$banks %>%
#'     filter(bank_status == "Approved") %>%
#'     select(bank_id, bank_name, total_acres)
#' }
#'
#' @export
ribits <- function(state = NULL, type = "banks",
                   transactions = "comprehensive", spatial = TRUE) {
  # implementation
}
```

#### Export vs Internal Functions
- Export ~5-10 main user-facing functions (marked with `@export`)
- Keep internal helpers private (no `@export`)
- Use `:::` for internal package functions (e.g., `RIBITSr:::.harmonize_data()`)
- Prefix truly internal functions with `.` (e.g., `.validate_args()`)

#### Dependencies
- Add imports to DESCRIPTION: `usethis::use_package("dplyr")`
- Import specific functions in R files: `#' @importFrom dplyr filter select`
- Never use `library()` or `require()` inside package functions
- Use `::` for explicit namespacing (e.g., `dplyr::filter()`)

### Tidyverse Philosophy & Style

#### Core Principles
1. **"Get everything, filter with tidyverse"** - Download maximum data by default
2. **Embrace the pipe** - Use `|>` or `%>%` for sequential operations
3. **Functional programming** - Use `purrr::map_*` instead of loops
4. **Tidy data** - One observation per row, one variable per column

#### Preferred Patterns
```r
# ✅ GOOD: Tidyverse pipeline
transactions %>%
  filter(!is.na(transaction_date)) %>%
  mutate(year = lubridate::year(transaction_date)) %>%
  group_by(bank_id, year) %>%
  summarize(
    n = n(),
    total_debits = sum(debits, na.rm = TRUE),
    .groups = "drop"
  )

# ❌ BAD: Base R loop
results <- list()
for (i in seq_along(transactions$bank_id)) {
  # ... manual aggregation
}

# ✅ GOOD: purrr for iteration
banks %>%
  split(~district) %>%
  map_df(~summarize_district(.x))

# ❌ BAD: lapply with manual binding
do.call(rbind, lapply(split(banks, banks$district), summarize_district))

# ✅ GOOD: across() for multiple columns
data %>%
  mutate(across(c(debits, credits), ~replace_na(.x, 0)))

# ❌ BAD: Repetitive mutations
data %>%
  mutate(
    debits = replace_na(debits, 0),
    credits = replace_na(credits, 0)
  )
```

#### Style Guide
- **Naming**: `snake_case` for functions and variables
- **Spacing**: Spaces around operators (`x + y`, not `x+y`)
- **Line length**: Max 80 characters (extend to 100 for readability if needed)
- **Indentation**: 2 spaces (no tabs)
- **Assignment**: Use `<-` for assignment, not `=`

### Critical Code Patterns

#### Date/Time Handling (⚠️ Common Pitfall)

**Unix Timestamps from EPA:**
- EPA APIs return Unix timestamps (seconds since Jan 1, 1970 UTC)
- **Always specify timezone explicitly** (`tz = "UTC"`)
- Pre-2000 dates need special handling (some systems use different epochs)

```r
# ✅ CORRECT: Explicit timezone, proper origin
parse_unix_timestamp <- function(ts) {
  if (is.na(ts) || ts <= 0) return(NA_POSIXct_)

  # Handle pre-2000 dates (before 946684800)
  if (ts < 946684800) {
    # Special logic in harmonization-handling.R
    return(handle_pre_2000_timestamp(ts))
  }

  as.POSIXct(ts, origin = "1970-01-01", tz = "UTC")
}

# ❌ WRONG: Missing timezone, wrong origin
as.POSIXct(timestamp)  # Defaults to local timezone - causes bugs!
```

**Date Parsing Best Practices:**
- Use `lubridate::as_datetime()` for consistent parsing
- Use `lubridate::ymd()`, `lubridate::mdy()` for flexible date strings
- Document date format assumptions in comments
- See `R/harmonization-handling.R` for pre-2000 date logic

#### API Response Handling

**EPA APIs:**
- Return JSON; parse with `jsonlite::fromJSON()`
- Can return 1000+ records; handle pagination
- Always check for `NULL` values and empty lists
- Use `httr2` for requests (retry logic, timeouts)

```r
# ✅ GOOD: Safe API response handling
fetch_api_data <- function(endpoint) {
  response <- httr2::request(endpoint) %>%
    httr2::req_timeout(30) %>%
    httr2::req_retry(max_tries = 3) %>%
    httr2::req_perform()

  data <- response %>%
    httr2::resp_body_string() %>%
    jsonlite::fromJSON()

  # Handle NULL/empty
  if (is.null(data) || length(data) == 0) {
    return(tibble::tibble())
  }

  # Handle nested lists
  if (is.list(data) && !is.data.frame(data)) {
    data <- purrr::map_df(data, ~as.data.frame(.x, stringsAsFactors = FALSE))
  }

  tibble::as_tibble(data)
}
```

#### Data Quality & Harmonization

**Three-Way Merge Strategy:**
- RIBITS API (real-time, all statuses)
- EPA ArcGIS (spatial data, approved banks only)
- CSV Reports (official transaction records, maximum detail)

**Harmonization Priority:**
```r
# Default source priority (configure with rb_config())
source_priority <- c("csv", "api", "epa")

# CSV reports are most authoritative for transactions
# API is most current for bank status
# EPA has best spatial data
```

**Handling Discrepancies:**
- Each data source has different column naming conventions
- Document conflicts in `.meta$discrepancies`
- Auto-resolve with priority rules (or flag for manual review)
- See `R/harmonization-*.R` for full logic

```r
# ✅ GOOD: Harmonize early, validate often
raw_data %>%
  harmonize_column_names() %>%
  resolve_conflicts(source_priority = c("csv", "api", "epa")) %>%
  validate_required_columns() %>%
  flag_quality_issues()
```

#### User Experience & Messaging

**CLI Messages:**
- Use `cli` package for all user messages
- Show progress for long operations
- Use clear, concise language
- Emoji allowed only if user explicitly requests

```r
# ✅ GOOD: Informative CLI messages
cli::cli_alert_info("Fetching data for {length(state_codes)} states...")
cli::cli_progress_bar("Downloading banks", total = n_banks)

# Show results summary
cli::cli_alert_success("Downloaded {n_banks} banks, {n_transactions} transactions")

# Warn about issues
if (n_discrepancies > 0) {
  cli::cli_alert_warning(
    "{n_discrepancies} discrepancies found. Run discrepancies(data) to review."
  )
}
```

**Function Defaults:**
- Maximize data by default (comprehensive transactions, all columns, spatial data)
- Make common operations simple (single function: `ribits()`)
- Allow customization through clear parameters
- Return tidy data ready for dplyr/tidyr

## Common Workflows

### Adding a New API Endpoint

1. Create `R/api-new-domain.R`
2. Document with roxygen2 comments
3. Add error handling and retry logic
4. Write tests in `tests/testthat/test-api-new-domain.R`
5. Update NAMESPACE if exporting: `devtools::document()`
6. Add usage example to README

### Harmonizing New Data Source

1. Create `R/harmonization-new-source.R`
2. Define column mappings in configuration
3. Implement conflict resolution rules
4. Use `purrr::map_*` for row-wise operations
5. Use `dplyr::rename_with()` for column standardization
6. Test edge cases (NULLs, duplicates, mismatches)

### Troubleshooting Date Issues

- **Symptoms**: Dates showing as 1970-01-01 or wrong year
- **First check**: `R/harmonization-handling.R` for pre-2000 date logic
- **Common cause**: Missing `tz = "UTC"` in date parsing
- **Diagnostic tool**: `rb_check(data)` to validate date coverage
- **See also**: `R/parsers.R` for date parsing utilities

### Refactoring Base R to Tidyverse

**Before refactoring:**
1. Read existing code fully
2. Understand edge cases and error handling
3. Check for dependencies on specific output format
4. Write tests for current behavior

**Refactoring patterns:**
```r
# Replace loops with map
for (i in seq_along(data)) { ... }
# becomes:
data %>% map_df(~transform_item(.x))

# Replace apply with across
data[, numeric_cols] <- lapply(data[, numeric_cols], round, 2)
# becomes:
data %>% mutate(across(all_of(numeric_cols), ~round(.x, 2)))

# Replace subset with filter
data[data$status == "active" & !is.na(data$value), ]
# becomes:
data %>% filter(status == "active", !is.na(value))
```

### Improving Data Quality

1. Run diagnostics: `rb_check(data)`
2. Review discrepancies: `discrepancies(data)`
3. Check source coverage: `rb_check(state = "CA")`
4. Validate required columns: `.validate_transaction_data()`
5. Document findings in comments or CLAUDE.md

## Testing Strategy

### Test File Organization
```
tests/testthat/
├── test-api-*.R         # API endpoint tests
├── test-harmonize-*.R   # Harmonization logic tests
├── test-ribits.R        # Main function integration tests
├── test-config.R        # Configuration tests
└── test-utils.R         # Utility function tests
```

### Writing Tests
```r
test_that("Transaction date parsing handles pre-2000 dates", {
  # Given: Unix timestamp for 1998-01-01
  ts <- 883612800

  # When: Parsing timestamp
  result <- parse_unix_timestamp(ts)

  # Then: Correct year extracted
  expect_equal(lubridate::year(result), 1998)
  expect_s3_class(result, "POSIXct")
})

test_that("API handles NULL responses gracefully", {
  # Mock NULL response
  result <- fetch_api_data(mock_empty_endpoint)

  # Should return empty tibble, not error
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
```

### Mocking API Calls
- Use `httptest2` for mocking HTTP requests (see DESCRIPTION: Suggests)
- Store mock responses in `tests/testthat/fixtures/`
- Test both success and failure cases

## Environment Variables

Configure package behavior via `.Renviron`:
```bash
RIBITS_MAX_RETRIES=5
RIBITS_RETRY_DELAY=2
RIBITS_TIMEOUT=30
RIBITS_USE_PERSISTENT_CACHE=true
RIBITS_CACHE_MAX_AGE_DAYS=7
RIBITS_VERBOSE=true
```

## Git Workflow

- **Main branch**: `master`
- **Commit style**: `type: description` (e.g., `fix: Resolve date parsing errors`, `refactor: Simplify API`)
- **Breaking changes**: Document in NEWS.md
- **File changes**: Keep commits focused (single feature or fix)

## Key References

- **Package overview**: See `README.md`
- **Dependencies**: See `DESCRIPTION`
- **Main API**: See `R/ribits-user.R`
- **Configuration**: See `R/config.R`
- **Harmonization**: See `R/harmonization-*.R`
- **Testing patterns**: See `tests/testthat/`

## Skills Available

Claude has access to specialized skills for this project:
- `/skills` - List available skills
- Use skills for: roxygen2 documentation, tidyverse refactoring, datetime handling, data quality checks, user experience improvements

## Recent Refactoring Notes

- **2025-12-27**: Split `harmonize-transactions.R` (644 lines) into 3 focused modules:
  - `transactions-fetch.R` (~214 lines)
  - `transactions-harmonize.R` (~265 lines)
  - `transactions-utils.R` (~165 lines)
- Aim: Keep files under 300 lines for maintainability

## Package Philosophy Reminder

**"Get everything, filter with tidyverse"**

Don't add complex filtering options to `ribits()`. Instead:
1. Download maximum data by default
2. Let users filter with `dplyr::filter()`, `dplyr::select()`, etc.
3. Keep the API simple and predictable
4. Trust users to use tidyverse tools

This makes the package easier to use, test, and maintain.
