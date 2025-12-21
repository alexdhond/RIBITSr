# CLAUDE.md

Instructions for Claude Code when working on the RIBITSr R package.

## Quick Context

This is an R package for accessing USACE RIBITS wetland mitigation bank data via their REST API. The package provides:
- Functions to list and retrieve banks, ILF programs, and umbrella instruments
- Extractors for nested data (ledger, contacts, geometry)
- Parsers for manually downloaded CSV reports
- Spatial data conversion to sf objects

## Key Files to Read First

1. `AGENTS.md` - Full project standards and API documentation
2. `data-raw/ribits_field_reference.md` - Complete API field inventory
3. `R/api-core.R` - Core request logic (if it exists)
4. `DESCRIPTION` - Package dependencies

## Development Commands

```bash
# Run all tests
Rscript -e "devtools::test()"

# Run tests for specific file
Rscript -e "devtools::test_active_file()" # or
Rscript -e "testthat::test_file('tests/testthat/test-banks.R')"

# Check package (comprehensive)
Rscript -e "devtools::check()"

# Document package (regenerate man/ files)
Rscript -e "devtools::document()"

# Load all functions interactively
Rscript -e "devtools::load_all()"

# Install package locally
Rscript -e "devtools::install()"

# Check code coverage
Rscript -e "covr::package_coverage()"

# Style code
Rscript -e "styler::style_pkg()"

# Build pkgdown site
Rscript -e "pkgdown::build_site()"
```

## Code Patterns

### Making API Requests

```r
#' Core pattern for RIBITS API requests
.rb_request <- function(endpoint, params = list(), email = NULL) {
  base_url <- "https://ribits.ops.usace.army.mil/ords/RI/public/"
  
  # Add email tracking if provided
if (!is.null(email)) {
    params$webconsumer_email <- email
  }
  
  # Build URL with JSON query
url <- paste0(base_url, endpoint, "/")
  
  if (length(params) > 0) {
    query_json <- jsonlite::toJSON(params, auto_unbox = TRUE)
    url <- httr2::url_parse(url)
    url$query <- list(q = query_json)
    url <- httr2::url_build(url)
  }
  
  # Make request with rate limiting
  resp <- httr2::request(url) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_throttle(rate = 1) |>  # 1 request per second
    httr2::req_perform()
  
  # Parse response
  httr2::resp_body_json(resp)
}
```

### Function Template

```r
#' Brief description
#'
#' Longer description of what the function does.
#'
#' @param param1 Type. Description.
#' @param param2 Type. Description. Default X.
#'
#' @return Description of return value.
#'
#' @export
#' @examples
#' \dontrun{
#' result <- rb_function(param1 = "value")
#' }
rb_function_name <- function(param1, param2 = NULL) {
  # Input validation
  if (missing(param1)) {
    rlang::abort("param1 is required", class = "ribits_error")
  }
  
  # User feedback
  cli::cli_alert_info("Processing...")
  
  # Core logic
  result <- do_something(param1, param2)
  
  # Return
  result
}
```

### Extracting Nested Data

```r
#' Extract ledger from bank data
#'
#' @param bank_data List returned by rb_get_bank()
#' @return A tibble of transaction records
#' @export
rb_extract_ledger <- function(bank_data) {
  ledger <- bank_data$LEDGER
  
  if (is.null(ledger) || length(ledger) == 0) {
    cli::cli_alert_warning("No ledger data available")
    return(tibble::tibble())
  }
  
  purrr::map_dfr(ledger, tibble::as_tibble)
}
```

### Converting to Spatial

```r
#' Extract footprint as sf object
#'
#' @param bank_data List returned by rb_get_bank()
#' @return An sf object or NULL if no geometry
#' @export
rb_extract_footprint <- function(bank_data) {
  geojson <- bank_data$BANK_FOOTPRINT
  
  if (is.null(geojson)) {
    return(NULL)
  }
  
  # Parse GeoJSON string
  sf::st_read(geojson, quiet = TRUE)
}
```

## Testing Patterns

### Basic Test Structure

```r
test_that("rb_list_banks returns expected structure", {
  skip_on_cran()
  
  httptest2::with_mock_dir("bank_site_list", {
    result <- rb_list_banks()
    
    expect_s3_class(result, "tbl_df")
    expect_named(result, c("BANK_ID", "BANK_NAME", "BANK_SITE_DATA_WS_URL"))
    expect_type(result$BANK_ID, "integer")
  })
})
```

### Recording Mock Responses

```r
# Run once to record, then comment out
httptest2::start_capturing("tests/testthat/fixtures")
result <- rb_list_banks(state = "CA")
httptest2::stop_capturing()
```

## Common Tasks

### Add a New Function

1. Create/edit R file in `R/`
2. Add roxygen2 documentation
3. Run `devtools::document()`
4. Create test file in `tests/testthat/`
5. Record mock fixtures if needed
6. Run `devtools::test()` and `devtools::check()`

### Fix a Bug

1. Write failing test that demonstrates bug
2. Fix the code
3. Verify test passes
4. Run full test suite
5. Update NEWS.md

### Add a Vignette

```r
usethis::use_vignette("vignette-name")
# Edit vignettes/vignette-name.Rmd
# Build with pkgdown::build_article("vignette-name")
```

## Don't Forget

- [ ] `devtools::document()` after changing roxygen
- [ ] `devtools::test()` before committing
- [ ] `devtools::check()` before PRs
- [ ] Update NEWS.md for user-facing changes
- [ ] Use `\dontrun{}` for API-calling examples
- [ ] Mock all HTTP requests in tests

## API Quick Reference

| Function | Endpoint | Key Params |
|----------|----------|------------|
| `rb_list_banks()` | `bank_site_list/` | kind, district, state |
| `rb_get_bank()` | `bank_site_data/` | bank_id, show_* flags |
| `rb_list_ilf_programs()` | `ilf_program_list/` | Similar filters |
| `rb_get_ilf_program()` | `ilf_program_data/` | program_id, show_* flags |
| `rb_list_umbrellas()` | `umbrella_instrument_list/` | Similar filters |
| `rb_get_umbrella()` | `umbrella_instrument_data/` | umbrella_id |

## When Stuck

1. Check `?function_name` for R documentation
2. Read `data-raw/ribits_field_reference.md` for API details
3. Look at existing similar functions in `R/`
4. Run `devtools::check()` for diagnostic messages
5. Check GitHub issues for known problems
