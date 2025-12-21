# RIBITS Data Automation Guide

This guide provides strategies for automating or streamlining the acquisition of RIBITS data that is not available through the API.

## Data Availability Matrix

| Data Type | API Available | Manual Download Required | Automation Possible |
|-----------|---------------|-------------------------|---------------------|
| Bank list & details | ✅ | ❌ | N/A (use API) |
| Basic ledger | ✅ | ❌ | N/A (use API) |
| Contact information | ✅ | ❌ | N/A (use API) |
| Spatial data (footprints/service areas) | ✅ | ❌ | N/A (use API) |
| Credit classification by jurisdiction | ❌ | ✅ | ⚠️ Limited |
| Potential credits by mitigation type | ❌ | ✅ | ⚠️ Limited |
| Detailed credit tracking (permittee info) | ❌ | ✅ | ⚠️ Limited |
| Credit withdrawal details | ❌ | ✅ | ⚠️ Limited |
| Service area comments | ❌ | ✅ | ⚠️ Limited |

---

## Automation Strategies

### 1. Web Scraping with rvest/RSelenium

**Pros:**
- Can automate report generation and download
- Works for dynamic content
- Can handle authentication if needed

**Cons:**
- Fragile (breaks when website changes)
- May violate terms of service
- Slower than API
- Requires maintenance

**Implementation approach:**

```r
library(RSelenium)
library(rvest)

#' Download RIBITS report using RSelenium
#'
#' @param report_type Character. Type of report to download.
#' @param output_dir Directory to save downloaded file.
#' @export
rb_download_report <- function(report_type = c("credit_classification",
                                                "credit_tracking",
                                                "potential_credits",
                                                "credit_withdrawal",
                                                "service_area_comments"),
                                output_dir = tempdir()) {
  report_type <- match.arg(report_type)

  # Start Selenium server
  driver <- rsDriver(browser = "firefox", port = 4567L)
  remDr <- driver[["client"]]

  # Navigate to RIBITS
  remDr$navigate("https://ribits.ops.usace.army.mil/ords/f?p=107:1")

  # Wait for page load
  Sys.sleep(2)

  # TODO: Navigate to reports section
  # TODO: Select report type
  # TODO: Set filters/parameters
  # TODO: Download report
  # TODO: Wait for download to complete

  # Close browser
  remDr$close()
  driver[["server"]]$stop()

  # Return path to downloaded file
  # (Implementation depends on RIBITS website structure)
}
```

**Status:** ⚠️ Not recommended - website structure may change

---

### 2. Scheduled Manual Downloads with Helpers

**Pros:**
- Simple and reliable
- No maintenance burden
- No terms of service concerns

**Cons:**
- Requires manual effort
- Prone to human error
- Not truly automated

**Implementation approach:**

Create helper functions to:
1. Standardize file naming
2. Validate downloaded files
3. Process files automatically once downloaded

```r
#' Setup download directory structure
#'
#' Creates a standardized directory structure for RIBITS manual downloads.
#'
#' @param base_dir Base directory for RIBITS data
#' @export
rb_setup_download_dir <- function(base_dir = "data/ribits_manual") {
  dirs <- c(
    file.path(base_dir, "credit_classification"),
    file.path(base_dir, "credit_tracking"),
    file.path(base_dir, "potential_credits"),
    file.path(base_dir, "credit_withdrawal"),
    file.path(base_dir, "service_area_comments"),
    file.path(base_dir, "bank_summary")
  )

  purrr::walk(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

  # Create README with instructions
  readme <- "
# RIBITS Manual Download Directory

## Download Instructions

1. Go to https://ribits.ops.usace.army.mil/ords/f?p=107:1
2. Navigate to Reports section
3. Download each report type to its corresponding folder
4. Use naming convention: ReportType_YYYY_MM_DD.csv

## Report Types

- `credit_classification/` - Credit Classification by Jurisdiction
- `credit_tracking/` - Bank and ILF Program Credit Tracking
- `potential_credits/` - Potential Credits by Mitigation Type
- `credit_withdrawal/` - Bank and ILF Program Credit Withdrawal
- `service_area_comments/` - Bank & ILF Program Service Area Comments
- `bank_summary/` - Bank Summary

## Processing

After downloading, use:
```r
rb_process_manual_downloads('data/ribits_manual')
```
"

  writeLines(readme, file.path(base_dir, "README.md"))

  cli::cli_alert_success("Created download directory structure at {base_dir}")
  invisible(base_dir)
}

#' Find most recent manual download
#'
#' @param report_type Type of report
#' @param base_dir Base directory for RIBITS data
#' @return Path to most recent file or NULL
#' @export
rb_find_latest_download <- function(report_type, base_dir = "data/ribits_manual") {
  report_dir <- file.path(base_dir, report_type)

  if (!dir.exists(report_dir)) {
    cli::cli_alert_warning("Directory not found: {report_dir}")
    return(NULL)
  }

  files <- list.files(report_dir, pattern = "\\.csv$", full.names = TRUE)

  if (length(files) == 0) {
    cli::cli_alert_info("No CSV files found in {report_dir}")
    return(NULL)
  }

  # Get most recent by modification time
  file_info <- file.info(files)
  latest <- files[which.max(file_info$mtime)]

  cli::cli_alert_info("Found: {basename(latest)} ({format(file_info$mtime[which.max(file_info$mtime)], '%Y-%m-%d')})")

  latest
}

#' Process all manual downloads
#'
#' @param base_dir Base directory for RIBITS data
#' @return List of processed data
#' @export
rb_process_manual_downloads <- function(base_dir = "data/ribits_manual") {

  results <- list()

  # Credit Classification
  file <- rb_find_latest_download("credit_classification", base_dir)
  if (!is.null(file)) {
    results$credit_classification <- rb_read_credit_classification(file)
  }

  # Credit Tracking
  file <- rb_find_latest_download("credit_tracking", base_dir)
  if (!is.null(file)) {
    results$credit_tracking <- rb_read_credit_tracking(file)
  }

  # Potential Credits
  file <- rb_find_latest_download("potential_credits", base_dir)
  if (!is.null(file)) {
    results$potential_credits <- rb_read_potential_credits(file)
  }

  # Credit Withdrawal
  file <- rb_find_latest_download("credit_withdrawal", base_dir)
  if (!is.null(file)) {
    results$credit_withdrawal <- rb_read_credit_withdrawal(file)
  }

  # Service Area Comments
  file <- rb_find_latest_download("service_area_comments", base_dir)
  if (!is.null(file)) {
    results$service_area_comments <- rb_read_service_area_comments(file)
  }

  # Bank Summary
  file <- rb_find_latest_download("bank_summary", base_dir)
  if (!is.null(file)) {
    results$bank_summary <- rb_read_bank_summary(file)
  }

  cli::cli_alert_success("Processed {length(results)} report types")

  invisible(results)
}
```

**Status:** ✅ Recommended approach

---

### 3. Caching Strategy

Since manual downloads don't change frequently, implement a caching system:

```r
#' Load RIBITS data with caching
#'
#' Loads RIBITS data from cache if available and recent, otherwise from
#' manual downloads.
#'
#' @param max_age Maximum age of cache in days (default: 30)
#' @param cache_dir Directory for cache files
#' @param download_dir Directory for manual downloads
#' @export
rb_load_with_cache <- function(max_age = 30,
                                cache_dir = "data/ribits_cache",
                                download_dir = "data/ribits_manual") {

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, "ribits_manual_data.rds")

  # Check if cache exists and is recent
  if (file.exists(cache_file)) {
    cache_age <- difftime(Sys.time(), file.info(cache_file)$mtime, units = "days")

    if (cache_age < max_age) {
      cli::cli_alert_info("Loading from cache (age: {round(cache_age, 1)} days)")
      return(readRDS(cache_file))
    } else {
      cli::cli_alert_warning("Cache expired (age: {round(cache_age, 1)} days)")
    }
  }

  # Load fresh data
  cli::cli_alert_info("Processing manual downloads...")
  data <- rb_process_manual_downloads(download_dir)

  # Save to cache
  saveRDS(data, cache_file)
  cli::cli_alert_success("Saved to cache: {cache_file}")

  data
}
```

---

### 4. USACE Contact for API Access

**Best long-term solution:** Contact USACE to request API endpoints for the missing data.

**Steps:**
1. Identify your use case
2. Contact RIBITS support: ribits@usace.army.mil
3. Reference existing API endpoints as precedent
4. Offer to beta test new endpoints

**Template email:**

```
Subject: Request for Additional RIBITS API Endpoints

Dear RIBITS Team,

I am developing an R package (RIBITSr) to facilitate programmatic access
to RIBITS data for wetland mitigation research. The existing API endpoints
(bank_site_data, ilf_program_data, etc.) have been incredibly useful.

However, several valuable datasets are only available via manual CSV downloads:
- Credit Classification by Jurisdiction
- Potential Credits by Mitigation Type
- Detailed Credit Tracking (with permittee information)
- Credit Withdrawal details
- Service Area Comments

Would it be possible to expose these datasets through API endpoints similar
to the existing ones? This would greatly enhance the reproducibility of
wetland research and reduce manual data management overhead.

I would be happy to beta test any new endpoints and provide feedback.

Thank you for maintaining this valuable resource.

Best regards,
[Your Name]
```

---

## Recommended Workflow

### For Package Users

**Initial Setup:**
```r
# 1. Setup directory structure
rb_setup_download_dir("data/ribits_manual")

# 2. Download reports manually (one-time or periodic)
# Follow instructions in data/ribits_manual/README.md

# 3. Process downloads
manual_data <- rb_load_with_cache(max_age = 30)
```

**Regular Usage:**
```r
# Load data (uses cache if recent)
manual_data <- rb_load_with_cache()

# Access specific datasets
credit_class <- manual_data$credit_classification
credit_tracking <- manual_data$credit_tracking
```

### For Package Development

Consider adding a GitHub Actions workflow to remind about updates:

```yaml
# .github/workflows/data-refresh-reminder.yml
name: Data Refresh Reminder

on:
  schedule:
    # Run on the 1st of every month
    - cron: '0 0 1 * *'
  workflow_dispatch:

jobs:
  remind:
    runs-on: ubuntu-latest
    steps:
      - name: Create Issue
        uses: actions/github-script@v6
        with:
          script: |
            github.rest.issues.create({
              owner: context.repo.owner,
              repo: context.repo.repo,
              title: 'Monthly Reminder: Update RIBITS Manual Downloads',
              body: `It's been a month! Consider updating the manual RIBITS downloads:

              1. Visit https://ribits.ops.usace.army.mil/ords/f?p=107:1
              2. Download updated reports
              3. Update cache files

              See AUTOMATION_GUIDE.md for details.`
            })
```

---

## Future Considerations

1. **Monitor RIBITS API changes**: Periodically check if new endpoints become available
2. **Request bulk download option**: Ask USACE for a single bulk export endpoint
3. **Consider data repository**: Publish processed datasets to Zenodo/Dryad with DOI
4. **Community contribution**: Allow users to share recent downloads

---

## Summary

**Immediate action:** Use the **Scheduled Manual Downloads with Helpers** approach (#2)

**Long-term goal:** Request API endpoints from USACE (#4)

**Not recommended:** Web scraping (#1) - too fragile and potentially violates ToS
