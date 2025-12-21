# R/manual_downloads.R
# Helper functions for managing manual RIBITS downloads

#' Setup download directory structure
#'
#' Creates a standardized directory structure for RIBITS manual downloads.
#'
#' @param base_dir Base directory for RIBITS data. Default: "data/ribits_manual"
#' @return Invisibly returns the base directory path
#' @export
#' @examples
#' \dontrun{
#' # Setup directory structure
#' rb_setup_download_dir()
#'
#' # Custom location
#' rb_setup_download_dir("~/Documents/ribits_data")
#' }
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
#' Searches for the most recently modified CSV file in a report directory.
#'
#' @param report_type Character. Type of report. One of: "credit_classification",
#'   "credit_tracking", "potential_credits", "credit_withdrawal",
#'   "service_area_comments", "bank_summary"
#' @param base_dir Base directory for RIBITS data
#' @return Path to most recent file or NULL if none found
#' @export
#' @examples
#' \dontrun{
#' # Find latest credit classification file
#' file <- rb_find_latest_download("credit_classification")
#'
#' # Read the file
#' if (!is.null(file)) {
#'   data <- rb_read_credit_classification(file)
#' }
#' }
rb_find_latest_download <- function(report_type,
                                     base_dir = "data/ribits_manual") {
  valid_types <- c("credit_classification", "credit_tracking",
                   "potential_credits", "credit_withdrawal",
                   "service_area_comments", "bank_summary")

  if (!report_type %in% valid_types) {
    rlang::abort(
      paste("report_type must be one of:",
            paste(valid_types, collapse = ", "))
    )
  }

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

  cli::cli_alert_info(
    "Found: {basename(latest)} ({format(file_info$mtime[which.max(file_info$mtime)], '%Y-%m-%d')})"
  )

  latest
}

#' Process all manual downloads
#'
#' Automatically finds and processes the most recent manual download for each
#' report type.
#'
#' @param base_dir Base directory for RIBITS data
#' @return Named list of processed data frames
#' @export
#' @examples
#' \dontrun{
#' # Process all available downloads
#' data <- rb_process_manual_downloads()
#'
#' # Access specific datasets
#' credit_class <- data$credit_classification
#' credit_tracking <- data$credit_tracking
#' }
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

#' Load RIBITS data with caching
#'
#' Loads RIBITS manual download data from cache if available and recent,
#' otherwise processes fresh downloads.
#'
#' @param max_age Maximum age of cache in days (default: 30)
#' @param cache_dir Directory for cache files
#' @param download_dir Directory for manual downloads
#' @return Named list of processed data frames
#' @export
#' @examples
#' \dontrun{
#' # Load with default 30-day cache
#' data <- rb_load_with_cache()
#'
#' # Force fresh processing with shorter cache
#' data <- rb_load_with_cache(max_age = 7)
#' }
rb_load_with_cache <- function(max_age = 30,
                                cache_dir = "data/ribits_cache",
                                download_dir = "data/ribits_manual") {

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, "ribits_manual_data.rds")

  # Check if cache exists and is recent
  if (file.exists(cache_file)) {
    cache_age <- difftime(Sys.time(),
                          file.info(cache_file)$mtime,
                          units = "days")

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

  # Only save to cache if we got some data
  if (length(data) > 0) {
    saveRDS(data, cache_file)
    cli::cli_alert_success("Saved to cache: {cache_file}")
  } else {
    cli::cli_alert_warning("No data to cache. Download files may be missing.")
  }

  data
}
