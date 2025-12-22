# R/reports.R
# Direct download functions for RIBITS reports

#' RIBITS Report Registry
#'
#' Mapping of friendly report names to their Oracle APEX Page IDs.
#' @keywords internal
RIBITS_REPORTS <- list(
  # Core Datasets
  banks_sites = list(page = 158, name = "Banks & Sites"),
  ilf_programs = list(page = 47, name = "ILF Programs"),
  umbrellas = list(page = 401, name = "Umbrella Instruments"),
  public_notices = list(page = 622, name = "Public Notices"),
  
  # Credit Data
  credit_classification = list(page = 206, name = "Credit Classification (Approved)"),
  credit_releases = list(page = 208, name = "Credit Releases (Next 5 Years)"),
  available_credits_huc = list(page = 6, name = "Available Credits (HUC8)"),
  
  # Transactions & Ledgers
  ledger_transactions = list(page = 490, name = "Ledger Transactions (Primary)"),
  transactions_watershed = list(page = 491, name = "Transactions by Watershed"),
  
  # Program Specific
  ilf_summary = list(page = 209, name = "ILF Program Summary"),
  nrda_projects = list(page = 620, name = "NRDA Projects"),
  blm_projects = list(page = 701, name = "BLM Projects & Programs")
)

#' Download a standard RIBITS report
#'
#' Downloads a CSV report directly from RIBITS using the Oracle APEX API.
#' This function bypasses the need for a browser or manual navigation.
#'
#' @param report_type Character. The type of report to download. See Details.
#' @param download_dir Directory to save the file. Default "data/ribits_reports".
#' @param filename Optional custom filename. If NULL, generates one based on report name and date.
#' @param reset_filters Logical. If TRUE (default), attempts to reset report filters to default (RIR).
#'
#' @details
#' Available report types:
#' * `banks_sites` - All Banks and Sites
#' * `ilf_programs` - In-Lieu Fee Programs
#' * `umbrellas` - Umbrella Instruments
#' * `public_notices` - Public Notices
#' * `credit_classification` - Credit Classification by Jurisdiction (Approved)
#' * `credit_releases` - Anticipated Credit Releases (Next 5 Years)
#' * `available_credits_huc` - Available Credits by HUC8
#' * `ledger_transactions` - Bank & ILF Program Ledger Transactions
#' * `ilf_summary` - ILF Program Summary
#' * `nrda_projects` - NRDA Projects
#' * `blm_projects` - BLM Projects & Programs
#'
#' @return The path to the downloaded CSV file.
#' @export
#' @examples
#' \dontrun{
#' # Download the Banks & Sites report
#' file <- rb_download_report("banks_sites")
#'
#' # Download Ledger Transactions
#' file <- rb_download_report("ledger_transactions")
#' }
rb_download_report <- function(report_type, 
                               download_dir = "data/ribits_reports",
                               filename = NULL,
                               reset_filters = TRUE) {
  
  # Validate report type
  if (!report_type %in% names(RIBITS_REPORTS)) {
    valid <- paste(names(RIBITS_REPORTS), collapse = ", ")
    cli::cli_abort(c(
      "Unknown report type: {.val {report_type}}",
      "i" = "Valid types are: {valid}"
    ))
  }
  
  config <- RIBITS_REPORTS[[report_type]]
  
  # Setup directory
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  }
  
  # Generate filename if needed
  if (is.null(filename)) {
    # Sanitize name
    safe_name <- gsub("[^a-zA-Z0-9]", "_", config$name)
    safe_name <- gsub("_+", "_", safe_name) # Dedup underscores
    timestamp <- format(Sys.time(), "%Y%m%d")
    filename <- paste0(safe_name, "_", timestamp, ".csv")
  }
  
  path <- file.path(download_dir, filename)
  
  # Construct APEX URL
  # Syntax: f?p=AppID:PageID:Session:Request:Debug:ClearCache:ItemNames:ItemValues
  base_url <- "https://ribits.ops.usace.army.mil/ords/f?p=107"
  
  # Session 0 = Public
  # Request CSV = CSV
  # ClearCache = RIR (Reset Interactive Report) or empty
  cc <- if (reset_filters) "RIR" else ""
  
  url <- paste0(base_url, ":", config$page, ":0:CSV:NO:", cc, "::")
  
  cli::cli_alert_info("Downloading {.strong {config$name}}" )
  # cli::cli_alert_info("URL: {url}")
  
  req <- httr2::request(url) |>
    httr2::req_user_agent("RIBITSr R package") |>
    httr2::req_timeout(300) # Reports can be large/slow
  
  tryCatch({
    httr2::req_perform(req, path = path)
    
    # Check file size to ensure it's not empty/error
    info <- file.info(path)
    if (info$size < 100) {
      # It might be an error page saved as CSV
      content <- paste(readLines(path, n = 5, warn = FALSE), collapse = "\n")
      cli::cli_alert_warning("Downloaded file is very small ({info$size} bytes). Content preview:")
      cli::cli_code(content)
    } else {
      cli::cli_alert_success("Saved to {.path {path}} ({round(info$size / 1024, 1)} KB)")
    }
    
    return(invisible(path))
    
  }, error = function(e) {
    cli::cli_abort("Download failed: {e$message}")
  })
}

#' List available report types
#' 
#' @return A data frame of available report types and descriptions.
#' @export
rb_report_types <- function() {
  tibble::tibble(
    type = names(RIBITS_REPORTS),
    description = sapply(RIBITS_REPORTS, function(x) x$name),
    page_id = sapply(RIBITS_REPORTS, function(x) x$page)
  )
}

#' Get parsed report data
#'
#' Downloads a RIBITS report and immediately parses it into a clean tibble.
#' This is the most convenient way to access bulk data.
#'
#' @param report_type Character. The type of report to retrieve. See `rb_report_types()`.
#' @param cache_dir Directory to cache the downloaded file. Default "data/ribits_cache".
#' @param force Logical. If TRUE, re-downloads even if a recent cache exists.
#' @param max_age_days Numeric. Maximum age of cache in days. Default 1.
#'
#' @return A tibble containing the report data.
#' @keywords internal
#' @examples
#' \dontrun{
#' # Get all banks and sites as a dataframe
#' banks <- rb_get_report_data("banks_sites")
#'
#' # Get ledger transactions
#' ledgers <- rb_get_report_data("ledger_transactions")
#' }
rb_get_report_data <- function(report_type,
                               cache_dir = "data/ribits_cache",
                               force = FALSE,
                               max_age_days = 1) {
  
  # Map report types to parser functions
  parsers <- list(
    banks_sites = rb_read_banks_sites,
    ilf_programs = rb_read_ilf_programs,
    umbrellas = rb_read_umbrellas,
    public_notices = rb_read_public_notices,
    credit_classification = rb_read_credit_classification,
    credit_releases = rb_read_credit_releases,
    available_credits_huc = rb_read_available_credits_huc,
    ledger_transactions = rb_read_credit_tracking, # Mapped to existing parser name
    transactions_watershed = rb_read_transactions_watershed,
    ilf_summary = rb_read_ilf_summary,
    nrda_projects = rb_read_nrda_projects,
    blm_projects = rb_read_blm_projects
  )
  
  if (!report_type %in% names(parsers)) {
    # If it's a valid report type but has no specific parser, use generic
    if (report_type %in% names(RIBITS_REPORTS)) {
      parser <- rb_read_generic_csv
    } else {
      cli::cli_abort("Unknown report type: {.val {report_type}}")
    }
  } else {
    parser <- parsers[[report_type]]
  }
  
  # Check cache
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  # Filename pattern: type_YYYYMMDD.csv
  # We look for the most recent one
  pattern <- paste0("^", report_type, "_\\d{8}\\.csv$")
  files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)
  
  cached_file <- NULL
  if (length(files) > 0) {
    # Sort by time
    info <- file.info(files)
    latest_idx <- which.max(info$mtime)
    latest_file <- files[latest_idx]
    
    age <- as.numeric(difftime(Sys.time(), info$mtime[latest_idx], units = "days"))
    
    if (age < max_age_days && !force) {
      cli::cli_alert_info("Using cached report ({round(age, 2)} days old)")
      cached_file <- latest_file
    }
  }
  
  path <- cached_file
  
  if (is.null(path)) {
    # Download fresh
    # We use a standard name for cache consistency
    timestamp <- format(Sys.time(), "%Y%m%d")
    filename <- paste0(report_type, "_", timestamp, ".csv")
    
    path <- rb_download_report(report_type, download_dir = cache_dir, filename = filename)
  }
  
  # Parse
  cli::cli_alert_info("Parsing data...")
  tryCatch({
    parser(path)
  }, error = function(e) {
    cli::cli_abort("Failed to parse report: {e$message}")
  })
}
