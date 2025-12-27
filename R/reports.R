# R/reports.R
# Direct download functions for RIBITS reports

#' Download a standard RIBITS report (Internal)
#'
#' @description
#' This function is internal and called automatically by `ribits()` and `rb_read()`.
#'
#' Downloads a CSV report directly from RIBITS using the Oracle APEX API.
#' This function bypasses the need for a browser or manual navigation.
#'
#' @param report_type Character. The type of report to download.
#' @param download_dir Directory to save the file. If NULL (default), uses a temporary directory.
#' @param filename Optional custom filename. If NULL, generates one based on report name and date.
#' @param reset_filters Logical. If TRUE (default), attempts to reset report filters to default (RIR).
#'
#' @return The path to the downloaded CSV file.
#' @keywords internal
#' @examples
#' \dontrun{
#' # Download the Banks & Sites report
#' file <- rb_download_report("banks_sites")
#'
#' # Download Ledger Transactions
#' file <- rb_download_report("ledger_transactions")
#' }
rb_download_report <- function(report_type, 
                               download_dir = NULL,
                               filename = NULL,
                               reset_filters = TRUE) {
  
  # Validate report type
  registry <- CSV_REPORT_REGISTRY
  if (!report_type %in% names(registry)) {
    valid <- paste(names(registry), collapse = ", ")
    cli::cli_abort(c(
      "Unknown report type: {.val {report_type}}",
      "i" = "Valid types are: {valid}"
    ))
  }
  
  config <- registry[[report_type]]
  
  # Setup directory
  if (is.null(download_dir)) {
    download_dir <- file.path(tempdir(), "ribits_reports")
  }

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
  
  cli::cli_alert_info("Downloading {.strong {config$name}}")

  req <- httr2::request(url) |>
    httr2::req_user_agent("RIBITSr R package")

  # Add progress tracking if verbose mode enabled
  # Note: Progress bar disabled due to conflict with type argument in nested calls
  pb_id <- NULL

  # Use retry wrapper for robust downloads
  # CSV reports can be large and prone to timeout
  resp <- rb_request_with_retry(
    req,
    description = paste("CSV download:", config$name),
    timeout = 300  # 5 minutes for large files
  )

  # Clean up progress bar
  if (!is.null(pb_id)) {
    cli::cli_progress_done(id = pb_id)
  }

  if (is.null(resp)) {
    cli::cli_abort(c(
      "Failed to download {config$name} after {.network_options$max_retries} attempts",
      "i" = "Check rb_network_failures() for details",
      "i" = "The RIBITS server may be temporarily unavailable"
    ))
  }

  # Save response body to file
  tryCatch({
    writeBin(httr2::resp_body_raw(resp), path)

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
    cli::cli_abort("Failed to save file: {e$message}")
  })
}

#' Get information about CSV reports (Internal)
#'
#' @description
#' This function is internal. Use `rb_info()` to explore available data sources.
#'
#' Unified function for exploring available RIBITS CSV reports.
#'
#' @param what What to show:
#'   - "types" (default): List all available report types
#'   - "structure": Show which reports are safe to merge
#'   - A specific report type name: Show detailed info about that report
#' @return A tibble (for "types") or printed info (for "structure" or specific report)
#' @keywords internal
#' @examples
#' \dontrun{
#' # List all report types
#' rb_reports()
#'
#' # See merge-safe vs transaction reports
#' rb_reports("structure")
#'
#' # Get info about a specific report
#' rb_reports("credit_classification")
#' }
rb_reports <- function(what = "types") {
  registry <- CSV_REPORT_REGISTRY
  
 if (what == "types") {
    # List all types
    return(tibble::tibble(
      type = names(registry),
      name = sapply(registry, function(x) x$name),
      grain = sapply(registry, function(x) x$grain),
      merge_safe = sapply(registry, function(x) isTRUE(x$merge_safe))
    ))
  } else if (what == "structure") {
    # Show structure guide (inlined)
    cli::cli_h1("RIBITS CSV Report Structure Guide")
    cli::cli_h2("SUMMARY TABLES (1 row per entity)")
    cli::cli_text("These can be safely merged with API/EPA data by name/ID:")
    for (rt in names(registry)) {
      info <- registry[[rt]]
      if (isTRUE(info$merge_safe)) {
        cli::cli_alert_success("{.strong {info$name}} ({rt})")
        cli::cli_text("   Grain: {info$grain} | ID: {info$id_col}")
      }
    }
    cli::cli_h2("TRANSACTION TABLES (multiple rows per entity)")
    cli::cli_text("Keep these separate - join to summary data when needed:")
    for (rt in names(registry)) {
      info <- registry[[rt]]
      if (!isTRUE(info$merge_safe)) {
        cli::cli_alert_warning("{.strong {info$name}} ({rt})")
        cli::cli_text("   Grain: {info$grain} | ID: {info$id_col}")
      }
    }
    return(invisible(NULL))
  } else if (what %in% names(registry)) {
    # Show specific report info
    info <- registry[[what]]
    cli::cli_h2("{info$name}")
    cli::cli_text("Type: {.val {what}}")
    cli::cli_text("Grain: {.val {info$grain}}")
    cli::cli_text("ID column: {.val {info$id_col}}")
    cli::cli_text("Merge safe: {.val {info$merge_safe}}")
    cli::cli_text("")
    cli::cli_text("{info$description}")
    return(invisible(info))
  } else {
    cli::cli_abort("Unknown report type: {.val {what}}")
  }
}


