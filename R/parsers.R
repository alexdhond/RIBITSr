# R/parsers.R
# Unified CSV reading functions for RIBITS reports


#' Validate CSV content (internal)
#'
#' Performs validation checks on CSV files before parsing to catch common issues
#' like HTML error pages saved as CSV, empty files, or corrupted downloads.
#'
#' @param path Path to CSV file
#' @param expected_cols Optional vector of expected column names
#'
#' @return TRUE if valid, aborts with error if invalid
#' @keywords internal
#' @noRd
.validate_csv_content <- function(path, expected_cols = NULL) {
  # Check file size
  info <- file.info(path)

  if (info$size < 100) {
    # File is very small - likely an error page or empty file
    content_preview <- paste(readLines(path, n = 5, warn = FALSE), collapse = "\n")

    # Check if it's an HTML error page
    if (grepl("<html|<body|<!DOCTYPE|<head", content_preview, ignore.case = TRUE)) {
      cli::cli_abort(c(
        "Downloaded file is an HTML error page, not a CSV",
        "x" = "The RIBITS server returned an error instead of data",
        "i" = "Content preview:",
        " " = content_preview,
        "i" = "Try again later or check if the report type is correct"
      ))
    }

    # Check for Oracle APEX error messages
    if (grepl("ORA-\\d{5}|Oracle.*error|APEX.*error", content_preview, ignore.case = TRUE)) {
      cli::cli_abort(c(
        "Downloaded file contains an Oracle database error",
        "x" = "The RIBITS server encountered a database error",
        "i" = "Content preview:",
        " " = content_preview,
        "i" = "The RIBITS server may be experiencing issues"
      ))
    }

    cli::cli_alert_warning("CSV file is suspiciously small ({info$size} bytes)")
    return(FALSE)
  }

  # Check for minimum row count
  first_lines <- readr::read_lines(path, n_max = 10, skip_empty_rows = FALSE)

  if (length(first_lines) < 2) {
    cli::cli_alert_warning("CSV has fewer than 2 lines - may be empty or corrupted")
    return(FALSE)
  }

  # Check header contains expected columns (if provided)
  if (!is.null(expected_cols) && length(expected_cols) > 0) {
    # Find the header line (skip empty lines)
    header_line <- first_lines[nchar(first_lines) > 0][1]

    if (!is.na(header_line)) {
      header_lower <- tolower(header_line)

      missing_cols <- expected_cols[!sapply(expected_cols, function(col) {
        grepl(tolower(col), header_lower, fixed = TRUE)
      })]

      if (length(missing_cols) > 0) {
        cli::cli_alert_warning(
          "CSV missing expected columns: {paste(missing_cols, collapse = ', ')}"
        )
        return(FALSE)
      }
    }
  }

  TRUE
}


#' Read RIBITS CSV Report
#'
#' Unified function to read any RIBITS CSV report. Automatically handles
#' different report formats including the complex "Potential Credits" report.
#'
#' @param path Path to the CSV file.
#' @param type Optional. Report type for special parsing. Usually auto-detected.
#'   Special types: "potential_credits" (requires hierarchical parsing).
#' @param validate Logical. If TRUE (default), performs validation checks on content
#'   (e.g., checking for HTML errors or empty files).
#' @return A tibble.
#' @export
#' @examples
#' \dontrun{
#' # Read any standard report
#' data <- rb_read("path/to/report.csv")
#'
#' # Read potential credits (auto-detected from filename)
#' pot <- rb_read("Potential Credits by Mitigation Type.csv")
#' }
rb_read <- function(path, type = NULL, validate = TRUE) {
  if (!file.exists(path)) {
    rlang::abort(paste("File not found:", path))
  }

  # Validate content before parsing
  if (validate) {
    .validate_csv_content(path)
  }

  # Auto-detect type from filename if not specified
  if (is.null(type)) {
    filename <- tolower(basename(path))
    if (grepl("potential.?credit", filename)) {
      type <- "potential_credits"
    }
  }

  # Special parsing for potential credits
  if (!is.null(type) && type == "potential_credits") {
    return(.rb_read_potential_credits(path))
  }

  # Standard CSV parsing
  .rb_read_generic_csv(path)
}

#' Generic CSV Reader (internal)
#'
#' Handles RIBITS CSV quirks like empty first rows and inconsistent headers.
#' @keywords internal
#' @noRd
.rb_read_generic_csv <- function(path) {
  tryCatch({
    # Read first few lines to detect format
    first_lines <- readr::read_lines(path, n_max = 5)

    # Check if first line is empty or mostly commas (RIBITS quirk)
    # Some CSVs have a "header label row" with mostly empty cells and one label
    # e.g., ",,,,,,Potential,,,,,,,,,,,,,,,,,," in available_credits_huc
    # Or just empty: ",,," in banks_sites
    first_line_content <- gsub(",", "", first_lines[1])
    first_line_empty <- nchar(first_line_content) == 0

    # Also check if first line is a "sparse header" - mostly empty with few values
    # Count commas vs non-comma content ratio
    n_commas <- nchar(first_lines[1]) - nchar(gsub(",", "", first_lines[1]))
    n_content <- nchar(first_line_content)
    first_line_sparse <- n_commas >= 2 && n_content < 20 && n_content > 0

    skip_rows <- 0
    if ((first_line_empty || first_line_sparse) && length(first_lines) > 1) {
      # Check if second line looks like proper headers (has more content than first)
      second_line_content <- gsub(",", "", first_lines[2])
      # Second line should have substantial content (more than first line)
      if (nchar(second_line_content) > nchar(first_line_content) + 10) {
        skip_rows <- 1
      }
    }

    # Read CSV with appropriate skip
    data <- readr::read_csv(path, skip = skip_rows, show_col_types = FALSE)

    # Clean column names immediately to avoid case-insensitive lookups
    if (ncol(data) > 0) {
      data <- janitor::clean_names(data)
    }

    # Try to extract bank_id from name if not present
    if (!"bank_id" %in% names(data) && "name" %in% names(data)) {
      # Many RIBITS names contain the bank ID in parentheses at end
      # e.g., "Some Bank Name (123)" or "Some Bank Name (123) "
      # First try parentheses pattern, then fall back to trailing digits
      data <- data |>
        dplyr::mutate(
          # Try to extract ID from parentheses first (most reliable)
          .id_from_parens = stringr::str_extract(.data$name, "\\(([0-9]+)\\)\\s*$"),
          .id_from_parens = stringr::str_replace_all(.data$.id_from_parens, "[\\(\\)\\s]", ""),
          # Fall back to trailing digits only if no parentheses match
          .id_from_trailing = dplyr::if_else(
            is.na(.data$.id_from_parens),
            stringr::str_extract(.data$name, "[0-9]+$"),
            NA_character_
          ),
          bank_id = as.integer(dplyr::coalesce(.data$.id_from_parens, .data$.id_from_trailing))
        ) |>
        dplyr::select(-dplyr::any_of(c(".id_from_parens", ".id_from_trailing")))
    }

    # Auto-parse date columns
    data <- .auto_parse_date_columns(data)

    data
  }, error = function(e) {
    rlang::abort(paste("Failed to parse CSV:", e$message))
  })
}


#' Auto-parse date columns (internal)
#'
#' Detects and converts character columns that contain dates in common RIBITS formats.
#' Supports MM/DD/YYYY format commonly used in RIBITS CSV exports.
#'
#' @param data A data frame
#' @return The data frame with date columns converted to Date class
#' @keywords internal
#' @noRd
.auto_parse_date_columns <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }


  # Columns likely to contain dates based on name patterns

date_col_patterns <- c(
    "date", "established", "created", "modified", "updated",
    "release", "approval", "effective", "expiration"
  )

  for (col in names(data)) {
    # Skip if already Date or not character
    if (!is.character(data[[col]])) next

    # Check if column name suggests it's a date
    col_lower <- tolower(col)
    is_date_col <- any(sapply(date_col_patterns, function(p) grepl(p, col_lower)))

    if (is_date_col) {
      # Try to parse as date
      parsed <- .try_parse_date(data[[col]])
      if (!is.null(parsed)) {
        data[[col]] <- parsed
      }
    }
  }

  data
}


#' Try to parse a character vector as dates (internal)
#'
#' Attempts multiple date formats commonly found in RIBITS data.
#'
#' @param x Character vector to parse
#' @return Date vector if successful, NULL if parsing fails
#' @keywords internal
#' @noRd
.try_parse_date <- function(x) {
  if (all(is.na(x))) return(as.Date(x))

  # Get non-NA sample for format detection
  sample_vals <- x[!is.na(x)][1:min(10, sum(!is.na(x)))]
  if (length(sample_vals) == 0) return(NULL)

  # Common RIBITS date formats to try
  formats <- c(
    "%m/%d/%Y",    # 12/31/2024 (most common in RIBITS CSVs)
    "%Y-%m-%d",    # 2024-12-31 (ISO format)
    "%m-%d-%Y",    # 12-31-2024
    "%d/%m/%Y",    # 31/12/2024
    "%Y/%m/%d"     # 2024/12/31
  )

  for (fmt in formats) {
    parsed <- suppressWarnings(as.Date(x, format = fmt))

    # Check if parsing was successful (most values parsed)
    success_rate <- sum(!is.na(parsed)) / sum(!is.na(x))

    if (success_rate > 0.9) {
      # Validate parsed dates are reasonable (between 1980 and 2050)
      valid_dates <- parsed[!is.na(parsed)]
      if (length(valid_dates) > 0) {
        min_date <- min(valid_dates)
        max_date <- max(valid_dates)

        if (min_date >= as.Date("1980-01-01") && max_date <= as.Date("2050-12-31")) {
          return(parsed)
        }
      }
    }
  }

  # Parsing failed - return NULL to keep original
  NULL
}

#' Read Potential Credits (internal)
#' @keywords internal
#' @noRd
.rb_read_potential_credits <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste("File not found:", path))
  }

  # Read all lines as text
  raw_lines <- readr::read_lines(path)

  # Define expected values
  resources <- c("Wetland", "Stream", "Species", "NRDA")
  methods <- c("Establishment", "Re-establishment", "Rehabilitation",
               "Preservation", "Enhancement", "Uplands (Buffer)",
               "-Unspecified-")

  # PASS 1: Map hierarchical context
  hierarchy_map <- tibble::tibble(raw_line = raw_lines) |>
    dplyr::mutate(
      # Clean the lines
      clean_line = stringr::str_remove_all(.data$raw_line, '"') |>
        stringr::str_trim(),

      # Detect context rows
      is_district = stringr::str_detect(.data$clean_line, "^USACE District"),
      is_resource = .data$clean_line %in% resources,
      is_method = stringr::str_detect(
        .data$clean_line,
        paste0("^(", paste(methods, collapse = "|"), ")")
      ),

      # Extract context values
      district_val = dplyr::if_else(.data$is_district, .data$clean_line, NA_character_),
      resource_val = dplyr::if_else(.data$is_resource, .data$clean_line, NA_character_),
      method_val = dplyr::if_else(.data$is_method, .data$clean_line, NA_character_)
    ) |>
    # Fill down context
    tidyr::fill(.data$district_val, .direction = "down") |>
    tidyr::fill(.data$resource_val, .direction = "down") |>
    # Reset method when resource changes
    dplyr::mutate(
      method_val = dplyr::if_else(.data$is_resource, NA_character_, .data$method_val)
    ) |>
    tidyr::fill(.data$method_val, .direction = "down") |>
    # Filter to data rows only
    dplyr::filter(
      # Remove lines with no commas (headers/context)
      stringr::str_count(.data$raw_line, ",") > 0,
      # Remove subtotal lines (start with empty field)
      !stringr::str_detect(.data$raw_line, '^"","'),
      # Remove total lines
      !stringr::str_detect(.data$clean_line, "^Total"),
      # Remove column header row
      !stringr::str_detect(.data$clean_line, "^Bank,Credit Classification")
    )

  # PASS 2: Parse as CSV
  parsed_data <- readr::read_csv(
    I(paste(hierarchy_map$raw_line, collapse = "\n")),
    col_names = c("bank_name", "credit_classification", "potential_credits",
                  "init_acres", "init_feet", "junk"),
    col_types = readr::cols(
      bank_name = readr::col_character(),
      credit_classification = readr::col_character(),
      potential_credits = readr::col_number(),
      init_acres = readr::col_number(),
      init_feet = readr::col_number(),
      junk = readr::col_character()
    ),
    trim_ws = TRUE
  )

  # Combine and clean
  final_data <- dplyr::bind_cols(
    hierarchy_map |>
      dplyr::select(district_raw = .data$district_val,
                    resource_type = .data$resource_val,
                    mitigation_method = .data$method_val),
    parsed_data
  ) |>
    dplyr::select(-.data$junk) |>
    # Parse district string
    tidyr::extract(
      col = .data$district_raw,
      into = c("district_name", "reported_bank_count", "reported_status"),
      regex = "^USACE District (.+) Reporting (\\d+) (.+)$",
      convert = TRUE
    ) |>
    dplyr::mutate(
      reported_status = stringr::str_remove(.data$reported_status, " Banks$")
    )

  # Validation
  n_fails <- sum(is.na(final_data$potential_credits))
  if (n_fails > 0) {
    cli::cli_alert_warning("{n_fails} rows failed to parse credits")
  }

  final_data
}
