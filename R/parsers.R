# R/parsers.R
# Unified CSV reading functions for RIBITS reports

#' Read RIBITS CSV Report
#'
#' Unified function to read any RIBITS CSV report. Automatically handles
#' different report formats including the complex "Potential Credits" report.
#'
#' @param path Path to the CSV file.
#' @param type Optional. Report type for special parsing. Usually auto-detected.
#'   Special types: "potential_credits" (requires hierarchical parsing).
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
rb_read <- function(path, type = NULL) {
  if (!file.exists(path)) {
    rlang::abort(paste("File not found:", path))
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
    
    # Check if first line is empty or just commas (RIBITS quirk)
    first_line_empty <- nchar(gsub(",", "", first_lines[1])) == 0
    
    skip_rows <- 0
    if (first_line_empty && length(first_lines) > 1) {
      # Check if second line looks like headers (contains text)
      if (nchar(gsub(",", "", first_lines[2])) > 0) {
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
      # e.g., "Some Bank Name (123)"
      data <- data |>
        dplyr::mutate(
          bank_id = as.integer(stringr::str_extract(.data$name, "\\d+$"))
        )
    }

    data
  }, error = function(e) {
    rlang::abort(paste("Failed to parse CSV:", e$message))
  })
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
