# R/data-catalog.R
# Data discovery and catalog functions for RIBITSr

#' Explore RIBITS data
#'
#' Unified function for data discovery. Shows available data, previews sources,
#' and lists columns.
#'
#' @param what What to show:
#'   - "catalog" (default): Overview of all data sources
#'   - "preview": Preview sample data from a source
#'   - "columns": List columns in a data source
#'   - "api", "epa", "csv": Details about specific source
#' @param source For preview/columns: "api", "epa", or a CSV report type
#' @param state Optional state filter
#' @param n Number of preview rows (default 5)
#'
#' @return Varies by what parameter
#' @export
#' @examples
#' \dontrun{
#' # Discover what's available
#' rb_info()  # Shows catalog of all data sources
#'
#' # Before downloading, explore the data
#' rb_info("preview", source = "api", state = "OR")
#'
#' # See what columns you'll get
#' rb_info("columns", source = "api")  # ~67 columns
#'
#' # Most users just do:
#' library(dplyr)
#' ca <- ribits(state = "CA")  # Get everything
#' ca$banks %>% glimpse()       # See what you got
#' }
rb_info <- function(what = "catalog", source = NULL, state = NULL, n = 5) {
  
  if (what == "catalog") {
    return(rb_catalog())
  } else if (what == "preview") {
    if (is.null(source)) cli::cli_abort("source required for preview")
    return(rb_preview(source, state = state, n = n))
  } else if (what == "columns") {
    if (is.null(source)) cli::cli_abort("source required for columns")
    return(rb_columns(source, state = state %||% "OR"))
  } else if (what %in% c("api", "epa", "csv")) {
    return(rb_catalog(source = what, details = TRUE))
  } else {
    cli::cli_abort("Unknown what: {what}. Try 'catalog', 'preview', 'columns', 'api', 'epa', or 'csv'")
  }
}


#' @rdname rb_info
#' @keywords internal
rb_catalog <- function(source = "all", details = FALSE) {
 
 cli::cli_h1("RIBITSr Data Catalog")
 cli::cli_text("Discover what mitigation banking data is available\n")
 
 # ==========================================================================
 # QUICK START
 # ==========================================================================
 if (source == "all") {
   cli::cli_h2("Quick Start")
   cli::cli_code('
# Get all California bank data (recommended starting point)
ca_data <- ribits(state = "CA")

# Access the data
ca_data$banks          # Bank summary (1 row per bank)
ca_data$ledger         # Credit transactions  
ca_data$footprints     # Bank boundary polygons
ca_data$service_areas  # Service area polygons
')
 }
 
 # ==========================================================================
 # API DATA
 # ==========================================================================
 if (source %in% c("all", "api")) {
   cli::cli_h2("RIBITS API")
   cli::cli_text("Real-time data directly from RIBITS database")
   cli::cli_text("{.emph Base URL: https://ribits.ops.usace.army.mil/ords/RI/public/}\n")
   
   api_endpoints <- tibble::tribble(
     ~endpoint, ~description, ~grain, ~example,
     "banks", "Mitigation bank list and details", "1 row/bank", 'rb_get("banks", state = "CA")',
     "ilf_programs", "In-Lieu Fee programs", "1 row/program", 'rb_get("ilf_programs")',
     "umbrella_mbi", "Umbrella instruments", "1 row/umbrella", 'rb_get("umbrella_mbi")'
   )
   
   for (i in seq_len(nrow(api_endpoints))) {
     ep <- api_endpoints[i, ]
     cli::cli_alert_success("{.strong {ep$endpoint}}: {ep$description}")
     cli::cli_text("   Grain: {ep$grain}")
     cli::cli_text("   {.code {ep$example}}")
   }
   
   if (details) {
     cli::cli_h3("API Bank Columns")
     cli::cli_text("Detailed bank data includes:")
     api_cols <- c(
       "bank_id" = "Unique numeric identifier",
       "name" = "Bank name",
       "total_acres" = "Total acreage",
       "bank_status" = "Approved, Pending, Sold-Out, etc.",
       "year_bank_approved" = "Year approved",
       "mitigation_types" = "Wetland, Stream, etc.",
       "service_areas" = "Geographic service areas",
       "ledger" = "Nested credit transaction history"
     )
     for (col in names(api_cols)) {
       cli::cli_li("{.field {col}}: {api_cols[col]}")
     }
   }
 }
 
 # ==========================================================================
 # EPA ARCGIS DATA
 # ==========================================================================
 if (source %in% c("all", "epa")) {
   cli::cli_h2("EPA ArcGIS MapServer")
   cli::cli_text("Spatial data and attributes from EPA's RIBITS mirror")
   cli::cli_text("{.emph Base URL: https://geopub.epa.gov/arcgis/rest/services/NEPAssist/RIBITS/MapServer}\n")
   
   epa_layers <- tibble::tribble(
     ~layer, ~description, ~geometry, ~example,
     "banks", "Bank locations (points) with attributes", "Point", 'rb_epa("banks", state = "CA")',
     "footprints", "Bank boundary polygons", "Polygon", 'rb_epa("footprints", state = "CA")',
     "service_areas", "Service area polygons", "Polygon", 'rb_epa("service_areas", state = "CA")'
   )
   
   for (i in seq_len(nrow(epa_layers))) {
     ly <- epa_layers[i, ]
     cli::cli_alert_success("{.strong {ly$layer}}: {ly$description}")
     cli::cli_text("   Geometry: {ly$geometry}")
     cli::cli_text("   {.code {ly$example}}")
   }
   
   if (details) {
     cli::cli_h3("EPA Bank Columns (22 fields)")
     epa_cols <- c(
       "bank_id" = "Numeric identifier (joins with API)",
       "bank_name" = "Bank name",
       "district" = "USACE district",
       "state_list" = "State(s)",
       "total_acres" = "Total acreage",
       "bank_status" = "Approval status",
       "year_established" = "Year established",
       "permit_number" = "Permit number",
       "geometry" = "Spatial point/polygon"
     )
     for (col in names(epa_cols)) {
       cli::cli_li("{.field {col}}: {epa_cols[col]}")
     }
   }
   
   cli::cli_h3("Check Spatial Availability (without downloading)")
   cli::cli_code('
# See which banks have spatial data before downloading
flags <- rb_spatial_availability(state = "CA
")
# Returns: bank_id, has_centroid, has_footprint, has_service_area
')
 }
 
 # ==========================================================================
 # CSV REPORTS
 # ==========================================================================
 if (source %in% c("all", "csv")) {
   cli::cli_h2("RIBITS CSV Reports")
   cli::cli_text("Bulk download official reports directly from RIBITS\n")
   
   cli::cli_h3("Summary Tables (1 row per entity)")
   cli::cli_text("Safe to merge with API/EPA data:")
   
   summary_reports <- list(
     banks_sites = "Bank name, type, status, state",
     ilf_programs = "ILF program list",
     ilf_summary = "ILF program details with stats",
     umbrellas = "Umbrella instruments"
   )
   
   for (rt in names(summary_reports)) {
     cli::cli_alert_success("{.strong {rt}}: {summary_reports[[rt]]}")
   }
   
   cli::cli_h3("Transaction Tables (multiple rows per entity)")
   cli::cli_text("Keep separate - join when needed:")
   
   transaction_reports <- list(
     credit_classification = "Credits by classification (~2 rows/bank)",
     credit_releases = "Anticipated releases (~3 rows/bank)",
     ledger_transactions = "Credit transactions (~7 rows/bank)",
     transactions_watershed = "Full transactions + geography (71 cols, ~32 rows/bank)",
     public_notices = "Public notice documents (~4 rows/bank)"
   )
   
   for (rt in names(transaction_reports)) {
     cli::cli_alert_warning("{.strong {rt}}: {transaction_reports[[rt]]}")
   }
   
   cli::cli_h3("Download CSV Reports")
   cli::cli_code('
# List all available reports
rb_reports()

# Download and parse a report
file <- rb_download_report("credit_classification")
data <- rb_read(file)

# Download to specific directory
file <- rb_download_report("banks_sites", download_dir = "data/")
')
   
   if (details) {
     cli::cli_h3("CSV Column Details")
     cli::cli_text("Run {.code rb_reports('structure')} for full column info")
   }
 }
 
 # ==========================================================================
 # HARMONIZED DATA
 # ==========================================================================
 if (source == "all") {
   cli::cli_h2("Harmonized Data (Recommended)")
   cli::cli_text("Automatically combines all sources:\n")
   
   cli::cli_code('
# Get everything for a state
data <- ribits(state = "CA")

# What you get:
data$banks          # Merged: API + EPA + CSV (1 row/bank)
data$ledger         # Transactions (many rows/bank)
data$footprints     # Spatial polygons
data$service_areas  # Spatial polygons
data$.meta          # Source info, any discrepancies
')
   
   cli::cli_h3("Filtering Options")
   cli::cli_code('
# By state
ribits(state = "OR")

# By district
ribits(district = "Portland")
 
# By specific bank IDs
ribits(ids = c(17, 100, 345))

# Choose data sources
ribits(state = "CA", sources = c("api", "epa"))  # Skip CSV
ribits(state = "CA", sources = "api")             # API only (fastest)
')
 }
 
 # ==========================================================================
 # NEXT STEPS
 # ==========================================================================
 cli::cli_h2("Learn More")
 cli::cli_bullets(c(
   ">" = '{.code rb_info("api")} - API column details',
   ">" = '{.code rb_info("epa")} - EPA column details',
   ">" = '{.code rb_reports("structure")} - Full CSV report guide',
   ">" = '{.code rb_reports()} - List all CSV report types',
   ">" = '{.code ?ribits} - Main function documentation'
 ))
 
 invisible(NULL)
}


#' @rdname rb_info
#' @keywords internal
rb_preview <- function(source, state = NULL, n = 5) {
  
  cli::cli_h2("Preview: {source}")
  
  data <- tryCatch({
    if (source == "api") {
      rb_get("banks", state = state) |> head(n)
    } else if (source == "epa") {
      rb_epa("banks", state = state) |> 
        sf::st_drop_geometry() |> 
        head(n)
    } else if (source %in% names(CSV_REPORT_REGISTRY)) {
      csv_file <- rb_download_report(source, download_dir = tempdir())
      rb_read(csv_file) |> head(n)
    } else {
      cli::cli_abort("Unknown source: {source}. Try 'api', 'epa', or a CSV report type.")
    }
  }, error = function(e) {
    cli::cli_alert_danger("Preview failed: {e$message}")
    return(NULL)
  })
  
  if (is.null(data)) return(invisible(NULL))
  
  cli::cli_alert_info("Columns ({ncol(data)}):")
  
  # Show columns with types
  col_info <- sapply(data, function(x) {
    type <- class(x)[1]
    if (type == "character") {
      sample <- paste(head(unique(na.omit(x)), 2), collapse = ", ")
      if (nchar(sample) > 30) sample <- paste0(substr(sample, 1, 30), "...")
      paste0("[chr] e.g., ", sample)
    } else if (type %in% c("numeric", "integer")) {
      paste0("[", type, "] range: ", min(x, na.rm = TRUE), " - ", max(x, na.rm = TRUE))
    } else {
      paste0("[", type, "]")
    }
  })
  
  for (col in names(col_info)) {
    cli::cli_li("{.field {col}}: {col_info[col]}")
  }
  
  cli::cli_h3("Sample Data ({n} rows)")
  print(data)
  
  invisible(data)
}


#' @rdname rb_info
#' @keywords internal
rb_columns <- function(source, state = "OR") {
  
  data <- tryCatch({
    if (source == "api") {
      rb_get("banks", state = state) |> head(1)
    } else if (source == "epa") {
      rb_epa("banks", state = state) |> 
        sf::st_drop_geometry() |> 
        head(1)
    } else if (source %in% names(CSV_REPORT_REGISTRY)) {
      csv_file <- rb_download_report(source, download_dir = tempdir())
      rb_read(csv_file) |> head(1)
    } else {
      cli::cli_abort("Unknown source: {source}")
    }
  }, error = function(e) {
    cli::cli_alert_danger("Failed: {e$message}")
    return(NULL)
  })
  
  if (is.null(data)) return(invisible(NULL))
  
  result <- tibble::tibble(
    column = names(data),
    type = sapply(data, function(x) class(x)[1])
  )
  
  cli::cli_h2("Columns in '{source}'")
  cli::cli_alert_info("{nrow(result)} columns:")
  
  for (i in seq_len(nrow(result))) {
    cli::cli_li("{.field {result$column[i]}} [{result$type[i]}]")
  }
  
  invisible(result)
}
