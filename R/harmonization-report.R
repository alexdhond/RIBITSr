# R/harmonization-report.R
# User-facing discrepancy reporting and export functions
# Split from R/discrepancy-handling.R (1,052 lines → focused modules)

#' Generate discrepancy report
#'
#' Create a comprehensive report of all data quality issues found across sources.
#'
#' @param data A ribits_data object
#' @param severity_filter Filter by severity: "all", "high", "medium", "low"
#' @param group_by How to group discrepancies: "field", "severity", "bank"
#' @param include_resolved Include resolved discrepancies? Default FALSE.
#'
#' @return A formatted tibble with discrepancy details
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' ca <- ribits(state = "CA")
#'
#' # View all discrepancies
#' rb_discrepancy_report(ca)
#'
#' # Only high-severity issues
#' rb_discrepancy_report(ca, severity_filter = "high")
#'
#' # Group by affected field
#' rb_discrepancy_report(ca, group_by = "field")
#' }
rb_discrepancy_report <- function(data,
                                   severity_filter = c("all", "high", "medium", "low"),
                                   group_by = c("severity", "field", "bank"),
                                   include_resolved = FALSE) {

  severity_filter <- match.arg(severity_filter)
  group_by <- match.arg(group_by)

  # Get discrepancies
  disc <- data$.meta$discrepancies

  if (is.null(disc) || nrow(disc) == 0) {
    cli::cli_alert_success("No discrepancies found!")
    return(invisible(tibble::tibble()))
  }

  # Filter by severity
  if (severity_filter != "all") {
    disc <- disc |> dplyr::filter(.data$severity == severity_filter)
  }

  # Filter resolved if requested
  if (!include_resolved && "resolved_value" %in% names(disc)) {
    disc <- disc |> dplyr::filter(is.na(.data$resolved_value))
  }

  # Print summary
  cli::cli_h2("Discrepancy Report")

  cli::cli_bullets(c(
    "i" = "Total discrepancies: {nrow(disc)}",
    "i" = "Severity breakdown:"
  ))

  severity_summary <- disc |>
    dplyr::group_by(.data$severity) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  for (i in seq_len(nrow(severity_summary))) {
    sev <- severity_summary$severity[i]
    cnt <- severity_summary$count[i]
    icon <- dplyr::case_when(
      sev == "high" ~ "x",
      sev == "medium" ~ "!",
      TRUE ~ "i"
    )
    cli::cli_bullets(c("{icon}" = "{sev}: {cnt}"))
  }

  # Group results
  if (group_by == "field") {
    cli::cli_h3("By Field")
    field_summary <- disc |>
      dplyr::group_by(.data$field) |>
      dplyr::summarise(
        count = dplyr::n(),
        high_severity = sum(.data$severity == "high", na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$count))

    print(field_summary)
  } else if (group_by == "bank") {
    cli::cli_h3("By Bank")
    if ("bank_id" %in% names(disc)) {
      bank_summary <- disc |>
        dplyr::group_by(.data$bank_id) |>
        dplyr::summarise(
          count = dplyr::n(),
          high_severity = sum(.data$severity == "high", na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(.data$count))

      print(bank_summary)
    }
  }

  invisible(disc)
}


#' View discrepancies with side-by-side source comparison
#'
#' @description
#' View side-by-side comparison of all data sources (API, CSV, EPA) for specific
#' banks or fields where there are discrepancies. This helps understand why sources
#' disagree and what the auto-harmonization logic chose.
#'
#' **Note:** For general ledger source comparison (data completeness metrics), use
#' `rb_compare_sources()` from diagnostics-sources.R instead.
#'
#' @param data A ribits_data object
#' @param bank_id Optional bank ID to filter comparison. If NULL, shows all banks
#'   with discrepancies.
#' @param field Optional field name to filter comparison. If NULL, shows all fields
#'   with discrepancies.
#'
#' @return Invisibly returns a tibble with the comparison data
#' @export
#'
#' @examples
#' \dontrun{
#' ca <- ribits(state = "CA")
#'
#' # View discrepancies for a specific bank
#' rb_view_discrepancies(ca, bank_id = "SAC-001")
#'
#' # View discrepancies for a specific field across all banks
#' rb_view_discrepancies(ca, field = "total_credits")
#'
#' # Focused view: one bank, one field
#' rb_view_discrepancies(ca, bank_id = "SAC-001", field = "total_credits")
#' }
rb_view_discrepancies <- function(data, bank_id = NULL, field = NULL) {

  # Get discrepancies and resolutions
  disc <- data$.meta$discrepancies
  resolutions <- data$.meta$harmonization_resolutions

  # Combine both to get full picture
  # Handle empty discrepancies
  if (!is.null(disc) && nrow(disc) > 0) {
    disc_with_flag <- disc |> dplyr::mutate(auto_resolved = FALSE)
  } else {
    disc_with_flag <- tibble::tibble()
  }

  # Handle empty resolutions - check if value1/value2 columns exist
  if (!is.null(resolutions) && nrow(resolutions) > 0) {
    if ("value1" %in% names(resolutions) && "value2" %in% names(resolutions)) {
      res_with_flag <- resolutions |>
        dplyr::rename(
          value1 = .data$value1,
          value2 = .data$value2
        ) |>
        dplyr::mutate(auto_resolved = TRUE)
    } else {
      # Columns don't exist, just add flag
      res_with_flag <- resolutions |> dplyr::mutate(auto_resolved = TRUE)
    }
  } else {
    res_with_flag <- tibble::tibble()
  }

  all_conflicts <- dplyr::bind_rows(disc_with_flag, res_with_flag)

  if (is.null(all_conflicts) || nrow(all_conflicts) == 0) {
    cli::cli_alert_success("No discrepancies found - all sources agree!")
    return(invisible(tibble::tibble()))
  }

  # Filter by bank_id if specified
  if (!is.null(bank_id)) {
    all_conflicts <- all_conflicts |> dplyr::filter(.data$bank_id == !!bank_id)
    if (nrow(all_conflicts) == 0) {
      cli::cli_alert_info("No discrepancies found for bank {bank_id}")
      return(invisible(tibble::tibble()))
    }
  }

  # Filter by field if specified
  if (!is.null(field)) {
    all_conflicts <- all_conflicts |> dplyr::filter(.data$field == !!field)
    if (nrow(all_conflicts) == 0) {
      cli::cli_alert_info("No discrepancies found for field {field}")
      return(invisible(tibble::tibble()))
    }
  }

  # Build three-way comparison
  comparison <- .build_three_way_comparison(all_conflicts)

  # Display results
  .display_source_comparison(comparison, bank_id, field)

  invisible(comparison)
}


#' Build three-way comparison from pairwise discrepancies
#' @keywords internal
.build_three_way_comparison <- function(conflicts) {

  # Group by bank_id and field to merge pairwise comparisons
  unique_conflicts <- conflicts |>
    dplyr::select(.data$bank_id, .data$field) |>
    dplyr::distinct()

  result <- list()

  for (i in seq_len(nrow(unique_conflicts))) {
    bank <- unique_conflicts$bank_id[i]
    fld <- unique_conflicts$field[i]

    # Get all pairwise comparisons for this bank-field
    pairs <- conflicts |>
      dplyr::filter(.data$bank_id == bank, .data$field == fld)

    # Extract values from all sources
    api_val <- NA
    csv_val <- NA
    epa_val <- NA

    for (j in seq_len(nrow(pairs))) {
      pair <- pairs[j, ]

      if (pair$source1 == "api" || pair$source1 == "ribits_api") {
        api_val <- pair$value1
      } else if (pair$source1 == "csv" || pair$source1 == "ribits_csv") {
        csv_val <- pair$value1
      } else if (pair$source1 == "epa" || pair$source1 == "epa_arcgis") {
        epa_val <- pair$value1
      }

      if (pair$source2 == "api" || pair$source2 == "ribits_api") {
        api_val <- pair$value2
      } else if (pair$source2 == "csv" || pair$source2 == "ribits_csv") {
        csv_val <- pair$value2
      } else if (pair$source2 == "epa" || pair$source2 == "epa_arcgis") {
        epa_val <- pair$value2
      }
    }

    # Get harmonization info if available
    harmonized_val <- NA
    harmonized_src <- NA
    resolution_rule <- NA
    confidence <- NA
    auto_resolved <- any(pairs$auto_resolved, na.rm = TRUE)

    if (auto_resolved) {
      res_row <- pairs |> dplyr::filter(.data$auto_resolved == TRUE) |> dplyr::slice(1)
      if (nrow(res_row) > 0) {
        harmonized_val <- res_row$harmonized_value %||% NA
        harmonized_src <- res_row$harmonized_source %||% NA
        resolution_rule <- res_row$resolution_rule %||% NA
        confidence <- res_row$confidence %||% NA
      }
    }

    result[[i]] <- tibble::tibble(
      bank_id = bank,
      field = fld,
      api_value = as.character(api_val),
      csv_value = as.character(csv_val),
      epa_value = as.character(epa_val),
      harmonized_value = as.character(harmonized_val),
      harmonized_source = as.character(harmonized_src),
      resolution_rule = as.character(resolution_rule),
      confidence = as.character(confidence),
      auto_resolved = auto_resolved
    )
  }

  dplyr::bind_rows(result)
}


#' Display source comparison in CLI
#' @keywords internal
.display_source_comparison <- function(comparison, bank_id = NULL, field = NULL) {

  # Header
  if (!is.null(bank_id) && !is.null(field)) {
    cli::cli_h2("Source Comparison: {bank_id} - {field}")
  } else if (!is.null(bank_id)) {
    cli::cli_h2("Source Comparison: {bank_id}")
  } else if (!is.null(field)) {
    cli::cli_h2("Source Comparison: {field}")
  } else {
    cli::cli_h2("Source Comparison")
  }

  cli::cli_text("")

  # Display each conflict
  for (i in seq_len(nrow(comparison))) {
    row <- comparison[i, ]

    # Show bank and field
    cli::cli_h3("{row$bank_id} - {row$field}")

    # Show source values
    cli::cli_text("  {.strong API:}     {row$api_value}")
    cli::cli_text("  {.strong CSV:}     {row$csv_value}")
    cli::cli_text("  {.strong EPA:}     {row$epa_value}")

    # Show harmonization decision
    if (row$auto_resolved && !is.na(row$harmonized_value)) {
      cli::cli_text("")
      cli::cli_alert_success(
        "Auto-resolved: {row$harmonized_value} (from {row$harmonized_source})"
      )
      cli::cli_text("  Rule: {row$resolution_rule}")
      cli::cli_text("  Confidence: {row$confidence}")
    } else {
      cli::cli_text("")
      cli::cli_alert_warning("Not auto-resolved - needs manual review")
    }

    cli::cli_text("")
  }

  # Summary
  n_resolved <- sum(comparison$auto_resolved, na.rm = TRUE)
  n_unresolved <- sum(!comparison$auto_resolved, na.rm = TRUE)

  cli::cli_rule()
  cli::cli_text("Summary: {n_resolved} auto-resolved, {n_unresolved} need manual review")
}


#' Get source metadata and statistics
#'
#' @description
#' Display information about each data source including fetch times, coverage,
#' and known limitations.
#'
#' @param data A ribits_data object
#'
#' @return Invisibly returns a list with source metadata
#' @export
#'
#' @examples
#' \dontrun{
#' ca <- ribits(state = "CA")
#' rb_source_info(ca)
#' }
rb_source_info <- function(data) {

  cli::cli_h1("Data Source Information")
  cli::cli_text("")

  # Overall fetch info
  if (!is.null(data$.meta$fetch_date)) {
    cli::cli_alert_info("Data fetched: {data$.meta$fetch_date}")
  }

  cli::cli_text("")
  cli::cli_h2("Sources Used")

  # Get sources from metadata
  sources_used <- data$.meta$sources

  # RIBITS API
  cli::cli_h3("1. RIBITS API")
  if (!is.null(sources_used$banks) && grepl("api", sources_used$banks, ignore.case = TRUE)) {
    cli::cli_alert_success("Used for this query")
    cli::cli_bullets(c(
      "i" = "{.strong Type:} Real-time REST API",
      "i" = "{.strong Coverage:} All bank statuses (approved, pending, etc.)",
      "i" = "{.strong Strengths:} Most current data, comprehensive ledger",
      "i" = "{.strong Limitations:} May lag in updates, variable completeness",
      "i" = "{.strong Best for:} Current status, transaction ledgers"
    ))
  } else {
    cli::cli_alert_warning("Not used for this query")
  }

  cli::cli_text("")

  # CSV Reports
  cli::cli_h3("2. CSV Reports")
  if (!is.null(sources_used$transactions) && grepl("csv", sources_used$transactions, ignore.case = TRUE)) {
    cli::cli_alert_success("Used for this query")
    cli::cli_bullets(c(
      "i" = "{.strong Type:} Official downloadable reports",
      "i" = "{.strong Coverage:} All transaction details (~85 columns)",
      "i" = "{.strong Strengths:} Most complete/official, maximum detail",
      "i" = "{.strong Limitations:} Potentially stale, manual downloads",
      "i" = "{.strong Best for:} Transaction analysis, official reporting"
    ))
  } else {
    cli::cli_alert_warning("Not used for this query")
  }

  cli::cli_text("")

  # EPA ArcGIS
  cli::cli_h3("3. EPA ArcGIS MapServer")
  if (!is.null(sources_used$geometry) && grepl("epa", sources_used$geometry, ignore.case = TRUE)) {
    cli::cli_alert_success("Used for this query")
    cli::cli_bullets(c(
      "i" = "{.strong Type:} Spatial web service",
      "i" = "{.strong Coverage:} Approved banks only",
      "i" = "{.strong Strengths:} Best spatial data (footprints, service areas)",
      "i" = "{.strong Limitations:} Approved banks only, may lag RIBITS",
      "i" = "{.strong Best for:} Mapping, spatial analysis, GIS integration"
    ))
  } else {
    cli::cli_alert_warning("Not used for this query")
  }

  cli::cli_text("")
  cli::cli_h2("Current Configuration")

  # Get discrepancy config
  config <- .get_discrepancy_config()

  cli::cli_bullets(c(
    "i" = "{.strong Source priority:} {paste(config$source_priority, collapse = ' > ')}",
    "i" = "{.strong Auto-harmonization:} {if(config$auto_harmonize) 'enabled' else 'disabled'}",
    "i" = "{.strong Numeric tolerance:} {config$numeric_tolerance * 100}%",
    "i" = "{.strong Date tolerance:} {config$date_tolerance_days} days"
  ))

  cli::cli_text("")
  cli::cli_text("Use {.code rb_discrepancy_config()} to modify these settings")

  # Return metadata
  invisible(list(
    fetch_date = data$.meta$fetch_date,
    sources_used = sources_used,
    config = config
  ))
}


#' Export discrepancies to CSV or Excel for manual review
#'
#' @param data A ribits_data object
#' @param format Export format: "csv", "excel", or "html"
#' @param file_path Path to save file. If NULL, auto-generates based on format.
#'
#' @return Invisibly returns the discrepancies tibble
#' @export
#'
#' @examples
#' \dontrun{
#' ca <- ribits(state = "CA")
#'
#' # Export to CSV
#' rb_export_discrepancies(ca, format = "csv")
#'
#' # Export to Excel with multiple sheets
#' rb_export_discrepancies(ca, format = "excel")
#'
#' # Export to HTML table
#' rb_export_discrepancies(ca, format = "html", file_path = "discrepancies.html")
#' }
rb_export_discrepancies <- function(data,
                                    format = c("csv", "excel", "html"),
                                    file_path = NULL) {

  format <- match.arg(format)

  disc <- data$.meta$discrepancies
  resolutions <- data$.meta$harmonization_resolutions

  if ((is.null(disc) || nrow(disc) == 0) &&
      (is.null(resolutions) || nrow(resolutions) == 0)) {
    cli::cli_alert_info("No discrepancies to export")
    return(invisible(NULL))
  }

  # Auto-generate file path if not specified
  if (is.null(file_path)) {
    file_path <- switch(format,
      csv = "ribits_discrepancies.csv",
      excel = "ribits_discrepancies.xlsx",
      html = "ribits_discrepancies.html"
    )
  }

  # Export based on format
  if (format == "csv") {
    .export_discrepancies_csv(disc, resolutions, file_path)
  } else if (format == "excel") {
    .export_discrepancies_excel(disc, resolutions, file_path)
  } else if (format == "html") {
    .export_discrepancies_html(disc, resolutions, file_path)
  }

  invisible(disc)
}


#' Export discrepancies to CSV
#' @keywords internal
.export_discrepancies_csv <- function(disc, resolutions, file_path) {

  # Combine unresolved and resolved
  combined <- dplyr::bind_rows(
    disc |> dplyr::mutate(status = "unresolved"),
    resolutions |> dplyr::mutate(status = "auto_resolved")
  )

  if (nrow(combined) == 0) {
    cli::cli_alert_info("No discrepancies to export")
    return(invisible(NULL))
  }

  readr::write_csv(combined, file_path)
  cli::cli_alert_success("Exported {nrow(combined)} discrepancies to {.file {file_path}}")
}


#' Export discrepancies to Excel with multiple sheets
#' @keywords internal
.export_discrepancies_excel <- function(disc, resolutions, file_path) {

  # Check if openxlsx is available
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    cli::cli_alert_warning("Package {.pkg openxlsx} required for Excel export")
    cli::cli_alert_info("Falling back to CSV export")
    .export_discrepancies_csv(disc, resolutions, gsub("\\.xlsx$", ".csv", file_path))
    return(invisible(NULL))
  }

  # Create workbook
  wb <- openxlsx::createWorkbook()

  # Sheet 1: Summary
  summary <- .create_discrepancy_summary(disc, resolutions)
  openxlsx::addWorksheet(wb, "Summary")
  openxlsx::writeData(wb, "Summary", summary)

  # Sheet 2: High severity discrepancies
  if (!is.null(disc) && nrow(disc) > 0) {
    high_sev <- disc |> dplyr::filter(.data$severity == "high")
    if (nrow(high_sev) > 0) {
      openxlsx::addWorksheet(wb, "High Severity")
      openxlsx::writeData(wb, "High Severity", high_sev)

      # Add conditional formatting (red for high severity)
      openxlsx::conditionalFormatting(
        wb, "High Severity",
        cols = 1:ncol(high_sev),
        rows = 2:(nrow(high_sev) + 1),
        rule = "TRUE",
        style = openxlsx::createStyle(bgFill = "#ffcccc")
      )
    }

    # Sheet 3: Medium severity
    med_sev <- disc |> dplyr::filter(.data$severity == "medium")
    if (nrow(med_sev) > 0) {
      openxlsx::addWorksheet(wb, "Medium Severity")
      openxlsx::writeData(wb, "Medium Severity", med_sev)

      openxlsx::conditionalFormatting(
        wb, "Medium Severity",
        cols = 1:ncol(med_sev),
        rows = 2:(nrow(med_sev) + 1),
        rule = "TRUE",
        style = openxlsx::createStyle(bgFill = "#ffffcc")
      )
    }

    # Sheet 4: Low severity
    low_sev <- disc |> dplyr::filter(.data$severity == "low")
    if (nrow(low_sev) > 0) {
      openxlsx::addWorksheet(wb, "Low Severity")
      openxlsx::writeData(wb, "Low Severity", low_sev)
    }
  }

  # Sheet 5: Auto-harmonization resolutions
  if (!is.null(resolutions) && nrow(resolutions) > 0) {
    openxlsx::addWorksheet(wb, "Auto-Resolved")
    openxlsx::writeData(wb, "Auto-Resolved", resolutions)

    # Green highlighting for auto-resolved
    openxlsx::conditionalFormatting(
      wb, "Auto-Resolved",
      cols = 1:ncol(resolutions),
      rows = 2:(nrow(resolutions) + 1),
      rule = "TRUE",
      style = openxlsx::createStyle(bgFill = "#ccffcc")
    )
  }

  # Save workbook
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  n_total <- nrow(disc) + nrow(resolutions)
  cli::cli_alert_success("Exported {n_total} discrepancies to {.file {file_path}}")
}


#' Create summary sheet for Excel export
#' @keywords internal
.create_discrepancy_summary <- function(disc, resolutions) {

  summary_data <- tibble::tibble(
    Metric = character(),
    Value = character()
  )

  # Unresolved discrepancies
  if (!is.null(disc) && nrow(disc) > 0) {
    summary_data <- dplyr::bind_rows(
      summary_data,
      tibble::tibble(Metric = "Unresolved Discrepancies", Value = as.character(nrow(disc)))
    )

    # By severity
    by_sev <- disc |>
      dplyr::count(.data$severity, name = "n") |>
      dplyr::mutate(Metric = paste0("  - ", stringr::str_to_title(.data$severity), " Severity")) |>
      dplyr::select(Metric, Value = .data$n) |>
      dplyr::mutate(Value = as.character(.data$Value))

    summary_data <- dplyr::bind_rows(summary_data, by_sev)

    # By field
    top_fields <- disc |>
      dplyr::count(.data$field, name = "n", sort = TRUE) |>
      dplyr::slice_head(n = 5)

    summary_data <- dplyr::bind_rows(
      summary_data,
      tibble::tibble(Metric = "", Value = ""),
      tibble::tibble(Metric = "Top 5 Fields with Discrepancies", Value = "")
    )

    for (i in seq_len(nrow(top_fields))) {
      summary_data <- dplyr::bind_rows(
        summary_data,
        tibble::tibble(
          Metric = paste0("  ", i, ". ", top_fields$field[i]),
          Value = as.character(top_fields$n[i])
        )
      )
    }
  }

  # Auto-resolved
  if (!is.null(resolutions) && nrow(resolutions) > 0) {
    summary_data <- dplyr::bind_rows(
      summary_data,
      tibble::tibble(Metric = "", Value = ""),
      tibble::tibble(Metric = "Auto-Resolved Discrepancies", Value = as.character(nrow(resolutions)))
    )

    # By confidence
    by_conf <- resolutions |>
      dplyr::count(.data$confidence, name = "n") |>
      dplyr::mutate(Metric = paste0("  - ", stringr::str_to_title(.data$confidence), " Confidence")) |>
      dplyr::select(Metric, Value = .data$n) |>
      dplyr::mutate(Value = as.character(.data$Value))

    summary_data <- dplyr::bind_rows(summary_data, by_conf)
  }

  summary_data
}


#' Export discrepancies to HTML table
#' @keywords internal
.export_discrepancies_html <- function(disc, resolutions, file_path) {

  # Combine unresolved and resolved
  combined <- dplyr::bind_rows(
    disc |> dplyr::mutate(status = "Unresolved"),
    resolutions |>
      dplyr::mutate(status = "Auto-Resolved") |>
      dplyr::select(-dplyr::any_of(c("harmonized_value", "harmonized_source", "resolution_rule", "confidence")))
  )

  if (nrow(combined) == 0) {
    cli::cli_alert_info("No discrepancies to export")
    return(invisible(NULL))
  }

  # Create HTML table
  html_content <- paste0(
    "<!DOCTYPE html>\n",
    "<html>\n",
    "<head>\n",
    "  <title>RIBITS Data Discrepancies</title>\n",
    "  <style>\n",
    "    body { font-family: Arial, sans-serif; margin: 20px; }\n",
    "    h1 { color: #333; }\n",
    "    table { border-collapse: collapse; width: 100%; margin-top: 20px; }\n",
    "    th { background-color: #4CAF50; color: white; padding: 10px; text-align: left; }\n",
    "    td { border: 1px solid #ddd; padding: 8px; }\n",
    "    tr:nth-child(even) { background-color: #f2f2f2; }\n",
    "    .high { background-color: #ffcccc; }\n",
    "    .medium { background-color: #ffffcc; }\n",
    "    .low { background-color: #ccffff; }\n",
    "    .resolved { background-color: #ccffcc; }\n",
    "  </style>\n",
    "</head>\n",
    "<body>\n",
    "  <h1>RIBITS Data Discrepancies</h1>\n",
    "  <p>Generated: ", as.character(Sys.time()), "</p>\n",
    "  <table>\n",
    "    <tr>\n"
  )

  # Add headers
  for (col_name in names(combined)) {
    html_content <- paste0(html_content, "      <th>", col_name, "</th>\n")
  }
  html_content <- paste0(html_content, "    </tr>\n")

  # Add rows
  for (i in seq_len(nrow(combined))) {
    row <- combined[i, ]

    # Determine row class based on severity or status
    row_class <- ""
    if ("severity" %in% names(row) && !is.na(row$severity)) {
      row_class <- row$severity
    } else if (row$status == "Auto-Resolved") {
      row_class <- "resolved"
    }

    html_content <- paste0(html_content, "    <tr class='", row_class, "'>\n")

    for (col in names(combined)) {
      val <- row[[col]]
      val_str <- if (is.na(val)) "" else as.character(val)
      html_content <- paste0(html_content, "      <td>", val_str, "</td>\n")
    }

    html_content <- paste0(html_content, "    </tr>\n")
  }

  html_content <- paste0(
    html_content,
    "  </table>\n",
    "</body>\n",
    "</html>"
  )

  # Write to file
  writeLines(html_content, file_path)
  cli::cli_alert_success("Exported {nrow(combined)} discrepancies to {.file {file_path}}")
}
