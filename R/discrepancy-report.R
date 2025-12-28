# R/discrepancy-report.R
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
#' @export
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


#' Export discrepancies to CSV for manual review
#'
#' @param data A ribits_data object
#' @param file_path Path to save CSV file
#'
#' @export
rb_export_discrepancies <- function(data, file_path = "ribits_discrepancies.csv") {
  disc <- data$.meta$discrepancies

  if (is.null(disc) || nrow(disc) == 0) {
    cli::cli_alert_info("No discrepancies to export")
    return(invisible(NULL))
  }

  readr::write_csv(disc, file_path)
  cli::cli_alert_success("Exported {nrow(disc)} discrepancies to {.file {file_path}}")

  invisible(disc)
}
