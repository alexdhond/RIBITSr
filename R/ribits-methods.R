# R/ribits-methods.R
# S3 methods for ribits_data objects
# Provides print, plot, dplyr verbs, and other convenient interfaces

#' Print method for ribits_data
#'
#' @param x A ribits_data object
#' @param ... Additional arguments passed to print
#' @export
print.ribits_data <- function(x, ...) {
  cli::cli_h1("RIBITS Data")

  # Fetch date
  if (!is.null(x$.meta$fetch_date)) {
    cli::cli_text("Fetched: {.val {x$.meta$fetch_date}}")
  }

  # Query info
  q <- x$.meta$query
  filters <- c()
  if (!is.null(q$state)) filters <- c(filters, paste0("state=", q$state))
  if (!is.null(q$district)) filters <- c(filters, paste0("district=", q$district))
  if (!is.null(q$bank_ids)) filters <- c(filters, paste0(length(q$bank_ids), " bank_ids"))
  if (length(filters) > 0) {
    cli::cli_text("Query: {paste(filters, collapse = ', ')}")
  }

  cli::cli_h2("Data")

  # Banks (now with summaries)
  if (!is.null(x$banks) && nrow(x$banks) > 0) {
    src <- x$.meta$sources$banks %||% "?"
    n_cols <- ncol(x$banks)
    cli::cli_alert_success("banks: {nrow(x$banks)} rows, {n_cols} columns (includes contact/credit summaries) [{src}]")
  } else {
    cli::cli_alert_warning("banks: none")
  }

  # Transactions (unified)
  if (!is.null(x$transactions) && nrow(x$transactions) > 0) {
    src <- x$.meta$sources$transactions %||% "?"
    n_cols <- ncol(x$transactions)
    cli::cli_alert_success("transactions: {nrow(x$transactions)} rows, {n_cols} columns (unified from watershed CSV + API + CSV ledger) [{src}]")
  } else {
    cli::cli_text("transactions: not requested (use include_detailed_transactions=TRUE)")
  }

  # Detailed contacts (optional)
  if (!is.null(x$.contacts) && nrow(x$.contacts) > 0) {
    src <- x$.meta$sources$contacts %||% "?"
    cli::cli_alert_success("detailed contacts: {nrow(x$.contacts)} rows [{src}]")
  } else {
    cli::cli_text("detailed contacts: not requested (use include_detailed_contacts=TRUE)")
  }

  # Geometry (unified)
  if (!is.null(x$geometry) && nrow(x$geometry) > 0) {
    src <- x$.meta$sources$geometry %||% "?"
    cli::cli_alert_success("geometry: {nrow(x$geometry)} features (centroids + footprints + service areas) [{src}]")
  } else {
    cli::cli_alert_warning("geometry: none")
  }

  # Data Quality Report
  n_disc <- nrow(x$.meta$discrepancies)
  n_resolved <- if (!is.null(x$.meta$harmonization_resolutions)) {
    nrow(x$.meta$harmonization_resolutions)
  } else {
    0
  }

  if (n_disc > 0 || n_resolved > 0) {
    cli::cli_h2("Data Quality")

    # Show harmonization results
    if (n_resolved > 0) {
      cli::cli_alert_success("{n_resolved} discrepancies auto-harmonized")

      # Summarize by confidence level
      resolutions <- x$.meta$harmonization_resolutions
      if (!is.null(resolutions) && "confidence" %in% names(resolutions)) {
        by_confidence <- resolutions |>
          dplyr::count(.data$confidence, name = "n") |>
          dplyr::arrange(dplyr::desc(.data$n))

        for (i in seq_len(nrow(by_confidence))) {
          conf <- by_confidence$confidence[i]
          cnt <- by_confidence$n[i]
          icon <- dplyr::case_when(
            conf == "high" ~ "v",
            conf == "medium" ~ "i",
            conf == "low" ~ "!",
            TRUE ~ "*"
          )
          cli::cli_bullets(c("{icon}" = "{conf} confidence: {cnt}"))
        }
      }

      cli::cli_text("Use {.code resolutions(x)} to view auto-harmonization details")
    }

    # Show remaining discrepancies
    if (n_disc > 0) {
      cli::cli_alert_warning("{n_disc} discrepancies need manual review")
      cli::cli_text("Use {.code discrepancies(x)} to view details")
    }
  }

  invisible(x)
}


#' Access discrepancies from ribits_data
#'
#' @param x A ribits_data object
#' @param ... Additional arguments passed to methods
#' @return A tibble of discrepancies
#' @export
discrepancies <- function(x, ...) {
  UseMethod("discrepancies")
}

#' @rdname discrepancies
#' @export
discrepancies.ribits_data <- function(x, ...) {
  disc <- x$.meta$discrepancies

  if (is.null(disc) || nrow(disc) == 0) {
    cli::cli_alert_success("No discrepancies found - all conflicts were auto-harmonized!")
    return(tibble::tibble())
  }

  cli::cli_h2("Data Discrepancies Needing Manual Review")
  cli::cli_text("These discrepancies could not be auto-harmonized:")
  cli::cli_text("")

  for (i in seq_len(min(10, nrow(disc)))) {
    d <- disc[i, ]
    cli::cli_alert_warning(
      "Bank {d$bank_id} {d$field}: {d$value1} ({d$source1}) vs {d$value2} ({d$source2})"
    )
  }

  if (nrow(disc) > 10) {
    cli::cli_text("")
    cli::cli_alert_info("Showing first 10 of {nrow(disc)} discrepancies")
  }

  invisible(disc)
}


#' Access auto-harmonization resolutions from ribits_data
#'
#' @param x A ribits_data object
#' @param ... Additional arguments passed to methods
#' @return A tibble of harmonization resolutions
#' @export
resolutions <- function(x, ...) {
  UseMethod("resolutions")
}

#' @rdname resolutions
#' @export
resolutions.ribits_data <- function(x, ...) {
  res <- x$.meta$harmonization_resolutions

  if (is.null(res) || nrow(res) == 0) {
    cli::cli_alert_info("No auto-harmonizations were performed")
    return(tibble::tibble())
  }

  cli::cli_h2("Auto-Harmonization Resolutions")
  cli::cli_text("These discrepancies were automatically resolved:")
  cli::cli_text("")

  # Group by resolution rule
  by_rule <- res |>
    dplyr::count(.data$resolution_rule, .data$confidence, name = "n") |>
    dplyr::arrange(dplyr::desc(.data$n))

  for (i in seq_len(nrow(by_rule))) {
    rule <- by_rule$resolution_rule[i]
    conf <- by_rule$confidence[i]
    cnt <- by_rule$n[i]

    rule_description <- switch(rule,
      missing_value_backfill = "Missing value backfill",
      negative_value_correction = "Negative value correction",
      future_date_correction = "Future date correction",
      rounding_precision = "Rounding precision",
      date_proximity_earlier = "Date proximity (earlier date)",
      string_normalization = "String normalization",
      substring_match = "Substring match",
      source_priority_csv = "Source priority (CSV)",
      rule
    )

    icon <- dplyr::case_when(
      conf == "high" ~ "v",
      conf == "medium" ~ "i",
      conf == "low" ~ "!",
      TRUE ~ "*"
    )

    cli::cli_bullets(c(
      "{icon}" = "{rule_description}: {cnt} ({conf} confidence)"
    ))
  }

  cli::cli_text("")
  cli::cli_text("View full resolution details:")
  cli::cli_text("")

  # Show first few examples
  for (i in seq_len(min(5, nrow(res)))) {
    r <- res[i, ]
    cli::cli_text(
      "  Bank {r$bank_id} - {r$field}: {r$value1} ({r$source1}) vs {r$value2} ({r$source2}) -> {.val {r$harmonized_value}}"
    )
  }

  if (nrow(res) > 5) {
    cli::cli_text("")
    cli::cli_alert_info("Showing first 5 of {nrow(res)} resolutions")
  }

  invisible(res)
}


#' Subset ribits_data by bank IDs
#'
#' @param x A ribits_data object
#' @param bank_ids Bank IDs to keep
#' @param ... Additional arguments ignored
#' @return Filtered ribits_data object
#' @export
subset.ribits_data <- function(x, bank_ids, ...) {
  result <- x

  if (!is.null(result$banks)) {
    result$banks <- result$banks |> dplyr::filter(.data$bank_id %in% bank_ids)
  }

  if (!is.null(result$ledger)) {
    result$ledger <- result$ledger |> dplyr::filter(.data$bank_id %in% bank_ids)
  }

  if (!is.null(result$footprints)) {
    id_col <- "bank_id"  # Standardized column name
    result$footprints <- result$footprints |>
      dplyr::filter(.data[[id_col]] %in% bank_ids)
  }

  if (!is.null(result$service_areas)) {
    id_col <- "bank_id"  # Standardized column name
    result$service_areas <- result$service_areas |>
      dplyr::filter(.data[[id_col]] %in% bank_ids)
  }

  result
}

# =============================================================================
# S3 Methods for ribits_data - dplyr verbs
# =============================================================================

#' Filter ribits_data objects
#'
#' @description
#' Apply dplyr::filter() to the banks dataframe within a ribits_data object.
#' This allows you to filter banks without accessing `$banks` directly.
#'
#' @param .data A ribits_data object
#' @param ... Filter expressions passed to dplyr::filter()
#'
#' @return A filtered ribits_data object
#' @exportS3Method dplyr::filter
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' ca_banks <- ribits(state = "CA")
#' large_banks <- ca_banks |> filter(total_acres > 100)
#' }
filter.ribits_data <- function(.data, ...) {
  .data$banks <- dplyr::filter(.data$banks, ...)

  # Also filter related data by bank_id if present
  if (!is.null(.data$banks) && nrow(.data$banks) > 0 && "bank_id" %in% names(.data$banks)) {
    remaining_ids <- .data$banks$bank_id

    if (!is.null(.data$transactions) && "bank_id" %in% names(.data$transactions)) {
      .data$transactions <- .data$transactions |>
        dplyr::filter(.data$bank_id %in% remaining_ids)
    }

    if (!is.null(.data$.contacts) && "bank_id" %in% names(.data$.contacts)) {
      .data$.contacts <- .data$.contacts |>
        dplyr::filter(.data$bank_id %in% remaining_ids)
    }

    if (!is.null(.data$geometry) && "bank_id" %in% names(.data$geometry)) {
      .data$geometry <- .data$geometry |>
        dplyr::filter(.data$bank_id %in% remaining_ids)
    }
  }

  .data
}

#' Arrange ribits_data objects
#'
#' @description
#' Apply dplyr::arrange() to the banks dataframe within a ribits_data object.
#'
#' @param .data A ribits_data object
#' @param ... Arrange expressions passed to dplyr::arrange()
#'
#' @return An arranged ribits_data object
#' @exportS3Method dplyr::arrange
#' @importFrom dplyr arrange
#'
#' @examples
#' \dontrun{
#' ca_banks <- ribits(state = "CA")
#' sorted_banks <- ca_banks |> arrange(desc(total_acres))
#' }
arrange.ribits_data <- function(.data, ...) {
  .data$banks <- dplyr::arrange(.data$banks, ...)
  .data
}

#' Select columns from ribits_data objects
#'
#' @description
#' Apply dplyr::select() to the banks dataframe within a ribits_data object.
#'
#' @param .data A ribits_data object
#' @param ... Select expressions passed to dplyr::select()
#'
#' @return A ribits_data object with selected columns
#' @exportS3Method dplyr::select
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#' ca_banks <- ribits(state = "CA")
#' simplified <- ca_banks |> select(bank_id, bank_name, total_acres)
#' }
select.ribits_data <- function(.data, ...) {
  .data$banks <- dplyr::select(.data$banks, ...)
  .data
}

#' Mutate ribits_data objects
#'
#' @description
#' Apply dplyr::mutate() to the banks dataframe within a ribits_data object.
#'
#' @param .data A ribits_data object
#' @param ... Mutate expressions passed to dplyr::mutate()
#'
#' @return A ribits_data object with mutated columns
#' @exportS3Method dplyr::mutate
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' ca_banks <- ribits(state = "CA")
#' with_hectares <- ca_banks |> mutate(total_hectares = total_acres * 0.404686)
#' }
mutate.ribits_data <- function(.data, ...) {
  .data$banks <- dplyr::mutate(.data$banks, ...)
  .data
}

#' Convert ribits_data to a tibble
#'
#' @description
#' Returns the main `banks` (or `ilf`/`umbrellas`) dataframe.
#' This allows `ribits_data` objects to be used directly in tidyverse pipelines
#' that expect a dataframe.
#'
#' @param x A ribits_data object
#' @param ... Additional arguments ignored
#'
#' @return A tibble
#' @export
#' @examples
#' \dontrun{
#' ribits(state = "CA") |> as_tibble()
#' }
as_tibble.ribits_data <- function(x, ...) {
  if (!is.null(x$banks)) {
    return(tibble::as_tibble(x$banks))
  }
  tibble::tibble()
}

#' Plot ribits_data objects
#'
#' @description
#' Provides a quick visual overview of the data.
#' - If spatial data is available, it maps the geometries.
#' - If no spatial data but transactions exist, it plots a credit summary.
#'
#' @param x A ribits_data object
#' @param ... Additional arguments passed to the underlying plot function
#'
#' @return NULL (called for side effects)
#' @export
#' @examples
#' \dontrun{
#' ca <- ribits(state = "CA")
#' plot(ca)
#' }
plot.ribits_data <- function(x, ...) {

  # 1. Try to plot geometry
  if (!is.null(x$geometry) && inherits(x$geometry, "sf") && nrow(x$geometry) > 0) {
    # Check if we have footprints or service areas to plot
    # Prefer footprints, then service areas, then centroids

    has_fp <- "footprint" %in% names(x$geometry) && !all(sf::st_is_empty(x$geometry$footprint))
    has_sa <- "service_area" %in% names(x$geometry) && !all(sf::st_is_empty(x$geometry$service_area))

    if (has_sa) {
      cli::cli_alert_info("Plotting service areas...")
      plot(sf::st_geometry(x$geometry$service_area), col = NA, border = "blue", main = "Bank Service Areas", ...)
      if (has_fp) {
        plot(sf::st_geometry(x$geometry$footprint), col = "red", add = TRUE, ...)
      } else {
        plot(sf::st_geometry(x$geometry), pch = 20, col = "red", add = TRUE, ...)
      }
      legend("bottomright", legend = c("Service Area", "Location"),
             fill = c(NA, "red"), border = c("blue", NA), pch = c(NA, 20))
      return(invisible(NULL))
    }

    if (has_fp) {
      cli::cli_alert_info("Plotting footprints...")
      plot(sf::st_geometry(x$geometry$footprint), col = "blue", border = "black", main = "Bank Footprints", ...)
      return(invisible(NULL))
    }

    # Fallback to centroids
    cli::cli_alert_info("Plotting centroids...")
    plot(sf::st_geometry(x$geometry), pch = 20, col = "blue", main = "Bank Locations", ...)
    return(invisible(NULL))
  }

  # 2. Try to plot transaction summary
  if (!is.null(x$transactions) && nrow(x$transactions) > 0) {
    if ("transaction_date" %in% names(x$transactions) && "credits" %in% names(x$transactions)) {
      cli::cli_alert_info("Plotting transaction history...")

      # Aggregate by year
      df <- x$transactions
      df$year <- as.integer(format(as.Date(df$transaction_date), "%Y"))

      # Base R aggregation to avoid heavy deps in plot method
      agg <- stats::aggregate(credits ~ year, data = df, sum, na.rm = TRUE)

      if (nrow(agg) > 0) {
        barplot(agg$credits, names.arg = agg$year,
                main = "Total Credits Transacted by Year",
                xlab = "Year", ylab = "Credits", col = "steelblue", ...)
        return(invisible(NULL))
      }
    }
  }

  # 3. Nothing to plot
  cli::cli_alert_warning("No spatial data or transaction history to plot.")
  invisible(NULL)
}
