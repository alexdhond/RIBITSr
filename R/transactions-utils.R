# R/transactions-utils.R
# Transaction utilities: print methods and validation
# Split from R/harmonize-transactions.R (644 lines → focused modules)

print.ribits_transactions <- function(x, ...) {
  cli::cli_h1("RIBITS Transaction Data")

  if (!is.null(x$.meta$fetch_date)) {
    cli::cli_text("Fetched: {.val {x$.meta$fetch_date}}")
  }

  cli::cli_h2("Data")

  # Transactions
  if (!is.null(x$transactions) && nrow(x$transactions) > 0) {
    src <- x$.meta$sources$transactions %||% "?"
    n_banks <- length(unique(x$transactions$bank_id))
    n_cols <- ncol(x$transactions)
    cli::cli_alert_success("transactions: {nrow(x$transactions)} rows ({n_banks} banks, {n_cols} columns) [{src}]")
  } else {
    cli::cli_alert_warning("transactions: none")
  }

  cli::cli_h2("Coverage")
  cli::cli_bullets(c(
    "*" = "Banks requested: {x$.meta$coverage$bank_ids_requested}",
    "*" = "With transactions: {x$.meta$coverage$banks_with_transactions}",
    "*" = "Total columns: {x$.meta$coverage$total_columns}"
  ))

  invisible(x)
}


#' Validate transaction data quality
#'
#' Checks for common data quality issues in transaction datasets:
#' - Missing bank IDs
#' - Missing or invalid credit values
#' - Negative credits
#' - Unusually large credit values
#' - Source breakdown
#'
#' @param transactions A tibble of transaction data
#' @return A list with validation results and warnings
#' @keywords internal
#' @noRd
.validate_transaction_data <- function(transactions) {
  if (is.null(transactions) || nrow(transactions) == 0) {
    return(list(valid = TRUE, warnings = character()))
  }

  validation <- list(
    valid = TRUE,
    warnings = character(),
    stats = list()
  )

  # Check for bank_id column
  if (!"bank_id" %in% names(transactions)) {
    cli::cli_alert_danger("CRITICAL: Missing 'bank_id' column")
    validation$valid <- FALSE
    validation$warnings <- c(validation$warnings, "Missing required column: bank_id")
    return(validation)
  }

  # Check for missing bank_ids
  n_missing_bank_id <- sum(is.na(transactions$bank_id))
  if (n_missing_bank_id > 0) {
    pct_missing <- round(100 * n_missing_bank_id / nrow(transactions), 1)
    cli::cli_alert_warning("Missing bank_id in {n_missing_bank_id} rows ({pct_missing}%)")
    validation$warnings <- c(
      validation$warnings,
      glue::glue("{n_missing_bank_id} rows missing bank_id ({pct_missing}%)")
    )
  }

  # Find credit column (could be "credit", "credits", "available_credit", etc.)
  credit_col <- NULL
  possible_credit_cols <- c("credit", "credits", "available_credit", "total_credit")
  for (col in possible_credit_cols) {
    if (col %in% names(transactions)) {
      credit_col <- col
      break
    }
  }

  if (!is.null(credit_col)) {
    credit_values <- transactions[[credit_col]]

    # Convert to numeric if needed
    if (!is.numeric(credit_values)) {
      credit_values <- suppressWarnings(as.numeric(credit_values))
    }

    # Check for missing credits
    n_missing_credit <- sum(is.na(credit_values))
    if (n_missing_credit > 0) {
      pct_missing <- round(100 * n_missing_credit / nrow(transactions), 1)
      cli::cli_alert_warning("Missing {credit_col} in {n_missing_credit} rows ({pct_missing}%)")
      validation$warnings <- c(
        validation$warnings,
        glue::glue("{n_missing_credit} rows missing {credit_col} ({pct_missing}%)")
      )
    }

    # Check for negative credits
    valid_credits <- credit_values[!is.na(credit_values)]
    if (length(valid_credits) > 0) {
      n_negative <- sum(valid_credits < 0)
      if (n_negative > 0) {
        cli::cli_alert_warning("Found {n_negative} negative credit values")
        validation$warnings <- c(
          validation$warnings,
          glue::glue("{n_negative} rows have negative {credit_col}")
        )
        validation$stats$negative_credits <- n_negative
      }

      # Check for unusually large credits (>10,000 is suspicious for most mitigation banks)
      n_large <- sum(valid_credits > 10000)
      if (n_large > 0) {
        max_credit <- max(valid_credits, na.rm = TRUE)
        cli::cli_alert_warning(
          "Found {n_large} unusually large credit values (max: {round(max_credit, 2)})"
        )
        validation$warnings <- c(
          validation$warnings,
          glue::glue("{n_large} rows have {credit_col} > 10,000 (max: {round(max_credit, 2)})")
        )
        validation$stats$large_credits <- n_large
      }

      # Credit statistics
      validation$stats$credit_col <- credit_col
      validation$stats$credit_range <- c(min = min(valid_credits, na.rm = TRUE),
                                          max = max(valid_credits, na.rm = TRUE))
      validation$stats$credit_mean <- mean(valid_credits, na.rm = TRUE)
      validation$stats$credit_median <- median(valid_credits, na.rm = TRUE)
    }
  } else {
    cli::cli_alert_warning("No credit column found (expected: {paste(possible_credit_cols, collapse = ', ')})")
    validation$warnings <- c(validation$warnings, "No credit column found")
  }

  # Source breakdown
  if ("source" %in% names(transactions)) {
    source_counts <- table(transactions$source, useNA = "ifany")
    validation$stats$source_breakdown <- as.list(source_counts)

    cli::cli_h3("Data Source Breakdown")
    for (src in names(source_counts)) {
      src_label <- if (is.na(src)) "unknown" else src
      pct <- round(100 * source_counts[[src]] / nrow(transactions), 1)
      cli::cli_bullets(c(
        "*" = "{src_label}: {source_counts[[src]]} rows ({pct}%)"
      ))
    }
  }

  # Overall validation summary
  if (length(validation$warnings) == 0) {
    cli::cli_alert_success("Data validation: No issues found")
  } else {
    cli::cli_alert_info("Data validation: {length(validation$warnings)} warning(s)")
  }

  validation
}
