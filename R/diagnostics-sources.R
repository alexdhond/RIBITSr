# R/diagnostics-sources.R
# Source comparison and analysis functions
# Split from R/data-diagnostics.R (848 lines → focused modules)

#' Data Diagnostics for RIBITS Data
#'
#' Functions to analyze data completeness and source quality.
#' @name data-diagnostics
NULL

#' Compare ledger data completeness across sources (Internal)
#'
#' @description
#' This function is internal. Use `rb_diagnose(data, type = "sources")` instead.
#'
#' Compares data completeness across sources (API, CSV, merged) to help
#' understand which source provides more complete data.
#'
#' @param data A ribits result object or ledger data frame
#' @param verbose Print detailed output. Default TRUE.
#' @return A list with completeness metrics (invisibly)
#' @keywords internal
#' @examples
#' \dontrun{
#' # Recommended: Use rb_diagnose() instead
#' wy <- ribits(state = "WY")
#' rb_diagnose(wy, type = "sources")
#'
#' # Internal usage
#' rb_compare_sources(wy)
#' }
rb_compare_sources <- function(data, verbose = TRUE) {
  # Extract ledger if full result object

ledger <- if (is.data.frame(data)) data else data$ledger
  
  if (is.null(ledger) || nrow(ledger) == 0) {
    cli::cli_alert_warning("No ledger data to diagnose")
    return(invisible(NULL))
  }
  
  # Ensure source column exists
if (!("source" %in% names(ledger))) {
    cli::cli_alert_warning("No 'source' column - cannot compare sources")
    return(invisible(NULL))
  }
  
  sources <- unique(ledger$source)
  
  # Key fields to check
  key_fields <- c(
    # Core transaction info
    "transaction_id", "transaction_date", "transaction_type", "credits", "acres",
    # Classification
    "credit_type", "credit_action", "credit_classification", "resource_type",
    # Project info
    "permit", "permittee", "permit_auth_date", "permit_list",
    "parent_project_name", "sub_ledger_project_name", "sub_ledger_id",
    # Flags
    "is_ilf", "is_purchased", "is_transferred", "is_blm_project_program_site",
    # Geographic
    "jurisdiction", "impact_huc", "impact_latitude", "impact_longitude"
  )
  
  # Filter to fields that exist
  key_fields <- intersect(key_fields, names(ledger))
  
  # Calculate completeness by source
  completeness <- list()
  for (src in sources) {
    sub <- ledger[ledger$source == src, , drop = FALSE]
    n <- nrow(sub)
    
    field_stats <- sapply(key_fields, function(col) {
      vals <- sub[[col]]
      non_empty <- sum(!is.na(vals) & vals != "" & vals != "NA")
      round(100 * non_empty / n, 1)
    })
    
    completeness[[src]] <- list(
      n_rows = n,
      field_pct = field_stats,
      avg_completeness = round(mean(field_stats), 1)
    )
  }
  
  # Identify fields unique to merged data
  if ("api+csv" %in% sources && "api" %in% sources) {
    merged <- ledger[ledger$source == "api+csv", , drop = FALSE]
    api_only <- ledger[ledger$source == "api", , drop = FALSE]
    
    # Fields where merged has significantly better coverage
    merged_better <- names(which(
      completeness[["api+csv"]]$field_pct - completeness[["api"]]$field_pct > 20
    ))
    
    # Fields where API-only has better coverage (rare but possible)
    api_better <- names(which(
      completeness[["api"]]$field_pct - completeness[["api+csv"]]$field_pct > 20
    ))
  } else {
    merged_better <- character(0)
    api_better <- character(0)
  }
  
  # Output
  if (verbose) {
    cli::cli_h1("Ledger Data Diagnostics")
    
    cli::cli_h2("Source Summary")
    for (src in sources) {
      cli::cli_alert_info("{src}: {completeness[[src]]$n_rows} transactions, {completeness[[src]]$avg_completeness}% avg field completeness")
    }
    
    cli::cli_h2("Field Completeness by Source")
    
    # Create comparison table
    comp_df <- data.frame(field = key_fields)
    for (src in sources) {
      comp_df[[src]] <- paste0(completeness[[src]]$field_pct, "%")
    }
    print(comp_df, row.names = FALSE)
    
    if (length(merged_better) > 0) {
      cli::cli_h2("Fields Enhanced by CSV Merge")
      cli::cli_alert_success("These fields have >20% better coverage in merged (api+csv) data:")
      cli::cli_ul(merged_better)
    }
    
    if (length(api_better) > 0) {
      cli::cli_h2("Fields Better in API-only")
      cli::cli_alert_info("These fields have better coverage in API-only data:")
      cli::cli_ul(api_better)
    }
    
    cli::cli_h2("Recommendation")
    if ("api+csv" %in% sources) {
      merged_pct <- completeness[["api+csv"]]$avg_completeness
      api_pct <- completeness[["api"]]$avg_completeness
      
      if (merged_pct > api_pct) {
        cli::cli_alert_success(
          "Merging API+CSV increases field completeness by {round(merged_pct - api_pct, 1)}%"
        )
        cli::cli_alert_info("CSV provides: {paste(merged_better, collapse = ', ')}")
      } else {
        cli::cli_alert_info("API data is equally or more complete than merged data")
      }
    }
  }
  
  invisible(list(
    completeness = completeness,
    merged_better_fields = merged_better,
    api_better_fields = api_better,
    n_total = nrow(ledger)
  ))
}
#' Internal: Source comparison diagnostics
#' @keywords internal
#' @noRd
.rb_diagnose_sources <- function(data, verbose = TRUE) {
  # Extract ledger/transactions if full result object
  ledger <- if (is.data.frame(data)) {
    data
  } else if (inherits(data, "ribits_data")) {
    data$transactions %||% data$ledger
  } else {
    NULL
  }

  if (is.null(ledger) || nrow(ledger) == 0) {
    cli::cli_alert_warning("No transaction data to diagnose")
    return(invisible(NULL))
  }

  # Ensure source column exists
  if (!("source" %in% names(ledger))) {
    cli::cli_alert_warning("No 'source' column - cannot compare sources")
    return(invisible(NULL))
  }

  sources <- unique(ledger$source)

  # Key fields to check
  key_fields <- c(
    # Core transaction info
    "transaction_id", "transaction_date", "transaction_type", "credits", "acres",
    # Classification
    "credit_type", "credit_action", "credit_classification", "resource_type",
    # Project info
    "permit", "permittee", "permit_auth_date", "permit_list",
    "parent_project_name", "sub_ledger_project_name", "sub_ledger_id",
    # Flags
    "is_ilf", "is_purchased", "is_transferred", "is_blm_project_program_site",
    # Geographic
    "jurisdiction", "impact_huc", "impact_latitude", "impact_longitude"
  )

  # Filter to fields that exist
  key_fields <- intersect(key_fields, names(ledger))

  # Calculate completeness by source
  completeness <- list()
  for (src in sources) {
    sub <- ledger[ledger$source == src, , drop = FALSE]
    n <- nrow(sub)

    field_stats <- sapply(key_fields, function(col) {
      vals <- sub[[col]]
      non_empty <- sum(!is.na(vals) & vals != "" & vals != "NA")
      round(100 * non_empty / n, 1)
    })

    completeness[[src]] <- list(
      n_rows = n,
      field_pct = field_stats,
      avg_completeness = round(mean(field_stats), 1)
    )
  }

  # Identify fields unique to merged data
  if ("api+csv" %in% sources && "api" %in% sources) {
    # Fields where merged has significantly better coverage
    merged_better <- names(which(
      completeness[["api+csv"]]$field_pct - completeness[["api"]]$field_pct > 20
    ))

    # Fields where API-only has better coverage (rare but possible)
    api_better <- names(which(
      completeness[["api"]]$field_pct - completeness[["api+csv"]]$field_pct > 20
    ))
  } else {
    merged_better <- character(0)
    api_better <- character(0)
  }

  # Output
  if (verbose) {
    cli::cli_h1("Source Comparison Diagnostics")

    cli::cli_h2("Source Summary")
    for (src in sources) {
      cli::cli_alert_info("{src}: {completeness[[src]]$n_rows} transactions, {completeness[[src]]$avg_completeness}% avg field completeness")
    }

    cli::cli_h2("Field Completeness by Source")

    # Create comparison table
    comp_df <- data.frame(field = key_fields)
    for (src in sources) {
      comp_df[[src]] <- paste0(completeness[[src]]$field_pct, "%")
    }
    print(comp_df, row.names = FALSE)

    if (length(merged_better) > 0) {
      cli::cli_h2("Fields Enhanced by CSV Merge")
      cli::cli_alert_success("These fields have >20% better coverage in merged (api+csv) data:")
      cli::cli_ul(merged_better)
    }

    if (length(api_better) > 0) {
      cli::cli_h2("Fields Better in API-only")
      cli::cli_alert_info("These fields have better coverage in API-only data:")
      cli::cli_ul(api_better)
    }

    cli::cli_h2("Recommendation")
    if ("api+csv" %in% sources) {
      merged_pct <- completeness[["api+csv"]]$avg_completeness
      api_pct <- completeness[["api"]]$avg_completeness

      if (merged_pct > api_pct) {
        cli::cli_alert_success(
          "Merging API+CSV increases field completeness by {round(merged_pct - api_pct, 1)}%"
        )
        if (length(merged_better) > 0) {
          cli::cli_alert_info("CSV provides: {paste(merged_better, collapse = ', ')}")
        }
      } else {
        cli::cli_alert_info("API data is equally or more complete than merged data")
      }
    }
  }

  invisible(list(
    completeness = completeness,
    merged_better_fields = merged_better,
    api_better_fields = api_better,
    n_total = nrow(ledger)
  ))
}
