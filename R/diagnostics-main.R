# R/diagnostics-main.R
# Main diagnostic dispatcher and overview functions
# Split from R/data-diagnostics.R (848 lines → focused modules)

#' Diagnose RIBITS data quality and harmonization
#'
#' Comprehensive diagnostic tool for RIBITS data. Can analyze:
#' - Overall data quality from a ribits() result
#' - Ledger-specific harmonization issues (API vs CSV comparison)
#' - Source completeness comparison
#'
#' @param data One of:
#'   - A ribits_data object (from ribits())
#'   - A data frame (interpreted as ledger/transaction data)
#'   - NULL (if providing bank_ids or state to fetch fresh data)
#' @param type Type of diagnostics to run:
#'   - "auto" (default): Auto-detect based on input
#'   - "overview": General data quality summary
#'   - "ledger": Detailed ledger harmonization diagnostics
#'   - "sources": Compare data completeness across sources
#' @param bank_ids Optional vector of bank IDs for ledger diagnostics
#' @param state Optional state filter for ledger diagnostics
#' @param verbose Print detailed output? Default TRUE
#' @return A list with diagnostic metrics (invisibly)
#' @keywords internal
#' @examples
#' \dontrun{
#' # Overall data quality
#' ca <- ribits(state = "CA")
#' rb_diagnose(ca)
#'
#' # Ledger-specific diagnostics
#' rb_diagnose(ca, type = "ledger")
#'
#' # Compare sources
#' rb_diagnose(ca, type = "sources")
#'
#' # Fetch and diagnose specific banks
#' rb_diagnose(bank_ids = c(17, 100), type = "ledger")
#' }
rb_diagnose <- function(data = NULL,
                        type = c("auto", "overview", "ledger", "sources"),
                        bank_ids = NULL,
                        state = NULL,
                        verbose = TRUE) {

  type <- match.arg(type)

  # Auto-detect type based on input
  if (type == "auto") {
    if (inherits(data, "ribits_data")) {
      type <- "overview"
    } else if (is.data.frame(data)) {
      type <- "sources"
    } else if (!is.null(bank_ids) || !is.null(state)) {
      type <- "ledger"
    } else {
      cli::cli_abort("Cannot auto-detect diagnostic type. Please specify 'type' parameter.")
    }
  }

  # Route to appropriate diagnostic
  if (type == "overview") {
    .rb_diagnose_overview(data, verbose)
  } else if (type == "sources") {
    .rb_diagnose_sources(data, verbose)
  } else if (type == "ledger") {
    .rb_diagnose_ledger_comparison(bank_ids, state, data, verbose)
  }
}

#' Internal: Overview diagnostics
#' @keywords internal
#' @noRd
.rb_diagnose_overview <- function(data, verbose = TRUE) {
  if (verbose) cli::cli_h1("RIBITS Data Quality Report")

  results <- list()

  # Banks
  if (!is.null(data$banks) && nrow(data$banks) > 0) {
    if (verbose) cli::cli_h2("Banks")
    if (verbose) cli::cli_alert_info("{nrow(data$banks)} banks")

    # Key field completeness
    key <- c("bank_name", "bank_status", "bank_type", "total_acres", "establishment_date")
    key <- intersect(key, names(data$banks))
    field_completeness <- list()

    for (col in key) {
      non_na <- sum(!is.na(data$banks[[col]]) & data$banks[[col]] != "")
      pct <- round(100 * non_na / nrow(data$banks), 0)
      field_completeness[[col]] <- pct
      if (verbose) cli::cli_alert("{col}: {pct}% complete")
    }

    results$banks <- list(
      n_rows = nrow(data$banks),
      field_completeness = field_completeness
    )
  }

  # Transactions (check both $transactions and $ledger for backwards compat)
  txn_data <- data$transactions %||% data$ledger
  if (!is.null(txn_data) && nrow(txn_data) > 0) {
    if (verbose) cli::cli_h2("Transactions")
    if (verbose) cli::cli_alert_info("{nrow(txn_data)} transactions")

    if ("source" %in% names(txn_data)) {
      src_tbl <- table(txn_data$source)
      if (verbose) {
        for (s in names(src_tbl)) {
          cli::cli_alert("  {s}: {src_tbl[s]}")
        }
      }
      results$transactions <- list(
        n_rows = nrow(txn_data),
        sources = as.list(src_tbl)
      )
    }

    # If source column exists, offer to run detailed source comparison
    if ("source" %in% names(txn_data) && verbose) {
      cli::cli_alert_info("Run rb_diagnose(data, type='sources') for detailed source comparison")
    }
  }

  # Contacts
  if (!is.null(data$contacts) && nrow(data$contacts) > 0) {
    if (verbose) cli::cli_h2("Contacts")
    if (verbose) cli::cli_alert_info("{nrow(data$contacts)} contacts")

    if ("contact_type" %in% names(data$contacts)) {
      type_tbl <- table(data$contacts$contact_type)
      if (verbose) {
        for (t in names(type_tbl)) {
          cli::cli_alert("  {t}: {type_tbl[t]}")
        }
      }
      results$contacts <- list(
        n_rows = nrow(data$contacts),
        types = as.list(type_tbl)
      )
    }
  }

  # Geometry
  if (!is.null(data$geometry) && nrow(data$geometry) > 0) {
    if (verbose) cli::cli_h2("Geometry")
    if (verbose) cli::cli_alert_info("{nrow(data$geometry)} banks with spatial data")

    geom_stats <- list(n_rows = nrow(data$geometry))

    # Check which geometry types are populated
    if ("centroid" %in% names(data$geometry)) {
      has_centroid <- sum(!sf::st_is_empty(data$geometry$centroid))
      if (verbose) cli::cli_alert("  centroids: {has_centroid}/{nrow(data$geometry)}")
      geom_stats$centroids <- has_centroid
    }
    if ("footprint" %in% names(data$geometry)) {
      has_fp <- sum(!sf::st_is_empty(data$geometry$footprint))
      if (verbose) cli::cli_alert("  footprints: {has_fp}/{nrow(data$geometry)}")
      geom_stats$footprints <- has_fp
    }
    if ("service_area" %in% names(data$geometry)) {
      has_sa <- sum(!sf::st_is_empty(data$geometry$service_area))
      if (verbose) cli::cli_alert("  service_areas: {has_sa}/{nrow(data$geometry)}")
      geom_stats$service_areas <- has_sa
    }

    results$geometry <- geom_stats
  }

  # Meta
  if (!is.null(data$.meta)) {
    if (verbose) cli::cli_h2("Metadata")

    meta_info <- list()

    if (!is.null(data$.meta$timing$duration_secs)) {
      duration <- round(data$.meta$timing$duration_secs, 1)
      if (verbose) cli::cli_alert_info("Fetch time: {duration}s")
      meta_info$duration_secs <- duration
    }
    if (!is.null(data$.meta$sources)) {
      sources_used <- paste(names(data$.meta$sources), collapse = ", ")
      if (verbose) cli::cli_alert_info("Sources used: {sources_used}")
      meta_info$sources <- names(data$.meta$sources)
    }

    results$meta <- meta_info
  }

  invisible(results)
}
