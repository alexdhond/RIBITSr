# R/csv-registry.R
# CSV Report Registry - Documentation of RIBITS CSV report structures

#' CSV Report Registry
#'
#' Defines the structure and purpose of each RIBITS CSV report.
#' This helps ensure we only merge compatible data structures.
#'
#' @format A list with report metadata:
#' \describe{
#'   \item{type}{Report type identifier}
#'   \item{name}{Human-readable name}
#'   \item{page}{Oracle APEX Page ID for download}
#'   \item{grain}{Data granularity: "bank", "program", "transaction", "credit"}
#'   \item{entity}{Primary entity type: "bank", "ilf_program", "umbrella"}
#'   \item{id_col}{Column containing entity identifier}
#'   \item{description}{What this report contains}
#'   \item{merge_safe}{Can this be directly merged with bank summary data?}
#'   \item{columns}{Key columns and their descriptions}
#' }
#' @keywords internal
CSV_REPORT_REGISTRY <- list(
  
  # ==========================================================================
  # SUMMARY TABLES - One row per entity (safe to merge with API/EPA)
  # ==========================================================================
  
  banks_sites = list(
    type = "banks_sites",
    name = "Banks & Sites",
    page = 158,
    grain = "bank",
    entity = "bank",
    id_col = "name",
    description = "One row per mitigation bank. Basic info: name, type, status, state.",
    merge_safe = TRUE,
    key_columns = c(
      "name" = "Bank name (primary identifier)",
      "bank_type" = "Type: Private Commercial, Public Commercial, etc.",
      "bank_status" = "Status: Approved, Pending, Sold-Out, etc.",
      "state_abbrev_list" = "State(s) where bank operates"
    )
  ),
  
  ilf_programs = list(
    type = "ilf_programs",
    name = "ILF Programs",
    page = 47,
    grain = "program",
    entity = "ilf_program",
    id_col = "program_name",
    description = "One row per In-Lieu Fee program. Basic info only.",
    merge_safe = TRUE,
    key_columns = c(
      "program_name" = "Program name (primary identifier)",
      "district_list" = "USACE district(s)",
      "program_status" = "Status: Approved, Pending, etc."
    )
  ),
  
  ilf_summary = list(
    type = "ilf_summary",
    name = "ILF Program Summary",
    page = 209,
    grain = "program",
    entity = "ilf_program",
    id_col = "program_name",
    description = "One row per ILF program with detailed summary stats.",
    merge_safe = TRUE,
    key_columns = c(
      "program_name" = "Program name (primary identifier)",
      "program_status" = "Approval status",
      "num_service_areas" = "Count of service areas",
      "num_ilf_sites" = "Count of ILF sites",
      "credit_types" = "Types of credits offered",
      "last_transaction_date" = "Most recent transaction"
    )
  ),
  
  umbrellas = list(
    type = "umbrellas",
    name = "Umbrella Instruments",
    page = 401,
    grain = "umbrella",
    entity = "umbrella",
    id_col = "umbrella_name",
    description = "One row per umbrella instrument.",
    merge_safe = TRUE,
    key_columns = c(
      "umbrella_name" = "Umbrella name (primary identifier)",
      "district_list" = "USACE district(s)",
      "status" = "Approval status"
    )
  ),
  
  # ==========================================================================
  # TRANSACTION TABLES - Multiple rows per entity (DO NOT merge directly)
  # ==========================================================================
  
  credit_classification = list(
    type = "credit_classification",
    name = "Credit Classification (Approved)",
    page = 206,
    grain = "credit_class",
    entity = "bank",
    id_col = "bank_name",
    description = "Multiple rows per bank - one per credit classification type. Contains available/released/potential credits by classification.",
    merge_safe = FALSE,
    avg_rows_per_entity = 2.2,
    key_columns = c(
      "bank_name" = "Bank name (join key)",
      "jurisdiction" = "Federal, State, etc.",
      "credit_classification" = "Classification name",
      "credit_classification_type" = "Type category",
      "available_credits" = "Currently available credits",
      "released_credits" = "Released credits",
      "potential_credits" = "Potential future credits"
    )
  ),
  
  credit_releases = list(
    type = "credit_releases",
    name = "Credit Releases (Next 5 Years)",
    page = 208,
    grain = "release",
    entity = "bank",
    id_col = "bank_name",
    description = "Multiple rows per bank - anticipated credit releases over next 5 years.",
    merge_safe = FALSE,
    avg_rows_per_entity = 3.2,
    key_columns = c(
      "bank_name" = "Bank name (join key)",
      "credit_classification" = "Classification for release",
      "anticipated_release_date" = "Expected release date",
      "credits" = "Number of credits to be released"
    )
  ),
  
  ledger_transactions = list(
    type = "ledger_transactions",
    name = "Ledger Transactions (Primary)",
    page = 490,
    grain = "transaction",
    entity = "bank",
    id_col = "name",
    description = "Multiple rows per bank - one per credit transaction. Core transaction ledger.",
    merge_safe = FALSE,
    avg_rows_per_entity = 6.6,
    key_columns = c(
      "name" = "Bank/program name (join key)",
      "type" = "Transaction type",
      "credits" = "Number of credits",
      "credit_action" = "Action: Release, Withdrawal, etc.",
      "permit_list" = "Associated permits",
      "permittee" = "Permit holder"
    )
  ),
  
  transactions_watershed = list(
    type = "transactions_watershed",
    name = "Transactions by Watershed",
    page = 491,
    grain = "transaction",
    entity = "bank",
    id_col = "name",
    description = "Most detailed transaction data - 71 columns! Multiple rows per bank with full watershed and geographic context.",
    merge_safe = FALSE,
    avg_rows_per_entity = 32.4,
    has_bank_id = TRUE,
    key_columns = c(
      "name" = "Bank/site name",
      "bank_id" = "Numeric bank ID (useful for joins!)",
      "impact_huc" = "HUC watershed code",
      "credits" = "Transaction credits",
      "transaction_date" = "Date of transaction",
      "permittee" = "Permit holder",
      "is_ilf" = "Is this an ILF transaction?"
    )
  ),
  
  public_notices = list(
    type = "public_notices",
    name = "Public Notices",
    page = 622,
    grain = "notice",
    entity = "bank",
    id_col = "bank_name",
    description = "Multiple rows per bank - one per public notice document.",
    merge_safe = FALSE,
    avg_rows_per_entity = 4.3,
    key_columns = c(
      "bank_name" = "Bank name (join key)",
      "create_date" = "Notice creation date",
      "filename" = "Document filename",
      "description" = "Notice description"
    )
  ),
  
  available_credits_huc = list(
    type = "available_credits_huc",
    name = "Available Credits (HUC8)",
    page = 6,
    grain = "huc",
    entity = "geographic",
    id_col = NA,
    description = "Credits aggregated by HUC8 watershed. Geographic lookup table.",
    merge_safe = FALSE,
    key_columns = c(
      "HUC columns" = "Watershed identifiers",
      "Credit columns" = "Available credits by type"
    )
  ),
  
  nrda_projects = list(
    type = "nrda_projects",
    name = "NRDA Projects",
    page = 620,
    grain = "project",
    entity = "nrda_project",
    id_col = "project_name",
    description = "Natural Resource Damage Assessment projects.",
    merge_safe = TRUE,
    key_columns = c("project_name", "trustees", "status")
  ),

  blm_projects = list(
    type = "blm_projects",
    name = "BLM Projects & Programs",
    page = 701,
    grain = "project",
    entity = "blm_project",
    id_col = "project_name",
    description = "Bureau of Land Management projects.",
    merge_safe = TRUE,
    key_columns = c("project_name", "status")
  )
)




#' Check if a CSV report is safe to merge with summary data
#'
#' @param report_type Character. The report type to check.
#' @return Logical. TRUE if safe to merge, FALSE otherwise.
#' @keywords internal
.is_merge_safe <- function(report_type) {
  if (!report_type %in% names(CSV_REPORT_REGISTRY)) {
    return(FALSE)
  }
  isTRUE(CSV_REPORT_REGISTRY[[report_type]]$merge_safe)
}