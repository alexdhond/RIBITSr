# R/column-registry.R
# Column mapping registry for standardizing column names across data sources

#' Column Registry
#'
#' Centralized mapping of column names across RIBITS data sources (API, EPA, CSV).
#' Maps semantic equivalents and aliases to canonical column names to eliminate
#' duplicates before merging.
#'
#' @keywords internal
#' @noRd
COLUMN_REGISTRY <- list(
  # ===== Primary Keys =====
  bank_id = list(
    canonical = "bank_id",
    aliases = c("BANK_ID", "bankId", "bank_identifier", "id")
  ),

  bank_name = list(
    canonical = "bank_name",
    aliases = c("name", "NAME", "BANK_NAME", "bank_name_1", "bank_name_2", "site_name")
  ),

  # ===== Status & Classification =====
  bank_status = list(
    canonical = "bank_status",
    aliases = c("status", "STATUS", "BANK_STATUS", "bank_status_1", "bank_status_2")
  ),

  bank_type = list(
    canonical = "bank_type",
    aliases = c("kind_of_bank", "KIND_OF_BANK", "BANK_TYPE", "type", "bank_type_1", "bank_type_2")
  ),

  bank_status_date = list(
    canonical = "bank_status_date",
    aliases = c("BANK_STATUS_DATE", "status_date")
  ),

  # ===== Geographic =====
  state_list = list(
    canonical = "state_list",
    aliases = c("state_abbrev_list", "STATE_LIST", "STATE_ABBREV_LIST", "state", "STATE")
  ),

  county_list = list(
    canonical = "county_list",
    aliases = c("COUNTY_LIST", "county", "COUNTY")
  ),

  district = list(
    canonical = "district",
    aliases = c("DISTRICT", "usace_district")
  ),

  # ===== Identifiers =====
  objectid = list(
    canonical = "objectid",
    aliases = c("OBJECTID", "object_id", "OBJECT_ID")
  ),

  permit_number = list(
    canonical = "permit_number",
    aliases = c("PERMIT_NUMBER", "permit_no")
  ),

  # ===== Dates & Temporal =====
  establishment_date = list(
    canonical = "establishment_date",
    aliases = c("ESTABLISHMENT_DATE", "date_established")
  ),

  year_established = list(
    canonical = "year_established",
    aliases = c("YEAR_ESTABLISHED", "year_bank_approved", "YEAR_BANK_APPROVED")
  ),

  # ===== Spatial =====
  bank_location_centroid = list(
    canonical = "bank_location_centroid",
    aliases = c("BANK_LOCATION_CENTROID", "centroid", "location")
  ),

  # ===== Metrics =====
  total_acres = list(
    canonical = "total_acres",
    aliases = c("TOTAL_ACRES", "acres", "ACRES", "total_bank_acres")
  ),

  # ===== Transaction Fields =====
  transaction_id = list(
    canonical = "transaction_id",
    aliases = c("TRANSACTION_ID", "parent_transaction_id", "PARENT_TRANSACTION_ID", "txn_id")
  ),

  transaction_type = list(
    canonical = "transaction_type",
    aliases = c("TRANSACTION_TYPE", "txn_type")
  ),

  transaction_date = list(
    canonical = "transaction_date",
    aliases = c("TRANSACTION_DATE", "txn_date", "date")
  ),

  transaction_status = list(
    canonical = "transaction_status",
    aliases = c("TRANSACTION_STATUS", "txn_status")
  ),

  credit_type = list(
    canonical = "credit_type",
    aliases = c("credit_type_list", "CREDIT_TYPE_LIST", "CREDIT_TYPE")
  ),

  credit_classification = list(
    canonical = "credit_classification",
    aliases = c("CREDIT_CLASSIFICATION", "classification")
  ),

  credits = list(
    canonical = "credits",
    aliases = c("CREDITS", "credit_amount")
  ),

  acres = list(
    canonical = "acres",
    aliases = c("ACRES", "acre")
  ),

  jurisdiction = list(
    canonical = "jurisdiction",
    aliases = c("JURISDICTION")
  ),

  # ===== Notes & Comments =====
  notes = list(
    canonical = "notes",
    aliases = c("comment", "comments", "COMMENT", "COMMENTS", "note", "NOTE")
  ),

  # ===== Entity Type =====
  entity_type = list(
    canonical = "entity_type",
    aliases = c("ENTITY_TYPE")
  ),

  # ===== Additional Transaction Fields =====
  permittee = list(
    canonical = "permittee",
    aliases = c("PERMITTEE")
  ),

  impact_huc = list(
    canonical = "impact_huc",
    aliases = c("IMPACT_HUC", "huc", "HUC")
  ),

  # ===== Contact Fields =====
  contact_type = list(
    canonical = "contact_type",
    aliases = c("CONTACT_TYPE")
  ),

  email = list(
    canonical = "email",
    aliases = c("EMAIL", "e_mail")
  ),

  phone = list(
    canonical = "phone",
    aliases = c("PHONE", "telephone")
  )
)


#' Normalize Column Names Using Registry
#'
#' Standardizes column names in a dataframe using the COLUMN_REGISTRY.
#' Converts aliases to canonical names and handles case-insensitive matching.
#'
#' @param df A dataframe
#' @param registry The column registry to use (default: COLUMN_REGISTRY)
#'
#' @return Dataframe with standardized column names
#'
#' @keywords internal
#' @noRd
.normalize_columns <- function(df, registry = COLUMN_REGISTRY) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  current_names <- names(df)
  new_names <- current_names

  # Build reverse lookup: alias -> canonical
  alias_to_canonical <- list()
  for (canonical_name in names(registry)) {
    entry <- registry[[canonical_name]]
    canonical <- entry$canonical
    aliases <- entry$aliases

    # Add canonical name itself
    alias_to_canonical[[tolower(canonical)]] <- canonical

    # Add all aliases
    for (alias in aliases) {
      alias_to_canonical[[tolower(alias)]] <- canonical
    }
  }

  # Rename columns
  for (i in seq_along(current_names)) {
    col_name <- current_names[i]
    col_lower <- tolower(col_name)

    if (col_lower %in% names(alias_to_canonical)) {
      new_names[i] <- alias_to_canonical[[col_lower]]
    }
  }

  # Handle duplicates created by normalization
  # If we have multiple columns that map to same canonical name,
  # keep the first one and remove others
  if (any(duplicated(new_names))) {
    keep_indices <- !duplicated(new_names)

    # For duplicates, coalesce values before removing
    unique_new_names <- unique(new_names)
    for (canonical in unique_new_names[duplicated(c(FALSE, unique_new_names[-1]))]) {
      # Find all columns with this canonical name
      dup_indices <- which(new_names == canonical)

      if (length(dup_indices) > 1) {
        # Coalesce: first non-NA value wins
        coalesced <- df[[dup_indices[1]]]
        for (j in dup_indices[-1]) {
          coalesced <- dplyr::coalesce(coalesced, df[[j]])
        }
        df[[dup_indices[1]]] <- coalesced
      }
    }

    df <- df[, keep_indices, drop = FALSE]
    new_names <- new_names[keep_indices]
  }

  names(df) <- new_names
  df
}


#' Get Canonical Column Name
#'
#' Returns the canonical name for a given column name (or alias).
#' Useful for checking if a column exists under any alias.
#'
#' @param col_name Column name or alias
#' @param registry The column registry to use (default: COLUMN_REGISTRY)
#'
#' @return Canonical column name, or the original name if not found in registry
#'
#' @keywords internal
#' @noRd
.get_canonical_name <- function(col_name, registry = COLUMN_REGISTRY) {
  col_lower <- tolower(col_name)

  for (canonical_name in names(registry)) {
    entry <- registry[[canonical_name]]
    canonical <- entry$canonical
    aliases <- entry$aliases

    if (tolower(canonical) == col_lower) {
      return(canonical)
    }

    if (col_lower %in% tolower(aliases)) {
      return(canonical)
    }
  }

  # Not found in registry - return original
  col_name
}


#' Check if Column Exists (Any Alias)
#'
#' Checks if a column exists in a dataframe under any of its aliases.
#'
#' @param df A dataframe
#' @param canonical_name The canonical column name to search for
#' @param registry The column registry to use (default: COLUMN_REGISTRY)
#'
#' @return Logical - TRUE if column exists under any alias
#'
#' @keywords internal
#' @noRd
.has_column <- function(df, canonical_name, registry = COLUMN_REGISTRY) {
  if (is.null(df) || !canonical_name %in% names(registry)) {
    return(FALSE)
  }

  entry <- registry[[canonical_name]]
  canonical <- entry$canonical
  aliases <- entry$aliases
  all_names <- c(canonical, aliases)

  any(tolower(names(df)) %in% tolower(all_names))
}
