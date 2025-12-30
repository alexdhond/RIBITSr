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
    aliases = c("BANK_TYPE", "bank_type_1", "bank_type_2")
  ),

  kind_of_bank = list(
    canonical = "kind_of_bank",
    aliases = c("KIND_OF_BANK", "bank_kind", "BANK_KIND")
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
    aliases = c("TRANSACTION_ID", "txn_id")
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
  ),

  # ===== API-Unique Transaction Fields =====
  is_transferred = list(
    canonical = "is_transferred",
    aliases = c("IS_TRANSFERRED", "transferred")
  ),

  is_purchased = list(
    canonical = "is_purchased",
    aliases = c("IS_PURCHASED", "purchased")
  ),

  # ===== CSV Ledger-Unique Transaction Fields =====
  sub_ledger_id = list(
    canonical = "sub_ledger_id",
    aliases = c("SUB_LEDGER_ID", "subledger_id", "sub_ledger")
  ),

  permit_auth_date = list(
    canonical = "permit_auth_date",
    aliases = c("PERMIT_AUTH_DATE", "permit_authorization_date", "permit_date")
  ),

  impact_latitude = list(
    canonical = "impact_latitude",
    aliases = c("IMPACT_LATITUDE", "impact_lat", "latitude", "lat")
  ),

  impact_longitude = list(
    canonical = "impact_longitude",
    aliases = c("IMPACT_LONGITUDE", "impact_lon", "impact_long", "longitude", "lon", "long")
  ),

  parent_transaction_id = list(
    canonical = "parent_transaction_id",
    aliases = c("PARENT_TRANSACTION_ID", "parent_txn_id")
  ),

  sub_ledger_project_name = list(
    canonical = "sub_ledger_project_name",
    aliases = c("SUB_LEDGER_PROJECT_NAME", "subledger_project")
  ),

  # ===== Additional Transaction Fields =====
  linear_feet = list(
    canonical = "linear_feet",
    aliases = c("LINEAR_FEET", "linearfeet")
  ),

  credit_action = list(
    canonical = "credit_action",
    aliases = c("CREDIT_ACTION", "action")
  ),

  permit_list = list(
    canonical = "permit_list",
    aliases = c("PERMIT_LIST", "permits")
  ),

  impact_quantity = list(
    canonical = "impact_quantity",
    aliases = c("IMPACT_QUANTITY")
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


# NOTE: .col_get() and .col_exists() are defined in utils-columns.R
# They provide case-insensitive column access with additional options.
# Do not duplicate definitions here.

#' Ensure column exists (consolidated helper)
#'
#' @description
#' Ensures a column exists in a dataframe. If it doesn't exist, adds it with default value.
#'
#' @param df A dataframe
#' @param col_name Column name to ensure exists
#' @param default Default value for the column if it doesn't exist
#'
#' @return Dataframe with column guaranteed to exist
#'
#' @keywords internal
#' @noRd
.col_ensure <- function(df, col_name, default = NA) {
  if (is.null(df) || !is.data.frame(df)) {
    return(df)
  }

  if (!.col_exists(df, col_name)) {
    df[[col_name]] <- default
  }

  df
}

# =============================================================================
# COLUMN ORDERING & CLEANING
# =============================================================================

#' Order columns for bank data
#'
#' Applies standard column ordering:
#' 1. Core Identification (bank_id, bank_name, objectid, permit_number)
#' 2. Status & Classification (bank_status, bank_type, etc.)
#' 3. Geographic & Administrative (state, county, district, etc.)
#' 4. Chronology (dates, year_established)
#' 5. Spatial & Technical (centroid, geometry, match_quality)
#' 6. External Links (website, urls)
#'
#' @param df A data frame to reorder
#' @param type Type of data: "banks", "ledger", "contacts", "geometry"
#' @return Data frame with reordered columns
#' @keywords internal
.order_columns <- function(df, type = "banks") {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  # Define column order by category
  col_order <- switch(type,
    banks = c(
      # 1. Core Identification
      "bank_id", "bank_name", "objectid", "permit_number",
      # 2. Status & Classification
      "bank_status", "bank_status_date", "bank_type", "kind_of_bank",
      "is_404", "is_conservation",
      # 3. Contact Summaries (NEW - from summarization)
      "primary_sponsor", "primary_poc_name", "primary_poc_email", "primary_poc_phone",
      "n_sponsors", "n_pocs", "n_managers", "n_irt_members", "n_other_contacts", "total_contacts",
      # 4. Credit Summaries (NEW - from summarization)
      "total_available_credits", "total_released_credits", "total_potential_credits",
      "n_credit_types", "primary_credit_classification", "primary_credits_available",
      # 5. Geographic & Administrative
      "state_list", "state_abbrev_list", "county_list", "total_acres",
      "district", "field_office", "chair",
      "nmfs_region", "noaa_fisheries_region",
      "secondary_district_list", "secondary_office_list", "secondary_noaa_region_list",
      # 6. Chronology
      "establishment_date", "year_established", "comments",
      # 7. Spatial & Technical
      "bank_location_centroid", "bank_geometry_obscured", "match_quality",
      # 8. External Links
      "website", "ribits_url_to_bank", "umbrella_instrument_data_ws_url"
    ),
    geometry = c(
      "bank_id", "bank_name", "bank_status",
      "centroid", "footprint", "service_area"
    ),
    transactions = c(
      # 1. Core IDs
      "bank_id", "bank_name", "transaction_id",
      # 2. Transaction Details
      "transaction_type", "transaction_date", "transaction_status",
      # 3. Classification
      "jurisdiction", "credit_type", "credit_classification",
      "resource_type", "activity_type",
      # 4. Quantities
      "credits", "acres", "linear_feet",
      # 5. Geographic (from watershed CSV)
      "impact_huc", "huc_name", "impact_latitude", "impact_longitude",
      "impact_state", "impact_county",
      # 6. Permittee & Permits
      "permittee", "permit_list", "permit_auth_date", "permit_number",
      # 7. Projects
      "parent_project_name", "sub_ledger_project_name", "sub_ledger_id",
      # 8. Additional Transaction Info
      "credit_action", "is_transferred", "is_purchased", "is_ilf",
      "notes",
      # 9. Source Tracking
      "source", "entity_type"
    ),
    # Legacy alias for backwards compatibility
    ledger = c(
      "bank_id", "bank_name", "transaction_id",
      "transaction_type", "transaction_date", "transaction_status",
      "credit_type", "resource_type", "activity_type",
      "credits", "acres", "linear_feet",
      "parent_project_name", "sub_ledger_project_name",
      "source"
    ),
    contacts = c(
      # 1. Bank Identification
      "bank_id", "bank_name",
      # 2. Contact Classification
      "contact_type", "poc_type",
      # 3. Name/Identity
      "salutation", "first_name", "middle_initial", "last_name", "sponsor_name", "title",
      # 4. Contact Info
      "email", "phone", "cell_phone", "fax",
      # 5. Address
      "address1", "address2", "city", "state", "zip"
    ),
    # Default: no specific order
    character(0)
  )
  
  # Get current columns
  current_cols <- names(df)
  
  # Order: defined order first, then remaining columns alphabetically
  ordered_cols <- intersect(col_order, current_cols)
  remaining_cols <- setdiff(current_cols, col_order)
  remaining_cols <- sort(remaining_cols)
  
  # Reorder
  df[, c(ordered_cols, remaining_cols), drop = FALSE]
}

#' Remove duplicate/redundant columns
#'
#' Removes columns with numbered suffixes (_1, _2) that may remain after merging.
#' Most duplicates are now handled by .normalize_columns() in the column registry.
#'
#' @param df A data frame
#' @return Data frame with duplicates removed
#' @keywords internal
.remove_duplicate_columns <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  # SIMPLIFIED: Most duplicates already handled by .normalize_columns()
  # This function now just handles edge cases and remaining numbered suffixes

  # Find columns with numbered suffixes (_1, _2)
  suffixed_cols <- grep("_[12]$", names(df), value = TRUE)

  for (col in suffixed_cols) {
    base <- sub("_[12]$", "", col)

    if (base %in% names(df)) {
      # Coalesce: canonical first, then alternatives
      df[[base]] <- dplyr::coalesce(
        as.character(df[[base]]),
        as.character(df[[col]])
      )
      df[[col]] <- NULL
    } else {
      # Base doesn't exist - rename this to base
      names(df)[names(df) == col] <- base
    }
  }

  df
}

#' Finalize data frame with standard cleaning
#'
#' Applies duplicate removal, column ordering, and clean_names.
#'
#' @param df A data frame
#' @param type Type for column ordering
#' @return Cleaned and ordered data frame
#' @keywords internal
.finalize_df <- function(df, type = "banks") {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  df |>
    janitor::clean_names() |>
    .remove_duplicate_columns() |>
    .order_columns(type = type)
}