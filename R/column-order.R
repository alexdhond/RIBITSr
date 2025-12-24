#' Standard Column Ordering for RIBITS Data
#'
#' Provides consistent column ordering across all data components.
#' @keywords internal

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
