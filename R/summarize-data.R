# R/summarize-data.R
# Summarization functions for contacts and credits

#' Summarize Contacts for Banks Dataframe
#'
#' Creates summary statistics from detailed contacts data to add to banks dataframe.
#' Includes primary contact info and counts by contact type.
#'
#' @param contacts A contacts dataframe from rb_extract_contacts()
#'
#' @return A tibble with one row per bank containing contact summaries
#'
#' @keywords internal
#' @noRd
.summarize_contacts <- function(contacts) {
  if (is.null(contacts) || nrow(contacts) == 0) {
    return(tibble::tibble(bank_id = integer()))
  }

  # Ensure bank_id exists
  if (!"bank_id" %in% names(contacts)) {
    cli::cli_alert_warning("No bank_id column in contacts - cannot summarize")
    return(tibble::tibble(bank_id = integer()))
  }

  contacts |>
    dplyr::group_by(bank_id) |>
    dplyr::summarise(
      # Primary sponsor
      primary_sponsor = dplyr::first(sponsor_name[contact_type == "bank_sponsors"], default = NA_character_),

      # Primary POC (Point of Contact)
      primary_poc_name = {
        first <- dplyr::first(first_name[contact_type == "bank_pocs"], default = NA_character_)
        last <- dplyr::first(last_name[contact_type == "bank_pocs"], default = NA_character_)
        if (!is.na(first) || !is.na(last)) {
          paste(first, last) |> trimws()
        } else {
          NA_character_
        }
      },
      primary_poc_email = dplyr::first(email[contact_type == "bank_pocs"], default = NA_character_),
      primary_poc_phone = dplyr::first(phone[contact_type == "bank_pocs"], default = NA_character_),

      # Counts by contact type
      n_sponsors = sum(contact_type == "bank_sponsors", na.rm = TRUE),
      n_pocs = sum(contact_type == "bank_pocs", na.rm = TRUE),
      n_managers = sum(contact_type == "bank_managers", na.rm = TRUE),
      n_irt_members = sum(contact_type == "bank_irt_members", na.rm = TRUE),
      n_other_contacts = sum(contact_type == "bank_other_contacts", na.rm = TRUE),
      total_contacts = dplyr::n(),

      .groups = "drop"
    )
}


#' Summarize Credit Classifications for Banks Dataframe
#'
#' Creates summary statistics from credit classification data to add to banks dataframe.
#' Includes totals across all classifications and primary classification info.
#'
#' @param credit_summary A credit summary dataframe from rb_download_report("credit_classification")
#'
#' @return A tibble with one row per bank containing credit summaries
#'
#' @keywords internal
#' @noRd
.summarize_credits <- function(credit_summary) {
  if (is.null(credit_summary) || nrow(credit_summary) == 0) {
    return(tibble::tibble(bank_id = integer()))
  }

  # Ensure bank_id exists
  if (!"bank_id" %in% names(credit_summary)) {
    cli::cli_alert_warning("No bank_id column in credit_summary - cannot summarize")
    return(tibble::tibble(bank_id = integer()))
  }

  # Ensure required columns exist
  required_cols <- c("available_credits", "released_credits", "potential_credits", "credit_classification")
  missing_cols <- setdiff(required_cols, names(credit_summary))

  if (length(missing_cols) > 0) {
    cli::cli_alert_warning("Missing columns in credit_summary: {paste(missing_cols, collapse = ', ')}")
    # Add missing columns as NA
    for (col in missing_cols) {
      credit_summary[[col]] <- NA_real_
    }
  }

  credit_summary |>
    dplyr::group_by(bank_id) |>
    dplyr::summarise(
      # Totals across all classifications
      total_available_credits = sum(available_credits, na.rm = TRUE),
      total_released_credits = sum(released_credits, na.rm = TRUE),
      total_potential_credits = sum(potential_credits, na.rm = TRUE),

      # Primary classification (one with most available credits)
      n_credit_types = dplyr::n(),
      primary_credit_classification = {
        max_idx <- which.max(available_credits)
        if (length(max_idx) > 0) {
          dplyr::first(credit_classification[max_idx], default = NA_character_)
        } else {
          NA_character_
        }
      },
      primary_credits_available = {
        if (any(!is.na(available_credits))) {
          max(available_credits, na.rm = TRUE)
        } else {
          NA_real_
        }
      },

      .groups = "drop"
    )
}
