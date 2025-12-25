# R/summarize-data.R
# Summarization functions for creating one-row-per-bank summaries from detailed data

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


#' Summarize Transactions for Banks Dataframe
#'
#' Creates summary statistics from transaction data to add to banks dataframe.
#' Includes transaction counts, credit/acreage totals, temporal patterns, and
#' geographic/permittee diversity metrics.
#'
#' @param transactions A transactions dataframe (from harmonized transactions)
#'
#' @return A tibble with one row per bank containing transaction summaries
#'
#' @keywords internal
#' @noRd
.summarize_transactions <- function(transactions) {
  if (is.null(transactions) || nrow(transactions) == 0) {
    return(tibble::tibble(bank_id = integer()))
  }

  # Ensure bank_id exists and is integer
  if (!"bank_id" %in% names(transactions)) {
    cli::cli_alert_warning("No bank_id column in transactions - cannot summarize")
    return(tibble::tibble(bank_id = integer()))
  }

  # Ensure bank_id is integer for consistent typing across joins
  transactions$bank_id <- as.integer(transactions$bank_id)

  # Helper: safely calculate months between dates
  months_diff <- function(from, to = Sys.Date()) {
    if (is.null(from) || all(is.na(from))) return(NA_real_)
    from_date <- if (is.character(from)) as.Date(from) else as.Date(from)
    as.numeric(difftime(to, max(from_date, na.rm = TRUE), units = "days")) / 30.44
  }

  # Helper: safely calculate years between dates
  years_diff <- function(from, to) {
    if (is.null(from) || is.null(to) || all(is.na(from)) || all(is.na(to))) return(NA_real_)
    from_date <- if (is.character(from)) as.Date(from) else as.Date(from)
    to_date <- if (is.character(to)) as.Date(to) else as.Date(to)
    as.numeric(difftime(max(to_date, na.rm = TRUE), min(from_date, na.rm = TRUE), units = "days")) / 365.25
  }

  transactions |>
    dplyr::group_by(bank_id) |>
    dplyr::summarise(
      # Transaction volume
      n_transactions = dplyr::n(),
      total_credits_transacted = if ("credits" %in% names(dplyr::cur_data())) {
        sum(as.numeric(credits), na.rm = TRUE)
      } else {
        NA_real_
      },
      total_acres_transacted = if ("acres" %in% names(dplyr::cur_data())) {
        sum(as.numeric(acres), na.rm = TRUE)
      } else {
        NA_real_
      },

      # By transaction type
      n_releases = if ("transaction_type" %in% names(dplyr::cur_data())) {
        sum(stringr::str_detect(transaction_type, stringr::regex("release", ignore_case = TRUE)), na.rm = TRUE)
      } else {
        NA_integer_
      },
      n_withdrawals = if ("transaction_type" %in% names(dplyr::cur_data())) {
        sum(stringr::str_detect(transaction_type, stringr::regex("withdrawal", ignore_case = TRUE)), na.rm = TRUE)
      } else {
        NA_integer_
      },

      # By credit action
      total_credits_released = if ("credit_action" %in% names(dplyr::cur_data())) {
        sum(as.numeric(credits[stringr::str_detect(credit_action, stringr::regex("release", ignore_case = TRUE))]), na.rm = TRUE)
      } else {
        NA_real_
      },
      total_credits_withdrawn = if ("credit_action" %in% names(dplyr::cur_data())) {
        sum(as.numeric(credits[stringr::str_detect(credit_action, stringr::regex("withdrawal", ignore_case = TRUE))]), na.rm = TRUE)
      } else {
        NA_real_
      },

      # Temporal patterns
      first_transaction_date = if ("transaction_date" %in% names(dplyr::cur_data())) {
        min(transaction_date, na.rm = TRUE)
      } else {
        as.Date(NA)
      },
      last_transaction_date = if ("transaction_date" %in% names(dplyr::cur_data())) {
        max(transaction_date, na.rm = TRUE)
      } else {
        as.Date(NA)
      },
      months_since_last_transaction = if ("transaction_date" %in% names(dplyr::cur_data())) {
        months_diff(transaction_date)
      } else {
        NA_real_
      },
      years_active = if ("transaction_date" %in% names(dplyr::cur_data())) {
        years_diff(transaction_date, transaction_date)
      } else {
        NA_real_
      },

      # Geographic diversity
      n_unique_hucs = if ("impact_huc" %in% names(dplyr::cur_data())) {
        dplyr::n_distinct(impact_huc, na.rm = TRUE)
      } else {
        NA_integer_
      },
      n_impact_states = if ("impact_state" %in% names(dplyr::cur_data())) {
        dplyr::n_distinct(impact_state, na.rm = TRUE)
      } else {
        NA_integer_
      },
      impact_states_list = if ("impact_state" %in% names(dplyr::cur_data())) {
        paste(unique(stats::na.omit(impact_state)), collapse = ", ")
      } else {
        NA_character_
      },

      # Permittee diversity
      n_unique_permittees = if ("permittee" %in% names(dplyr::cur_data())) {
        dplyr::n_distinct(permittee, na.rm = TRUE)
      } else {
        NA_integer_
      },
      top_permittee = if ("permittee" %in% names(dplyr::cur_data())) {
        permittee_counts <- table(permittee)
        if (length(permittee_counts) > 0) {
          names(permittee_counts)[which.max(permittee_counts)]
        } else {
          NA_character_
        }
      } else {
        NA_character_
      },

      .groups = "drop"
    )
}


#' Summarize Credit Releases for Banks Dataframe
#'
#' Creates summary statistics from anticipated credit releases (next 5 years)
#' to add to banks dataframe. Shows upcoming release schedule and totals.
#'
#' @param credit_releases A credit_releases dataframe from rb_download_report("credit_releases")
#'
#' @return A tibble with one row per bank containing credit release summaries
#'
#' @keywords internal
#' @noRd
.summarize_credit_releases <- function(credit_releases) {
  if (is.null(credit_releases) || nrow(credit_releases) == 0) {
    return(tibble::tibble(bank_id = integer()))
  }

  # Ensure bank_id exists
  if (!"bank_id" %in% names(credit_releases)) {
    cli::cli_alert_warning("No bank_id column in credit_releases - cannot summarize")
    return(tibble::tibble(bank_id = integer()))
  }

  # Ensure required columns exist
  if (!"credits" %in% names(credit_releases)) {
    cli::cli_alert_warning("Missing 'credits' column in credit_releases")
    credit_releases$credits <- NA_real_
  }

  if (!"anticipated_release_date" %in% names(credit_releases)) {
    cli::cli_alert_warning("Missing 'anticipated_release_date' column in credit_releases")
    credit_releases$anticipated_release_date <- as.Date(NA)
  }

  credit_releases |>
    dplyr::group_by(bank_id) |>
    dplyr::summarise(
      # Totals
      total_anticipated_credits = sum(credits, na.rm = TRUE),
      n_upcoming_releases = dplyr::n(),

      # Timeline
      earliest_release_date = min(anticipated_release_date, na.rm = TRUE),
      latest_release_date = max(anticipated_release_date, na.rm = TRUE),

      # Near-term (next year)
      credits_releasing_next_year = {
        next_year <- Sys.Date() + 365
        sum(credits[anticipated_release_date <= next_year], na.rm = TRUE)
      },

      .groups = "drop"
    )
}


#' Summarize Public Notices for Banks Dataframe
#'
#' Creates summary statistics from public notice documents to add to banks dataframe.
#' Shows document counts and recency of notices.
#'
#' @param public_notices A public_notices dataframe from rb_download_report("public_notices")
#'
#' @return A tibble with one row per bank containing public notice summaries
#'
#' @keywords internal
#' @noRd
.summarize_public_notices <- function(public_notices) {
  if (is.null(public_notices) || nrow(public_notices) == 0) {
    return(tibble::tibble(bank_id = integer()))
  }

  # Ensure bank_id exists
  if (!"bank_id" %in% names(public_notices)) {
    cli::cli_alert_warning("No bank_id column in public_notices - cannot summarize")
    return(tibble::tibble(bank_id = integer()))
  }

  # Ensure create_date exists
  if (!"create_date" %in% names(public_notices)) {
    cli::cli_alert_warning("Missing 'create_date' column in public_notices")
    public_notices$create_date <- as.Date(NA)
  }

  public_notices |>
    dplyr::group_by(bank_id) |>
    dplyr::summarise(
      # Counts
      n_public_notices = dplyr::n(),

      # Recency
      most_recent_notice_date = max(create_date, na.rm = TRUE),
      oldest_notice_date = min(create_date, na.rm = TRUE),
      months_since_last_notice = {
        if (any(!is.na(create_date))) {
          as.numeric(difftime(Sys.Date(), max(create_date, na.rm = TRUE), units = "days")) / 30.44
        } else {
          NA_real_
        }
      },

      .groups = "drop"
    )
}
