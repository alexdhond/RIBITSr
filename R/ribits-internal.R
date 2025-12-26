# R/ribits-internal.R
# Internal helper functions for .ribits_engine to streamline ribits.R

#' Fetch data from RIBITS API with standard handling
#' @keywords internal
#' @noRd
.fetch_ribits_api_data <- function(type, bank_ids, state, district, quietly = FALSE) {
  if (!quietly) cli::cli_progress_step("Querying RIBITS API...")
  
  tryCatch({
    # First get list of bank IDs
    bank_list <- rb_get(type, state = state, district = district)
    
    if (is.null(bank_list) || nrow(bank_list) == 0) {
      return(list(banks = NULL, contacts = NULL))
    }
    
    # Normalize columns immediately
    bank_list <- .normalize_columns(bank_list)
    
    # Check for bank_id
    if (!.col_exists(bank_list, "bank_id")) {
      cli::cli_alert_warning("No bank_id column found in API list")
      return(list(banks = NULL, contacts = NULL))
    }
    
    list_ids <- bank_list$bank_id
    
    # Filter to specific IDs if provided
    ids_to_fetch <- if (!is.null(bank_ids)) {
      intersect(list_ids, bank_ids)
    } else {
      list_ids
    }
    
    if (length(ids_to_fetch) == 0) {
      return(list(banks = NULL, contacts = NULL))
    }
    
    # Fetch detailed data for each bank
    if (!quietly) cli::cli_alert_info("Fetching detailed API data for {length(ids_to_fetch)} banks...")
    
    detailed <- rb_get(type, id = ids_to_fetch, ledger = FALSE, 
                       footprint = FALSE, service_area = FALSE, contacts = TRUE)
    
    # Extract summary and contacts
    banks_data <- if (is.list(detailed) && "summary" %in% names(detailed)) {
      detailed$summary
    } else {
      bank_list
    }
    
    contacts_data <- if (is.list(detailed) && !is.null(detailed$contacts) && 
                         nrow(detailed$contacts) > 0) {
      detailed$contacts
    } else {
      NULL
    }
    
    # Normalize result columns
    if (!is.null(banks_data)) banks_data <- .normalize_columns(banks_data)
    if (!is.null(contacts_data)) contacts_data <- .normalize_columns(contacts_data)
    
    list(banks = banks_data, contacts = contacts_data)
    
  }, error = function(e) {
    if (!quietly) cli::cli_alert_warning("API query failed: {e$message}")
    list(banks = NULL, contacts = NULL)
  })
}

#' Fetch data from EPA ArcGIS with standard handling
#' @keywords internal
#' @noRd
.fetch_epa_data <- function(type, bank_ids, state, district, quietly = FALSE) {
  if (!quietly) cli::cli_progress_step("Querying EPA ArcGIS...")
  
  tryCatch({
    # Map type to layer (assuming "banks" type -> "banks" layer for now)
    # This might need refinement for ILF/Umbrellas if EPA structure differs
    layer <- if (type == "banks") "banks" else type 
    
    epa <- rb_epa(layer, state = state, district = district)
    
    if (is.null(epa) || nrow(epa) == 0) {
      return(NULL)
    }
    
    banks_epa <- sf::st_drop_geometry(epa)
    
    # Normalize columns
    banks_epa <- .normalize_columns(banks_epa)
    
    # Filter by bank_ids if provided
    if (!is.null(bank_ids)) {
      banks_epa <- .filter_by_bank_ids(banks_epa, bank_ids, quietly = quietly)
    }
    
    banks_epa
    
  }, error = function(e) {
    if (!quietly) cli::cli_alert_warning("EPA query failed: {e$message}")
    NULL
  })
}

#' Fetch data from CSV reports with standard handling
#' @keywords internal
#' @noRd
.fetch_csv_data <- function(type, bank_ids, state, cache, quietly = FALSE) {
  if (!quietly) cli::cli_progress_step("Downloading CSV reports...")
  
  tryCatch({
    cache_dir <- .get_cache_dir(cache)
    
    # Determine report name based on type
    report_name <- "banks_sites" # Default for now
    
    csv_file <- rb_download_report(report_name, download_dir = cache_dir)
    banks <- rb_read(csv_file)
    
    # Normalize columns immediately
    banks <- .normalize_columns(banks)
    
    # Filter by state (using standardized column)
    if (!is.null(state)) {
      if (.col_exists(banks, "state_list")) {
        banks <- banks |> dplyr::filter(grepl(!!state, state_list, ignore.case = TRUE))
      }
    }
    
    # Ensure bank_id exists
    banks <- .ensure_bank_id(banks, quietly = quietly)
    
    # Filter by bank_ids if specified
    if (!is.null(bank_ids)) {
      banks <- .filter_by_bank_ids(banks, bank_ids, quietly = quietly)
    }
    
    banks
  }, error = function(e) {
    if (!quietly) cli::cli_alert_warning("CSV fetch failed: {e$message}")
    NULL
  })
}

#' Fetch and unify spatial data
#' @keywords internal
#' @noRd
.fetch_harmonized_spatial <- function(query_ids, use_epa, use_api, quietly = FALSE) {
  if (!quietly) cli::cli_h3("Step 6: Fetching spatial data")
  
  discrepancies <- tibble::tibble()
  
  # 1. EPA Footprints
  epa_footprints <- NULL
  if (use_epa) {
    if (!quietly) cli::cli_progress_step("Querying EPA footprints...")
    tryCatch({
      fp <- rb_epa("footprints", bank_ids = query_ids)
      if (!is.null(fp) && nrow(fp) > 0) {
        fp$source <- "epa_arcgis"
        fp <- .normalize_columns(fp)
        if (!quietly) cli::cli_alert_success("{nrow(fp)} footprints from EPA")
        epa_footprints <- fp
      }
    }, error = function(e) {
      if (!quietly) cli::cli_alert_warning("EPA footprints failed: {e$message}")
    })
  }

  # 2. EPA Service Areas
  epa_service_areas <- NULL
  if (use_epa) {
    if (!quietly) cli::cli_progress_step("Querying EPA service areas...")
    tryCatch({
      sa <- rb_epa("service_areas", bank_ids = query_ids)
      if (!is.null(sa) && nrow(sa) > 0) {
        sa$source <- "epa_arcgis"
        sa <- .normalize_columns(sa)
        if (!quietly) cli::cli_alert_success("{nrow(sa)} service areas from EPA")
        epa_service_areas <- sa
      }
    }, error = function(e) {
      if (!quietly) cli::cli_alert_warning("EPA service areas failed: {e$message}")
    })
  }
  
  # 3. Missing footprints from API
  ribits_footprints <- NULL
  if (use_api) {
    epa_fp_ids <- if (!is.null(epa_footprints)) .col_get(epa_footprints, "bank_id", integer()) else integer()
    missing_fp_ids <- setdiff(query_ids, epa_fp_ids)
    
    if (length(missing_fp_ids) > 0 && length(missing_fp_ids) <= 30) {
       if (!quietly) cli::cli_progress_step("Fetching {length(missing_fp_ids)} missing footprints from API...")
       ribits_fp_list <- list()
       for (bid in missing_fp_ids) {
         tryCatch({
           bd <- rb_get("banks", id = bid, footprint = TRUE, ledger = FALSE, 
                        service_area = FALSE, contacts = FALSE)
           if (!is.null(bd$footprint) && nrow(bd$footprint) > 0) {
             fp <- bd$footprint
             fp$source <- "ribits_api"
             fp$bank_id <- bid
             ribits_fp_list[[length(ribits_fp_list) + 1]] <- fp
           }
         }, error = function(e) NULL)
         Sys.sleep(0.05)
       }
       if (length(ribits_fp_list) > 0) {
         ribits_footprints <- dplyr::bind_rows(ribits_fp_list)
         ribits_footprints <- .normalize_columns(ribits_footprints)
         if (!quietly) cli::cli_alert_success("{nrow(ribits_footprints)} additional footprints from API")
       }
    }
  }

  # 4. Missing service areas from API
  ribits_service_areas <- NULL
  if (use_api) {
    epa_sa_ids <- if (!is.null(epa_service_areas)) .col_get(epa_service_areas, "bank_id", integer()) else integer()
    missing_sa_ids <- setdiff(query_ids, epa_sa_ids)
    
    if (length(missing_sa_ids) > 0 && length(missing_sa_ids) <= 30) {
       if (!quietly) cli::cli_progress_step("Fetching {length(missing_sa_ids)} service areas from API...")
       ribits_sa_list <- list()
       for (bid in missing_sa_ids) {
         tryCatch({
           bd <- rb_get("banks", id = bid, service_area = TRUE, footprint = FALSE, 
                        ledger = FALSE, contacts = FALSE)
           if (!is.null(bd$service_area) && nrow(bd$service_area) > 0) {
             sa <- bd$service_area
             sa$source <- "ribits_api"
             sa$bank_id <- bid
             ribits_sa_list[[length(ribits_sa_list) + 1]] <- sa
           }
         }, error = function(e) NULL)
         Sys.sleep(0.05)
       }
       if (length(ribits_sa_list) > 0) {
         ribits_service_areas <- dplyr::bind_rows(ribits_sa_list)
         ribits_service_areas <- .normalize_columns(ribits_service_areas)
         if (!quietly) cli::cli_alert_success("{nrow(ribits_service_areas)} service areas from API")
       }
    }
  }

  # Combine
  footprints_data <- dplyr::bind_rows(epa_footprints, ribits_footprints)
  service_areas_data <- dplyr::bind_rows(epa_service_areas, ribits_service_areas)
  
  # Check discrepancies (simplified)
  if (!is.null(epa_footprints) && !is.null(ribits_footprints)) {
    overlap_ids <- intersect(epa_footprints$bank_id, ribits_footprints$bank_id)
    for (bid in overlap_ids) {
      disc <- .compare_geometries(
        ribits_footprints[ribits_footprints$bank_id == bid, ],
        epa_footprints[epa_footprints$bank_id == bid, ],
        bid, "footprint"
      )
      if (!is.null(disc)) discrepancies <- dplyr::bind_rows(discrepancies, disc)
    }
  }
  
  list(
    footprints = footprints_data,
    service_areas = service_areas_data,
    discrepancies = discrepancies
  )
}

#' Summarize Contacts for Banks Dataframe
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
