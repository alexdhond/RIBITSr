# R/ribits-internal.R
# Internal helper functions for .ribits_engine to streamline ribits.R

#' Fetch data from RIBITS API with standard handling
#' @keywords internal
#' @noRd
.fetch_ribits_api_data <- function(type, bank_ids, state, district, include_spatial = FALSE, quietly = FALSE) {
  if (!quietly) cli::cli_progress_step("Querying RIBITS API...")
  
  tryCatch({
    # First get list of bank IDs
    bank_list <- rb_get(type, state = state, district = district)
    
    if (is.null(bank_list) || nrow(bank_list) == 0) {
      return(list(banks = NULL, contacts = NULL, geometry = NULL))
    }
    
    # Normalize columns immediately
    bank_list <- .normalize_columns(bank_list)
    
    # Check for bank_id
    if (!.col_exists(bank_list, "bank_id")) {
      cli::cli_alert_warning("No bank_id column found in API list")
      return(list(banks = NULL, contacts = NULL, geometry = NULL))
    }
    
    list_ids <- bank_list$bank_id
    
    # Filter to specific IDs if provided
    ids_to_fetch <- if (!is.null(bank_ids)) {
      intersect(list_ids, bank_ids)
    } else {
      list_ids
    }
    
    if (length(ids_to_fetch) == 0) {
      return(list(banks = NULL, contacts = NULL, geometry = NULL))
    }
    
    # Fetch detailed data for each bank
    # Optimization: Fetch spatial data here if requested to avoid double fetch later
    if (!quietly) cli::cli_alert_info("Fetching detailed API data for {length(ids_to_fetch)} banks...")
    
    detailed <- rb_get(type, id = ids_to_fetch, 
                       ledger = FALSE, 
                       footprint = include_spatial, 
                       service_area = include_spatial, 
                       contacts = TRUE)
    
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
    
    # Extract geometry if requested and present
    geometry_data <- NULL
    if (include_spatial && is.list(detailed)) {
        # Return structured list instead of binding rows
        # This prevents schema mismatch issues
        geometry_data <- list(
            footprints = if (!is.null(detailed$footprint)) detailed$footprint else NULL,
            service_areas = if (!is.null(detailed$service_area)) detailed$service_area else NULL
        )
    }
    
    # Normalize result columns
    if (!is.null(banks_data)) banks_data <- .normalize_columns(banks_data)
    if (!is.null(contacts_data)) contacts_data <- .normalize_columns(contacts_data)
    
    list(banks = banks_data, contacts = contacts_data, geometry = geometry_data)
    
  }, error = function(e) {
    if (!quietly) cli::cli_alert_warning("API query failed: {e$message}")
    list(banks = NULL, contacts = NULL, geometry = NULL)
  })
}

#' Fetch data from EPA ArcGIS with standard handling
#'
#' Queries all EPA bank status layers (approved, pending, terminated) and
#' combines the results. The terminated layer includes Sold-Out, Suspended,
#' Terminated, and Withdrawn banks.
#'
#' @keywords internal
#' @noRd
.fetch_epa_data <- function(type, bank_ids, state, district, quietly = FALSE) {
  if (!quietly) cli::cli_progress_step("Querying EPA ArcGIS (all status layers)...")

  if (type != "banks") {
    # For non-bank types (ILF, umbrellas), use the original simple approach
    tryCatch({
      epa <- rb_epa(type, state = state, district = district)

      if (is.null(epa) || nrow(epa) == 0) {
        return(NULL)
      }

      result <- sf::st_drop_geometry(epa)
      result <- .normalize_columns(result)

      if (!is.null(bank_ids)) {
        result <- .filter_by_bank_ids(result, bank_ids, quietly = quietly)
      }

      result

    }, error = function(e) {
      if (!quietly) cli::cli_alert_warning("EPA query failed: {e$message}")
      NULL
    })
  } else {
    # For banks, query all three status layers and combine
    tryCatch({
      results <- list()

      # Layer 2: Approved banks
      approved <- tryCatch({
        epa <- rb_epa("approved", state = state, district = district)
        if (!is.null(epa) && nrow(epa) > 0) {
          df <- sf::st_drop_geometry(epa)
          df$epa_layer <- "approved_banks"
          df
        } else {
          NULL
        }
      }, error = function(e) NULL)

      if (!is.null(approved) && nrow(approved) > 0) {
        results$approved <- approved
        if (!quietly) cli::cli_alert_success("EPA approved: {nrow(approved)} banks")
      }

      # Layer 3: Pending banks
      pending <- tryCatch({
        epa <- rb_epa("pending", state = state, district = district)
        if (!is.null(epa) && nrow(epa) > 0) {
          df <- sf::st_drop_geometry(epa)
          df$epa_layer <- "pending_banks"
          df
        } else {
          NULL
        }
      }, error = function(e) NULL)

      if (!is.null(pending) && nrow(pending) > 0) {
        results$pending <- pending
        if (!quietly) cli::cli_alert_success("EPA pending: {nrow(pending)} banks")
      }

      # Layer 4: Terminated banks (includes Sold-Out, Suspended, Terminated, Withdrawn)
      terminated <- tryCatch({
        epa <- rb_epa("terminated", state = state, district = district)
        if (!is.null(epa) && nrow(epa) > 0) {
          df <- sf::st_drop_geometry(epa)
          df$epa_layer <- "terminated_banks"
          df
        } else {
          NULL
        }
      }, error = function(e) NULL)

      if (!is.null(terminated) && nrow(terminated) > 0) {
        results$terminated <- terminated
        if (!quietly) cli::cli_alert_success("EPA terminated/sold-out/etc: {nrow(terminated)} banks")
      }

      # Combine all results
      if (length(results) == 0) {
        return(NULL)
      }

      banks_epa <- dplyr::bind_rows(results)

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
}

#' Fetch data from CSV reports with standard handling
#' @keywords internal
#' @noRd
.fetch_csv_data <- function(resource_type, bank_ids, state, cache, quietly = FALSE) {
  if (!quietly) cli::cli_progress_step("Downloading CSV reports...")
  
  # Ensure type is a string (defensive)
  if (!is.character(resource_type)) {
    cli::cli_abort("resource_type must be a character string")
  }
  
  tryCatch({
    cache_dir <- .get_cache_dir(cache)
    
    # Determine report name based on type
    report_name <- switch(resource_type,
      "banks" = "banks_sites",
      "ilf" = "ilf_programs", 
      "umbrellas" = "umbrellas",
      "banks_sites" # Default fallback
    )
    
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
        
        banks  }, error = function(e) {
    if (!quietly) cli::cli_alert_warning("CSV fetch failed: {e$message}")
    NULL
  })
}

#' Fetch and unify spatial data
#' @keywords internal
#' @noRd
.fetch_harmonized_spatial <- function(query_ids, use_epa, use_api, geometry_api = NULL, quietly = FALSE) {
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
  
  # 3. API Spatial Data (Footprints & Service Areas)
  ribits_footprints <- NULL
  ribits_service_areas <- NULL
  
  if (use_api) {
      # Use pre-fetched API geometry if available (Passed as list)
      if (!is.null(geometry_api) && is.list(geometry_api)) {
           if (!is.null(geometry_api$footprints)) {
               ribits_footprints <- geometry_api$footprints
               ribits_footprints$source <- "ribits_api"
               ribits_footprints <- .normalize_columns(ribits_footprints)
           }
           if (!is.null(geometry_api$service_areas)) {
               ribits_service_areas <- geometry_api$service_areas
               ribits_service_areas$source <- "ribits_api"
               ribits_service_areas <- .normalize_columns(ribits_service_areas)
           }
      }
      
      # Fallback: Gap Filling Logic (if no pre-fetched geometry or gaps exist)
      # Footprints
      if (is.null(ribits_footprints)) {
          epa_fp_ids <- if (!is.null(epa_footprints)) .col_get(epa_footprints, "bank_id", error_if_missing = FALSE, default = integer()) else integer()
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
      
      # Service Areas
      if (is.null(ribits_service_areas)) {
          epa_sa_ids <- if (!is.null(epa_service_areas)) .col_get(epa_service_areas, "bank_id", error_if_missing = FALSE, default = integer()) else integer()
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

# =============================================================================
# Summarization Helpers
# =============================================================================

#' Validate data for summarization
#'
#' Common validation for all summarize functions. Checks for null/empty data,
#' validates bank_id column exists, and optionally adds missing required columns.
#'
#' @param data Data frame to validate
#' @param name Name of the summarizer (for error messages)
#' @param required_cols Character vector of required column names (optional)
#' @param fill_type Type to use for filling missing columns: "real", "character", "integer", "date"
#' @return List with `valid` (logical) and `data` (possibly modified data frame)
#' @keywords internal
#' @noRd
.validate_for_summarize <- function(data, name, required_cols = NULL, fill_type = "real") {
  # Check null/empty
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, data = NULL))
  }

  # Check bank_id exists
  if (!"bank_id" %in% names(data)) {
    cli::cli_alert_warning("No bank_id column in {name} - cannot summarize")
    return(list(valid = FALSE, data = NULL))
  }

  # Add missing required columns as NA
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      cli::cli_alert_warning("Missing columns in {name}: {paste(missing_cols, collapse = ', ')}")
      fill_value <- switch(fill_type,
        real = NA_real_,
        character = NA_character_,
        integer = NA_integer_,
        date = as.Date(NA),
        NA
      )
      for (col in missing_cols) {
        data[[col]] <- fill_value
      }
    }
  }

  list(valid = TRUE, data = data)
}


#' Summarize Contacts for Banks Dataframe
#' @keywords internal
#' @noRd
.summarize_contacts <- function(contacts) {
  check <- .validate_for_summarize(contacts, "contacts")
  if (!check$valid) return(tibble::tibble(bank_id = integer()))

  check$data |>
    dplyr::group_by(.data$bank_id) |>
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
  check <- .validate_for_summarize(
    credit_summary,
    "credit_summary",
    required_cols = c("available_credits", "released_credits", "potential_credits", "credit_classification"),
    fill_type = "real"
  )
  if (!check$valid) return(tibble::tibble(bank_id = integer()))

  check$data |>
    dplyr::group_by(.data$bank_id) |>
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
  check <- .validate_for_summarize(transactions, "transactions")
  if (!check$valid) return(tibble::tibble(bank_id = integer()))

  transactions <- check$data
  # Ensure bank_id is integer for consistent typing across joins
  transactions$bank_id <- as.integer(transactions$bank_id)

  # Helper: safely parse dates (handles various formats)
  safe_parse_date <- function(x) {
    if (is.null(x) || all(is.na(x))) return(as.Date(NA))
    if (inherits(x, "Date")) return(x)
    parsed <- .try_parse_date(x)
    if (!is.null(parsed)) parsed else as.Date(NA)
  }

  # Helper: safely calculate months between dates
  months_diff <- function(from, to = Sys.Date()) {
    if (is.null(from) || all(is.na(from))) return(NA_real_)
    from_date <- safe_parse_date(from)
    if (all(is.na(from_date))) return(NA_real_)
    as.numeric(difftime(to, max(from_date, na.rm = TRUE), units = "days")) / 30.44
  }

  # Helper: safely calculate years between dates
  years_diff <- function(from, to) {
    if (is.null(from) || is.null(to) || all(is.na(from)) || all(is.na(to))) return(NA_real_)
    from_date <- safe_parse_date(from)
    to_date <- safe_parse_date(to)
    if (all(is.na(from_date)) || all(is.na(to_date))) return(NA_real_)
    as.numeric(difftime(max(to_date, na.rm = TRUE), min(from_date, na.rm = TRUE), units = "days")) / 365.25
  }

  transactions |>
    dplyr::group_by(.data$bank_id) |>
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
        parsed <- safe_parse_date(transaction_date)
        if (all(is.na(parsed))) as.Date(NA) else min(parsed, na.rm = TRUE)
      } else {
        as.Date(NA)
      },
      last_transaction_date = if ("transaction_date" %in% names(dplyr::cur_data())) {
        parsed <- safe_parse_date(transaction_date)
        if (all(is.na(parsed))) as.Date(NA) else max(parsed, na.rm = TRUE)
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
  check <- .validate_for_summarize(
    credit_releases,
    "credit_releases",
    required_cols = c("credits"),
    fill_type = "real"
  )
  if (!check$valid) return(tibble::tibble(bank_id = integer()))

  credit_releases <- check$data

  # Handle anticipated_release_date specially (needs date parsing)
  if (!"anticipated_release_date" %in% names(credit_releases)) {
    credit_releases$anticipated_release_date <- as.Date(NA)
  } else if (is.character(credit_releases$anticipated_release_date)) {
    # Parse date column if it's character (MM/DD/YYYY format from CSV)
    credit_releases$anticipated_release_date <- as.Date(
      credit_releases$anticipated_release_date,
      format = "%m/%d/%Y"
    )
  }

  # Check for stale data and warn user
  today <- Sys.Date()
  total_releases <- nrow(credit_releases)
  stale_releases <- sum(credit_releases$anticipated_release_date < today, na.rm = TRUE)
  stale_pct <- round(100 * stale_releases / total_releases, 0)

  if (stale_pct > 50) {
    cli::cli_alert_warning(
      "Credit releases data is stale: {stale_pct}% of dates have passed ({stale_releases}/{total_releases})"
    )
  }

  # Summarize with both all releases and future-only metrics
  credit_releases |>
    dplyr::group_by(.data$bank_id) |>
    dplyr::summarise(
      # All releases (including past - for historical context)
      total_anticipated_credits = sum(credits, na.rm = TRUE),
      n_total_releases = dplyr::n(),

      # Future releases only (truly upcoming)
      n_future_releases = sum(anticipated_release_date >= today, na.rm = TRUE),
      future_anticipated_credits = sum(
        credits[anticipated_release_date >= today],
        na.rm = TRUE
      ),

      # Timeline for future releases
      next_release_date = {
        future_dates <- anticipated_release_date[anticipated_release_date >= today]
        if (length(future_dates[!is.na(future_dates)]) > 0) {
          min(future_dates, na.rm = TRUE)
        } else {
          as.Date(NA)
        }
      },
      latest_release_date = max(anticipated_release_date, na.rm = TRUE),

      # Near-term (next year) - only future releases
      credits_releasing_next_year = {
        next_year <- today + 365
        sum(
          credits[anticipated_release_date >= today & anticipated_release_date <= next_year],
          na.rm = TRUE
        )
      },

      .groups = "drop"
    )
}


#' Summarize Public Notices for Banks Dataframe
#' @keywords internal
#' @noRd
.summarize_public_notices <- function(public_notices) {
  check <- .validate_for_summarize(public_notices, "public_notices")
  if (!check$valid) return(tibble::tibble(bank_id = integer()))

  public_notices <- check$data

  # Handle create_date specially (needs date parsing)
  if (!"create_date" %in% names(public_notices)) {
    public_notices$create_date <- as.Date(NA)
  } else if (!inherits(public_notices$create_date, "Date")) {
    # Convert to Date if not already (handles character dates from CSV)
    parsed <- .try_parse_date(public_notices$create_date)
    public_notices$create_date <- if (!is.null(parsed)) parsed else as.Date(NA)
  }

  public_notices |>
    dplyr::group_by(.data$bank_id) |>
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

#' Pivot credits to wide format (Master Summary)
#' 
#' Pivots credit classification data to wide format (one row per bank),
#' creating columns like `wetland_available`, `stream_released`, etc.
#' 
#' @param credit_data Long-format credit data
#' @return Tibble with one row per bank and multiple credit columns
#' @keywords internal
#' @noRd
.pivot_credits_wide <- function(credit_data) {
  if (is.null(credit_data) || nrow(credit_data) == 0) {
    return(tibble::tibble(bank_id = integer()))
  }

  # Validate required columns
  if (!all(c("bank_id", "credit_classification", "available_credits") %in% names(credit_data))) {
    return(tibble::tibble(bank_id = integer()))
  }

  tryCatch({
    # Clean up classifications for column names
    # Simplify common terms to keep names reasonable
    # e.g. "Palustrine Forested Wetland" -> "palustrine_forested_wetland"
    
    wide_prep <- credit_data |>
      dplyr::mutate(
        # Create cleaner slugs
        slug = tolower(credit_classification),
        slug = gsub(" wetland", "", slug), # Remove redundant "wetland" if desired? Maybe keep it.
        slug = gsub("[^a-z0-9]", "_", slug),
        slug = gsub("_+", "_", slug),
        slug = gsub("^_|_$", "", slug)
      ) |>
      # Group by bank and slug to handle duplicates (sum them up if any)
      dplyr::group_by(bank_id, slug) |>
      dplyr::summarise(
        avail = sum(available_credits, na.rm = TRUE),
        released = sum(released_credits, na.rm = TRUE),
        potential = sum(potential_credits, na.rm = TRUE),
        .groups = "drop"
      )

    # Pivot
    wide <- wide_prep |>
      tidyr::pivot_wider(
        id_cols = "bank_id",
        names_from = "slug",
        values_from = c("avail", "released", "potential"),
        names_glue = "{slug}_{.value}",
        values_fill = 0
      )
    
    wide
  }, error = function(e) {
    cli::cli_alert_warning("Failed to pivot credits: {e$message}")
    tibble::tibble(bank_id = integer())
  })
}

