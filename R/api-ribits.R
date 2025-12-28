# R/api-ribits.R
# RIBITS API data retrieval with flexible filtering
# Split from R/unified-api.R (627 lines → focused modules)

# R/unified-api.R
# Unified, streamlined API for RIBITSr package
# Consolidates multiple similar functions into generic, flexible functions

# =============================================================================
# UNIFIED DATA RETRIEVAL FUNCTION
# =============================================================================

#' Get RIBITS data with flexible filtering (Advanced)
#'
#' @description
#' Advanced API function for power users who need fine-grained control over
#' data retrieval from the RIBITS API.
#'
#' **For most users:** Use `ribits()` instead, which automatically harmonizes
#' data from multiple sources and handles all complexity.
#'
#' **Use `rb_get()` when you need:**
#' - Raw API data without harmonization
#' - Specific filters not available in `ribits()` (field_office, noaa_region, kind)
#' - WQT project data
#' - Fine-grained control over which components to fetch
#'
#' This function queries the RIBITS API directly and returns data as-is, without
#' the multi-source harmonization that `ribits()` provides.
#'
#' @param type Data type to retrieve. One of: "banks", "ilf", "umbrellas", "wqt"
#' @param id Optional. Specific ID(s) to retrieve detailed data for.
#' @param state State filter (e.g., "CA", "OR", "TX")
#' @param district USACE district filter (e.g., "Portland", "Sacramento")
#' @param field_office FWS field office filter
#' @param noaa_region NOAA region filter
#' @param kind Bank type filter: "Standard", "ILF", "Umbrella", "NRDA" (banks only)
#' @param status Bank status filter: "Approved", "Pending", "Terminated"
#' @param ledger Include ledger/transaction data? Default FALSE.
#' @param footprint Include footprint geometry? Default FALSE.
#' @param service_area Include service area geometry? Default FALSE.
#' @param contacts Include contact information? Default FALSE.
#' @return A tibble or list depending on what's requested
#' @seealso [ribits()] for the recommended user-friendly interface
#' @export
#' @examples
#' \dontrun{
#' # Recommended: Use ribits() for most tasks
#' ca_banks <- ribits(state = "CA")
#'
#' # Advanced: Use rb_get() for specific filters
#' # List all Oregon banks from API only
#' rb_get("banks", state = "OR")
#'
#' # Get specific bank with all data (API only, no harmonization)
#' rb_get("banks", id = 17, ledger = TRUE, footprint = TRUE)
#'
#' # Filter by field office (not available in ribits())
#' rb_get("banks", field_office = "Sacramento")
#'
#' # Get WQT projects (not available in ribits())
#' rb_get("wqt", state = "CA")
#'
#' # Show available options
#' rb_get()
#' }
rb_get <- function(type = NULL,
                   id = NULL,
                   state = NULL,
                   district = NULL,
                   field_office = NULL,
                   noaa_region = NULL,
                   kind = NULL,
                   status = NULL,
                   ledger = FALSE,
                   footprint = FALSE,
                   service_area = FALSE,
                   contacts = FALSE) {
  
  available_types <- c("banks", "ilf", "umbrellas", "wqt")
  
  # If no type, show help
 if (is.null(type)) {
    cli::cli_h2("RIBITS Data Types")
    cli::cli_bullets(c(
      "*" = "{.field banks}: Mitigation banks (~4,700+)",
      "*" = "{.field ilf}: In-Lieu Fee programs (~170+)",
      "*" = "{.field umbrellas}: Umbrella instruments (~400+)",
      "*" = "{.field wqt}: Water Quality Trading projects (~380+)"
    ))
    cli::cli_h3("Available Filters")
    cli::cli_bullets(c(
      "*" = "{.arg state}: State code (e.g., 'CA', 'OR')",
      "*" = "{.arg district}: USACE district (e.g., 'Portland')",
      "*" = "{.arg field_office}: FWS field office",
      "*" = "{.arg noaa_region}: NOAA region",
      "*" = "{.arg kind}: Bank type (Standard/ILF/Umbrella/NRDA)",
      "*" = "{.arg status}: Approval status"
    ))
    cli::cli_h3("Data Options")
    cli::cli_bullets(c(
      "*" = "{.arg ledger}: Include transaction history",
      "*" = "{.arg footprint}: Include footprint geometry",
      "*" = "{.arg service_area}: Include service area geometry",
      "*" = "{.arg contacts}: Include contact information"
    ))
    cli::cli_text("")
    cli::cli_alert_info("Example: {.code rb_get(\"banks\", state = \"OR\", ledger = TRUE)}")
    return(invisible(NULL))
  }
  
  # Normalize type
  type <- tolower(type)
  type <- switch(type,
    "bank" = "banks",
    "ilf_programs" = "ilf",
    "ilf_program" = "ilf",
    "program" = "ilf",
    "programs" = "ilf",
    "umbrella" = "umbrellas",
    "wqt_projects" = "wqt",
    "wqt_project" = "wqt",
    type
  )
  
  if (!(type %in% available_types)) {
    cli::cli_abort(c(
      "Unknown type: {type}",
      "i" = "Available: {paste(available_types, collapse = ', ')}"
    ))
  }
  
  # Map to internal type names
  internal_type <- switch(type,
    banks = "banks",
    ilf = "ilf_programs",
    umbrellas = "umbrellas",
    wqt = "wqt_projects"
  )
  
  # If specific ID(s) requested, get detailed data
  if (!is.null(id)) {
    return(.rb_get_by_id(internal_type, id, ledger, footprint, service_area, contacts))
  }
  
  # Otherwise, list with filters
  .rb_list_filtered(internal_type, state, district, field_office, noaa_region, kind, status)
}


#' Internal: Get data by ID (calls generic functions directly)
#' @keywords internal
#' @noRd
.rb_get_by_id <- function(type, id, ledger, footprint, service_area, contacts) {
  # Build show_params based on type
  config <- rb_get_endpoint_config(type)
  
  show_params <- list()
  if ("ledger" %in% config$show_params) {
    show_params$show_ledger <- rb_bool_to_text(ledger)
  }
  if ("footprint" %in% config$show_params) {
    show_params$show_footprint <- rb_bool_to_text(footprint)
  }
  if ("service_area" %in% config$show_params) {
    show_params$show_service_area <- rb_bool_to_text(service_area)
  }
  if ("contacts" %in% config$show_params) {
    show_params$show_contacts <- rb_bool_to_text(contacts)
  }
  
  # Single ID - get and extract components
  if (length(id) == 1) {
    raw <- rb_get_generic(type, id, show_params = show_params)
    if (nrow(raw) == 0) return(list())
    
    # Extract components
    out <- list(
      summary = rb_flatten_record(raw),
      ledger = if (ledger) rb_extract_ledger(raw) else NULL,
      contacts = if (contacts) rb_extract_contacts(raw) else NULL,
      footprint = if (footprint) rb_extract_footprint(raw) else NULL,
      service_area = if (service_area) rb_extract_service_area(raw) else NULL,
      sponsors = rb_extract_sponsors(raw),
      pocs = rb_extract_pocs(raw),
      managers = rb_extract_managers(raw),
      irt_members = rb_extract_irt_members(raw),
      other_contacts = rb_extract_other_contacts(raw)
    )
    
    # Add type-specific extractions
    if (type == "ilf_programs") out$program_sites <- rb_extract_program_sites(raw)
    if (type == "umbrellas") out$umbrella_sites <- rb_extract_umbrella_sites(raw)
    
    # Remove NULL entries
    return(out[!sapply(out, is.null)])
  }
  
  # Multiple IDs - use generic batch function
  .rb_get_multiple_by_id(type, id, show_params)
}

#' Internal: Get multiple items by ID (with parallel chunked requests)
#' @keywords internal
#' @noRd
.rb_get_multiple_by_id <- function(type, ids, show_params, progress = TRUE,
                                    chunk_size = 10, max_concurrent = 5) {
  if (length(ids) == 0) return(tibble::tibble())
  
  config <- rb_get_endpoint_config(type)
  
  # Build all request objects upfront
  reqs <- lapply(ids, function(id) {
    .rb_build_detail_request(type, id, show_params)
  })
  
  if (progress) {
    est_secs <- length(ids) * 1.0  # ~1 sec per bank estimate
    est_str <- if (est_secs > 60) paste0(round(est_secs / 60, 1), " min") else paste0(round(est_secs), " sec")
    cli::cli_alert_info("Fetching {length(ids)} {config$type_name} in parallel (~{est_str} estimated)...")
  }
  
  # Process in chunks to be polite to the server
  chunks <- split(seq_along(reqs), ceiling(seq_along(reqs) / chunk_size))
  all_responses <- list()
  start_time <- Sys.time()
  
  for (i in seq_along(chunks)) {
    chunk_idx <- chunks[[i]]
    chunk_reqs <- reqs[chunk_idx]
    
    if (progress && length(chunks) > 1) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      if (i > 1 && elapsed > 0) {
        rate <- (i - 1) / elapsed * chunk_size  # items per second
        remaining <- (length(chunks) - i + 1) * chunk_size / max(rate, 0.1)
        remaining_str <- if (remaining > 60) paste0(round(remaining / 60, 1), " min") else paste0(round(remaining), " sec")
        cli::cli_progress_step("Processing chunk {i}/{length(chunks)} (~{remaining_str} remaining)...")
      } else {
        cli::cli_progress_step("Processing chunk {i}/{length(chunks)}...")
      }
    }
    
    # Perform parallel requests within chunk
    chunk_responses <- tryCatch({
      httr2::req_perform_parallel(
        chunk_reqs,
        on_error = "continue",
        progress = FALSE
      )
    }, error = function(e) {
      # Fallback to sequential if parallel fails
      lapply(chunk_reqs, function(req) {
        tryCatch(httr2::req_perform(req), error = function(e) NULL)
      })
    })
    
    all_responses <- c(all_responses, chunk_responses)
    
    # Small delay between chunks to be polite
    if (i < length(chunks)) Sys.sleep(0.5)
  }
  
  # Parse responses
  results <- purrr::map2(all_responses, ids, function(resp, id) {
    tryCatch({
      if (is.null(resp) || inherits(resp, "error")) return(NULL)
      
      raw <- .rb_parse_detail_response(resp)
      if (is.null(raw) || nrow(raw) == 0) return(NULL)
      
      # Note: We preserve original column names here - clean_names() is applied at final output
      
      list(
        summary = rb_flatten_record(raw),
        ledger = rb_extract_ledger(raw),
        contacts = rb_extract_contacts(raw),
        footprint = rb_extract_footprint(raw),
        service_area = rb_extract_service_area(raw)
      )
    }, error = function(e) {
      if (progress) cli::cli_alert_warning("Failed to parse {config$type_singular} {id}")
      NULL
    })
  })
  
  if (progress) cli::cli_alert_success("Completed {sum(!sapply(results, is.null))}/{length(ids)} requests")
  
  # Combine results by component
  results <- results[!sapply(results, is.null)]
  if (length(results) == 0) return(list())
  
  components <- unique(unlist(lapply(results, names)))
  out <- list()
  
  for (comp in components) {
    items <- lapply(results, function(x) x[[comp]])
    items <- items[!sapply(items, is.null)]
    if (length(items) > 0) {
      if (inherits(items[[1]], "sf")) {
        out[[comp]] <- do.call(rbind, items)
      } else {
        out[[comp]] <- dplyr::bind_rows(items)
      }
    }
  }
  
  out
}

#' Build a detail request object (for parallel execution)
#' @keywords internal
#' @noRd
.rb_build_detail_request <- function(type, id, show_params) {
  config <- rb_get_endpoint_config(type)
  
  # Build URL - use get_endpoint (not detail_endpoint)
  url <- paste0(rb_base_url(), config$get_endpoint)
  
  # Build parameters
  params <- c(
    stats::setNames(list(id), config$id_param),
    show_params
  )
  
  q_json <- jsonlite::toJSON(params, auto_unbox = TRUE)
  
  httr2::request(url) |>
    httr2::req_user_agent("RIBITSr R package") |>
    httr2::req_url_query(q = q_json) |>
    httr2::req_timeout(30)
}

#' Parse a detail response
#' @keywords internal
#' @noRd
.rb_parse_detail_response <- function(resp) {
  if (is.null(resp)) return(NULL)
  
  tryCatch({
    body <- httr2::resp_body_string(resp)
    data <- jsonlite::fromJSON(body, flatten = TRUE)
    
    # Extract items (handle both ITEMS and items)
    items_key <- intersect(c("ITEMS", "items"), names(data))
    if (length(items_key) > 0) {
      items <- data[[items_key[1]]]
      if (is.data.frame(items) && nrow(items) > 0) {
        return(tibble::as_tibble(items))
      }
    }
    NULL
  }, error = function(e) NULL)
}


#' Internal: List with filters (calls generic functions directly)
#' @keywords internal
#' @noRd
.rb_list_filtered <- function(type, state, district, field_office, noaa_region, kind, status) {
  # Build params based on type config
  params <- list()
  if (!is.null(state)) params$state <- state
  if (!is.null(district)) params$district <- district
  if (!is.null(field_office)) params$fieldoffice <- field_office
  if (!is.null(noaa_region)) params$noaaregion <- noaa_region
  if (!is.null(kind) && type == "banks") params$kind_of_bank <- kind
  
  # Call generic list function
  result <- do.call(rb_list_generic, c(list(type = type), params))
  
  # Post-filter by status if requested
 if (!is.null(status) && !is.null(result) && nrow(result) > 0) {
    status_col <- grep("status", names(result), ignore.case = TRUE, value = TRUE)
    if (length(status_col) > 0) {
      result <- result[grepl(status, result[[status_col[1]]], ignore.case = TRUE), ]
    }
  }
  
  result
}




