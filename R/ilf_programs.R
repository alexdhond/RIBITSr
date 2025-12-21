# R/ilf_programs.R

#' List ILF programs
#'
#' Retrieves a list of In-Lieu Fee (ILF) programs from RIBITS, optionally
#' filtered by various parameters.
#'
#' @param state Character. Two-letter state abbreviation (e.g., "FL").
#' @param district Character. USACE district code.
#' @param field_office Character. FWS field office name.
#' @param noaa_region Character. NOAA/NMFS region name.
#'
#' @return A tibble containing ILF program information.
#' @export
#' @examples
#' \dontrun{
#' # List all ILF programs
#' all_ilf <- rb_list_ilf_programs()
#'
#' # Filter by state
#' ca_ilf <- rb_list_ilf_programs(state = "CA")
#'
#' # Filter by district
#' district_ilf <- rb_list_ilf_programs(district = "Sacramento")
#' }
rb_list_ilf_programs <- function(state = NULL, district = NULL,
                                  field_office = NULL, noaa_region = NULL) {
  params <- list()
  if (!is.null(state)) params$state <- state
  if (!is.null(district)) params$district <- district
  if (!is.null(field_office)) params$fieldoffice <- field_office
  if (!is.null(noaa_region)) params$noaaregion <- noaa_region

  resp <- rb_request("ilf_program_list/", params)
  data <- rb_parse_response(resp)

  # Handle ORDS response structure
  if ("items" %in% names(data)) {
    tibble::as_tibble(data$items) |> janitor::clean_names()
  } else if ("ITEMS" %in% names(data)) {
    tibble::as_tibble(data$ITEMS) |> janitor::clean_names()
  } else {
    tibble::as_tibble(data) |> janitor::clean_names()
  }
}

#' Get ILF program details
#'
#' Retrieves detailed information for a specific In-Lieu Fee program.
#'
#' @param program_id Integer. The unique RIBITS ILF program identifier.
#' @param service_area Logical. Include service area geometries? Default TRUE.
#' @param contacts Logical. Include contact information? Default TRUE.
#' @param ledger Logical. Include transaction ledger? Default TRUE.
#'
#' @return A list containing ILF program attributes and requested nested data.
#' @export
#' @examples
#' \dontrun{
#' # Get a specific ILF program with all details
#' ilf_program <- rb_get_ilf_program(1)
#'
#' # Get without ledger
#' ilf_basic <- rb_get_ilf_program(1, ledger = FALSE)
#' }
rb_get_ilf_program <- function(program_id, service_area = TRUE,
                                contacts = TRUE, ledger = TRUE) {
  if (missing(program_id)) {
    rlang::abort("program_id is required", class = "ribits_error_missing_param")
  }

  params <- list(
    program_id = program_id,
    show_service_area = tolower(as.character(service_area)),
    show_contacts = tolower(as.character(contacts)),
    show_ledger = tolower(as.character(ledger))
  )

  resp <- rb_request("ilf_program_data/", params)
  data <- rb_parse_response(resp)

  # Handle ITEMS wrapper if present
  if ("items" %in% names(data)) {
    data <- data$items
  } else if ("ITEMS" %in% names(data)) {
    data <- data$ITEMS
  }

  # If it's a data frame, return as tibble
  if (is.data.frame(data)) {
    return(tibble::as_tibble(data) |> janitor::clean_names())
  }

  # If it's a list, convert to 1-row tibble with list-columns for nested data
  if (is.list(data)) {
    processed_data <- lapply(data, function(x) {
      if (is.recursive(x) || length(x) != 1) {
        list(x)
      } else {
        x
      }
    })
    return(tibble::as_tibble(processed_data) |> janitor::clean_names())
  }

  tibble::as_tibble(data) |> janitor::clean_names()
}

#' Get details for multiple ILF programs
#'
#' Retrieves detailed information for multiple ILF programs, handling
#' rate limiting and errors.
#'
#' @param program_ids Integer vector. Unique RIBITS ILF program identifiers.
#' @param ... Additional arguments passed to `rb_get_ilf_program()`.
#' @param progress Logical. Show progress bar? Default TRUE.
#'
#' @return A tibble containing combined ILF program information.
#' @export
#' @examples
#' \dontrun{
#' # Get multiple ILF programs
#' programs <- rb_get_ilf_programs(c(1, 2, 3))
#' }
rb_get_ilf_programs <- function(program_ids, ..., progress = TRUE) {
  if (length(program_ids) == 0) {
    return(tibble::tibble())
  }

  if (progress) {
    bar_id <- cli::cli_progress_bar("Downloading ILF programs",
                                    total = length(program_ids))
  }

  results <- purrr::map(program_ids, function(id) {
    if (progress) cli::cli_progress_update(id = bar_id)

    # Rate limiting
    Sys.sleep(0.5)

    tryCatch({
      rb_get_ilf_program(id, ...)
    }, error = function(e) {
      cli::cli_alert_warning("Failed to retrieve ILF program {id}: {e$message}")
      return(NULL)
    })
  })

  if (progress) cli::cli_progress_done(id = bar_id)

  # Combine results
  dplyr::bind_rows(results)
}

#' Extract program sites from ILF program object
#'
#' @param ilf_program A tibble returned by `rb_get_ilf_program()`
#' @return A tibble containing program sites.
#' @export
rb_extract_program_sites <- function(ilf_program) {
  val <- ilf_program$program_sites

  # Handle list-column from tibble
  if (is.list(val) && length(val) == 1 &&
      (tibble::is_tibble(ilf_program) || is.data.frame(ilf_program))) {
    val <- val[[1]]
  }

  if (is.null(val) || all(is.na(val))) {
    return(tibble::tibble())
  }

  # If it's already a data frame, return it as tibble
  if (is.data.frame(val)) {
    return(tibble::as_tibble(val) |> janitor::clean_names())
  }

  # If it's a list, try to bind rows
  if (is.list(val)) {
    return(dplyr::bind_rows(val) |> janitor::clean_names())
  }

  return(tibble::tibble())
}
