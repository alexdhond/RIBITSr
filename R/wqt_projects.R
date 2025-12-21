# R/wqt_projects.R

#' List WQT projects
#'
#' Retrieves a list of Water Quality Trading (WQT) projects from RIBITS,
#' optionally filtered by various parameters.
#'
#' @param state Character. Two-letter state abbreviation (e.g., "FL").
#' @param district Character. USACE district code.
#'
#' @return A tibble containing WQT project information.
#' @export
#' @examples
#' \dontrun{
#' # List all WQT projects
#' all_wqt <- rb_list_wqt_projects()
#'
#' # Filter by state
#' ca_wqt <- rb_list_wqt_projects(state = "CA")
#' }
rb_list_wqt_projects <- function(state = NULL, district = NULL) {
  params <- list()
  if (!is.null(state)) params$state <- state
  if (!is.null(district)) params$district <- district

  resp <- rb_request("wqt_project_list/", params)
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

#' Get WQT project details
#'
#' Retrieves detailed information for a specific Water Quality Trading project.
#'
#' Note: Based on API exploration (see ribits_field_reference.md), the
#' wqt_project_data endpoint may require different parameters than other
#' endpoints. This function may need adjustment based on actual API behavior.
#'
#' @param project_id Integer. The unique RIBITS WQT project identifier.
#'
#' @return A list containing WQT project attributes.
#' @export
#' @examples
#' \dontrun{
#' # Get a specific WQT project
#' wqt_project <- rb_get_wqt_project(1)
#' }
rb_get_wqt_project <- function(project_id) {
  if (missing(project_id)) {
    rlang::abort("project_id is required",
                 class = "ribits_error_missing_param")
  }

  params <- list(project_id = project_id)

  tryCatch({
    resp <- rb_request("wqt_project_data/", params)
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
  }, error = function(e) {
    cli::cli_alert_warning(
      "WQT project data endpoint may require different parameters. Error: {e$message}"
    )
    rlang::abort(
      "Failed to retrieve WQT project data. This endpoint may not be fully supported.",
      class = "ribits_error_wqt"
    )
  })
}

#' Get details for multiple WQT projects
#'
#' Retrieves detailed information for multiple WQT projects, handling
#' rate limiting and errors.
#'
#' @param project_ids Integer vector. Unique RIBITS WQT project identifiers.
#' @param progress Logical. Show progress bar? Default TRUE.
#'
#' @return A tibble containing combined WQT project information.
#' @export
#' @examples
#' \dontrun{
#' # Get multiple WQT projects
#' projects <- rb_get_wqt_projects(c(1, 2, 3))
#' }
rb_get_wqt_projects <- function(project_ids, progress = TRUE) {
  if (length(project_ids) == 0) {
    return(tibble::tibble())
  }

  if (progress) {
    bar_id <- cli::cli_progress_bar("Downloading WQT projects",
                                    total = length(project_ids))
  }

  results <- purrr::map(project_ids, function(id) {
    if (progress) cli::cli_progress_update(id = bar_id)

    # Rate limiting
    Sys.sleep(0.5)

    tryCatch({
      rb_get_wqt_project(id)
    }, error = function(e) {
      cli::cli_alert_warning("Failed to retrieve WQT project {id}: {e$message}")
      return(NULL)
    })
  })

  if (progress) cli::cli_progress_done(id = bar_id)

  # Combine results
  dplyr::bind_rows(results)
}
