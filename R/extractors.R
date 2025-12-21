#' @keywords internal
.rb_extract_generic <- function(bank, col_name) {
  val <- bank[[col_name]]
  
  # Handle list-column from tibble
  if (is.list(val) && length(val) == 1 && (tibble::is_tibble(bank) || is.data.frame(bank))) {
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

#' Extract ledger from bank object
#'
#' @param bank A tibble returned by `rb_get_bank()`
#' @return A tibble containing the ledger, or an empty tibble if not found.
#' @export
rb_extract_ledger <- function(bank) {
  .rb_extract_generic(bank, "ledger")
}

#' Extract contacts from bank object
#'
#' @param bank A tibble returned by `rb_get_bank()`
#' @return A tibble containing contacts.
#' @export
rb_extract_contacts <- function(bank) {
  .rb_extract_generic(bank, "contacts")
}

#' Extract sponsors from bank object
#'
#' @param bank A tibble returned by `rb_get_bank()`
#' @return A tibble containing sponsors.
#' @export
rb_extract_sponsors <- function(bank) {
  .rb_extract_generic(bank, "bank_sponsors")
}

#' Extract POCs from bank object
#'
#' @param bank A tibble returned by `rb_get_bank()`
#' @return A tibble containing POCs.
#' @export
rb_extract_pocs <- function(bank) {
  .rb_extract_generic(bank, "bank_pocs")
}

#' Extract managers from bank object
#'
#' @param bank A tibble returned by `rb_get_bank()`
#' @return A tibble containing managers.
#' @export
rb_extract_managers <- function(bank) {
  .rb_extract_generic(bank, "bank_managers")
}

#' Extract IRT members from bank object
#'
#' @param bank A tibble returned by `rb_get_bank()`
#' @return A tibble containing IRT members.
#' @export
rb_extract_irt_members <- function(bank) {
  .rb_extract_generic(bank, "bank_irt_members")
}

#' Extract other contacts from bank object
#'
#' @param bank A tibble returned by `rb_get_bank()`
#' @return A tibble containing other contacts.
#' @export
rb_extract_other_contacts <- function(bank) {
  .rb_extract_generic(bank, "bank_other_contacts")
}


#' Flatten bank details to a single-row tibble
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' `rb_get_bank()` now returns a tibble by default. This function is kept for
#' backward compatibility but simply returns the input if it's already a tibble,
#' or selects atomic columns.
#'
#' @param bank A tibble returned by `rb_get_bank()`
#' @return A single-row tibble.
#' @export
rb_flatten_bank <- function(bank) {
  if (tibble::is_tibble(bank) || is.data.frame(bank)) {
    # Select only atomic columns (drop list-columns)
    return(dplyr::select(bank, tidyselect::where(is.atomic)))
  }
  
  # Legacy list support
  simple_fields <- purrr::keep(bank, function(x) {
    !is.list(x) && length(x) == 1
  })
  
  tibble::as_tibble(simple_fields)
}
