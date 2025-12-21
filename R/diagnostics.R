#' Analyze missing data in bank object
#'
#' Reports on which fields are missing (NA or NULL) in a bank object.
#'
#' @param bank A tibble returned by `rb_get_bank()`
#' @return A tibble with columns: field, status (present, missing_na, missing_null), class
#' @export
rb_scan_missing <- function(bank) {
  purrr::map_dfr(names(bank), function(nm) {
    val <- bank[[nm]]
    
    # If it's a list-column in a 1-row tibble, extract the element
    if (is.list(val) && length(val) == 1 && (tibble::is_tibble(bank) || is.data.frame(bank))) {
      val <- val[[1]]
    }
    
    status <- "present"
    if (is.null(val)) {
      status <- "missing_null"
    } else if (all(is.na(val))) {
      status <- "missing_na"
    }
    
    tibble::tibble(
      field = nm,
      status = status,
      class = if (is.null(val)) "NULL" else class(val)[1]
    )
  })
}
