# R/harmonization-merge.R
# Data merging utilities with column preservation
# Split from R/discrepancy-handling.R (1,052 lines → focused modules)

#' Merge data frames preserving all columns from both sources
#'
#' Uses coalesce logic to combine matching columns and preserves
#' columns unique to each source. Priority source fills values first.
#'
#' @param df1 First data frame (priority source)
#' @param df2 Second data frame (secondary source)
#' @param by Column(s) to join on. Default "bank_id".
#' @param suffix Suffixes for duplicate columns. Default c("_1", "_2").
#' @param preserve_from_second Character vector of column names that should
#'   keep the second source's value instead of coalescing (df1 priority).
#'   Use this for columns that are unique/authoritative in the second source.
#'
#' @return A merged data frame with all columns from both sources
#' @keywords internal
.merge_preserving_columns <- function(df1, df2, by = "bank_id", suffix = c("_1", "_2"),
                                       preserve_from_second = character(0)) {
  if (is.null(df1) || nrow(df1) == 0) return(df2)
  if (is.null(df2) || nrow(df2) == 0) return(df1)

  # STEP 1: Normalize columns using registry
  # This handles semantic equivalents and standardizes naming
  df1 <- .normalize_columns(df1)
  df2 <- .normalize_columns(df2)

  # STEP 2: Normalize column names to lowercase for consistent merging
  names(df1) <- tolower(names(df1))
  names(df2) <- tolower(names(df2))
  by <- tolower(by)

  # Remove columns that are just logical indicators for nested data
  # (these indicate presence of data, not actual data)
  logical_indicator_cols <- c("bank_footprint", "service_areas", "bank_sponsors",
                               "bank_pocs", "bank_managers", "bank_irt_members",
                               "bank_other_contacts", "ledger")
  for (col in logical_indicator_cols) {
    if (col %in% names(df1) && is.logical(df1[[col]])) {
      df1 <- df1 |> dplyr::select(-dplyr::all_of(col))
    }
    if (col %in% names(df2) && is.logical(df2[[col]])) {
      df2 <- df2 |> dplyr::select(-dplyr::all_of(col))
    }
  }

  # Ensure join column exists in both
  if (!all(by %in% names(df1)) || !all(by %in% names(df2))) {
    cli::cli_alert_warning("Join column '{by}' not found in both data frames")
    return(df1)
  }

  # Get columns unique to each source (excluding join columns)
  cols1_only <- setdiff(names(df1), c(names(df2), by))
  cols2_only <- setdiff(names(df2), c(names(df1), by))
  common_cols <- setdiff(intersect(names(df1), names(df2)), by)

  # Full join to preserve all rows
  merged <- dplyr::full_join(df1, df2, by = by, suffix = suffix)

  # Normalize preserve_from_second to lowercase
  preserve_from_second <- tolower(preserve_from_second)

  # Coalesce common columns (df1 takes priority, unless in preserve_from_second)
  for (col in common_cols) {
    col1 <- paste0(col, suffix[1])
    col2 <- paste0(col, suffix[2])

    if (col1 %in% names(merged) && col2 %in% names(merged)) {
      # Handle type mismatches by converting to character if needed
      val1 <- merged[[col1]]
      val2 <- merged[[col2]]

      # If types differ, convert both to character
      if (class(val1)[1] != class(val2)[1]) {
        val1 <- as.character(val1)
        val2 <- as.character(val2)
      }

      # Check if this column should be preserved from second source
      if (col %in% preserve_from_second) {
        # Coalesce with df2 priority (second source's value preferred)
        merged[[col]] <- dplyr::coalesce(val2, val1)
      } else {
        # Default: Coalesce with df1 priority (first source's value preferred)
        merged[[col]] <- dplyr::coalesce(val1, val2)
      }
      merged <- merged |> dplyr::select(-dplyr::all_of(c(col1, col2)))
    }
  }

  # Handle columns that exist only in df2 but are in preserve_from_second
  # (these would have come through as cols2_only and are already present)

  merged
}

#' Merge multiple data frames preserving all columns
#'
#' @param df_list Named list of data frames to merge
#' @param by Column(s) to join on
#' @param priority_order Vector of names specifying merge priority
#'
#' @return A merged data frame
#' @keywords internal
.merge_multiple_sources <- function(df_list, by = "bank_id", priority_order = NULL) {
  # Filter out NULL/empty data frames
  df_list <- purrr::keep(df_list, ~ !is.null(.) && nrow(.) > 0)

  if (length(df_list) == 0) return(NULL)
  if (length(df_list) == 1) return(df_list[[1]])

  # Order by priority if specified
  if (!is.null(priority_order)) {
    available <- intersect(priority_order, names(df_list))
    others <- setdiff(names(df_list), priority_order)
    df_list <- df_list[c(available, others)]
  }

  # Sequentially merge
  result <- df_list[[1]]
  for (i in 2:length(df_list)) {
    result <- .merge_preserving_columns(result, df_list[[i]], by = by)
  }

  result
}
