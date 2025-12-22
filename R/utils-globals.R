# R/utils-globals.R
# Global variable bindings to avoid R CMD check NOTEs
# These variables are used in dplyr/tidyverse NSE contexts

#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom stats na.omit
#' @importFrom tidyselect where
NULL

# Suppress R CMD check NOTEs about undefined global variables
# These are column names used in dplyr operations with .data pronoun
utils::globalVariables(c(
  # Column names used in dplyr operations
  "bank_id",
  "Bank ID",
  "name",
  "Name",
  "State List",
  "name_normalized",
  ".name_normalized",
  "bank_id_exact",
  "bank_id_fuzzy",
  "fuzzy_score",
  "transaction_id",
  "resolved_source",
  "source1",
  "value1",
  "value2",
  "severity",
  "resolved_value",
  "field",
  "count"
))
