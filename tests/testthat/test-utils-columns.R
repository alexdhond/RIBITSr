test_that(".col_exists correctly identifies existing columns", {
  df <- tibble::tibble(BankID = 1:3, BankName = c("A", "B", "C"))

  # Case-insensitive matches
  expect_true(.col_exists(df, "bank_id"))
  expect_true(.col_exists(df, "BankID"))
  expect_true(.col_exists(df, "BANKID"))
  expect_true(.col_exists(df, "bank_name"))

  # Non-existent columns
  expect_false(.col_exists(df, "missing"))
  expect_false(.col_exists(df, "status"))
})

test_that(".col_exists handles case-sensitive mode", {
  df <- tibble::tibble(BankID = 1:3, BankName = c("A", "B", "C"))

  # Case-sensitive matches
  expect_true(.col_exists(df, "BankID", case_insensitive = FALSE))
  expect_true(.col_exists(df, "BankName", case_insensitive = FALSE))

  # Case-sensitive mismatches
  expect_false(.col_exists(df, "bank_id", case_insensitive = FALSE))
  expect_false(.col_exists(df, "bankid", case_insensitive = FALSE))
})

test_that(".col_exists handles NULL and invalid inputs", {
  expect_false(.col_exists(NULL, "col"))
  expect_false(.col_exists(list(), "col"))
  expect_false(.col_exists(tibble::tibble(), "col"))

  df <- tibble::tibble(id = 1:3)
  expect_false(.col_exists(df, NULL))
  expect_false(.col_exists(df, NA))
  expect_false(.col_exists(df, ""))
})

test_that(".col_get retrieves columns correctly", {
  df <- tibble::tibble(
    BankID = 1:3,
    BankName = c("A", "B", "C"),
    Status = c("active", "inactive", "active")
  )

  # Case-insensitive retrieval
  ids <- .col_get(df, "bank_id")
  expect_equal(ids, c(1, 2, 3))

  names <- .col_get(df, "BANK_NAME")
  expect_equal(names, c("A", "B", "C"))
})

test_that(".col_get errors on missing columns when error_if_missing = TRUE", {
  df <- tibble::tibble(BankID = 1:3)

  expect_error(
    .col_get(df, "nonexistent"),
    class = "ribits_column_error"
  )

  expect_error(
    .col_get(df, "missing", error_if_missing = TRUE),
    "not found"
  )
})

test_that(".col_get returns default when error_if_missing = FALSE", {
  df <- tibble::tibble(BankID = 1:3)

  result <- .col_get(df, "nonexistent", error_if_missing = FALSE, default = NA)
  expect_true(is.na(result))

  result <- .col_get(df, "missing", error_if_missing = FALSE, default = NULL)
  expect_null(result)

  result <- .col_get(df, "missing", error_if_missing = FALSE, default = "default_value")
  expect_equal(result, "default_value")
})

test_that(".col_get handles NULL dataframe", {
  expect_error(
    .col_get(NULL, "col"),
    class = "ribits_column_error"
  )

  result <- .col_get(NULL, "col", error_if_missing = FALSE, default = "default")
  expect_equal(result, "default")
})

test_that(".col_set creates or updates columns", {
  df <- tibble::tibble(BankID = 1:3)

  # Add new column
  df <- .col_set(df, "status", c("A", "I", "A"))
  expect_true("status" %in% names(df))
  expect_equal(df$status, c("A", "I", "A"))

  # Update existing column (case-insensitive)
  df <- .col_set(df, "bank_id", c(10, 20, 30))
  expect_equal(df$BankID, c(10, 20, 30))

  # Column name preserved (BankID not changed to bank_id)
  expect_true("BankID" %in% names(df))
  expect_false("bank_id" %in% names(df))
})

test_that(".col_set errors with NULL dataframe", {
  expect_error(
    .col_set(NULL, "col", 1:3),
    class = "ribits_column_error"
  )
})

test_that(".col_get_multiple retrieves multiple columns", {
  df <- tibble::tibble(
    BankID = 1:3,
    BankName = c("A", "B", "C"),
    Status = c("A", "I", "A")
  )

  # Get all existing columns
  result <- .col_get_multiple(df, c("bank_id", "bank_name"))
  expect_equal(ncol(result), 2)
  expect_true(all(c("BankID", "BankName") %in% names(result)))

  # Mixed existing and missing
  result <- .col_get_multiple(df, c("bank_id", "missing"), require_all = FALSE)
  expect_equal(ncol(result), 1)
  expect_true("BankID" %in% names(result))
})

test_that(".col_get_multiple errors when require_all = TRUE and columns missing", {
  df <- tibble::tibble(BankID = 1:3, BankName = c("A", "B", "C"))

  expect_error(
    .col_get_multiple(df, c("bank_id", "missing"), require_all = TRUE),
    class = "ribits_column_error"
  )
})

test_that(".col_get_multiple handles empty results", {
  df <- tibble::tibble(BankID = 1:3)

  result <- .col_get_multiple(df, c("missing1", "missing2"), require_all = FALSE)
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "tbl_df")
})

test_that(".col_rename renames columns correctly", {
  df <- tibble::tibble(BankID = 1:3, OldName = c("A", "B", "C"))

  # Rename with case-insensitive match
  df <- .col_rename(df, "bank_id", "id")
  expect_true("id" %in% names(df))
  expect_false("BankID" %in% names(df))

  # Rename exact match
  df <- .col_rename(df, "OldName", "NewName", case_insensitive = FALSE)
  expect_true("NewName" %in% names(df))
  expect_false("OldName" %in% names(df))
})

test_that(".col_rename errors on missing column when error_if_missing = TRUE", {
  df <- tibble::tibble(BankID = 1:3)

  expect_error(
    .col_rename(df, "nonexistent", "new_name"),
    class = "ribits_column_error"
  )
})

test_that(".col_rename returns unchanged df when error_if_missing = FALSE", {
  df <- tibble::tibble(BankID = 1:3)

  result <- .col_rename(df, "nonexistent", "new_name", error_if_missing = FALSE)
  expect_equal(result, df)
  expect_true("BankID" %in% names(result))
})

test_that(".col_require validates required columns exist", {
  df <- tibble::tibble(
    BankID = 1:3,
    BankName = c("A", "B", "C")
  )

  # Should pass with existing columns
  expect_silent(.col_require(df, c("bank_id", "bank_name")))

  # Should error with missing column
  expect_error(
    .col_require(df, c("bank_id", "nonexistent")),
    class = "ribits_column_error"
  )
})

test_that(".col_require handles case sensitivity", {
  df <- tibble::tibble(BankID = 1:3, BankName = c("A", "B", "C"))

  # Case-insensitive (default)
  expect_silent(.col_require(df, c("bank_id", "BANK_NAME")))

  # Case-sensitive
  expect_error(
    .col_require(df, c("bank_id"), case_insensitive = FALSE),
    class = "ribits_column_error"
  )

  expect_silent(.col_require(df, c("BankID"), case_insensitive = FALSE))
})

test_that(".col_require errors with NULL dataframe", {
  expect_error(
    .col_require(NULL, c("col")),
    class = "ribits_column_error"
  )
})

test_that("column utilities work together in realistic scenario", {
  # Start with data
  df <- tibble::tibble(
    BANK_ID = 1:3,
    Bank_Name = c("Alpha", "Beta", "Gamma"),
    Established = c("2020-01-01", "2019-05-15", "2021-03-20")
  )

  # Check columns exist
  expect_true(.col_exists(df, "bank_id"))
  expect_true(.col_exists(df, "bank_name"))

  # Require certain columns
  expect_silent(.col_require(df, c("bank_id", "bank_name")))

  # Get specific columns
  ids <- .col_get(df, "bank_id")
  expect_length(ids, 3)

  # Get multiple columns
  subset <- .col_get_multiple(df, c("bank_id", "bank_name"))
  expect_equal(ncol(subset), 2)

  # Add new column
  df <- .col_set(df, "status", c("Active", "Inactive", "Active"))
  expect_true(.col_exists(df, "status"))

  # Rename column
  df <- .col_rename(df, "established", "establishment_date")
  expect_true(.col_exists(df, "establishment_date"))
  expect_false(.col_exists(df, "established"))

  # Get with default for missing column
  region <- .col_get(df, "region", error_if_missing = FALSE, default = "Unknown")
  expect_equal(region, "Unknown")
})

test_that("column utilities preserve dataframe class and attributes", {
  # Create tibble with attributes
  df <- tibble::tibble(id = 1:3, value = c("a", "b", "c"))
  attr(df, "custom") <- "test_attribute"

  # Get column
  result <- .col_get(df, "id")
  expect_equal(result, c(1, 2, 3))

  # Set column
  df2 <- .col_set(df, "new_col", c(10, 20, 30))
  expect_s3_class(df2, "tbl_df")
  expect_equal(attr(df2, "custom"), "test_attribute")

  # Get multiple columns
  subset <- .col_get_multiple(df, c("id", "value"))
  expect_s3_class(subset, "tbl_df")

  # Rename column
  df3 <- .col_rename(df, "value", "new_value")
  expect_s3_class(df3, "tbl_df")
})
