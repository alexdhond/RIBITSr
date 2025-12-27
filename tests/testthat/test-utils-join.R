test_that(".safe_full_join validates join columns exist in both dataframes", {
  df1 <- tibble::tibble(id = 1:3, value_a = c("a", "b", "c"))
  df2 <- tibble::tibble(id = 1:3, value_b = c("x", "y", "z"))

  # Should work with valid columns
  result <- .safe_full_join(df1, df2, by = "id", quietly = TRUE)
  expect_equal(nrow(result), 3)
  expect_true("value_a" %in% names(result))
  expect_true("value_b" %in% names(result))

  # Should error with missing column in df1
  expect_error(
    .safe_full_join(df1, df2, by = "nonexistent", quietly = TRUE),
    class = "ribits_join_error"
  )

  # Should error with missing column in df2
  expect_error(
    .safe_full_join(df1, df2, by = "value_a", quietly = TRUE),
    class = "ribits_join_error"
  )
})

test_that(".safe_full_join handles NULL and empty inputs gracefully", {
  df1 <- tibble::tibble(id = 1:3, value = c("a", "b", "c"))
  df2 <- tibble::tibble(id = 4:6, value = c("x", "y", "z"))
  empty_df <- tibble::tibble()

  # NULL df1 should return df2
  result <- .safe_full_join(NULL, df2, by = "id", quietly = TRUE)
  expect_equal(result, df2)

  # Empty df1 should return df2
  result <- .safe_full_join(empty_df, df2, by = "id", quietly = TRUE)
  expect_equal(result, df2)

  # NULL df2 should return df1
  result <- .safe_full_join(df1, NULL, by = "id", quietly = TRUE)
  expect_equal(result, df1)

  # Empty df2 should return df1
  result <- .safe_full_join(df1, empty_df, by = "id", quietly = TRUE)
  expect_equal(result, df1)
})

test_that(".safe_full_join warns about unexpected row multiplication", {
  # Create dataframes with duplicate keys
  df1 <- tibble::tibble(id = c(1, 1, 2), value_a = c("a1", "a2", "b"))
  df2 <- tibble::tibble(id = c(1, 1, 2), value_b = c("x1", "x2", "y"))

  # Should warn about row multiplication
  expect_warning(
    .safe_full_join(df1, df2, by = "id", quietly = FALSE),
    "more rows than expected"
  )

  # Should not warn with quietly = TRUE
  expect_no_warning(
    .safe_full_join(df1, df2, by = "id", quietly = TRUE)
  )
})

test_that(".safe_full_join produces correct results", {
  df1 <- tibble::tibble(
    id = 1:3,
    name = c("Alice", "Bob", "Charlie")
  )

  df2 <- tibble::tibble(
    id = 2:4,
    score = c(85, 90, 95)
  )

  result <- .safe_full_join(df1, df2, by = "id", quietly = TRUE)

  # Should have all rows from both dataframes
  expect_equal(nrow(result), 4)
  expect_true(all(c(1, 2, 3, 4) %in% result$id))

  # Should preserve columns from both
  expect_true("name" %in% names(result))
  expect_true("score" %in% names(result))

  # Should have NAs for non-matching rows
  expect_true(is.na(result$score[result$id == 1]))
  expect_true(is.na(result$name[result$id == 4]))
})

test_that(".safe_left_join preserves left dataframe row count", {
  df1 <- tibble::tibble(id = 1:3, value_a = c("a", "b", "c"))
  df2 <- tibble::tibble(id = 2:4, value_b = c("x", "y", "z"))

  result <- .safe_left_join(df1, df2, by = "id", quietly = TRUE)

  # Should have same number of rows as df1
  expect_equal(nrow(result), nrow(df1))
  expect_equal(result$id, df1$id)

  # Should have columns from both
  expect_true("value_a" %in% names(result))
  expect_true("value_b" %in% names(result))
})

test_that(".safe_left_join warns if row count changes", {
  # df2 has duplicate keys
  df1 <- tibble::tibble(id = 1:3, value_a = c("a", "b", "c"))
  df2 <- tibble::tibble(id = c(2, 2, 3), value_b = c("x1", "x2", "y"))

  # Should warn about row count change
  expect_warning(
    .safe_left_join(df1, df2, by = "id", quietly = FALSE),
    "changed row count"
  )
})

test_that(".safe_inner_join returns only matching rows", {
  df1 <- tibble::tibble(id = 1:3, value_a = c("a", "b", "c"))
  df2 <- tibble::tibble(id = 2:4, value_b = c("x", "y", "z"))

  result <- .safe_inner_join(df1, df2, by = "id", quietly = TRUE)

  # Should only have rows with matching ids
  expect_equal(nrow(result), 2)
  expect_true(all(result$id %in% c(2, 3)))
  expect_true("value_a" %in% names(result))
  expect_true("value_b" %in% names(result))
})

test_that(".safe_inner_join returns empty tibble if no matches", {
  df1 <- tibble::tibble(id = 1:3, value_a = c("a", "b", "c"))
  df2 <- tibble::tibble(id = 4:6, value_b = c("x", "y", "z"))

  result <- .safe_inner_join(df1, df2, by = "id", quietly = TRUE)

  expect_equal(nrow(result), 0)
  expect_s3_class(result, "tbl_df")
})

test_that(".validate_join_columns catches missing columns", {
  df1 <- tibble::tibble(id = 1:3, value = c("a", "b", "c"))
  df2 <- tibble::tibble(id = 1:3, score = c(1, 2, 3))

  # Should pass with valid column
  expect_silent(.validate_join_columns(df1, df2, "id"))

  # Should error with invalid column
  expect_error(
    .validate_join_columns(df1, df2, "nonexistent"),
    class = "ribits_join_error"
  )
})

test_that(".validate_join_columns warns about type mismatches", {
  df1 <- tibble::tibble(id = c("1", "2", "3"), value = c("a", "b", "c"))
  df2 <- tibble::tibble(id = 1:3, score = c(1, 2, 3))

  # Should warn about character vs numeric
  expect_warning(
    .validate_join_columns(df1, df2, "id"),
    "different types"
  )
})

test_that("safe joins handle suffix parameter correctly", {
  df1 <- tibble::tibble(id = 1:2, value = c("a", "b"))
  df2 <- tibble::tibble(id = 1:2, value = c("x", "y"))

  result <- .safe_full_join(df1, df2, by = "id", suffix = c("_left", "_right"),
                            quietly = TRUE)

  expect_true("value_left" %in% names(result))
  expect_true("value_right" %in% names(result))
  expect_false("value" %in% names(result))
})

test_that("safe joins work with multiple join columns", {
  df1 <- tibble::tibble(
    id1 = c(1, 1, 2),
    id2 = c("a", "b", "a"),
    value_x = c(10, 20, 30)
  )

  df2 <- tibble::tibble(
    id1 = c(1, 1, 2),
    id2 = c("a", "b", "a"),
    value_y = c(100, 200, 300)
  )

  result <- .safe_full_join(df1, df2, by = c("id1", "id2"), quietly = TRUE)

  expect_equal(nrow(result), 3)
  expect_true(all(c("value_x", "value_y") %in% names(result)))
})
