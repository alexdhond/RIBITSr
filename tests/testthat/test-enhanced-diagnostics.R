# tests/testthat/test-enhanced-diagnostics.R
# Tests for enhanced discrepancy diagnostic functions

# Create mock data for testing
create_mock_data_with_discrepancies <- function() {

  # Mock banks data
  banks <- tibble::tibble(
    bank_id = c("SAC-001", "SAC-002", "LA-001"),
    bank_name = c("Sacramento Bank", "North Sac Bank", "LA Basin Bank"),
    total_credits = c(100.5, 200.0, 150.0),
    total_acres = c(50, 75, 100)
  )

  # Mock discrepancies (unresolved)
  discrepancies <- tibble::tibble(
    bank_id = c("SAC-001", "SAC-002"),
    field = c("total_credits", "total_acres"),
    source1 = c("api", "csv"),
    value1 = c(100.5, 75),
    source2 = c("csv", "epa"),
    value2 = c(105.0, 80),
    discrepancy_type = c("numeric_difference", "numeric_difference"),
    severity = c("medium", "low"),
    diff_pct = c(4.3, 6.5)
  )

  # Mock harmonization resolutions (auto-resolved)
  resolutions <- tibble::tibble(
    bank_id = c("LA-001"),
    field = c("total_credits"),
    source1 = c("api"),
    value1 = c(150.0),
    source2 = c("csv"),
    value2 = c(NA),
    harmonized_value = c("150.0"),
    harmonized_source = c("api"),
    resolution_rule = c("missing_value_backfill"),
    confidence = c("high"),
    discrepancy_type = c("missing_value")
  )

  # Create ribits_data object
  data <- structure(
    list(
      banks = banks,
      .meta = list(
        fetch_date = Sys.time(),
        query = list(state = "CA"),
        sources = list(
          banks = "ribits_api",
          transactions = "ribits_csv",
          geometry = "epa_arcgis"
        ),
        discrepancies = discrepancies,
        harmonization_resolutions = resolutions
      )
    ),
    class = "ribits_data"
  )

  data
}


test_that("rb_view_discrepancies() displays side-by-side comparison", {

  data <- create_mock_data_with_discrepancies()

  # Test filtering by bank_id
  result <- rb_view_discrepancies(data, bank_id = "SAC-001")

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true("api_value" %in% names(result))
  expect_true("csv_value" %in% names(result))
  expect_true("epa_value" %in% names(result))
  expect_true("harmonized_value" %in% names(result))

  # Check filtering worked
  expect_true(all(result$bank_id == "SAC-001"))
})


test_that("rb_view_discrepancies() filters by field", {

  data <- create_mock_data_with_discrepancies()

  result <- rb_view_discrepancies(data, field = "total_credits")

  expect_s3_class(result, "tbl_df")
  expect_true(all(result$field == "total_credits"))
})


test_that("rb_view_discrepancies() handles bank and field filter together", {

  data <- create_mock_data_with_discrepancies()

  result <- rb_view_discrepancies(data, bank_id = "SAC-001", field = "total_credits")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$bank_id, "SAC-001")
  expect_equal(result$field, "total_credits")
})


test_that("rb_view_discrepancies() handles no discrepancies gracefully", {

  data <- create_mock_data_with_discrepancies()
  data$.meta$discrepancies <- tibble::tibble()
  data$.meta$harmonization_resolutions <- tibble::tibble()

  result <- rb_view_discrepancies(data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})


test_that("rb_source_info() displays source metadata", {

  data <- create_mock_data_with_discrepancies()

  result <- rb_source_info(data)

  expect_type(result, "list")
  expect_true("fetch_date" %in% names(result))
  expect_true("sources_used" %in% names(result))
  expect_true("config" %in% names(result))
})


test_that("rb_export_discrepancies() exports to CSV", {

  data <- create_mock_data_with_discrepancies()

  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  result <- rb_export_discrepancies(data, format = "csv", file_path = temp_file)

  expect_true(file.exists(temp_file))

  # Read back and verify
  exported <- readr::read_csv(temp_file, show_col_types = FALSE)
  expect_s3_class(exported, "tbl_df")
  expect_true(nrow(exported) > 0)
  expect_true("status" %in% names(exported))
})


test_that("rb_export_discrepancies() exports to HTML", {

  data <- create_mock_data_with_discrepancies()

  temp_file <- tempfile(fileext = ".html")
  on.exit(unlink(temp_file))

  result <- rb_export_discrepancies(data, format = "html", file_path = temp_file)

  expect_true(file.exists(temp_file))

  # Read and verify HTML structure
  html_content <- readLines(temp_file)
  expect_true(any(grepl("RIBITS Data Discrepancies", html_content)))
  expect_true(any(grepl("<table>", html_content)))
})


test_that("rb_export_discrepancies() handles Excel format gracefully if openxlsx not available", {

  data <- create_mock_data_with_discrepancies()

  # This will fall back to CSV if openxlsx is not available
  temp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(temp_file))

  # Should not error even if openxlsx not installed
  expect_no_error({
    result <- rb_export_discrepancies(data, format = "excel", file_path = temp_file)
  })
})


test_that("rb_resolve() manually resolves discrepancy by source", {

  data <- create_mock_data_with_discrepancies()

  # Resolve by choosing CSV source
  updated_data <- rb_resolve(data,
    bank_id = "SAC-001",
    field = "total_credits",
    source = "csv"
  )

  expect_s3_class(updated_data, "ribits_data")

  # Check that manual resolution was tracked
  expect_true(!is.null(updated_data$.meta$manual_resolutions))
  expect_true(nrow(updated_data$.meta$manual_resolutions) > 0)

  # Check that discrepancy was removed from unresolved list
  remaining_disc <- updated_data$.meta$discrepancies
  if (!is.null(remaining_disc) && nrow(remaining_disc) > 0) {
    expect_false(any(remaining_disc$bank_id == "SAC-001" & remaining_disc$field == "total_credits"))
  }

  # Check that the value was updated in banks data
  bank_row <- updated_data$banks |> dplyr::filter(bank_id == "SAC-001")
  expect_equal(bank_row$total_credits, 105.0)
})


test_that("rb_resolve() manually resolves discrepancy by exact value", {

  data <- create_mock_data_with_discrepancies()

  # Resolve by specifying exact value
  updated_data <- rb_resolve(data,
    bank_id = "SAC-001",
    field = "total_credits",
    value = 110.0
  )

  expect_s3_class(updated_data, "ribits_data")

  # Check that the exact value was used
  bank_row <- updated_data$banks |> dplyr::filter(bank_id == "SAC-001")
  expect_equal(bank_row$total_credits, 110.0)

  # Check manual resolution metadata
  manual_res <- updated_data$.meta$manual_resolutions
  expect_equal(manual_res$chosen_source, "manual")
  expect_equal(manual_res$chosen_value, "110")
})


test_that("rb_resolve() requires either source or value", {

  data <- create_mock_data_with_discrepancies()

  expect_error(
    rb_resolve(data, bank_id = "SAC-001", field = "total_credits"),
    "Either source or value must be specified"
  )
})


test_that("rb_resolve() validates source parameter", {

  data <- create_mock_data_with_discrepancies()

  expect_error(
    rb_resolve(data, bank_id = "SAC-001", field = "total_credits", source = "invalid"),
    "source must be one of"
  )
})


test_that("rb_resolve() handles non-existent discrepancy gracefully", {

  data <- create_mock_data_with_discrepancies()

  # Try to resolve a discrepancy that doesn't exist
  result <- rb_resolve(data,
    bank_id = "NONEXISTENT",
    field = "total_credits",
    source = "api"
  )

  # Should return data unchanged
  expect_equal(result, data)
})


test_that("rb_clear_manual_resolutions() clears saved preferences", {

  # This function should work even if no preferences exist
  expect_no_error({
    result <- rb_clear_manual_resolutions()
  })

  expect_true(result)
})


test_that(".build_three_way_comparison() merges pairwise comparisons", {

  # Create pairwise conflicts
  conflicts <- tibble::tibble(
    bank_id = c("SAC-001", "SAC-001"),
    field = c("total_credits", "total_credits"),
    source1 = c("api", "csv"),
    value1 = c(100.5, 105.0),
    source2 = c("csv", "epa"),
    value2 = c(105.0, 103.0),
    auto_resolved = c(FALSE, FALSE),
    harmonized_value = c(NA, NA),
    harmonized_source = c(NA, NA),
    resolution_rule = c(NA, NA),
    confidence = c(NA, NA)
  )

  result <- RIBITSr:::.build_three_way_comparison(conflicts)

  expect_s3_class(result, "tbl_df")
  expect_true("api_value" %in% names(result))
  expect_true("csv_value" %in% names(result))
  expect_true("epa_value" %in% names(result))

  # Should have consolidated the pairwise comparisons into one row
  expect_equal(nrow(result), 1)
  expect_equal(result$bank_id, "SAC-001")
  expect_equal(result$field, "total_credits")
})


test_that(".get_source_values() extracts values from all sources", {

  # Create discrepancies with api vs csv comparison
  matching_disc <- tibble::tibble(
    bank_id = c("SAC-001"),
    field = c("total_credits"),
    source1 = c("api"),
    value1 = c(100.5),
    source2 = c("csv"),
    value2 = c(105.0)
  )

  # Create resolutions with csv vs epa comparison for SAME field
  matching_res <- tibble::tibble(
    bank_id = c("SAC-001"),
    field = c("total_credits"),  # Same field as discrepancy
    source1 = c("csv"),
    value1 = c(105.0),  # Same CSV value
    source2 = c("epa"),
    value2 = c(103.0)
  )

  result <- RIBITSr:::.get_source_values(matching_disc, matching_res, "SAC-001", "total_credits")

  expect_type(result, "list")
  expect_true("api" %in% names(result))
  expect_true("csv" %in% names(result))
  expect_true("epa" %in% names(result))

  # Check extracted values
  expect_equal(result$api, 100.5)
  expect_equal(result$csv, 105.0)
  expect_equal(result$epa, 103.0)
})


test_that(".create_discrepancy_summary() generates summary data", {

  disc <- tibble::tibble(
    bank_id = c("SAC-001", "SAC-002"),
    field = c("total_credits", "total_acres"),
    severity = c("high", "medium")
  )

  resolutions <- tibble::tibble(
    bank_id = c("LA-001"),
    field = c("total_credits"),
    confidence = c("high")
  )

  result <- RIBITSr:::.create_discrepancy_summary(disc, resolutions)

  expect_s3_class(result, "tbl_df")
  expect_true("Metric" %in% names(result))
  expect_true("Value" %in% names(result))
  expect_true(nrow(result) > 0)
})
