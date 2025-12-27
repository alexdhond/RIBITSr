test_that(".ribits_error creates base error class", {
  expect_error(
    .ribits_error("Test error"),
    class = "ribits_error"
  )

  # Should include custom message
  expect_error(
    .ribits_error("Custom message"),
    "Custom message"
  )
})

test_that(".network_error creates network-specific errors", {
  expect_error(
    .network_error("Network failure"),
    class = "ribits_network_error"
  )

  # Can include status code and URL
  err <- tryCatch(
    .network_error("Failed", status_code = 500, url = "https://example.com"),
    error = function(e) e
  )

  expect_equal(err$status_code, 500)
  expect_equal(err$url, "https://example.com")
})

test_that(".data_error creates data quality errors", {
  expect_error(
    .data_error("Data validation failed"),
    class = "ribits_data_error"
  )

  # Can include source and validation type
  err <- tryCatch(
    .data_error("Invalid data", data_source = "CSV", validation_type = "completeness"),
    error = function(e) e
  )

  expect_equal(err$data_source, "CSV")
  expect_equal(err$validation_type, "completeness")
})

test_that(".config_error creates configuration errors", {
  expect_error(
    .config_error("Invalid configuration"),
    class = "ribits_config_error"
  )

  # Can include config parameter
  err <- tryCatch(
    .config_error("Invalid rate limit", config_param = "rate_limit"),
    error = function(e) e
  )

  expect_equal(err$config_param, "rate_limit")
})

test_that(".column_error creates column access errors", {
  expect_error(
    .column_error("Column not found"),
    class = "ribits_column_error"
  )

  # Can include column info
  err <- tryCatch(
    .column_error(
      "Missing column",
      column_name = "bank_id",
      available_columns = c("id", "name")
    ),
    error = function(e) e
  )

  expect_equal(err$column_name, "bank_id")
  expect_equal(err$available_columns, c("id", "name"))
})

test_that(".join_error creates join operation errors", {
  expect_error(
    .join_error("Join failed"),
    class = "ribits_join_error"
  )

  # Can include join info
  err <- tryCatch(
    .join_error("Type mismatch", join_type = "full", join_columns = "id"),
    error = function(e) e
  )

  expect_equal(err$join_type, "full")
  expect_equal(err$join_columns, "id")
})

test_that(".parse_error creates parsing errors", {
  expect_error(
    .parse_error("Parse failed"),
    class = "ribits_parse_error"
  )

  # Can include file info
  err <- tryCatch(
    .parse_error("Invalid CSV", file_path = "data.csv", format = "CSV"),
    error = function(e) e
  )

  expect_equal(err$file_path, "data.csv")
  expect_equal(err$format, "CSV")
})

test_that(".spatial_error creates spatial data errors", {
  expect_error(
    .spatial_error("Invalid geometry"),
    class = "ribits_spatial_error"
  )

  # Can include geometry info
  err <- tryCatch(
    .spatial_error("CRS mismatch", geometry_type = "POLYGON", crs = "EPSG:4326"),
    error = function(e) e
  )

  expect_equal(err$geometry_type, "POLYGON")
  expect_equal(err$crs, "EPSG:4326")
})

test_that("error messages can include bullet points", {
  err <- tryCatch(
    .network_error(
      c(
        "API request failed",
        x = "Status code: 500",
        i = "Try again later"
      )
    ),
    error = function(e) e
  )

  msg <- conditionMessage(err)
  expect_match(msg, "API request failed")
})

test_that("is_ribits_error correctly identifies RIBITSr errors", {
  ribits_err <- tryCatch(
    .ribits_error("test"),
    error = function(e) e
  )

  network_err <- tryCatch(
    .network_error("test"),
    error = function(e) e
  )

  base_err <- tryCatch(
    stop("test"),
    error = function(e) e
  )

  expect_true(is_ribits_error(ribits_err))
  expect_true(is_ribits_error(network_err))
  expect_false(is_ribits_error(base_err))
})

test_that("get_error_type returns correct error type", {
  network_err <- tryCatch(
    .network_error("test"),
    error = function(e) e
  )

  data_err <- tryCatch(
    .data_error("test"),
    error = function(e) e
  )

  base_err <- tryCatch(
    stop("test"),
    error = function(e) e
  )

  expect_equal(get_error_type(network_err), "ribits_network_error")
  expect_equal(get_error_type(data_err), "ribits_data_error")
  expect_null(get_error_type(base_err))
})

test_that("errors can be caught with specific handlers", {
  caught_type <- NULL

  tryCatch(
    .network_error("Network issue"),
    ribits_network_error = function(e) {
      caught_type <<- "network"
    },
    ribits_data_error = function(e) {
      caught_type <<- "data"
    }
  )

  expect_equal(caught_type, "network")

  # Reset
  caught_type <- NULL

  tryCatch(
    .data_error("Data issue"),
    ribits_network_error = function(e) {
      caught_type <<- "network"
    },
    ribits_data_error = function(e) {
      caught_type <<- "data"
    }
  )

  expect_equal(caught_type, "data")
})

test_that("specific errors can be caught as general ribits_error", {
  caught <- FALSE

  tryCatch(
    .network_error("Network issue"),
    ribits_error = function(e) {
      caught <<- TRUE
    }
  )

  expect_true(caught)

  # Reset
  caught <- FALSE

  tryCatch(
    .config_error("Config issue"),
    ribits_error = function(e) {
      caught <<- TRUE
    }
  )

  expect_true(caught)
})

test_that("error inheritance chain works correctly", {
  network_err <- tryCatch(
    .network_error("test"),
    error = function(e) e
  )

  # Should inherit from both specific class and base class
  expect_true(inherits(network_err, "ribits_network_error"))
  expect_true(inherits(network_err, "ribits_error"))
  expect_true(inherits(network_err, "error"))
  expect_true(inherits(network_err, "condition"))
})

test_that("errors include call information", {
  test_function <- function() {
    .network_error("Test error")
  }

  err <- tryCatch(
    test_function(),
    error = function(e) e
  )

  # Error should have call information
  expect_true(!is.null(err$call))
})

test_that("custom error fields are accessible", {
  err <- tryCatch(
    .network_error(
      "API failed",
      status_code = 404,
      url = "https://api.example.com",
      custom_field = "custom_value"
    ),
    error = function(e) e
  )

  expect_equal(err$status_code, 404)
  expect_equal(err$url, "https://api.example.com")
  expect_equal(err$custom_field, "custom_value")
})

test_that("all error types are documented and functional", {
  error_types <- list(
    ribits_error = function() .ribits_error("base error"),
    network = function() .network_error("network error"),
    data = function() .data_error("data error"),
    config = function() .config_error("config error"),
    column = function() .column_error("column error"),
    join = function() .join_error("join error"),
    parse = function() .parse_error("parse error"),
    spatial = function() .spatial_error("spatial error")
  )

  # All error types should throw errors
  for (name in names(error_types)) {
    expect_error(error_types[[name]](), class = "error")
  }
})
