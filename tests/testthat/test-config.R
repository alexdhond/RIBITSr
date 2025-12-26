# tests/testthat/test-config.R
# Tests for unified configuration API

test_that("rb_config retrieves current settings", {
  # Reset to defaults first
  rb_config(reset = TRUE)

  cfg <- rb_config()

  expect_type(cfg, "list")
  expect_true("network" %in% names(cfg))
  expect_true("performance" %in% names(cfg))
  expect_true("data_quality" %in% names(cfg))

  # Check default values
  expect_equal(cfg$network$max_retries, 3)
  expect_equal(cfg$network$retry_delay, 2)
  expect_equal(cfg$network$timeout, 30)
  expect_equal(cfg$performance$rate_limit, 5)
  expect_equal(cfg$performance$use_persistent_cache, FALSE)
  expect_equal(cfg$performance$cache_max_age_days, 30)
})

test_that("rb_config updates network settings", {
  rb_config(reset = TRUE)

  rb_config(
    max_retries = 5,
    timeout = 60,
    verbose = FALSE
  )

  cfg <- rb_config()
  expect_equal(cfg$network$max_retries, 5)
  expect_equal(cfg$network$timeout, 60)
  expect_equal(cfg$network$verbose, FALSE)

  # Reset
  rb_config(reset = TRUE)
})

test_that("rb_config updates performance settings", {
  rb_config(reset = TRUE)

  rb_config(
    rate_limit = 2,
    use_persistent_cache = TRUE,
    cache_max_age_days = 7
  )

  cfg <- rb_config()
  expect_equal(cfg$performance$rate_limit, 2)
  expect_equal(cfg$performance$use_persistent_cache, TRUE)
  expect_equal(cfg$performance$cache_max_age_days, 7)

  # Reset
  rb_config(reset = TRUE)
})

test_that("rb_config can disable rate limiting", {
  rb_config(reset = TRUE)

  rb_config(rate_limit = NULL)

  cfg <- rb_config()
  expect_null(cfg$performance$rate_limit)

  # Reset
  rb_config(reset = TRUE)
})

test_that("rb_config updates data quality settings", {
  rb_config(reset = TRUE)

  rb_config(
    source_priority = c("api", "csv", "epa"),
    auto_resolve = FALSE
  )

  cfg <- rb_config()
  expect_equal(cfg$data_quality$source_priority, c("api", "csv", "epa"))
  expect_equal(cfg$data_quality$auto_resolve, FALSE)

  # Reset
  rb_config(reset = TRUE)
})

test_that("rb_config reset works correctly", {
  # Change some settings
  rb_config(
    max_retries = 10,
    rate_limit = 1,
    use_persistent_cache = TRUE
  )

  # Verify changed
  cfg <- rb_config()
  expect_equal(cfg$network$max_retries, 10)

  # Reset
  rb_config(reset = TRUE)

  # Verify back to defaults
  cfg <- rb_config()
  expect_equal(cfg$network$max_retries, 3)
  expect_equal(cfg$performance$rate_limit, 5)
  expect_equal(cfg$performance$use_persistent_cache, FALSE)
})

test_that(".network_options environment is updated", {
  rb_config(reset = TRUE)

  rb_config(max_retries = 7, timeout = 90)

  # Check internal environment directly
  expect_equal(RIBITSr:::.network_options$max_retries, 7)
  expect_equal(RIBITSr:::.network_options$timeout, 90)

  # Reset
  rb_config(reset = TRUE)
})
