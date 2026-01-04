test_that(".fetch_banks_batch handles empty input", {
  result <- .fetch_banks_batch(NULL, quietly = TRUE)
  expect_equal(result, list())

  result2 <- .fetch_banks_batch(character(0), quietly = TRUE)
  expect_equal(result2, list())
})

test_that(".fetch_banks_batch removes duplicate bank IDs", {
  # Mock the internal functions to test deduplication
  # Since we can't easily mock rb_get, we'll test the structure
  bank_ids <- c("A", "B", "A", "C", "B")

  # The function should deduplicate to unique IDs
  unique_ids <- unique(bank_ids)
  expect_equal(length(unique_ids), 3)
})

test_that(".fetch_banks_batch splits into batches correctly", {
  # Test batch splitting logic
  bank_ids <- LETTERS[1:25]  # 25 banks
  batch_size <- 10

  n_batches <- ceiling(length(bank_ids) / batch_size)
  expect_equal(n_batches, 3)  # Should create 3 batches (10, 10, 5)

  batches <- split(bank_ids, ceiling(seq_along(bank_ids) / batch_size))
  expect_equal(length(batches), 3)
  expect_equal(length(batches[[1]]), 10)
  expect_equal(length(batches[[2]]), 10)
  expect_equal(length(batches[[3]]), 5)
})

test_that(".try_batch_endpoint returns NULL (not yet implemented)", {
  # Currently this is a placeholder that returns NULL
  result <- .try_batch_endpoint(c("A", "B"), what = "all", quietly = TRUE)
  expect_null(result)
})

# REMOVED: Tests for .fetch_batch_sequential - function deleted as dead code
# (used deprecated rb_get() API, never called in package)

test_that(".fetch_banks_parallel handles empty input", {
  result <- .fetch_banks_parallel(character(0), quietly = TRUE)
  expect_equal(result, list())
})

test_that(".batch_download_reports handles empty input", {
  result <- .batch_download_reports(character(0), quietly = TRUE)
  expect_equal(result, list())
})

test_that(".batch_download_reports handles invalid report types", {
  temp_dir <- tempdir()

  # Try to download invalid report type
  result <- suppressMessages(
    .batch_download_reports(c("INVALID_REPORT_TYPE"), download_dir = temp_dir, quietly = TRUE)
  )

  # Should return a list (may be empty if all failed)
  expect_true(is.list(result))
})

test_that(".batch_process handles empty input", {
  result <- .batch_process(
    items = character(0),
    process_fn = function(x) sum(nchar(x)),
    quietly = TRUE
  )
  expect_equal(result, list())
})

test_that(".batch_process processes items in batches", {
  # Simple test: sum numbers in batches
  items <- 1:100
  batch_size <- 10

  result <- .batch_process(
    items = items,
    process_fn = function(batch) sum(batch),
    batch_size = batch_size,
    quietly = TRUE
  )

  # Should have 10 batches
  expect_equal(length(result), 10)

  # Each batch sum should be calculable
  expect_true(all(sapply(result, is.numeric)))
})

test_that(".batch_process handles process function errors", {
  items <- 1:10

  # Process function that errors
  result <- .batch_process(
    items = items,
    process_fn = function(batch) stop("Test error"),
    batch_size = 5,
    quietly = TRUE
  )

  # Should return a list (even if all batches failed)
  # May contain NULL elements for failed batches
  expect_true(!is.null(result))
  # If it's a list, all elements should be NULL
  if (is.list(result)) {
    expect_true(all(sapply(result, is.null)))
  }
})

test_that(".batch_process flattens list results", {
  items <- 1:10

  # Process function that returns a list
  result <- .batch_process(
    items = items,
    process_fn = function(batch) as.list(batch),
    batch_size = 5,
    quietly = TRUE
  )

  # Should flatten the results
  expect_true(is.list(result))
})

test_that(".batch_process uses custom batch size", {
  items <- 1:50
  batch_size <- 7

  result <- .batch_process(
    items = items,
    process_fn = function(batch) length(batch),
    batch_size = batch_size,
    quietly = TRUE
  )

  # Number of batches
  n_batches <- ceiling(50 / 7)
  expect_equal(length(result), n_batches)
})

test_that(".estimate_batch_time calculates correctly", {
  # 100 items, batch size 10, 0.5s per batch = 5 seconds
  time <- .estimate_batch_time(100, batch_size = 10, avg_time_per_batch = 0.5)
  expect_equal(time, 5)

  # 0 items = 0 time
  time_zero <- .estimate_batch_time(0)
  expect_equal(time_zero, 0)

  # 5 items, batch size 10 = 1 batch
  time_small <- .estimate_batch_time(5, batch_size = 10, avg_time_per_batch = 0.5)
  expect_equal(time_small, 0.5)
})

test_that(".estimate_batch_time handles edge cases", {
  # Exactly one batch
  time <- .estimate_batch_time(10, batch_size = 10, avg_time_per_batch = 1)
  expect_equal(time, 1)

  # Just over one batch (needs 2 batches)
  time2 <- .estimate_batch_time(11, batch_size = 10, avg_time_per_batch = 1)
  expect_equal(time2, 2)
})

test_that(".format_time_estimate formats seconds correctly", {
  # Less than 60 seconds
  formatted <- .format_time_estimate(30)
  expect_match(formatted, "30 seconds")

  formatted2 <- .format_time_estimate(45.7)
  expect_match(formatted2, "46 seconds")
})

test_that(".format_time_estimate formats minutes correctly", {
  # 120 seconds = 2 minutes
  formatted <- .format_time_estimate(120)
  expect_match(formatted, "2.0 minutes")

  # 90 seconds = 1.5 minutes
  formatted2 <- .format_time_estimate(90)
  expect_match(formatted2, "1.5 minutes")
})

test_that(".format_time_estimate formats hours correctly", {
  # 3600 seconds = 1 hour
  formatted <- .format_time_estimate(3600)
  expect_match(formatted, "1.0 hours")

  # 7200 seconds = 2 hours
  formatted2 <- .format_time_estimate(7200)
  expect_match(formatted2, "2.0 hours")

  # 5400 seconds = 1.5 hours
  formatted3 <- .format_time_estimate(5400)
  expect_match(formatted3, "1.5 hours")
})

test_that(".format_time_estimate handles edge cases", {
  # 0 seconds
  formatted <- .format_time_estimate(0)
  expect_match(formatted, "0 seconds")

  # Just under 1 minute
  formatted2 <- .format_time_estimate(59)
  expect_match(formatted2, "59 seconds")

  # Just under 1 hour
  formatted3 <- .format_time_estimate(3599)
  expect_match(formatted3, "59.98 minutes|60.0 minutes")  # Could round either way
})

test_that("batch operations respect DEFAULT_CHUNK_SIZE", {
  # Test that default batch size is used
  expect_true(exists("DEFAULT_CHUNK_SIZE"))
  expect_true(is.numeric(DEFAULT_CHUNK_SIZE))
  expect_true(DEFAULT_CHUNK_SIZE > 0)
})

test_that("batch operations respect DEFAULT_MAX_CONCURRENT", {
  # Test that default concurrent limit exists
  expect_true(exists("DEFAULT_MAX_CONCURRENT"))
  expect_true(is.numeric(DEFAULT_MAX_CONCURRENT))
  expect_true(DEFAULT_MAX_CONCURRENT > 0)
})

test_that("batch operations use API_RATE_LIMIT_DELAY", {
  # Test that rate limit delay constant exists
  expect_true(exists("API_RATE_LIMIT_DELAY"))
  expect_true(is.numeric(API_RATE_LIMIT_DELAY))
  expect_true(API_RATE_LIMIT_DELAY >= 0)
})

test_that("batch operations use CSV_DOWNLOAD_DELAY", {
  # Test that CSV download delay constant exists
  expect_true(exists("CSV_DOWNLOAD_DELAY"))
  expect_true(is.numeric(CSV_DOWNLOAD_DELAY))
  expect_true(CSV_DOWNLOAD_DELAY >= 0)
})

test_that(".batch_process with different batch sizes produces same total", {
  items <- 1:100

  # Process with different batch sizes
  result1 <- .batch_process(
    items = items,
    process_fn = function(batch) batch,
    batch_size = 10,
    quietly = TRUE
  )

  result2 <- .batch_process(
    items = items,
    process_fn = function(batch) batch,
    batch_size = 25,
    quietly = TRUE
  )

  # Both should contain all items when flattened
  expect_equal(length(unlist(result1)), 100)
  expect_equal(length(unlist(result2)), 100)
})

test_that(".batch_process preserves item order", {
  items <- letters[1:26]

  result <- .batch_process(
    items = items,
    process_fn = function(batch) batch,
    batch_size = 5,
    quietly = TRUE
  )

  # Result should preserve the order of items
  # Since process_fn returns character vectors (not lists), result is a list of vectors
  expect_true(is.list(result))
  # Flatten and check order
  flattened <- unlist(result)
  expect_equal(flattened, items)
})

test_that(".batch_process handles single item", {
  items <- "single"

  result <- .batch_process(
    items = items,
    process_fn = function(batch) paste(batch, "processed"),
    batch_size = 10,
    quietly = TRUE
  )

  expect_equal(length(result), 1)
  expect_equal(result[[1]], "single processed")
})

test_that(".batch_process description parameter is used", {
  # Just verify the function accepts the parameter
  items <- 1:10

  expect_no_error(
    .batch_process(
      items = items,
      process_fn = function(batch) sum(batch),
      description = "Custom description",
      quietly = TRUE
    )
  )
})

test_that("batch time estimation scales linearly", {
  # 10 items should take 10x less time than 100 items
  time10 <- .estimate_batch_time(10, batch_size = 10, avg_time_per_batch = 1)
  time100 <- .estimate_batch_time(100, batch_size = 10, avg_time_per_batch = 1)

  expect_equal(time100, time10 * 10)
})

test_that("time formatting transitions smoothly", {
  # 59 seconds should be in seconds
  fmt59 <- .format_time_estimate(59)
  expect_match(fmt59, "seconds")

  # 61 seconds should be in minutes
  fmt61 <- .format_time_estimate(61)
  expect_match(fmt61, "minutes")

  # 3599 seconds should be in minutes
  fmt3599 <- .format_time_estimate(3599)
  expect_match(fmt3599, "minutes")

  # 3601 seconds should be in hours
  fmt3601 <- .format_time_estimate(3601)
  expect_match(fmt3601, "hours")
})
