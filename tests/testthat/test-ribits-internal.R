library(testthat)

test_that("Internal helpers exist and work", {
  # Test .normalize_columns (core dependency for new helpers)
  df <- data.frame(
    BANK_ID = 1,
    State_Abbrev_List = "CA",
    Total_Acres = 100
  )
  # Access internal function using :::
  normalized <- RIBITSr:::.normalize_columns(df)
  expect_equal(names(normalized), c("bank_id", "state_list", "total_acres"))
})

test_that(".fetch_ribits_api_data standardizes columns", {
  # Mock rb_get
  local_mocked_bindings(
    rb_get = function(type, id = NULL, ...) {
      if (!is.null(id)) {
        # Detail request
        list(
          summary = data.frame(BANK_ID = id, NAME = "Test Bank"),
          contacts = data.frame(BANK_ID = id, CONTACT_TYPE = "POC")
        )
      } else {
        # List request
        data.frame(BANK_ID = 1:2, NAME = c("Bank 1", "Bank 2"))
      }
    },
    .package = "RIBITSr"
  )
  
  res <- RIBITSr:::.fetch_ribits_api_data("banks", NULL, NULL, NULL, quietly = TRUE)
  
  expect_true("bank_id" %in% names(res$banks))
  expect_true("bank_name" %in% names(res$banks))
  expect_equal(nrow(res$banks), 2)
})

test_that(".ribits_engine runs with mocked API", {
  # Mock everything needed
  local_mocked_bindings(
    .fetch_ribits_api_data = function(...) {
      list(
        banks = data.frame(bank_id = 1, bank_name = "Test Bank", state_list = "CA"),
        contacts = NULL
      )
    },
    .fetch_epa_data = function(...) NULL,
    .fetch_csv_data = function(...) NULL,
    .fetch_harmonized_spatial = function(...) list(footprints=NULL, service_areas=NULL, discrepancies=data.frame()),
    .package = "RIBITSr"
  )
  
  # Run engine
  res <- RIBITSr:::.ribits_engine(bank_ids = 1, sources = "api", quietly = TRUE)
  
  expect_s3_class(res, "ribits_data")
  expect_equal(res$banks$bank_id, 1)
  expect_equal(res$banks$bank_name, "Test Bank")
})
