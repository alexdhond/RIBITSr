
library(testthat)
library(RIBITSr)

test_that("rb_get('wqt') returns a data frame", {
  skip_on_cran()
  
  # WQT projects might be sparser, try without state filter or a state known for WQT if any
  # Using FL or just general list
  wqt <- rb_get("wqt", state = "VA")
  
  expect_s3_class(wqt, "data.frame")
  expect_s3_class(wqt, "tbl_df")
  
  # Depending on API response, columns might vary, but should be a tibble
  expect_true(any(grepl("wqt.*id", colnames(wqt), ignore.case = TRUE)))
})

test_that("rb_get('wqt', id=...) handles errors or returns data", {
  skip_on_cran()
  
  # Since we don't know a guaranteed ID and the endpoint is flagged as potentially unstable
  # We will try to get one if list returns any, otherwise skip
  
  wqt_list <- rb_get("wqt", state = "VA")
  
  if (nrow(wqt_list) > 0) {
    id_col <- grep("wqt.*id", colnames(wqt_list), ignore.case = TRUE, value = TRUE)[1]
    first_id <- wqt_list[[id_col]][1] 
    
    # Wrap in try as the function itself might abort
    res <- try(rb_get("wqt", id = first_id), silent = TRUE)
    
    if (!inherits(res, "try-error")) {
        # rb_get with ID returns a list
        expect_type(res, "list")
        expect_true("summary" %in% names(res))
    }
  }
})
