
library(testthat)
library(RIBITSr)

test_that("rb_get('umbrellas') returns a data frame with expected columns", {
  skip_on_cran()
  
  # Fetch umbrellas for a specific state
  # FL usually has mitigation banks, checking if it has umbrellas
  umbrellas <- rb_get("umbrellas", state = "CA")
  
  expect_s3_class(umbrellas, "data.frame")
  expect_s3_class(umbrellas, "tbl_df")
  
  expect_true(any(grepl("umbrella.*id", colnames(umbrellas), ignore.case = TRUE)))
  
  if (nrow(umbrellas) > 0) {
    id_col <- grep("umbrella.*id", colnames(umbrellas), ignore.case = TRUE, value = TRUE)[1]
    first_id <- umbrellas[[id_col]][1]
    
    umb <- rb_get("umbrellas", id = first_id)
    
    expect_type(umb, "list")
    expect_true("summary" %in% names(umb))
    expect_s3_class(umb$summary, "tbl_df")
    expect_equal(nrow(umb$summary), 1)
    
    # Check for umbrella_sites
    if ("umbrella_sites" %in% names(umb)) {
      expect_s3_class(umb$umbrella_sites, "tbl_df")
    }
  }
})
