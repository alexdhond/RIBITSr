
library(testthat)
library(RIBITSr)

test_that("rb_get('ilf') returns a data frame with expected columns", {
  skip_on_cran()
  
  # Fetch ILF programs for a specific state to reduce load
  ilfs <- rb_get("ilf", state = "FL")
  
  expect_s3_class(ilfs, "data.frame")
  expect_s3_class(ilfs, "tbl_df")
  
  # Check for typical columns (looser check to avoid breakage on minor name changes)
  expect_true(any(grepl("program_id|id", colnames(ilfs), ignore.case = TRUE)))
  expect_true(any(grepl("name", colnames(ilfs), ignore.case = TRUE)))
  
  if (nrow(ilfs) > 0) {
    # If we have data, we can test rb_get("ilf") with a real ID
    # Handle different possible column names
    id_col <- grep("program_id|id", colnames(ilfs), ignore.case = TRUE, value = TRUE)[1]
    first_id <- ilfs[[id_col]][1]
    
    prog <- rb_get("ilf", id = first_id)
    
    expect_type(prog, "list")
    expect_true("summary" %in% names(prog))
    expect_s3_class(prog$summary, "tbl_df")
    expect_equal(nrow(prog$summary), 1)
    
    # Check for other components that should exist for ILF
    if ("program_sites" %in% names(prog)) {
      expect_s3_class(prog$program_sites, "tbl_df")
    }
  }
})
