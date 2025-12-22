
devtools::load_all()

# Get a pool of bank IDs
cli::cli_alert_info("Fetching bank list for Florida to get IDs...")
banks <- rb_list_banks(state = "FL")
all_ids <- banks$bank_id

if (length(all_ids) < 30) {
  cli::cli_alert_warning("Not enough banks in FL for full test. Using available.")
}

test_batch <- function(n) {
  if (n > length(all_ids)) {
    cli::cli_alert_warning(glue::glue("Skipping batch size {n} (not enough IDs)"))
    return(NULL)
  }
  
  ids <- all_ids[1:n]
  cli::cli_h2(glue::glue("Testing batch size: {n}"))
  
  start_time <- Sys.time()
  res <- rb_get_banks(ids, progress = TRUE)
  end_time <- Sys.time()
  
  duration <- round(difftime(end_time, start_time, units = "secs"), 2)
  cli::cli_alert_info(glue::glue("Duration: {duration} seconds"))
  
  if (length(res$summary$bank_id) == n) {
    cli::cli_alert_success(glue::glue("Successfully retrieved {n} records"))
  } else {
    cli::cli_alert_danger(glue::glue("Mismatch: Expected {n}, got {length(res$summary$bank_id)}"))
  }
  return(res)
}

# Run tests
res_10 <- test_batch(10)
res_20 <- test_batch(20)
res_30 <- test_batch(30)
