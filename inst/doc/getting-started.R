## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----install------------------------------------------------------------------
# # Install from GitHub
# # install.packages("pak")
# pak::pak("alexanderdhond/RIBITSr")
# 
# # Load the package
# library(RIBITSr)
# library(dplyr)  # For filtering and analysis

## ----simple-example-----------------------------------------------------------
# library(RIBITSr)
# 
# # Get all data for California (one line!)
# ca <- ribits(state = "CA")
# 
# # What did we get?
# ca
# #> <ribits_data: 128 banks>
# #> ✔ 128 banks with summaries
# #> ✔ 4,521 transactions (~85 columns, harmonized from 3 sources)
# #> ✔ 342 contacts
# #> ✔ 256 spatial geometries

## ----filter-example-----------------------------------------------------------
# library(dplyr)
# 
# # Find approved banks with high credit availability
# high_credit_banks <- ca$banks %>%
#   filter(
#     bank_status == "Approved",
#     available_credits > 100,
#     total_acres > 50
#   ) %>%
#   select(bank_id, bank_name, bank_type, available_credits, total_acres) %>%
#   arrange(desc(available_credits))
# 
# high_credit_banks
# #> # A tibble: 23 × 5
# #>    bank_id bank_name                  bank_type            available_credits total_acres
# #>      <int> <chr>                      <chr>                            <dbl>       <dbl>
# #>  1     123 Sacramento River Bank      Private Commercial               1245.        456
# #>  2     456 Napa Valley Conservation   Non-Profit                       892.         234
# #>  3     789 Bay Delta Mitigation       Public                           654.         678
# #>  ...

## ----data-structure-----------------------------------------------------------
# # Main bank information (1 row per bank)
# ca$banks
# #> # A tibble: 128 × 67
# #>    bank_id bank_name          bank_status district    state_list total_acres ...
# #>      <int> <chr>              <chr>       <chr>       <chr>            <dbl>
# #>  1      17 Vernal Pool Ranch  Approved    Sacramento  CA                 567
# #>  2     100 Cosumnes River...  Approved    Sacramento  CA                 1234
# #>  ...
# 
# # Transaction data (many rows per bank, ~85 columns!)
# ca$transactions
# #> # A tibble: 4,521 × 85
# #>    bank_id transaction_id transaction_date permittee_name debits huc8 ...
# #>      <int> <chr>          <date>           <chr>          <dbl> <chr>
# #>  1      17 TXN-2023-001   2023-05-15       Acme Dev        12.5 18020111
# #>  2      17 TXN-2023-045   2023-06-22       Smith Bros      8.3  18020111
# #>  ...
# 
# # Spatial data (sf object ready for mapping)
# ca$geometry
# #> Simple feature collection with 256 features
# #>    bank_id geometry_type      geometry
# #>      <int> <chr>              <POLYGON>
# #>  1      17 footprint          POLYGON((...)
# #>  2      17 service_area       POLYGON((...)
# #>  ...
# 
# # Metadata (sources, discrepancies, timing)
# ca$.meta
# #> $sources
# #> [1] "api" "epa" "csv"
# #>
# #> $timing
# #> $timing$duration_secs
# #> [1] 45.3
# #>
# #> $discrepancies
# #> # 3 minor discrepancies found and resolved

## ----filter-banks-------------------------------------------------------------
# # Banks in specific watershed
# huc8_banks <- ca$banks %>%
#   filter(grepl("18020111", huc8_list))
# 
# # Banks established after 2015
# recent_banks <- ca$banks %>%
#   filter(year_established > 2015)
# 
# # Banks by type
# private_banks <- ca$banks %>%
#   filter(bank_type == "Private Commercial")
# 
# # Combine multiple conditions
# large_approved <- ca$banks %>%
#   filter(
#     bank_status == "Approved",
#     total_acres > 200,
#     available_credits > 50
#   )

## ----analyze-transactions-----------------------------------------------------
# # Recent transaction activity
# recent_activity <- ca$transactions %>%
#   filter(transaction_date >= as.Date("2020-01-01")) %>%
#   group_by(bank_id, bank_name) %>%
#   summarize(
#     n_transactions = n(),
#     total_debits = sum(debits, na.rm = TRUE),
#     avg_transaction = mean(debits, na.rm = TRUE),
#     first_transaction = min(transaction_date),
#     last_transaction = max(transaction_date)
#   ) %>%
#   arrange(desc(total_debits))
# 
# # Top permittees by credit usage
# top_permittees <- ca$transactions %>%
#   group_by(permittee_name) %>%
#   summarize(
#     total_credits = sum(debits, na.rm = TRUE),
#     n_transactions = n(),
#     n_banks_used = n_distinct(bank_id)
#   ) %>%
#   filter(total_credits > 0) %>%
#   arrange(desc(total_credits)) %>%
#   head(10)

## ----summarize-district-------------------------------------------------------
# # Credit availability by district
# district_summary <- ca$banks %>%
#   filter(bank_status == "Approved") %>%
#   group_by(district) %>%
#   summarize(
#     n_banks = n(),
#     total_available = sum(available_credits, na.rm = TRUE),
#     total_released = sum(released_credits, na.rm = TRUE),
#     avg_acres = mean(total_acres, na.rm = TRUE),
#     median_acres = median(total_acres, na.rm = TRUE)
#   ) %>%
#   arrange(desc(total_available))

## ----join-example-------------------------------------------------------------
# # Get bank details with transaction counts
# bank_activity <- ca$transactions %>%
#   group_by(bank_id) %>%
#   summarize(
#     n_transactions = n(),
#     total_debits = sum(debits, na.rm = TRUE)
#   ) %>%
#   left_join(ca$banks, by = "bank_id") %>%
#   select(bank_id, bank_name, district, n_transactions, total_debits, available_credits)

## ----spatial-analysis---------------------------------------------------------
# library(sf)
# library(ggplot2)
# 
# # Map bank footprints
# ggplot(ca$geometry %>% filter(geometry_type == "footprint")) +
#   geom_sf(aes(geometry = geometry), fill = "lightblue", alpha = 0.6) +
#   theme_minimal() +
#   labs(title = "California Mitigation Bank Footprints")
# 
# # Find banks near a specific location
# library(sf)
# 
# # Convert banks to sf object
# ca_banks_sf <- st_as_sf(ca$banks,
#                         coords = c("longitude", "latitude"),
#                         crs = 4326)
# 
# # Define target location (Sacramento)
# sacramento <- st_point(c(-121.4944, 38.5816)) %>%
#   st_sfc(crs = 4326)
# 
# # Find banks within 50km
# nearby_banks <- ca_banks_sf %>%
#   mutate(
#     distance_km = as.numeric(st_distance(geometry, sacramento)) / 1000
#   ) %>%
#   filter(distance_km < 50) %>%
#   arrange(distance_km)

## ----speed-options------------------------------------------------------------
# # Maximum data (default) - comprehensive transactions (~85 columns)
# full <- ribits(state = "OR")
# 
# # Faster - basic transactions (~20 columns)
# basic <- ribits(state = "OR", transactions = "basic")
# 
# # Fastest - no transactions
# fast <- ribits(state = "OR", transactions = "none", spatial = FALSE)

## ----data-types---------------------------------------------------------------
# # ILF programs
# ilf_programs <- ribits(type = "ilf", state = "CA")
# 
# # Umbrella instruments
# umbrellas <- ribits(type = "umbrellas", state = "FL")
# 
# # Specific banks by ID
# my_banks <- ribits(ids = c(17, 100, 345))

## ----quality-checks-----------------------------------------------------------
# # Check for conflicts between sources
# discrepancies(ca)
# #> # A tibble: 3 × 5
# #>   bank_id field          api_value  epa_value  resolution
# #>     <int> <chr>          <chr>      <chr>      <chr>
# #> 1      17 total_acres    567        565        Used API (more recent)
# #> 2     100 bank_status    Approved   Pending    Used API (authoritative)
# 
# # See how conflicts were resolved
# resolutions(ca)
# #> All discrepancies resolved using priority: api > csv > epa
# 
# # Before downloading, check coverage
# rb_check(state = "WA")
# #> Data Coverage: state=WA
# #> ✔ Banks in API: 45
# #> ✔ Banks in EPA: 38 (84% of API)
# #> ✔ With footprints: 35 (78%)
# 
# # After downloading, check quality
# wa <- ribits(state = "WA")
# rb_check(wa)
# #> Data Quality Report
# #> ✔ 45 banks
# #> ✔ bank_name: 100% complete
# #> ✔ total_acres: 96% complete

## ----discovery----------------------------------------------------------------
# # See all available data sources
# rb_info()
# #> RIBITSr Data Catalog
# #>
# #> Quick Start:
# #>   ca_data <- ribits(state = "CA")
# #>
# #> Data Sources:
# #>   - RIBITS API: Real-time data
# #>   - EPA ArcGIS: Spatial layers
# #>   - CSV Reports: Official records

## ----watershed-example--------------------------------------------------------
# library(RIBITSr)
# library(dplyr)
# library(ggplot2)
# 
# # Get all California data
# ca <- ribits(state = "CA")
# 
# # Filter to Delta watershed
# delta_banks <- ca$banks %>%
#   filter(grepl("18020104", huc8_list)) %>%
#   select(bank_id, bank_name, bank_status, available_credits, total_acres)
# 
# # Get transactions for these banks
# delta_transactions <- ca$transactions %>%
#   filter(bank_id %in% delta_banks$bank_id) %>%
#   mutate(year = lubridate::year(transaction_date))
# 
# # Analyze annual activity
# annual_activity <- delta_transactions %>%
#   filter(year >= 2015, year <= 2024) %>%
#   group_by(year) %>%
#   summarize(
#     n_transactions = n(),
#     total_credits = sum(debits, na.rm = TRUE),
#     n_banks_active = n_distinct(bank_id)
#   )
# 
# # Visualize trends
# ggplot(annual_activity, aes(x = year, y = total_credits)) +
#   geom_line(size = 1.2, color = "steelblue") +
#   geom_point(size = 3) +
#   labs(
#     title = "Mitigation Banking Activity in Sacramento-San Joaquin Delta",
#     subtitle = "HUC8: 18020104",
#     x = "Year",
#     y = "Total Credits Used"
#   ) +
#   theme_minimal()

## ----configuration------------------------------------------------------------
# # View current settings
# rb_config()
# 
# # Enable persistent caching (survives R restart)
# rb_config(
#   use_persistent_cache = TRUE,
#   cache_max_age_days = 7  # Refresh weekly
# )
# 
# # For slow networks
# rb_config(
#   max_retries = 5,
#   timeout = 120,
#   rate_limit = 2  # Slower rate
# )
# 
# # Reset to defaults
# rb_config(reset = TRUE)

