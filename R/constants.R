#' Package Constants
#'
#' This file defines all constants used throughout the RIBITSr package.
#' Centralizing constants makes them easier to find, document, and modify.
#'
#' @name constants
#' @keywords internal
NULL

# File Size Validation --------------------------------------------------------

#' Minimum CSV file size in bytes
#'
#' Files smaller than this are likely errors (empty or error pages).
#' Default: 100 bytes
#'
#' @keywords internal
MIN_CSV_FILE_SIZE <- 100

# Geometry Conversions --------------------------------------------------------

#' Square meters per acre conversion factor
#'
#' Used for converting geometry areas between units.
#' 1 acre = 4046.86 square meters
#'
#' @keywords internal
SQ_METERS_PER_ACRE <- 4046.86

# Batch Processing ------------------------------------------------------------

#' Default chunk size for batch processing
#'
#' Number of items to process in each batch when making bulk requests.
#' Default: 10
#'
#' @keywords internal
DEFAULT_CHUNK_SIZE <- 10

#' Maximum concurrent requests
#'
#' Maximum number of concurrent API requests when using parallel fetching.
#' Default: 5
#'
#' @keywords internal
DEFAULT_MAX_CONCURRENT <- 5

#' Default number of preview rows
#'
#' Number of rows to show in data previews and samples.
#' Default: 5
#'
#' @keywords internal
DEFAULT_PREVIEW_ROWS <- 5

# Network Delays --------------------------------------------------------------

#' API rate limit delay in seconds
#'
#' Delay between API requests to avoid rate limiting.
#' Default: 0.05 seconds (50ms)
#'
#' @keywords internal
API_RATE_LIMIT_DELAY <- 0.05

#' CSV download delay in seconds
#'
#' Delay between CSV download requests.
#' Default: 0.5 seconds (500ms)
#'
#' @keywords internal
CSV_DOWNLOAD_DELAY <- 0.5

# Data Quality Thresholds -----------------------------------------------------

#' Default validation configuration
#'
#' Default thresholds for data quality checks and discrepancy detection.
#' Users can override these with rb_validation_config().
#'
#' @format List with the following elements:
#' \describe{
#'   \item{numeric_tolerance}{Tolerance for numeric comparisons (0.01)}
#'   \item{date_tolerance}{Tolerance for date comparisons in days (0)}
#'   \item{flag_threshold}{Threshold for flagging percentage discrepancies (0.05 = 5%)}
#'   \item{geometry_diff_threshold}{Tolerance for geometry differences (0.01 = 1%)}
#'   \item{string_normalization_days}{Days within which to normalize date strings (7)}
#'   \item{completeness_good}{Threshold for "good" data completeness (0.90 = 90%)}
#'   \item{completeness_acceptable}{Threshold for "acceptable" completeness (0.50 = 50%)}
#'   \item{large_credit_threshold}{Credits larger than this flagged as potentially suspicious (10000)}
#'   \item{min_similarity_score}{Minimum similarity score for fuzzy name matching (0.8)}
#' }
#'
#' @keywords internal
.validation_defaults <- list(
  # Discrepancy detection
  numeric_tolerance = 0.01,  # Allow 1% difference in numeric values
  date_tolerance = 0,        # Require exact date match
  flag_threshold = 0.05,     # Flag if >5% difference
  geometry_diff_threshold = 0.01,  # Allow 1% difference in geometry area
  string_normalization_days = 7,   # Normalize dates within 7 days

  # Data completeness
  completeness_good = 0.90,        # 90% complete is "good"
  completeness_acceptable = 0.50,  # 50% complete is "acceptable"

  # Transaction validation
  large_credit_threshold = 10000,  # Credits >10k acres flagged as potentially suspicious

  # Name matching
  min_similarity_score = 0.8  # Require 80% similarity for fuzzy matching
)

# HTTP Status Codes -----------------------------------------------------------

#' HTTP status codes for different response types
#'
#' Used for classifying API responses and determining retry behavior.
#'
#' @keywords internal
HTTP_SUCCESS <- 200
HTTP_CREATED <- 201
HTTP_NO_CONTENT <- 204
HTTP_MOVED_PERMANENTLY <- 301
HTTP_FOUND <- 302
HTTP_BAD_REQUEST <- 400
HTTP_UNAUTHORIZED <- 401
HTTP_FORBIDDEN <- 403
HTTP_NOT_FOUND <- 404
HTTP_METHOD_NOT_ALLOWED <- 405
HTTP_TIMEOUT <- 408
HTTP_TOO_MANY_REQUESTS <- 429
HTTP_INTERNAL_ERROR <- 500
HTTP_BAD_GATEWAY <- 502
HTTP_SERVICE_UNAVAILABLE <- 503
HTTP_GATEWAY_TIMEOUT <- 504

#' HTTP status codes that should not be retried
#'
#' These represent permanent failures that won't be fixed by retrying.
#'
#' @keywords internal
HTTP_NO_RETRY_CODES <- c(
  HTTP_BAD_REQUEST,
  HTTP_UNAUTHORIZED,
  HTTP_FORBIDDEN,
  HTTP_NOT_FOUND,
  HTTP_METHOD_NOT_ALLOWED
)

#' HTTP status codes that indicate temporary failures
#'
#' These may succeed if retried after a delay.
#'
#' @keywords internal
HTTP_RETRY_CODES <- c(
  HTTP_TIMEOUT,
  HTTP_TOO_MANY_REQUESTS,
  HTTP_INTERNAL_ERROR,
  HTTP_BAD_GATEWAY,
  HTTP_SERVICE_UNAVAILABLE,
  HTTP_GATEWAY_TIMEOUT
)

# API Limits ------------------------------------------------------------------

#' Maximum number of banks to fetch in single API call
#'
#' Some API endpoints have limits on batch sizes.
#' Default: 30 banks
#'
#' @keywords internal
API_MAX_BANKS_PER_REQUEST <- 30

# Column Names ----------------------------------------------------------------

#' Required column names for bank data
#'
#' Core columns that must be present in bank dataframes.
#'
#' @keywords internal
REQUIRED_BANK_COLUMNS <- c("bank_id", "bank_name", "bank_status")

#' Required column names for transaction data
#'
#' Core columns that must be present in transaction dataframes.
#'
#' @keywords internal
REQUIRED_TRANSACTION_COLUMNS <- c("bank_id", "transaction_date")

#' Required column names for geometry data
#'
#' Core columns that must be present in spatial dataframes.
#'
#' @keywords internal
REQUIRED_GEOMETRY_COLUMNS <- c("bank_id")

# Cache Settings --------------------------------------------------------------

#' Default cache age in days
#'
#' How long cached data remains valid before refresh.
#' Default: 30 days
#'
#' @keywords internal
DEFAULT_CACHE_AGE_DAYS <- 30

#' Name lookup cache age in days
#'
#' How long name lookup tables are cached.
#' Default: 30 days (bank names change infrequently)
#'
#' @keywords internal
NAME_LOOKUP_CACHE_AGE_DAYS <- 30

# Coordinate Reference Systems ------------------------------------------------

#' Default CRS for spatial data
#'
#' EPSG:4326 is WGS84 (GPS coordinates, lat/lon)
#'
#' @keywords internal
DEFAULT_CRS <- "EPSG:4326"

# Progress and Verbosity ------------------------------------------------------

#' Default verbosity level
#'
#' Controls how much progress information is displayed.
#' Can be overridden with rb_config(verbose = TRUE/FALSE)
#'
#' @keywords internal
DEFAULT_VERBOSE <- FALSE

#' Show progress bars for operations longer than this many seconds
#'
#' Operations faster than this skip progress bars to reduce clutter.
#' Default: 2 seconds
#'
#' @keywords internal
PROGRESS_BAR_MIN_DURATION <- 2

# String Matching -------------------------------------------------------------

#' Maximum string distance for fuzzy matching
#'
#' Used in name matching when exact matches aren't found.
#' Lower values require closer matches.
#' Default: 0.2 (20% difference allowed)
#'
#' @keywords internal
MAX_STRING_DISTANCE <- 0.2

# Retry Logic -----------------------------------------------------------------

#' Default maximum retries for network requests
#'
#' Number of times to retry failed requests before giving up.
#' Default: 5
#'
#' @keywords internal
DEFAULT_MAX_RETRIES <- 5

#' Default retry delay in seconds
#'
#' Initial delay before first retry (doubles with each retry).
#' Default: 2 seconds
#'
#' @keywords internal
DEFAULT_RETRY_DELAY <- 2

#' Maximum retry delay in seconds
#'
#' Cap on exponential backoff to prevent excessive waits.
#' Default: 60 seconds
#'
#' @keywords internal
MAX_RETRY_DELAY <- 60

# Timeouts --------------------------------------------------------------------

#' Default API timeout in seconds
#'
#' How long to wait for API responses before timing out.
#' Default: 30 seconds
#'
#' @keywords internal
DEFAULT_API_TIMEOUT <- 30

#' Default CSV download timeout in seconds
#'
#' CSV files can be large, so allow longer timeout.
#' Default: 300 seconds (5 minutes)
#'
#' @keywords internal
DEFAULT_CSV_TIMEOUT <- 300

# Data Source Priority --------------------------------------------------------

#' Default priority order for data sources
#'
#' When discrepancies exist, this order determines which source to trust.
#' Default: CSV > API > EPA
#' Rationale:
#' - CSV reports are official quarterly reports
#' - API is real-time but sometimes has data quality issues
#' - EPA ArcGIS can be outdated
#'
#' @keywords internal
DEFAULT_SOURCE_PRIORITY <- c("csv", "api", "epa")

# Discrepancy Resolution Rules ------------------------------------------------

#' Number of auto-harmonization rules
#'
#' The package implements this many intelligent rules for resolving
#' data discrepancies automatically.
#'
#' @keywords internal
NUM_HARMONIZATION_RULES <- 7

# Export Formats --------------------------------------------------------------

#' Supported export formats for spatial data
#'
#' @keywords internal
SUPPORTED_SPATIAL_FORMATS <- c("geojson", "shapefile", "gpkg", "kml")

#' Default spatial export format
#'
#' @keywords internal
DEFAULT_SPATIAL_FORMAT <- "geojson"

# Logging ---------------------------------------------------------------------

#' Default log level
#'
#' Controls detail of logged information.
#' Levels: "error", "warn", "info", "debug"
#'
#' @keywords internal
DEFAULT_LOG_LEVEL <- "info"

# Data Types ------------------------------------------------------------------

#' Valid bank types
#'
#' @keywords internal
VALID_BANK_TYPES <- c("banks", "ilf", "umbrellas", "wqt")

#' Valid bank statuses
#'
#' All known bank statuses from RIBITS data sources.
#'
#' @keywords internal
VALID_BANK_STATUSES <- c("Approved", "Pending", "Sold-Out", "Suspended", "Terminated", "Withdrawn")

#' EPA layer mapping for bank statuses
#'
#' Maps each bank status to its corresponding EPA ArcGIS layer.
#' Note: The "terminated_banks" layer contains multiple statuses.
#'
#' @keywords internal
BANK_STATUS_EPA_LAYERS <- list(
  Approved = "approved_banks",
  Pending = "pending_banks",
 `Sold-Out` = "terminated_banks",
  Suspended = "terminated_banks",
  Terminated = "terminated_banks",
  Withdrawn = "terminated_banks"
)

#' Expected field availability by bank status
#'
#' Documents which fields are typically populated for each bank status.
#' Used for data quality validation and user guidance.
#'
#' @format List keyed by status with expected field availability:
#' \describe{
#'   \item{has_establishment_date}{Whether YEAR_ESTABLISHED is typically populated}
#'   \item{has_ledger}{Whether transaction ledger data is typically available}
#'   \item{has_credits}{Whether credit information is typically available}
#'   \item{has_footprint}{Approximate percentage with footprint geometry}
#'   \item{has_service_area}{Approximate percentage with service area geometry}
#'   \item{has_transactions}{Whether transaction history is typically available}
#' }
#'
#' @keywords internal
BANK_STATUS_FIELD_EXPECTATIONS <- list(
  Approved = list(
    has_establishment_date = TRUE,
    has_ledger = TRUE,
    has_credits = TRUE,
    has_footprint = 0.60,
    has_service_area = 0.94,
    has_transactions = TRUE,
    description = "Active banks with full data available"
  ),
  Pending = list(
    has_establishment_date = FALSE,  # Not yet approved
    has_ledger = FALSE,              # Not yet operating
    has_credits = FALSE,             # No credits released
    has_footprint = 0.25,
    has_service_area = 0.28,
    has_transactions = FALSE,
    description = "Banks awaiting approval - limited data expected"
  ),
  `Sold-Out` = list(
    has_establishment_date = TRUE,
    has_ledger = FALSE,              # Historical only
    has_credits = FALSE,             # All credits sold
    has_footprint = 0.40,
    has_service_area = 0.65,
    has_transactions = TRUE,         # Historical transactions exist
    description = "Banks with all credits sold - historical data available"
  ),
  Suspended = list(
    has_establishment_date = TRUE,
    has_ledger = TRUE,               # May still have active transactions
    has_credits = TRUE,
    has_footprint = 0.40,
    has_service_area = 0.65,
    has_transactions = TRUE,
    description = "Temporarily suspended banks - data may be stale"
  ),
  Terminated = list(
    has_establishment_date = TRUE,
    has_ledger = FALSE,
    has_credits = FALSE,
    has_footprint = 0.40,
    has_service_area = 0.65,
    has_transactions = FALSE,
    description = "Permanently terminated banks - historical data only"
  ),
  Withdrawn = list(
    has_establishment_date = FALSE,  # May never have been fully approved
    has_ledger = FALSE,
    has_credits = FALSE,
    has_footprint = 0.40,
    has_service_area = 0.65,
    has_transactions = FALSE,
    description = "Withdrawn applications - minimal data expected"
  )
)

#' Valid transaction types
#'
#' @keywords internal
VALID_TRANSACTION_LEVELS <- c("none", "basic", "comprehensive")

# Validation Limits -----------------------------------------------------------

#' Minimum reasonable credit value
#'
#' Credits below this are flagged as potentially erroneous.
#' Default: 0 (no negative credits allowed)
#'
#' @keywords internal
MIN_CREDIT_VALUE <- 0

#' Maximum reasonable credit value
#'
#' Credits above this are flagged for review (unusually large).
#' Default: 50000 acres
#'
#' @keywords internal
MAX_CREDIT_VALUE <- 50000

#' Minimum reasonable establishment year
#'
#' Banks established before this year are flagged as potentially erroneous.
#' Default: 1990 (RIBITS program started in early 1990s)
#'
#' @keywords internal
MIN_ESTABLISHMENT_YEAR <- 1990

#' Maximum reasonable establishment year
#'
#' Banks established after current year + this offset flagged as erroneous.
#' Default: 2 (allow 2 years in future for planning)
#'
#' @keywords internal
MAX_ESTABLISHMENT_YEAR_OFFSET <- 2
