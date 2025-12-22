# R/extractors.R

#' Extract field from RIBITS data
#'
#' Generic extractor that handles all nested fields consistently. This is the
#' main function users should use to extract data from RIBITS API responses.
#'
#' @param data A tibble or list returned by `rb_get_*()` functions
#' @param field Character. Field name to extract (e.g., "ledger", "contacts",
#'   "bank_footprint")
#' @param type Character. Type of extraction: "nested_list", "geojson", or
#'   "auto" (default). Auto-detection works for most cases.
#' @param add_id Logical. If TRUE, adds the ID column from the source data to
#'   the extracted data. Default TRUE.
#'
#' @return A tibble (for nested lists) or sf object (for geojson)
#'
#' @export
#' @examples
#' \dontrun{
#' # Get bank data
#' bank <- rb_get_bank(3646, ledger = TRUE, contacts = TRUE, footprint = TRUE)
#'
#' # Extract ledger (auto-detects type)
#' ledger <- rb_extract(bank, "ledger")
#'
#' # Extract contacts
#' contacts <- rb_extract(bank, "bank_pocs")
#'
#' # Extract footprint (returns sf object)
#' footprint <- rb_extract(bank, "bank_footprint")
#'
#' # Extract without adding ID
#' ledger_no_id <- rb_extract(bank, "ledger", add_id = FALSE)
#' }
rb_extract <- function(data, field,
                       type = c("auto", "nested_list", "geojson"),
                       add_id = TRUE) {
  type <- match.arg(type)

  # Get the field value (case-insensitive column lookup)
  field_col <- names(data)[tolower(names(data)) == tolower(field)]
  if (length(field_col) == 0) {
    return(tibble::tibble())
  }
  val <- data[[field_col[1]]]

  # Handle list-column extraction from tibble
  if (is.list(val) && length(val) == 1 && is.data.frame(data)) {
    val <- val[[1]]
  }

  # Return empty tibble if no data
  if (is.null(val) || all(is.na(val))) {
    return(tibble::tibble())
  }

  # Auto-detect type if requested
  if (type == "auto") {
    type <- .rb_detect_field_type(field, val)
  }

  # Extract based on type
  result <- switch(type,
    nested_list = .rb_extract_nested_list(val),
    geojson = .rb_extract_geojson(val),
    tibble::tibble()
  )

  # Add ID column if requested and we have results
  if (add_id && !is.null(result) && nrow(result) > 0) {
    # Try to find an ID column in the source data
    id_col <- .rb_find_id_column(data)
    if (!is.null(id_col)) {
      result[[id_col$name]] <- id_col$value
      # Move ID to front
      result <- result[, c(id_col$name, setdiff(names(result), id_col$name))]
    }
  }

  result
}

#' Detect field type for extraction
#'
#' @param field Character. Field name
#' @param val The field value
#' @return Character. "nested_list" or "geojson"
#' @keywords internal
#' @noRd
.rb_detect_field_type <- function(field, val) {
  # GeoJSON fields
  geojson_fields <- c(
    "bank_footprint", "service_areas", "bank_location_centroid",
    "program_footprint", "umbrella_footprint"
  )

  if (field %in% geojson_fields) {
    return("geojson")
  }

  # Check if it looks like GeoJSON
  if (is.character(val) && grepl("^\\s*\\{", val[1])) {
    return("geojson")
  }

  # Default to nested list
  "nested_list"
}

#' Find ID column in data
#'
#' @param data Data frame or tibble
#' @return List with name and value, or NULL
#' @keywords internal
#' @noRd
.rb_find_id_column <- function(data) {
  id_patterns <- c("bank_id", "program_id", "umbrella_id", "project_id",
                   "transaction_id")

  for (pattern in id_patterns) {
    if (pattern %in% names(data)) {
      value <- data[[pattern]]
      # Handle list-column
      if (is.list(value) && length(value) == 1) {
        value <- value[[1]]
      }
      return(list(name = pattern, value = value))
    }
  }

  NULL
}

#' Extract nested list field
#'
#' @param val Field value
#' @return A tibble
#' @keywords internal
#' @noRd
.rb_extract_nested_list <- function(val) {
  # Helper to robustly convert to char tibble
  to_char_tibble <- function(x) {
    tibble::as_tibble(x) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), function(col) {
        # Handle lists within cells
        if (is.list(col)) {
           sapply(col, function(item) {
             if (is.null(item)) return(NA_character_)
             if (is.atomic(item) && length(item) == 1) return(as.character(item))
             as.character(jsonlite::toJSON(item, auto_unbox = TRUE))
           })
        } else {
           as.character(col)
        }
      }))
      # Note: We preserve original column names here - clean_names() is applied at final output
  }

  # Already a data frame
  if (is.data.frame(val)) {
    return(to_char_tibble(val))
  }

  # Not a list
  if (!is.list(val)) {
    return(tibble::tibble())
  }

  # Empty list
  if (length(val) == 0) {
    return(tibble::tibble())
  }

  # Normalize structure - if first element isn't a list, wrap it
  if (!is.list(val[[1]])) {
    val <- list(val)
  }

  # Filter valid rows (must be lists with names)
  valid_rows <- purrr::keep(val, ~ is.list(.x) && !is.null(names(.x)))

  if (length(valid_rows) == 0) {
    return(tibble::tibble())
  }

  # Convert to data frame using purrr
  # We use map_dfr but with the to_char_tibble logic to ensure safety
  purrr::map_dfr(valid_rows, function(row_list) {
    # Replace NULLs with NA
    row_list[sapply(row_list, is.null)] <- NA
    to_char_tibble(row_list)
  })
}

#' Extract GeoJSON field
#'
#' @param val Field value
#' @return An sf object or NULL
#' @keywords internal
#' @noRd
.rb_extract_geojson <- function(val) {
  # Must be character
  if (!is.character(val)) {
    # Might be nested in a list
    if (is.list(val) && length(val) > 0) {
      val <- .rb_find_geojson_in_list(val)
    }

    if (!is.character(val)) {
      return(NULL)
    }
  }

  # Basic validation
  if (length(val) == 0 || nchar(trimws(val)) < 10) {
    return(NULL)
  }

  trimmed <- trimws(val)

  if (!grepl("^\\s*\\{", trimmed) || !grepl("}\\s*$", trimmed)) {
    return(NULL)
  }

  # Parse GeoJSON
  tryCatch({
    geom <- sf::st_read(trimmed, quiet = TRUE, drivers = "GeoJSON")

    if (nrow(geom) == 0) {
      return(NULL)
    }

    # Standardize Geometry
    # 1. Transform to WGS84 (EPSG:4326)
    if (is.na(sf::st_crs(geom))) {
      sf::st_crs(geom) <- 4326
    } else {
      geom <- sf::st_transform(geom, 4326)
    }

    # 2. Standardize Geometry Column Name to "geometry"
    # This prevents issues when binding rows with different geometry column names
    geom <- sf::st_set_geometry(geom, "geometry")

    # Note: We preserve original column names here - clean_names() is applied at final output

    # Return as sf object
    geom
  }, error = function(e) {
    NULL
  })
}

#' Find GeoJSON string in nested list
#'
#' @param x List to search
#' @param depth Recursion depth
#' @return Character string or NULL
#' @keywords internal
#' @noRd
.rb_find_geojson_in_list <- function(x, depth = 0) {
  if (depth > 10) return(NULL)
  if (!is.list(x)) return(NULL)

  # Common GeoJSON field names
  geojson_fields <- c("GEOM", "CENTROID", "SERVICE_AREA",
                      "FOOTPRINT", "GEOMETRY", "SHAPE", "POLYGON")

  # Check current level
  for (field_name in geojson_fields) {
    if (!is.null(x[[field_name]])) {
      field_value <- x[[field_name]]
      if (is.character(field_value) && length(field_value) > 0 &&
          field_value != "null" && !is.na(field_value) &&
          nchar(trimws(field_value)) > 4) {
        return(field_value)
      }
    }
  }

  # Recurse into nested lists
  for (i in seq_along(x)) {
    result <- .rb_find_geojson_in_list(x[[i]], depth + 1)
    if (!is.null(result)) return(result)
  }

  NULL
}

# =============================================================================
# Convenience Wrapper Functions
# =============================================================================
# These provide backward compatibility and easier discoverability

#' Extract ledger from RIBITS data
#'
#' Convenience wrapper for `rb_extract(data, "ledger")`.
#'
#' @param data A tibble returned by `rb_get_*()` functions
#' @return A tibble containing the ledger
#' @keywords internal
rb_extract_ledger <- function(data) {
  rb_extract(data, "ledger", type = "nested_list")
}

#' Extract contacts from RIBITS data
#'
#' Extracts all contact types. For specific contact types, use
#' `rb_extract(data, "bank_pocs")`, `rb_extract(data, "bank_sponsors")`, etc.
#'
#' @param data A tibble returned by `rb_get_*()` functions
#' @return A tibble containing contacts (may be empty if show_contacts = FALSE)
#' @keywords internal
rb_extract_contacts <- function(data) {
  # Try to extract all contact types and combine
  contact_fields <- c("bank_sponsors", "bank_pocs", "bank_managers",
                      "bank_irt_members", "bank_other_contacts",
                      "program_sponsors", "program_pocs", "program_managers",
                      "program_irt_members", "program_other_contacts")

  results <- purrr::map(contact_fields, function(field) {
    if (field %in% names(data)) {
      df <- rb_extract(data, field, add_id = FALSE)
      if (nrow(df) > 0) {
        df$contact_type <- field
        return(df)
      }
    }
    NULL
  })

  dplyr::bind_rows(purrr::compact(results))
}

#' Extract sponsors from RIBITS data
#' @param data A tibble returned by `rb_get_*()` functions
#' @return A tibble
#' @keywords internal
rb_extract_sponsors <- function(data) {
  if ("bank_sponsors" %in% names(data)) {
    rb_extract(data, "bank_sponsors")
  } else if ("program_sponsors" %in% names(data)) {
    rb_extract(data, "program_sponsors")
  } else {
    tibble::tibble()
  }
}

#' Extract POCs from RIBITS data
#' @param data A tibble returned by `rb_get_*()` functions
#' @return A tibble
#' @keywords internal
rb_extract_pocs <- function(data) {
  if ("bank_pocs" %in% names(data)) {
    rb_extract(data, "bank_pocs")
  } else if ("program_pocs" %in% names(data)) {
    rb_extract(data, "program_pocs")
  } else {
    tibble::tibble()
  }
}

#' Extract managers from RIBITS data
#' @param data A tibble returned by `rb_get_*()` functions
#' @return A tibble
#' @keywords internal
rb_extract_managers <- function(data) {
  if ("bank_managers" %in% names(data)) {
    rb_extract(data, "bank_managers")
  } else if ("program_managers" %in% names(data)) {
    rb_extract(data, "program_managers")
  } else {
    tibble::tibble()
  }
}

#' Extract IRT members from RIBITS data
#' @param data A tibble returned by `rb_get_*()` functions
#' @return A tibble
#' @keywords internal
rb_extract_irt_members <- function(data) {
  if ("bank_irt_members" %in% names(data)) {
    rb_extract(data, "bank_irt_members")
  } else if ("program_irt_members" %in% names(data)) {
    rb_extract(data, "program_irt_members")
  } else {
    tibble::tibble()
  }
}

#' Extract other contacts from RIBITS data
#' @param data A tibble returned by `rb_get_*()` functions
#' @return A tibble
#' @keywords internal
rb_extract_other_contacts <- function(data) {
  if ("bank_other_contacts" %in% names(data)) {
    rb_extract(data, "bank_other_contacts")
  } else if ("program_other_contacts" %in% names(data)) {
    rb_extract(data, "program_other_contacts")
  } else {
    tibble::tibble()
  }
}

#' Extract program sites from ILF program data
#' @param data A tibble returned by `rb_get_ilf_program()`
#' @return A tibble
#' @keywords internal
rb_extract_program_sites <- function(data) {
  rb_extract(data, "program_sites")
}

#' Extract umbrella sites from umbrella data
#' @param data A tibble returned by `rb_get_umbrella()`
#' @return A tibble
#' @keywords internal
rb_extract_umbrella_sites <- function(data) {
  rb_extract(data, "umbrella_sites")
}

#' Extract footprint from RIBITS data
#' @param data A tibble returned by `rb_get_*()` functions
#' @return An sf object or empty tibble
#' @keywords internal
rb_extract_footprint <- function(data) {
  if ("bank_footprint" %in% names(data)) {
    rb_extract(data, "bank_footprint", type = "geojson")
  } else if ("program_footprint" %in% names(data)) {
    rb_extract(data, "program_footprint", type = "geojson")
  } else if ("umbrella_footprint" %in% names(data)) {
    rb_extract(data, "umbrella_footprint", type = "geojson")
  } else {
    tibble::tibble()
  }
}

#' Extract service area from RIBITS data
#' @param data A tibble returned by `rb_get_*()` functions
#' @return An sf object or empty tibble
#' @keywords internal
rb_extract_service_area <- function(data) {
  if ("bank_service_area" %in% names(data)) {
    rb_extract(data, "bank_service_area", type = "geojson")
  } else if ("program_service_area" %in% names(data)) {
    rb_extract(data, "program_service_area", type = "geojson")
  } else if ("umbrella_service_area" %in% names(data)) {
    rb_extract(data, "umbrella_service_area", type = "geojson")
  } else {
    tibble::tibble()
  }
}

#' Flatten a RIBITS record
#'
#' Removes nested list-columns (ledgers, footprints, etc.) and returns only the
#' atomic fields (attributes). This is useful for getting a clean summary table.
#'
#' @param data A tibble or list (usually 1 row)
#' @return A tibble with only atomic columns
#' @keywords internal
rb_flatten_record <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(tibble::tibble())
  
  # Select only columns that are NOT lists
  data |>
    dplyr::select(where(function(x) !is.list(x)))
}
