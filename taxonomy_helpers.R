#' @title Taxonomy Helper Functions
#'
#' @description Helper functions for querying ITIS and GBIF APIs and processing
#'   taxonomic data for the arthropod enrichment pipeline.
#'
#' @note All functions use purrr (no for-loops), native R pipe, and namespaced
#'   calls.
#'
#' # Rate limiting and error handling ----
#' @title Safe API Call with Retries
#'
#' @description Executes an API function with retry/backoff and returns a
#'   standardized list.
#'
#' @param api_function function A zero-argument function that performs the API
#'   request.
#' @param max_retries integer Maximum retries on error.
#' @param delay numeric Base delay (seconds) for exponential backoff.
#'
#' @return list A list with `success` (logical), `data` (on success), or `error`
#'   (message on failure).
safe_api_call <- function(api_function, max_retries = 3, delay = 1) {
  for (i in 1:max_retries) {
    result <- tryCatch({
      api_function()
    }, error = function(e) {
      if (i == max_retries) {
        return(list(success = FALSE, error = e$message))  
      }
      Sys.sleep(delay * i)  # exponential backoff
      NULL
    })
    
    if (!is.null(result)) {
      return(list(success = TRUE, data = result))
    }
  }
  
  return(list(success = FALSE, error = "Max retries exceeded"))
}

# ensure ritis and rgbif are available (no library() attachments)
ensure_installed <- function(pkgs, repos = "https://cloud.r-project.org") {
  missing <- pkgs[
    !vapply(pkgs, function(p) requireNamespace(p, quietly = TRUE), logical(1))
  ]
  if (length(missing) == 0L) {
    return(invisible(TRUE))
  }
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  tryCatch(
    {
      install.packages(missing, repos = repos, dependencies = TRUE)
      still_missing <- missing[
        !vapply(
          missing,
          function(p) requireNamespace(p, quietly = TRUE),
          logical(1)
        )
      ]
      if (length(still_missing)) {
        stop(
          "Package installation failed for: ",
          paste(still_missing, collapse = ", "),
          call. = FALSE
        )
      }
      invisible(TRUE)
    },
    error = function(e) {
      stop(
        "Package installation error: ",
        conditionMessage(e),
        "\nTips: ensure network access and a writable library; on HPC, preload modules or use a local CRAN mirror.",
        call. = FALSE
      )
    }
  )
}

ensure_installed(c("ritis", "rgbif"))


# ITIS enrichment functions ----

#' @title Enrich One ITIS Record
#'
#' @description Fetches ITIS hierarchy, truncates at the focal TSN, and returns
#'   a formatted taxonomy row.
#'
#' @param display_name character Input taxon name.
#' @param authority character Taxonomic authority (e.g., "ITIS").
#' @param authority_id character|numeric ITIS TSN identifier.
#' @param rank character Taxonomic rank of the input.
#'
#' @return list A list with `taxonomy` (tibble row) and `failures` (tibble of
#'   issues, possibly empty).
enrich_itis_record <- function(display_name, authority, authority_id, rank) {
  # Convert authority_id to numeric for ITIS
  tsn <- tryCatch(
    {
      as.numeric(authority_id)
    },
    error = function(e) {
      return(create_failed_record(
        display_name,
        authority,
        authority_id,
        "Invalid TSN format"
      ))
    }
  )

  if (is.na(tsn)) {
    return(create_failed_record(
      display_name,
      authority,
      authority_id,
      "TSN is NA"
    ))
  }

  # Query ITIS hierarchy
  hierarchy_result <- safe_api_call(function() {
    ritis::hierarchy_full(tsn = tsn)
  })

  if (!hierarchy_result$success) {
    return(create_failed_record(
      display_name,
      authority,
      authority_id,
      hierarchy_result$error
    ))
  }

  hierarchy <- hierarchy_result$data

  if (nrow(hierarchy) == 0) {
    return(create_failed_record(
      display_name,
      authority,
      authority_id,
      "No hierarchy returned from ITIS"
    ))
  }

  # Filter hierarchy to stop at the target taxon rank
  # This prevents including child taxa (e.g., species under a genus)
  target_row <- which(hierarchy$tsn == as.character(tsn))

  if (length(target_row) == 0) {
    return(create_failed_record(
      display_name,
      authority,
      authority_id,
      "Target TSN not found in hierarchy"
    ))
  }

  # Keep only up to and including the target taxon
  filtered_hierarchy <- hierarchy[1:target_row[1], ]

  # Convert to standardized taxonomy format
  taxonomy_record <- process_itis_hierarchy(
    filtered_hierarchy,
    display_name,
    authority,
    authority_id,
    rank
  )

  return(list(
    taxonomy = taxonomy_record,
    failures = tibble::tibble()
  ))
}

#' @title Process ITIS Hierarchy
#'
#' @description Maps ITIS hierarchy ranks into a standardized flat taxonomy row.
#'
#' @param hierarchy tibble Full ITIS hierarchy up to the focal TSN.
#' @param display_name character Input taxon name.
#' @param authority character Authority label ("ITIS").
#' @param authority_id character|numeric ITIS TSN.
#' @param rank character Input rank.
#'
#' @return tibble One-row tibble with standardized taxonomy columns and
#'   metadata.
process_itis_hierarchy <- function(hierarchy, display_name, authority, authority_id, rank) {
  
  # Initialize all possible taxonomic ranks
  taxa_columns <- c(
    "kingdom",
    "subkingdom",
    "infrakingdom",
    "superphylum",
    "phylum",
    "subphylum",
    "superclass",
    "class",
    "subclass",
    "infraclass",
    "superorder",
    "order",
    "suborder",
    "infraorder",
    "superfamily",
    "family",
    "subfamily",
    "tribe",
    "subtribe",
    "genus",
    "subgenus",
    "species",
    "subspecies",
    "variety",
    "form"
  )
  
  # Create empty result with all columns as NA
  result <- setNames(rep(NA_character_, length(taxa_columns)), taxa_columns)
  
  # Map ITIS rank names to our standardized names
  rank_mapping <- c(
    "Kingdom" = "kingdom",
    "Subkingdom" = "subkingdom",
    "Infrakingdom" = "infrakingdom",
    "Superphylum" = "superphylum",
    "Phylum" = "phylum",
    "Subphylum" = "subphylum",
    "Superclass" = "superclass",
    "Class" = "class",
    "Subclass" = "subclass",
    "Infraclass" = "infraclass",
    "Superorder" = "superorder",
    "Order" = "order",
    "Suborder" = "suborder",
    "Infraorder" = "infraorder",
    "Superfamily" = "superfamily",
    "Family" = "family",
    "Subfamily" = "subfamily",
    "Tribe" = "tribe",
    "Subtribe" = "subtribe",
    "Genus" = "genus",
    "Subgenus" = "subgenus",
    "Species" = "species",
    "Subspecies" = "subspecies",
    "Variety" = "variety",
    "Form" = "form"
  )
  
  # Fill in available taxonomic information
  if (nrow(hierarchy) > 0) {
    for (i in seq_len(nrow(hierarchy))) {
      itis_rank <- hierarchy$rankname[i]
      taxon_name <- hierarchy$taxonname[i]
      
      if (itis_rank %in% names(rank_mapping)) {
        standard_rank <- rank_mapping[itis_rank]
        result[standard_rank] <- taxon_name
      }
    }
  }
  
  # Convert to tibble and add metadata
  tibble::tibble(
    display_name         = display_name,
    authority            = authority,
    authority_id         = authority_id,
    rank                 = rank,
    !!!result,
    processing_timestamp = Sys.time(),
    match_status         = "success"
  )
}

# GBIF enrichment functions ----

#' @title Enrich One GBIF Record
#'
#' @description Fetches GBIF backbone/usage data and returns a formatted
#'   taxonomy row.
#'
#' @param display_name character Input taxon name.
#' @param authority character Authority label ("GBIF").
#' @param authority_id character|numeric GBIF usage key when available.
#' @param rank character Input rank.
#'
#' @return list A list with `taxonomy` (tibble row) and `failures` (tibble of
#'   issues, possibly empty).
enrich_gbif_record <- function(display_name, authority, authority_id, rank) {
  # Convert authority_id to numeric for GBIF
  usage_key <- tryCatch(
    {
      as.numeric(authority_id)
    },
    error = function(e) {
      return(create_failed_record(
        display_name,
        authority,
        authority_id,
        "Invalid usage key format"
      ))
    }
  )

  if (is.na(usage_key)) {
    return(create_failed_record(
      display_name,
      authority,
      authority_id,
      "Usage key is NA"
    ))
  }

  # Query GBIF using the usage key directly
  backbone_result <- safe_api_call(function() {
    rgbif::name_backbone_checklist(
      name = "",
      usageKey = as.character(usage_key)
    )
  })

  if (!backbone_result$success) {
    return(create_failed_record(
      display_name,
      authority,
      authority_id,
      backbone_result$error
    ))
  }

  backbone_data <- backbone_result$data

  if (nrow(backbone_data) == 0) {
    return(create_failed_record(
      display_name,
      authority,
      authority_id,
      "No data returned from GBIF"
    ))
  }

  # Convert to standardized taxonomy format
  taxonomy_record <- process_gbif_backbone(
    backbone_data,
    display_name,
    authority,
    authority_id,
    rank
  )

  return(list(
    taxonomy = taxonomy_record,
    failures = tibble::tibble()
  ))
}

#' @title Process GBIF Backbone Result
#'
#' @description Maps GBIF backbone/usage fields into a standardized flat
#'   taxonomy row.
#'
#' @param backbone_data data.frame GBIF result (normalized single-row or first
#'   row will be used).
#' @param display_name character Input taxon name.
#' @param authority character Authority label ("GBIF").
#' @param authority_id character|numeric GBIF usage key.
#' @param rank character Input rank.
#'
#' @return tibble One-row tibble with standardized taxonomy columns and
#'   metadata.
process_gbif_backbone <- function(
  backbone_data,
  display_name,
  authority,
  authority_id,
  rank
) {
  # Initialize all possible taxonomic ranks
  taxa_columns <- c(
    "kingdom",
    "subkingdom",
    "infrakingdom",
    "superphylum",
    "phylum",
    "subphylum",
    "superclass",
    "class",
    "subclass",
    "infraclass",
    "superorder",
    "order",
    "suborder",
    "infraorder",
    "superfamily",
    "family",
    "subfamily",
    "tribe",
    "subtribe",
    "genus",
    "subgenus",
    "species",
    "subspecies",
    "variety",
    "form"
  )

  # Create empty result with all columns as NA
  result <- setNames(rep(NA_character_, length(taxa_columns)), taxa_columns)

  # GBIF provides direct taxonomic hierarchy in the backbone response
  gbif_data <- backbone_data[1, ] # Take first (should be only) result

  # Helper function to safely extract column value
  safe_extract <- function(data, column_name) {
    if (column_name %in% colnames(data) && !is.na(data[[column_name]])) {
      return(data[[column_name]])
    }
    return(NA_character_)
  }

  # Map GBIF columns to our standardized format
  result["kingdom"] <- safe_extract(gbif_data, "kingdom")
  result["phylum"]  <- safe_extract(gbif_data, "phylum")
  result["class"]   <- safe_extract(gbif_data, "class")
  result["order"]   <- safe_extract(gbif_data, "order")
  result["family"]  <- safe_extract(gbif_data, "family")
  result["genus"]   <- safe_extract(gbif_data, "genus")
  result["species"] <- safe_extract(gbif_data, "species")

  # Note: GBIF backbone doesn't provide all intermediate ranks like ITIS
  # Additional ranks would need separate API calls if required

  # Convert to tibble and add metadata
  tibble::tibble(
    display_name         = display_name,
    authority            = authority,
    authority_id         = authority_id,
    rank                 = rank,
    !!!result,
    processing_timestamp = Sys.time(),
    match_status         = "success"
  )
}

# Utility functions ----

#' @title Create Failed Record
#'
#' @description Builds a standardized failure taxonomy row and an accompanying
#'   failure report row.
#'
#' @param display_name character Input taxon name.
#' @param authority character Authority label.
#' @param authority_id character|numeric Identifier from the authority.
#' @param error_message character Failure message.
#'
#' @return list A list with `taxonomy` (empty/NA taxonomy row) and `failures`
#'   (one-row tibble explaining the failure).
create_failed_record <- function(display_name, authority, authority_id, error_message) {
  
  # Create empty taxonomy record
  taxa_columns <- c(
    "kingdom",
    "subkingdom",
    "infrakingdom",
    "superphylum",
    "phylum",
    "subphylum",
    "superclass",
    "class",
    "subclass",
    "infraclass",
    "superorder",
    "order",
    "suborder",
    "infraorder",
    "superfamily",
    "family",
    "subfamily",
    "tribe",
    "subtribe",
    "genus",
    "subgenus",
    "species",
    "subspecies",
    "variety",
    "form"
  )
  
  empty_taxonomy <- setNames(rep(NA_character_, length(taxa_columns)), taxa_columns)
  
  taxonomy_record <- tibble::tibble(
    display_name         = display_name,
    authority            = authority,
    authority_id         = authority_id, 
    rank                 = NA_character_,
    !!!empty_taxonomy,
    processing_timestamp = Sys.time(),
    match_status         = "failed"
  )
  
  # Create failure record
  failure_record <- tibble::tibble(
    display_name  = display_name,
    authority     = authority,
    authority_id  = authority_id,
    error_message = error_message,
    timestamp     = Sys.time()
  )
  
  return(list(
    taxonomy = taxonomy_record,
    failures = failure_record
  ))
}

#' @title Format Taxonomy Table
#'
#' @description Ensures all expected taxonomy columns exist (added as NA if
#'   missing), selects, and orders them.
#'
#' @param enriched_data tibble Combined taxonomy rows from ITIS/GBIF processing.
#'
#' @return tibble Formatted taxonomy table with stable column order.
format_taxonomy_table <- function(enriched_data) {
  # Ensure all expected columns exist
  expected_cols <- c(
    "display_name",
    "authority",
    "authority_id",
    "rank",
    "kingdom",
    "subkingdom",
    "infrakingdom",
    "superphylum",
    "phylum",
    "subphylum",
    "superclass",
    "class",
    "subclass",
    "infraclass",
    "superorder",
    "order",
    "suborder",
    "infraorder",
    "superfamily",
    "family",
    "subfamily",
    "tribe",
    "subtribe",
    "genus",
    "subgenus",
    "species",
    "subspecies",
    "variety",
    "form",
    "processing_timestamp",
    "match_status"
  )

  # Add missing columns as NA
  for (col in expected_cols) {
    if (!col %in% colnames(enriched_data)) {
      enriched_data[[col]] <- NA_character_
    }
  }

  # Select and arrange columns in logical order
  enriched_data <- dplyr::select(enriched_data, dplyr::all_of(expected_cols))
  enriched_data <- enriched_data[order(enriched_data$display_name), ]

  return(enriched_data)
}

# String concatenation operator for cleaner output formatting
`%+%` <- function(x, y) paste0(x, y)
