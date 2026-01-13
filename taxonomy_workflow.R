#' @title CAP LTER Arthropod Taxonomy Workflow
#' @description End-to-end workflow to enrich arthropod taxonomy records using
#'   ITIS and GBIF, generate a flat hierarchy table, run preflight validation,
#'   and export results and summaries.
#' @note Uses purrr (no for-loops), native R pipe, and namespaced calls. Expects
#'   a live DB connection `pg`. Enrichment respects `archive = FALSE` and
#'   restricts authority to ITIS/GBIF. Preflight reports potential
#'   API/data-quality issues before production runs.
#' @examples
#' # Quick preflight and sample enrichment
#' # source("taxonomy_workflow.R")
#' # run_preflight(250)
#' # quick_test(5)

# "📖 ARTHROPOD TAXONOMY ENRICHMENT USAGE"
# "quick_test(5)          # Test with 5 records"
# "production_run()       # Process all records"

# Load helper functions
source("taxonomy_helpers.R")

# configuration ----
CONFIG <- list(
  
  # processing settings
  batch_size       = 100,      # Process in batches to manage memory
  max_retries      = 3,       # API retry attempts
  rate_limit_delay = 0.5, # Seconds between API calls
  
  # output settings
  output_dir = ".",
  timestamp  = format(Sys.time(), "%Y%m%d_%H%M%S")
)


#' @title Enrich Arthropod Taxonomy (Complete Run)
#'
#' @description Process active taxa (ITIS/GBIF), query external APIs, assemble a
#'   comprehensive flat taxonomy table, and write CSV + summary report.
#'
#' @param sample_size integer Optional limit of records to process.
#' @param output_file character Optional output CSV path; defaults to
#'   timestamped file in `CONFIG$output_dir`.
#'
#' @return list A list with `taxonomy_table`, `failed_matches`, `summary_stats`,
#'   and `output_files`.
#'
#' @note Uses `purrr::pmap()` for processing, rate-limited via
#'   `CONFIG$rate_limit_delay`.

enrich_arthropod_taxonomy_complete <- function(
  sample_size = NULL,
  output_file = NULL
) {
  # Setup ----
  start_time <- Sys.time()
  cat("🚀 Starting complete arthropod taxonomy enrichment\n")
  cat("⏰ Start time:", format(start_time), "\n")

  # Create output directory
  if (!dir.exists(CONFIG$output_dir)) {
    dir.create(CONFIG$output_dir, recursive = TRUE)
  }

  # Step 1: Extract data ----
  cat("📊 Extracting active taxonomy records...\n")

  base_query <- "
  WITH authorities AS (
    SELECT
      *,
      CASE
        WHEN authority = 'ITIS' THEN 'ITIS'
        WHEN authority ~~* '%gbif%' or authority ~~* '%global%' THEN 'GBIF'
        ELSE 'OTHER'
      END AS authority_source   
    FROM arthropods.arthropod_taxonomy 
  )
  SELECT
    display_name,
    authority,
    authority_source,
    authority_id,
    rank,
    archive
  FROM authorities
  WHERE
    archive IS NOT TRUE
    AND authority_source IN ('ITIS', 'GBIF')
    -- AND (authority = 'ITIS' OR authority ~~* '%gbif%' or authority ~~* '%global%')
    AND authority_id IS NOT NULL
    AND display_name IS NOT NULL
    "

  if (!is.null(sample_size)) {
    query <- paste(base_query, "LIMIT", sample_size, ";")
    cat("📝 Using sample size:", sample_size, "\n")
  } else {
    query <- paste(base_query, ";")
  }

  active_taxa <- DBI::dbGetQuery(pg, query)

  cat("✅ Extracted", nrow(active_taxa), "records\n")
  # Use authority_source (normalized) for counts/logic
  if ("authority_source" %in% names(active_taxa)) {
    cat("   - GBIF records:", sum(active_taxa$authority_source == "GBIF", na.rm = TRUE), "\n")
    cat("   - ITIS records:", sum(active_taxa$authority_source == "ITIS", na.rm = TRUE), "\n")
  } else {
    cat("   - GBIF records:", sum(active_taxa$authority == "GBIF", na.rm = TRUE), "\n")
    cat("   - ITIS records:", sum(active_taxa$authority == "ITIS", na.rm = TRUE), "\n")
  }

  if (nrow(active_taxa) == 0) {
    cat("❌ No records found. Exiting.\n")
    return(NULL)
  }

  # Step 2: Process records ----
  cat("\n🔄 Processing taxonomic enrichment...\n")

  # Initialize results containers
  enriched_records <- tibble::tibble()
  failed_records   <- tibble::tibble()

  # Process in batches to manage memory and provide progress updates
  total_batches <- ceiling(nrow(active_taxa) / CONFIG$batch_size)

  for (batch_num in 1:total_batches) {

    start_idx  <- (batch_num - 1) * CONFIG$batch_size + 1
    end_idx    <- min(batch_num * CONFIG$batch_size, nrow(active_taxa))
    batch_data <- active_taxa[start_idx:end_idx, ]

    cat(glue::glue(
      "🔄 Processing batch {batch_num}/{total_batches} ({nrow(batch_data)} records)\n"
    ))

    # Process each record in the batch
    # Restrict columns passed to pmap to avoid unused argument errors from extra
    # columns
    pmap_input <- batch_data |>
      dplyr::select(dplyr::any_of(c(
        "display_name",
        "authority_source",
        "authority_id",
        "rank"
      )))
    
    batch_results <- pmap_input |>
      purrr::pmap(function(display_name, authority_source, authority_id, rank) {
        # Add small delay to respect API rate limits
        Sys.sleep(CONFIG$rate_limit_delay)
        auth_norm <- authority_source
        if (auth_norm == "ITIS") {
          result <- enrich_itis_record(
            display_name,
            auth_norm,
            authority_id,
            rank
          )
        } else if (auth_norm == "GBIF") {
          result <- enrich_gbif_record(
            display_name,
            auth_norm,
            authority_id,
            rank
          )
        } else {
          result <- create_failed_record(
            display_name,
            auth_norm,
            authority_id,
            "Unsupported authority"
          )
        }

        return(result)
      })

    # Extract successful and failed records from batch
    batch_enriched <- purrr::map_dfr(batch_results, ~ .x$taxonomy)
    batch_failed   <- purrr::map_dfr(batch_results, ~ .x$failures)

    # Add to overall results
    enriched_records <- dplyr::bind_rows(enriched_records, batch_enriched)
    failed_records   <- dplyr::bind_rows(failed_records, batch_failed)

    # Progress update
    success_count <- sum(batch_enriched$match_status == "success", na.rm = TRUE)
    cat(glue::glue(
      "   ✅ {success_count}/{nrow(batch_data)} successful in this batch\n"
    ))
  }

  # Step 4: Format final results ----
  cat("\n📋 Formatting final taxonomy table...\n")

  final_taxonomy <- format_taxonomy_table(enriched_records)

  # Step 5: Generate comprehensive report ----
  end_time        <- Sys.time()
  processing_time <- difftime(end_time, start_time, units = "secs")

  summary_stats <- list(
    total_processed = nrow(active_taxa),
    successfully_enriched = sum(
      final_taxonomy$match_status == "success",
      na.rm = TRUE
    ),
    failed_matches = nrow(failed_records),
    success_rate = round(
      sum(final_taxonomy$match_status == "success", na.rm = TRUE) /
        nrow(active_taxa) *
        100,
      1
    ),
    processing_time_seconds = as.numeric(processing_time),
    processing_time_minutes = round(as.numeric(processing_time) / 60, 2)
  )

  # Print summary
  cat("📊 ENRICHMENT COMPLETE - SUMMARY REPORT\n")
  cat("📝 Total records processed:    ", summary_stats$total_processed, "\n")
  cat(
    "✅ Successfully enriched:      ",
    summary_stats$successfully_enriched,
    "\n"
  )
  cat("❌ Failed matches:             ", summary_stats$failed_matches, "\n")
  cat("📈 Success rate:               ", summary_stats$success_rate, "%\n")
  cat(
    "⏱️  Processing time:            ",
    summary_stats$processing_time_minutes,
    " minutes\n"
  )
  cat("💾 Output directory:           ", CONFIG$output_dir, "\n")

  # Step 6: Save results ----
  cat("💾 Saving results...\n")

  # Determine output filename
  if (is.null(output_file)) {
    output_file <- file.path(
      CONFIG$output_dir,
      glue::glue("arthropod_taxonomy_enriched_{CONFIG$timestamp}.csv")
    )
  }

  # Save main results
  readr::write_csv(final_taxonomy, output_file)
  cat("✅ Main results saved:", output_file, "\n")

  # Save failed matches if any
  if (nrow(failed_records) > 0) {
    failed_file <- file.path(
      CONFIG$output_dir,
      glue::glue("failed_matches_{CONFIG$timestamp}.csv")
    )
    readr::write_csv(failed_records, failed_file)
    cat("❌ Failed matches saved:", failed_file, "\n")
  }

  # Save summary report
  summary_file <- file.path(
    CONFIG$output_dir,
    glue::glue("summary_report_{CONFIG$timestamp}.txt")
  )

  report_lines <- c(
    "CAP LTER Arthropod Taxonomy Enrichment Report",
    "",
    paste("Processing Date:", format(Sys.time())),
    "",
    "SUMMARY STATISTICS:",
    paste("Total records processed:", summary_stats$total_processed),
    paste("Successfully enriched:", summary_stats$successfully_enriched),
    paste("Failed matches:", summary_stats$failed_matches),
    paste("Success rate:", summary_stats$success_rate, "%"),
    paste("Processing time:", summary_stats$processing_time_minutes, "minutes"),
    "",
    "OUTPUT FILES:",
    paste("Main results:", basename(output_file)),
    if (nrow(failed_records) > 0) {
      paste("Failed matches:", basename(failed_file))
    } else {
      NULL
    },
    paste("Summary report:", basename(summary_file)),
    "",
    "TAXONOMIC COMPLETENESS:",
    paste(
      "Kingdom complete:",
      round(
        sum(!is.na(final_taxonomy$kingdom)) / nrow(final_taxonomy) * 100,
        1
      ),
      "%"
    ),
    paste(
      "Phylum complete:",
      round(sum(!is.na(final_taxonomy$phylum)) / nrow(final_taxonomy) * 100, 1),
      "%"
    ),
    paste(
      "Class complete:",
      round(sum(!is.na(final_taxonomy$class)) / nrow(final_taxonomy) * 100, 1),
      "%"
    ),
    paste(
      "Order complete:",
      round(sum(!is.na(final_taxonomy$order)) / nrow(final_taxonomy) * 100, 1),
      "%"
    ),
    paste(
      "Family complete:",
      round(sum(!is.na(final_taxonomy$family)) / nrow(final_taxonomy) * 100, 1),
      "%"
    ),
    paste(
      "Genus complete:",
      round(sum(!is.na(final_taxonomy$genus)) / nrow(final_taxonomy) * 100, 1),
      "%"
    )
  )

  writeLines(report_lines, summary_file)
  cat("📋 Summary report saved:", summary_file, "\n")

  # Step 7: Display sample results ----
  if (nrow(final_taxonomy) > 0) {
    cat("\n📋 Sample of enriched taxonomy (first 5 records):\n")
    sample_display <- final_taxonomy |>
      head(5) |>
      dplyr::select(
        display_name,
        authority,
        kingdom,
        phylum,
        class,
        order,
        family,
        genus,
        match_status
      )

    print(sample_display)
  }

  # return comprehensive results
  list(
    taxonomy_table = final_taxonomy,
    failed_matches = failed_records,
    summary_stats  = summary_stats,
    output_files   = list(
      main_results   = output_file,
      failed_matches = if (nrow(failed_records) > 0) failed_file else NULL,
      summary_report = summary_file
    )
  )
}

# Convenience Functions ----

#' @title Quick Test Run
#'
#' @description Run enrichment on a small sample to verify configuration and API
#'   access.
#'
#' @param n integer Number of records to process.
#'
#' @return list See `enrich_arthropod_taxonomy_complete()` return value.
#' @examples
#' # quick_test(5)
quick_test <- function(n = 5) {
  cat("⚡ Quick test with", n, "records...\n")
  enrich_arthropod_taxonomy_complete(sample_size = n)
}

#' @title Production Run
#'
#' @description Run enrichment for all active records with supported
#'   authorities.
#'
#' @return list See `enrich_arthropod_taxonomy_complete()` return value.
#' @examples
#' # production_run()
production_run <- function() {
  cat("🏭 Starting production enrichment of all records...\n")
  result <- enrich_arthropod_taxonomy_complete()
  
  if (!is.null(result)) {
    cat("🎉 Production run complete!\n")
    cat("📁 Check the", CONFIG$output_dir, "directory for all output files.\n")
  }
  
  return(result)
}


#' @title Get Latest Enriched Taxonomy CSV
#'
#' @description Find and return the path to the most recent
#'   `arthropod_taxonomy_enriched_*.csv` in a directory.
#'
#' @param dir character Directory to search; defaults to `CONFIG$output_dir`.
#'
#' @return character File path to the newest enriched CSV.
#' @examples
#' # latest_csv <- get_latest_enriched_csv()
get_latest_enriched_csv <- function(dir = CONFIG$output_dir) {
  files <- base::list.files(path = dir, pattern = "^arthropod_taxonomy_enriched_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No arthropod_taxonomy_enriched_*.csv files found in: ", dir)
  }
  files |>
    purrr::map_chr(~ .x) |>
    purrr::map_dfr(~ tibble::tibble(file = .x, mtime = base::file.info(.x)$mtime)) |>
    dplyr::arrange(dplyr::desc(.data$mtime)) |>
    dplyr::slice(1) |>
    dplyr::pull(.data$file)
}

# taxonomy table: enriched taxonomy + non-ITIS/GBIF # records ----  

latest_csv <- get_latest_enriched_csv()

taxonomy_table <- readr::read_csv(latest_csv) |>
  dplyr::select(
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
    "subspecies"
  )

not_itis_gbif <- DBI::dbGetQuery(
  conn = pg,
  statement = "
  WITH authorities AS (
    SELECT
      *,
      CASE
        WHEN authority = 'ITIS' THEN 'ITIS'
        WHEN authority ~~* '%gbif%' or authority ~~* '%global%' THEN 'GBIF'
        ELSE 'OTHER'
      END AS authority_source   
    FROM arthropods.arthropod_taxonomy 
  )
  SELECT
    display_name,
    authority,
    -- authority_source,
    authority_id,
    rank
  FROM authorities
  WHERE
    archive IS NOT TRUE
    AND display_name !~~* 'unknown'
    AND authority_source = 'OTHER'
    AND display_name IS NOT NULL
    ;
    "
)

taxonomy_table <- taxonomy_table |>
  dplyr::mutate(authority_id = as.integer(authority_id)) |>
  dplyr::bind_rows(
    not_itis_gbif |>
      dplyr::mutate(authority_id = as.integer(authority_id))
  )

# postprocessing: checking output that result in edits to the taxonomy table
# that are documented in the databases_change_log.

(itis_result <- ritis::itis_search(q = "nameWOInd:acari"))
(itis_result <- ritis::itis_search(q = "nameWOInd:arachnida"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Anobiidae"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Ptinidae"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Apionidae"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Brentidae"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Archaeognatha"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Charhyphus"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Cicadidae"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Cicindelinae"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Cicindelidae"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Coccoidea"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Collembola"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Cymindis\\ Pinacodera"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Nothrus"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Polyphagidae"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Corydiidae"))
(itis_result <- ritis::itis_search(q = "nameWOInd:Dacnochilus"))

(gbif_result <- rgbif::name_backbone("Cicindelinae"))
(gbif_result <- rgbif::name_backbone("Charhyphus"))