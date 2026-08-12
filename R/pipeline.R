#' Write a standardized CASSN occurrence CSV
#'
#' Read one conventionally staged folder containing Wildlife Insights, NABat or
#' SonoBat, Motus, and CASSN metadata inputs. The platform transforms apply
#' the package's inclusion rules, the enrichment step restores CASSN deployment
#' and site metadata, and the result is written as one CSV.
#'
#' @param staged_dir Path to the staged input folder. See Details for its layout.
#' @param output_csv Path for the occurrence CSV. Its parent directory is created
#'   when needed. An existing file at this exact path is replaced.
#' @param organization Organization value written to platform rows before any
#'   more specific ingest metadata is joined. The MVP default is `"UC"`.
#' @param timezone IANA time-zone name used to interpret Wildlife Insights and
#'   NABat local timestamps and to express Motus UTC times locally.
#'
#' @details The required layout is:
#' ```
#' staged_dir/
#'   wildlife_insights/
#'     DEPLOYMENT_ID/
#'       deployments.csv
#'       images*.csv
#'   nabat/
#'     DEPLOYMENT_ID/
#'       nabat_export.csv
#'   motus/
#'     RECEIVER_SERIAL.motus     # one or more receiver databases
#'   metadata_inputs/
#'     sites.csv
#'     motus.csv
#'     DEPLOYMENT_EVENT/
#'       audio_file_metadata.csv
#'       image_file_metadata.csv
#' ```
#' Metadata CSVs are discovered recursively under `metadata_inputs/`;
#' `image_file_metadata.csv` is optional and adds camera make and model when
#' present. The function fails with a short error when a required input,
#' metadata join, site crosswalk, folder identity, or unique occurrence
#' identifier is missing.
#'
#' @return The written occurrence data frame, invisibly. Its `output_csv` and
#'   `run_summary` attributes contain the normalized output path and the input,
#'   excluded, and published counts by platform.
#' @export
#'
#' @examples
#' \dontrun{
#' occurrences <- write_occurrence_csv(
#'   "/path/to/staged_dir",
#'   tempfile(fileext = ".csv")
#' )
#' }
write_occurrence_csv <- function(staged_dir, output_csv,
                                 organization = "UC",
                                 timezone = "America/Los_Angeles") {
  staged_dir <- normalizePath(staged_dir, mustWork = TRUE)
  output_csv <- path.expand(output_csv)

  paths <- list(
    wi = file.path(staged_dir, "wildlife_insights"),
    nabat = file.path(staged_dir, "nabat"),
    motus = file.path(staged_dir, "motus"),
    metadata = file.path(staged_dir, "metadata_inputs"),
    sites = file.path(staged_dir, "metadata_inputs", "sites.csv"),
    motus_crosswalk = file.path(staged_dir, "metadata_inputs", "motus.csv")
  )
  required <- unlist(paths, use.names = FALSE)
  missing <- required[!file.exists(required)]
  if (length(missing)) {
    stop("Missing staged input(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  motus_paths <- list.files(
    paths$motus, pattern = "\\.motus$", full.names = TRUE, ignore.case = TRUE
  )
  if (!length(motus_paths)) {
    stop("No .motus databases found under ", paths$motus, call. = FALSE)
  }

  wi_input <- read_wi_export(paths$wi)
  nabat_input <- read_nabat_export(paths$nabat)
  motus_input_n <- sum(vapply(motus_paths, function(path) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    tryCatch(
      DBI::dbGetQuery(con, "select count(*) as n from allruns")$n[[1]],
      finally = DBI::dbDisconnect(con)
    )
  }, numeric(1)))

  wi <- transform_wi_occurrences(
    images = wi_input$images, deployments = wi_input$deployments,
    organization = organization, timezone = timezone
  )
  nabat <- transform_nabat_occurrences(
    data = nabat_input, organization = organization, timezone = timezone
  )
  motus <- dplyr::bind_rows(lapply(motus_paths, function(path) {
    transform_motus_occurrences(
      path = path, organization = organization, timezone = timezone
    )
  }))

  occurrences <- bind_occurrences(wi, nabat, motus)
  occurrences <- enrich_nabat_from_audio_metadata(occurrences, paths$metadata)
  occurrences <- enrich_site_hierarchy(occurrences, paths$sites)
  occurrences <- enrich_sensor(occurrences, paths$metadata)
  occurrences <- enrich_motus_site(
    occurrences, paths$sites, paths$motus_crosswalk
  )
  occurrences <- conform_occurrence_schema(occurrences)
  .write_occurrence_table_csv(occurrences, output_csv)

  input <- c(nrow(wi_input$images), nrow(nabat_input), motus_input_n)
  published <- c(nrow(wi), nrow(nabat), nrow(motus))
  summary <- data.frame(
    platform = c("Wildlife Insights", "NABat", "Motus"),
    input = input,
    excluded = input - published,
    published = published,
    row.names = NULL,
    check.names = FALSE
  )
  message("Occurrence CSV: ", normalizePath(output_csv, mustWork = TRUE))
  message(paste(utils::capture.output(print(summary, row.names = FALSE)), collapse = "\n"))

  attr(occurrences, "output_csv") <- normalizePath(output_csv, mustWork = TRUE)
  attr(occurrences, "run_summary") <- summary
  invisible(occurrences)
}
