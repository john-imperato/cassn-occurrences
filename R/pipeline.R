# Deployment identities named by the staged CASSN ingest metadata. Used to
# report which Wildlife Insights deployments have no CASSN metadata behind them.
staged_metadata_deployment_ids <- function(metadata_root) {
  files <- list.files(
    metadata_root,
    pattern = "^(audio|image)_file_metadata\\.csv$",
    recursive = TRUE, full.names = TRUE
  )
  ids <- unlist(lapply(files, function(path) {
    data <- tryCatch(
      readr::read_csv(
        path, show_col_types = FALSE, progress = FALSE,
        col_select = dplyr::any_of("deployment_id"),
        col_types = readr::cols(.default = readr::col_character())
      ),
      error = function(e) NULL
    )
    if (is.null(data) || !"deployment_id" %in% names(data)) return(NULL)
    blank_to_na(data$deployment_id)
  }), use.names = FALSE)
  unique(ids[!is.na(ids)])
}

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
#'     WI_DOWNLOAD_BUNDLE/       # the unzipped Wildlife Insights download
#'       deployments.csv
#'       images*.csv
#'       projects.csv
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
#' One Wildlife Insights download bundle is the unit of staging: unzip the
#' download and drop the folder in as it came. Every deployment the bundle
#' contains is discovered from `deployments.csv` and published, so scope the
#' Wildlife Insights request to what the build should cover. Several bundles may
#' be staged together as long as no deployment appears in two of them. NABat
#' exports keep one deployment-ID subfolder each, because NABat exports arrive
#' per deployment rather than per request.
#'
#' Metadata CSVs are discovered recursively under `metadata_inputs/`;
#' `image_file_metadata.csv` is optional and adds camera make and model when
#' present. Staged metadata normally covers more deployments than the Wildlife
#' Insights bundle does, which is expected; a Wildlife Insights deployment with
#' no staged metadata is reported and published without sensor detail. The
#' function fails with a short error when a required input, metadata join, site
#' crosswalk, bundle integrity check, or unique occurrence identifier is missing.
#'
#' @return The written occurrence data frame, invisibly. Its `output_csv`,
#'   `build_receipt`, `run_summary`, and `wi_deployments` attributes contain the
#'   normalized output path, the build receipt path, the input, excluded, and
#'   published counts by platform, and the per-deployment Wildlife Insights
#'   counts.
#'
#' @details A build receipt is written beside the CSV as
#'   `<product>_build_receipt.json`, recording the package version and git state,
#'   the R version and platform, the build timestamp and user, the build
#'   parameters, and the product's row count, column count, and SHA-256. These
#'   are the facts a later promotion step cannot recover; everything else in a
#'   release manifest is recomputable from the CSV and the staged inputs. Git
#'   GitHub-installed packages retain their resolved source commit; Git fields
#'   are `null` only when neither installation metadata nor a source checkout
#'   identifies one. `git_clean` is `false` when a source checkout carried
#'   uncommitted changes — a release should not be published from such a build,
#'   because the recorded commit is then not the complete code that ran.
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
  # Report the bundle scope before any transform runs, so the deployments this
  # build will publish are visible at the top of the output.
  message(
    "Wildlife Insights bundle(s):\n",
    paste(
      sprintf(
        "- %s (project %s): %d deployment(s), %d image row(s)",
        wi_input$bundles$bundle, wi_input$bundles$project_id,
        wi_input$bundles$deployments, wi_input$bundles$images
      ),
      collapse = "\n"
    ),
    "\n",
    paste(
      sprintf(
        "  %s (%d image rows)",
        wi_input$deployment_summary$deployment_id,
        wi_input$deployment_summary$images
      ),
      collapse = "\n"
    )
  )

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

  # Written now, not by a later promotion step: package version, git state, R
  # version, build time, and user cease to be knowable once the build ends.
  receipt_path <- build_receipt_path(output_csv)
  write_build_receipt(
    build_receipt(output_csv, occurrences, organization, timezone),
    receipt_path
  )

  wi_deployments <- wi_input$deployment_summary
  wi_deployments$published <- vapply(
    wi_deployments$deployment_id,
    function(id) sum(wi$deploymentID == id, na.rm = TRUE),
    numeric(1)
  )
  wi_deployments$published <- as.integer(wi_deployments$published)

  # One-directional on purpose. Staged metadata routinely covers deployments a
  # given Wildlife Insights download does not carry, which is normal; a bundled
  # deployment with no CASSN metadata is the case worth surfacing, because it
  # publishes without sensor make and model.
  unmatched <- setdiff(
    wi_deployments$deployment_id, staged_metadata_deployment_ids(paths$metadata)
  )
  if (length(unmatched)) {
    warning(
      "No staged CASSN metadata for Wildlife Insights deployment(s): ",
      paste(unmatched, collapse = ", "),
      ". Published without sensor make and model.",
      call. = FALSE
    )
  }

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
  message("Wildlife Insights by deployment:")
  message(paste(
    utils::capture.output(print(
      wi_deployments[c("deployment_id", "images", "published")],
      row.names = FALSE
    )),
    collapse = "\n"
  ))

  message("Build receipt: ", normalizePath(receipt_path, mustWork = TRUE))

  attr(occurrences, "output_csv") <- normalizePath(output_csv, mustWork = TRUE)
  attr(occurrences, "build_receipt") <- normalizePath(receipt_path, mustWork = TRUE)
  attr(occurrences, "run_summary") <- summary
  attr(occurrences, "wi_deployments") <- wi_deployments
  invisible(occurrences)
}
