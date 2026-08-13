#' Stage CASSN metadata for an occurrence build
#'
#' Copy the current CASSN site and Motus configuration plus the ingest-app
#' metadata from explicitly selected deployment event folders into a staged occurrence
#' folder. Source files are copied, not linked, so the staged folder records the
#' metadata snapshot used for that build.
#'
#' @param staged_dir Destination staged-folder root. It is created when needed.
#' @param deployment_dirs Character vector of CASSN field-data deployment event
#'   folders. Each folder must contain `audio_file_metadata.csv`,
#'   `image_file_metadata.csv`, or both.
#' @param reference_dir Directory containing the canonical `sites.csv` and
#'   `motus.csv` reference files. This is normally the Box-synchronized CASSN
#'   `app_config` folder, but any local directory with those files may be used.
#'
#' @return A file-level staging summary, invisibly, with source and destination
#'   paths for each copied file.
#'
#' @details Configuration files are copied to `metadata_inputs/`. Deployment
#' metadata is kept in one `metadata_inputs/<deployment-event-folder-name>/`
#' subfolder per deployment event. [write_occurrence_csv()] searches these
#' subfolders recursively.
#' Rerunning the function refreshes the selected snapshots in place. Create a
#' new staged folder for each new publication build; reuse an existing staged
#' folder when reproducing or debugging that same build.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' stage_metadata_inputs(
#'   "/path/to/staged_dir",
#'   c("/path/to/UC_SiteOne_20260101", "/path/to/UC_SiteTwo_20260102"),
#'   "/path/to/CASSN/app_config"
#' )
#' }
stage_metadata_inputs <- function(staged_dir, deployment_dirs, reference_dir) {
  scalar_path <- function(x, name) {
    if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
      stop(name, " must be one nonblank path.", call. = FALSE)
    }
    path.expand(x)
  }

  staged_dir <- scalar_path(staged_dir, "staged_dir")
  reference_dir <- scalar_path(reference_dir, "reference_dir")
  if (!dir.exists(reference_dir)) {
    stop("reference_dir does not exist: ", reference_dir, call. = FALSE)
  }
  reference_dir <- normalizePath(reference_dir, mustWork = TRUE)

  if (!length(deployment_dirs)) {
    stop("deployment_dirs must contain at least one deployment folder.", call. = FALSE)
  }
  deployment_dirs <- path.expand(as.character(deployment_dirs))
  blank <- is.na(deployment_dirs) | !nzchar(trimws(deployment_dirs))
  if (any(blank)) {
    stop("deployment_dirs contains a blank path.", call. = FALSE)
  }
  missing_dirs <- deployment_dirs[!dir.exists(deployment_dirs)]
  if (length(missing_dirs)) {
    stop(
      "Deployment folder(s) do not exist: ",
      paste(missing_dirs, collapse = ", "),
      call. = FALSE
    )
  }
  deployment_dirs <- normalizePath(deployment_dirs, mustWork = TRUE)
  if (anyDuplicated(deployment_dirs)) {
    stop("deployment_dirs contains a duplicate folder.", call. = FALSE)
  }

  deployment_names <- basename(deployment_dirs)
  if (anyDuplicated(deployment_names)) {
    stop(
      "Deployment folders must have unique folder names: ",
      paste(unique(deployment_names[duplicated(deployment_names)]), collapse = ", "),
      call. = FALSE
    )
  }

  config_names <- c("sites.csv", "motus.csv")
  config_sources <- file.path(reference_dir, config_names)
  missing_config <- config_sources[!file.exists(config_sources)]
  if (length(missing_config)) {
    stop(
      "Missing canonical configuration file(s): ",
      paste(missing_config, collapse = ", "),
      call. = FALSE
    )
  }

  metadata_names <- c("audio_file_metadata.csv", "image_file_metadata.csv")
  deployment_files <- lapply(deployment_dirs, function(path) {
    candidates <- file.path(path, metadata_names)
    candidates[file.exists(candidates)]
  })
  missing_metadata <- deployment_names[lengths(deployment_files) == 0L]
  if (length(missing_metadata)) {
    stop(
      "No ingest metadata CSV found in deployment folder(s): ",
      paste(missing_metadata, collapse = ", "),
      call. = FALSE
    )
  }

  dir.create(staged_dir, recursive = TRUE, showWarnings = FALSE)
  staged_dir <- normalizePath(staged_dir, mustWork = TRUE)
  metadata_root <- file.path(staged_dir, "metadata_inputs")
  dir.create(metadata_root, recursive = TRUE, showWarnings = FALSE)

  copied <- list()
  copy_snapshot <- function(source, destination, category, deployment = NA_character_) {
    ok <- file.copy(
      source, destination, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE
    )
    if (!isTRUE(ok)) {
      stop("Could not stage metadata file: ", source, call. = FALSE)
    }
    copied[[length(copied) + 1L]] <<- data.frame(
      category = category,
      deployment = deployment,
      file = basename(source),
      source = normalizePath(source, mustWork = TRUE),
      staged = normalizePath(destination, mustWork = TRUE),
      stringsAsFactors = FALSE
    )
  }

  for (i in seq_along(config_sources)) {
    copy_snapshot(
      config_sources[[i]], file.path(metadata_root, config_names[[i]]),
      category = "configuration"
    )
  }

  for (i in seq_along(deployment_dirs)) {
    destination_dir <- file.path(metadata_root, deployment_names[[i]])
    dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)

    # Exact replacement prevents a removed source sidecar from surviving as a
    # stale file when the same staged build is deliberately refreshed.
    old_files <- file.path(destination_dir, metadata_names)
    unlink(old_files[file.exists(old_files)])

    for (source in deployment_files[[i]]) {
      copy_snapshot(
        source, file.path(destination_dir, basename(source)),
        category = "deployment metadata",
        deployment = deployment_names[[i]]
      )
    }
  }

  summary <- dplyr::bind_rows(copied)
  message(
    "Staged CASSN metadata in metadata_inputs/:\n",
    "- Field-data deployment event folders: ", length(deployment_dirs), "\n",
    "- Ingest metadata CSVs: ",
    sum(summary$category == "deployment metadata"), "\n",
    "- Configuration CSVs: ",
    sum(summary$category == "configuration")
  )
  invisible(summary)
}
