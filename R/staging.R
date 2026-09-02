# CASSN metadata files name their own deployment event: every row of
# `audio_file_metadata.csv` / `image_file_metadata.csv` carries an exact
# `deployment_id`, and the file sits in that deployment's event folder. Reading
# those two columns is therefore authoritative, where parsing a date out of the
# deployment ID is not: a deployment's end date is its own, not its event's, and
# the two differ whenever a device is retrieved on a different day. A `-seqNN`
# suffix is likewise part of an opaque identifier, not something to parse.

# Event folders sit a few levels down a human-named tree
# (`field_data/<year>/<Reserve Full Name>/<event>`) and hold bulky `raw_data`
# subfolders. The search widens one level at a time and stops descending as soon
# as a directory yields metadata, which keeps it out of the media.
CASSN_METADATA_FILES <- c("audio_file_metadata.csv", "image_file_metadata.csv")

find_event_metadata_files <- function(field_data_root, max_depth = 5L) {
  found <- character(0)
  level <- field_data_root
  depth <- 0L
  while (length(level) && depth < max_depth) {
    hits <- unlist(lapply(level, function(dir) {
      paths <- file.path(dir, CASSN_METADATA_FILES)
      paths[file.exists(paths)]
    }), use.names = FALSE)
    found <- c(found, hits)

    # Directories that yielded metadata are event folders; do not descend into
    # their raw_data.
    barren <- level[!vapply(level, function(dir) {
      any(file.exists(file.path(dir, CASSN_METADATA_FILES)))
    }, logical(1))]
    if (!length(barren)) break
    level <- list.dirs(barren, recursive = FALSE, full.names = TRUE)
    depth <- depth + 1L
  }
  found
}

read_deployment_id_column <- function(path) {
  header <- names(readr::read_csv(
    path, n_max = 0, col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  ))
  if (!"deployment_id" %in% header) {
    stop(
      "CASSN metadata file has no deployment_id column: ", path,
      call. = FALSE
    )
  }
  ids <- readr::read_csv(
    path,
    col_types = readr::cols_only(deployment_id = readr::col_character()),
    progress = FALSE
  )[["deployment_id"]]
  unique(ids[!is.na(ids) & nzchar(trimws(ids))])
}

# Index every deployment recorded anywhere under the field-data root to the
# event folder whose metadata names it. One event folder normally contributes
# both an audio and an image file, so directories are deduplicated per ID.
index_deployment_event_dirs <- function(field_data_root) {
  metadata_paths <- find_event_metadata_files(field_data_root)
  if (!length(metadata_paths)) {
    stop(
      "No CASSN metadata files found under ", field_data_root,
      ".\nExpected audio_file_metadata.csv or image_file_metadata.csv in each ",
      "deployment event folder.",
      call. = FALSE
    )
  }

  index <- new.env(parent = emptyenv())
  for (path in metadata_paths) {
    event_dir <- dirname(path)
    for (id in read_deployment_id_column(path)) {
      index[[id]] <- unique(c(index[[id]], event_dir))
    }
  }
  index
}

# Deployment identities already staged for this build: Wildlife Insights bundles
# name theirs inside deployments.csv, NABat exports name theirs by folder.
staged_deployment_ids <- function(staged_dir) {
  wi_dir <- file.path(staged_dir, "wildlife_insights")
  nabat_dir <- file.path(staged_dir, "nabat")

  ids <- character(0)
  if (dir.exists(wi_dir) && length(list.files(wi_dir))) {
    ids <- c(ids, wi_staged_deployment_ids(wi_dir))
  }
  if (dir.exists(nabat_dir)) {
    ids <- c(ids, list.dirs(nabat_dir, recursive = FALSE, full.names = FALSE))
  }
  unique(ids[!is.na(ids) & nzchar(ids)])
}

derive_deployment_dirs <- function(staged_dir, field_data_root) {
  if (!dir.exists(field_data_root)) {
    stop("field_data_root does not exist: ", field_data_root, call. = FALSE)
  }

  deployment_ids <- staged_deployment_ids(staged_dir)
  if (!length(deployment_ids)) {
    stop(
      "No staged platform exports found under ", staged_dir,
      ".\nDrop the Wildlife Insights download bundle in wildlife_insights/ ",
      "(and any NABat exports in nabat/) before staging metadata.",
      call. = FALSE
    )
  }

  index <- index_deployment_event_dirs(field_data_root)
  resolved <- lapply(deployment_ids, function(id) index[[id]])
  names(resolved) <- deployment_ids

  missing <- deployment_ids[lengths(resolved) == 0L]
  if (length(missing)) {
    stop(
      "No CASSN metadata under ", field_data_root,
      " names deployment(s): ", paste(sort(missing), collapse = ", "),
      ".\nStage the field data for these deployments, or correct the ",
      "deployment ID in the platform export.",
      call. = FALSE
    )
  }

  # One deployment belongs to exactly one event. Two event folders naming the
  # same deployment means the field data is duplicated or misfiled, and picking
  # either one would silently publish against the wrong metadata snapshot.
  ambiguous <- deployment_ids[lengths(resolved) > 1L]
  if (length(ambiguous)) {
    stop(
      "Deployment(s) named by more than one deployment event folder under ",
      field_data_root, ":\n",
      paste(
        vapply(
          sort(ambiguous),
          function(id) paste0(id, "\n  ", paste(resolved[[id]], collapse = "\n  ")),
          character(1)
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  event_dirs <- sort(unique(unlist(resolved, use.names = FALSE)))
  message(
    "Resolved ", length(deployment_ids), " staged deployment(s) to ",
    length(event_dirs), " deployment event(s):\n- ",
    paste(basename(event_dirs), collapse = "\n- ")
  )
  event_dirs
}

#' Stage CASSN metadata for an occurrence build
#'
#' Copy the current CASSN site and Motus configuration plus the ingest-app
#' metadata for this build's deployment events into a staged occurrence folder.
#' Source files are copied, not linked, so the staged folder records the
#' metadata snapshot used for that build.
#'
#' @param staged_dir Destination staged-folder root. It is created when needed.
#' @param deployment_dirs Character vector of CASSN field-data deployment event
#'   folders. Each folder must contain `audio_file_metadata.csv`,
#'   `image_file_metadata.csv`, or both. Leave `NULL` to derive the folders from
#'   the already-staged platform exports; see `field_data_root`.
#' @param reference_dir Directory containing the canonical `sites.csv` and
#'   `motus.csv` reference files. This is normally the Box-synchronized CASSN
#'   `app_config` folder, but any local directory with those files may be used.
#' @param field_data_root Root of the CASSN field-data tree, normally the
#'   Box-synchronized `CASSN/field_data` folder. When supplied instead of
#'   `deployment_dirs`, the deployment events for this build are derived from
#'   the staged platform exports and resolved to folders under this root.
#'
#' @return A file-level staging summary, invisibly, with source and destination
#'   paths for each copied file.
#'
#' @details Supply exactly one of `deployment_dirs` or `field_data_root`.
#'
#' With `field_data_root`, staging reads the deployment IDs from the Wildlife
#' Insights bundles in `wildlife_insights/` and the NABat export folders in
#' `nabat/`, then resolves each one through the CASSN metadata under the root:
#' every `audio_file_metadata.csv` and `image_file_metadata.csv` names the
#' deployments recorded in its own event folder, so the mapping is read from the
#' data rather than inferred from the identifier. Stage the platform exports
#' first, since they define which deployments this build needs. Staging fails
#' when no metadata names a staged deployment, or when two event folders name
#' the same one.
#'
#' Deployment IDs are treated as opaque. A deployment's date token is its own
#' end date rather than its event's, and the two differ whenever a device is
#' retrieved on a different day, so the event is never parsed out of the ID.
#'
#' An event folder covers every deployment recorded for that event, including
#' deployments absent from a given Wildlife Insights download, so the staged
#' metadata is normally broader than the staged exports.
#'
#' Configuration files are copied to `metadata_inputs/`. Deployment
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
#' # Derive the deployment events from the staged Wildlife Insights bundle.
#' stage_metadata_inputs(
#'   "/path/to/staged_dir",
#'   reference_dir = "/path/to/CASSN/app_config",
#'   field_data_root = "/path/to/CASSN/field_data"
#' )
#'
#' # Or name the deployment event folders explicitly.
#' stage_metadata_inputs(
#'   "/path/to/staged_dir",
#'   c("/path/to/UC_SiteOne_20260101", "/path/to/UC_SiteTwo_20260102"),
#'   "/path/to/CASSN/app_config"
#' )
#' }
stage_metadata_inputs <- function(staged_dir, deployment_dirs = NULL,
                                  reference_dir, field_data_root = NULL) {
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

  if (!is.null(deployment_dirs) && !is.null(field_data_root)) {
    stop(
      "Supply either deployment_dirs or field_data_root, not both.",
      call. = FALSE
    )
  }
  if (is.null(deployment_dirs)) {
    if (is.null(field_data_root)) {
      stop(
        "Supply deployment_dirs, or field_data_root to derive them from the ",
        "staged platform exports.",
        call. = FALSE
      )
    }
    field_data_root <- scalar_path(field_data_root, "field_data_root")
    if (!dir.exists(staged_dir)) {
      stop(
        "staged_dir does not exist: ", staged_dir,
        "\nStage the platform exports before deriving deployment events.",
        call. = FALSE
      )
    }
    deployment_dirs <- derive_deployment_dirs(
      normalizePath(staged_dir, mustWork = TRUE), field_data_root
    )
  }

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
