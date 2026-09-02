# A Wildlife Insights download bundle is the staging unit. Wildlife Insights
# fulfills one download per request and hands back a single folder holding
# `deployments.csv`, one or more `images*.csv`, and usually `projects.csv` and
# `cameras.csv` alongside its PDF documentation. One bundle covers a whole
# request, so it may carry many deployments. The deployments in a bundle are
# discovered from its files; no folder-naming convention is imposed.

# Columns the transform cannot do without. A missing one means the export is
# the wrong shape, so we name it rather than let dplyr fail deep in a pipeline.
wi_required_deployment_columns <- c(
  "project_id", "deployment_id", "longitude", "latitude",
  "start_date", "end_date"
)
wi_required_image_columns <- c(
  "project_id", "deployment_id", "image_id", "filename", "is_blank",
  "timestamp", "class", "order", "family", "genus", "species", "common_name"
)

# Columns the transform reads but can publish without. Wildlife Insights omits
# some of these depending on project settings (for example `number_of_objects`
# when the count field was made optional), so absence is filled rather than
# treated as an error.
wi_optional_deployment_columns <- c(
  "bait_type", "bait_description", "feature_type", "feature_type_methodology",
  "camera_functioning", "camera_id", "camera_name", "placename",
  "subproject_name", "event_name"
)
wi_optional_image_columns <- c(
  "number_of_objects", "uncertainty", "cv_confidence", "bounding_boxes",
  "highlighted", "behavior", "individual_animal_notes", "identified_by",
  "wi_taxon_id", "age", "sex", "individual_id", "markings", "license",
  "location"
)

fill_missing_columns <- function(df, columns) {
  for (column in setdiff(columns, names(df))) {
    df[[column]] <- NA_character_
  }
  df
}

assert_columns <- function(df, required, label, path) {
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop(
      label, " is missing required column(s): ",
      paste(missing, collapse = ", "), "\n  ", path,
      call. = FALSE
    )
  }
  invisible(df)
}

# Every directory holding a deployments.csv is one bundle. This finds the
# bundle whether its folder was dropped in whole (the normal case) or its
# contents were extracted directly into `dir`.
wi_bundle_dirs <- function(dir) {
  deployment_paths <- list.files(
    dir, pattern = "^deployments\\.csv$", recursive = TRUE, full.names = TRUE
  )
  if (!length(deployment_paths)) {
    zipped <- list.files(
      dir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(zipped)) {
      stop(
        "Wildlife Insights download(s) under ", dir, " are still zipped: ",
        paste(basename(zipped), collapse = ", "),
        "\nUnzip the download and drop the extracted folder here.",
        call. = FALSE
      )
    }
    stop("No Wildlife Insights deployments.csv found under ", dir, call. = FALSE)
  }
  unique(dirname(deployment_paths))
}

read_wi_bundle <- function(bundle_dir) {
  character_columns <- readr::cols(.default = readr::col_character())
  read_csv_chr <- function(path) {
    readr::read_csv(
      path, show_col_types = FALSE, progress = FALSE,
      col_types = character_columns
    )
  }

  deployments_path <- file.path(bundle_dir, "deployments.csv")
  images_paths <- sort(list.files(
    bundle_dir, pattern = "^images.*\\.csv$", full.names = TRUE
  ))
  if (!length(images_paths)) {
    stop(
      "No Wildlife Insights images CSV beside ", deployments_path,
      call. = FALSE
    )
  }

  deployments <- read_csv_chr(deployments_path)
  # Large exports are split across several images CSVs, so every match is read.
  images <- dplyr::bind_rows(lapply(images_paths, read_csv_chr))

  assert_columns(
    deployments, wi_required_deployment_columns, "deployments.csv",
    deployments_path
  )
  assert_columns(
    images, wi_required_image_columns, "Wildlife Insights images CSV",
    paste(images_paths, collapse = ", ")
  )
  deployments <- fill_missing_columns(deployments, wi_optional_deployment_columns)
  images <- fill_missing_columns(images, wi_optional_image_columns)

  # Sequence-level projects publish identifications in sequences.csv and their
  # images.csv carries neither is_blank nor number_of_objects, so the image
  # occurrence rules do not apply to them.
  projects_path <- file.path(bundle_dir, "projects.csv")
  project_id <- NA_character_
  if (file.exists(projects_path)) {
    projects <- read_csv_chr(projects_path)
    if ("project_type" %in% names(projects)) {
      types <- unique(blank_to_na(projects$project_type))
      types <- types[!is.na(types)]
      unsupported <- types[tolower(types) != "image"]
      if (length(unsupported)) {
        stop(
          "Wildlife Insights bundle ", basename(bundle_dir),
          " is a ", paste(unsupported, collapse = "/"),
          "-level project; only image-level projects are supported.",
          call. = FALSE
        )
      }
    }
    if ("project_id" %in% names(projects)) {
      project_id <- paste(
        unique(blank_to_na(projects$project_id)), collapse = ", "
      )
    }
  }
  if (is.na(project_id)) {
    project_id <- paste(
      unique(blank_to_na(deployments$project_id)), collapse = ", "
    )
  }

  # Public downloads obscure coordinates and flag it here. Those rows cannot be
  # published as occurrence localities, so the bundle is rejected outright.
  if ("fuzzed" %in% names(deployments)) {
    fuzzed <- tolower(trimws(as.character(deployments$fuzzed)))
    if (any(fuzzed %in% c("true", "1", "yes"), na.rm = TRUE)) {
      stop(
        "Wildlife Insights bundle ", basename(bundle_dir),
        " has fuzzed (obscured) coordinates; request a private download.",
        call. = FALSE
      )
    }
  }

  deployment_ids <- blank_to_na(deployments$deployment_id)
  if (anyNA(deployment_ids)) {
    stop(
      "deployments.csv has a blank deployment_id in bundle ",
      basename(bundle_dir), call. = FALSE
    )
  }
  duplicated_ids <- unique(deployment_ids[duplicated(deployment_ids)])
  if (length(duplicated_ids)) {
    stop(
      "deployments.csv repeats deployment_id(s) in bundle ",
      basename(bundle_dir), ": ", paste(duplicated_ids, collapse = ", "),
      call. = FALSE
    )
  }

  image_deployment_ids <- blank_to_na(images$deployment_id)
  orphans <- setdiff(
    unique(image_deployment_ids[!is.na(image_deployment_ids)]), deployment_ids
  )
  if (length(orphans)) {
    stop(
      "Wildlife Insights images reference deployment(s) absent from ",
      "deployments.csv in bundle ", basename(bundle_dir), ": ",
      paste(orphans, collapse = ", "),
      call. = FALSE
    )
  }

  counts <- vapply(
    deployment_ids,
    function(id) sum(image_deployment_ids == id, na.rm = TRUE),
    numeric(1)
  )
  deployment_summary <- data.frame(
    bundle = basename(bundle_dir),
    deployment_id = deployment_ids,
    images = as.integer(counts),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  list(
    deployments = deployments,
    images = images,
    bundle = data.frame(
      bundle = basename(bundle_dir),
      path = normalizePath(bundle_dir, mustWork = TRUE),
      project_id = project_id,
      deployments = length(deployment_ids),
      images = nrow(images),
      images_files = length(images_paths),
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    deployment_summary = deployment_summary
  )
}

read_wi_export <- function(dir) {
  bundle_dirs <- wi_bundle_dirs(dir)
  bundles <- lapply(bundle_dirs, read_wi_bundle)

  deployment_summary <- dplyr::bind_rows(
    lapply(bundles, `[[`, "deployment_summary")
  )
  # The same deployment arriving in two bundles would mint duplicate
  # occurrenceIDs, which surfaces much later as an opaque uniqueness failure.
  repeated <- unique(deployment_summary$deployment_id[
    duplicated(deployment_summary$deployment_id)
  ])
  if (length(repeated)) {
    stop(
      "Deployment(s) appear in more than one Wildlife Insights bundle: ",
      paste(repeated, collapse = ", "),
      "\nKeep one bundle per deployment under ", dir,
      call. = FALSE
    )
  }

  list(
    deployments = dplyr::bind_rows(lapply(bundles, `[[`, "deployments")),
    images = dplyr::bind_rows(lapply(bundles, `[[`, "images")),
    bundles = dplyr::bind_rows(lapply(bundles, `[[`, "bundle")),
    deployment_summary = deployment_summary
  )
}

# Deployment identities only, for staging. Reading deployments.csv alone avoids
# parsing the (often very large) images CSVs just to learn what is staged.
wi_staged_deployment_ids <- function(dir) {
  bundle_dirs <- wi_bundle_dirs(dir)
  ids <- unlist(lapply(bundle_dirs, function(bundle_dir) {
    deployments <- readr::read_csv(
      file.path(bundle_dir, "deployments.csv"), show_col_types = FALSE,
      progress = FALSE, col_types = readr::cols(.default = readr::col_character())
    )
    if (!"deployment_id" %in% names(deployments)) {
      stop(
        "deployments.csv is missing deployment_id in bundle ",
        basename(bundle_dir), call. = FALSE
      )
    }
    blank_to_na(deployments$deployment_id)
  }), use.names = FALSE)
  unique(ids[!is.na(ids)])
}

# CASSN camera protocol from the device-type token in the deployment name
# (`Org_Site_plotN_<DEVTYPE>_date`). Self-describing per row, so a single export
# spanning both ML and SA projects resolves correctly.
wi_sampling_protocol <- function(deployment_id) {
  dplyr::case_when(
    grepl("_ML_", deployment_id) ~ "CASSN ML Camera",
    grepl("_SA_", deployment_id) ~ "CASSN SA Camera",
    TRUE ~ "CASSN Camera"
  )
}

wi_vehicle_record <- function(...) {
  labels <- lapply(list(...), function(x) {
    tolower(trimws(as.character(x))) %in% c("vehicle", "vehicles")
  })
  Reduce(`|`, labels)
}

transform_wi_occurrences <- function(dir = NULL, images = NULL, deployments = NULL,
                                     organization = "UC",
                                     timezone = "America/Los_Angeles") {
  if (!is.null(dir)) {
    export <- read_wi_export(dir)
    images <- export$images
    deployments <- export$deployments
  }
  if (is.null(images) || is.null(deployments)) {
    stop("Provide either dir or both images and deployments.", call. = FALSE)
  }

  deployments_min <- deployments |>
    dplyr::transmute(
      deployment_id = as.character(.data$deployment_id),
      decimalLatitude = suppressWarnings(as.numeric(.data$latitude)),
      decimalLongitude = suppressWarnings(as.numeric(.data$longitude)),
      deployment_start_date = as.Date(.data$start_date),
      deployment_end_date = as.Date(.data$end_date),
      wi_bait_type = as.character(.data$bait_type),
      wi_bait_description = as.character(.data$bait_description),
      wi_feature_type = as.character(.data$feature_type),
      wi_camera_functioning = as.character(.data$camera_functioning)
    )

  joined <- images |>
    dplyr::left_join(deployments_min, by = c("deployment_id" = "deployment_id"))

  # scientificName = most specific rank present; humans/sentinels dropped.
  taxa <- resolve_taxon(joined$class, joined$order, joined$family,
                        joined$genus, joined$species)
  joined$scientificName <- taxa$scientificName
  joined$taxonRank <- taxa$taxonRank
  joined$isVehicle <- wi_vehicle_record(
    joined$common_name, joined$class, joined$order, joined$family,
    joined$genus, joined$species
  )
  joined$occurrenceKey <- make_occurrence_key(
    "wildlife_insights",
    occurrence_id_from_filename(joined$filename),
    joined$scientificName
  )
  joined$eventDateRaw <- lubridate::force_tz(
    lubridate::ymd_hms(joined$timestamp, quiet = TRUE), timezone
  )

  out <- joined |>
    dplyr::filter(
      suppressWarnings(as.integer(.data$is_blank)) != 1L,
      !.data$isVehicle,
      presence_filter(.data$scientificName)
    ) |>
    dplyr::transmute(
      occurrenceID = occurrence_id_from_key(.data$occurrenceKey),
      occurrenceKey = .data$occurrenceKey,
      platform = "wildlife_insights",
      basisOfRecord = "MachineObservation",
      organization = organization,
      eventDate = format_event_date(.data$eventDateRaw, timezone),
      eventTimeZone = timezone,
      utc_timestamp = to_utc_timestamp(.data$eventDateRaw),
      scientificName = .data$scientificName,
      vernacularName = blank_to_na(.data$common_name),
      taxonRank = .data$taxonRank,
      decimalLatitude = .data$decimalLatitude,
      decimalLongitude = .data$decimalLongitude,
      geodeticDatum = "WGS84",
      individualCount = suppressWarnings(as.integer(.data$number_of_objects)),
      samplingProtocol = wi_sampling_protocol(.data$deployment_id),
      deploymentID = as.character(.data$deployment_id),
      class = clean_taxon_token(.data$class),
      order = clean_taxon_token(.data$order),
      family = clean_taxon_token(.data$family),
      genus = clean_taxon_token(.data$genus),
      species = clean_taxon_token(.data$species),
      associatedMedia = blank_to_na(.data$filename),
      deployment_start_date = .data$deployment_start_date,
      deployment_end_date = .data$deployment_end_date,
      wi_image_id = as.character(.data$image_id),
      wi_project_id = as.character(.data$project_id),
      wi_cv_confidence = suppressWarnings(as.numeric(.data$cv_confidence)),
      wi_uncertainty = as.character(.data$uncertainty),
      wi_bait_type = .data$wi_bait_type,
      wi_bait_description = .data$wi_bait_description,
      wi_feature_type = .data$wi_feature_type,
      wi_camera_functioning = .data$wi_camera_functioning,
      wi_bounding_boxes = as.character(.data$bounding_boxes),
      wi_highlighted = as.character(.data$highlighted),
      wi_behavior = as.character(.data$behavior),
      wi_individual_animal_notes = blank_to_na(.data$individual_animal_notes)
    )

  conform_occurrence_schema(out)
}
