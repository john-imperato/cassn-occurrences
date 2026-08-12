read_wi_export <- function(dir) {
  deployment_paths <- list.files(
    dir, pattern = "^deployments\\.csv$", recursive = TRUE, full.names = TRUE
  )
  if (!length(deployment_paths)) {
    stop("No Wildlife Insights deployments.csv found under ", dir, call. = FALSE)
  }

  exports <- lapply(deployment_paths, function(deployments_path) {
    export_dir <- dirname(deployments_path)
    staged_deployment_id <- basename(export_dir)
    images_path <- list.files(
      export_dir, pattern = "^images.*\\.csv$", full.names = TRUE
    )
    if (length(images_path) != 1L) {
      stop("Expected one Wildlife Insights images CSV beside ", deployments_path,
           "; found ", length(images_path), call. = FALSE)
    }
    character_columns <- readr::cols(.default = readr::col_character())
    deployments <- readr::read_csv(
      deployments_path, show_col_types = FALSE, col_types = character_columns
    )
    images <- readr::read_csv(
      images_path, show_col_types = FALSE, col_types = character_columns
    )
    if (!"deployment_id" %in% names(deployments) ||
        !"deployment_id" %in% names(images)) {
      stop("Wildlife Insights files are missing deployment_id under ", export_dir,
           call. = FALSE)
    }
    represented <- unique(blank_to_na(c(
      as.character(deployments$deployment_id),
      as.character(images$deployment_id)
    )))
    represented <- represented[!is.na(represented)]
    if (length(represented) != 1L || represented[[1]] != staged_deployment_id) {
      stop(
        "Wildlife Insights folder must contain exactly deployment ",
        staged_deployment_id, "; found: ",
        paste(represented, collapse = ", "),
        call. = FALSE
      )
    }
    list(deployments = deployments, images = images)
  })

  list(
    deployments = dplyr::bind_rows(lapply(exports, `[[`, "deployments")),
    images = dplyr::bind_rows(lapply(exports, `[[`, "images"))
  )
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
