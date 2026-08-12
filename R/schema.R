cassn_occurrence_fields <- tibble::tribble(
  ~name, ~type, ~required,
  "occurrenceID", "string", TRUE,
  "occurrenceKey", "string", TRUE,
  "platform", "string", TRUE,
  "basisOfRecord", "string", TRUE,
  "organization", "string", TRUE,
  "eventDate", "string", TRUE,
  "eventTimeZone", "string", TRUE,
  "utc_timestamp", "timestamp", TRUE,
  "scientificName", "string", TRUE,
  "vernacularName", "string", FALSE,
  "taxonRank", "string", FALSE,
  "decimalLatitude", "double", TRUE,
  "decimalLongitude", "double", TRUE,
  "geodeticDatum", "string", TRUE,
  "site", "string", FALSE,
  "site_short_name", "string", FALSE,
  "site_code", "string", FALSE,
  "plot", "string", FALSE,
  "individualCount", "integer", FALSE,
  "organismID", "string", FALSE,
  "samplingProtocol", "string", TRUE,
  "deploymentID", "string", FALSE,
  "class", "string", FALSE,
  "order", "string", FALSE,
  "family", "string", FALSE,
  "genus", "string", FALSE,
  "species", "string", FALSE,
  "associatedMedia", "string", FALSE,
  "sensor_make", "string", FALSE,
  "sensor_model", "string", FALSE,
  "sensor_firmware", "string", FALSE,
  "date_installed", "date", FALSE,
  "deployment_start_date", "date", FALSE,
  "deployment_end_date", "date", FALSE,
  "wi_image_id", "string", FALSE,
  "wi_project_id", "string", FALSE,
  "wi_cv_confidence", "double", FALSE,
  "wi_uncertainty", "string", FALSE,
  "wi_bait_type", "string", FALSE,
  "wi_bait_description", "string", FALSE,
  "wi_feature_type", "string", FALSE,
  "wi_camera_functioning", "string", FALSE,
  "wi_bounding_boxes", "string", FALSE,
  "wi_highlighted", "string", FALSE,
  "wi_behavior", "string", FALSE,
  "wi_individual_animal_notes", "string", FALSE,
  "nabat_grts_cell_id", "string", FALSE,
  "nabat_auto_species_code", "string", FALSE,
  "nabat_manual_species_code", "string", FALSE,
  "nabat_auto_species_list", "string", FALSE,
  "nabat_manual_species_list", "string", FALSE,
  "nabat_grid_cell_quadrant", "string", FALSE,
  "nabat_microphone_type", "string", FALSE,
  "nabat_mic_serial_number", "string", FALSE,
  "nabat_water_type", "string", FALSE,
  "nabat_water_distance", "double", FALSE,
  "nabat_usnvc_habitat_code", "string", FALSE,
  "nabat_land_unit_code", "string", FALSE,
  "nabat_auto_id_software", "string", FALSE,
  "nabat_unusual_occurrences", "string", FALSE,
  "nabat_broad_habitat_type", "string", FALSE,
  "m_run_id", "string", FALSE,
  "m_motus_tag_id", "string", FALSE,
  "m_tag_deploy_id", "string", FALSE,
  "m_species_id", "string", FALSE,
  "m_full_id", "string", FALSE,
  "m_ambig_id", "string", FALSE,
  "m_tag_project", "string", FALSE,
  "m_tag_manufacturer", "string", FALSE,
  "m_tag_model", "string", FALSE,
  "m_nomFreq", "double", FALSE,
  "m_species_group", "string", FALSE,
  "m_run_length", "double", FALSE,
  "m_port", "string", FALSE,
  "m_antenna_type", "string", FALSE,
  "m_antenna_bearing", "double", FALSE,
  "m_receiver_name", "string", FALSE,
  "m_receiver_serial", "string", FALSE,
  "m_station_id", "string", FALSE,
  "m_receiver_type", "string", FALSE,
  "m_recv_deploy_id", "string", FALSE
)

empty_occurrence_table <- function() {
  x <- stats::setNames(vector("list", nrow(cassn_occurrence_fields)), cassn_occurrence_fields$name)
  conform_occurrence_schema(tibble::as_tibble(x))
}

conform_occurrence_schema <- function(df) {
  for (field in cassn_occurrence_fields$name) {
    if (!field %in% names(df)) {
      df[[field]] <- NA
    }
  }

  df <- df[cassn_occurrence_fields$name]
  for (i in seq_len(nrow(cassn_occurrence_fields))) {
    field <- cassn_occurrence_fields$name[[i]]
    type <- cassn_occurrence_fields$type[[i]]
    df[[field]] <- cast_occurrence_column(df[[field]], type)
  }
  tibble::as_tibble(df)
}

assert_unique_occurrence_ids <- function(df) {
  ids <- df$occurrenceID
  n_missing <- sum(is.na(ids) | !nzchar(ids))
  if (n_missing > 0) {
    stop(sprintf("%d occurrence row(s) have a missing occurrenceID.", n_missing), call. = FALSE)
  }
  invalid <- !grepl("^CASSN-[0-9a-f]{32}$", ids)
  if (any(invalid)) {
    stop(sprintf("%d occurrenceID value(s) do not match the CASSN text-ID format.",
                 sum(invalid)),
         call. = FALSE)
  }
  keys <- df$occurrenceKey
  n_missing_keys <- sum(is.na(keys) | !nzchar(keys))
  if (n_missing_keys > 0) {
    stop(sprintf("%d occurrence row(s) have a missing occurrenceKey.", n_missing_keys),
         call. = FALSE)
  }
  expected <- occurrence_id_from_key(keys)
  mismatched <- ids != expected
  if (any(mismatched)) {
    stop(sprintf("%d occurrenceID value(s) do not match occurrenceKey.", sum(mismatched)),
         call. = FALSE)
  }
  dups <- unique(ids[duplicated(ids)])
  if (length(dups) > 0) {
    stop(sprintf("Duplicate occurrenceID(s): %d, e.g. %s",
                 length(dups), paste(utils::head(dups, 3), collapse = ", ")), call. = FALSE)
  }
  invisible(df)
}

bind_occurrences <- function(...) {
  combined <- dplyr::bind_rows(...)
  assert_unique_occurrence_ids(combined)
  combined
}

.write_occurrence_table_csv <- function(df, path) {
  df <- conform_occurrence_schema(df)
  assert_unique_occurrence_ids(df)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path, na = "")
  invisible(path)
}

cast_occurrence_column <- function(x, type) {
  switch(
    type,
    string = as.character(x),
    double = suppressWarnings(as.numeric(x)),
    integer = suppressWarnings(as.integer(x)),
    logical = as.logical(x),
    date = as.Date(x),
    timestamp = as.POSIXct(x, tz = "UTC"),
    timestamp_tz = as.POSIXct(x, tz = attr(x, "tzone") %||% "UTC"),
    x
  )
}
