# Enrichment helpers run on the combined occurrence table before the CSV is
# written, rather than inside the individual platform transforms.

# Fill the site hierarchy (site / site_short_name / site_code / plot) from the CASSN
# app config `sites.csv`. Authoritative names come from that file; the deployment's
# site token + plot are parsed from the CASSN deploymentID convention
# (`UC_<siteCode>_<plotN>_<device>_<date>`). Motus is handled separately through
# an explicit Motus-station crosswalk because towers belong to sites, not plots.
# Rows that don't match the convention (e.g. non-UC data) keep NA site fields.
#
# Canonical app `sites.csv` columns are `site_name`, `site_short_name`, and
# `site_code`; `site_short_name` is the relational key used in deployment IDs.
enrich_site_hierarchy <- function(df, sites_path) {
  sites <- readr::read_csv(sites_path, show_col_types = FALSE)
  names(sites) <- tolower(names(sites))
  required <- c("site_name", "site_short_name", "site_code")
  missing <- setdiff(required, names(sites))
  if (length(missing)) {
    stop("sites.csv is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  dep <- as.character(df$deploymentID)
  is_uc <- grepl("^UC_[^_]+_", dep) & !is.na(dep)

  token <- ifelse(is_uc, sub("^UC_([^_]+)_.*$", "\\1", dep), NA_character_)

  plot <- ifelse(grepl("^UC_[^_]+_(plot[0-9]+)_", dep),
                 sub("^UC_[^_]+_(plot[0-9]+)_.*$", "\\1", dep), NA_character_)

  idx <- match(token, sites$site_short_name)
  hit <- !is.na(idx)
  df$site[hit] <- sites$site_name[idx[hit]]
  df$site_short_name[hit] <- sites$site_short_name[idx[hit]]
  df$site_code[hit] <- sites$site_code[idx[hit]]
  df$plot[!is.na(plot)] <- plot[!is.na(plot)]
  df
}

metadata_filename_key <- function(x) {
  x <- basename(as.character(x))
  x <- sub("\\.[^.]*$", "", x)
  tolower(trimws(x))
}

enrich_nabat_from_audio_metadata <- function(df, metadata_root, strict = TRUE) {
  files <- list.files(metadata_root, pattern = "^audio_file_metadata\\.csv$",
                      recursive = TRUE, full.names = TRUE)
  if (!length(files)) {
    stop("No audio_file_metadata.csv found under ", metadata_root, call. = FALSE)
  }

  metadata <- dplyr::bind_rows(lapply(
    files,
    readr::read_csv,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  ))
  required <- c("filename", "original_filename", "deployment_id", "organization",
                "site", "site_full_name", "site_code", "plot_number")
  missing <- setdiff(required, names(metadata))
  if (length(missing)) {
    stop("audio_file_metadata.csv is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  filename_key <- metadata_filename_key(metadata$filename)
  original_key <- metadata_filename_key(metadata$original_filename)

  rows <- which(df$platform == "nabat")
  key <- metadata_filename_key(df$associatedMedia[rows])
  match_unique <- function(keys, candidates, field) {
    vapply(keys, function(value) {
      hits <- which(!is.na(candidates) & candidates == value)
      if (length(hits) > 1L) {
        stop(
          "Audio metadata has multiple ", field, " matches for: ", value,
          call. = FALSE
        )
      }
      if (length(hits)) hits[[1]] else NA_integer_
    }, integer(1))
  }
  idx <- match_unique(key, filename_key, "filename")
  fallback <- is.na(idx)
  idx[fallback] <- match_unique(
    key[fallback], original_key, "original_filename"
  )

  if (strict && anyNA(idx)) {
    stop("No ingest metadata match for NABat recording(s): ",
         paste(df$associatedMedia[rows][is.na(idx)], collapse = ", "), call. = FALSE)
  }

  hit <- !is.na(idx)
  rows <- rows[hit]
  idx <- idx[hit]
  if (!length(rows)) return(df)

  value <- function(name, default = NA_character_) {
    if (name %in% names(metadata)) metadata[[name]][idx] else rep(default, length(idx))
  }
  plot_number <- blank_to_na(as.character(value("plot_number")))
  plot <- rep(NA_character_, length(plot_number))
  has_plot <- !is.na(plot_number)
  plot[has_plot] <- ifelse(
    grepl("^plot", plot_number[has_plot], ignore.case = TRUE),
    tolower(plot_number[has_plot]),
    paste0("plot", plot_number[has_plot])
  )

  matched_deployment_id <- as.character(value("deployment_id"))
  staged_deployment_id <- blank_to_na(df$deploymentID[rows])
  mismatch <- !is.na(staged_deployment_id) &
    staged_deployment_id != matched_deployment_id
  if (any(mismatch)) {
    stop(
      "NABat export folder does not match ingest metadata deployment_id for: ",
      paste(df$associatedMedia[rows][mismatch], collapse = ", "),
      call. = FALSE
    )
  }

  df$deploymentID[rows] <- matched_deployment_id
  df$organization[rows] <- as.character(value("organization"))
  df$site[rows] <- as.character(value("site_full_name"))
  df$site_short_name[rows] <- as.character(value("site"))
  df$site_code[rows] <- as.character(value("site_code"))
  df$plot[rows] <- plot
  df$decimalLatitude[rows] <- suppressWarnings(as.numeric(value("latitude")))
  df$decimalLongitude[rows] <- suppressWarnings(as.numeric(value("longitude")))
  df$sensor_make[rows] <- as.character(value("ARU_make"))
  df$sensor_model[rows] <- as.character(value("ARU_model"))
  df$date_installed[rows] <- as.Date(value("date_installed"))
  df$deployment_start_date[rows] <- as.Date(value("deployment_start_date"))
  df$deployment_end_date[rows] <- as.Date(value("deployment_end_date"))
  df
}

enrich_motus_site <- function(df, sites_path, motus_config_path, strict = TRUE) {
  sites <- readr::read_csv(sites_path, show_col_types = FALSE)
  names(sites) <- tolower(names(sites))
  site_required <- c("site_name", "site_short_name", "site_code")
  site_missing <- setdiff(site_required, names(sites))
  if (length(site_missing)) {
    stop("sites.csv is missing: ", paste(site_missing, collapse = ", "), call. = FALSE)
  }
  crosswalk <- readr::read_csv(motus_config_path, show_col_types = FALSE,
                               col_types = readr::cols(.default = readr::col_character()))
  required <- c("m_station_id", "site_short_name")
  missing <- setdiff(required, names(crosswalk))
  if (length(missing)) {
    stop("motus.csv is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(crosswalk$m_station_id)) {
    stop("motus.csv contains duplicate m_station_id values.", call. = FALSE)
  }

  rows <- which(df$platform == "motus")
  idx <- match(df$m_station_id[rows], crosswalk$m_station_id)
  if (strict && anyNA(idx)) {
    stop("motus.csv has no site mapping for Motus station ID(s): ",
         paste(unique(df$m_station_id[rows][is.na(idx)]), collapse = ", "), call. = FALSE)
  }

  hit <- !is.na(idx)
  rows <- rows[hit]
  idx <- idx[hit]
  if (!length(rows)) return(df)
  site_idx <- match(crosswalk$site_short_name[idx], sites$site_short_name)
  if (strict && anyNA(site_idx)) {
    stop("Motus crosswalk references unknown site_short_name value(s).", call. = FALSE)
  }

  df$site[rows] <- sites$site_name[site_idx]
  df$site_short_name[rows] <- sites$site_short_name[site_idx]
  df$site_code[rows] <- sites$site_code[site_idx]
  df$plot[rows] <- NA_character_
  df$deploymentID[rows] <- NA_character_
  df
}

# Fill sensor_make / sensor_model from the CASSN per-file device metadata under
# `field_data_root`: cameras from image_file_metadata.csv (camera_make/camera_model),
# ARUs from audio_file_metadata.csv (ARU_make/ARU_model). Make/model are constant per
# deployment, so we dedupe to a deployment_id -> make/model lookup and join on
# deploymentID. Motus (receiver-derived sensor, not in these files) is preserved.
# NOTE: for production this deployment dimension should be cached rather than rescanned.
enrich_sensor <- function(df, field_data_root) {
  read_sensor <- function(pattern, make_col, model_col) {
    files <- list.files(field_data_root, pattern = pattern, recursive = TRUE, full.names = TRUE)
    rows <- lapply(files, function(f) {
      d <- tryCatch(
        readr::read_csv(f, show_col_types = FALSE, progress = FALSE,
                        col_select = dplyr::any_of(c("deployment_id", make_col, model_col))),
        error = function(e) NULL)
      if (is.null(d) || !all(c("deployment_id", make_col, model_col) %in% names(d))) return(NULL)
      unique(data.frame(
        deployment_id = as.character(d[["deployment_id"]]),
        sensor_make = as.character(d[[make_col]]),
        sensor_model = as.character(d[[model_col]]),
        stringsAsFactors = FALSE))
    })
    do.call(rbind, rows)
  }

  lut <- rbind(
    read_sensor("^image_file_metadata\\.csv$", "camera_make", "camera_model"),
    read_sensor("^audio_file_metadata\\.csv$", "ARU_make", "ARU_model")
  )
  if (is.null(lut) || !nrow(lut)) return(df)
  lut <- lut[!is.na(lut$deployment_id) & nzchar(lut$deployment_id), ]
  lut <- lut[!duplicated(lut$deployment_id), ]

  idx <- match(as.character(df$deploymentID), lut$deployment_id)
  hit <- !is.na(idx)
  df$sensor_make[hit] <- lut$sensor_make[idx[hit]]
  df$sensor_model[hit] <- lut$sensor_model[idx[hit]]
  df
}
