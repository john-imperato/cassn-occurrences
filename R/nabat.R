read_nabat_export <- function(dir) {
  paths <- list.files(
    dir, pattern = "^nabat_export\\.csv$", recursive = TRUE, full.names = TRUE
  )
  if (!length(paths)) {
    stop("No nabat_export.csv found under ", dir, call. = FALSE)
  }

  exports <- lapply(paths, function(path) {
    export_dir <- dirname(path)
    if (normalizePath(export_dir, mustWork = TRUE) ==
        normalizePath(dir, mustWork = TRUE)) {
      stop(
        "Place each nabat_export.csv in a deployment-ID subfolder under ", dir,
        call. = FALSE
      )
    }
    data <- utils::read.csv(
      path, check.names = FALSE, stringsAsFactors = FALSE,
      colClasses = "character"
    )
    data$.staged_deployment_id <- basename(export_dir)
    data
  })

  dplyr::bind_rows(exports)
}

transform_nabat_occurrences <- function(path = NULL, data = NULL,
                                        species_lookup = NULL,
                                        prefer_manual_id = TRUE,
                                        organization = "UC",
                                        timezone = "America/Los_Angeles") {
  if (!is.null(path)) {
    data <- read_nabat_export(path)
  }
  if (is.null(data)) {
    stop("Provide either path or data.", call. = FALSE)
  }
  if (!".staged_deployment_id" %in% names(data)) {
    stop("NABat data is missing its staged deployment-folder identity.", call. = FALSE)
  }

  survey_start <- lubridate::ymd_hms(data[["Survey Start Time"]], tz = timezone, quiet = TRUE)
  survey_end <- lubridate::ymd_hms(data[["Survey End Time"]], tz = timezone, quiet = TRUE)
  event_date <- lubridate::ymd_hms(data[["Audio Recording Time"]], tz = timezone, quiet = TRUE)

  auto_code <- blank_to_na(data[["Auto Id"]])
  manual_code <- blank_to_na(data[["Manual Id"]])
  # Manual (human-vetted) ID is authoritative; fall back to Auto only where a
  # human did not review the recording.
  species_code <- if (prefer_manual_id) dplyr::coalesce(manual_code, auto_code) else auto_code
  species <- resolve_nabat_species(species_code, species_lookup)

  occurrence_key <- make_occurrence_key(
    "nabat",
    data$.staged_deployment_id,
    occurrence_id_from_filename(data[["Audio Recording Name"]]),
    species$scientificName
  )

  out <- tibble::tibble(
    occurrenceID = occurrence_id_from_key(occurrence_key),
    occurrenceKey = occurrence_key,
    platform = "nabat",
    basisOfRecord = "MachineObservation",
    organization = organization,
    eventDate = format_event_date(event_date, timezone),
    eventTimeZone = timezone,
    utc_timestamp = to_utc_timestamp(event_date),
    scientificName = species$scientificName,
    vernacularName = species$vernacularName,
    taxonRank = taxon_rank_from_name(species$scientificName),
    decimalLatitude = suppressWarnings(as.numeric(data[["Latitude"]])),
    decimalLongitude = suppressWarnings(as.numeric(data[["Longitude"]])),
    geodeticDatum = "WGS84",
    individualCount = NA_integer_,
    samplingProtocol = "CASSN BT ARU",
    deploymentID = blank_to_na(data$.staged_deployment_id),
    genus = genus_from_scientific_name(species$scientificName),
    species = species_from_scientific_name(species$scientificName),
    associatedMedia = blank_to_na(data[["Audio Recording Name"]]),
    sensor_model = blank_to_na(data[["Detector Model"]]),
    deployment_start_date = as.Date(survey_start),
    deployment_end_date = as.Date(survey_end),
    nabat_grts_cell_id = as.character(data[["| GRTS Cell Id"]]),
    nabat_auto_species_code = auto_code,
    nabat_manual_species_code = manual_code,
    nabat_auto_species_list = blank_to_na(data[["Name of Species List for Auto Id"]]),
    nabat_manual_species_list = blank_to_na(data[["Name of Species List for Manual Id"]]),
    nabat_grid_cell_quadrant = blank_to_na(data[["Grid Cell Quadrant"]]),
    nabat_microphone_type = blank_to_na(data[["Microphone Model"]]),
    nabat_mic_serial_number = blank_to_na(data[["Microphone Serial Number"]]),
    nabat_water_type = blank_to_na(data[["Water Type"]]),
    nabat_water_distance = suppressWarnings(as.numeric(data[["Distance to Nearest Water (m)"]])),
    nabat_usnvc_habitat_code = blank_to_na(data[["USNVC Habitat Code"]]),
    nabat_land_unit_code = blank_to_na(data[["Land Unit Code"]]),
    nabat_auto_id_software = blank_to_na(data[["Auto Id Software"]]),
    nabat_unusual_occurrences = blank_to_na(data[["Unusual Occurrences"]]),
    nabat_broad_habitat_type = blank_to_na(data[["Broad Habitat Type"]])
  )

  out <- out |>
    dplyr::filter(presence_filter(.data$scientificName))

  conform_occurrence_schema(out)
}

# Resolve a NABat 4-/6-letter code to a binomial via the authoritative reference.
# Only single-species codes resolve; groupings/frequency/non-ID codes return NA
# (they are not one species) and are dropped by the caller for now. No guessed
# fallback: an authoritative reference is required.
resolve_nabat_species <- function(code, species_lookup = NULL) {
  if (is.null(species_lookup)) species_lookup <- load_nabat_species_codes()
  lk <- species_lookup
  names(lk) <- tolower(names(lk))
  if (!"scientific_name" %in% names(lk) && "scientificname" %in% names(lk)) {
    lk$scientific_name <- lk$scientificname
  }
  if (!"common_name" %in% names(lk) && "commonname" %in% names(lk)) {
    lk$common_name <- lk$commonname
  }
  if (!"category" %in% names(lk)) lk$category <- "species"
  lk$code <- tolower(as.character(lk$code))

  key <- tolower(blank_to_na(code))
  idx <- match(key, lk$code)
  is_species <- !is.na(idx) & tolower(lk$category[idx]) == "species"

  tibble::tibble(
    scientificName = blank_to_na(ifelse(is_species, lk$scientific_name[idx], NA_character_)),
    vernacularName = blank_to_na(ifelse(is_species, lk$common_name[idx], NA_character_))
  )
}

# Rebuild the pinned NABat species-code reference from NABat's official code
# workbook (the .xlsx published on the Partner Portal). This is the authoritative
# source; refreshing = download the latest workbook and re-run this. Writes
# out_path for you to review (`git diff`) and commit. No network, no auth.
build_nabat_species_codes <- function(xlsx_path,
                                      out_path = file.path("inst", "extdata", "nabat_species_codes.csv")) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("readxl is required to build the NABat species-code reference.", call. = FALSE)
  }
  need <- c("Species Codes", "Couplets and Groupings", "Frequency Classes", "Non-ID Codes")
  missing <- setdiff(need, readxl::excel_sheets(xlsx_path))
  if (length(missing)) {
    stop("Workbook is missing expected sheet(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  norm <- function(x) {
    x <- trimws(gsub("[[:space:]]+", " ", as.character(x)))
    x[x %in% c("NA", "")] <- NA_character_
    x
  }
  make <- function(code, category, scientific_name, common_name, definition) {
    data.frame(code = norm(code), category = category,
               scientific_name = norm(scientific_name), common_name = norm(common_name),
               definition = norm(definition), stringsAsFactors = FALSE)
  }

  sp <- readxl::read_excel(xlsx_path, sheet = "Species Codes")
  species <- rbind(
    make(sp[["Four-letter species code"]], "species", sp[["Scientific name"]], sp[["Common name"]], NA),
    make(sp[["Six-letter species code"]],  "species", sp[["Scientific name"]], sp[["Common name"]], NA)
  )

  gp <- readxl::read_excel(xlsx_path, sheet = "Couplets and Groupings")
  grouping <- make(gp[["Code"]], "grouping", gp[["Scientific names"]], gp[["Common name"]], NA)

  fq <- readxl::read_excel(xlsx_path, sheet = "Frequency Classes")
  frequency <- make(fq[["Code"]], "frequency", NA, NA, fq[["Definition"]])

  ni <- readxl::read_excel(xlsx_path, sheet = "Non-ID Codes")
  ni_codes <- strsplit(as.character(ni[["Code"]]), "\\s+or\\s+")  # e.g. "NOTBAT or NOBAT"
  non_id <- do.call(rbind, Map(function(codes, defn) {
    make(codes, "non_id", NA, NA, rep(defn, length(codes)))
  }, ni_codes, ni[["Definition"]]))

  out <- rbind(species, grouping, frequency, non_id)
  out <- out[!is.na(out$code) & nzchar(out$code), ]

  dir <- dirname(out_path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  readr::write_csv(out, out_path)
  message(sprintf("Built NABat reference: %d rows -> %s", nrow(out), out_path))
  invisible(out)
}
