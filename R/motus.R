#' Synchronize all staged CASSN Motus receiver databases
#'
#' Read receiver serials from the staged CASSN Motus configuration and create
#' or update one receiver-scoped `.motus` SQLite database per configured
#' receiver. The databases are written to the staged `motus/` folder for use by
#' [write_occurrence_csv()].
#'
#' @param staged_dir Path to a staged CASSN folder. It must contain
#'   `metadata_inputs/motus.csv` with `m_station_id`, `receiver_serial`, and
#'   `site_short_name` columns.
#' @param update If `TRUE`, download and merge data available from Motus. If
#'   `FALSE`, only inspect databases that already exist in the staged folder.
#'
#' @return A synchronization summary, invisibly, with one row per receiver.
#'
#' @details Motus authentication is handled by the `motus` package and may
#'   prompt on the first network call in an R session. This function downloads
#'   the detections and refreshes the tag, species, and receiver metadata used
#'   by the occurrence pipeline. It skips Motus activity, node, and
#'   deprecated-batch data, which the pipeline does not use. Existing databases
#'   are updated in place; missing databases are created when `update = TRUE`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' summary <- sync_motus_receivers("/path/to/staged_dir")
#' }
sync_motus_receivers <- function(staged_dir, update = TRUE) {
  if (!requireNamespace("motus", quietly = TRUE)) {
    stop(
      "The 'motus' package is required for sync_motus_receivers(). ",
      "Install it with:\n",
      "install.packages(\"motus\", repos = c(",
      "birdscanada = \"https://birdscanada.r-universe.dev\", ",
      "CRAN = \"https://cloud.r-project.org\"))",
      call. = FALSE
    )
  }

  staged_dir <- normalizePath(staged_dir, mustWork = TRUE)
  config_path <- file.path(staged_dir, "metadata_inputs", "motus.csv")
  if (!file.exists(config_path)) {
    stop("Missing staged Motus configuration: ", config_path, call. = FALSE)
  }

  config <- readr::read_csv(
    config_path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  required <- c("m_station_id", "receiver_serial", "site_short_name")
  missing <- setdiff(required, names(config))
  if (length(missing)) {
    stop("motus.csv is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  for (name in required) {
    config[[name]] <- trimws(config[[name]])
  }
  blank <- !nzchar(config$m_station_id) |
    !nzchar(config$receiver_serial) |
    !nzchar(config$site_short_name)
  if (any(blank)) {
    stop("motus.csv contains a blank required value.", call. = FALSE)
  }
  if (anyDuplicated(config$receiver_serial)) {
    stop("motus.csv contains duplicate receiver_serial values.", call. = FALSE)
  }

  motus_dir <- file.path(staged_dir, "motus")
  dir.create(motus_dir, recursive = TRUE, showWarnings = FALSE)
  result <- vector("list", nrow(config))

  for (i in seq_len(nrow(config))) {
    serial <- config$receiver_serial[[i]]
    snapshot <- file.path(motus_dir, paste0(serial, ".motus"))
    is_new <- !file.exists(snapshot)
    if (is_new && !isTRUE(update)) {
      stop(
        "No staged database for receiver ", serial,
        "; rerun with update = TRUE to download it.",
        call. = FALSE
      )
    }

    message(if (is_new) "Downloading Motus receiver: " else "Updating Motus receiver: ", serial)
    sql <- motus::tagme(
      projRecv = serial,
      update = update,
      new = is_new,
      dir = motus_dir,
      forceMeta = TRUE,
      skipActivity = TRUE,
      skipNodes = TRUE,
      skipDeprecated = TRUE
    )
    run_count <- tryCatch(
      DBI::dbGetQuery(sql, "select count(*) as n from allruns")$n[[1]],
      finally = if (DBI::dbIsValid(sql)) DBI::dbDisconnect(sql)
    )
    if (!file.exists(snapshot)) {
      stop("Motus did not create the expected database: ", snapshot, call. = FALSE)
    }

    result[[i]] <- data.frame(
      m_station_id = config$m_station_id[[i]],
      receiver_serial = serial,
      site_short_name = config$site_short_name[[i]],
      status = if (is_new) "downloaded" else if (isTRUE(update)) "updated" else "inspected",
      runs = as.integer(run_count),
      snapshot = normalizePath(snapshot, mustWork = TRUE),
      stringsAsFactors = FALSE
    )
  }

  summary <- dplyr::bind_rows(result)
  message(paste(utils::capture.output(print(summary, row.names = FALSE)), collapse = "\n"))
  invisible(summary)
}

# Manufacturer of the CASSN receiver doing the detecting, keyed on Motus
# `receiverType`. The receiver is CASSN-owned hardware, so it populates the
# canonical sensor_* fields (the foreign tag never does). Extend as CASSN
# deploys other receiver types.
motus_receiver_make <- function(receiver_type) {
  dplyr::case_when(
    toupper(receiver_type) == "SENSORSTATION" ~ "Cellular Tracking Technologies",
    TRUE ~ NA_character_
  )
}

transform_motus_occurrences <- function(path = NULL, sql = NULL,
                                        cutoff = NULL,
                                        organization = "UC",
                                        timezone = "America/Los_Angeles") {
  con <- NULL
  close_con <- FALSE
  if (!is.null(sql)) {
    con <- sql
  } else if (!is.null(path)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    close_con <- TRUE
  } else {
    stop("Provide either path or sql.", call. = FALSE)
  }
  on.exit(if (close_con) DBI::dbDisconnect(con), add = TRUE)

  runs <- dplyr::tbl(con, "allruns")
  if (!is.null(cutoff)) {
    cutoff_unix <- as.numeric(as.POSIXct(cutoff, tz = "UTC"))
    runs <- runs |> dplyr::filter(.data$tsBeginCorrected >= cutoff_unix)
  }

  raw <- runs |>
    dplyr::filter(.data$motusFilter == 1) |>
    dplyr::collect()
  raw <- raw[!presence_filter(raw$ambigID), , drop = FALSE]

  # receiverType lives in recvDeps, not the allruns view; join it on for the
  # sensor_* (CASSN receiver) fields.
  recv_meta <- dplyr::tbl(con, "recvDeps") |>
    dplyr::select("deployID", "stationID", "receiverType") |>
    dplyr::collect()
  receiver_idx <- match(raw$recvDeployID, recv_meta$deployID)
  receiver_type <- recv_meta$receiverType[receiver_idx]
  station_id <- recv_meta$stationID[receiver_idx]

  event_date <- as.POSIXct(raw$tsBeginCorrected, origin = "1970-01-01", tz = "UTC")
  # colon-free UTC stamp so it can't collide with the ':' id delimiter
  run_start_id <- format(event_date, "%Y%m%dT%H%M%SZ", tz = "UTC")
  occurrence_key <- make_occurrence_key(
    "motus", raw$tagDeployID, raw$recvDeployID, run_start_id
  )

  out <- tibble::tibble(
    # runID is unstable across Motus reprocessing; mint from stable physical facts
    occurrenceID = occurrence_id_from_key(occurrence_key),
    occurrenceKey = occurrence_key,
    platform = "motus",
    basisOfRecord = "MachineObservation",
    organization = organization,
    eventDate = format_event_date(event_date, timezone),
    eventTimeZone = timezone,
    utc_timestamp = to_utc_timestamp(event_date),
    scientificName = blank_to_na(raw$speciesSci),
    vernacularName = blank_to_na(raw$speciesEN),
    taxonRank = taxon_rank_from_name(raw$speciesSci),
    decimalLatitude = suppressWarnings(as.numeric(raw$recvDeployLat)),
    decimalLongitude = suppressWarnings(as.numeric(raw$recvDeployLon)),
    geodeticDatum = "WGS84",
    individualCount = 1L,  # one tag detection run = one individual animal
    organismID = as.character(raw$tagDeployID),
    samplingProtocol = "CASSN Motus",
    deploymentID = NA_character_,
    class = dplyr::case_when(
      raw$speciesGroup == "BIRDS" ~ "Aves",
      raw$speciesGroup == "MAMMALS" ~ "Mammalia",
      TRUE ~ NA_character_
    ),
    genus = genus_from_scientific_name(raw$speciesSci),
    species = species_from_scientific_name(raw$speciesSci),
    sensor_make = motus_receiver_make(receiver_type),
    sensor_model = blank_to_na(receiver_type),
    sensor_firmware = NA_character_,
    m_run_id = as.character(raw$runID),
    m_motus_tag_id = as.character(raw$motusTagID),
    m_tag_deploy_id = as.character(raw$tagDeployID),
    m_species_id = as.character(raw$speciesID),
    m_full_id = as.character(raw$fullID),
    m_ambig_id = as.character(raw$ambigID),
    m_tag_project = as.character(raw$tagProjName),
    m_tag_manufacturer = blank_to_na(raw$mfg),
    m_tag_model = blank_to_na(raw$tagModel),
    m_nomFreq = suppressWarnings(as.numeric(raw$nomFreq)),
    m_species_group = blank_to_na(raw$speciesGroup),
    m_run_length = suppressWarnings(as.numeric(raw$runLen)),
    m_port = as.character(raw$port),
    m_antenna_type = blank_to_na(raw$antType),
    m_antenna_bearing = suppressWarnings(as.numeric(raw$antBearing)),
    m_receiver_name = blank_to_na(first_nonmissing(raw$recvDeployName, raw$recv)),
    m_receiver_serial = blank_to_na(raw$recv),
    m_station_id = as.character(station_id),
    m_receiver_type = blank_to_na(receiver_type),
    m_recv_deploy_id = as.character(raw$recvDeployID)
  )

  out <- out |>
    dplyr::filter(presence_filter(.data$scientificName))

  conform_occurrence_schema(out)
}
