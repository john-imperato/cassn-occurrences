make_nabat_enrichment_input <- function() {
  conform_occurrence_schema(tibble::tibble(
    occurrenceID = "CASSN-00000000000000000000000000000000",
    occurrenceKey = "nabat:test",
    platform = "nabat",
    associatedMedia = "SOURCE.WAV",
    deploymentID = "UC_ExampleDavisCampus_plot99_BT_99991231"
  ))
}

write_audio_metadata_fixture <- function(path, canonical = TRUE, legacy = FALSE,
                                         conflict = FALSE) {
  metadata <- tibble::tibble(
    filename = "UC_ExampleDavisCampus_plot99_BT_99991231_00001.wav",
    original_filename = "SOURCE.WAV",
    deployment_id = "UC_ExampleDavisCampus_plot99_BT_99991231",
    organization = "UC",
    site_code = "EXUCD",
    plot_number = "99",
    latitude = "38.533539",
    longitude = "-121.747565",
    ARU_make = "Open Acoustic Devices",
    ARU_model = "AudioMoth-Firmware-Basic 1.12.1",
    date_installed = "9999-12-27",
    deployment_start_date = "9999-12-27",
    deployment_end_date = "9999-12-31"
  )
  if (canonical) {
    metadata$site_name <- "EXAMPLE UC Davis Campus"
    metadata$site_short_name <- "ExampleDavisCampus"
  }
  if (legacy) {
    metadata$site_full_name <- if (conflict) "Different Site" else "EXAMPLE UC Davis Campus"
    metadata$site <- "ExampleDavisCampus"
  }
  readr::write_csv(metadata, path)
}

test_that("NABat enrichment accepts canonical v4 site fields", {
  root <- tempfile("canonical-audio-metadata-")
  dir.create(root)
  write_audio_metadata_fixture(file.path(root, "audio_file_metadata.csv"))

  enriched <- enrich_nabat_from_audio_metadata(make_nabat_enrichment_input(), root)

  expect_identical(enriched$site, "EXAMPLE UC Davis Campus")
  expect_identical(enriched$site_short_name, "ExampleDavisCampus")
  expect_identical(enriched$site_code, "EXUCD")
  expect_identical(enriched$plot, "plot99")
  expect_equal(enriched$decimalLatitude, 38.533539)
  expect_equal(enriched$decimalLongitude, -121.747565)
})

test_that("NABat enrichment remains compatible with legacy site fields", {
  root <- tempfile("legacy-audio-metadata-")
  dir.create(root)
  write_audio_metadata_fixture(
    file.path(root, "audio_file_metadata.csv"), canonical = FALSE, legacy = TRUE
  )

  enriched <- enrich_nabat_from_audio_metadata(make_nabat_enrichment_input(), root)

  expect_identical(enriched$site, "EXAMPLE UC Davis Campus")
  expect_identical(enriched$site_short_name, "ExampleDavisCampus")
})

test_that("NABat enrichment rejects conflicting site field generations", {
  root <- tempfile("conflicting-audio-metadata-")
  dir.create(root)
  write_audio_metadata_fixture(
    file.path(root, "audio_file_metadata.csv"), legacy = TRUE, conflict = TRUE
  )

  expect_error(
    enrich_nabat_from_audio_metadata(make_nabat_enrichment_input(), root),
    "conflicting canonical and legacy site name"
  )
})
