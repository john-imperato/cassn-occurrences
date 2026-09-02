# Deployment events are resolved from CASSN metadata, never parsed out of a
# deployment ID: a deployment's date token is its own end date, not its event's.

make_event <- function(root, event, deployment_ids,
                       file = "audio_file_metadata.csv",
                       year = "2026", reserve = "Quail Ridge Reserve") {
  event_dir <- file.path(root, year, reserve, event)
  dir.create(event_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(deployment_id = deployment_ids, filename = "x.wav"),
    file.path(event_dir, file), row.names = FALSE
  )
  # Media lives beside the metadata and must never be walked into.
  dir.create(file.path(event_dir, "raw_data", "p1_BD"), recursive = TRUE,
             showWarnings = FALSE)
  event_dir
}

make_staged <- function(deployment_ids) {
  staged <- withr::local_tempdir(.local_envir = parent.frame())
  for (id in deployment_ids) {
    dir.create(file.path(staged, "nabat", id), recursive = TRUE,
               showWarnings = FALSE)
  }
  staged
}

test_that("a deployment resolves to the event whose metadata names it", {
  root <- withr::local_tempdir()
  event_dir <- make_event(root, "UC_QuailRidge_20260108",
                          "UC_QuailRidge_plot1_BT_20260108")
  staged <- make_staged("UC_QuailRidge_plot1_BT_20260108")

  expect_equal(
    suppressMessages(derive_deployment_dirs(staged, root)),
    event_dir
  )
})

test_that("a deployment ending before its event still resolves", {
  # The device came out on 2026-01-06; the event closed 2026-01-08. Parsing the
  # ID would look for a nonexistent UC_QuailRidge_20260106 folder.
  root <- withr::local_tempdir()
  event_dir <- make_event(root, "UC_QuailRidge_20260108",
                          "UC_QuailRidge_plot1_BT_20260106")
  staged <- make_staged("UC_QuailRidge_plot1_BT_20260106")

  expect_equal(
    suppressMessages(derive_deployment_dirs(staged, root)),
    event_dir
  )
})

test_that("a -seqNN deployment resolves as an opaque identifier", {
  root <- withr::local_tempdir()
  event_dir <- make_event(root, "UC_QuailRidge_20260108",
                          "UC_QuailRidge_plot1_BT_20260108-seq01")
  staged <- make_staged("UC_QuailRidge_plot1_BT_20260108-seq01")

  expect_equal(
    suppressMessages(derive_deployment_dirs(staged, root)),
    event_dir
  )
})

test_that("several deployments in one event yield one directory", {
  root <- withr::local_tempdir()
  ids <- c(
    "UC_QuailRidge_plot1_BT_20260108",
    "UC_QuailRidge_plot2_BT_20260108",
    "UC_QuailRidge_plot3_BT_20260106"
  )
  event_dir <- make_event(root, "UC_QuailRidge_20260108", ids)
  staged <- make_staged(ids)

  expect_equal(suppressMessages(derive_deployment_dirs(staged, root)), event_dir)
})

test_that("audio and image metadata in one event do not duplicate it", {
  root <- withr::local_tempdir()
  event_dir <- make_event(root, "UC_QuailRidge_20260108",
                          "UC_QuailRidge_plot1_BT_20260108")
  utils::write.csv(
    data.frame(deployment_id = "UC_QuailRidge_plot1_ML_20260108", filename = "x.jpg"),
    file.path(event_dir, "image_file_metadata.csv"), row.names = FALSE
  )
  staged <- make_staged(c("UC_QuailRidge_plot1_BT_20260108",
                          "UC_QuailRidge_plot1_ML_20260108"))

  expect_equal(suppressMessages(derive_deployment_dirs(staged, root)), event_dir)
})

test_that("an unnamed deployment fails with the deployment ID", {
  root <- withr::local_tempdir()
  make_event(root, "UC_QuailRidge_20260108", "UC_QuailRidge_plot1_BT_20260108")
  staged <- make_staged("UC_QuailRidge_plot9_BT_20260108")

  expect_error(
    derive_deployment_dirs(staged, root),
    "UC_QuailRidge_plot9_BT_20260108"
  )
})

test_that("a deployment named by two events is ambiguous, not silently picked", {
  root <- withr::local_tempdir()
  make_event(root, "UC_QuailRidge_20260504", "UC_QuailRidge_plot1_BT_20260504")
  make_event(root, "UC_QuailRidge_20260618", "UC_QuailRidge_plot1_BT_20260504")
  staged <- make_staged("UC_QuailRidge_plot1_BT_20260504")

  expect_error(
    derive_deployment_dirs(staged, root),
    "more than one deployment event folder"
  )
})

test_that("metadata without a deployment_id column fails clearly", {
  root <- withr::local_tempdir()
  event_dir <- file.path(root, "2026", "Quail Ridge Reserve", "UC_QuailRidge_20260108")
  dir.create(event_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(filename = "x.wav"),
    file.path(event_dir, "audio_file_metadata.csv"), row.names = FALSE
  )
  staged <- make_staged("UC_QuailRidge_plot1_BT_20260108")

  expect_error(derive_deployment_dirs(staged, root), "no deployment_id column")
})

test_that("a root with no CASSN metadata fails before resolving anything", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "2026", "Quail Ridge Reserve"), recursive = TRUE)
  staged <- make_staged("UC_QuailRidge_plot1_BT_20260108")

  expect_error(derive_deployment_dirs(staged, root), "No CASSN metadata files")
})

test_that("the metadata search does not descend into raw_data", {
  root <- withr::local_tempdir()
  event_dir <- make_event(root, "UC_QuailRidge_20260108",
                          "UC_QuailRidge_plot1_BT_20260108")
  # A stray metadata file under the media must not register as an event.
  utils::write.csv(
    data.frame(deployment_id = "UC_QuailRidge_plot1_BT_20260108"),
    file.path(event_dir, "raw_data", "p1_BD", "audio_file_metadata.csv"),
    row.names = FALSE
  )

  expect_equal(find_event_metadata_files(root),
               file.path(event_dir, "audio_file_metadata.csv"))
})
