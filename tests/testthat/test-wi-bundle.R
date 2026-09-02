wi_deployment_row <- function(deployment_id, ...) {
  row <- list(
    project_id = "2010527",
    deployment_id = deployment_id,
    placename = "TestPlace",
    longitude = "-122.15",
    latitude = "38.51",
    start_date = "2025-11-07 00:00:00",
    end_date = "2026-01-08 23:59:59",
    bait_type = "Scent",
    bait_description = "Gusto",
    feature_type = "Road dirt",
    camera_id = "2175928",
    camera_name = "8019434",
    camera_functioning = "Camera Functioning",
    subproject_name = "UC_TestSite_20260108"
  )
  utils::modifyList(row, list(...))
}

wi_image_row <- function(deployment_id, filename, ...) {
  row <- list(
    project_id = "2010527",
    deployment_id = deployment_id,
    image_id = paste0("img-", filename),
    filename = filename,
    is_blank = "0",
    identified_by = "Tester",
    class = "Mammalia",
    order = "Carnivora",
    family = "Canidae",
    genus = "Canis",
    species = "latrans",
    common_name = "Coyote",
    timestamp = "2025-11-08 04:39:20",
    number_of_objects = "1",
    cv_confidence = "0.9",
    uncertainty = "",
    bounding_boxes = "",
    highlighted = "false",
    behavior = "",
    individual_animal_notes = ""
  )
  utils::modifyList(row, list(...))
}

write_wi_bundle <- function(root, bundle_name, deployments, images,
                            project_type = "Image", images_parts = 1L) {
  bundle_dir <- file.path(root, bundle_name)
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)

  deployments_df <- dplyr::bind_rows(lapply(deployments, tibble::as_tibble))
  utils::write.csv(
    deployments_df, file.path(bundle_dir, "deployments.csv"), row.names = FALSE
  )

  images_df <- dplyr::bind_rows(lapply(images, tibble::as_tibble))
  parts <- split(
    seq_len(nrow(images_df)),
    rep(seq_len(images_parts), length.out = nrow(images_df))
  )
  for (i in seq_along(parts)) {
    utils::write.csv(
      images_df[parts[[i]], , drop = FALSE],
      file.path(bundle_dir, sprintf("images_2010527_%d.csv", i)),
      row.names = FALSE
    )
  }

  utils::write.csv(
    data.frame(project_id = "2010527", project_type = project_type),
    file.path(bundle_dir, "projects.csv"), row.names = FALSE
  )
  # The documentation PDFs ship inside every bundle and must be ignored.
  writeLines("not a csv", file.path(bundle_dir, "Data-Dictionary.pdf"))
  bundle_dir
}

test_that("a bundle publishes every deployment it contains", {
  root <- withr::local_tempdir()
  write_wi_bundle(
    root, "wildlife-insights_abc_project-2010527_data",
    deployments = list(
      wi_deployment_row("UC_TestSite_plot1_ML_20260108"),
      wi_deployment_row("UC_TestSite_plot2_ML_20260108"),
      wi_deployment_row("UC_TestSite_plot3_SA_20260108")
    ),
    images = list(
      wi_image_row("UC_TestSite_plot1_ML_20260108", "a.jpg"),
      wi_image_row("UC_TestSite_plot2_ML_20260108", "b.jpg"),
      wi_image_row("UC_TestSite_plot3_SA_20260108", "c.jpg")
    )
  )

  export <- read_wi_export(root)

  expect_equal(nrow(export$bundles), 1L)
  expect_equal(export$bundles$deployments, 3L)
  expect_setequal(
    export$deployment_summary$deployment_id,
    c("UC_TestSite_plot1_ML_20260108", "UC_TestSite_plot2_ML_20260108",
      "UC_TestSite_plot3_SA_20260108")
  )
  expect_true(all(export$deployment_summary$images == 1L))

  occurrences <- transform_wi_occurrences(
    images = export$images, deployments = export$deployments
  )
  expect_equal(nrow(occurrences), 3L)
  expect_setequal(
    occurrences$samplingProtocol, c("CASSN ML Camera", "CASSN SA Camera")
  )
})

test_that("a bundle folder name is not required to match a deployment", {
  root <- withr::local_tempdir()
  write_wi_bundle(
    root, "wildlife-insights_d1aae852_project-2010527_data",
    deployments = list(wi_deployment_row("UC_TestSite_plot1_ML_20260108")),
    images = list(wi_image_row("UC_TestSite_plot1_ML_20260108", "a.jpg"))
  )

  expect_no_error(read_wi_export(root))
})

test_that("split images CSVs are read as one export", {
  root <- withr::local_tempdir()
  images <- lapply(
    sprintf("img%02d.jpg", 1:6),
    function(name) wi_image_row("UC_TestSite_plot1_ML_20260108", name)
  )
  write_wi_bundle(
    root, "bundle",
    deployments = list(wi_deployment_row("UC_TestSite_plot1_ML_20260108")),
    images = images, images_parts = 3L
  )

  export <- read_wi_export(root)

  expect_equal(export$bundles$images_files, 3L)
  expect_equal(nrow(export$images), 6L)
})

test_that("images referencing an unknown deployment are rejected", {
  root <- withr::local_tempdir()
  write_wi_bundle(
    root, "bundle",
    deployments = list(wi_deployment_row("UC_TestSite_plot1_ML_20260108")),
    images = list(
      wi_image_row("UC_TestSite_plot1_ML_20260108", "a.jpg"),
      wi_image_row("UC_TestSite_plot9_ML_20260108", "b.jpg")
    )
  )

  expect_error(read_wi_export(root), "UC_TestSite_plot9_ML_20260108")
})

test_that("a deployment shared by two bundles is rejected", {
  root <- withr::local_tempdir()
  for (bundle in c("bundle_one", "bundle_two")) {
    write_wi_bundle(
      root, bundle,
      deployments = list(wi_deployment_row("UC_TestSite_plot1_ML_20260108")),
      images = list(wi_image_row("UC_TestSite_plot1_ML_20260108", "a.jpg"))
    )
  }

  expect_error(read_wi_export(root), "more than one Wildlife Insights bundle")
})

test_that("sequence-level projects are rejected", {
  root <- withr::local_tempdir()
  write_wi_bundle(
    root, "bundle",
    deployments = list(wi_deployment_row("UC_TestSite_plot1_ML_20260108")),
    images = list(wi_image_row("UC_TestSite_plot1_ML_20260108", "a.jpg")),
    project_type = "Sequence"
  )

  expect_error(read_wi_export(root), "image-level projects")
})

test_that("fuzzed coordinates are rejected", {
  root <- withr::local_tempdir()
  write_wi_bundle(
    root, "bundle",
    deployments = list(
      wi_deployment_row("UC_TestSite_plot1_ML_20260108", fuzzed = "True")
    ),
    images = list(wi_image_row("UC_TestSite_plot1_ML_20260108", "a.jpg"))
  )

  expect_error(read_wi_export(root), "fuzzed")
})

test_that("a still-zipped download reports how to stage it", {
  root <- withr::local_tempdir()
  writeLines("x", file.path(root, "wildlife-insights_abc_project-1_data.zip"))

  expect_error(read_wi_export(root), "still zipped")
})

test_that("missing required columns are named", {
  root <- withr::local_tempdir()
  bundle_dir <- write_wi_bundle(
    root, "bundle",
    deployments = list(wi_deployment_row("UC_TestSite_plot1_ML_20260108")),
    images = list(wi_image_row("UC_TestSite_plot1_ML_20260108", "a.jpg"))
  )
  deployments <- utils::read.csv(file.path(bundle_dir, "deployments.csv"))
  deployments$latitude <- NULL
  utils::write.csv(
    deployments, file.path(bundle_dir, "deployments.csv"), row.names = FALSE
  )

  expect_error(read_wi_export(root), "latitude")
})

test_that("optional columns absent from an export are filled, not fatal", {
  root <- withr::local_tempdir()
  bundle_dir <- write_wi_bundle(
    root, "bundle",
    deployments = list(wi_deployment_row("UC_TestSite_plot1_ML_20260108")),
    images = list(wi_image_row("UC_TestSite_plot1_ML_20260108", "a.jpg"))
  )
  images <- utils::read.csv(
    file.path(bundle_dir, "images_2010527_1.csv"), colClasses = "character"
  )
  images$number_of_objects <- NULL
  utils::write.csv(
    images, file.path(bundle_dir, "images_2010527_1.csv"), row.names = FALSE
  )

  export <- read_wi_export(root)
  occurrences <- transform_wi_occurrences(
    images = export$images, deployments = export$deployments
  )

  expect_equal(nrow(occurrences), 1L)
  expect_true(is.na(occurrences$individualCount))
})

test_that("deployment identities are read without parsing the images CSVs", {
  root <- withr::local_tempdir()
  write_wi_bundle(
    root, "bundle",
    deployments = list(
      wi_deployment_row("UC_TestSite_plot1_ML_20260108"),
      wi_deployment_row("UC_TestSite_plot2_ML_20260108")
    ),
    images = list(wi_image_row("UC_TestSite_plot1_ML_20260108", "a.jpg"))
  )

  expect_setequal(
    wi_staged_deployment_ids(root),
    c("UC_TestSite_plot1_ML_20260108", "UC_TestSite_plot2_ML_20260108")
  )
})
