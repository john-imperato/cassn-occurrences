test_that("text occurrence IDs are deterministic and cross-language stable", {
  key <- paste(
    "nabat",
    "CDFW_CentralPlains_plot1_BT_20250505",
    "117408_CPER1_20250501_232429",
    "Tadarida_brasiliensis",
    sep = ":"
  )

  expect_equal(
    occurrence_id_from_key(key),
    "CASSN-be7b4d2bad5ca550edaff85316615cc8"
  )
  expect_equal(occurrence_id_from_key(key), occurrence_id_from_key(key))
  expect_match(occurrence_id_from_key(key), "^CASSN-[0-9a-f]{32}$")
})

test_that("the readable occurrence key is retained beside the surrogate ID", {
  key <- make_occurrence_key(
    "nabat", "CDFW_CentralPlains_plot1_BT_20250505",
    "117408_CPER1_20250501_232429", "Tadarida brasiliensis"
  )

  expect_equal(
    key,
    paste(
      "nabat",
      "CDFW_CentralPlains_plot1_BT_20250505",
      "117408_CPER1_20250501_232429",
      "Tadarida_brasiliensis",
      sep = ":"
    )
  )
  expect_equal(make_occurrence_id(
    "nabat", "CDFW_CentralPlains_plot1_BT_20250505",
    "117408_CPER1_20250501_232429", "Tadarida brasiliensis"
  ), "CASSN-be7b4d2bad5ca550edaff85316615cc8")
})

test_that("occurrence identity validation rejects altered IDs and duplicate IDs", {
  keys <- c("nabat:file-one:Species_one", "nabat:file-two:Species_two")
  valid <- data.frame(
    occurrenceID = occurrence_id_from_key(keys),
    occurrenceKey = keys,
    stringsAsFactors = FALSE
  )
  expect_invisible(assert_unique_occurrence_ids(valid))

  altered <- valid
  altered$occurrenceID[[1]] <- "CASSN-00000000000000000000000000000001"
  expect_error(assert_unique_occurrence_ids(altered), "do not match")

  duplicate <- valid
  duplicate[2, ] <- duplicate[1, ]
  expect_error(assert_unique_occurrence_ids(duplicate), "Duplicate occurrenceID")
})
