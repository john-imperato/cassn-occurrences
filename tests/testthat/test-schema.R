test_that("schema conformance enforces the complete output contract", {
  schema <- cassn_occurrence_fields

  sample_value <- function(type) {
    switch(
      type,
      string = "sample",
      double = "1.5",
      integer = "2",
      logical = "TRUE",
      date = "2026-01-02",
      timestamp = "2026-01-02 03:04:05",
      timestamp_tz = "2026-01-02 03:04:05",
      stop("Unsupported occurrence field type in test: ", type)
    )
  }

  input <- tibble::as_tibble(stats::setNames(
    lapply(schema$type, sample_value),
    schema$name
  ))
  input$unexpected_field <- "remove me"

  conformed <- conform_occurrence_schema(input)

  expect_identical(names(conformed), schema$name)
  expect_false("unexpected_field" %in% names(conformed))

  expected_storage_type <- c(
    string = "character",
    double = "double",
    integer = "integer",
    logical = "logical",
    date = "double",
    timestamp = "double",
    timestamp_tz = "double"
  )
  for (i in seq_len(nrow(schema))) {
    expect_type(
      conformed[[schema$name[[i]]]],
      unname(expected_storage_type[[schema$type[[i]]]])
    )
  }

  expect_s3_class(conformed$date_installed, "Date")
  expect_s3_class(conformed$utc_timestamp, "POSIXct")
  expect_identical(attr(conformed$utc_timestamp, "tzone"), "UTC")
})

test_that("schema conformance adds every missing field as a typed blank", {
  schema <- cassn_occurrence_fields
  conformed <- conform_occurrence_schema(tibble::tibble(
    occurrenceID = c("one", "two"),
    unexpected_field = c("a", "b")
  ))

  expect_identical(names(conformed), schema$name)
  expect_equal(nrow(conformed), 2L)

  added_fields <- setdiff(schema$name, "occurrenceID")
  for (field in added_fields) {
    expect_true(all(is.na(conformed[[field]])))
  }
})

test_that("the installed Excel dictionary matches the R schema contract", {
  skip_if_not_installed("readxl")

  dictionary_path <- system.file(
    "documentation",
    "CASSN_Metadata_Mapping.xlsx",
    package = "cassnoccurrences"
  )
  expect_true(nzchar(dictionary_path))

  dictionary <- readxl::read_excel(
    dictionary_path,
    sheet = "Data Dictionary",
    skip = 2
  )
  dictionary <- dictionary[!is.na(dictionary[["Output field"]]), ]

  dictionary_contract <- tibble::tibble(
    name = trimws(dictionary[["Output field"]]),
    type = tolower(trimws(dictionary[["Type"]])),
    required = tolower(trimws(dictionary[["Required"]])) == "yes"
  )

  expect_equal(dictionary_contract, cassn_occurrence_fields)
})
