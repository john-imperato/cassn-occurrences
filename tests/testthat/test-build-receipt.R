# The receipt exists to record what a later promotion step cannot recover.

test_that("the receipt records product identity and build parameters", {
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "cassn_occurrences.csv")
  occurrences <- data.frame(a = 1:3, b = letters[1:3])
  utils::write.csv(occurrences, csv, row.names = FALSE)

  receipt <- build_receipt(csv, occurrences, "UC", "America/Los_Angeles")

  expect_equal(receipt$receipt_version, 1L)
  expect_equal(receipt$package$name, "cassnoccurrences")
  expect_equal(receipt$parameters$organization, "UC")
  expect_equal(receipt$parameters$timezone, "America/Los_Angeles")
  expect_equal(receipt$product$file, "cassn_occurrences.csv")
  expect_equal(receipt$product$row_count, 3L)
  expect_equal(receipt$product$column_count, 2L)
  expect_equal(
    receipt$product$sha256,
    digest::digest(csv, algo = "sha256", file = TRUE)
  )
})

test_that("the product hash is of the file on disk, not the data frame", {
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "occurrences.csv")
  occurrences <- data.frame(a = 1)
  utils::write.csv(occurrences, csv, row.names = FALSE)
  before <- build_receipt(csv, occurrences, "UC", "UTC")$product$sha256

  cat("trailing change\n", file = csv, append = TRUE)
  after <- build_receipt(csv, occurrences, "UC", "UTC")$product$sha256

  expect_false(identical(before, after))
})

test_that("the receipt is named for its product", {
  expect_equal(
    build_receipt_path("/tmp/out/cassn_occurrences_1.csv"),
    "/tmp/out/cassn_occurrences_1_build_receipt.json"
  )
})

test_that("the receipt round-trips as valid JSON", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "occurrences.csv")
  utils::write.csv(data.frame(a = 1), csv, row.names = FALSE)

  receipt <- build_receipt(csv, data.frame(a = 1), "UC", "America/Los_Angeles")
  path <- write_build_receipt(receipt, build_receipt_path(csv))
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  expect_equal(parsed$receipt_version, 1L)
  expect_equal(parsed$package$name, "cassnoccurrences")
  expect_equal(parsed$product$file, "occurrences.csv")
  expect_equal(parsed$parameters$timezone, "America/Los_Angeles")
})

test_that("unknown git state is written as null rather than a guess", {
  receipt <- list(package = list(git_commit = NA_character_, git_clean = NA))
  rendered <- json_object(receipt)

  expect_match(rendered, '"git_commit": null', fixed = TRUE)
  expect_match(rendered, '"git_clean": null', fixed = TRUE)
})

test_that("installed GitHub revision is retained without guessing cleanliness", {
  state <- description_git_state(list(
    RemoteSha = "0123456789abcdef",
    RemoteRef = "main"
  ))

  expect_equal(state$commit, "0123456789abcdef")
  expect_equal(state$branch, "main")
  expect_true(is.na(state$clean))
})

test_that("JSON strings escape quotes and backslashes", {
  expect_equal(json_scalar('a "quoted" \\ path'), '"a \\"quoted\\" \\\\ path"')
  expect_equal(json_scalar(TRUE), "true")
  expect_equal(json_scalar(FALSE), "false")
  expect_equal(json_scalar(NA), "null")
})

test_that("git state on a source checkout reports a commit and cleanliness", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  repo <- withr::local_tempdir()
  system2("git", c("-C", shQuote(repo), "init", "-q"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", shQuote(repo), "config", "user.email", "t@example.com"))
  system2("git", c("-C", shQuote(repo), "config", "user.name", "Test"))
  writeLines("x", file.path(repo, "f.txt"))
  system2("git", c("-C", shQuote(repo), "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", shQuote(repo), "commit", "-qm", "init"),
          stdout = FALSE, stderr = FALSE)

  clean <- git_state(repo)
  expect_true(nzchar(clean$commit))
  expect_true(clean$clean)

  writeLines("y", file.path(repo, "f.txt"))
  expect_false(git_state(repo)$clean)
})

test_that("git state is unknown outside a work tree", {
  state <- git_state(withr::local_tempdir())
  expect_true(is.na(state$commit))
  expect_true(is.na(state$clean))
})
