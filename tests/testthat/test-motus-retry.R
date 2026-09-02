test_that("a receiver that succeeds first time is not retried", {
  calls <- 0L
  result <- retry_receiver_download(
    function() {
      calls <<- calls + 1L
      "database"
    },
    attempts = 3L, label = "CTT-TEST"
  )

  expect_equal(result, "database")
  expect_equal(calls, 1L)
})

test_that("a transient timeout is retried and the later attempt is kept", {
  calls <- 0L
  result <- NULL
  messages <- testthat::capture_messages(
    result <- retry_receiver_download(
      function() {
        calls <<- calls + 1L
        if (calls < 3L) stop("The server did not respond within 120s.")
        "database"
      },
      attempts = 3L, label = "CTT-TEST"
    )
  )

  expect_equal(result, "database")
  expect_equal(calls, 3L)
  expect_match(messages, "attempt 1 of 3 failed", all = FALSE)
  expect_match(messages, "attempt 2 of 3 failed", all = FALSE)
  expect_match(messages, "batches already downloaded are kept", all = FALSE)
})

test_that("exhausted attempts return the error rather than raising it", {
  calls <- 0L
  result <- suppressMessages(retry_receiver_download(
    function() {
      calls <<- calls + 1L
      stop("The server did not respond within 120s.")
    },
    attempts = 2L, label = "CTT-TEST"
  ))

  # Returned, not thrown, so the caller can carry on to the next receiver.
  expect_s3_class(result, "error")
  expect_match(conditionMessage(result), "did not respond")
  expect_equal(calls, 2L)
})

test_that("a single attempt makes exactly one call", {
  calls <- 0L
  suppressMessages(retry_receiver_download(
    function() {
      calls <<- calls + 1L
      stop("nope")
    },
    attempts = 1L, label = "CTT-TEST"
  ))

  expect_equal(calls, 1L)
})

test_that("every receiver is attempted and the run still fails", {
  skip_if_not_installed("motus")

  staged_dir <- withr::local_tempdir()
  dir.create(file.path(staged_dir, "metadata_inputs"), recursive = TRUE)
  utils::write.csv(
    data.frame(
      m_station_id = c("16144", "16143"),
      receiver_serial = c("CTT-ONE", "CTT-TWO"),
      site_short_name = c("SNARL", "WhiteMtn")
    ),
    file.path(staged_dir, "metadata_inputs", "motus.csv"), row.names = FALSE
  )

  attempted <- character(0)
  testthat::local_mocked_bindings(
    tagme = function(projRecv, ...) {
      attempted <<- c(attempted, projRecv)
      stop("The server did not respond within 120s.")
    },
    srvTimeout = function(...) invisible(NULL),
    .package = "motus"
  )

  expect_error(
    suppressMessages(
      sync_motus_receivers(staged_dir, timeout = 600, attempts = 2L)
    ),
    "failed for 2 receiver\\(s\\)"
  )
  # Two receivers, two attempts each: the first failure must not skip the second
  # receiver.
  expect_equal(attempted, c("CTT-ONE", "CTT-ONE", "CTT-TWO", "CTT-TWO"))
})

test_that("the Motus request timeout is restored after the call", {
  skip_if_not_installed("motus")

  staged_dir <- withr::local_tempdir()
  dir.create(file.path(staged_dir, "metadata_inputs"), recursive = TRUE)
  utils::write.csv(
    data.frame(
      m_station_id = "16144", receiver_serial = "CTT-ONE",
      site_short_name = "SNARL"
    ),
    file.path(staged_dir, "metadata_inputs", "motus.csv"), row.names = FALSE
  )

  withr::local_options(motus.timeout = 120)
  applied <- NULL
  testthat::local_mocked_bindings(
    tagme = function(...) stop("boom"),
    srvTimeout = function(timeout, ...) {
      applied <<- timeout
      options(motus.timeout = timeout)
    },
    .package = "motus"
  )

  expect_error(
    suppressMessages(sync_motus_receivers(staged_dir, timeout = 900, attempts = 1L))
  )
  expect_equal(applied, 900)
  expect_equal(getOption("motus.timeout"), 120)
})
