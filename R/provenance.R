# Build provenance for a published occurrence release.
#
# Package version, git state, R version, build time, and who ran the build stop
# existing the moment a build finishes. A promotion script run later can only
# record what is true on the day it runs, and would report that as though it
# were true at build time. So the facts that cannot be recomputed are written
# beside the product while the build still knows them; everything else --
# coverage dates, row counts, input identities -- stays recomputable from the
# CSV and the staged folder.

BUILD_RECEIPT_VERSION <- 1L

# JSON is written by hand rather than adding a dependency: a receipt is a flat
# object of scalars, and the package's Imports are deliberately lean.
json_scalar <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return("null")
  if (is.logical(x)) return(if (x) "true" else "false")
  if (is.numeric(x)) return(format(x, scientific = FALSE, trim = TRUE))
  escaped <- gsub("\\\\", "\\\\\\\\", as.character(x))
  escaped <- gsub('"', '\\\\"', escaped)
  escaped <- gsub("\n", "\\\\n", escaped)
  escaped <- gsub("\r", "\\\\r", escaped)
  escaped <- gsub("\t", "\\\\t", escaped)
  paste0('"', escaped, '"')
}

json_object <- function(x, indent = 0L) {
  pad <- strrep("  ", indent + 1L)
  parts <- vapply(names(x), function(name) {
    value <- x[[name]]
    rendered <- if (is.list(value)) {
      json_object(value, indent + 1L)
    } else {
      json_scalar(value)
    }
    paste0(pad, json_scalar(name), ": ", rendered)
  }, character(1))
  paste0("{\n", paste(parts, collapse = ",\n"), "\n", strrep("  ", indent), "}")
}

# GitHub installers preserve the resolved source revision in the installed
# DESCRIPTION. Prefer a live checkout when one is available because it can also
# report uncommitted changes; otherwise retain the installed revision rather
# than discarding the exact commit that supplied the package.
description_git_state <- function(description) {
  first_nonblank <- function(fields) {
    for (field in fields) {
      value <- description[[field]]
      if (!is.null(value) && length(value) && !is.na(value[[1]]) &&
          nzchar(trimws(value[[1]]))) {
        return(as.character(value[[1]]))
      }
    }
    NA_character_
  }
  list(
    commit = first_nonblank(c("RemoteSha", "GithubSHA1", "GitCommit")),
    branch = first_nonblank(c("RemoteRef", "GithubRef")),
    clean = NA
  )
}

git_state <- function(source_dir = system.file(package = "cassnoccurrences")) {
  description <- tryCatch(
    utils::packageDescription("cassnoccurrences"),
    error = function(e) list()
  )
  installed <- description_git_state(description)
  if (!nzchar(source_dir) || !dir.exists(source_dir)) return(installed)
  if (!nzchar(Sys.which("git"))) return(installed)

  run <- function(...) {
    out <- suppressWarnings(system2(
      "git", c("-C", shQuote(source_dir), ...),
      stdout = TRUE, stderr = FALSE
    ))
    if (!is.null(attr(out, "status")) && attr(out, "status") != 0) return(NULL)
    out
  }
  if (is.null(run("rev-parse", "--is-inside-work-tree"))) return(installed)

  commit <- run("rev-parse", "HEAD")
  branch <- run("rev-parse", "--abbrev-ref", "HEAD")
  status <- run("status", "--porcelain")
  list(
    commit = if (length(commit)) commit[[1]] else NA_character_,
    branch = if (length(branch)) branch[[1]] else NA_character_,
    clean = if (is.null(status)) NA else length(status) == 0L
  )
}

build_receipt <- function(output_csv, occurrences, organization, timezone,
                          built_at = Sys.time()) {
  git <- git_state()
  list(
    receipt_version = BUILD_RECEIPT_VERSION,
    package = list(
      name = "cassnoccurrences",
      version = as.character(utils::packageVersion("cassnoccurrences")),
      git_commit = git$commit,
      git_branch = git$branch,
      git_clean = git$clean
    ),
    r_version = R.version.string,
    platform = R.version$platform,
    built_at = format(built_at, "%Y-%m-%dT%H:%M:%S%z"),
    built_by = unname(Sys.info()[["user"]]),
    parameters = list(
      organization = organization,
      timezone = timezone
    ),
    product = list(
      file = basename(output_csv),
      row_count = nrow(occurrences),
      column_count = ncol(occurrences),
      sha256 = digest::digest(output_csv, algo = "sha256", file = TRUE)
    )
  )
}

# The receipt is named for its product so several builds can share one output
# directory without colliding.
build_receipt_path <- function(output_csv) {
  file.path(
    dirname(output_csv),
    paste0(tools::file_path_sans_ext(basename(output_csv)), "_build_receipt.json")
  )
}

write_build_receipt <- function(receipt, path) {
  writeLines(json_object(receipt), path)
  invisible(path)
}
