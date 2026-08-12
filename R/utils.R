## Declare the dplyr data-mask pronoun used throughout the package so R CMD
## check does not mistake it for an undefined global variable.
utils::globalVariables(".data")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

blank_to_na <- function(x) {
  x <- as.character(x)
  x[!nzchar(trimws(x))] <- NA_character_
  x
}

source_col <- function(df, name, default = NA) {
  if (name %in% names(df)) df[[name]] else rep(default, nrow(df))
}

first_nonmissing <- function(...) {
  args <- list(...)
  out <- args[[1]]
  for (arg in args[-1]) {
    out[is.na(out) | !nzchar(as.character(out))] <- arg[is.na(out) | !nzchar(as.character(out))]
  }
  out
}

specific_epithet <- function(scientific_name) {
  parts <- strsplit(as.character(scientific_name), "\\s+")
  vapply(parts, function(x) if (length(x) >= 2) x[[2]] else NA_character_, character(1))
}

genus_from_scientific_name <- function(scientific_name) {
  parts <- strsplit(as.character(scientific_name), "\\s+")
  vapply(parts, function(x) if (length(x) >= 1 && !is.na(x[[1]])) x[[1]] else NA_character_, character(1))
}

species_from_scientific_name <- function(scientific_name) {
  specific_epithet(scientific_name)
}

presence_filter <- function(scientific_name) {
  !is.na(scientific_name) & nzchar(trimws(scientific_name))
}

occurrence_id_from_filename <- function(filename) {
  x <- basename(as.character(filename))
  sub("\\.[^.]*$", "", x)
}

# Normalize one component of the readable occurrence key: collapse internal
# whitespace to underscores and trim.
id_token <- function(x) {
  x <- trimws(as.character(x))
  gsub("\\s+", "_", x)
}

# Mint the readable canonical key from stable source components. If ANY part is
# missing, the key is NA (we do not emit a partial or unstable identity).
make_occurrence_key <- function(...) {
  parts <- lapply(list(...), id_token)
  n <- max(vapply(parts, length, integer(1)))
  parts <- lapply(parts, function(p) if (length(p) == 1L) rep(p, n) else p)
  complete <- Reduce(`&`, lapply(parts, function(p) !is.na(p) & nzchar(p)))
  ids <- do.call(paste, c(parts, sep = ":"))
  ids[!complete] <- NA_character_
  ids
}

# Deterministic text ID. The visible CASSN prefix prevents CSV readers and
# spreadsheets from coercing the identifier to a number. The first 128 bits of
# the namespaced SHA-256 digest provide ample collision resistance for this use.
occurrence_id_from_key <- function(key) {
  key <- enc2utf8(as.character(key))
  out <- rep(NA_character_, length(key))
  complete <- !is.na(key) & nzchar(key)
  hashes <- vapply(
    paste0("cassn:v1:", key[complete]),
    digest::digest,
    character(1),
    algo = "sha256",
    serialize = FALSE
  )
  out[complete] <- paste0("CASSN-", substr(unname(hashes), 1, 32))
  out
}

make_occurrence_id <- function(...) {
  occurrence_id_from_key(make_occurrence_key(...))
}

format_event_date <- function(x, timezone = "America/Los_Angeles") {
  local <- lubridate::with_tz(x, timezone)
  out <- format(local, "%Y-%m-%dT%H:%M:%S%z")
  sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", out)
}

# Same instant as x, expressed in UTC alongside the local `eventDate` value.
to_utc_timestamp <- function(x) {
  lubridate::with_tz(x, "UTC")
}

# Tokens that are not real taxa (case-insensitive) -> NA. Extend deliberately;
# over-listing here silently drops legitimate occurrences.
taxon_sentinels <- c("no cv result", "unknown", "unidentifiable", "not listed", "none", "na")

clean_taxon_token <- function(x) {
  x <- blank_to_na(x)
  x[tolower(trimws(as.character(x))) %in% taxon_sentinels] <- NA_character_
  x
}

# Rank-aware taxon assembly: scientificName = the most specific rank present,
# taxonRank = which rank that was. Sentinels are stripped; humans are dropped
# (privacy) by returning NA at every rank. Vectorized over equal-length inputs.
resolve_taxon <- function(class = NULL, order = NULL, family = NULL,
                          genus = NULL, species = NULL) {
  cl <- clean_taxon_token(class)
  od <- clean_taxon_token(order)
  fm <- clean_taxon_token(family)
  gn <- clean_taxon_token(genus)
  sp <- clean_taxon_token(species)

  n <- max(length(cl), length(od), length(fm), length(gn), length(sp))
  pad <- function(v) if (length(v) == 0) rep(NA_character_, n) else v
  cl <- pad(cl); od <- pad(od); fm <- pad(fm); gn <- pad(gn); sp <- pad(sp)

  scientificName <- rep(NA_character_, n)
  taxonRank <- rep(NA_character_, n)

  is_species <- presence_filter(gn) & presence_filter(sp)
  scientificName[is_species] <- paste(gn[is_species], sp[is_species])
  taxonRank[is_species] <- "species"

  fill <- function(name, rank, value) {
    hit <- is.na(scientificName) & presence_filter(value)
    scientificName[hit] <<- value[hit]
    taxonRank[hit] <<- rank
  }
  fill("genus", "genus", gn)
  fill("family", "family", fm)
  fill("order", "order", od)
  fill("class", "class", cl)

  # Drop humans at any rank (camera-trap images of people are not published).
  human <- !is.na(gn) & tolower(gn) == "homo"
  scientificName[human] <- NA_character_
  taxonRank[human] <- NA_character_

  tibble::tibble(scientificName = scientificName, taxonRank = taxonRank)
}

# Rank of an already-resolved binomial/trinomial (NABat, Motus): 2 tokens ->
# species, 3 -> subspecies. NA for anything with fewer than two tokens.
taxon_rank_from_name <- function(scientific_name) {
  parts <- strsplit(as.character(scientific_name), "\\s+")
  vapply(parts, function(x) {
    x <- x[nzchar(x)]
    if (length(x) >= 3) "subspecies" else if (length(x) == 2) "species" else NA_character_
  }, character(1))
}

# Authoritative NABat 4-/6-letter species-code reference (built from NABat's
# official code workbook; see inst/extdata). No guessed fallback: callers must
# have this reference or an explicit override.
load_nabat_species_codes <- function() {
  path <- system.file("extdata", "nabat_species_codes.csv", package = "cassnoccurrences")
  if (!nzchar(path)) path <- file.path("inst", "extdata", "nabat_species_codes.csv")
  if (!file.exists(path)) {
    stop("NABat species-code reference not found (inst/extdata/nabat_species_codes.csv). ",
         "Pass species_lookup explicitly or restore the reference file.", call. = FALSE)
  }
  readr::read_csv(path, show_col_types = FALSE)
}
