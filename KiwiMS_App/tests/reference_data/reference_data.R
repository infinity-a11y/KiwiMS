# Reference datasets: real acquisitions that live outside the repository and
# are verified against the MD5 manifest committed next to this file before a
# test uses them.
#
# Plain R, no box modules: sourced by tests/testthat/helper-reference-data.R
# and by the scripts in this folder and in tests/edge_cases.
#
# Location: KIWIMS_REFERENCE_DATA names the directory holding the datasets,
# one sub-directory each (default E:/KF_Testing/Test-Data, where they were
# acquired).

# ref_dir(): This folder (tests/reference_data) ----
ref_dir <- function(app_root = getOption("box.path")[1]) {
  file.path(app_root, "tests", "reference_data")
}

# ref_root(): Directory holding the reference datasets ----
ref_root <- function() {
  Sys.getenv("KIWIMS_REFERENCE_DATA", unset = "E:/KF_Testing/Test-Data")
}

# ref_path(): Where a dataset is expected ----
ref_path <- function(dataset) file.path(ref_root(), dataset)

# ref_manifest(): The committed digests of a dataset ----
# One row per file: its path relative to the dataset, size and MD5.
ref_manifest <- function(dataset, dir = ref_dir()) {
  utils::read.delim(
    file.path(dir, dataset, "manifest.tsv"),
    colClasses = c("character", "numeric", "character")
  )
}

# ref_list_files(): Files of a dataset as the manifest names them ----
ref_list_files <- function(path) {
  sort(list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE))
}

# ref_hash(): Manifest rows for the files of a dataset on disk ----
ref_hash <- function(path, files = ref_list_files(path)) {
  full <- file.path(path, files)
  data.frame(
    file = files,
    bytes = file.size(full),
    md5 = unname(tools::md5sum(full))
  )
}

# ref_cache_file(): Where a successful full check is remembered ----
ref_cache_file <- function(dataset) {
  file.path(
    tools::R_user_dir("KiwiMS", "cache"),
    "reference_data",
    paste0(dataset, ".rds")
  )
}

# ref_verify(): Check a dataset against its manifest ----
# Returns list(status, path, problems) with status
#   "missing"  - no directory at the expected place;
#   "mismatch" - files missing, extra, of another size or another MD5;
#   "ok"       - identical to the manifest.
# Hashing 6 GB takes a few minutes, so a passed check is remembered with the
# size and modification time of every file and the manifest it was checked
# against; it is repeated only when one of those changes, or always with
# rehash = TRUE (KIWIMS_REFERENCE_REHASH=1).
ref_verify <- function(
  dataset,
  path = ref_path(dataset),
  dir = ref_dir(),
  rehash = nzchar(Sys.getenv("KIWIMS_REFERENCE_REHASH"))
) {
  out <- function(status, problems = character(0)) {
    list(status = status, path = path, problems = problems)
  }
  if (!dir.exists(path)) {
    return(out("missing"))
  }

  manifest <- ref_manifest(dataset, dir)
  files <- ref_list_files(path)
  problems <- c(
    sprintf("missing: %s", setdiff(manifest$file, files)),
    sprintf("not in the manifest: %s", setdiff(files, manifest$file))
  )
  common <- intersect(manifest$file, files)
  size <- file.size(file.path(path, common))
  expected <- manifest$bytes[match(common, manifest$file)]
  problems <- c(
    problems,
    sprintf("size %s: %s, expected %s", common, size, expected)[size != expected]
  )
  if (length(problems)) {
    return(out("mismatch", problems))
  }

  info <- file.info(file.path(path, files))
  fingerprint <- ref_lines_md5(c(
    normalizePath(path, winslash = "/"),
    files,
    info$size,
    format(info$mtime, "%Y-%m-%d %H:%M:%OS3")
  ))
  manifest_md5 <- unname(tools::md5sum(file.path(dir, dataset, "manifest.tsv")))
  cache <- ref_cache_file(dataset)
  if (!rehash && file.exists(cache)) {
    seen <- readRDS(cache)
    if (identical(seen$fingerprint, fingerprint) && identical(seen$manifest, manifest_md5)) {
      return(out("ok"))
    }
  }

  hashed <- ref_hash(path, manifest$file)
  bad <- hashed$md5 != manifest$md5
  if (any(bad)) {
    return(out("mismatch", sprintf("md5 differs: %s", hashed$file[bad])))
  }
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(fingerprint = fingerprint, manifest = manifest_md5), cache)
  out("ok")
}

# ref_lines_md5(): MD5 of a character vector, one element per line ----
ref_lines_md5 <- function(lines) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(as.character(lines), tmp)
  unname(tools::md5sum(tmp))
}

# ref_peaks(): The expected peak list of a dataset ----
# The deconvolution result every reference test compares against, in the
# order the samples were written: sample, mass, intensity.
ref_peaks <- function(dataset, dir = ref_dir()) {
  utils::read.delim(
    file.path(dir, dataset, "peaks.tsv"),
    colClasses = c("character", "numeric", "numeric")
  )
}

# ref_params(): Deconvolution parameters the expected peaks were made with ----
# Named list in the shape of the UI's parameter frame; an empty value is an
# open bound (NA).
ref_params <- function(dataset, dir = ref_dir()) {
  p <- utils::read.delim(
    file.path(dir, dataset, "params.tsv"),
    colClasses = "character",
    na.strings = ""
  )
  stats::setNames(as.list(as.numeric(p$value)), p$key)
}

# ref_write_peaks(): Write a peak list losslessly ----
# Each number with the fewest significant digits (15-17) that read back as
# the same double.
ref_write_peaks <- function(peaks, file) {
  num <- function(x) {
    out <- sprintf("%.17g", x)
    for (d in 16:15) {
      s <- sprintf(paste0("%.", d, "g"), x)
      exact <- as.numeric(s) == x
      out[exact] <- s[exact]
    }
    out
  }
  lines <- c(
    "sample\tmass\tintensity",
    paste(peaks$sample, num(peaks$mass), num(peaks$intensity), sep = "\t")
  )
  writeLines(lines, file, useBytes = TRUE)
}
