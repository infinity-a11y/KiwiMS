# Writes the MD5 manifest of a reference dataset: one row per file with its
# path relative to the dataset, size and MD5. Run it once when a dataset is
# added (or deliberately replaced) and commit the manifest.
#
# Usage (from KiwiMS_App):
#   Rscript tests/reference_data/make_manifest.R <dataset> [path]
# path defaults to $KIWIMS_REFERENCE_DATA/<dataset>.

here <- dirname(normalizePath(sub(
  "^--file=",
  "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)
)))
source(file.path(here, "reference_data.R"))

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop("Usage: make_manifest.R <dataset> [path]")
}
dataset <- args[1]
path <- if (length(args) > 1) args[2] else ref_path(dataset)
if (!dir.exists(path)) {
  stop("No dataset at ", path)
}

started <- Sys.time()
manifest <- ref_hash(path)
dir.create(file.path(here, dataset), showWarnings = FALSE)
out <- file.path(here, dataset, "manifest.tsv")
utils::write.table(
  manifest,
  out,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
cat(sprintf(
  "Wrote %s: %d files, %.2f GB, hashed in %.0f s\n",
  out,
  nrow(manifest),
  sum(manifest$bytes) / 1024^3,
  as.numeric(difftime(Sys.time(), started, units = "secs"))
))
