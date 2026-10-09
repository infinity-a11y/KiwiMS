# Checks a reference dataset against its committed manifest and says whether
# it can be used. Exit status 0 when it is identical, 1 otherwise.
#
# Usage (from KiwiMS_App):
#   Rscript tests/reference_data/verify.R <dataset> [path] [--rehash]
# path defaults to $KIWIMS_REFERENCE_DATA/<dataset>; --rehash ignores a
# remembered earlier check and hashes every file again.

here <- dirname(normalizePath(sub(
  "^--file=",
  "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)
)))
source(file.path(here, "reference_data.R"))

args <- commandArgs(trailingOnly = TRUE)
rehash <- "--rehash" %in% args
args <- setdiff(args, "--rehash")
if (!length(args)) {
  stop("Usage: verify.R <dataset> [path] [--rehash]")
}
dataset <- args[1]
path <- if (length(args) > 1) args[2] else ref_path(dataset)

result <- ref_verify(dataset, path, dir = here, rehash = rehash)
cat(sprintf("%s at %s: %s\n", dataset, result$path, toupper(result$status)))
if (length(result$problems)) {
  cat(paste0("  ", utils::head(result$problems, 20)), sep = "\n")
  if (length(result$problems) > 20) {
    cat(sprintf("  and %d more\n", length(result$problems) - 20))
  }
}
quit(status = if (result$status == "ok") 0 else 1)
