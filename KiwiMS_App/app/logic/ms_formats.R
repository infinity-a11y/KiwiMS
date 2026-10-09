# app/logic/ms_formats.R
#
# One place that knows which mass-spectrometry inputs KiwiMS accepts, and how to
# get from a path to a sample name.
#
# Format detection deliberately does not sniff file contents. The extension plus
# one stat() is enough, because the only ambiguous case is .raw, and there a
# directory means Waters (MassLynx writes _FUNC001.DAT and friends inside it)
# while a file means Thermo. That is the same rule UniDec's own
# tools.get_importer() applies, so the R side and the Python side agree by
# construction.

box::use(
  fs[dir_ls],
)

# Longest extension first: ".mzml.gz" has to win over ".gz" when both could
# match the same name.
extensions <- c(".mzml.gz", ".mzxml", ".mzml", ".raw")

# Anchored, case-insensitive. Thermo exports are routinely SAMPLE.RAW, and
# UniDec's own examples use test.RAW, so every match here is ignore.case.
ext_pattern <- "\\.(mzml\\.gz|mzxml|mzml|raw)$"

#' @export
ms_extensions <- function() {
  extensions
}

# ms_sample_base(): path or filename -> sample name ----
# Strips the directory and any one supported extension. Everything downstream
# (DB keys, config Sample column, result folder names) uses this, so a sample
# is named the same whichever format it arrived in.
#' @export
ms_sample_base <- function(x) {
  if (length(x) == 0) {
    return(character(0))
  }
  gsub(ext_pattern, "", basename(as.character(x)), ignore.case = TRUE)
}

# ms_result_dirname(): sample -> the "<base>_rawdata_unidecfiles" folder name ----
# Replaces the twelve hand-rolled gsub() calls that built this string, and is
# anchored so a sample name containing "raw" is left alone.
#' @export
ms_result_dirname <- function(x) {
  paste0(ms_sample_base(x), "_rawdata_unidecfiles")
}

# ms_sample_in(): which of `x` name a sample that is also in `y` ----
# A config may list a sample with or without its extension ("A1.raw" or "A1"),
# and the extension is not what identifies it -- a Thermo A1.raw and an
# A1.mzML are the same sample name. So config-to-folder matching compares
# sample names, never file names.
#' @export
ms_sample_in <- function(x, y) {
  ms_sample_base(x) %in% ms_sample_base(y)
}

# ms_duplicate_samples(): inputs that would run under the same sample name ----
# X.raw next to X.mzML (or X.raw and x.mzML) both become sample "X": one DB key,
# one result folder -- and Windows paths ignore case, so X_rawdata_unidecfiles
# and x_rawdata_unidecfiles are the same folder too. The run cannot tell them
# apart, so such a set must be refused before it starts. Returns a list keyed
# by sample name, each entry the file names sharing it; empty when all differ.
#' @export
ms_duplicate_samples <- function(paths) {
  if (length(paths) < 2L) {
    return(list())
  }
  key <- tolower(ms_sample_base(paths))
  dup <- unique(key[duplicated(key)])
  groups <- lapply(dup, function(k) basename(paths[key == k]))
  names(groups) <- vapply(
    dup,
    function(k) ms_sample_base(paths[key == k][1]),
    character(1L)
  )
  groups
}

# describe_duplicate_samples(): "X (X.raw, X.mzML); Y (...)" for messages ----
#' @export
describe_duplicate_samples <- function(groups) {
  paste(
    paste0(names(groups), " (", vapply(groups, paste, character(1L), collapse = ", "), ")"),
    collapse = "; "
  )
}

#' @export
has_ms_extension <- function(path) {
  if (length(path) == 0) {
    return(logical(0))
  }
  grepl(ext_pattern, path, ignore.case = TRUE)
}

# is_ms_input(): does this path name something we can actually deconvolve? ----
# A .raw is valid either way -- directory (Waters) or file (Thermo) -- but the
# open formats are files only, so a directory called "run.mzML" is not an input.
#' @export
is_ms_input <- function(path) {
  vapply(
    path,
    function(p) {
      if (!nzchar(p) || !has_ms_extension(p)) {
        return(FALSE)
      }
      if (grepl("\\.raw$", p, ignore.case = TRUE)) {
        return(dir.exists(p) || file.exists(p))
      }
      file.exists(p) && !dir.exists(p)
    },
    logical(1L),
    USE.NAMES = FALSE
  )
}

# ms_format_label(): what to call this input in the interface ----
# The Waters/Thermo split is the whole reason the app cannot key off the
# extension alone, so it is worth surfacing to the operator: seeing "Thermo"
# next to a file they expected to be Waters is the fastest way to catch a
# half-copied MassLynx directory.
#' @export
ms_format_label <- function(path) {
  vapply(
    path,
    function(p) {
      lower <- tolower(p)
      if (grepl("\\.mzml\\.gz$", lower)) {
        return("mzML (gz)")
      }
      if (grepl("\\.mzml$", lower)) {
        return("mzML")
      }
      if (grepl("\\.mzxml$", lower)) {
        return("mzXML")
      }
      if (grepl("\\.raw$", lower)) {
        return(if (dir.exists(p)) "Waters" else "Thermo")
      }
      "unknown"
    },
    character(1L),
    USE.NAMES = FALSE
  )
}

# list_ms_inputs(): every deconvolvable input directly inside `dir` ----
# Non-recursive by design: a Waters .raw *is* a directory, so recursing would
# walk into MassLynx internals and, on a network share, turn one listing into
# hundreds of round trips -- the exact cost the folder picker exists to avoid.
#' @export
list_ms_inputs <- function(dir, full_names = TRUE) {
  if (length(dir) != 1L || !nzchar(dir) || !dir.exists(dir)) {
    return(character(0))
  }
  entries <- tryCatch(
    as.character(dir_ls(dir, recurse = FALSE, type = "any", fail = FALSE)),
    error = function(e) character(0)
  )
  if (!length(entries)) {
    return(character(0))
  }
  entries <- entries[has_ms_extension(entries)]
  if (!length(entries)) {
    return(character(0))
  }
  entries <- entries[is_ms_input(entries)]
  entries <- sort(entries)
  if (full_names) entries else basename(entries)
}

# describe_ms_inputs(): "9 Waters, 2 Thermo" for the sidebar ----
#' @export
describe_ms_inputs <- function(paths) {
  if (!length(paths)) {
    return("")
  }
  tab <- table(ms_format_label(paths))
  paste(paste(unname(tab), names(tab)), collapse = ", ")
}

# Human list of what the picker accepts, for tooltips and validation copy.
#' @export
ms_formats_phrase <- function() {
  "Thermo .raw files, Waters .raw folders, mzML or mzXML"
}
