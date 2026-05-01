# app/logic/user_settings.R
# Persistent key-value store for user preferences (LOCALAPPDATA/KiwiMS/settings/user_settings.rds)

settings_path <- file.path(
  Sys.getenv("LOCALAPPDATA"),
  "KiwiMS",
  "settings",
  "user_settings.rds"
)

#' @export
get_default_user_settings <- function() {
  list(
    peak_tolerance = 3,
    max_multiples = 4,
    deconv_startz = 1,
    deconv_endz = 50,
    deconv_minmz = 710,
    deconv_maxmz = 1100,
    deconv_masslb = 10000,
    deconv_massub = 60000,
    deconv_time_start = 0.5,
    deconv_time_end = 1.5,
    deconv_peakwindow = 40,
    deconv_peaknorm = 2,
    deconv_peakthresh = 0.07,
    deconv_massbins = 0.5,
    deconv_keep_raw_output = FALSE,
    deconv_input_dir = "",
    log_dir = ""
  )
}

#' @export
read_user_settings <- function() {
  stored <- tryCatch(readRDS(settings_path), error = function(e) list())
  # Drop any NA entries so they fall back to the built-in default rather than
  # overriding it (modifyList keeps NAs from stored, which would persist blanks).
  stored <- stored[
    !vapply(
      stored,
      function(v) length(v) == 1L && !is.null(v) && is.na(v),
      logical(1L)
    )
  ]
  utils::modifyList(get_default_user_settings(), stored)
}

#' @export
save_user_settings <- function(s) {
  d <- dirname(settings_path)
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
  saveRDS(s, settings_path)
}

#' @export
update_user_setting <- function(key, value) {
  s <- read_user_settings()
  s[[key]] <- value
  save_user_settings(s)
}

#' @export
clear_user_setting <- function(key) {
  s <- read_user_settings()
  s[[key]] <- get_default_user_settings()[[key]]
  save_user_settings(s)
}
