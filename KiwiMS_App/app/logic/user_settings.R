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
    # Off by default: measuring the peak width from the data usually improves
    # the fit but not always, and switching it on silently would change every
    # existing user's numbers. See the tooltip on the control.
    deconv_auto_peak_width = FALSE,
    deconv_keep_raw_output = FALSE,
    deconv_input_dir = "",
    log_dir = "",
    # One-shot acknowledgement of the release in which the elution window began
    # to take effect. Before it, time_start/time_end were written into the run
    # config but never applied, so every deconvolution silently used the whole
    # acquisition; results from this release on will differ for anyone whose
    # window is narrower than their run. Only operators with settings saved
    # before the change are shown the notice -- see main.R.
    deconv_time_window_notice_seen = FALSE
  )
}

# user_settings_exist(): has this machine ever saved a setting? ----
# Distinguishes an upgrade from a first install, which is what decides whether
# a behaviour-change notice is worth showing at all.
#' @export
user_settings_exist <- function() {
  file.exists(settings_path)
}

#' @export
read_user_settings <- function() {
  # Guard with file.exists(): readRDS() emits a "cannot open compressed file"
  # warning before it errors, and tryCatch(error=) swallows only the error - the
  # warning still reaches the log. The file is absent by definition until the
  # first setting is saved, so a fresh install would log it on every read.
  stored <- if (file.exists(settings_path)) {
    tryCatch(readRDS(settings_path), error = function(e) list())
  } else {
    list()
  }
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
