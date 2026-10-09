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
    # Elution window [min]. Blank (NA) by default, which reads the whole
    # acquisition. No fixed window is safe across methods: on the test data
    # 0.5-1.5 min held the whole protein peak for the 1.4-min Waters runs but
    # clipped the 2-min runs (peak to ~1.7 min), selected no scans at all on a
    # run acquired from 1.4 min, and cut most of a 10-min Thermo gradient.
    deconv_time_start = NA_real_,
    deconv_time_end = NA_real_,
    deconv_peakwindow = 40,
    deconv_peaknorm = 2,
    deconv_peakthresh = 0.07,
    deconv_massbins = 0.5,
    deconv_keep_raw_output = FALSE,
    deconv_input_dir = "",
    log_dir = "",
    # Marks settings as written by a release that applies the elution window;
    # see migrate_time_window().
    deconv_time_window_applied = TRUE
  )
}

# migrate_time_window(): Drop the old fixed elution window from stored settings ----
# Earlier releases defaulted the window to 0.5-1.5 min but never applied it, so
# every deconvolution used the whole acquisition. update_user_setting() saves
# the full merged list, so that pair was persisted for anyone who ever saved a
# setting, whether they chose it or not. Now that the window takes effect,
# keeping it would silently narrow their runs, so the untouched pair is dropped
# and they get the whole acquisition, as before. A window the operator changed
# is kept. The marker key comes in from the defaults on the next save, so this
# runs only against files written before the change.
#' @export
migrate_time_window <- function(stored) {
  if (isTRUE(stored$deconv_time_window_applied)) {
    return(stored)
  }
  if (
    identical(as.numeric(stored$deconv_time_start), 0.5) &&
      identical(as.numeric(stored$deconv_time_end), 1.5)
  ) {
    stored$deconv_time_start <- NULL
    stored$deconv_time_end <- NULL
  }
  stored$deconv_time_window_notice_seen <- NULL
  stored
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
  stored <- migrate_time_window(stored)
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
