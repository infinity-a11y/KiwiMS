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
    # Rule picking the preferred reading of a peak one compound fits in more
    # than one way (conversion_constants$hit_preference_rules)
    hit_preference = "stoichiometry",
    deconv_startz = 1,
    # Max charge must reach max mass / min m/z (60,000 / 710 = 85): below that,
    # the high charge states of a large protein are forced onto wrong masses.
    # At 50, 44 kDa proteins (PROTwt, RACA) showed artifact peaks ~430 Da apart
    # and, over 10-60 kDa, fake half-mass peaks as the tallest; at 100 they were
    # gone, and proteins up to ~30 kDa gave identical peaks.
    deconv_endz = 100,
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
    # Peak threshold, relative to the tallest point of the mass spectrum. A peak
    # below it counts as zero, so binding snaps to 0 % (complex missed) or
    # 100 % (unbound missed). 0.07, the earlier default, had no documented
    # source (UniDec's own default is 0.1). Over the test corpus 0.05 removed
    # most of that clipping -- MLKL kinact/KI 334 -> 353 (357 at 0.03), COOB
    # fittable at all -- while raising the false-hit rate of decoy compounds by
    # at most 2.5 points; 0.03 raised it by up to 7 and added screen hits at
    # about the rate noise predicts.
    deconv_peakthresh = 0.05,
    deconv_massbins = 0.5,
    deconv_keep_raw_output = FALSE,
    deconv_input_dir = "",
    log_dir = "",
    # Version of the stored defaults; see migrate_settings().
    settings_version = 2L
  )
}

# migrate_settings(): Bring settings saved by an older release up to date ----
# update_user_setting() saves the full merged list, so every default of the
# release that wrote the file was persisted for anyone who ever saved a
# setting, whether they chose it or not. When a default changes, the old value
# still sitting untouched in the file would override the new one; it is
# dropped so the new default applies. A value the operator changed is kept --
# though one that happens to equal the old default can't be told apart.
# settings_version comes in from the defaults on the next save, so each step
# runs only against files written before it.
#   1. 0.7.5: the elution window takes effect. The never-applied 0.5-1.5 min
#      default is dropped (blank = the whole acquisition, as before).
#   2. Max charge 50 -> 100, peak threshold 0.07 -> 0.05.
#' @export
migrate_settings <- function(stored) {
  version <- suppressWarnings(as.integer(stored$settings_version))
  if (!length(version) || is.na(version)) {
    # Files of the first 0.7.5 builds mark step 1 with a flag of their own
    version <- if (isTRUE(stored$deconv_time_window_applied)) 1L else 0L
  }
  is_old <- function(key, value) {
    identical(suppressWarnings(as.numeric(stored[[key]])), value)
  }

  if (version < 1L) {
    if (is_old("deconv_time_start", 0.5) && is_old("deconv_time_end", 1.5)) {
      stored$deconv_time_start <- NULL
      stored$deconv_time_end <- NULL
    }
  }
  if (version < 2L) {
    if (is_old("deconv_endz", 50)) {
      stored$deconv_endz <- NULL
    }
    if (is_old("deconv_peakthresh", 0.07)) {
      stored$deconv_peakthresh <- NULL
    }
  }

  stored$deconv_time_window_notice_seen <- NULL
  stored$deconv_time_window_applied <- NULL
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
  stored <- migrate_settings(stored)
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
