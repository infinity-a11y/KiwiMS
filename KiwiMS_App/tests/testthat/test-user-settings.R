# Unit tests for the persisted user settings.

box::use(
  app/logic/user_settings[get_default_user_settings, migrate_settings],
  app/logic/conversion_constants[hit_preference_rules],
)

# merged(): What read_user_settings() returns for a stored file ----
merged <- function(stored) {
  utils::modifyList(get_default_user_settings(), migrate_settings(stored))
}

test_that("the elution window defaults to the whole acquisition", {
  d <- get_default_user_settings()
  expect_true(is.na(d$deconv_time_start))
  expect_true(is.na(d$deconv_time_end))
})

test_that("max charge and peak threshold default to 100 and 0.05", {
  d <- get_default_user_settings()
  expect_equal(d$deconv_endz, 100)
  expect_equal(d$deconv_peakthresh, 0.05)
  # The max charge reaches the mass range at the lowest m/z
  expect_gte(d$deconv_endz, d$deconv_massub / d$deconv_minmz)
})

test_that("the preferred assignment defaults to the first rule", {
  d <- get_default_user_settings()
  expect_identical(d$hit_preference, names(hit_preference_rules)[1])
  # Settings saved before the option existed fall back to it
  expect_identical(merged(list(max_multiples = 3))$hit_preference, d$hit_preference)
})

test_that("the defaults carry the current settings version", {
  expect_identical(get_default_user_settings()$settings_version, 2L)
  # So a file saved now is not migrated again
  d <- get_default_user_settings()
  expect_equal(migrate_settings(d), d)
})

# ---- Step 1: elution window -------------------------------------------------

test_that("the old 0.5-1.5 min default is dropped from pre-change settings", {
  # Earlier releases persisted 0.5-1.5 for everyone who saved any setting but
  # never applied it; keeping it now would silently narrow their runs.
  stored <- list(
    deconv_time_start = 0.5,
    deconv_time_end = 1.5,
    deconv_time_window_notice_seen = TRUE,
    deconv_peakthresh = 0.03
  )
  out <- migrate_settings(stored)
  expect_null(out$deconv_time_start)
  expect_null(out$deconv_time_end)
  expect_null(out$deconv_time_window_notice_seen)
  expect_equal(out$deconv_peakthresh, 0.03)
  expect_true(is.na(merged(stored)$deconv_time_start))
})

test_that("a window the operator changed survives the migration", {
  stored <- list(deconv_time_start = 0.6, deconv_time_end = 1.2)
  expect_equal(migrate_settings(stored), stored)

  stored <- list(deconv_time_start = 0.5, deconv_time_end = 3)
  expect_equal(migrate_settings(stored), stored)
})

test_that("a window saved after step 1 is left alone", {
  # The first 0.7.5 builds marked step 1 with deconv_time_window_applied; a
  # deliberately saved 0.5-1.5 behind that flag must not be cleared.
  stored <- list(
    deconv_time_start = 0.5,
    deconv_time_end = 1.5,
    deconv_time_window_applied = TRUE
  )
  out <- migrate_settings(stored)
  expect_equal(out$deconv_time_start, 0.5)
  expect_equal(out$deconv_time_end, 1.5)
  # The flag is superseded by settings_version
  expect_null(out$deconv_time_window_applied)

  stored$settings_version <- 1L
  stored$deconv_time_window_applied <- NULL
  expect_equal(migrate_settings(stored)$deconv_time_start, 0.5)
})

# ---- Step 2: max charge and peak threshold ----------------------------------

test_that("the old max charge and peak threshold defaults are dropped", {
  # A file written before step 2 holds 50 and 0.07 for everyone who saved any
  # setting; the new defaults must replace them.
  for (marker in list(list(), list(deconv_time_window_applied = TRUE), list(settings_version = 1L))) {
    stored <- c(list(deconv_endz = 50, deconv_peakthresh = 0.07, deconv_startz = 1), marker)
    out <- merged(stored)
    expect_equal(out$deconv_endz, 100)
    expect_equal(out$deconv_peakthresh, 0.05)
    expect_equal(out$deconv_startz, 1)
  }
})

test_that("a max charge or peak threshold the operator changed is kept", {
  stored <- list(deconv_endz = 60, deconv_peakthresh = 0.03)
  out <- merged(stored)
  expect_equal(out$deconv_endz, 60)
  expect_equal(out$deconv_peakthresh, 0.03)

  # Each value is judged on its own
  out <- merged(list(deconv_endz = 50, deconv_peakthresh = 0.1))
  expect_equal(out$deconv_endz, 100)
  expect_equal(out$deconv_peakthresh, 0.1)
})

test_that("50 and 0.07 saved after step 2 are left alone", {
  stored <- list(deconv_endz = 50, deconv_peakthresh = 0.07, settings_version = 2L)
  out <- merged(stored)
  expect_equal(out$deconv_endz, 50)
  expect_equal(out$deconv_peakthresh, 0.07)
})

test_that("a migrated file saved again keeps the new defaults", {
  # update_user_setting() writes the merged list back; reading that again must
  # not migrate anything a second time.
  once <- merged(list(deconv_endz = 50, deconv_peakthresh = 0.07, deconv_time_start = 0.5, deconv_time_end = 1.5))
  expect_identical(once$settings_version, 2L)
  expect_equal(merged(once), once)
})
