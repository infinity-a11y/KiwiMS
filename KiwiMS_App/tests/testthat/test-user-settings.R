# Unit tests for the persisted user settings.

box::use(
  app/logic/user_settings[get_default_user_settings, migrate_time_window],
)

test_that("the elution window defaults to the whole acquisition", {
  d <- get_default_user_settings()
  expect_true(is.na(d$deconv_time_start))
  expect_true(is.na(d$deconv_time_end))
})

test_that("the old 0.5-1.5 min default is dropped from pre-change settings", {
  # Earlier releases persisted 0.5-1.5 for everyone who saved any setting but
  # never applied it; keeping it now would silently narrow their runs.
  stored <- list(
    deconv_time_start = 0.5,
    deconv_time_end = 1.5,
    deconv_time_window_notice_seen = TRUE,
    deconv_peakthresh = 0.05
  )
  out <- migrate_time_window(stored)
  expect_null(out$deconv_time_start)
  expect_null(out$deconv_time_end)
  expect_null(out$deconv_time_window_notice_seen)
  expect_equal(out$deconv_peakthresh, 0.05)
})

test_that("a window the operator changed survives the migration", {
  stored <- list(deconv_time_start = 0.6, deconv_time_end = 1.2)
  expect_equal(migrate_time_window(stored), stored)

  stored <- list(deconv_time_start = 0.5, deconv_time_end = 3)
  expect_equal(migrate_time_window(stored), stored)
})

test_that("settings written after the change are left alone", {
  # A deliberately saved 0.5-1.5 carries the marker and must not be cleared.
  stored <- list(
    deconv_time_start = 0.5,
    deconv_time_end = 1.5,
    deconv_time_window_applied = TRUE
  )
  expect_equal(migrate_time_window(stored), stored)
})
