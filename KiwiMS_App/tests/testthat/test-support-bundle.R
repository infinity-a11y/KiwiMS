# Unit tests for the support package (app/logic/support_bundle.R)

box::use(
  app/logic/support_bundle[
    session_choices,
    session_has_errors,
    session_start,
    write_support_bundle,
  ],
)

# fake_session_dir(): a session folder of the current layout under `root`
fake_session_dir <- function(root, id = "1234", error = FALSE) {
  dir <- file.path(root, "2026-10-21", paste0("KiwiMS_2026-10-21_id", id))
  dir.create(dir, recursive = TRUE)
  writeLines(
    c("Log Start Time: 2026-10-21 10:39:52", "[INFO] Session started", if (error) "[ERROR] boom"),
    file.path(dir, paste0(basename(dir), ".log"))
  )
  writeLines(
    c("[INFO] Session started", "Warning: Error in f: boom"),
    file.path(dir, "console.log")
  )
  writeLines("", file.path(dir, "console_stderr.log"))
  writeLines("launcher lines", file.path(dir, "launcher.log"))
  writeLines("decon", file.path(dir, "deconvolution_103955.log"))
  writeLines("workers", file.path(dir, "deconvolution_103955_workers.log"))
  dir
}

zip_files <- function(zipfile) zip::zip_list(zipfile)$filename

test_that("the support package holds the logs of the session chosen, and no analysis", {
  withr::local_envvar(LOCALAPPDATA = withr::local_tempdir())
  # The app runs from its root, where resources/version.txt is
  withr::local_dir(kiwims_app_root())
  root <- withr::local_tempdir()
  chosen <- fake_session_dir(root, "1234")
  other <- fake_session_dir(root, "5678")

  zipfile <- file.path(withr::local_tempdir(), "support.zip")
  write_support_bundle(zipfile, session = chosen, client = list(userAgent = "test browser"))
  files <- zip_files(zipfile)

  expect_setequal(files, c(
    "README.txt",
    "system/info.txt",
    "system/settings.json",
    "session/KiwiMS_2026-10-21_id1234.log",
    "session/console.log",
    "session/console_stderr.log",
    "session/launcher.log",
    "session/deconvolution_103955.log",
    "session/deconvolution_103955_workers.log"
  ))
  # Only the session chosen, and no analysis data
  expect_false(any(grepl("5678", files)))
  expect_false(any(startsWith(files, "analysis/")))

  out <- withr::local_tempdir()
  zip::unzip(zipfile, exdir = out)
  info <- readLines(file.path(out, "system/info.txt"))
  expect_true(any(grepl("userAgent: test browser", info, fixed = TRUE)))
  expect_true("Session: KiwiMS_2026-10-21_id1234" %in% info)
  kiwims <- info[seq(which(info == "== KiwiMS ==") + 1, which(info == "== System ==") - 1)]
  expect_true(any(grepl("^version: [0-9]", kiwims)))
})

test_that("a session of the earlier layout comes with the shared launch log", {
  local_appdata <- withr::local_tempdir()
  dir.create(file.path(local_appdata, "KiwiMS"))
  writeLines("launcher", file.path(local_appdata, "KiwiMS", "launch.log"))
  withr::local_envvar(LOCALAPPDATA = local_appdata)
  withr::local_dir(kiwims_app_root())
  # One file per session in the date folder
  day <- file.path(withr::local_tempdir(), "2026-09-01")
  dir.create(day)
  legacy <- file.path(day, "KiwiMS_2026-09-01_id4321.log")
  writeLines("[INFO] Session started", legacy)

  zipfile <- file.path(withr::local_tempdir(), "support.zip")
  write_support_bundle(zipfile, session = legacy)
  files <- zip_files(zipfile)
  expect_true(all(c("session/KiwiMS_2026-09-01_id4321.log", "launcher/launch.log") %in% files))
})

test_that("a session without a session log is listed without warnings", {
  # Started, but never opened in a browser: console logs only
  root <- withr::local_tempdir()
  dir <- fake_session_dir(root, "2468")
  file.remove(file.path(dir, paste0(basename(dir), ".log")))
  expect_no_warning(write_support_bundle(
    file.path(withr::local_tempdir(), "support.zip"),
    session = dir
  ))
  expect_no_warning(session_has_errors(dir))
  expect_no_warning(session_start(dir))
})

test_that("an R error in the console log marks the session, in German too", {
  root <- withr::local_tempdir()
  dir <- fake_session_dir(root, "1357")
  console <- file.path(dir, "console.log")
  expect_true(session_has_errors(dir))
  writeLines("[INFO] Session started", console)
  expect_false(session_has_errors(dir))

  # R's own formats, top level and in a call, the call alone on its line
  for (line in c("Error: boom", "Error in f(x) : boom", "Error in f(x) :", "Fehler in f(x) : boom")) {
    writeLines(c("Listening on http://127.0.0.1:1234", line), console)
    expect_true(session_has_errors(dir), label = line)
  }
  writeLines("Fehler: boom", file.path(dir, "console_stderr.log"))
  expect_true(session_has_errors(dir))
})

test_that("a handled message starting with Error does not mark the session", {
  root <- withr::local_tempdir()
  dir <- fake_session_dir(root, "2468")
  # Offline: the update check fails, KiwiMS carries on
  writeLines(
    c(
      "Error fetching version: Could not resolve host: raw.githubusercontent.com",
      "Error fetching latest release URL: Timeout was reached",
      "[ERROR] logged by KiwiMS itself appears in the session log, not here"
    ),
    file.path(dir, "console.log")
  )
  expect_false(session_has_errors(dir))
})

test_that("an error at the end of a long console log marks the session", {
  root <- withr::local_tempdir()
  dir <- fake_session_dir(root, "3579")
  console <- file.path(dir, "console.log")
  # Over 5 MB of deconvolution progress, then the error that ended R
  writeLines(rep("Checking progress at: 2026-10-21 10:00:00", 150000), console)
  cat("Error in f() : boom\n", file = console, append = TRUE)
  expect_gt(file.size(console), 5 * 1024^2)
  expect_true(session_has_errors(dir))
})

test_that("a start the launcher could not complete marks the session", {
  root <- withr::local_tempdir()
  dir <- fake_session_dir(root, "4680")
  writeLines("[INFO] Session started", file.path(dir, "console.log"))
  expect_false(session_has_errors(dir))
  cat(
    "10/21/2026 14:55:52 - [26520] CRITICAL ERROR: R process exited unexpectedly (code 1).\n",
    file = file.path(dir, "launcher.log"),
    append = TRUE
  )
  expect_true(session_has_errors(dir))
})

test_that("the session picker names start, ID and logged errors", {
  sessions <- data.frame(
    path = c("a", "b"),
    id = c("1234", "5678"),
    start = as.POSIXct(c("2026-10-21 11:46:00", "2026-10-21 10:39:00")),
    current = c(TRUE, FALSE),
    errors = c(FALSE, TRUE)
  )
  expect_equal(
    session_choices(sessions),
    c(
      "Current session · 2026-10-21 11:46 · ID 1234" = "a",
      "2026-10-21 10:39 · ID 5678 · errors logged" = "b"
    )
  )
})
