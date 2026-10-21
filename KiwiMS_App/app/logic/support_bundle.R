# app/logic/support_bundle.R

# The support package: one ZIP with what is needed to look into a problem a
# user reports - the logs of one session (the current one, or an earlier one
# chosen, e.g. one that crashed), the launcher log, versions and settings.
# No analysis data: no declaration tables, results or measurements.
#
# A session is one start of KiwiMS; its logs are in a folder of its own (see
# logging.R). Files still being written are taken as they are at that moment,
# R's own console output flushed first. An earlier session has ended, so its
# logs are complete: after a crash, start KiwiMS again and choose the session
# that crashed. When KiwiMS does not start at all, the launcher
# (dev/launch.ps1) packs the logs of that start itself.

box::use(
  app / logic / helper_functions[get_kiwims_version],
  app /
    logic /
    logging[get_log, get_log_dir, get_session_dir, session_dir_pattern],
  app / logic / user_settings[read_user_settings],
)

# Sessions offered to choose from, newest first
sessions_max <- 30

kiwims_dir <- function() {
  file.path(Sys.getenv("LOCALAPPDATA"), "KiwiMS")
}

# Folders holding the date folders of the session logs: the log folder of the
# settings, and the one this session logs to (the launcher picks it; the two
# differ for one start after the log folder was changed)
log_roots <- function() {
  roots <- unique(normalizePath(
    c(get_log_dir(), dirname(dirname(get_session_dir()))),
    winslash = "/",
    mustWork = FALSE
  ))
  roots[dir.exists(roots)]
}

# The session log of a session, `path` its folder or (earlier layout) the log
session_log_of <- function(path) {
  if (dir.exists(path)) {
    file.path(path, paste0(basename(path), ".log"))
  } else {
    path
  }
}

# An error as R reports it in its console: "Error: msg", "Error in f() : msg"
# (German "Fehler", from a development console; KiwiMS.exe runs R in
# English), or Shiny's report of one in an output or observer. Not messages
# that merely start with the word, like the update check's "Error fetching
# version: ..." when offline, which KiwiMS handles.
r_error_pattern <- "^(Error|Fehler)(:| in .* :)( |$)|^Warning: Error in "

# Whether `file` has a line matching `pattern`. Read in chunks: a console log
# grows to many MB over long deconvolutions, and the error that ended a
# session is at its end. Read only what exists: a session never opened in a
# browser has no session log, and readLines() warns about it - logr would log
# each warning.
file_has <- function(file, pattern) {
  if (!file.exists(file) || dir.exists(file)) {
    return(FALSE)
  }
  con <- tryCatch(file(file, open = "r", encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(con)) {
    return(FALSE)
  }
  on.exit(close(con))
  repeat {
    lines <- tryCatch(
      readLines(con, n = 20000, warn = FALSE),
      error = function(e) character(0)
    )
    if (!length(lines)) {
      return(FALSE)
    }
    if (any(grepl(pattern, lines, useBytes = TRUE))) {
      return(TRUE)
    }
  }
}

# Whether a session logged an error: an [ERROR] line in its session log, an
# R error in its console logs, or a start the launcher could not complete
#' @export
session_has_errors <- function(path) {
  file_has(session_log_of(path), "^\\[ERROR\\]") ||
    (dir.exists(path) &&
      (file_has(file.path(path, "console.log"), r_error_pattern) ||
        file_has(file.path(path, "console_stderr.log"), r_error_pattern) ||
        file_has(file.path(path, "launcher.log"), "CRITICAL ERROR:")))
}

# When a session started: the start time logr writes into the head of its
# session log, else the folder's (or file's) creation time
#' @export
session_start <- function(path) {
  log <- session_log_of(path)
  head <- if (file.exists(log)) {
    tryCatch(readLines(log, n = 15, warn = FALSE), error = function(e) character(0))
  } else {
    character(0)
  }
  stamp <- sub("^Log Start Time: ", "", grep("^Log Start Time: ", head, value = TRUE))
  start <- if (length(stamp)) {
    as.POSIXct(stamp[1], format = "%Y-%m-%d %H:%M:%OS")
  } else {
    NA
  }
  if (is.na(start)) {
    start <- file.info(path)$ctime
  }
  start
}

# The sessions with logs: data.frame(path, id, start, current, errors), newest
# first. A session is a folder of the current layout or the single log file of
# the earlier one.
#' @export
list_log_sessions <- function() {
  paths <- unlist(lapply(log_roots(), function(root) {
    days <- list.dirs(root, recursive = FALSE, full.names = TRUE)
    days <- days[grepl("^\\d{4}-\\d{2}-\\d{2}$", basename(days))]
    unlist(lapply(days, function(day) {
      entries <- list.files(day, full.names = TRUE, include.dirs = TRUE)
      folders <- entries[
        dir.exists(entries) & grepl(session_dir_pattern, basename(entries))
      ]
      files <- entries[
        !dir.exists(entries) &
          grepl("^KiwiMS_\\d{4}-\\d{2}-\\d{2}_id\\d{4}\\.log$", basename(entries))
      ]
      c(folders, files)
    }))
  }))
  current <- normalizePath(get_session_dir(), winslash = "/", mustWork = FALSE)
  paths <- unique(c(current, normalizePath(paths, winslash = "/", mustWork = FALSE)))
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return(data.frame(
      path = character(0),
      id = character(0),
      start = as.POSIXct(character(0)),
      current = logical(0),
      errors = logical(0)
    ))
  }

  # The newest by creation time (one stat each) before any file is read, so
  # years of sessions cost no more than the few offered
  created <- file.info(paths)$ctime
  paths <- utils::head(
    paths[order(paths != current, -as.numeric(created))],
    sessions_max
  )
  start <- do.call(c, lapply(paths, session_start))
  sessions <- data.frame(
    path = paths,
    id = sub("^.*_id(\\d{4})(\\.log)?$", "\\1", basename(paths)),
    start = start,
    current = paths == current,
    stringsAsFactors = FALSE
  )
  sessions <- sessions[order(!sessions$current, -as.numeric(sessions$start)), ]
  sessions$errors <- vapply(sessions$path, session_has_errors, logical(1))
  rownames(sessions) <- NULL
  sessions
}

# Choices of a picker of the sessions: labels naming start, ID and whether
# errors were logged, values their paths
#' @export
session_choices <- function(sessions = list_log_sessions()) {
  labels <- paste0(
    ifelse(sessions$current, "Current session · ", ""),
    format(sessions$start, "%Y-%m-%d %H:%M"),
    " · ID ",
    sessions$id,
    ifelse(sessions$errors, " · errors logged", "")
  )
  stats::setNames(sessions$path, labels)
}

copy_into <- function(files, dir) {
  files <- files[file.exists(files) & !dir.exists(files)]
  if (!length(files)) {
    return(character(0))
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(dir, basename(files))
  ok <- file.copy(files, target, overwrite = TRUE, copy.date = TRUE)
  target[ok]
}

write_text <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(as.character(lines), con)
}

system_info <- function(session, client = NULL) {
  version <- tryCatch(
    suppressWarnings(get_kiwims_version()),
    error = function(e) NULL
  )
  info <- Sys.info()
  env <- Sys.getenv(
    c(
      "KIWIMS_SESSION_DIR",
      "KIWIMS_SESSION_FILE",
      "KIWIMS_RSCRIPT",
      "KIWIMS_DEV_MODE",
      "RETICULATE_PYTHON",
      "R_HOME"
    ),
    unset = "(not set)"
  )
  c(
    "KiwiMS support package",
    paste("Created:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Session:", basename(session)),
    "Versions, system and browser below are those of the session that made",
    "the package.",
    "",
    "== KiwiMS ==",
    if (!is.null(version)) paste0(names(version), ": ", version),
    paste("Installed in:", normalizePath(getwd(), winslash = "\\")),
    paste("Session log:", get_log()),
    paste("Log folder:", get_log_dir()),
    paste("R process:", Sys.getpid()),
    "",
    "== System ==",
    paste0(names(info), ": ", info),
    paste("Locale:", Sys.getlocale()),
    paste("Time zone:", Sys.timezone()),
    paste("Logical cores:", parallel::detectCores()),
    "",
    "== Browser ==",
    if (length(client)) paste0(names(client), ": ", unlist(client)) else "(unknown)",
    "",
    "== Environment ==",
    paste0(names(env), ": ", env),
    "",
    "== R session ==",
    utils::capture.output(print(utils::sessionInfo()))
  )
}

# Console logs of a session the launcher did not put in its folder: a launcher
# from before the session folders wrote one pair per launcher PID to the
# KiwiMS folder; the newest pair is this session's
legacy_console_logs <- function() {
  logs <- list.files(
    kiwims_dir(),
    pattern = "^app_(output|error)_\\d+\\.log$",
    full.names = TRUE
  )
  if (!length(logs)) {
    return(character(0))
  }
  pid <- sub("^app_(output|error)_(\\d+)\\.log$", "\\2", basename(logs))
  newest <- pid[which.max(file.mtime(logs))]
  logs[pid == newest]
}

# Writes the package of `session` (a path from list_log_sessions(); the
# current session by default) to `zipfile`. `client` is what the browser
# told about itself.
#' @export
write_support_bundle <- function(
  zipfile,
  session = get_session_dir(),
  client = NULL
) {
  staging <- tempfile("kiwims_support_")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)

  # What R has printed so far, out of its buffers into the console log
  try(flush(stdout()), silent = TRUE)
  try(flush(stderr()), silent = TRUE)

  current <- identical(
    normalizePath(session, winslash = "/", mustWork = FALSE),
    normalizePath(get_session_dir(), winslash = "/", mustWork = FALSE)
  )
  session_files <- if (dir.exists(session)) {
    list.files(session, full.names = TRUE)
  } else {
    session
  }
  copied <- copy_into(session_files, file.path(staging, "session"))
  if (
    current &&
      !any(grepl("^console", basename(copied))) &&
      !any(grepl("^app_", basename(copied)))
  ) {
    copy_into(legacy_console_logs(), file.path(staging, "session"))
  }
  # The launcher's lines are in the session folder from the session folders
  # on; for a session of the earlier layout, the shared launch log
  if (!any(basename(copied) == "launcher.log")) {
    copy_into(file.path(kiwims_dir(), "launch.log"), file.path(staging, "launcher"))
  }

  write_text(system_info(session, client), file.path(staging, "system", "info.txt"))
  settings <- tryCatch(read_user_settings(), error = function(e) NULL)
  if (!is.null(settings)) {
    write_text(
      jsonlite::toJSON(settings, auto_unbox = TRUE, pretty = TRUE, null = "null"),
      file.path(staging, "system", "settings.json")
    )
  }

  write_text(
    c(
      "KiwiMS support package",
      "",
      paste0("session   the logs of session ", basename(session), ":"),
      "          KiwiMS_<date>_id<n>.log  what KiwiMS did (session log)",
      "          console.log              R's console: output, messages,",
      "                                   warnings and errors in order",
      "          console_stderr.log       what bypassed R's console (native",
      "                                   code, a hard crash); mostly empty",
      "          launcher.log             the launcher's lines for this start",
      "          deconvolution_*.log      output of each deconvolution run",
      "                                   (*_workers.log: of its workers)",
      "system    versions, operating system, browser, KiwiMS settings",
      "",
      "The logs name files, folders and samples. No analysis or measurement",
      "data is included."
    ),
    file.path(staging, "README.txt")
  )

  files <- list.files(staging, recursive = TRUE)
  zip::zip(zipfile, files = files, root = staging, mode = "mirror")
  invisible(zipfile)
}
