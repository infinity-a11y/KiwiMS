# app/logic/logging.R

box::use(
  logr[log_open, log_print, log_close, log_warning, log_error]
)

# The log folder, %LOCALAPPDATA%\KiwiMS\logs: in the user's profile, where
# KiwiMS can always write. It used to be a setting; a folder that was not
# writable, or that the launcher and the app saw differently, lost the logs.
log_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "KiwiMS", "logs")
log_daily <- file.path(log_dir, Sys.Date())
session_start_time <- Sys.time()

# Every start of KiwiMS (a session: one R process, however many browser pages
# it serves) logs into a folder of its own, <log dir>/<date>/KiwiMS_<date>_id<n>,
# with all its logs: the session log, R's console the launcher redirects
# (console.log; console_stderr.log, what bypasses R), the launcher's lines for
# this start (launcher.log), and the output of each deconvolution run. The
# launcher makes the folder, as it opens the console logs before R starts, and
# hands it over in KIWIMS_SESSION_DIR; started otherwise (development), R makes
# one itself.
#' @export
session_dir_pattern <- "^KiwiMS_\\d{4}-\\d{2}-\\d{2}_id\\d{4}$"

new_session_dir <- function(daily = log_daily) {
  repeat {
    session_id <- sample(1000:9999, 1) # Random 4-digit session ID
    name <- paste0("KiwiMS_", Sys.Date(), "_id", session_id)
    # Not taken by a session folder, nor by a session log of the earlier
    # layout (one file per session in the date folder)
    if (
      !dir.exists(file.path(daily, name)) &&
        !file.exists(file.path(daily, paste0(name, ".log")))
    ) {
      message(paste("Assigned ID", session_id, "to current session."))
      return(file.path(daily, name))
    }
  }
}

# The session's folder and log; changed by start_logging() when the folder
# cannot be made
session <- new.env()
session$dir <- local({
  given <- Sys.getenv("KIWIMS_SESSION_DIR")
  # A folder this R process made itself belongs to an earlier start of the app
  # in it (development: runApp() again in the same console). Taken up again,
  # its session log would be overwritten.
  own <- identical(Sys.getenv("KIWIMS_SESSION_DIR_PID"), as.character(Sys.getpid()))
  if (nzchar(given) && grepl(session_dir_pattern, basename(given)) && !own) {
    given
  } else {
    new_session_dir()
  }
})
session$log_path <- file.path(session$dir, paste0(basename(session$dir), ".log"))
session$moved_from <- NULL
# For the deconvolution processes, which write their output here too
set_session_dir_env <- function(dir) {
  Sys.setenv(
    KIWIMS_SESSION_DIR = dir,
    KIWIMS_SESSION_DIR_PID = Sys.getpid()
  )
}
set_session_dir_env(session$dir)

# The folder of this session's logs
#' @export
get_session_dir <- function() {
  session$dir
}

# Start logging. Should the log folder not be writable after all (a profile
# locked down by policy), the session logs to R's temporary folder, so KiwiMS
# still starts.
#' @export
start_logging <- function() {
  dir.create(session$dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(session$dir)) {
    session$moved_from <- session$dir
    session$dir <- new_session_dir(file.path(tempdir(), "logs", Sys.Date()))
    session$log_path <- file.path(
      session$dir,
      paste0(basename(session$dir), ".log")
    )
    dir.create(session$dir, recursive = TRUE, showWarnings = FALSE)
    set_session_dir_env(session$dir)
  }
  # The pointer the launcher read while the log folder was a setting
  unlink(file.path(Sys.getenv("LOCALAPPDATA"), "KiwiMS", "settings", "log_dir.txt"))

  log_open(session$log_path, logdir = FALSE)
  if (!is.null(session$moved_from)) {
    write_log(
      paste0(
        "Log folder not writable (",
        dirname(dirname(session$moved_from)),
        "), logging to R's temporary folder"
      ),
      level = "WARN"
    )
  }
}

# Get the current 4-digit session ID
#' @export
get_session_id <- function() {
  # Regex looks for "id" followed by 4 digits before the .log extension
  session_id <- gsub(".*_id(\\d{4})\\.log$", "\\1", session$log_path)
  return(session_id)
}

# Get full session prefix e.g. "KiwiMS_2026-04-19_id2686"
#' @export
get_session_prefix <- function() {
  gsub("\\.log$", "", basename(session$log_path))
}

# Log messages; level "INFO", "WARN" or "ERROR". logr rewraps a message into
# 80 columns and so runs its lines together, which turns a stack trace or a
# table into one block of text: a message of several lines is written to the
# file as it is, its lines after the first indented, with logr's time note.
#' @export
write_log <- function(msg, level = "INFO") {
  text <- paste0("[", level, "] ", msg)
  if (!grepl("\n", text, fixed = TRUE)) {
    log_print(text)
    return(invisible(text))
  }
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  entry <- c(
    lines[1],
    paste0("    ", lines[-1]),
    "",
    paste("NOTE: Log Print Time: ", format(Sys.time())),
    ""
  )
  cat(entry, sep = "\n")
  ok <- tryCatch(
    {
      cat(entry, sep = "\n", file = session$log_path, append = TRUE)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!ok) {
    log_print(text)
  }
  invisible(text)
}

# The last `n` lines of a text file, for the log ("(not readable)" without it)
#' @export
log_tail <- function(path, n = 30) {
  lines <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(lines)) {
    return("(not readable)")
  }
  paste(utils::tail(lines, n), collapse = "\n")
}

# The last error log_app_errors() saw (`seen`), and the last it logged
# (`message`, `time`): a repeat within a minute is seen but not logged
app_errors <- new.env()
app_errors$message <- ""
app_errors$time <- -Inf
app_errors$seen <- -Inf

# Errors of the app's outputs and observers to the session log, with where
# they happened and their stack trace. Without, the log has only the one line
# logr makes of Shiny's warning ("Warning: Error in f: ..."); the trace is
# printed to the console alone (console.log, kept for the ten newest
# sessions). Called once per process.
#' @export
log_app_errors <- function() {
  last <- app_errors

  # Called by Shiny's calling handler (shinyCallingHandlers()) for every error
  # that is not a req()/validate() one; the condition is in that handler's
  # frame. An output failing on every update is logged once a minute.
  options(shiny.error = function() {
    cond <- tryCatch(
      get("e", envir = parent.frame(), inherits = FALSE),
      error = function(e) NULL
    )
    if (!inherits(cond, "condition")) {
      return(invisible(NULL))
    }
    msg <- conditionMessage(cond)
    now <- as.numeric(Sys.time())
    # Always noted: the same error ending a session again must still be told
    # apart from a dropped connection (last_app_error_age())
    last$seen <- now
    if (identical(msg, last$message) && now - last$time < 60) {
      return(invisible(NULL))
    }
    last$message <- msg
    last$time <- now
    # The trace's messages caught as they come: capture.output(type =
    # "message") would reset the launcher's sink of R's messages into
    # console.log to stderr when done
    trace <- character(0)
    tryCatch(
      withCallingHandlers(
        shiny::printStackTrace(cond),
        message = function(m) {
          trace <<- c(trace, sub("\n$", "", conditionMessage(m)))
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) NULL
    )
    call <- conditionCall(cond)
    try(
      write_log(
        paste0(
          msg,
          if (!is.null(call)) {
            paste0("\nIn: ", paste(deparse(call, nlines = 3), collapse = " "))
          },
          if (length(trace)) {
            paste0("\n", paste(utils::head(trace, 40), collapse = "\n"))
          }
        ),
        level = "ERROR"
      ),
      silent = TRUE
    )
    invisible(NULL)
  })
  invisible(NULL)
}

# Seconds since the last error log_app_errors() saw (Inf without one), logged
# or not: an error in an observer ends the session, which main.R tells apart
# from a dropped connection this way
#' @export
last_app_error_age <- function() {
  as.numeric(Sys.time()) - app_errors$seen
}

# Close logging
#' @export
close_logging <- function() {
  log_close()
}

# Get current session log file path
#' @export
get_log <- function() {
  return(session$log_path)
}

# Get base log directory (the parent that holds daily sub-folders)
#' @export
get_log_dir <- function() {
  return(log_dir)
}

# Get session start time
#' @export
get_session_start <- function() {
  return(session_start_time)
}

# Format log
#' @export
format_log <- function(log_text) {
  log_text <- gsub("\033\\[[0-9;]*m", "", log_text)
  lines <- strsplit(log_text, "\n")[[1]]
  formatted_lines <- c()

  in_info_block <- FALSE
  info_block <- c()

  for (line in lines) {
    if (grepl("^\\[INFO\\]", line)) {
      # Flush previous INFO block if needed
      if (in_info_block) {
        formatted_lines <- c(
          formatted_lines,
          paste0(
            "<span style='color: #1e90ff; font-weight: bold; white-space: pre-line;'>",
            paste(info_block, collapse = "\n"),
            "</span>"
          )
        )
        info_block <- c()
      }

      # Start new INFO block
      in_info_block <- TRUE
      info_block <- c(line)
    } else if (in_info_block && line != "") {
      # Collect additional INFO lines
      info_block <- c(info_block, line)
    } else {
      # Flush INFO block before processing the new line
      if (in_info_block) {
        formatted_lines <- c(
          formatted_lines,
          paste0(
            "<span style='color: #1e90ff; font-weight: bold; white-space: pre-line;'>",
            paste(info_block, collapse = "\n"),
            "</span>"
          )
        )
        in_info_block <- FALSE
        info_block <- c()
      }

      # Style Headers
      if (grepl("^={10,}$", line)) {
        formatted_lines <- c(
          formatted_lines,
          "<hr style='border: 1px solid #e5e5e5;'>"
        )
      } else if (
        # Metadata Section (Gray & Italic)
        grepl(
          "^Log Path:|^Program Path:|^Working Directory:|^User Name:|^R Version:|^Machine:|^Operating System:|^Base Packages:|^Other Packages:|^Log Start Time:",
          line
        )
      ) {
        formatted_lines <- c(
          formatted_lines,
          paste0(
            "<div style='color: color: lightgray; font-style: italic;'>",
            line,
            "</div>"
          )
        )
      } else if (grepl("^\\[WARN\\]|^Warning:", line)) {
        # Convert "Warning:" messages to "[WARNING]" format
        warning_message <- gsub("^Warning:", "[WARNING]", line)

        # Extract the first letter after [WARNING] and capitalize it
        first_letter <- sub(
          "^\\[WARNING\\] (\\w)(.*)",
          "\\1",
          warning_message,
          perl = TRUE
        )
        rest_of_message <- sub(
          "^\\[WARNING\\] \\w(.*)",
          "\\1",
          warning_message,
          perl = TRUE
        )

        # Reconstruct the formatted warning message
        formatted_warning <- paste0(
          "[WARNING] ",
          toupper(first_letter),
          rest_of_message
        )

        formatted_lines <- c(
          formatted_lines,
          paste0(
            "<div style='color: orange; font-weight: bold;'>",
            formatted_warning,
            "</div>"
          )
        )
      } else if (grepl("^\\[ERROR\\]", line)) {
        # Errors (Red)
        formatted_lines <- c(
          formatted_lines,
          paste0("<div style='color: red; font-weight: bold;'>", line, "</div>")
        )
      } else if (grepl("^NOTE: Elapsed Time:", line)) {
        # Elapsed Time (Light Gray)
        formatted_lines <- c(
          formatted_lines,
          paste0(
            "<div style='color: lightgray; font-size: 0.9em; margin-bottom: 1em;'>",
            line,
            "</div>"
          )
        )
      } else if (grepl("^NOTE: Log Print Time:", line)) {
        # Log Timestamps (Green)
        formatted_lines <- c(
          formatted_lines,
          paste0("<div style='color: green;'>", line, "</div>")
        )
      } else {
        # Default (Normal text)
        formatted_lines <- c(formatted_lines, paste0("<div>", line, "</div>"))
      }
    }
  }

  # Flush remaining INFO block at the end
  if (in_info_block) {
    formatted_lines <- c(
      formatted_lines,
      paste0(
        "<span style='color: #1e90ff; font-weight: bold; white-space: pre-line;'>",
        paste(info_block, collapse = "\n"),
        "</span>"
      )
    )
  }

  return(paste(formatted_lines, collapse = ""))
}
