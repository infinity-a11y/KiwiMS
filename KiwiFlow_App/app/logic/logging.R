# app/logic/logging.R

box::use(
  logr[log_open, log_print, log_close, log_warning, log_error]
)

documents_path <- Sys.getenv("USERPROFILE")
log_dir <- file.path(documents_path, "Documents", "KiwiFlow", "logs")
log_daily <- file.path(log_dir, Sys.Date())

# Get new session id return log path
new_session_path <- function() {
  valid_id <- FALSE
  while (!valid_id) {
    session_id <- sample(1000:9999, 1) # Random 4-digit session ID
    log_filename <- paste0("KiwiFlow_", Sys.Date(), "_id", session_id, ".log")

    if (!log_filename %in% list.files(log_daily)) {
      valid_id <- TRUE
      log_path <- file.path(log_daily, log_filename)
      message(paste("Assigned ID", session_id, "to current session."))
      return(log_path)
    } else {
      message(paste("Session ID", session_id, "already present. Redrawing ..."))
    }
  }
}

log_path <- new_session_path()

# Start logging
#' @export
start_logging <- function() {
  # Create log dir in Documents
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  # Create daily log dir
  if (!dir.exists(log_daily)) {
    dir.create(log_daily, recursive = TRUE)
  }

  log_open(log_path, logdir = FALSE)
}

# Get the current 4-digit session ID
#' @export
get_session_id <- function() {
  # Regex looks for "id" followed by 4 digits before the .log extension
  session_id <- gsub(".*_id(\\d{4})\\.log$", "\\1", log_path)
  return(session_id)
}

# Log messages
#' @export
write_log <- function(msg, level = "INFO") {
  log_print(paste0("[", level, "] ", msg))
}

# Close logging
#' @export
close_logging <- function() {
  log_close()
}

# Get log directory
#' @export
get_log <- function() {
  return(log_path)
}

# Format log
#' @export
format_log <- function(log_text) {
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
