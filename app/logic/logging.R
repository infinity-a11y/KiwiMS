# app/logic/logging.R

box::use(
  logr[log_open, log_print, log_close, log_warning, log_error]
)

log_dir <- file.path("logs", Sys.Date())

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
session_id <- sample(1000:9999, 1) # Random 4-digit session ID
log_filename <- paste0("KiwiFlow_", timestamp, "_id", session_id, ".log")
log_path <- file.path(log_dir, log_filename)

# Start logging
#' @export
start_logging <- function() {
  if (!dir.exists("logs")) dir.create("logs", recursive = TRUE)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_open(log_path, logdir = FALSE)
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
  return(file.path(getwd(), log_path))
}

# Format log
#' @export
format_log <- function(log_text) {
  lines <- strsplit(log_text, "\n")[[1]]
  formatted_lines <- c()

  for (line in lines) {
    # Style Headers
    if (grepl("^={10,}$", line)) {
      formatted_lines <- c(
        formatted_lines,
        "<hr style='border: 1px solid #555;'>"
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
        paste0("<div style='color: gray; font-style: italic;'>", line, "</div>")
      )
    } else if (grepl("^\\[INFO\\]", line)) {
      # INFO Messages (Blue)
      formatted_lines <- c(
        formatted_lines,
        paste0(
          "<div style='color: #1e90ff; font-weight: bold;'>",
          line,
          "</div>"
        )
      )
    } else if (grepl("^\\[WARN\\]", line)) {
      # Warnings (Yellow)
      formatted_lines <- c(
        formatted_lines,
        paste0(
          "<div style='color: orange; font-weight: bold;'>",
          line,
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
          "<div style='color: lightgray; font-size: 0.9em;'>",
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

  return(paste(formatted_lines, collapse = ""))
}
