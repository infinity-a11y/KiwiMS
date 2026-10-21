# app/view/log_view.R

box::use(
  bslib[nav_menu, nav_panel, navset_card_tab],
  shiny[
    div,
    HTML,
    isolate,
    moduleServer,
    NS,
    observe,
    observeEvent,
    reactiveVal,
    renderUI,
    tagList,
    uiOutput,
  ],
  clipr[write_clip],
)

box::use(
  app / logic / logging[get_log, get_session_dir, format_log]
)

# Lines of a log shown at most; longer ones show their end (Show File opens
# the whole file). The console logs grow with every deconvolution.
log_lines_max <- 5000

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "logs-tabset",
      uiOutput(ns("log_tabs"), fill = TRUE)
    )
  )
}

# The log files of the current session (its folder, see logging.R), as tabs:
# data.frame(value, label, path, kind, run) in the order shown. A
# deconvolution run has its process and worker log; `run` names it. The
# console and launcher logs always have a tab: only KiwiMS.exe writes them, so
# a session started otherwise (from R) says so in place of the log. What
# bypassed R's console (console_stderr.log, mostly empty) is shown below it.
session_log_files <- function() {
  dir <- get_session_dir()
  files <- if (dir.exists(dir)) list.files(dir, full.names = TRUE) else character(0)
  fixed <- data.frame(
    value = c("session", "console", "launcher"),
    label = c("Session Log", "R Console", "Launcher"),
    path = c(get_log(), file.path(dir, c("console.log", "launcher.log"))),
    kind = c("session", "text", "text"),
    run = NA_character_,
    extra = c(NA_character_, file.path(dir, "console_stderr.log"), NA_character_)
  )

  runs <- files[grepl("^deconvolution_\\d{6}(_workers)?\\.log$", basename(files))]
  if (!length(runs)) {
    return(fixed)
  }
  runs <- runs[order(basename(runs))]
  stamp <- sub("^deconvolution_(\\d{6}).*$", "\\1", basename(runs))
  time <- paste(
    substr(stamp, 1, 2),
    substr(stamp, 3, 4),
    substr(stamp, 5, 6),
    sep = ":"
  )
  workers <- grepl("_workers\\.log$", basename(runs))
  decon <- data.frame(
    value = sub("\\.log$", "", basename(runs)),
    label = ifelse(workers, "Workers", "Process"),
    path = runs,
    kind = rep("text", length(runs)),
    run = paste("Run", time),
    extra = NA_character_
  )
  rbind(fixed, decon)
}

# A log's lines for display, at most log_lines_max of them, the last
read_log <- function(path) {
  if (!file.exists(path)) {
    if (identical(path, get_log())) {
      return(list(lines = "No logs yet.", cut = 0))
    }
    return(list(
      lines = paste(
        "Not written in this session: KiwiMS.exe writes this log, and this",
        "session was not started with it (its console output is in the R",
        "console)."
      ),
      cut = 0
    ))
  }
  lines <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) "Log not readable."
  )
  if (!length(lines)) {
    lines <- "Nothing written yet."
  }
  cut <- max(0, length(lines) - log_lines_max)
  list(lines = utils::tail(lines, log_lines_max), cut = cut)
}

log_panel_body <- function(entry) {
  log <- read_log(entry$path)
  text <- paste(log$lines, collapse = "\n")
  div(
    class = "log-file-output",
    if (log$cut > 0) {
      div(
        class = "log-file-cut",
        sprintf(
          "The first %d lines are not shown - Show File opens the whole log.",
          log$cut
        )
      )
    },
    if (entry$kind == "session") {
      HTML(format_log(text))
    } else {
      div(class = "log-file-text", text)
    },
    extra_log(entry$extra)
  )
}

# A second log shown below the first, when it has anything in it
extra_log <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(NULL)
  }
  log <- read_log(path)
  # The launcher creates the file with a byte order mark alone
  written <- nzchar(trimws(sub("^﻿", "", log$lines)))
  if (!any(written) || identical(log$lines, "Nothing written yet.")) {
    return(NULL)
  }
  div(
    class = "log-file-extra",
    div(
      class = "log-file-cut",
      paste0("Written outside R's console (", basename(path), "):")
    ),
    div(class = "log-file-text", paste(log$lines, collapse = "\n"))
  )
}

#' @export
server <- function(id, active_tab_reactive, log_buttons) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    files <- reactiveVal(NULL)

    # The logs as they are now, while the Logs tab is open or on Refresh. A
    # failure is shown in place of the logs: the view that helps with a
    # problem must not end the session itself.
    refresh <- function() {
      tryCatch(
        refresh_tabs(),
        error = function(e) {
          output$log_tabs <- renderUI(
            div(
              class = "log-file-output",
              "The logs could not be shown: ",
              conditionMessage(e),
              ". Show File opens the session log."
            )
          )
        }
      )
    }

    refresh_tabs <- function() {
      entries <- session_log_files()
      files(entries)
      selected <- isolate(input$log_file)
      if (is.null(selected) || !selected %in% entries$value) {
        selected <- "session"
      }
      panels <- lapply(seq_len(nrow(entries))[is.na(entries$run)], function(i) {
        nav_panel(entries$label[i], log_panel_body(entries[i, ]), value = entries$value[i])
      })
      runs <- entries[!is.na(entries$run), ]
      menu <- if (nrow(runs)) {
        do.call(
          nav_menu,
          c(
            list(title = "Deconvolution"),
            lapply(seq_len(nrow(runs)), function(i) {
              nav_panel(
                paste(runs$run[i], "·", runs$label[i]),
                log_panel_body(runs[i, ]),
                value = runs$value[i]
              )
            })
          )
        )
      }
      output$log_tabs <- renderUI({
        do.call(
          navset_card_tab,
          c(
            list(id = ns("log_file"), selected = selected),
            panels,
            if (!is.null(menu)) list(menu)
          )
        )
      })
    }

    observe({
      if (identical(active_tab_reactive(), "Logs")) {
        refresh()
      }
    })

    observeEvent(log_buttons$refresh(), {
      if (identical(active_tab_reactive(), "Logs")) {
        refresh()
      }
    })

    # The log of the tab shown
    shown_path <- function() {
      entries <- files()
      if (is.null(entries)) {
        return(get_log())
      }
      at <- match(input$log_file, entries$value)
      if (is.na(at)) get_log() else entries$path[at]
    }

    # Neither may end the session either (no clipboard, no program for .log)
    observeEvent(log_buttons$copy(), {
      try(
        write_clip(read_log(shown_path())$lines, allow_non_interactive = TRUE),
        silent = TRUE
      )
    })

    observeEvent(log_buttons$show_file(), {
      try(
        {
          path <- shown_path()
          if (file.exists(path)) {
            shell.exec(normalizePath(path))
          }
        },
        silent = TRUE
      )
    })
  })
}
