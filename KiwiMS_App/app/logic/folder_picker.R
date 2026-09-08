# app/logic/folder_picker.R
#
# Directory selection that does no filesystem work in the R process.
#
# It replaces shinyFiles for the deconvolution sidebar. shinyFiles resolves the
# browser tree server side: traverseDirs() runs dir_ls() plus a dir.exists()
# stat on every entry of every *expanded* node, and re-walks the whole expanded
# tree on every click rather than just the node being opened. Measured on a
# local SSD that is ~2900 stat calls for a three-level tree. Each of those is a
# network round trip when the profile is redirected or a share is mapped, and
# because R is single threaded it runs inside the Shiny observer - so the
# sidebar, the menu and every other output freeze along with the picker.
#
# Here the shell enumerates instead, in its own process, lazily, with its own
# handling of offline shares. R spawns it, then polls a result file every
# 250 ms; between polls the session stays fully reactive.

box::use(
  processx[process],
  shiny,
  shinyjs[disable, enable],
)

# Resolved against this module's own directory, not the working directory.
# launch.ps1 and dev/dev_mode.R's runApp() do not have to agree on getwd(), and
# a wd-relative path fails in the worst possible way here: file.exists() is
# FALSE, no dialog opens, and the button silently does nothing.
script_path <- function() {
  candidate <- tryCatch(box::file("select_folder.ps1"), error = function(e) "")
  if (length(candidate) == 1L && nzchar(candidate) && file.exists(candidate)) {
    return(candidate)
  }
  # box::file() only works while loaded as a module; keep the old path for a
  # plain source() (the unit tests do this).
  file.path("app", "logic", "select_folder.ps1")
}

#' @export
folder_dialog_available <- function() {
  Sys.info()[["sysname"]] == "Windows" && file.exists(script_path())
}

# "OK\n<path>" or "CANCEL", UTF-8 without BOM. Paths here routinely carry
# umlauts, so the connection has to be opened with an explicit encoding.
read_result <- function(out) {
  con <- file(out, encoding = "UTF-8")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  lines <- tryCatch(
    readLines(con, warn = FALSE),
    warning = function(w) NULL,
    error = function(e) NULL
  )
  lines <- lines[nzchar(trimws(lines))]

  if (!length(lines) || identical(trimws(lines[1]), "CANCEL")) {
    return(list(status = "cancel", path = NULL))
  }
  if (length(lines) < 2L) {
    return(list(status = "error", path = NULL))
  }

  # parseDirPath() used to hand the rest of the app forward slashes; keep that
  # so nothing downstream has to learn a second path flavour.
  list(status = "ok", path = gsub("\\\\", "/", trimws(lines[2])))
}

#' @export
start_folder_dialog <- function(initial_dir = NULL, title = "Select folder") {
  script <- script_path()
  if (!file.exists(script)) {
    message(
      "[WARN] Folder dialog script not found at '", script,
      "' (working directory: ", getwd(), ")."
    )
    return(NULL)
  }

  out <- tempfile(pattern = "kiwims_folder_", fileext = ".txt")
  args <- c(
    "-NoProfile",
    # The shell dialog is apartment threaded; powershell.exe already defaults to
    # STA, but -Sta makes that independent of how the host was configured.
    "-Sta",
    "-ExecutionPolicy",
    "Bypass",
    "-WindowStyle",
    "Hidden",
    "-File",
    normalizePath(script, winslash = "\\"),
    "-OutFile",
    out,
    "-Title",
    title
  )

  # A saved path can point at a share that has since gone away; the dialog
  # treats the start folder as advisory, so passing a stale one is harmless.
  if (
    !is.null(initial_dir) &&
      length(initial_dir) == 1L &&
      !is.na(initial_dir) &&
      nzchar(initial_dir)
  ) {
    args <- c(args, "-InitialDir", initial_dir)
  }

  # Deliberately no windows_hide_window: that sets SW_HIDE in the child's
  # STARTUPINFO, which is documented as hiding "the application's window" and
  # can suppress the dialog itself depending on how the host R process was
  # started - the dialog appeared under Rscript but not under RStudio. The
  # console is hidden by -WindowStyle Hidden above instead, which only ever
  # touches the console host.
  proc <- tryCatch(
    process$new("powershell", args),
    error = function(e) {
      message("[WARN] Could not start the folder dialog: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(proc)) {
    unlink(out)
    return(NULL)
  }

  message("[INFO] Folder dialog opened (pid ", proc$get_pid(), ").")
  list(proc = proc, out = out)
}

#' @export
collect_folder_dialog <- function(handle) {
  if (is.null(handle)) {
    return(list(status = "error", path = NULL))
  }

  # Read is_alive() *before* testing for the file. The other order has a race:
  # the process can write its result and exit between the two checks, which
  # would look like "died without answering" and discard a real selection.
  alive <- tryCatch(handle$proc$is_alive(), error = function(e) FALSE)

  if (file.exists(handle$out)) {
    res <- read_result(handle$out)
    unlink(handle$out)
    return(res)
  }

  if (!alive) {
    return(list(status = "error", path = NULL))
  }

  NULL
}

#' Wire a button to the native folder dialog.
#'
#' Drop-in for a shinyDirButton/shinyDirChoose pair inside a moduleServer.
#' Returns a reactive holding the most recent selection, character() until the
#' user picks something - the same shape parseDirPath() returned.
#'
#' @export
folder_picker <- function(
  input,
  session,
  id,
  title = "Select folder",
  initial_dir = function() NULL,
  on_unavailable = NULL
) {
  selected <- shiny::reactiveVal(character())
  state <- shiny::reactiveValues(handle = NULL, open = FALSE)

  shiny::observeEvent(input[[id]], {
    # Ignore a second click while a dialog is already up: the button is
    # disabled below, but a queued click can still arrive first.
    if (isTRUE(state$open)) {
      return()
    }

    start <- tryCatch(initial_dir(), error = function(e) NULL)
    handle <- start_folder_dialog(start, title)

    if (is.null(handle)) {
      message("[WARN] Could not open the folder dialog.")
      if (is.function(on_unavailable)) on_unavailable()
      return()
    }

    state$handle <- handle
    state$open <- TRUE
    disable(id)
  })

  shiny::observe({
    if (!isTRUE(state$open)) {
      return()
    }
    shiny::invalidateLater(250, session)

    res <- collect_folder_dialog(state$handle)
    if (is.null(res)) {
      return()
    }

    state$open <- FALSE
    state$handle <- NULL
    enable(id)

    if (identical(res$status, "ok")) {
      selected(res$path)
    } else if (identical(res$status, "error")) {
      message("[WARN] Folder dialog exited without returning a selection.")
      if (is.function(on_unavailable)) on_unavailable()
    }
  })

  shiny::reactive(selected())
}
