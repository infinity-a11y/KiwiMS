# app/logic/session_restore.R

# The work of a browser tab, kept across a dropped connection.
#
# A Shiny session ends with its connection: when the browser drops it (a tab
# put to sleep, standby), the page reloads into a new session that would start
# from scratch. The R process keeps running (see `connections` in main.R), so
# each session parks its state here when it ends, under the token of its
# browser tab (static/js/connection.js), and the session the same tab opens
# next takes it back - the loaded results, declarations and the place in the
# app, without computing anything again. The state is held in memory, by
# reference: nothing is copied or written.

box::use(
  shiny,
)

# token -> list(time, state)
parked <- new.env()

# token -> environment(session, park, retired): the sessions still connected
live <- new.env()

# A state nobody came back for is dropped after this long, and only the most
# recent few are kept: each can hold a whole analysis
park_hours <- 12
park_max <- 4

prune_parked <- function() {
  tokens <- ls(parked)
  if (!length(tokens)) {
    return(invisible(NULL))
  }
  times <- vapply(tokens, function(t) as.numeric(parked[[t]]$time), numeric(1))
  old <- tokens[as.numeric(Sys.time()) - times > park_hours * 3600]
  surplus <- tokens[order(times, decreasing = TRUE)][-seq_len(park_max)]
  drop <- unique(c(old, surplus[!is.na(surplus)]))
  if (length(drop)) {
    rm(list = drop, envir = parked)
  }
  invisible(NULL)
}

# Token of the browser tab, from the page address (?tab=, set by
# connection.js before the page connects); NULL without a valid one
#' @export
tab_token <- function(session) {
  search <- shiny$isolate(session$clientData$url_search)
  token <- shiny$parseQueryString(if (is.null(search)) "" else search)$tab
  if (is.null(token) || !grepl("^[A-Za-z0-9-]{8,64}$", token)) {
    return(NULL)
  }
  token
}

#' @export
park_state <- function(token, state) {
  if (is.null(token) || is.null(state)) {
    return(invisible(NULL))
  }
  parked[[token]] <- list(time = Sys.time(), state = state)
  prune_parked()
  invisible(NULL)
}

# The state parked for the tab, once: it is removed as it is taken. A session
# of the tab still counted as connected is the page being replaced - its
# connection died without the server noticing (standby) - so its state is
# taken from it, and it is closed.
# With `orphan`, a page without state of its own takes the most recent one
# parked: KiwiMS opened again (a new tab) while no other page is open, after
# its tab was lost without being closed - a crashed or discarded browser tab.
# A closed tab leaves nothing behind, as the server stops soon after.
#' @export
take_state <- function(token, orphan = FALSE) {
  if (is.null(token)) {
    return(NULL)
  }
  old <- live[[token]]
  if (!is.null(old)) {
    old$retired <- TRUE
    rm(list = token, envir = live)
    park_state(token, tryCatch(old$park(), error = function(e) NULL))
    try(old$session$close(), silent = TRUE)
  }
  prune_parked()
  key <- token
  if (is.null(parked[[key]]) && isTRUE(orphan) && length(ls(parked))) {
    tokens <- ls(parked)
    times <- vapply(
      tokens,
      function(t) as.numeric(parked[[t]]$time),
      numeric(1)
    )
    key <- tokens[which.max(times)]
  }
  entry <- parked[[key]]
  if (!is.null(entry)) {
    rm(list = key, envir = parked)
  }
  entry$state
}

# Registers a connected session of the tab. `park` returns its state. The
# returned handle tells the session, when it ends, whether to park its state
# itself (retired: it was taken by take_state() already).
#' @export
register_live <- function(token, session, park) {
  handle <- new.env()
  handle$session <- session
  handle$park <- park
  handle$retired <- FALSE
  if (!is.null(token)) {
    live[[token]] <- handle
  }
  handle
}

#' @export
unregister_live <- function(token, handle) {
  if (!is.null(token) && identical(live[[token]], handle)) {
    rm(list = token, envir = live)
  }
  invisible(NULL)
}

# Runs `fn` once, as soon as the input `id` has a value - i.e. once the
# browser has bound it. For the parts of a restored state that can only be
# applied to an element on the page (a disabled picker, a button label).
#' @export
when_bound <- function(input, id, fn) {
  observer <- shiny$observe({
    shiny$req(!is.null(input[[id]]))
    shiny$isolate(fn())
    observer$destroy()
  })
  invisible(observer)
}
