# app/logic/help_modal.R
#
# Building blocks of the Help modals. Every Help button of the app opens one of
# these, so they share one layout: a page-like dialog that is as tall as its
# content and never taller than the window, with sections, definition lists,
# notes and display formulas styled alike (.help-modal in main.scss). The
# content of the modals lives in help_pages.R.
#
# Formulas are plain HTML (fraction bars, sub- and superscripts) rather than
# MathJax: shiny::withMathJax() loads MathJax from a CDN, and KiwiMS runs on
# lab PCs that are often offline, where the TeX source would show instead.

# Modal ----

# The dialog itself. `...` is its content.
#' @export
help_modal <- function(title, ...) {
  shiny::div(
    class = "help-modal",
    shiny::modalDialog(
      shiny::div(class = "help-modal-body", ...),
      title = title,
      easyClose = TRUE,
      footer = shiny::modalButton("Dismiss")
    )
  )
}

# Show the help page `id` of `pages` whenever the input of the same name fires.
# `pages` maps input ids to functions returning a help_modal(); several ids can
# share one function. Called once per module server.
#' @export
bind_help <- function(input, pages) {
  lapply(names(pages), function(id) {
    shiny::observeEvent(
      input[[id]],
      shiny::showModal(pages[[id]]()),
      ignoreInit = TRUE
    )
  })
  invisible(NULL)
}

# Content ----

# A titled section of a modal
#' @export
help_section <- function(heading, ...) {
  shiny::div(
    class = "help-section",
    shiny::h5(class = "help-heading", heading),
    ...
  )
}

# Term / description pairs, e.g. the columns of a table
#' @export
help_defs <- function(...) {
  htmltools::tags$dl(class = "help-defs", ...)
}

#' @export
help_def <- function(term, ...) {
  htmltools::tagList(
    htmltools::tags$dt(term),
    htmltools::tags$dd(...)
  )
}

# A highlighted remark: a caveat or a practical tip
#' @export
help_note <- function(...) {
  shiny::div(class = "help-note", ...)
}

# Link to external documentation, opened in the browser
#' @export
help_link <- function(href, label) {
  shiny::div(
    class = "help-link",
    shiny::a(
      href = href,
      target = "_blank",
      shiny::icon("arrow-up-right-from-square"),
      label
    )
  )
}

# Formulas ----

# A displayed (centred) formula
#' @export
help_formula <- function(...) {
  shiny::div(class = "help-formula", shiny::span(class = "math", ...))
}

# Inline formula within running text
#' @export
math <- function(...) {
  shiny::span(class = "math", ..., .noWS = "outside")
}

# A variable: italic symbol with an upright descriptive subscript, e.g.
# mvar("k", "obs") for k_obs
#' @export
mvar <- function(symbol, sub = NULL, sup = NULL) {
  shiny::span(
    class = "math-var",
    .noWS = "outside",
    htmltools::tags$i(symbol, .noWS = "outside"),
    if (!is.null(sub)) htmltools::tags$sub(sub, .noWS = "outside"),
    if (!is.null(sup)) htmltools::tags$sup(sup, .noWS = "outside")
  )
}

# A fraction with a horizontal bar
#' @export
frac <- function(num, den) {
  shiny::span(
    class = "math-frac",
    .noWS = "outside",
    shiny::span(class = "math-num", num),
    shiny::span(class = "math-den", den)
  )
}

# A binary operator or relation with the spacing of typeset math
#' @export
op <- function(symbol) {
  shiny::span(class = "math-op", symbol)
}

# A reaction arrow with its rate or equilibrium constant above it
#' @export
rxn <- function(arrow, label) {
  shiny::span(
    class = "math-rxn",
    shiny::span(class = "math-rxn-label", label),
    shiny::span(class = "math-rxn-arrow", arrow)
  )
}

# Symbols used throughout the kinetics pages
#' @export
k_obs <- function() mvar("k", "obs")

#' @export
k_inact <- function() mvar("k", "inact")

#' @export
K_i <- function() mvar("K", "i")

# [C], the compound concentration
#' @export
conc_c <- function() {
  shiny::span(
    class = "math-var",
    .noWS = "outside",
    "[",
    htmltools::tags$i("C", .noWS = "outside"),
    "]"
  )
}
