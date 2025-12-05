# app/logic/conversion_constants.R

# empty data frame for protein and compound input tables
#' @export
empty_tab <- data.frame(
  name = as.character(rep(NA, 9)),
  mass_shift1 = as.numeric(rep(NA, 9)),
  mass_shift3 = as.numeric(rep(NA, 9)),
  mass_shift3 = as.numeric(rep(NA, 9)),
  mass_shift4 = as.numeric(rep(NA, 9)),
  mass_shift5 = as.numeric(rep(NA, 9)),
  mass_shift6 = as.numeric(rep(NA, 9)),
  mass_shift7 = as.numeric(rep(NA, 9)),
  mass_shift8 = as.numeric(rep(NA, 9)),
  mass_shift9 = as.numeric(rep(NA, 9))
)

# Warning symbol
#' @export
warning_sym <- "\u26A0"

# Custom symbols for plotly plots
#' @export
symbols <- c(
  "circle",
  "triangle-up",
  "square",
  "cross-thin-open",
  "square-x-open",
  "asterisk-open",
  "diamond",
  "triangle-down",
  "square",
  "x",
  "hexagram",
  "hourglass"
)

# keybind menu ui
#' @export
keybind_menu_ui <- shiny::div(
  class = "shortcut-bar",
  shiny::div(
    class = "shortcut-item",
    shiny::span(class = "key", "Ctrl"),
    " + ",
    shiny::span(class = "key", "C"),
    " Copy"
  ),
  shiny::div(
    class = "shortcut-item",
    shiny::span(class = "key", "Ctrl"),
    " + ",
    shiny::span(class = "key", "V"),
    " Paste"
  ),
  shiny::div(
    class = "shortcut-item",
    shiny::span(class = "key", "Ctrl"),
    " + ",
    shiny::span(class = "key", "X"),
    " Cut"
  ),
  shiny::div(
    class = "shortcut-item",
    shiny::span(class = "key", "←"),
    shiny::span(class = "key", "↑"),
    shiny::span(class = "key", "→"),
    shiny::span(class = "key", "↓"),
    " Move"
  ),
  shiny::div(
    class = "shortcut-item",
    shiny::span(class = "key", "Tab"),
    " Move Columns"
  ),
  shiny::div(
    class = "shortcut-item",
    shiny::span(class = "key", "Enter"),
    " Move Rows"
  )
)

# Table Legend UI
#' @export
table_legend <- shiny::div(
  class = "table-legend",
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell duplicated-names"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= duplicated names"
    )
  ),
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell numeric-mass"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= non-numeric mass values"
    )
  ),
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell duplicated-mass"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= mass shifts of one protein duplicated (proximity < peak tolerance)"
    )
  ),
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell duplicated-mass-between"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= mass shifts duplicated between different proteins (proximity < peak tolerance)"
    )
  )
)
