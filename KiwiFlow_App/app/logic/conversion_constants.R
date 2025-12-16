# app/logic/conversion_constants.R

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

# Time unit choices
#' @export
time_unit_input_ui <- function(ns) {
  shinyWidgets::pickerInput(
    inputId = ns("time_unit"),
    label = "Time Unit",
    choices = c("s", "min"),
    choicesOpt = list(
      content = {
        w_col1 <- "45px"
        w_col2 <- "95px"
        units <- c("s", "min")
        names <- c("seconds", "minutes")
        sprintf(
          paste0(
            "<span style='display: inline-block; width: %s; font-weight: 700; text-align: left;'>%s</span>",
            "<span style='display: inline-block; width: %s; font-style: italic; text-align: right;'>%s</span>"
          ),
          w_col1,
          units,
          w_col2,
          names
        )
      }
    ),
    options = shinyWidgets::pickerOptions(
      size = 10,
      showContent = FALSE,
      alignRight = TRUE
    )
  )
}

# Concentration unit choices
#' @export
conc_unit_input_ui <- function(ns) {
  shinyWidgets::pickerInput(
    inputId = ns("conc_unit"),
    label = "Conc. Unit",
    choices = c("M", "mM", "μM", "nM", "pM"),
    choicesOpt = list(
      content = {
        w_col1 <- "50px"
        w_col2 <- "95px"
        w_col3 <- "40px"
        units <- c("M", "mM", "μM", "nM", "pM")
        names <- c(
          "molar",
          "millimolar",
          "micromolar",
          "nanomolar",
          "picomolar"
        )
        powers <- c("10⁰", "10⁻³", "10⁻⁶", "10⁻⁹", "10⁻¹²")

        sprintf(
          paste0(
            "<span style='display: inline-block; width: %s; font-weight: 700;'>%s</span>",
            "<span style='display: inline-block; width: %s; font-style: italic;'>%s</span>",
            "<span style='display: inline-block; width: %s; text-align: right;'>%s</span>"
          ),
          w_col1,
          units,
          w_col2,
          names,
          w_col3,
          powers
        )
      }
    ),
    options = shinyWidgets::pickerOptions(
      size = 10,
      showContent = FALSE,
      alignRight = TRUE
    )
  )
}

# Empty protein declaration table
#' @export
empty_protein_table <- data.frame(
  name = as.character(rep(NA, 9)),
  rep(list(as.numeric(rep(NA, 9))), 9)
) |>
  stats::setNames(c("Protein", paste("Mass", 1:9)))

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

# Sample table legend UI
#' @export
sample_table_legend <- shiny::div(
  class = "table-legend",
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell duplicated-names"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= duplicated compounds"
    )
  ),
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell numeric-mass"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= unknown name"
    )
  ),
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell duplicated-mass"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= protein contains duplicated compound masses (proximity < peak tolerance)"
    )
  )
)
