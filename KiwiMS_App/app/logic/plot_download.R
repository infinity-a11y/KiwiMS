# app/logic/plot_download.R

box::use(
  bslib,
  htmlwidgets[saveWidget],
  openxlsx[write.xlsx],
  plotly[as_widget, plotly_json],
  shiny,
  shinyWidgets[show_toast],
)

box::use(
  rlang[`%||%`],
)

# Wraps a gear-icon popover in a "Settings" hover tooltip.
#' @export
card_settings_popover <- function(content) {
  bslib::tooltip(
    shiny::div(
      bslib::popover(
        shiny::icon("gear"),
        content,
        title = NULL
      )
    ),
    "Settings",
    placement = "top"
  )
}

#' @export
plot_dl_popover <- function(ns, prefix) {
  bslib::tooltip(
    shiny::div(
      bslib::popover(
        shiny::icon("arrow-up-from-bracket"),
        shiny::div(
          class = "plot-dl-popover",
          shiny::radioButtons(
            ns(paste0(prefix, "_dl_theme")),
            label = "Theme",
            choices = c("Dark" = "light", "Light" = "dark"),
            selected = "light",
            inline = TRUE
          ),
          shiny::radioButtons(
            ns(paste0(prefix, "_dl_quality")),
            label = "Quality (PNG)",
            choices = c("Low" = "low", "Normal" = "normal", "High" = "high"),
            selected = "normal",
            inline = TRUE
          ),
          shiny::radioButtons(
            ns(paste0(prefix, "_dl_context")),
            label = "Label Size",
            choices = c(
              "Small" = "small",
              "Medium" = "normal",
              "Large" = "large",
              "Very Large" = "xlarge"
            ),
            selected = "large",
            inline = TRUE
          ),
          shiny::div(class = "plot-dl-label", "File Format"),
          shiny::div(
            class = "plot-dl-buttons",
            shiny::downloadButton(
              ns(paste0("dl_", prefix, "_html")),
              "HTML",
              class = "btn-sm btn-default",
              icon = NULL
            ),
            shiny::actionButton(
              ns(paste0("dl_", prefix, "_png")),
              "PNG",
              class = "btn-sm btn-default"
            ),
            shiny::actionButton(
              ns(paste0("dl_", prefix, "_svg")),
              "SVG",
              class = "btn-sm btn-default"
            )
          )
        ),
        title = "Export Plot"
      )
    ),
    "Export",
    placement = "top"
  )
}

# Registers HTML/PNG/SVG download handlers for a plot card.
# build_fn(theme) must return a plotly figure.
# filename_fn() must return a string (no extension).
#' @export
setup_plot_dl <- function(
  input,
  output,
  session,
  prefix,
  build_fn,
  filename_fn,
  available_fn = NULL
) {
  if (!is.null(available_fn)) {
    shiny::observe({
      session$sendCustomMessage("setExportState", list(
        prefix = prefix,
        enabled = isTRUE(available_fn())
      ))
    })
  }
  no_plot_toast <- function() {
    shinyWidgets::show_toast(
      "No plot to export",
      text = "Generate a plot first before exporting.",
      type = "warning",
      timer = 4000,
      timerProgressBar = TRUE
    )
  }

  try_build <- function(theme) {
    tryCatch(
      shiny::isolate(build_fn(theme)),
      error = function(e) NULL
    )
  }

  output[[paste0("dl_", prefix, "_html")]] <- shiny::downloadHandler(
    filename = function() paste0(filename_fn(), ".html"),
    content = function(file) {
      p <- try_build(input[[paste0(prefix, "_dl_theme")]] %||% "light")
      if (is.null(p)) {
        no_plot_toast()
        shiny::req(FALSE)
      }
      show_toast(
        "Exporting as HTML",
        text = NULL,
        type = "info",
        timer = 3000,
        timerProgressBar = TRUE
      )
      saveWidget(as_widget(p), file, selfcontained = TRUE)
    }
  )

  shiny::observeEvent(input[[paste0("dl_", prefix, "_png")]], {
    p <- try_build(input[[paste0(prefix, "_dl_theme")]] %||% "light")
    if (is.null(p)) {
      no_plot_toast()
      return(invisible(NULL))
    }
    show_toast(
      "Exporting as PNG",
      text = NULL,
      type = "info",
      timer = 3000,
      timerProgressBar = TRUE
    )
    session$sendCustomMessage(
      "downloadPlot",
      list(
        json = plotly_json(p, jsonedit = FALSE),
        format = "png",
        quality = input[[paste0(prefix, "_dl_quality")]] %||% "normal",
        context = input[[paste0(prefix, "_dl_context")]] %||% "normal",
        filename = filename_fn()
      )
    )
  })

  shiny::observeEvent(input[[paste0("dl_", prefix, "_svg")]], {
    p <- try_build(input[[paste0(prefix, "_dl_theme")]] %||% "light")
    if (is.null(p)) {
      no_plot_toast()
      return(invisible(NULL))
    }
    show_toast(
      "Exporting as SVG",
      text = NULL,
      type = "info",
      timer = 3000,
      timerProgressBar = TRUE
    )
    session$sendCustomMessage(
      "downloadPlot",
      list(
        json = plotly_json(p, jsonedit = FALSE),
        format = "svg",
        quality = input[[paste0(prefix, "_dl_quality")]] %||% "normal",
        context = input[[paste0(prefix, "_dl_context")]] %||% "normal",
        filename = filename_fn()
      )
    )
  })
}

# Export popover for DT tables inside card headers (same icon as plot exports).
#' @export
table_dl_popover <- function(ns, prefix) {
  bslib::tooltip(
    shiny::div(
      bslib::popover(
        shiny::icon("arrow-up-from-bracket"),
        shiny::div(
          class = "plot-dl-popover",
          shiny::div(class = "plot-dl-label", "File Format"),
          shiny::div(
            class = "plot-dl-buttons",
            shiny::downloadButton(
              ns(paste0("dl_", prefix, "_csv")),
              "CSV",
              class = "btn-sm btn-default",
              icon = NULL
            ),
            shiny::downloadButton(
              ns(paste0("dl_", prefix, "_xlsx")),
              "Excel",
              class = "btn-sm btn-default",
              icon = NULL
            )
          )
        ),
        title = "Export Table"
      )
    ),
    "Export",
    placement = "top"
  )
}

# Renders CSV/Excel export buttons for a DT table.
#' @export
table_dl_buttons <- function(ns, prefix) {
  shiny::div(
    class = "table-dl-buttons",
    shiny::downloadButton(
      ns(paste0("dl_", prefix, "_csv")),
      "CSV",
      class = "btn-sm btn-default",
      icon = NULL
    ),
    shiny::downloadButton(
      ns(paste0("dl_", prefix, "_xlsx")),
      "Excel",
      class = "btn-sm btn-default",
      icon = NULL
    )
  )
}

# Prepares a hits table data.frame for export: replaces NA with "N/A" and
# drops the internal truncSample_ID column.
#' @export
prepare_hits_export <- function(table) {
  table[is.na(table)] <- "N/A"
  table[,
    !names(table) %in%
      c(
        "truncSample_ID",
        "label_color",
        "col_var",
        "trunc_label"
      )
  ]
}

# Registers CSV/Excel download handlers for a DT table.
# data_fn() must return a plain data.frame to export.
# filename_fn() must return a string (no extension).
#' @export
setup_table_dl <- function(
  input,
  output,
  session,
  prefix,
  data_fn,
  filename_fn
) {
  output[[paste0("dl_", prefix, "_csv")]] <- shiny::downloadHandler(
    filename = function() paste0(filename_fn(), ".csv"),
    content = function(file) {
      show_toast(
        "Exporting as CSV",
        text = NULL,
        type = "info",
        timer = 3000,
        timerProgressBar = TRUE
      )
      utils::write.csv(data_fn(), file, row.names = FALSE)
    }
  )

  output[[paste0("dl_", prefix, "_xlsx")]] <- shiny::downloadHandler(
    filename = function() paste0(filename_fn(), ".xlsx"),
    content = function(file) {
      show_toast(
        "Exporting as Excel",
        text = NULL,
        type = "info",
        timer = 3000,
        timerProgressBar = TRUE
      )
      write.xlsx(data_fn(), file)
    }
  )
}
