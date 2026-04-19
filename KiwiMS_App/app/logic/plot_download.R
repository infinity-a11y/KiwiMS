# app/logic/plot_download.R

box::use(
  bslib,
  htmlwidgets[saveWidget],
  plotly[as_widget, plotly_json],
  shiny,
  shinyWidgets[show_toast],
)

box::use(
  rlang[`%||%`],
)

#' @export
plot_dl_popover <- function(ns, prefix) {
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
        selected = "normal",
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
}

# Registers HTML/PNG/SVG download handlers for a plot card.
# build_fn(theme) must return a plotly figure.
# filename_fn() must return a string (no extension).
#' @export
setup_plot_dl <- function(input, output, session, prefix, build_fn, filename_fn) {
  output[[paste0("dl_", prefix, "_html")]] <- shiny::downloadHandler(
    filename = function() paste0(filename_fn(), ".html"),
    content = function(file) {
      show_toast("Exporting as HTML", text = NULL, type = "info",
        timer = 3000, timerProgressBar = TRUE)
      p <- build_fn(input[[paste0(prefix, "_dl_theme")]] %||% "light")
      saveWidget(as_widget(p), file, selfcontained = TRUE)
    }
  )

  shiny::observeEvent(input[[paste0("dl_", prefix, "_png")]], {
    show_toast("Exporting as PNG", text = NULL, type = "info",
      timer = 3000, timerProgressBar = TRUE)
    p <- build_fn(input[[paste0(prefix, "_dl_theme")]] %||% "light")
    session$sendCustomMessage("downloadPlot", list(
      json     = plotly_json(p, jsonedit = FALSE),
      format   = "png",
      quality  = input[[paste0(prefix, "_dl_quality")]] %||% "normal",
      context  = input[[paste0(prefix, "_dl_context")]] %||% "normal",
      filename = filename_fn()
    ))
  })

  shiny::observeEvent(input[[paste0("dl_", prefix, "_svg")]], {
    show_toast("Exporting as SVG", text = NULL, type = "info",
      timer = 3000, timerProgressBar = TRUE)
    p <- build_fn(input[[paste0(prefix, "_dl_theme")]] %||% "light")
    session$sendCustomMessage("downloadPlot", list(
      json     = plotly_json(p, jsonedit = FALSE),
      format   = "svg",
      quality  = input[[paste0(prefix, "_dl_quality")]] %||% "normal",
      context  = input[[paste0(prefix, "_dl_context")]] %||% "normal",
      filename = filename_fn()
    ))
  })
}
