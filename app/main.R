# app/main.R

box::use(
  bsicons[bs_icon],
  bslib,
  shiny[div, moduleServer, NS, reactive, stopApp, tagList, tags],
  shinyjs[useShinyjs],
  waiter[spin_fading_circles, useWaiter, waiterShowOnLoad, waiter_hide],
)

box::use(
  app/logic/dev_utils,
  app/view/conversion_main,
  app/view/conversion_sidebar,
  app/view/deconvolution_process,
  app/view/deconvolution_results,
  app/view/deconvolution_sidebar,
  app/view/ki_kinact_sidebar,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    dev_utils$add_dev_headers(),
    div(id = "blocking-overlay"),
    useWaiter(),
    waiterShowOnLoad(html = waiter::spin_orbit()),
    useShinyjs(),
    bslib$page_navbar(
      title = tags$div(
        tags$img(src = "static/logo.svg", height = "37rem",
                 style = "margin-right: 5px;"),
        tags$span("KiwiFlow",
                  style = "font-size: 1em;top: 2px; position: relative;")
      ),
      window_title = "KiwiFlow 0.1.0",
      underline = TRUE,
      bslib$nav_panel(title = "Deconvolution",
                      bslib$page_sidebar(
                        sidebar = deconvolution_sidebar$ui(
                          ns("deconvolution_pars")),
                        bslib$navset_card_tab(
                          bslib$nav_panel(
                            title = "Deconvolution",
                            deconvolution_process$ui(
                              ns("deconvolution_process"))),
                          bslib$nav_panel(
                            title = "Mass Spectra",
                            deconvolution_results$ui(
                              ns("deconvolution_plot")))))),
      bslib$nav_panel(title = "Mass Conversion",
                      bslib$page_sidebar(
                        sidebar = conversion_sidebar$ui(ns("mass_conversion")),
                        bslib$card(bslib$card_header("Conversion Table"),
                                   conversion_main$ui(ns("conversion_card"))))),
      bslib$ nav_panel(title = "KI/Kinact",
                       bslib$page_sidebar(
                         sidebar = ki_kinact_sidebar$ui(ns("ki")),
                         bslib$navset_card_tab(
                           bslib$nav_panel(title = "Kobs Table"),
                           bslib$nav_panel(title = "Kinact Table")))),
      bslib$nav_spacer(),
      bslib$nav_menu(
        title = "Links",
        align = "right",
        icon = bs_icon("link-45deg", size = "1rem"),
        bslib$nav_item(
          tags$a(
            tags$span(
              tags$i(class = "fa-brands fa-github me-1"),
              "GitHub"
            ),
            href = "https://github.com/infinity-a11y/MSFlow",
            target = "_blank",
            class = "nav-link"
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Kill server on session end
    session$onSessionEnded(function() {
      stopApp()
    })

    # initiate module servers
    conversion_main$server("conversion_card")

    # transfer selected directories paths
    dirs <- deconvolution_sidebar$server("deconvolution_pars")
    deconvolution_process$server("deconvolution_process", dirs)

    waiter_hide()
  })
}
