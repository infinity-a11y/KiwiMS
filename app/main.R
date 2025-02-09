box::use(
  bsicons[bs_icon],
  bslib,
  shiny[div, moduleServer, NS, tags, tagList, reactive, renderPrint, 
        verbatimTextOutput],
  shinyjs[useShinyjs],
)

box::use(
  app/view/deconvolution_process,
  app/view/deconvolution_results,
  app/view/deconvolution_sidebar,
  app/view/conversion_main,
  app/view/conversion_sidebar,
  app/view/ki_kinact_sidebar,
  app/logic/dev_utils,
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    dev_utils$add_dev_headers(),
    div(id = "blocking-overlay"),
    useShinyjs(),
    bslib$page_navbar(
      title = "MSFlow 0.0.1",
      bg = "#35357A",
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
        icon = bs_icon("link-45deg", size = "1rem",),
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
    
    # initiate module servers
    conversion_main$server("conversion_card")
    
    # transfer selected waters dir path
    waters_dir <- deconvolution_sidebar$server("deconvolution_pars")
    deconvolution_process$server("deconvolution_process", reactive(waters_dir()))
    deconvolution_results$server("deconvolution_plot", reactive(waters_dir()))
  })
}
