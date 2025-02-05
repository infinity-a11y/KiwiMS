box::use(
  bslib,
  shiny[moduleServer, NS, tags, tagList, reactive, renderPrint, 
        verbatimTextOutput],
)

box::use(
  app/view/upload_spectra,
  app/view/ki_kinact,
  app/view/conversion_card,
  app/view/deconvolution_card,
  app/view/deconvolution_sidebar,
  app/logic/dev_utils,
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    dev_utils$add_dev_headers(),
    bslib$page_navbar(
      title = "MSFlow 0.0.1",
      bg = "#35357A",
      underline = TRUE,
      bslib$nav_panel(title = "Deconvolution", 
                      bslib$page_sidebar(
                        sidebar = deconvolution_sidebar$ui(
                          ns("deconvolution_pars")),
                        bslib$card(bslib$card_header("Mass Spectra"),
                                   # verbatimTextOutput(ns("test")),
                                   deconvolution_card$ui(ns("deconvolution_plot"))))),
      bslib$nav_panel(title = "Mass Conversion", 
                      bslib$page_sidebar(
                        sidebar = upload_spectra$ui(ns("mass_conversion")),
                        bslib$card(bslib$card_header("Conversion Table"),
                                   conversion_card$ui(ns("conversion_card"))))),
      bslib$ nav_panel(title = "KI/Kinact",
                       bslib$page_sidebar(
                         sidebar = ki_kinact$ui(ns("ki")),
                         bslib$navset_card_tab(
                           bslib$nav_panel(title = "Kobs Table"),
                           bslib$nav_panel(title = "Kinact Table")))),
      bslib$nav_spacer(),
      bslib$nav_menu(
        title = "Links",
        align = "right"
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    conversion_card$server("conversion_card")
    
    waters_dir <- deconvolution_sidebar$server("deconvolution_pars")
    
    deconvolution_card$server("deconvolution_plot", reactive(waters_dir()))
  })
}
