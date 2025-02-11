# app/view/deconvolution_sidebar.R

box::use(
  fs[path_home],
  shiny[column, div, fluidRow, h6, NS, moduleServer, reactive, reactiveValues],
  shinyFiles[parseDirPath, shinyDirButton, shinyDirChoose],
  bslib[card, card_body, card_header, sidebar],
)

box::use(
  app/logic/helper_functions[collapsiblePanelUI],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  sidebar(
    title = "File Upload",
    shinyDirButton(
      ns("folder"),
      "Select folder(s)",
      icon = shiny::icon("folder-open"),
      title = "Select Waters .raw Folder(s)",
      buttonType = "default",
      root = path_home()
    ),
    shiny::verbatimTextOutput(ns("path_selected")),
    card(
      card_header(
        class = "bg-dark",
        "Charge state [z]"
      ),
      card_body(
        fluidRow(
          column(
            width = 3,
            h6("Low", style = "margin-top: 8px;")
          ),
          column(
            width = 3,
            div(
              class = "deconv-param-input",
              shiny::numericInput(
                ns("startz"),
                "",
                min = 0,
                max = 100,
                value = 1,
                width = "130%"
              ) 
            )
          ),
          column(
            width = 3,
            h6("High", style = "margin-top: 8px;")
          ),
          column(
            width = 3,
            div(
              class = "deconv-param-input",
              shiny::numericInput(
                ns("endz"),
                "",
                min = 0,
                max = 100,
                value = 50,
                width = "130%"
              )
            )
          )
        )
      )
    ),
    card(
      card_header(
        class = "bg-dark",
        "Spectrum range [m/z]"
      ),
      card_body(
        fluidRow(
          column(
            width = 3,
            h6("Low", style = "margin-top: 8px;")
          ),
          column(
            width = 3,
            div(
              class = "deconv-param-input",
              shiny::numericInput(
                ns("minmz"),
                "",
                min = 0,
                max = 100000,
                value = 710,
                width = "130%"
              )
            )
          ),
          column(
            width = 3,
            h6("High", style = "margin-top: 8px;")
          ),
          column(
            width = 3,
            div(
              class = "deconv-param-input",
              shiny::numericInput(
                ns("maxmz"),
                "",
                min = 0,
                max = 100000,
                value = 1100,
                width = "130%"
              )
            )
          )
        )
      )
    ),
    card(
      card_header(
        class = "bg-dark",
        "Mass range [Mw]"
      ),
      card_body(
        fluidRow(
          column(
            width = 3,
            h6("Low", style = "margin-top: 8px;")
          ),
          column(
            width = 3,
            div(
              class = "deconv-param-input",
              shiny::numericInput(
                ns("masslb"),
                "",
                min = 0,
                max = 100000,
                value = 35000,
                width = "130%"
              )
            )
          ),
          column(
            width = 3,
            h6("High", style = "margin-top: 8px;")
          ),
          column(
            width = 3,
            div(
              class = "deconv-param-input",
              shiny::numericInput(
                ns("massub"),
                "",
                min = 0,
                max = 100000,
                value = 42000,
                width = "130%"
              )
            )
          )
        )
      )
    ),
    collapsiblePanelUI(
      "advanced_params",
      "Advanced Parameter Settings",
      fluidRow(
        column(
          width = 12,
          card(
            card_header(
              class = "bg-dark",
              "Mass range"
            ),
            card_body(
              fluidRow(
                column(
                  width = 5,
                  h6("Bin size", style = "margin-top: 8px;")
                ),
                column(
                  width = 5,
                  div(
                    class = "deconv-param-input-adv",
                    shiny::numericInput(
                      ns("massbins"),
                      "",
                      min = 0,
                      max = 100,
                      value = 0.5,
                      step = 0.1,
                      width = "130%"
                    )
                  )
                )
              )
            )
          ),
          card(
            card_header(
              class = "bg-dark",
              "Peak parameters"
            ),
            card_body(
              fluidRow(
                column(
                  width = 5,
                  h6("Threshold", style = "margin-top: 8px;")
                ),
                column(
                  width = 5,
                  div(
                    class = "deconv-param-input-adv",
                    shiny::numericInput(
                      ns("peakthresh"),
                      "",
                      min = 0,
                      max = 1,
                      value = 0.07,
                      width = "130%",
                      step = 0.01
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 5,
                  h6("Window", style = "margin-top: 8px;")
                ),
                column(
                  width = 5,
                  div(
                   class = "deconv-param-input-adv",
                   shiny::numericInput(
                     ns("peakwindow"),
                     "",
                     min = 0,
                     max = 1000,
                     value = 40,
                     width = "130%"
                   )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 5,
                  h6("Norm", style = "margin-top: 8px;")
                ),
                column(
                  width = 5,
                  div(
                    class = "deconv-param-input-adv",
                    shiny::numericInput(
                      ns("peaknorm"),
                      "",
                      min = 0,
                      max = 100,
                      value = 2,
                      width = "130%"
                    )
                  )
                )
              )
            )
          ),
          card(
            card_header(
              class = "bg-dark",
              "Retention time [min]"
            ),
            card_body(
              fluidRow(
                column(
                  width = 3,
                  h6("Start", style = "margin-top: 8px;")
                ),
                column(
                  width = 3,
                  div(
                    class = "deconv-param-input-time",
                    shiny::numericInput(
                      ns("peakthresh"),
                      "",
                      min = 0,
                      max = 100,
                      value = 1,
                      width = "130%",
                      step = 0.05
                    )
                  )
                ),
                column(
                  width = 3,
                  h6("End", style = "margin-top: 8px;")
                ),
                column(
                  width = 3,
                  div(
                    class = "deconv-param-input-time",
                    shiny::numericInput(
                      ns("time_end"),
                      "",
                      min = 0,
                      max = 100,
                      value = 1.35,
                      width = "130%",
                      step = 0.05
                    )
                  )
                )
              )
            )
          ) 
        )    
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Define roots for directory browsing
    roots <- c(
      Home = path_home(),
      C = "C:/",
      D = "D:/" 
    )
    
    # Initialize directory selection
    shinyDirChoose(
      input,
      id = "folder",
      roots = roots,
      defaultRoot = "Home",
      session = session
    )
    
    waters_dir <- reactive({
      shiny::validate(shiny::need(input$folder, "Nothing selected"))
      parseDirPath(roots, input$folder)
    })
    
    output$path_selected <- shiny::renderPrint({
      if (!is.null(waters_dir()) && length(waters_dir()) > 0) {
        waters_dir()
      } else {
        cat("Nothing selected")
      }
    })
    
    reactiveValues(
      dir = waters_dir,
      config_startz = reactive(input$startz),
      config_endz = reactive(input$endz),
      config_minmz = reactive(input$minmz),
      config_maxmz = reactive(input$maxmz)
    )
  })
}
