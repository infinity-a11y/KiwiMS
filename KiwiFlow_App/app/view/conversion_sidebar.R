# app/view/upload_spectra.R

box::use(
  bslib[card, card_body, card_header, sidebar],
  shiny[actionButton, fileInput, NS, textInput, moduleServer, reactive],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  sidebar(
    shinyjs::useShinyjs(),
    shiny::div(
      class = "flowchart-container",
      shiny::div(
        id = ns("module_Proteins_box"),
        class = "module-box not-done",
        "Proteins"
      ),
      shiny::div(class = "arrow"),
      shiny::div(
        id = ns("module_Compounds_box"),
        class = "module-box not-done",
        "Compounds"
      ),
      shiny::div(class = "arrow"),
      shiny::div(
        id = ns("module_Samples_box"),
        class = "module-box not-done",
        "Samples"
      ),
    ),
    shiny::hr(),
    shiny::uiOutput(ns("module_sidebar"))
  )
}

#' @export
server <- function(id, selected_tab, set_selected_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    module_status <- shiny::reactiveValues(
      proteins = FALSE,
      compounds = FALSE,
      samples = FALSE
    )

    # Check and confirm module entries
    shiny::observeEvent(input$confirm_module, {
      shiny::req(selected_tab())

      if (selected_tab() == "Proteins") {
        # Here check if proteins declared correctly
        # Set status variable accordingly

        module_status$proteins <- TRUE
        shinyjs::runjs(
          'document.querySelector(".nav-link[data-value=\'Proteins\']").classList.add("done");'
        )
        set_selected_tab("Compounds")
      } else if (selected_tab() == "Compounds") {
        module_status$proteins <- TRUE
        shinyjs::runjs(
          'document.querySelector(".nav-link[data-value=\'Compounds\']").classList.add("done");'
        )
        set_selected_tab("Samples")
      } else if (selected_tab() == "Samples") {
        module_status$proteins <- TRUE
        shinyjs::runjs(
          'document.querySelector(".nav-link[data-value=\'Samples\']").classList.add("done");'
        )
      }

      # Toggle class on button click
      id_module <- paste0("module_", selected_tab(), "_box")

      shinyjs::addClass(id = id_module, class = "done")
    })

    # Edit modules
    shiny::observeEvent(input$edit_module, {
      if (selected_tab() == "Proteins") {
        module_status$proteins <- FALSE
        shinyjs::runjs(
          'document.querySelector(".nav-link[data-value=\'Proteins\']").classList.remove("done");'
        )
      } else if (selected_tab() == "Compounds") {
        module_status$proteins <- FALSE
        shinyjs::runjs(
          'document.querySelector(".nav-link[data-value=\'Compounds\']").classList.remove("done");'
        )
      } else if (selected_tab() == "Samples") {
        module_status$proteins <- FALSE
        shinyjs::runjs(
          'document.querySelector(".nav-link[data-value=\'Samples\']").classList.remove("done");'
        )
      }

      # Toggle class on button click
      id_module <- paste0("module_", selected_tab(), "_box")

      shinyjs::removeClass(id = id_module, class = "done")
    })

    # Handle clicks on module boxes to switch tabs
    shinyjs::onclick(id = "module_Proteins_box", {
      set_selected_tab("Proteins")
    })
    shinyjs::onclick(id = "module_Compounds_box", {
      set_selected_tab("Compounds")
    })
    shinyjs::onclick(id = "module_Samples_box", {
      set_selected_tab("Samples")
    })

    # Reactive value for uploaded result list path
    shiny::reactiveValues(
      result = shiny::reactive(input$result_input$datapath)
    )

    output$module_sidebar <- shiny::renderUI({
      if (selected_tab() == "Proteins") {}

      shiny::div(
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::textOutput(ns("selected_module"))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            fileInput(
              ns("result_input"),
              "Select Results File",
              multiple = FALSE,
              accept = c(".rds")
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::actionButton(
              ns("confirm_module"),
              label = "Confirm",
              icon = shiny::icon("check")
            )
          ),
          shiny::column(
            width = 6,
            shiny::actionButton(
              ns("edit_module"),
              label = "Edit",
              icon = shiny::icon("pen-to-square")
            )
          )
        )
      )
    })

    output$selected_module <- shiny::renderText({
      shiny::req(selected_tab())

      paste(selected_tab(), "Declaration")
    })
  })
}
