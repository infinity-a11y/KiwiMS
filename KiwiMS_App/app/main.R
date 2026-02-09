# app/main.R

box::use(
  bslib,
  shiny,
  shinyjs[disable, enable, hide, hidden, show, runjs, useShinyjs],
  waiter[useWaiter, waiter_hide, waiterShowOnLoad],
)

box::use(
  app / logic / dev_utils,
  app / view / conversion_main,
  app / view / conversion_sidebar,
  app / view / deconvolution_main,
  app / view / deconvolution_sidebar,
  app / view / log_view,
  app / view / log_sidebar,
  app / logic / logging[start_logging, write_log, close_logging],
  app /
    logic /
    helper_functions[
      check_github_version,
      get_kiwims_version,
      get_latest_release_url,
    ],
  app / logic / conversion_constants[gpl3_licence, ]
)

suppressWarnings(library(logr))

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    dev_utils$add_dev_headers(),
    shiny$div(id = "blocking-overlay"),
    useWaiter(),
    waiterShowOnLoad(
      html = shiny$tags$div(
        style = "text-align: center;",
        shiny$tags$img(
          src = "static/logo_animated.svg",
          width = "400px",
          height = "400px"
        ),
        shiny$tags$div(
          style = paste0(
            "font-family: monospace; font-size: 50px; color: blac",
            "k; opacity: 0; animation: fadeIn 1s ease-in forwards",
            "; animation-delay: 1s;"
          ),
          "KiwiMS"
        )
      )
    ),
    useShinyjs(),
    bslib$page_navbar(
      id = ns("tabs"),
      title = shiny$tags$div(
        shiny$tags$img(
          src = "static/logo.svg",
          height = "42rem",
          style = "margin-right: 5px; margin-top: -2px"
        ),
        shiny$tags$span(
          "KiwiMS",
          style = "font-size: 21px; font-family: monospace;"
        )
      ),
      window_title = paste("KiwiMS", get_kiwims_version()["version"]),
      navbar_options = bslib$navbar_options(underline = TRUE),
      bslib$nav_panel(
        title = "Deconvolution",
        bslib$page_sidebar(
          sidebar = deconvolution_sidebar$ui(
            ns("deconvolution_pars")
          ),
          deconvolution_main$ui(
            ns("deconvolution_main")
          )
        )
      ),
      bslib$nav_panel(
        title = "Protein Conversion",
        bslib$page_sidebar(
          sidebar = conversion_sidebar$ui(ns("conversion_sidebar")),
          conversion_main$ui(ns("conversion_main"))
        )
      ),
      bslib$nav_panel(
        title = "Logs",
        icon = shiny::icon("list-check"),
        bslib$page_sidebar(
          sidebar = log_sidebar$ui(ns("log_sidebar")),
          bslib$card(
            log_view$ui(ns("logs"))
          )
        )
      ),
      bslib$nav_spacer(),
      bslib$nav_item(
        shiny::actionButton(
          ns("settings"),
          "Settings",
          icon = shiny::icon("gear"),
          class = "nav-link"
        )
      ),
      bslib$nav_item(
        shiny::actionButton(
          ns("licence"),
          "License",
          icon = shiny::icon("info"),
          class = "nav-link"
        )
      ),
      bslib$nav_item(shiny::uiOutput(
        ns("update_button"),
        class = "nav-link",
        style = "cursor: pointer;",
        onclick = "Shiny.setInputValue('app-open_update_modal', Math.random());"
      )),
      bslib$nav_item(
        shiny::tags$a(
          id = "unidec-tag",
          style = "cursor: pointer;",
          onclick = "Shiny.setInputValue('app-unidec_click', Math.random());",
          shiny::tags$img(
            src = "static/UniDec.png",
            width = "auto",
            style = "    top: -1px;
    position: relative;",
            height = "18px"
          ),
          "UniDec"
        )
      ),
      bslib$nav_menu(
        title = "Links",
        align = "right",
        icon = shiny$icon("link"),
        bslib$nav_item(
          shiny$tags$a(
            shiny$tags$span(
              shiny$tags$i(class = "fa-brands fa-github me-1"),
              "KiwiMS GitHub"
            ),
            href = "https://github.com/infinity-a11y/MSFlow",
            target = "_blank",
            class = "nav-link"
          )
        ),
        bslib$nav_item(
          shiny$tags$a(
            shiny$tags$span(
              shiny$tags$i(class = "fa-brands fa-github me-1"),
              "UniDec GitHub"
            ),
            href = "https://github.com/michaelmarty/UniDec",
            target = "_blank",
            class = "nav-link"
          )
        ),
        bslib$nav_item(
          shiny$tags$a(
            shiny$tags$span(
              shiny$tags$img(
                src = "static/liora_logo.png",
                style = "height: 1em; margin-right: 5px;"
              ),
              "Liora Bioinformatics"
            ),
            href = "https://www.liora-bioinformatics.com",
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
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Kill server on session end
    session$onSessionEnded(function() {
      write_log("Session closed")
      shiny$stopApp()
    })

    shiny::observeEvent(input$quit_kiwims, {
      shiny::stopApp() # This signals the mother process that the app is done
    })

    # Initiate logging
    start_logging()
    write_log("Session started")

    # Log view server
    active_tab_reactive <- shiny$reactive({
      input$tabs
    })
    log_buttons <- log_sidebar$server("log_sidebar")
    log_view$server("logs", active_tab_reactive, log_buttons)

    reset_button <- shiny$reactiveVal(0)

    # Deconvolution sidebar server
    deconvolution_sidebar_vars <- deconvolution_sidebar$server(
      "deconvolution_pars",
      reset_button = reset_button
    )

    # Deconvolution process server
    deconvolution_main_vars <- deconvolution_main$server(
      "deconvolution_main",
      deconvolution_sidebar_vars,
      conversion_main_vars,
      reset_button = reset_button
    )

    # Conversion sidebar server
    conversion_sidebar_vars <- conversion_sidebar$server(
      "conversion_sidebar",
      conversion_main_vars,
      deconvolution_main_vars
    )

    # Conversion main server
    conversion_main_vars <- conversion_main$server(
      "conversion_main",
      conversion_sidebar_vars,
      deconvolution_main_vars
    )

    # Check update availability
    version_info <- readLines("resources/version.txt", warn = FALSE)

    local_version <- sub(".*=", "", version_info[1])
    release <- sub(".*=", "", version_info[2])
    url <- sub(".*=", "", version_info[3])
    remote_version <- sub(".*=", "", check_github_version())

    if (identical(local_version, remote_version)) {
      # Variables for modal
      message <- "KiwiMS is up-to-date"
      hint <- "No action needed. Update anyway?"
      release_url <- get_latest_release_url()
      link <- ifelse(
        is.null(release_url),
        "https://github.com/infinity-a11y/KiwiMS/tree/master",
        release_url
      )

      # Variables for button
      icon <- shiny$icon("circle-info")
      label <- "Version"

      write_log(paste("KiWiFlow Version", local_version, "-", message))
    } else {
      # Variables for modal
      message <- "Update available"
      hint <- paste(
        "Download the latest version <strong>",
        remote_version,
        "</strong>from the release page:"
      )
      release_url <- get_latest_release_url()
      link <- ifelse(
        is.null(release_url),
        "https://github.com/infinity-a11y/KiwiMS/tree/master",
        release_url
      )

      # Variables for button
      icon <- shiny$icon("circle-exclamation")
      label <- "Update"

      write_log(paste("KiWiFlow Version", local_version, "-", message))
    }

    output$update_button <- shiny$renderUI({
      shiny$req(icon, label)

      shiny::tags$a(
        icon,
        label
      )

      # shiny$actionButton(
      #   inputId = ns("open_update_modal"),
      #   label = label,
      #   icon = icon,
      #   class = "nav-link"
      # )
    })

    # Switch Protein Conversion tab when user forwards
    shiny$observeEvent(deconvolution_main_vars$forward_deconvolution(), {
      bslib::nav_select(
        "tabs",
        session = session,
        "Protein Conversion"
      )
    })

    # Switch back to Deconvolution module when user forwards
    shiny$observeEvent(conversion_main_vars$cancel_continuation(), {
      bslib::nav_select(
        "tabs",
        session = session,
        "Deconvolution"
      )
    })

    # Licence Modal Window ----
    shiny::observeEvent(input$licence, {
      shiny$showModal(
        shiny$div(
          class = "unidec-modal",
          shiny$modalDialog(
            title = "End-User License Agreement (GPL v3)",
            size = "l",
            easyClose = TRUE,
            shiny$div(
              style = "font-size: 14px;",
              shiny$tags$p(
                "KiwiMS is released under the following license:"
              ),
              shiny$tags$pre(
                style = "height: 400px; overflow-y: scroll; background-color: #f8f9fa; 
                 font-size: 11px; padding: 30px; border: 1px solid #ddd; width: fit-content; margin: 0; justify-self: center;",
                shiny::HTML(gpl3_licence)
              )
            ),
            footer = shiny$tagList(
              shiny$modalButton("Close")
            )
          )
        )
      )
    })

    # Unidec Modal Window ----
    shiny::observeEvent(input$unidec_click, {
      shiny$showModal(
        shiny$div(
          class = "unidec-modal",
          shiny$modalDialog(
            title = shiny$span(
              shiny$icon("info-circle"),
              "UniDec - Acknowledgement"
            ),
            size = "l",
            easyClose = TRUE,

            shiny$div(
              style = "font-size: 14px;",
              shiny::HTML(
                '
        <p>The deconvolution and peak picking algorithms within this software are powered by 
        <b>UniDec</b> - Universal Deconvolution of Mass and Ion Mobility Spectra (<a href="https://github.com/michaelmarty/UniDec" target="_blank">github.com/michaelmarty/UniDec</a>).</p>
        
        <p>We gratefully acknowledge the work of <b>Marty et al.</b> in developing these 
        Bayesian deconvolution methods.</p>
        
        <hr style="margin: 1rem 0;">
        
        <h5 style="color: #2c3e50;">Citation Request</h4>
        <p>If you utilize the deconvolution or peak picking results from this software in 
        your research or publications, the authors of UniDec request that you cite their original paper:</p>
        
        <div style="background-color: #f8f9fa; padding: 15px; border-left: 5px solid #007bff; margin-bottom: 10px;">
          M. T. Marty, A. J. Baldwin, E. G. Marklund, G. K. A. Hochberg, J. L. P. Benesch, C. V. Robinson. 
          <br><b>"UniDec: Universal Deconvolution of Mass and Ion Mobility Spectra."</b> 
          <br><i>Anal. Chem.</i> 2015, 87, 4370-4376.
        </div>
      '
              ),
              shiny::div(
                shiny$tags$textarea(
                  id = "bibtex_unidec",
                  readonly = "readonly",
                  style = "width: 100%; height: 140px; font-family: monospace; font-size: 12px; 
             background-color: #f4f4f4; padding: 10px;  
             resize: none; border: 1px solid #ccc; border-radius: 4px;",
                  "@article{Marty2015UniDec,
  author = {Marty, Michael T. and Baldwin, Andrew J. and Marklund, Erik G. and Hochberg, Georg K. A. and Benesch, Justin L. P. and Robinson, Carol V.},
  title = {UniDec: Universal Deconvolution of Mass and Ion Mobility Spectra},
  journal = {Analytical Chemistry},
  volume = {87},
  number = {8},
  pages = {4370-4376},
  year = {2015},
  doi = {10.1021/acs.analchem.5b00140}
}"
                ),
                shiny$tags$button(
                  "Copy",
                  id = "copy_btn",
                  class = "btn btn-default btn-sm",
                  style = "position: absolute; bottom: 40px; right: 2.5rem; z-index: 10; opacity: 0.8;",
                  onclick = "
      var textArea = document.getElementById('bibtex_unidec');
      textArea.select();
      document.execCommand('copy');
      var btn = document.getElementById('copy_btn');
      btn.innerHTML = 'Copied!';
      setTimeout(function(){ btn.innerHTML = 'Copy'; }, 2000);
    "
                )
              )
            ),

            footer = shiny$tagList(
              shiny$modalButton("Dismiss")
            )
          )
        )
      )
    })

    # Update modal
    shiny$observeEvent(input$open_update_modal, {
      shiny$req(local_version, release, message, link, hint)

      shiny$showModal(
        shiny$div(
          class = "start-modal",
          shiny$modalDialog(
            shiny$fluidRow(
              shiny$br(),
              shiny$column(
                width = 11,
                shiny$fluidRow(
                  shiny$column(
                    width = 6,
                    shiny$p("Current Version")
                  ),
                  shiny$column(
                    width = 6,
                    shiny$p(local_version, style = "font-style: italic")
                  )
                ),
                shiny$fluidRow(
                  shiny$column(
                    width = 6,
                    shiny$p("Release date")
                  ),
                  shiny$column(
                    width = 6,
                    shiny$p(release, style = "font-style: italic")
                  )
                ),
                shiny$br(),
                shiny$fluidRow(
                  shiny$column(
                    width = 12,
                    shiny$h6(message, style = "font-weight: bold"),
                    shiny$p(
                      shiny$HTML(hint),
                      style = "font-style: italic; margin-top: 1rem;"
                    ),
                    shiny$tags$a(href = link, link, target = "_blank")
                  )
                )
              )
            ),
            title = "Version and Update",
            easyClose = TRUE,
            footer = shiny$tagList(
              shiny$modalButton("Dismiss")
            )
          )
        )
      )
    })

    # Hide waiter
    waiter_hide()
  })
}
