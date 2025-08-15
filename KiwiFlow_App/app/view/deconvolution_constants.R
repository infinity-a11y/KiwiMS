# app/view/deconvolution_constants.R

box::use(
  bslib[card, card_body, card_header, tooltip],
  fs[dir_ls],
  plotly[event_data, event_register, plotlyOutput, renderPlotly],
  processx[process],
  shiny,
  shinyjs[delay, disable, disabled, enable, hide, show, hidden, runjs],
  shinyWidgets[
    pickerInput,
    progressBar,
    radioGroupButtons,
    updateProgressBar
  ],
  clipr[write_clip],
  utils[head, tail],
  waiter[useWaiter, spin_wandering_cubes, waiter_show, waiter_hide, withWaiter],
)

# Deconvolution initiation interface
#' @export
deconvolution_init_ui <- function(ns) {
  shiny$div(
    shiny$fluidRow(
      shiny$column(
        width = 12,
        shiny$fluidRow(
          shiny$column(
            width = 12,
            shiny$div(
              class = "sidebar-title",
              shiny$HTML("Configure Spectrum Deconvolution")
            )
          )
        ),
        shiny$fluidRow(
          shiny$column(
            width = 8,
            shiny$div(
              class = "deconvolution_info",
              shiny$HTML(
                paste(
                  "1. Use the sidebar to select the Waters .raw folder(s) for processing.",
                  "<br/>",
                  "2. Check and configure parameters in the main panel and start deconvolution."
                )
              )
            ),
            shiny$br()
          ),
          shiny$column(
            width = 4,
            shiny$checkboxInput(
              ns("show_advanced"),
              "Edit advanced settings",
              value = FALSE
            )
          )
        ),
        shiny$fluidRow(
          shiny$column(
            width = 4,
            shiny$div(
              class = "card-custom",
              card(
                card_header(
                  class = "bg-dark",
                  "Charge state [z]",
                  tooltip(
                    shiny$icon("circle-question"),
                    "The number of charges the ionized molecule is expected to carry.",
                    placement = "right"
                  )
                ),
                card_body(
                  shiny$fluidRow(
                    shiny$column(
                      width = 4,
                      shiny$h6("Low", style = "margin-top: 8px;")
                    ),
                    shiny$column(
                      width = 6,
                      shiny$div(
                        class = "deconv-param-input",
                        shiny$numericInput(
                          ns("startz"),
                          "",
                          min = 0,
                          max = 100,
                          value = 1
                        )
                      )
                    )
                  ),
                  shiny$fluidRow(
                    shiny$column(
                      width = 4,
                      shiny$h6("High", style = "margin-top: 8px;")
                    ),
                    shiny$column(
                      width = 6,
                      shiny$div(
                        class = "deconv-param-input",
                        shiny$numericInput(
                          ns("endz"),
                          "",
                          min = 0,
                          max = 100,
                          value = 50
                        )
                      )
                    )
                  )
                )
              )
            ),
            shiny$div(
              class = "card-custom",
              card(
                card_header(
                  class = "bg-dark",
                  "Spectrum range [m/z]",
                  tooltip(
                    shiny$icon("circle-question"),
                    "The span of molecular weights to be analyzed.",
                    placement = "right"
                  )
                ),
                card_body(
                  shiny$fluidRow(
                    shiny$column(
                      width = 4,
                      shiny$h6("Low", style = "margin-top: 8px;")
                    ),
                    shiny$column(
                      width = 6,
                      shiny$div(
                        class = "deconv-param-input",
                        shiny$numericInput(
                          ns("minmz"),
                          "",
                          min = 0,
                          max = 100000,
                          value = 710
                        )
                      )
                    )
                  ),
                  shiny$fluidRow(
                    shiny$column(
                      width = 4,
                      shiny$h6("High", style = "margin-top: 8px;")
                    ),
                    shiny$column(
                      width = 6,
                      shiny$div(
                        class = "deconv-param-input",
                        shiny$numericInput(
                          ns("maxmz"),
                          "",
                          min = 0,
                          max = 100000,
                          value = 1100
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          shiny$column(
            width = 4,
            shiny$div(
              class = "card-custom",
              card(
                card_header(
                  class = "bg-dark",
                  "Mass range [Mw]",
                  tooltip(
                    shiny$icon("circle-question"),
                    "The range of mass-to-charge ratios to be detected.",
                    placement = "right"
                  )
                ),
                card_body(
                  shiny$fluidRow(
                    shiny$column(
                      width = 4,
                      shiny$h6("Low", style = "margin-top: 8px;")
                    ),
                    shiny$column(
                      width = 6,
                      shiny$div(
                        class = "deconv-param-input",
                        shiny$numericInput(
                          ns("masslb"),
                          "",
                          min = 0,
                          max = 100000,
                          value = 35000
                        )
                      )
                    )
                  ),
                  shiny$fluidRow(
                    shiny$column(
                      width = 4,
                      shiny$h6("High", style = "margin-top: 8px;")
                    ),
                    shiny$column(
                      width = 6,
                      shiny$div(
                        class = "deconv-param-input",
                        shiny$numericInput(
                          ns("massub"),
                          "",
                          min = 0,
                          max = 100000,
                          value = 42000
                        )
                      )
                    )
                  )
                )
              )
            ),
            shiny$div(
              class = "card-custom",
              card(
                card_header(
                  class = "bg-dark",
                  "Retention time [min]",
                  tooltip(
                    shiny$icon("circle-question"),
                    "The anticipated time for the analyte to travel through a chromatography column.",
                    placement = "right"
                  )
                ),
                card_body(
                  shiny$fluidRow(
                    shiny$column(
                      width = 4,
                      shiny$h6("Start", style = "margin-top: 8px;")
                    ),
                    shiny$column(
                      width = 6,
                      shiny$div(
                        class = "deconv-param-input",
                        shiny$numericInput(
                          ns("time_start"),
                          "",
                          min = 0,
                          max = 100,
                          value = 1,
                          step = 0.05
                        )
                      )
                    )
                  ),
                  shiny$fluidRow(
                    shiny$column(
                      width = 4,
                      shiny$h6("End", style = "margin-top: 8px;")
                    ),
                    shiny$column(
                      width = 6,
                      shiny$div(
                        class = "deconv-param-input",
                        shiny$numericInput(
                          ns("time_end"),
                          "",
                          min = 0,
                          max = 100,
                          value = 1.35,
                          step = 0.05
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          shiny$column(
            width = 4,
            shiny$div(
              class = "card-custom",
              card(
                card_header(
                  class = "bg-dark",
                  "Peak parameters",
                  tooltip(
                    shiny$icon("circle-question"),
                    "Expected characteristics of spectral peaks.",
                    placement = "right"
                  )
                ),
                card_body(
                  shiny$fluidRow(
                    shiny$column(
                      width = 4,
                      shiny$h6("Window", style = "margin-top: 8px;")
                    ),
                    shiny$column(
                      width = 6,
                      shiny$div(
                        class = "deconv-param-input-adv",
                        disabled(
                          shiny$numericInput(
                            ns("peakwindow"),
                            "",
                            min = 0,
                            max = 1000,
                            value = 40
                          )
                        )
                      )
                    )
                  ),
                  shiny$fluidRow(
                    shiny$column(
                      width = 4,
                      shiny$h6("Norm", style = "margin-top: 8px;")
                    ),
                    shiny$column(
                      width = 6,
                      shiny$div(
                        class = "deconv-param-input-adv",
                        disabled(
                          shiny$numericInput(
                            ns("peaknorm"),
                            "",
                            min = 0,
                            max = 100,
                            value = 2
                          )
                        )
                      )
                    )
                  ),
                  shiny$fluidRow(
                    shiny$column(
                      width = 4,
                      shiny$h6(
                        "Threshold",
                        style = "font-size: 0.9em; margin-top: 8px;"
                      )
                    ),
                    shiny$column(
                      width = 6,
                      shiny$div(
                        class = "deconv-param-input-adv",
                        disabled(
                          shiny$numericInput(
                            ns("peakthresh"),
                            "",
                            min = 0,
                            max = 1,
                            value = 0.07,
                            step = 0.01
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            shiny$div(
              class = "card-custom",
              card(
                card_header(
                  class = "bg-dark",
                  "Mass Bins",
                  tooltip(
                    shiny$icon("circle-question"),
                    "Discrete intervals of mass values for the spectra.",
                    placement = "right"
                  )
                ),
                card_body(
                  shiny$fluidRow(
                    shiny$column(
                      width = 4,
                      shiny$h6("Size", style = "margin-top: 8px;")
                    ),
                    shiny$column(
                      width = 6,
                      shiny$div(
                        class = "deconv-param-input-adv",
                        disabled(
                          shiny$numericInput(
                            ns("massbins"),
                            "",
                            min = 0,
                            max = 100,
                            value = 0.5,
                            step = 0.1
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
      )
    ),
    shiny$div(
      class = "align-row",
      shiny$fluidRow(
        shiny$column(
          width = 12,
          align = "center",
          shiny$uiOutput(ns("deconvolute_start_ui")),
          shiny$br(),
          shiny$uiOutput(ns("deconvolute_progress"))
        )
      )
    )
  )
}

# Deconvolution running interface (batch mode)
#' @export
deconvolution_running_ui_plate <- function(ns) {
  shiny$column(
    width = 12,
    useWaiter(),
    shiny$fluidRow(
      shiny$column(
        width = 2,
        align = "center",
        shinyjs::hidden(
          shiny$div(
            id = ns("processing"),
            shiny$HTML(
              paste0(
                '<i class="fa fa-spinner fa-spin fa-fw fa-2x" style="color: ',
                '#38387C; margin-top: 0.5em"></i>'
              )
            )
          )
        ),
        shinyjs::hidden(
          shiny$div(
            id = ns("processing_stop"),
            shiny$HTML(
              paste0(
                '<i class="fa-solid fa-spinner fa-2x" style="color: ',
                '#38387C; margin-top: 0.5em"></i>'
              )
            )
          )
        ),
        shinyjs::hidden(
          shiny$div(
            id = ns("processing_fin"),
            shiny$HTML(
              paste0(
                '<i class="fa-solid fa-circle-check fa-2x" style="color: ',
                '#38387C; margin-top: 0.5em"></i>'
              )
            )
          )
        )
      ),
      shiny$column(
        width = 6,
        progressBar(
          id = ns("progressBar"),
          value = 0,
          title = "Initiating Deconvolution",
          display_pct = TRUE
        )
      ),
      shiny$column(
        width = 2,
        shiny$actionButton(
          ns("deconvolute_end"),
          "Abort",
          icon = shiny$icon("circle-stop")
        )
      ),
      shiny$column(
        width = 2,
        shiny$div(
          class = "decon-btn",
          disabled(
            shiny$actionButton(
              ns("forward_deconvolution"),
              "Next Step",
              icon = shiny$icon("forward-fast")
            )
          )
        )
      )
    ),
    shiny$hr(style = "margin: 1.5rem 0; opacity: 0.8;"),
    shiny$fluidRow(
      shiny$column(
        width = 3,
        shiny$div(
          class = "decon-btn",
          shiny$actionButton(
            ns("show_log"),
            "Output",
            icon = shiny$icon("code"),
            width = "100%"
          )
        )
      ),
      shiny$column(
        width = 3,
        shiny$div(
          class = "decon-btn",
          disabled(
            shiny$actionButton(
              ns("deconvolution_report"),
              "Report",
              icon = shiny$icon("square-poll-vertical")
            )
          )
        )
      ),
      shiny$column(
        width = 3,
        align = "center",
        shiny$uiOutput(ns("result_picker_ui"))
      ),
      shiny$column(
        width = 3,
        align = "center",
        disabled(
          radioGroupButtons(
            ns("toggle_result"),
            choiceNames = c("Deconvoluted", "Raw m/z"),
            choiceValues = c(FALSE, TRUE)
          )
        )
      )
    ),
    shiny$fluidRow(
      shiny$column(
        width = 6,
        shiny$br(),
        shiny$div(
          class = "card-custom-plate",
          card(
            card_header(
              class = "bg-dark",
              "384-Well Plate Heatmap"
            ),
            card_body(
              withWaiter(
                plotlyOutput(ns("heatmap"))
              )
            )
          )
        )
      ),
      shiny$column(
        width = 6,
        shiny$div(
          class = "card-custom-plate2",
          card(
            full_screen = TRUE,
            card_header(
              class = "bg-dark",
              "Spectrum"
            ),
            card_body(
              plotlyOutput(ns("spectrum"))
            )
          )
        )
      )
    )
  )
}

# Deconvolution running interface (no batch mode)
#' @export
deconvolution_running_ui_noplate <- function(ns) {
  shiny$div(
    class = "deconvolution-running-interface",
    shiny$fluidRow(
      shiny$column(
        width = 5,
        card(
          class = "deconvolution-running-control-card",
          shiny$fluidRow(
            shiny$column(
              width = 3,
              align = "center",
              shinyjs::hidden(
                shiny$div(
                  id = ns("processing"),
                  shiny$HTML(
                    paste0(
                      '<i class="fa fa-spinner fa-spin fa-fw fa-2x" style="color: ',
                      '#38387C;"></i>'
                    )
                  )
                )
              ),
              shinyjs::hidden(
                shiny$div(
                  id = ns("processing_stop"),
                  shiny$HTML(
                    paste0(
                      '<i class="fa-solid fa-spinner fa-2x" style="color: ',
                      '#38387C;"></i>'
                    )
                  )
                )
              ),
              shinyjs::hidden(
                shiny$div(
                  id = ns("processing_error"),
                  shiny$HTML(
                    paste0(
                      '<i class="fa-solid fa-circle-exclamation fa-2x" style="color: ',
                      '#D17050;"></i>'
                    )
                  )
                )
              ),
              shinyjs::hidden(
                shiny$div(
                  id = ns("processing_fin"),
                  shiny$HTML(
                    paste0(
                      '<i class="fa-solid fa-circle-check fa-2x" style="color: ',
                      '#38387C;"></i>'
                    )
                  )
                )
              )
            ),
            shiny$column(
              width = 8,
              progressBar(
                id = ns("progressBar"),
                value = 0,
                title = "Initiating Deconvolution",
                display_pct = TRUE
              )
            )
          )
        )
      ),
      shiny$column(
        width = 7,
        card(
          class = "deconvolution-running-control-card",
          shiny$fluidRow(
            shiny$column(
              width = 3,
              shiny$actionButton(
                ns("deconvolute_end"),
                "Abort",
                icon = shiny$icon("circle-stop"),
                width = "100%"
              )
            ),
            shiny$column(
              width = 3,
              shiny$div(
                class = "decon-btn",
                shiny$actionButton(
                  ns("show_log"),
                  "Output",
                  icon = shiny$icon("code"),
                  width = "100%"
                )
              )
            ),
            shiny$column(
              width = 3,
              shiny$div(
                class = "decon-btn",
                disabled(
                  shiny$actionButton(
                    ns("deconvolution_report"),
                    "Report",
                    icon = shiny$icon("square-poll-vertical"),
                    width = "100%"
                  )
                )
              )
            ),
            shiny$column(
              width = 3,
              shiny$div(
                class = "decon-btn",
                disabled(
                  shiny$actionButton(
                    ns("forward_deconvolution"),
                    "Continue",
                    icon = shiny$icon("forward-fast"),
                    width = "100%"
                  )
                )
              )
            )
          )
        )
      )
    ),
    card(
      class = "deconvolution-running-result-card",
      shiny$div(
        class = "deconvolution-result-content",
        shiny$fluidRow(
          shiny$column(
            width = 3,
            align = "center",
            shiny$uiOutput(ns("result_picker_ui"))
          ),
          shiny$column(
            width = 3,
            align = "center",
            disabled(
              radioGroupButtons(
                ns("toggle_result"),
                choiceNames = c("Deconvoluted", "Raw m/z"),
                choiceValues = c(FALSE, TRUE)
              )
            )
          )
        ),
        shiny$fluidRow(
          shiny$column(2),
          shiny$column(
            width = 8,
            shiny$div(
              class = "card-custom-plate2",
              card(
                full_screen = TRUE,
                card_header(
                  class = "bg-dark",
                  "Spectrum"
                ),
                card_body(
                  plotlyOutput(ns("spectrum"))
                )
              )
            )
          )
        )
      )
    )
  )

  # shiny$column(
  #   width = 12,
  #   useWaiter(),
  #   shiny$fluidRow(
  #     shiny$column(
  #       width = 2,
  #       align = "center",
  #       shinyjs::hidden(
  #         shiny$div(
  #           id = ns("processing"),
  #           shiny$HTML(
  #             paste0(
  #               '<i class="fa fa-spinner fa-spin fa-fw fa-2x" style="color: ',
  #               '#38387C; margin-top: 0.5em"></i>'
  #             )
  #           )
  #         )
  #       ),
  #       shinyjs::hidden(
  #         shiny$div(
  #           id = ns("processing_stop"),
  #           shiny$HTML(
  #             paste0(
  #               '<i class="fa-solid fa-spinner fa-2x" style="color: ',
  #               '#38387C; margin-top: 0.5em"></i>'
  #             )
  #           )
  #         )
  #       ),
  #       shinyjs::hidden(
  #         shiny$div(
  #           id = ns("processing_error"),
  #           shiny$HTML(
  #             paste0(
  #               '<i class="fa-solid fa-circle-exclamation fa-2x" style="color: ',
  #               '#D17050; margin-top: 0.5em"></i>'
  #             )
  #           )
  #         )
  #       ),
  #       shinyjs::hidden(
  #         shiny$div(
  #           id = ns("processing_fin"),
  #           shiny$HTML(
  #             paste0(
  #               '<i class="fa-solid fa-circle-check fa-2x" style="color: ',
  #               '#38387C; margin-top: 0.5em"></i>'
  #             )
  #           )
  #         )
  #       )
  #     ),
  #     shiny$column(
  #       width = 6,
  #       progressBar(
  #         id = ns("progressBar"),
  #         value = 0,
  #         title = "Initiating Deconvolution",
  #         display_pct = TRUE
  #       )
  #     ),
  #     shiny$column(
  #       width = 2,
  #       align = "left",
  #       shiny$actionButton(
  #         ns("deconvolute_end"),
  #         "Abort",
  #         icon = shiny$icon("circle-stop")
  #       )
  #     ),
  #     shiny$column(
  #       width = 2,
  #       shiny$div(
  #         class = "decon-btn",
  #         disabled(
  #           shiny$actionButton(
  #             ns("forward_deconvolution"),
  #             "Next Step",
  #             icon = shiny$icon("forward-fast")
  #           )
  #         )
  #       )
  #     )
  #   ),
  #   shiny$hr(style = "margin: 1.5rem 0; opacity: 0.8;"),
  #   shiny$fluidRow(
  #     shiny$column(
  #       width = 3,
  #       shiny$div(
  #         class = "decon-btn",
  #         shiny$actionButton(
  #           ns("show_log"),
  #           "Show Log",
  #           icon = shiny$icon("code")
  #         )
  #       )
  #     ),
  #     shiny$column(
  #       width = 3,
  #       shiny$div(
  #         class = "decon-btn",
  #         disabled(
  #           shiny$actionButton(
  #             ns("deconvolution_report"),
  #             "Create Report",
  #             icon = shiny$icon("square-poll-vertical")
  #           )
  #         )
  #       )
  #     ),
  #     shiny$column(
  #       width = 3,
  #       align = "center",
  #       shiny$uiOutput(ns("result_picker_ui"))
  #     ),
  #     shiny$column(
  #       width = 3,
  #       align = "center",
  #       disabled(
  #         radioGroupButtons(
  #           ns("toggle_result"),
  #           choiceNames = c("Deconvoluted", "Raw m/z"),
  #           choiceValues = c(FALSE, TRUE)
  #         )
  #       )
  #     )
  #   ),
  #   shiny$fluidRow(
  #     shiny$column(2),
  #     shiny$column(
  #       width = 8,
  #       shiny$div(
  #         class = "card-custom-plate2",
  #         card(
  #           card_header(
  #             class = "bg-dark",
  #             "Spectrum"
  #           ),
  #           card_body(
  #             plotlyOutput(ns("spectrum"))
  #           )
  #         )
  #       )
  #     )
  #   )
  # )
}
