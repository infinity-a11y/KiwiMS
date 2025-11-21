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
  card(
    shiny$div(
      class = "deconvolution-init-ui-wrapper",
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
                class = "instruction-info",
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
                    class = "bg-dark help-header",
                    tooltip(
                      "Charge state [z]",
                      "The number of charges the ionized molecule is expected to carry.",
                      placement = "bottom"
                    ),
                    shiny$div(
                      class = "tooltip-bttn",
                      shiny$actionButton(
                        ns("charge_range_tooltip_bttn"),
                        label = "",
                        icon = shiny$icon("circle-question")
                      )
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
                    class = "bg-dark help-header",
                    tooltip(
                      "Deconvolution range [m/z]",
                      "The span of molecular weights to be deconvoluted.",
                      placement = "bottom"
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
                    class = "bg-dark help-header",
                    tooltip(
                      "Mass range [Da]",
                      "The span of molecular weights to be deconvoluted.",
                      placement = "bottom"
                    ),
                    shiny$div(
                      class = "tooltip-bttn",
                      shiny$actionButton(
                        ns("mass_range_tooltip_bttn"),
                        label = "",
                        icon = shiny$icon("circle-question")
                      )
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
                            value = 10000
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
                            value = 60000
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
                    class = "bg-dark help-header",
                    tooltip(
                      "Retention time [min]",
                      "The anticipated time for the analyte to travel through a chromatography column.",
                      placement = "bottom"
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
                            value = 0.5,
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
                            value = 1.5,
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
                    class = "bg-dark help-header",
                    tooltip(
                      "Peak parameters",
                      "Expected characteristics of spectral peaks.",
                      placement = "right"
                    )
                  ),
                  card_body(
                    shiny$fluidRow(
                      shiny$column(
                        width = 6,
                        shiny$h6(
                          "Detection\n window [Da]",
                          style = "margin-top: 8px;"
                        )
                      ),
                      shiny$column(
                        width = 4,
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
                      ),
                      shiny$column(
                        width = 2,
                        shiny$div(
                          class = "tooltip-bttn",
                          shiny$actionButton(
                            ns("detection_window_tooltip_bttn"),
                            label = "",
                            icon = shiny$icon("circle-question")
                          )
                        )
                      )
                    ),
                    shiny$fluidRow(
                      shiny$column(
                        width = 6,
                        tooltip(
                          shiny$h6(
                            "Peak\n normalization",
                            style = "margin-top: 8px;"
                          ),
                          shiny$HTML(
                            "Peak intensity normalization mode:<br> 0 = no normalization<br> 1 = max normalization<br> 2 = normalization to the sum"
                          ),
                          placement = "left"
                        )
                      ),
                      shiny$column(
                        width = 5,
                        shiny$div(
                          class = "deconv-param-input-adv",
                          disabled(
                            shiny$selectInput(
                              ns("peaknorm"),
                              "",
                              choices = c(
                                "No normalization" = 0,
                                "Max Normalization" = 1,
                                "Normalization to Sum" = 2
                              ),
                              selected = "Normalization to Sum"
                            )
                          )
                        )
                      )
                    ),
                    shiny$fluidRow(
                      shiny$column(
                        width = 6,
                        shiny$h6(
                          "Threshold",
                          style = "font-size: 1vw; margin-top: 8px;"
                        )
                      ),
                      shiny$column(
                        width = 4,
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
                      ),
                      shiny$column(
                        width = 2,
                        shiny$div(
                          class = "tooltip-bttn",
                          shiny$actionButton(
                            ns("threshold_tooltip_bttn"),
                            label = "",
                            icon = shiny$icon("circle-question")
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
                    class = "bg-dark help-header",
                    tooltip(
                      "Sample Rate (Resolution)",
                      "Discrete intervals of mass values for the spectra.",
                      placement = "bottom"
                    ),
                    shiny$div(
                      class = "tooltip-bttn",
                      shiny$actionButton(
                        ns("sample_rate_tooltip_bttn"),
                        label = "",
                        icon = shiny$icon("circle-question")
                      )
                    )
                  ),
                  card_body(
                    shiny$fluidRow(
                      shiny$column(
                        width = 6,
                        shiny$h6("Size [Da]", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 4,
                        shiny$div(
                          class = "deconv-param-input-adv mass-bin-input",
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
        class = "full-height-row",
        shiny$fluidRow(
          shiny$column(
            width = 12,
            align = "center",
            shiny$uiOutput(ns("deconvolute_start_ui"))
          )
        )
      )
    )
  )
}

# Deconvolution result controls interface
deconvolution_status_controls <- function(ns) {
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
            width = 2,
            shiny$div(
              class = "decon-btn",
              shiny$actionButton(
                ns("show_log"),
                "",
                icon = shiny$icon("code"),
                width = "100%"
              )
            )
          ),
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
  )
}

# Deconvolution running interface (batch mode)
#' @export
deconvolution_running_ui_plate <- function(ns) {
  card(
    class = "deconvolution-parent-card",
    shiny$div(
      class = "deconvolution-running-interface",
      deconvolution_status_controls(ns),
      card(
        class = "deconvolution-running-result-card",
        shiny$fluidRow(
          shiny$column(
            width = 6
          ),
          shiny$column(
            width = 3,
            align = "center",
            shiny$div(
              class = "deconvolution-result-controls-noplate",
              disabled(
                radioGroupButtons(
                  ns("toggle_result"),
                  choiceNames = c("Deconvoluted", "Raw m/z"),
                  choiceValues = c(FALSE, TRUE)
                )
              )
            )
          ),
          shiny$column(
            width = 3,
            align = "center",
            shiny$div(
              class = "deconvolution-result-controls-noplate",
              shiny$uiOutput(ns("result_picker_ui"))
            )
          )
        ),
        shiny$div(
          class = "deconvolution-result-content-plate",
          shiny$fluidRow(
            shiny$column(
              width = 6,
              shiny$div(
                class = "heatmap-plot",
                withWaiter(
                  plotlyOutput(ns("heatmap"), height = "100%")
                )
              )
            ),
            shiny$column(
              width = 6,
              shiny$div(
                class = "spectrum-plot-plate",
                plotlyOutput(ns("spectrum"), height = "100%")
              )
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
  card(
    class = "deconvolution-parent-card",
    shiny$div(
      class = "deconvolution-running-interface",
      deconvolution_status_controls(ns),
      card(
        class = "deconvolution-running-result-card",
        shiny$div(
          class = "deconvolution-result-content-noplate",
          shiny$fluidRow(
            shiny$column(
              width = 4,
              align = "center",
              shiny$div(
                class = "deconvolution-result-controls",
                disabled(
                  radioGroupButtons(
                    ns("toggle_result"),
                    choiceNames = c("Deconvoluted", "Raw m/z"),
                    choiceValues = c(FALSE, TRUE)
                  )
                ),
                shiny$br(),
                shiny$uiOutput(ns("result_picker_ui"))
              )
            ),
            shiny$column(
              width = 7,
              align = "center",
              shiny$div(
                class = "spectrum-plot-noplate",
                plotlyOutput(ns("spectrum"), height = "100%")
              )
            )
          )
        )
      )
    )
  )
}
