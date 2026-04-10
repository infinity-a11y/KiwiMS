# app/logic/deconvolution_ui.R
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
  waiter[useWaiter, spin_wave, waiter_show, waiter_hide, withWaiter],
)

box::use(
  app / logic / user_settings[read_user_settings],
)

# Deconvolution initiation interface
#' @export
deconvolution_init_ui <- function(ns, analysis_name_default = "") {
  s <- read_user_settings()
  startz_def <- s$deconv_startz
  endz_def <- s$deconv_endz
  minmz_def <- s$deconv_minmz
  maxmz_def <- s$deconv_maxmz
  masslb_def <- s$deconv_masslb
  massub_def <- s$deconv_massub
  time_start_def <- s$deconv_time_start
  time_end_def <- s$deconv_time_end
  peakwindow_def <- s$deconv_peakwindow
  peaknorm_def <- as.character(s$deconv_peaknorm)
  peakthresh_def <- s$deconv_peakthresh
  massbins_def <- s$deconv_massbins

  card(
    card_header(
      shiny$div(
        class = "deconvolution-title",
        "Configure Spectrum Deconvolution"
      )
    ),
    card_body(
      class = "deconvolution-init-card",
      shiny$div(
        class = "deconvolution-init-ui-wrapper",
        shiny$column(
          width = 12,
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
              shiny$div(
                class = "show-advanced-ui",
                shiny$checkboxInput(
                  ns("show_advanced"),
                  "Edit advanced settings",
                  value = FALSE
                )
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
                        label = NULL,
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
                        width = 7,
                        shiny$div(
                          class = "dest-folder-row",
                          shiny$div(
                            class = "deconv-param-input",
                            shiny$numericInput(
                              ns("startz"),
                              "",
                              min = 1,
                              max = 100,
                              value = startz_def,
                              step = 1
                            )
                          ),
                          shiny::div(
                            style = "height: -webkit-fill-available;",
                            tooltip(
                              shiny$div(
                                class = "save-button",
                                shiny$actionButton(
                                  ns("save_startz_btn"),
                                  NULL,
                                  icon = shiny$icon("floppy-disk"),
                                  class = "btn-default"
                                )
                              ),
                              "Save as default value",
                              placement = "bottom"
                            )
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
                        width = 7,
                        shiny$div(
                          class = "dest-folder-row",
                          shiny$div(
                            class = "deconv-param-input",
                            shiny$numericInput(
                              ns("endz"),
                              "",
                              min = 1,
                              max = 100,
                              value = endz_def,
                              step = 1
                            )
                          ),
                          shiny::div(
                            style = "height: -webkit-fill-available;",
                            tooltip(
                              shiny$div(
                                class = "save-button",
                                shiny$actionButton(
                                  ns("save_endz_btn"),
                                  NULL,
                                  icon = shiny$icon("floppy-disk"),
                                  class = "btn-default"
                                )
                              ),
                              "Save as default value",
                              placement = "bottom"
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
                        width = 7,
                        shiny$div(
                          class = "dest-folder-row",
                          shiny$div(
                            class = "deconv-param-input",
                            shiny$numericInput(
                              ns("minmz"),
                              "",
                              min = 1,
                              max = 100000,
                              value = minmz_def,
                              step = 1
                            )
                          ),
                          shiny::div(
                            style = "height: -webkit-fill-available;",
                            tooltip(
                              shiny$div(
                                class = "save-button",
                                shiny$actionButton(
                                  ns("save_minmz_btn"),
                                  NULL,
                                  icon = shiny$icon("floppy-disk"),
                                  class = "btn-default"
                                )
                              ),
                              "Save as default value",
                              placement = "bottom"
                            )
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
                        width = 7,
                        shiny$div(
                          class = "dest-folder-row",
                          shiny$div(
                            class = "deconv-param-input",
                            shiny$numericInput(
                              ns("maxmz"),
                              "",
                              min = 1,
                              max = 100000,
                              value = maxmz_def,
                              step = 1
                            )
                          ),
                          shiny::div(
                            style = "height: -webkit-fill-available;",
                            tooltip(
                              shiny$div(
                                class = "save-button",
                                shiny$actionButton(
                                  ns("save_maxmz_btn"),
                                  NULL,
                                  icon = shiny$icon("floppy-disk"),
                                  class = "btn-default"
                                )
                              ),
                              "Save as default value",
                              placement = "bottom"
                            )
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
                        label = NULL,
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
                        width = 7,
                        shiny$div(
                          class = "dest-folder-row",
                          shiny$div(
                            class = "deconv-param-input",
                            shiny$numericInput(
                              ns("masslb"),
                              "",
                              min = 1,
                              max = 2000000,
                              value = masslb_def,
                              step = 1
                            )
                          ),
                          shiny::div(
                            style = "height: -webkit-fill-available;",
                            tooltip(
                              shiny$div(
                                class = "save-button",
                                shiny$actionButton(
                                  ns("save_masslb_btn"),
                                  NULL,
                                  icon = shiny$icon("floppy-disk"),
                                  class = "btn-default"
                                )
                              ),
                              "Save as default value",
                              placement = "bottom"
                            )
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
                        width = 7,
                        shiny$div(
                          class = "dest-folder-row",
                          shiny$div(
                            class = "deconv-param-input",
                            shiny$numericInput(
                              ns("massub"),
                              "",
                              min = 1,
                              max = 2000000,
                              value = massub_def,
                              step = 1
                            )
                          ),
                          shiny::div(
                            style = "height: -webkit-fill-available;",
                            tooltip(
                              shiny$div(
                                class = "save-button",
                                shiny$actionButton(
                                  ns("save_massub_btn"),
                                  NULL,
                                  icon = shiny$icon("floppy-disk"),
                                  class = "btn-default"
                                )
                              ),
                              "Save as default value",
                              placement = "bottom"
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
                        width = 7,
                        shiny$div(
                          class = "dest-folder-row",
                          shiny$div(
                            class = "deconv-param-input",
                            shiny$numericInput(
                              ns("time_start"),
                              "",
                              min = 0,
                              max = 100,
                              value = time_start_def,
                              step = 0.05
                            )
                          ),
                          shiny::div(
                            style = "height: -webkit-fill-available;",
                            tooltip(
                              shiny$div(
                                class = "save-button",
                                shiny$actionButton(
                                  ns("save_time_start_btn"),
                                  NULL,
                                  icon = shiny$icon("floppy-disk"),
                                  class = "btn-default"
                                )
                              ),
                              "Save as default value",
                              placement = "bottom"
                            )
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
                        width = 7,
                        shiny$div(
                          class = "dest-folder-row",
                          shiny$div(
                            class = "deconv-param-input",
                            shiny$numericInput(
                              ns("time_end"),
                              "",
                              min = 0,
                              max = 100,
                              value = time_end_def,
                              step = 0.05
                            )
                          ),
                          shiny::div(
                            style = "height: -webkit-fill-available;",
                            tooltip(
                              shiny$div(
                                class = "save-button",
                                shiny$actionButton(
                                  ns("save_time_end_btn"),
                                  NULL,
                                  icon = shiny$icon("floppy-disk"),
                                  class = "btn-default"
                                )
                              ),
                              "Save as default value",
                              placement = "bottom"
                            )
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
                  style = "overflow: visible !important;",
                  card_header(
                    class = "bg-dark help-header",
                    tooltip(
                      "Peak parameters",
                      "Expected characteristics of spectral peaks.",
                      placement = "bottom"
                    ),
                    shiny$div(
                      class = "tooltip-bttn",
                      shiny$actionButton(
                        ns("peak_parameter_tooltip_bttn"),
                        label = NULL,
                        icon = shiny$icon("circle-question")
                      )
                    )
                  ),
                  card_body(
                    style = "overflow: visible !important;",
                    shiny$fluidRow(
                      shiny$column(
                        width = 6,
                        shiny$h6(
                          "Detection\n window [Da]",
                          style = "margin-top: 8px;"
                        )
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "dest-folder-row",
                          shiny$div(
                            class = "deconv-param-input-adv",
                            disabled(
                              shiny$numericInput(
                                ns("peakwindow"),
                                "",
                                min = 1,
                                max = 500,
                                value = peakwindow_def,
                                step = 1
                              )
                            )
                          ),
                          shiny::div(
                            style = "height: -webkit-fill-available;",
                            tooltip(
                              shiny$div(
                                class = "save-button",
                                shiny$actionButton(
                                  ns("save_peakwindow_btn"),
                                  NULL,
                                  icon = shiny$icon("floppy-disk"),
                                  class = "btn-default"
                                )
                              ),
                              "Save as default value",
                              placement = "bottom"
                            )
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
                          class = "dest-folder-row",
                          shiny$div(
                            class = "deconv-param-input-adv peaknorm-selector",
                            disabled(
                              shiny$selectInput(
                                ns("peaknorm"),
                                "",
                                choices = c(
                                  "No normalization" = 0,
                                  "Max Normalization" = 1,
                                  "Normalization to Sum" = 2
                                ),
                                selected = peaknorm_def
                              )
                            )
                          ),
                          shiny::div(
                            style = "height: -webkit-fill-available;",
                            tooltip(
                              shiny$div(
                                class = "save-button",
                                shiny$actionButton(
                                  ns("save_peaknorm_btn"),
                                  NULL,
                                  icon = shiny$icon("floppy-disk"),
                                  class = "btn-default"
                                )
                              ),
                              "Save as default value",
                              placement = "bottom"
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
                        width = 6,
                        shiny$div(
                          class = "dest-folder-row",
                          shiny$div(
                            class = "deconv-param-input-adv",
                            disabled(
                              shiny$numericInput(
                                ns("peakthresh"),
                                "",
                                min = 0,
                                max = 1,
                                value = peakthresh_def,
                                step = 0.01
                              )
                            )
                          ),
                          shiny::div(
                            style = "height: -webkit-fill-available;",
                            tooltip(
                              shiny$div(
                                class = "save-button",
                                shiny$actionButton(
                                  ns("save_peakthresh_btn"),
                                  NULL,
                                  icon = shiny$icon("floppy-disk"),
                                  class = "btn-default"
                                )
                              ),
                              "Save as default value",
                              placement = "bottom"
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
                        label = NULL,
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
                        width = 6,
                        shiny$div(
                          class = "dest-folder-row",
                          shiny$div(
                            class = "deconv-param-input-adv mass-bin-input",
                            disabled(
                              shiny$numericInput(
                                ns("massbins"),
                                "",
                                min = 0.1,
                                max = 10,
                                value = massbins_def,
                                step = 0.1
                              )
                            )
                          ),
                          shiny::div(
                            style = "height: -webkit-fill-available;",
                            tooltip(
                              shiny$div(
                                class = "save-button",
                                shiny$actionButton(
                                  ns("save_massbins_btn"),
                                  NULL,
                                  icon = shiny$icon("floppy-disk"),
                                  class = "btn-default"
                                )
                              ),
                              "Save as default value",
                              placement = "bottom"
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
          class = "analysis-init-wrapper",
          shiny$column(
            width = 12,
            shiny$fluidRow(
              shiny$column(
                width = 4,
                shiny$div(
                  class = "analysis-name-ui",
                  shiny$textInput(
                    ns("analysis_name"),
                    "Analysis Name",
                    placeholder = analysis_name_default,
                    value = analysis_name_default
                  )
                )
              ),
              shiny$column(
                width = 4,
                shiny$uiOutput(ns("analysis_name_feedback"))
              ),
              shiny$column(
                width = 4,
                shiny$uiOutput(ns("deconvolute_start_ui"))
              )
            )
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
      width = 6,
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
                    '#7777f9;"></i>'
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
                    '#7777f9;"></i>'
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
                    '#7777f9;"></i>'
                  )
                )
              )
            )
          ),
          shiny$column(
            width = 7,
            progressBar(
              id = ns("progressBar"),
              value = 0,
              title = "Initiating Deconvolution",
              display_pct = TRUE
            )
          ),
          shiny$column(
            width = 1,
            shiny$div(
              class = "decon-btn",
              shiny$actionButton(
                ns("show_log"),
                "",
                icon = shiny$icon("code"),
                width = "100%"
              )
            )
          )
        )
      )
    ),
    shiny$column(
      width = 6,
      card(
        class = "deconvolution-running-control-card",
        shiny$column(
          width = 12,
          shiny$fluidRow(
            shiny$column(
              width = 4,
              shiny$actionButton(
                ns("deconvolute_end"),
                "Abort",
                icon = shiny$icon("circle-stop"),
                width = "100%"
              )
            ),
            shiny$column(
              width = 4,
              shiny$div(
                class = "decon-btn",
                bslib::tooltip(
                  disabled(
                    shiny$actionButton(
                      ns("deconvolution_report"),
                      "Report",
                      icon = shiny$icon("square-poll-vertical"),
                      width = "100%"
                    )
                  ),
                  "Report generation is temporarily unavailable",
                  placement = "top"
                )
              )
            ),
            shiny$column(
              width = 4,
              shiny$div(
                class = "decon-btn",
                disabled(
                  shiny$actionButton(
                    ns("forward_deconvolution"),
                    "Conversion",
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
  )
}


# Deconvolution running interface (no batch mode)
#' @export
deconvolution_results_ui <- function(ns, show_heatmap = FALSE) {
  spectrum_card <- shiny::div(
    class = "deconvolution-spectrum-card card-custom",
    bslib::card(
      bslib::card_header(
        class = "bg-dark help-header d-flex justify-content-between",
        "Spectrum",
        shiny::div(
          class = "box-header-settings-help",
          bslib::popover(
            shiny::icon("gear"),
            shiny::div(
              shiny::div(
                class = "spectrum-radio-button",
                radioGroupButtons(
                  ns("toggle_result"),
                  choiceNames = c("Deconvoluted", "Raw m/z"),
                  choiceValues = c(FALSE, TRUE)
                )
              ),
              shinyWidgets::materialSwitch(
                ns("spectrum_annotation"),
                label = "Annotate Hits",
                value = TRUE,
                right = TRUE
              ),
              style = "margin-right: 20px;"
            ),
            title = NULL
          ),
          shiny::div(
            class = "tooltip-bttn",
            shiny::actionButton(
              ns("mass_spectra_tooltip_bttn"),
              label = NULL,
              icon = shiny::icon("circle-question")
            )
          )
        )
      ),
      shiny$div(
        class = "spectrum-plot",
        plotlyOutput(ns("spectrum"), height = "100%"),
        shiny$uiOutput(ns("spectrum_failure_msg"))
      ),
      full_screen = TRUE
    )
  )

  card(
    class = "deconvolution-parent-card",
    shiny$div(
      class = "deconvolution-running-interface",
      deconvolution_status_controls(ns),
      card(
        class = "deconvolution-running-result-card",
        shiny$div(
          class = "deconvolution-result-content",
          shiny$div(
            class = "deconvolution-result-controls",
            shiny::div(
              shiny$uiOutput(ns("running_dest_ui")),
              shiny::div(
                class = "deconvolution-sample-picker",
                shiny$uiOutput(ns("result_picker_ui"))
              )
            ),
            shiny::div(
              class = "card-custom",
              bslib::card(
                bslib::card_header(
                  class = "bg-dark help-header",
                  "Deconvolution Metrics",
                  shiny::div(
                    class = "tooltip-bttn",
                    shiny::actionButton(
                      ns("conversion_samples_protein_tooltip_bttn"),
                      label = NULL,
                      icon = shiny::icon("circle-question")
                    )
                  )
                ),
                shiny::div(
                  class = "deconvolution-metrics-body",
                  DT::dataTableOutput(ns("deconvolution_data")),
                  shiny$uiOutput(ns("metrics_failure_msg"))
                )
              )
            )
          ),
          if (show_heatmap) {
            bslib::layout_sidebar(
              spectrum_card,
              sidebar = bslib::sidebar(
                width = 450,
                shiny::div(
                  class = "deconvolution-heatmap-card card-custom",
                  bslib::card(
                    bslib::card_header(class = "bg-dark", "Well Plate"),
                    shiny$div(
                      class = "heatmap-plot",
                      withWaiter(plotlyOutput(ns("heatmap"), height = "100%"))
                    )
                  )
                ),
                position = "right",
                open = TRUE
              ),
              border = FALSE
            )
          } else {
            spectrum_card
          }
        )
      )
    )
  )
}
