# app/logic/conversion_ui.R

box::use(
  app /
    logic /
    conversion_constants[
      empty_protein_table,
      chart_js,
      sequential_scales,
      qualitative_scales,
      gradient_scales,
      hits_table_names,
      popover_autoclose,
    ],
  app /
    logic /
    conversion_functions[
      format_scientific,
    ],
)

# Ki/kinact results interface
#' @export
ki_kinact_concentrations_tabs <- function(ns, local_ui_id, conc_result, units) {
  shiny::div(
    class = "result-conc-tab",
    shiny::div(
      class = "card-custom spectrum",
      bslib::card(
        bslib::card_header(
          class = "bg-dark help-header",
          "Mass Spectra",
          shiny::div(
            class = "box-header-settings-help",
            bslib::popover(
              shiny::icon("gear"),
              shiny::div(
                shiny::div(
                  class = "spectrum-radio-button",
                  shinyWidgets::radioGroupButtons(
                    ns(paste0(
                      local_ui_id,
                      "_kind"
                    )),
                    choices = c("3D", "Planar")
                  )
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
        full_screen = TRUE,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns(paste0(local_ui_id, "_spectra")),
            height = "100%"
          ),
          type = 1,
          color = "#7777f9"
        )
      )
    ),
    shiny::div(
      class = "card-custom binding",
      bslib::card(
        bslib::card_header(
          class = "bg-dark help-header",
          "Binding Curve",
          shiny::div(
            class = "tooltip-bttn",
            shiny::actionButton(
              ns("binding_curve_single_tooltip_bttn"),
              label = NULL,
              icon = shiny::icon("circle-question")
            )
          )
        ),
        full_screen = TRUE,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns(paste0(
              local_ui_id,
              "_binding_plot"
            )),
            height = "100%"
          ),
          type = 1,
          color = "#7777f9"
        )
      )
    ),
    shiny::div(
      class = "result-cards",
      shiny::div(
        class = "card-custom",
        bslib::card(
          bslib::card_header(
            class = "bg-dark help-header",
            htmltools::tagList(
              shiny::div(
                "k",
                htmltools::tags$sub("obs")
              )
            ),
            shiny::div(
              class = "tooltip-bttn",
              shiny::actionButton(
                ns("kobs_value_tooltip_bttn"),
                label = NULL,
                icon = shiny::icon("circle-question")
              )
            )
          ),
          shiny::div(
            class = "kobs-val",
            paste(
              format_scientific(conc_result$kobs),
              paste0(
                gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
                "⁻¹"
              )
            )
          )
        )
      ),
      shiny::div(
        class = "card-custom",
        bslib::card(
          bslib::card_header(
            class = "bg-dark help-header",
            "Binding Plateau",
            shiny::div(
              class = "tooltip-bttn",
              shiny::actionButton(
                ns("binding_plateau_tooltip_bttn"),
                label = NULL,
                icon = shiny::icon("circle-question")
              )
            )
          ),
          shiny::div(
            class = "kobs-val",
            paste0(format_scientific(conc_result$plateau), "%")
          )
        )
      ),
      shiny::div(
        class = "card-custom",
        bslib::card(
          bslib::card_header(
            class = "bg-dark help-header",
            "Velocity v",
            shiny::div(
              class = "tooltip-bttn",
              shiny::actionButton(
                ns("v_value_tooltip_bttn"),
                label = NULL,
                icon = shiny::icon("circle-question")
              )
            )
          ),
          shiny::div(
            class = "kobs-val",
            format_scientific(conc_result$v)
          )
        )
      )
    ),
    shiny::div(
      class = "card-custom hits",
      bslib::card(
        bslib::card_header(
          class = "bg-dark help-header d-flex justify-content-between",
          "Table View",
          shiny::div(
            class = "box-header-settings-help",
            bslib::popover(
              shiny::icon("gear"),
              shiny::div(
                shinyWidgets::materialSwitch(
                  ns(paste0(
                    local_ui_id,
                    "concentrations_table_view_binding_bar"
                  )),
                  label = "%-Binding Bar",
                  value = TRUE,
                  right = TRUE
                ),
                shinyWidgets::materialSwitch(
                  ns(paste0(
                    local_ui_id,
                    "concentrations_table_view_tot_binding_bar"
                  )),
                  label = "Total %-Binding Bar",
                  value = FALSE,
                  right = TRUE
                ),
                style = "margin-right: 20px;"
              ),
              title = NULL
            ),
            shiny::div(
              class = "tooltip-bttn",
              shiny::actionButton(
                ns("hits_table_tooltip_bttn"),
                label = NULL,
                icon = shiny::icon("circle-question")
              )
            )
          )
        ),
        full_screen = TRUE,
        shiny::div(
          class = "conc-hits-table",
          shinycssloaders::withSpinner(
            DT::DTOutput(ns(paste0(local_ui_id, "_hits"))),
            type = 1,
            color = "#7777f9"
          )
        )
      )
    )
  )
}

# Ki/kinact results interface
#' @export
ki_kinact_results_ui <- function(
  ns,
  hits_summary,
  concentrations,
  dynamic_ui_ids
) {
  # Generate the dynamic concentration panels
  concentration_panels <- lapply(seq_along(concentrations), function(i) {
    concentration <- concentrations[[i]]
    ui_id <- dynamic_ui_ids[[i]]

    bslib::nav_panel(
      title = paste0("[", concentration, "]"),
      shiny::div(
        class = "conversion-result-wrapper",
        shiny::uiOutput(ns(ui_id))
      ),
      shiny::tags$script(popover_autoclose)
    )
  })

  # bslib::navset_card_tab(
  #   id = ns("tabs"),
  static_panels <- list(
    bslib::nav_panel(
      title = "Hits",
      shiny::div(
        class = "conversion-result-wrapper hits-tab",
        # shiny::div(
        #   class = "tooltip-bttn hits-tab-tooltip",
        #   shiny::actionButton(
        #     ns("hits_table_tooltip_bttn"),
        #     label = NULL,
        #     icon = shiny::icon("circle-question")
        #   )
        # ),
        shiny::fluidRow(
          shiny::column(
            width = 2,
            align = "center",
            shiny::div(
              class = "hits-tab-checkboxes",
              shiny::div(
                class = "hits-tab-expand-box",
                shiny::checkboxInput(
                  ns("kikinact_hits_tab_expand"),
                  label = "Expand Samples",
                  value = TRUE
                )
              ),
              shiny::div(
                class = "hits-tab-na-box",
                shiny::checkboxInput(
                  ns("kikinact_hits_tab_na"),
                  label = "Include NA",
                  value = TRUE
                )
              )
            )
          ),
          shiny::column(
            width = 2,
            align = "center",
            shiny::div(
              class = "hits-table-control-select",
              shinyWidgets::pickerInput(
                ns("kikinact_hits_tab_sample_select"),
                label = "Select Samples",
                choices = unique(
                  hits_summary$`Sample ID`
                ),
                selected = unique(
                  hits_summary$`Sample ID`
                ),
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE
                )
              )
            )
          ),
          shiny::column(
            width = 2,
            align = "center",
            shiny::div(
              class = "hits-table-control-select",
              shinyWidgets::pickerInput(
                ns("kikinact_hits_tab_compound_select"),
                label = "Select Compounds",
                choices = unique(
                  hits_summary$`Cmp Name`
                )[
                  !is.na(unique(
                    hits_summary$`Cmp Name`
                  ))
                ],
                selected = unique(
                  hits_summary$`Cmp Name`
                )[
                  !is.na(unique(
                    hits_summary$`Cmp Name`
                  ))
                ],
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE
                )
              )
            )
          ),
          shiny::column(
            width = 2,
            align = "center",
            shiny::div(
              class = "hits-table-control-select",
              shinyWidgets::pickerInput(
                ns("kikinact_hits_tab_col_select"),
                label = "Select Columns",
                choices = names(hits_summary)[
                  !names(hits_summary) %in%
                    c("Sample ID", "Cmp Name", "truncSample_ID")
                ],
                selected = names(hits_summary)[
                  !names(hits_summary) %in%
                    c("Sample ID", "Cmp Name", "truncSample_ID")
                ][-c(1:2, 4:5, 7, 9)],
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE
                )
              )
            )
          ),
          shiny::column(
            width = 2,
            align = "center",
            shiny::div(
              class = "hits-table-control-select",
              shinyWidgets::pickerInput(
                ns("kikinact_binding_chart"),
                label = "Show Binding Bars",
                choices = c("%-Binding", "Total %-Binding"),
                selected = "Total %-Binding",
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE
                )
              )
            )
          )
        ),
        shiny::div(
          class = "hits-table-wrapper",
          shinycssloaders::withSpinner(
            DT::DTOutput(ns("kikinact_hits_tab")),
            type = 1,
            color = "#7777f9"
          )
        )
      ),
      shiny::tags$script(
        popover_autoclose
      )
    ),
    bslib::nav_panel(
      title = "Binding",
      shiny::div(
        class = "conversion-result-wrapper",
        shiny::div(
          class = "binding-analysis-tab",
          shiny::div(
            class = "card-custom",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                class = "bg-dark help-header",
                "Binding Curve",
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("binding_curve_tooltip_bttn"),
                    label = NULL,
                    icon = shiny::icon("circle-question")
                  )
                )
              ),
              bslib::card_body(
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                    ns("binding_plot"),
                    height = "100%"
                  ),
                  type = 1,
                  color = "#7777f9"
                )
              )
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                class = "bg-dark help-header",
                htmltools::tagList(
                  shiny::div(
                    "k",
                    htmltools::tags$sub("obs"),
                    " Curve"
                  )
                ),
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("kobs_curve_tooltip_bttn"),
                    label = NULL,
                    icon = shiny::icon("circle-question")
                  )
                )
              ),
              bslib::card_body(
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                    ns("kobs_plot"),
                    height = "100%"
                  ),
                  type = 1,
                  color = "#7777f9"
                )
              )
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                class = "bg-dark help-header",
                "Binding Analysis",
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("binding_analysis_tooltip_bttn"),
                    label = NULL,
                    icon = shiny::icon("circle-question")
                  )
                )
              ),
              bslib::card_body(
                shinycssloaders::withSpinner(
                  DT::DTOutput(ns("kobs_result")),
                  type = 1,
                  color = "#7777f9"
                )
              )
            )
          ),
          shiny::div(
            class = "result-cards",
            shiny::div(
              class = "card-custom",
              bslib::card(
                bslib::card_header(
                  class = "bg-dark help-header",
                  htmltools::tagList(
                    shiny::div(
                      "k",
                      htmltools::tags$sub("inact")
                    )
                  ),
                  shiny::div(
                    class = "tooltip-bttn",
                    shiny::actionButton(
                      ns("kinact_tooltip_bttn"),
                      label = NULL,
                      icon = shiny::icon("circle-question")
                    )
                  )
                ),
                shiny::div(
                  class = "kobs-val",
                  shinycssloaders::withSpinner(
                    shiny::uiOutput(ns("kinact")),
                    type = 1,
                    color = "#7777f9"
                  )
                )
              )
            ),
            shiny::div(
              class = "card-custom",
              bslib::card(
                bslib::card_header(
                  class = "bg-dark help-header",
                  htmltools::tagList(
                    shiny::div(
                      "K",
                      htmltools::tags$sub("i")
                    )
                  ),
                  shiny::div(
                    class = "tooltip-bttn",
                    shiny::actionButton(
                      ns("Ki_tooltip_bttn"),
                      label = NULL,
                      icon = shiny::icon("circle-question")
                    )
                  )
                ),
                shiny::div(
                  class = "kobs-val",
                  shinycssloaders::withSpinner(
                    shiny::uiOutput(ns("Ki")),
                    type = 1,
                    color = "#7777f9"
                  )
                )
              )
            ),
            shiny::div(
              class = "card-custom",
              bslib::card(
                bslib::card_header(
                  class = "bg-dark help-header",
                  htmltools::tagList(
                    shiny::div(
                      "K",
                      htmltools::tags$sub("i"),
                      "/ k",
                      htmltools::tags$sub("inact"),
                    )
                  ),
                  shiny::div(
                    class = "tooltip-bttn",
                    shiny::actionButton(
                      ns("Ki_kinact_tooltip_bttn"),
                      label = NULL,
                      icon = shiny::icon("circle-question")
                    )
                  )
                ),
                shiny::div(
                  class = "kobs-val",
                  shinycssloaders::withSpinner(
                    shiny::uiOutput(ns("Ki_kinact")),
                    type = 1,
                    color = "#7777f9"
                  )
                )
              )
            )
          )
        )
      ),
      shiny::tags$script(
        popover_autoclose
      )
    )
  )

  all_tabs <- c(static_panels, concentration_panels)

  do.call(
    bslib::navset_card_tab,
    c(
      list(id = ns("tabs")),
      all_tabs
    )
  )
}

# Binding results interface
#' @export
binding_results_ui <- function(ns, hits_summary) {
  bslib::navset_card_tab(
    id = ns("tabs"),
    bslib::nav_panel(
      title = "Hits",
      shiny::div(
        class = "conversion-result-wrapper hits-tab",
        shiny::fluidRow(
          shiny::column(
            width = 2,
            align = "center",
            shiny::div(
              class = "hits-tab-checkboxes",
              shiny::div(
                class = "hits-tab-expand-box",
                shiny::checkboxInput(
                  ns("relbinding_hits_tab_expand"),
                  label = "Expand Samples",
                  value = TRUE
                )
              ),
              shiny::div(
                class = "hits-tab-na-box",
                shiny::checkboxInput(
                  ns("relbinding_hits_tab_na"),
                  label = "Include NA",
                  value = TRUE
                )
              )
            )
          ),
          shiny::column(
            width = 2,
            align = "center",
            shiny::div(
              class = "hits-table-control-select",
              shinyWidgets::pickerInput(
                ns("relbinding_hits_tab_sample_select"),
                label = "Select Samples",
                choices = unique(
                  hits_summary$`Sample ID`
                ),
                selected = unique(
                  hits_summary$`Sample ID`
                ),
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE
                )
              )
            )
          ),
          shiny::column(
            width = 2,
            align = "center",
            shiny::div(
              class = "hits-table-control-select",
              shinyWidgets::pickerInput(
                ns("relbinding_hits_tab_compound_select"),
                label = "Select Compounds",
                choices = unique(
                  hits_summary$`Cmp Name`
                )[
                  !is.na(unique(
                    hits_summary$`Cmp Name`
                  ))
                ],
                selected = unique(
                  hits_summary$`Cmp Name`
                )[
                  !is.na(unique(
                    hits_summary$`Cmp Name`
                  ))
                ],
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE
                )
              )
            )
          ),
          shiny::column(
            width = 2,
            align = "center",
            shiny::div(
              class = "hits-table-control-select",
              shinyWidgets::pickerInput(
                ns("relbinding_hits_tab_col_select"),
                label = "Select Columns",
                choices = names(hits_summary)[
                  !names(hits_summary) %in%
                    c(
                      "Sample ID",
                      "Cmp Name",
                      if (length(units) == 2) {
                        c(units[["Concentration"]], units[["Time"]])
                      },
                      "truncSample_ID"
                    )
                ],
                selected = names(hits_summary)[
                  !names(hits_summary) %in%
                    c(
                      "Sample ID",
                      "Cmp Name",
                      if (length(units) == 2) {
                        c(units[["Concentration"]], units[["Time"]])
                      },
                      "truncSample_ID"
                    )
                ][-c(1:2, 4:5, 7, 9)],
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE
                )
              )
            )
          ),
          shiny::column(
            width = 2,
            align = "center",
            shiny::div(
              class = "hits-table-control-select",
              shinyWidgets::pickerInput(
                ns("relbinding_binding_chart"),
                label = "Show Binding Bars",
                choices = c("%-Binding", "Total %-Binding"),
                selected = "Total %-Binding",
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE
                )
              )
            )
          )
        ),
        shiny::div(
          class = "hits-table-wrapper",
          shinycssloaders::withSpinner(
            DT::DTOutput(ns("relbinding_hits_tab")),
            type = 1,
            color = "#7777f9"
          )
        )
      )
    ),
    bslib::nav_panel(
      title = "Samples View",
      shiny::div(
        class = "conversion-result-wrapper",
        shiny::div(
          class = "conversion-samples-wrapper",
          shiny::div(
            class = "conversion-samples-control",
            shiny::div(
              class = "sample-cmp-prot-picker",
              shinyWidgets::pickerInput(
                ns("conversion_sample_picker"),
                "Select Sample",
                choices = if (anyNA(hits_summary$`Cmp Name`)) {
                  # Extract the vectors
                  hits_vec <- unique(hits_summary$`Sample ID`[
                    !is.na(hits_summary$`Cmp Name`)
                  ])
                  no_hits_vec <- unique(hits_summary$`Sample ID`[is.na(
                    hits_summary$`Cmp Name`
                  )])

                  choices_list <- list()
                  if (length(hits_vec)) {
                    choices_list[["Hits"]] <- stats::setNames(
                      hits_vec,
                      hits_vec
                    )
                  }
                  if (length(no_hits_vec)) {
                    choices_list[["No Hits"]] <- stats::setNames(
                      no_hits_vec,
                      no_hits_vec
                    )
                  }

                  choices_list
                } else {
                  unique(hits_summary$`Sample ID`)
                }
              )
            ),
            shiny::div(
              class = "conversion-samples-stats",
              shiny::div(
                class = "card-custom",
                bslib::card(
                  bslib::card_header(
                    class = "bg-dark help-header",
                    "Protein",
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
                    class = "kobs-val",
                    shinycssloaders::withSpinner(
                      shiny::uiOutput(ns("samples_selected_protein")),
                      type = 1,
                      color = "#7777f9"
                    )
                  )
                )
              ),
              shiny::div(
                class = "card-custom",
                bslib::card(
                  bslib::card_header(
                    class = "bg-dark help-header",
                    "Total %-Binding",
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("total_pct_bind_tooltip_bttn"),
                        label = NULL,
                        icon = shiny::icon("circle-question")
                      )
                    )
                  ),
                  shiny::div(
                    class = "kobs-val",
                    shinycssloaders::withSpinner(
                      shiny::uiOutput(ns(
                        "samples_total_pct_binding"
                      )),
                      type = 1,
                      color = "#7777f9"
                    )
                  )
                )
              )
            )
          ),
          shiny::div(
            class = "card-custom cmp-table",
            id = "upper-section",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Table View",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::popover(
                    shiny::icon("gear"),
                    shiny::div(
                      shinyWidgets::materialSwitch(
                        ns(
                          "samples_table_view_binding_bar"
                        ),
                        label = "%-Binding Bar",
                        value = TRUE,
                        right = TRUE
                      ),
                      shinyWidgets::materialSwitch(
                        ns(
                          "samples_table_view_tot_binding_bar"
                        ),
                        label = "Total %-Binding Bar",
                        value = FALSE,
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
              shinycssloaders::withSpinner(
                DT::DTOutput(
                  ns("samples_table_view")
                ),
                type = 1,
                color = "#7777f9"
              ),
              full_screen = TRUE
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header",
                "Compound Distribution",
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("mass_spectra_tooltip_bttn"),
                    label = NULL,
                    icon = shiny::icon("circle-question")
                  )
                )
              ),
              shinycssloaders::withSpinner(
                shiny::uiOutput(
                  ns("samples_compound_distribution_ui")
                ),
                type = 1,
                color = "#7777f9"
              ),
              full_screen = TRUE
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Annotated Spectrum",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::popover(
                    shiny::icon("gear"),
                    shiny::div(
                      shinyWidgets::materialSwitch(
                        ns("sample_view_spectrum_diff"),
                        label = "Show Distance",
                        value = TRUE,
                        right = TRUE
                      ),
                      shinyWidgets::materialSwitch(
                        ns("sample_view_spectrum_annotation"),
                        label = "Annotate Hits",
                        value = FALSE,
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
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("samples_annotated_spectrum"),
                  height = "100%"
                ),
                type = 1,
                color = "#7777f9"
              ),
              full_screen = TRUE
            )
          )
        )
      ),
      shiny::tags$script(
        popover_autoclose
      )
    ),
    bslib::nav_panel(
      title = "Compounds View",
      shiny::div(
        class = "conversion-result-wrapper",
        shiny::div(
          class = "conversion-samples-wrapper",
          shiny::div(
            class = "conversion-samples-control",
            shiny::div(
              class = "sample-cmp-prot-picker",
              shinyWidgets::pickerInput(
                ns("conversion_compound_picker"),
                "Select Compound",
                choices = unique(
                  hits_summary$`Cmp Name`
                )[
                  !is.na(unique(
                    hits_summary$`Cmp Name`
                  ))
                ]
              )
            ),
            shiny::div(
              class = "conversion-samples-stats",
              shiny::div(
                class = "card-custom",
                bslib::card(
                  bslib::card_header(
                    class = "bg-dark help-header",
                    "Compound",
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
                    class = "kobs-val",
                    shinycssloaders::withSpinner(
                      shiny::uiOutput(ns(
                        "compounds_selected_compound"
                      )),
                      type = 1,
                      color = "#7777f9"
                    )
                  )
                )
              ),
              shiny::div(
                class = "card-custom",
                bslib::card(
                  bslib::card_header(
                    class = "bg-dark help-header",
                    "Total %-Binding",
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("total_pct_bind_tooltip_bttn"),
                        label = NULL,
                        icon = shiny::icon("circle-question")
                      )
                    )
                  ),
                  shiny::div(
                    class = "kobs-val",
                    shinycssloaders::withSpinner(
                      shiny::uiOutput(ns(
                        "compounds_total_pct_binding"
                      )),
                      type = 1,
                      color = "#7777f9"
                    )
                  )
                )
              )
            )
          ),
          shiny::div(
            class = "card-custom cmp-table",
            id = "upper-section",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Table View",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::popover(
                    shiny::icon("gear"),
                    shiny::div(
                      shinyWidgets::materialSwitch(
                        ns("compounds_table_view_binding_bar"),
                        label = "%-Binding Bar",
                        value = TRUE,
                        right = TRUE
                      ),
                      shinyWidgets::materialSwitch(
                        ns("compounds_table_view_tot_binding_bar"),
                        label = "Total %-Binding Bar",
                        value = FALSE,
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
              shinycssloaders::withSpinner(
                DT::DTOutput(
                  ns("compounds_table_view")
                ),
                type = 1,
                color = "#7777f9"
              ),
              full_screen = TRUE
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header",
                "Compound Distribution",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::popover(
                    shiny::icon("gear"),
                    shiny::div(
                      shiny::radioButtons(
                        inputId = ns("cmp_distribution_scale"),
                        label = "Range",
                        choices = c(
                          "Maximum",
                          "100"
                        )
                      ),
                      shiny::uiOutput(ns(
                        "compounds_distribution_labels_ui"
                      )),
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
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("compounds_compound_distribution"),
                  height = "100%"
                ),
                type = 1,
                color = "#7777f9"
              ),
              full_screen = TRUE
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header",
                "Annotated Spectrum",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::popover(
                    shiny::icon("gear"),
                    shiny::div(
                      shiny::uiOutput(ns(
                        "compounds_spectrum_labels_ui"
                      )),
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
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("compounds_annotated_spectrum"),
                  height = "100%"
                ),
                type = 1,
                color = "#7777f9"
              ),
              full_screen = TRUE
            )
          )
        )
      ),
      shiny::tags$script(
        popover_autoclose
      )
    ),
    bslib::nav_panel(
      title = "Proteins View",
      shiny::div(
        class = "conversion-result-wrapper",
        shiny::div(
          class = "conversion-samples-wrapper",
          shiny::div(
            class = "conversion-samples-control",
            shiny::div(
              class = "sample-cmp-prot-picker",
              shinyWidgets::pickerInput(
                ns("conversion_protein_picker"),
                "Select Protein",
                choices = unique(
                  hits_summary$`Protein`
                )[
                  !is.na(unique(hits_summary$`Protein`))
                ]
              )
            ),
            shiny::div(
              class = "conversion-samples-stats",
              shiny::div(
                class = "card-custom",
                bslib::card(
                  bslib::card_header(
                    class = "bg-dark help-header",
                    "Protein",
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
                    class = "kobs-val",
                    shinycssloaders::withSpinner(
                      shiny::uiOutput(ns(
                        "proteins_selected_protein"
                      )),
                      type = 1,
                      color = "#7777f9"
                    )
                  )
                )
              ),
              shiny::div(
                class = "card-custom",
                bslib::card(
                  bslib::card_header(
                    class = "bg-dark help-header d-flex justify-content-between",
                    "Total %-Binding",
                    shiny::div(
                      class = "box-header-settings-help",
                      bslib::popover(
                        shiny::icon("gear"),
                        shiny::div(
                          shiny::uiOutput(ns(
                            "proteins_total_pct_binding"
                          )),
                          style = "margin-right: 20px;"
                        ),
                        title = NULL
                      ),
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::actionButton(
                          ns("total_pct_bind_tooltip_bttn"),
                          label = NULL,
                          icon = shiny::icon("circle-question")
                        )
                      )
                    )
                  ),
                  shiny::div(
                    class = "kobs-val",
                    shinycssloaders::withSpinner(
                      shiny::uiOutput(ns("total_pct_prot_binding")),
                      type = 1,
                      color = "#7777f9"
                    )
                  )
                )
              )
            )
          ),
          shiny::div(
            class = "card-custom cmp-table",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Table View",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::popover(
                    shiny::icon("gear"),
                    shiny::div(
                      shinyWidgets::materialSwitch(
                        ns("proteins_table_view_binding_bar"),
                        label = "%-Binding Bar",
                        value = TRUE,
                        right = TRUE
                      ),
                      shinyWidgets::materialSwitch(
                        ns("proteins_table_view_tot_binding_bar"),
                        label = "Total %-Binding Bar",
                        value = FALSE,
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
              shinycssloaders::withSpinner(
                DT::DTOutput(
                  ns("proteins_table_view")
                ),
                type = 1,
                color = "#7777f9"
              ),
              full_screen = TRUE
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Compound Distribution",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::popover(
                    shiny::icon("gear"),
                    shiny::div(
                      shiny::radioButtons(
                        inputId = ns("protein_distribution_scale"),
                        label = "Range",
                        choices = c(
                          "Maximum",
                          "100"
                        )
                      ),
                      shiny::uiOutput(ns(
                        "proteins_distribution_labels_ui"
                      )),
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
              shinycssloaders::withSpinner(
                shiny::uiOutput(ns(
                  "proteins_present_compounds_ui"
                )),
                type = 1,
                color = "#7777f9"
              ),
              full_screen = TRUE
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Annotated Spectrum",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::popover(
                    shiny::icon("gear"),
                    shiny::div(
                      shiny::uiOutput(ns(
                        "proteins_spectrum_labels_ui"
                      )),
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
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("proteins_annotated_spectrum"),
                  height = "100%"
                ),
                type = 1,
                color = "#7777f9"
              ),
              full_screen = TRUE
            )
          )
        )
      ),
      shiny::tags$script(
        popover_autoclose
      )
    ),
    bslib::nav_item(
      id = ns("conversion_tab_items"),
      class = "conversion-tab-item-wrapper",
      shiny::div(
        class = "conversion-tab-items",
        shiny::div(
          class = "conversion-tab-items-truncate",
          shiny::div(
            class = "conversion-tab-items-label",
            shiny::HTML("Short Sample IDs")
          ),
          shinyWidgets::materialSwitch(
            ns("truncate_names"),
            label = NULL,
            value = TRUE
          )
        ),
        shiny::uiOutput(ns("color_variable_ui")),
        shiny::selectInput(
          ns("color_scale"),
          label = NULL,
          choices = NULL
        ),
        shiny::div(
          class = "tooltip-bttn",
          shiny::actionButton(
            ns("conversion_tooltip_bttn"),
            label = NULL,
            icon = shiny::icon("circle-question")
          )
        )
      )
    )
  )
}

# Declaration interface
#' @export
conversion_declaration_ui <- function(
  ns,
  proteins_status = "",
  compounds_status = "",
  samples_status = ""
) {
  if (proteins_status == "confirmed") {
    proteins_control_buttons <- shiny::div(
      class = "table-control-buttons",
      shinyjs::disabled(
        shiny::actionButton(
          ns("confirm_proteins"),
          label = "Saved",
          icon = shiny::icon("check"),
          width = "100%"
        )
      ),
      shiny::actionButton(
        ns("edit_proteins"),
        label = "Edit",
        icon = shiny::icon("pen-to-square"),
        width = "100%"
      ),
      shinyjs::disabled(
        shiny::actionButton(
          ns("clear_proteins"),
          label = "Clear",
          icon = shiny::icon("eraser"),
          width = "100%"
        )
      )
    )
  } else {
    proteins_control_buttons <- shiny::div(
      class = "table-control-buttons",
      shinyjs::disabled(
        shiny::actionButton(
          ns("confirm_proteins"),
          label = "Save",
          icon = shiny::icon("bookmark"),
          width = "100%"
        )
      ),
      shinyjs::disabled(
        shiny::actionButton(
          ns("edit_proteins"),
          label = "Edit",
          icon = shiny::icon("pen-to-square"),
          width = "100%"
        )
      ),
      shiny::actionButton(
        ns("clear_proteins"),
        label = "Clear",
        icon = shiny::icon("eraser"),
        width = "100%"
      )
    )
  }

  if (compounds_status == "confirmed") {
    compounds_control_buttons <- shiny::div(
      class = "table-control-buttons",
      shinyjs::disabled(
        shiny::actionButton(
          ns("confirm_compounds"),
          label = "Saved",
          icon = shiny::icon("check"),
          width = "100%"
        )
      ),
      shiny::actionButton(
        ns("edit_compounds"),
        label = "Edit",
        icon = shiny::icon("pen-to-square"),
        width = "100%"
      ),
      shinyjs::disabled(
        shiny::actionButton(
          ns("clear_compounds"),
          label = "Clear",
          icon = shiny::icon("eraser"),
          width = "100%"
        )
      )
    )
  } else {
    compounds_control_buttons <- shiny::div(
      class = "table-control-buttons",
      shinyjs::disabled(
        shiny::actionButton(
          ns("confirm_compounds"),
          label = "Save",
          icon = shiny::icon("bookmark"),
          width = "100%"
        )
      ),
      shinyjs::disabled(
        shiny::actionButton(
          ns("edit_compounds"),
          label = "Edit",
          icon = shiny::icon("pen-to-square"),
          width = "100%"
        )
      ),
      shiny::actionButton(
        ns("clear_compounds"),
        label = "Clear",
        icon = shiny::icon("eraser"),
        width = "100%"
      )
    )
  }

  bslib::navset_card_tab(
    id = ns("tabs"),
    bslib::nav_panel(
      "Proteins",
      shinyjs::useShinyjs(),
      waiter::useWaiter(),
      shiny::div(
        class = "comp-prot-controls",
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::div(
              class = "table-input",
              shiny::fileInput(
                ns("proteins_fileinput"),
                "",
                multiple = FALSE,
                accept = c(".csv", ".tsv", ".xlsx", ".xls", ".txt")
              )
            )
          ),
          shiny::column(
            width = 1,
            shiny::div(
              class = "tooltip-bttn",
              shiny::actionButton(
                ns("fileinput_tooltip_bttn"),
                label = NULL,
                icon = shiny::icon("circle-question")
              )
            )
          ),
          shiny::column(
            width = 2,
            shiny::textOutput(ns("proteins_table_info")),
          ),
          shiny::column(
            width = 5,
            proteins_control_buttons
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          rhandsontable::rHandsontableOutput(
            ns("proteins_table")
          ),
          table_legend
        )
      ),
      keybind_menu_ui,
      shiny::tags$script(
        popover_autoclose
      )
    ),
    bslib::nav_panel(
      "Compounds",
      shiny::div(
        class = "comp-prot-controls",
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::div(
              class = "table-input",
              shiny::fileInput(
                ns("compounds_fileinput"),
                "",
                multiple = FALSE,
                accept = c(".csv", ".tsv", ".xlsx", ".xls", ".txt")
              )
            )
          ),
          shiny::column(
            width = 1,
            shiny::div(
              class = "tooltip-bttn",
              shiny::actionButton(
                ns("fileinput_tooltip_bttn"),
                label = NULL,
                icon = shiny::icon("circle-question")
              )
            )
          ),
          shiny::column(
            width = 2,
            shiny::textOutput(ns("compounds_table_info"))
          ),
          shiny::column(
            width = 5,
            compounds_control_buttons
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          rhandsontable::rHandsontableOutput(ns("compounds_table")),
          table_legend
        )
      ),
      keybind_menu_ui,
      shiny::tags$script(
        popover_autoclose
      )
    ),
    bslib::nav_panel(
      "Samples",
      shiny::div(
        class = "samples-controls",
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::div(
              class = "table-input",
              shiny::fileInput(
                ns("samples_fileinput"),
                "Select File",
                multiple = FALSE,
                accept = c(".rds")
              )
            )
          ),
          shiny::column(
            width = 2,
            shiny::div(
              class = "sample-declaration-info-ui",
              shiny::div(
                class = "tooltip-bttn",
                shiny::actionButton(
                  ns("resultinput_tooltip_bttn"),
                  label = NULL,
                  icon = shiny::icon("circle-question")
                )
              ),
              shiny::textOutput(ns("sample_number_info"))
            )
          ),
          shiny::column(
            width = 2,
            shiny::textOutput(ns("samples_table_info"))
          ),
          shiny::column(
            width = 3,
            shiny::div(
              class = "table-control-buttons",
              bslib::tooltip(
                shiny::div(
                  style = "width: 100%;",
                  shinyjs::disabled(
                    shiny::actionButton(
                      ns("confirm_samples"),
                      label = NULL,
                      icon = shiny::icon("bookmark"),
                      width = "100%"
                    )
                  )
                ),
                "Confirm Sample Table",
                placement = "top"
              ),
              bslib::tooltip(
                shiny::div(
                  style = "width: 100%;",
                  shinyjs::disabled(
                    shiny::actionButton(
                      ns("edit_samples"),
                      label = NULL,
                      icon = shiny::icon("pen-to-square"),
                      width = "100%"
                    )
                  )
                ),
                "Edit Sample Table",
                placement = "top"
              ),
              bslib::tooltip(
                shiny::div(
                  style = "width: 100%;",
                  shiny::actionButton(
                    ns("clear_samples"),
                    label = NULL,
                    icon = shiny::icon("eraser"),
                    width = "100%"
                  )
                ),
                "Clear Sample Table",
                placement = "top"
              )
            )
          ),
          shiny::column(
            width = 1,
            shiny::div(
              class = "unit-selectors",
              conc_unit_input_ui(ns)
            )
          ),
          shiny::column(
            width = 1,
            shiny::div(
              class = "unit-selectors",
              time_unit_input_ui(ns)
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          rhandsontable::rHandsontableOutput(ns("samples_table"))
        )
      ),
      sample_table_legend,
      keybind_menu_ui,
      shiny::tags$script(
        popover_autoclose
      )
    ),
    bslib::nav_item(
      id = ns("declaration_info"),
      class = "conversion-tab-item-wrapper",
      shiny::uiOutput(ns("declaration_info_ui"))
    )
  )
}

# Time unit choices
#' @export
time_unit_input_ui <- function(ns) {
  shinyWidgets::pickerInput(
    inputId = ns("time_unit"),
    label = "Time Unit",
    choices = c("s", "min"),
    choicesOpt = list(
      content = {
        w_col1 <- "45px"
        w_col2 <- "95px"
        units <- c("s", "min")
        names <- c("seconds", "minutes")
        sprintf(
          paste0(
            "<span style='display: inline-block; width: %s; font-weight: 700; text-align: left;'>%s</span>",
            "<span style='display: inline-block; width: %s; font-style: italic; text-align: right;'>%s</span>"
          ),
          w_col1,
          units,
          w_col2,
          names
        )
      }
    ),
    options = shinyWidgets::pickerOptions(
      size = 10,
      showContent = FALSE,
      alignRight = TRUE
    )
  )
}

# Concentration unit choices
#' @export
conc_unit_input_ui <- function(ns) {
  shinyWidgets::pickerInput(
    inputId = ns("conc_unit"),
    label = "Conc. Unit",
    choices = c("M", "mM", "μM", "nM", "pM"),
    choicesOpt = list(
      content = {
        w_col1 <- "50px"
        w_col2 <- "95px"
        w_col3 <- "40px"
        units <- c("M", "mM", "μM", "nM", "pM")
        names <- c(
          "molar",
          "millimolar",
          "micromolar",
          "nanomolar",
          "picomolar"
        )
        powers <- c("10⁰", "10⁻³", "10⁻⁶", "10⁻⁹", "10⁻¹²")

        sprintf(
          paste0(
            "<span style='display: inline-block; width: %s; font-weight: 700;'>%s</span>",
            "<span style='display: inline-block; width: %s; font-style: italic;'>%s</span>",
            "<span style='display: inline-block; width: %s; text-align: right;'>%s</span>"
          ),
          w_col1,
          units,
          w_col2,
          names,
          w_col3,
          powers
        )
      }
    ),
    options = shinyWidgets::pickerOptions(
      size = 10,
      showContent = FALSE,
      alignRight = TRUE
    )
  )
}

# keybind menu ui
#' @export
keybind_menu_ui <- shiny::div(
  class = "shortcut-bar",
  shiny::div(
    class = "shortcut-item",
    shiny::span(class = "key", "Ctrl"),
    " + ",
    shiny::span(class = "key", "C"),
    " Copy"
  ),
  shiny::div(
    class = "shortcut-item",
    shiny::span(class = "key", "Ctrl"),
    " + ",
    shiny::span(class = "key", "V"),
    " Paste"
  ),
  shiny::div(
    class = "shortcut-item",
    shiny::span(class = "key", "Ctrl"),
    " + ",
    shiny::span(class = "key", "X"),
    " Cut"
  ),
  shiny::div(
    class = "shortcut-item",
    shiny::span(class = "key", "←"),
    shiny::span(class = "key", "↑"),
    shiny::span(class = "key", "→"),
    shiny::span(class = "key", "↓"),
    " Move"
  ),
  shiny::div(
    class = "shortcut-item",
    shiny::span(class = "key", "Tab"),
    " Move Columns"
  ),
  shiny::div(
    class = "shortcut-item",
    shiny::span(class = "key", "Enter"),
    " Move Rows"
  )
)

# Table Legend UI
#' @export
table_legend <- shiny::div(
  class = "table-legend",
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell duplicated-names"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= duplicated names"
    )
  ),
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell numeric-mass"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= non-numeric mass values"
    )
  ),
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell duplicated-mass"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= mass shifts of one protein duplicated (proximity < peak tolerance)"
    )
  ),
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell duplicated-mass-between"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= mass shifts duplicated between different proteins (proximity < peak tolerance)"
    )
  )
)

# Sample table legend UI
#' @export
sample_table_legend <- shiny::div(
  class = "table-legend",
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell duplicated-names"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= duplicated compounds"
    )
  ),
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell numeric-mass"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= unknown name"
    )
  ),
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell duplicated-mass"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= protein contains duplicated compound masses (proximity < peak tolerance)"
    )
  )
)
