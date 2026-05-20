# app/logic/conversion_ui.R

box::use(
  app /
    logic /
    conversion_constants[
      popover_autoclose,
    ],
  app /
    logic /
    conversion_functions[
      format_scientific,
      stats_histogram,
      stats_boxplot,
      stats_scatter,
      stats_violin,
    ],
  app /
    logic /
    plot_download[
      card_settings_popover,
      plot_dl_popover,
      table_dl_buttons,
      table_dl_popover
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
          class = "bg-dark help-header d-flex justify-content-between",
          "Mass Spectra",
          shiny::div(
            class = "box-header-settings-help",
            card_settings_popover(
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
              )
            ),
            plot_dl_popover(ns, paste0(local_ui_id, "_spectra")),
            bslib::tooltip(
              shiny::div(
                class = "tooltip-bttn",
                shiny::tags$button(
                  type = "button",
                  class = "btn btn-default",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', Math.random());",
                    ns("mass_spectra_tooltip_bttn")
                  ),
                  shiny::icon("circle-question")
                )
              ),
              "Help",
              placement = "top"
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
          class = "bg-dark help-header d-flex justify-content-between",
          "Binding Curve",
          shiny::div(
            class = "box-header-settings-help",
            plot_dl_popover(ns, paste0(local_ui_id, "_binding")),
            bslib::tooltip(
              shiny::div(
                class = "tooltip-bttn",
                shiny::actionButton(
                  ns("binding_curve_single_tooltip_bttn"),
                  label = NULL,
                  icon = shiny::icon("circle-question")
                )
              ),
              "Help",
              placement = "top"
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
            bslib::tooltip(
              shiny::div(
                class = "tooltip-bttn",
                shiny::actionButton(
                  ns("kobs_value_tooltip_bttn"),
                  label = NULL,
                  icon = shiny::icon("circle-question")
                )
              ),
              "Help",
              placement = "top"
            )
          ),
          shiny::div(
            class = "result-card-content",
            shiny::div(
              class = "main-result",
              shiny::HTML(paste(
                format_scientific(conc_result$kobs),
                paste0(
                  gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
                  "⁻¹"
                )
              ))
            ),
            shiny::div(
              class = "error-result",
              shiny::HTML(paste(
                "±",
                if (is.na(conc_result$kobs_se)) {
                  "n.a."
                } else {
                  format_scientific(conc_result$kobs_se)
                }
              ))
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
            bslib::tooltip(
              shiny::div(
                class = "tooltip-bttn",
                shiny::actionButton(
                  ns("binding_plateau_tooltip_bttn"),
                  label = NULL,
                  icon = shiny::icon("circle-question")
                )
              ),
              "Help",
              placement = "top"
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
            bslib::tooltip(
              shiny::div(
                class = "tooltip-bttn",
                shiny::actionButton(
                  ns("v_value_tooltip_bttn"),
                  label = NULL,
                  icon = shiny::icon("circle-question")
                )
              ),
              "Help",
              placement = "top"
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
            card_settings_popover(
              shiny::div(
                shinyWidgets::materialSwitch(
                  ns(paste0(
                    local_ui_id,
                    "concentrations_table_view_binding_bar"
                  )),
                  label = "Binding [%] Bar",
                  value = TRUE,
                  right = TRUE
                ),
                shinyWidgets::materialSwitch(
                  ns(paste0(
                    local_ui_id,
                    "concentrations_table_view_tot_binding_bar"
                  )),
                  label = "Tot. Binding [%] Bar",
                  value = FALSE,
                  right = TRUE
                ),
                style = "margin-right: 20px;"
              )
            ),
            table_dl_popover(ns, paste0(local_ui_id, "_hits")),
            bslib::tooltip(
              shiny::div(
                class = "tooltip-bttn",
                shiny::actionButton(
                  ns("hits_table_tooltip_bttn"),
                  label = NULL,
                  icon = shiny::icon("circle-question")
                )
              ),
              "Help",
              placement = "top"
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
                class = "bg-dark help-header d-flex justify-content-between",
                "Binding Curve",
                shiny::div(
                  class = "box-header-settings-help",
                  plot_dl_popover(ns, "binding"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("binding_curve_tooltip_bttn"),
                        label = NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
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
                class = "bg-dark help-header d-flex justify-content-between",
                htmltools::tagList(
                  shiny::div(
                    "k",
                    htmltools::tags$sub("obs"),
                    " Curve"
                  )
                ),
                shiny::div(
                  class = "box-header-settings-help",
                  plot_dl_popover(ns, "kobs"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("kobs_curve_tooltip_bttn"),
                        label = NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
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
                class = "bg-dark help-header d-flex justify-content-between",
                "Binding Analysis",
                shiny::div(
                  class = "box-header-settings-help",
                  table_dl_popover(ns, "kobs_result"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("binding_analysis_tooltip_bttn"),
                        label = NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
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
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("kinact_tooltip_bttn"),
                        label = NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
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
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("Ki_tooltip_bttn"),
                        label = NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
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
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("Ki_kinact_tooltip_bttn"),
                        label = NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
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

# Summary interface
#' @export
summary_results_ui <- function(ns, batch_control) {
  bslib::navset_card_tab(
    id = ns("summary_tabs"),
    bslib::nav_panel(
      title = "Protocol",
      shiny::div(
        class = "protocol-tab",
        shiny::div(
          class = "protocol-left-col",
          shiny::div(
            class = "card-custom protocol-log-card",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Conversion Log",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::tooltip(
                    shiny::div(
                      bslib::popover(
                        shiny::icon("arrow-up-from-bracket"),
                        shiny::div(
                          class = "plot-dl-popover",
                          shiny::div(class = "plot-dl-label", "File Format"),
                          shiny::div(
                            class = "plot-dl-buttons",
                            shiny::actionButton(
                              ns("copy_protocol_log"),
                              "Clip",
                              icon = shiny::icon("clipboard"),
                              class = "btn-sm btn-default"
                            ),
                            shiny::actionButton(
                              ns("save_protocol_log"),
                              "Save",
                              icon = shiny::icon("file-lines"),
                              class = "btn-sm btn-default"
                            )
                          )
                        ),
                        title = "Export Log"
                      )
                    ),
                    "Export",
                    placement = "top"
                  ),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("protocol_log_help_bttn"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(
                shiny::div(
                  class = "protocol-log-wrapper",
                  shiny::div(
                    id = ns("protocol_log_body"),
                    class = "protocol-log-body",
                    shiny::uiOutput(ns("summary_protocol"))
                  ),
                  shiny::actionButton(
                    ns("protocol_scroll_top"),
                    NULL,
                    icon = shiny::icon("arrow-up"),
                    title = "Jump to top"
                  ),
                  shiny::actionButton(
                    ns("protocol_scroll_bot"),
                    NULL,
                    icon = shiny::icon("arrow-down"),
                    title = "Jump to bottom"
                  ),
                  shiny::tags$script(shiny::HTML(sprintf(
                    "
                  (function() {
                    var cId = '%s', tId = '%s', bId = '%s';
                    function setup() {
                      var c = document.getElementById(cId);
                      var t = document.getElementById(tId);
                      var b = document.getElementById(bId);
                      if (!c || !t || !b) { setTimeout(setup, 100); return; }
                      function update() {
                        t.disabled = c.scrollTop <= 10;
                        b.disabled = (c.scrollHeight - c.scrollTop - c.clientHeight) <= 10;
                      }
                      c.addEventListener('scroll', update);
                      new MutationObserver(update).observe(c, { childList: true, subtree: true });
                      t.addEventListener('click', function() {
                        c.scrollTo({ top: 0, behavior: 'smooth' });
                      });
                      b.addEventListener('click', function() {
                        c.scrollTo({ top: c.scrollHeight, behavior: 'smooth' });
                      });
                      update();
                    }
                    setup();
                  })();
                ",
                    ns("protocol_log_body"),
                    ns("protocol_scroll_top"),
                    ns("protocol_scroll_bot")
                  ))),
                )
              )
            )
          ),
          shiny::div(
            class = "protocol-secondary-grid",
            shiny::div(
              class = "card-custom",
              bslib::card(
                bslib::card_header(
                  class = "bg-dark help-header d-flex justify-content-between",
                  "Alerts",
                  shiny::div(
                    class = "box-header-settings-help",
                    bslib::tooltip(
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::actionButton(
                          ns("pstat_alerts_help"),
                          NULL,
                          icon = shiny::icon("circle-question")
                        )
                      ),
                      "Help",
                      placement = "top"
                    )
                  )
                ),
                bslib::card_body(
                  class = "protocol-stat-body",
                  shiny::uiOutput(ns("pstat_alerts"))
                )
              )
            ),
            shiny::div(
              class = "card-custom",
              bslib::card(
                bslib::card_header(
                  class = "bg-dark help-header d-flex justify-content-between",
                  "Warnings",
                  shiny::div(
                    class = "box-header-settings-help",
                    bslib::tooltip(
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::actionButton(
                          ns("pstat_warnings_help"),
                          NULL,
                          icon = shiny::icon("circle-question")
                        )
                      ),
                      "Help",
                      placement = "top"
                    )
                  )
                ),
                bslib::card_body(
                  class = "protocol-stat-body",
                  shiny::uiOutput(ns("pstat_warnings"))
                )
              )
            )
          )
        ),
        shiny::div(
          class = "protocol-stats-grid",
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Screened Samples",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("pstat_n_samples_help"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(
                class = "protocol-stat-body",
                shiny::uiOutput(ns("pstat_n_samples")),
              )
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Hits Detected",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("pstat_n_hits_help"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(
                class = "protocol-stat-body",
                shiny::uiOutput(ns("pstat_n_hits"))
              )
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Proteins Detected",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("pstat_n_proteins_help"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(
                class = "protocol-stat-body",
                shiny::uiOutput(ns("pstat_n_proteins"))
              )
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Compounds Detected",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("pstat_n_compounds_help"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(
                class = "protocol-stat-body",
                shiny::uiOutput(ns("pstat_n_compounds"))
              )
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Correct [%]",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("pstat_correct_help"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(
                class = "protocol-stat-body",
                shiny::uiOutput(ns("pstat_correct"))
              )
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Unmatched [%]",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("pstat_unmatched_help"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(
                class = "protocol-stat-body",
                shiny::uiOutput(ns("pstat_unmatched"))
              )
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Peak Tolerance",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("pstat_peak_tol_help"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(
                class = "protocol-stat-body",
                shiny::uiOutput(ns("pstat_peak_tol"))
              )
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Max. Stoichiometry",
                shiny::div(
                  class = "box-header-settings-help",
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("pstat_max_stoich_help"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(
                class = "protocol-stat-body",
                shiny::uiOutput(ns("pstat_max_stoich"))
              )
            )
          )
        )
      )
    ),
    bslib::nav_panel(
      title = "Statistics",
      shiny::div(
        class = "conversion-result-wrapper",
        shiny::div(
          class = "statistics-tab",
          shiny::div(
            class = "input-stat-panel",
            shiny::div(
              class = "input-panel",
              shiny::div(
                class = "panel-group",
                shiny::tags$label(
                  "Hit Rate Parameter"
                ),
                shinyWidgets::radioGroupButtons(
                  ns("stats_show_metric"),
                  label = NULL,
                  choices = c("Correct", "Unmatched"),
                  selected = "Correct",
                  size = "sm"
                )
              ),
              shiny::div(
                class = "panel-group",
                shiny::tags$label(
                  "Include only hits"
                ),
                shinyWidgets::radioGroupButtons(
                  ns("stats_exclude_extremes"),
                  label = NULL,
                  choices = c("All", "Hits only"),
                  selected = "All",
                  size = "sm"
                )
              )
            ),
            shiny::div(
              class = "statistics-correct-unmatched-cards",
              shiny::div(
                class = "card-custom",
                bslib::card(
                  bslib::card_header(
                    class = "bg-dark help-header d-flex justify-content-between",
                    "Correct [%]",
                    shiny::div(
                      class = "box-header-settings-help",
                      bslib::tooltip(
                        shiny::div(
                          class = "tooltip-bttn",
                          shiny::actionButton(
                            ns("pstat_correct_help"),
                            NULL,
                            icon = shiny::icon("circle-question")
                          )
                        ),
                        "Help",
                        placement = "top"
                      )
                    )
                  ),
                  bslib::card_body(
                    class = "protocol-stat-body",
                    shinycssloaders::withSpinner(
                      shiny::uiOutput(ns("pstat_correct_stat")),
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
                    "Unmatched [%]",
                    shiny::div(
                      class = "box-header-settings-help",
                      bslib::tooltip(
                        shiny::div(
                          class = "tooltip-bttn",
                          shiny::actionButton(
                            ns("pstat_unmatched_help"),
                            NULL,
                            icon = shiny::icon("circle-question")
                          )
                        ),
                        "Help",
                        placement = "top"
                      )
                    )
                  ),
                  bslib::card_body(
                    class = "protocol-stat-body",
                    shinycssloaders::withSpinner(
                      shiny::uiOutput(ns("pstat_unmatched_stat")),
                      type = 1,
                      color = "#7777f9"
                    )
                  )
                )
              )
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Hit Rate Distribution",
                shiny::div(
                  class = "box-header-settings-help",
                  plot_dl_popover(ns, "stats_histogram"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("stats_histogram_help_bttn"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("stats_histogram"), height = "100%"),
                type = 1,
                color = "#7777f9"
              ))
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Hit Rate Summary Statistics",
                shiny::div(
                  class = "box-header-settings-help",
                  card_settings_popover(shiny::div(
                    shinyWidgets::materialSwitch(
                      ns("stats_boxplot_show_points"),
                      label = "Show Points",
                      value = TRUE,
                      right = TRUE
                    ),
                    shinyWidgets::materialSwitch(
                      ns("stats_boxplot_fixed_range"),
                      label = "Full Scale (0–100%)",
                      value = FALSE,
                      right = TRUE
                    ),
                    style = "margin-right:20px;"
                  )),
                  plot_dl_popover(ns, "stats_boxplot"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("stats_boxplot_help_bttn"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("stats_boxplot"), height = "100%"),
                type = 1,
                color = "#7777f9"
              ))
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Binding vs. Hit Rate",
                shiny::div(
                  class = "box-header-settings-help",
                  card_settings_popover(shiny::div(
                    bslib::tooltip(
                      shiny::selectInput(
                        ns("stats_scatter_color_scale"),
                        label = "Color Scale",
                        choices = NULL
                      ) |>
                        shiny::tagAppendAttributes(class = "palette-select"),
                      "Color palette",
                      placement = "top"
                    ),
                    shiny::selectInput(
                      ns("stats_scatter_groupby"),
                      label = "Color By",
                      choices = c("Protein", "Compound"),
                      selected = "Protein",
                      width = "140px"
                    ),
                    shinyWidgets::materialSwitch(
                      ns("stats_scatter_full_scale"),
                      label = "Full Scale (0–100%)",
                      value = FALSE,
                      right = TRUE
                    ),
                    style = "margin-right:20px;"
                  )),
                  plot_dl_popover(ns, "stats_scatter"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("stats_scatter_help_bttn"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("stats_scatter"), height = "100%"),
                type = 1,
                color = "#7777f9"
              ))
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                class = "bg-dark help-header d-flex justify-content-between",
                "Hit Rate Distribution by Group",
                shiny::div(
                  class = "box-header-settings-help",
                  card_settings_popover(shiny::div(
                    bslib::tooltip(
                      shiny::selectInput(
                        ns("stats_violin_color_scale"),
                        label = "Color Scale",
                        choices = NULL
                      ) |>
                        shiny::tagAppendAttributes(class = "palette-select"),
                      "Color palette",
                      placement = "top"
                    ),
                    shiny::selectInput(
                      ns("stats_violin_groupby"),
                      label = "Group By",
                      choices = c("Protein", "Compound"),
                      selected = "Protein",
                      width = "140px"
                    ),
                    shinyWidgets::materialSwitch(
                      ns("stats_violin_full_scale"),
                      label = "Full Scale (0–100%)",
                      value = FALSE,
                      right = TRUE
                    ),
                    shiny::div(
                      class = "conversion-tab-items-label",
                      shiny::HTML("Inner")
                    ),
                    shinyWidgets::radioGroupButtons(
                      ns("stats_violin_inner"),
                      label = NULL,
                      choices = c("Box", "Points"),
                      selected = "Box",
                      size = "sm"
                    ),
                    style = "margin-right:20px;"
                  )),
                  plot_dl_popover(ns, "stats_violin"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("stats_violin_help_bttn"),
                        NULL,
                        icon = shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              bslib::card_body(shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("stats_violin"), height = "100%"),
                type = 1,
                color = "#7777f9"
              ))
            )
          )
        )
      )
    ),
    if (batch_control) {
      bslib::nav_panel(
        title = "Batch Control",
        shiny::div(
          class = "conversion-result-wrapper",
          shiny::div(
            class = "batch-control-tab",
            shiny::uiOutput(ns("batch_heatmap_cards"))
          )
        )
      )
    },
    bslib::nav_item(
      id = ns("summary_tab_items"),
      class = "conversion-tab-item-wrapper",
      shiny::div(
        class = "conversion-tab-items",
        bslib::tooltip(
          shiny::div(
            class = "tooltip-bttn",
            shiny::actionButton(
              ns("summary_tooltip_bttn"),
              label = NULL,
              icon = shiny::icon("circle-question")
            )
          ),
          "Help",
          placement = "top"
        )
      )
    )
  )
}

# Binding results interface
#' @export
binding_results_ui <- function(ns, hits_summary) {
  bslib::navset_card_tab(
    id = ns("tabs"),
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
                    bslib::tooltip(
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::tags$button(
                          type = "button",
                          class = "btn btn-default",
                          onclick = sprintf(
                            "Shiny.setInputValue('%s', Math.random());",
                            ns("conversion_samples_protein_tooltip_bttn")
                          ),
                          shiny::icon("circle-question")
                        )
                      ),
                      "Help",
                      placement = "top"
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
                    "Tot. Binding [%]",
                    bslib::tooltip(
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::tags$button(
                          type = "button",
                          class = "btn btn-default",
                          onclick = sprintf(
                            "Shiny.setInputValue('%s', Math.random());",
                            ns("total_pct_bind_tooltip_bttn")
                          ),
                          shiny::icon("circle-question")
                        )
                      ),
                      "Help",
                      placement = "top"
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
                  card_settings_popover(
                    shiny::div(
                      shinyWidgets::materialSwitch(
                        ns(
                          "samples_table_view_binding_bar"
                        ),
                        label = "Binding [%] Bar",
                        value = TRUE,
                        right = TRUE
                      ),
                      shinyWidgets::materialSwitch(
                        ns(
                          "samples_table_view_tot_binding_bar"
                        ),
                        label = "Tot. Binding [%] Bar",
                        value = FALSE,
                        right = TRUE
                      ),
                      style = "margin-right: 20px;"
                    )
                  ),
                  table_dl_popover(ns, "samples_table_view"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::tags$button(
                        type = "button",
                        class = "btn btn-default",
                        onclick = sprintf(
                          "Shiny.setInputValue('%s', Math.random());",
                          ns("mass_spectra_tooltip_bttn")
                        ),
                        shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
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
                class = "bg-dark help-header d-flex justify-content-between",
                "Compound Distribution",
                shiny::div(
                  class = "box-header-settings-help",
                  plot_dl_popover(ns, "samples_cmp_dist"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::tags$button(
                        type = "button",
                        class = "btn btn-default",
                        onclick = sprintf(
                          "Shiny.setInputValue('%s', Math.random());",
                          ns("mass_spectra_tooltip_bttn")
                        ),
                        shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
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
                  card_settings_popover(
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
                    )
                  ),
                  plot_dl_popover(ns, "samples_spectrum"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::tags$button(
                        type = "button",
                        class = "btn btn-default",
                        onclick = sprintf(
                          "Shiny.setInputValue('%s', Math.random());",
                          ns("mass_spectra_tooltip_bttn")
                        ),
                        shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
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
                    bslib::tooltip(
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::tags$button(
                          type = "button",
                          class = "btn btn-default",
                          onclick = sprintf(
                            "Shiny.setInputValue('%s', Math.random());",
                            ns("conversion_samples_protein_tooltip_bttn")
                          ),
                          shiny::icon("circle-question")
                        )
                      ),
                      "Help",
                      placement = "top"
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
                    "Tot. Binding [%]",
                    bslib::tooltip(
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::tags$button(
                          type = "button",
                          class = "btn btn-default",
                          onclick = sprintf(
                            "Shiny.setInputValue('%s', Math.random());",
                            ns("total_pct_bind_tooltip_bttn")
                          ),
                          shiny::icon("circle-question")
                        )
                      ),
                      "Help",
                      placement = "top"
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
                  card_settings_popover(
                    shiny::div(
                      shinyWidgets::materialSwitch(
                        ns("compounds_table_view_binding_bar"),
                        label = "Binding [%] Bar",
                        value = TRUE,
                        right = TRUE
                      ),
                      shinyWidgets::materialSwitch(
                        ns("compounds_table_view_tot_binding_bar"),
                        label = "Tot. Binding [%] Bar",
                        value = FALSE,
                        right = TRUE
                      ),
                      style = "margin-right: 20px;"
                    )
                  ),
                  table_dl_popover(ns, "compounds_table_view"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::tags$button(
                        type = "button",
                        class = "btn btn-default",
                        onclick = sprintf(
                          "Shiny.setInputValue('%s', Math.random());",
                          ns("mass_spectra_tooltip_bttn")
                        ),
                        shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
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
                class = "bg-dark help-header d-flex justify-content-between",
                "Compound Distribution",
                shiny::div(
                  class = "box-header-settings-help",
                  card_settings_popover(
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
                    )
                  ),
                  plot_dl_popover(ns, "compounds_cmp_dist"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::tags$button(
                        type = "button",
                        class = "btn btn-default",
                        onclick = sprintf(
                          "Shiny.setInputValue('%s', Math.random());",
                          ns("mass_spectra_tooltip_bttn")
                        ),
                        shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              shiny::uiOutput(ns("compounds_compound_distribution_ui")),
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
                  card_settings_popover(
                    shiny::div(
                      shiny::div(
                        class = "spectrum-radio-button",
                        shinyWidgets::radioGroupButtons(
                          ns("compounds_spectrum_kind"),
                          choices = c("3D", "Planar")
                        )
                      ),
                      shinyWidgets::materialSwitch(
                        ns("compounds_spectrum_labels"),
                        label = "Show Labels",
                        value = local({
                          cmp <- unique(hits_summary$`Cmp Name`)[1]
                          tbl <- hits_summary[hits_summary$`Cmp Name` == cmp, ]
                          if (is.na(cmp) || nrow(tbl) < 2) {
                            return(TRUE)
                          }
                          ids <- tbl$`Sample ID`
                          ids <- ids[!is.na(ids)]
                          length(unique(ids)) <= 8 &
                            max(nchar(as.character(ids))) <= 20
                        }),
                        right = TRUE
                      ),
                      style = "margin-right: 20px;"
                    )
                  ),
                  plot_dl_popover(ns, "compounds_spectrum"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::tags$button(
                        type = "button",
                        class = "btn btn-default",
                        onclick = sprintf(
                          "Shiny.setInputValue('%s', Math.random());",
                          ns("mass_spectra_tooltip_bttn")
                        ),
                        shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              shiny::uiOutput(ns("cmp_annotated_spectrum_container")),
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
                    bslib::tooltip(
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::tags$button(
                          type = "button",
                          class = "btn btn-default",
                          onclick = sprintf(
                            "Shiny.setInputValue('%s', Math.random());",
                            ns("conversion_samples_protein_tooltip_bttn")
                          ),
                          shiny::icon("circle-question")
                        )
                      ),
                      "Help",
                      placement = "top"
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
                    "Tot. Binding [%]",
                    shiny::div(
                      class = "box-header-settings-help",
                      card_settings_popover(
                        shiny::div(
                          shiny::selectInput(
                            ns("total_pct_prot_binding_select"),
                            "Select Compound",
                            choices = unique(hits_summary$`Cmp Name`[
                              !is.na(hits_summary$`Cmp Name`)
                            ])
                          ),
                          style = "margin-right: 20px;"
                        )
                      ),
                      bslib::tooltip(
                        shiny::div(
                          class = "tooltip-bttn",
                          shiny::tags$button(
                            type = "button",
                            class = "btn btn-default",
                            onclick = sprintf(
                              "Shiny.setInputValue('%s', Math.random());",
                              ns("total_pct_bind_tooltip_bttn")
                            ),
                            shiny::icon("circle-question")
                          )
                        ),
                        "Help",
                        placement = "top"
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
                  card_settings_popover(
                    shiny::div(
                      shinyWidgets::materialSwitch(
                        ns("proteins_table_view_binding_bar"),
                        label = "Binding [%] Bar",
                        value = TRUE,
                        right = TRUE
                      ),
                      shinyWidgets::materialSwitch(
                        ns("proteins_table_view_tot_binding_bar"),
                        label = "Tot. Binding [%] Bar",
                        value = FALSE,
                        right = TRUE
                      ),
                      style = "margin-right: 20px;"
                    )
                  ),
                  table_dl_popover(ns, "proteins_table_view"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::tags$button(
                        type = "button",
                        class = "btn btn-default",
                        onclick = sprintf(
                          "Shiny.setInputValue('%s', Math.random());",
                          ns("mass_spectra_tooltip_bttn")
                        ),
                        shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
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
                  card_settings_popover(
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
                    )
                  ),
                  plot_dl_popover(ns, "proteins_cmp_dist"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::tags$button(
                        type = "button",
                        class = "btn btn-default",
                        onclick = sprintf(
                          "Shiny.setInputValue('%s', Math.random());",
                          ns("mass_spectra_tooltip_bttn")
                        ),
                        shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
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
                  card_settings_popover(
                    shiny::div(
                      shiny::div(
                        class = "spectrum-radio-button",
                        shinyWidgets::radioGroupButtons(
                          ns("proteins_spectrum_kind"),
                          choices = c("3D", "Planar")
                        )
                      ),
                      shinyWidgets::materialSwitch(
                        ns("proteins_spectrum_labels"),
                        label = "Show Labels",
                        value = local({
                          prot <- unique(hits_summary$`Protein`)[1]
                          tbl <- hits_summary[hits_summary$`Protein` == prot, ]
                          if (is.na(prot) || nrow(tbl) < 2) {
                            return(TRUE)
                          }
                          ids <- tbl$`Sample ID`
                          ids <- ids[!is.na(ids)]
                          length(unique(ids)) <= 8 &
                            max(nchar(as.character(ids))) <= 20
                        }),
                        right = TRUE
                      ),
                      style = "margin-right: 20px;"
                    )
                  ),
                  plot_dl_popover(ns, "proteins_spectrum"),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::tags$button(
                        type = "button",
                        class = "btn btn-default",
                        onclick = sprintf(
                          "Shiny.setInputValue('%s', Math.random());",
                          ns("mass_spectra_tooltip_bttn")
                        ),
                        shiny::icon("circle-question")
                      )
                    ),
                    "Help",
                    placement = "top"
                  )
                )
              ),
              shiny::uiOutput(ns("annotated_spectrum_container")),
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
        bslib::tooltip(
          shiny::selectInput(
            ns("color_scale"),
            label = NULL,
            choices = NULL,
            width = "120px"
          ) |>
            shiny::tagAppendAttributes(class = "palette-select"),
          "Color palette",
          placement = "top"
        ),
        bslib::tooltip(
          shiny::div(
            class = "tooltip-bttn",
            shiny::actionButton(
              ns("conversion_tooltip_bttn"),
              label = NULL,
              icon = shiny::icon("circle-question")
            )
          ),
          "Help",
          placement = "top"
        )
      )
    )
  )
}

# Unified Hits interface (single card, no tabs)
#' @export
hits_results_ui <- function(ns, hits_summary, units) {
  bslib::card(
    class = "hits-unified-card",
    bslib::card_body(
      class = "conversion-result-wrapper hits-tab",
      shiny::div(
        class = "hits-controls input-panel",
        shiny::radioButtons(
          ns("hits_per_adduct"),
          label = "Display",
          choices = c("Hit View", "Adduct View"),
          selected = "Hit View"
        ),
        shinyWidgets::pickerInput(
          ns("hits_color_variable"),
          label = "Color Variable",
          choices = if ("Concentration" %in% names(units)) {
            c("Concentration", "Compounds", "Samples", "None")
          } else {
            c("Compounds", "Samples", "None")
          },
          selected = if ("Concentration" %in% names(units)) {
            "Concentration"
          } else {
            "Compounds"
          }
        ),
        shiny::selectInput(
          ns("hits_color_scale"),
          label = "Color Scale",
          choices = NULL
        ) |>
          shiny::tagAppendAttributes(class = "palette-select"),
        shinyWidgets::pickerInput(
          ns("hits_tab_sample_select"),
          label = "Select Samples",
          choices = unique(hits_summary$`Sample ID`),
          selected = unique(hits_summary$`Sample ID`),
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        shinyWidgets::pickerInput(
          ns("hits_tab_compound_select"),
          label = "Select Compounds",
          choices = unique(hits_summary$`Cmp Name`)[
            !is.na(unique(hits_summary$`Cmp Name`))
          ],
          selected = unique(hits_summary$`Cmp Name`)[
            !is.na(unique(hits_summary$`Cmp Name`))
          ],
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        shinyWidgets::pickerInput(
          ns("hits_tab_col_select"),
          label = "Select Columns",
          choices = names(hits_summary)[
            !names(hits_summary) %in%
              c("Sample ID", "Protein", "Cmp Name", "truncSample_ID", "Tot. Binding [%]")
          ],
          selected = names(hits_summary)[
            !names(hits_summary) %in%
              c(
                "Sample ID",
                "Protein",
                "Cmp Name",
                "truncSample_ID",
                "Tot. Binding [%]",
                "Well",
                "Replicate",
                "Unmatched [%]",
                "Preferred",
                "Meas. Prot. [Da]",
                "Δ Prot. [Da]",
                "Int. Prot. [%]",
                "Int. Cmp [%]",
                "Δ Cmp [Da]"
              )
          ],
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        shinyWidgets::pickerInput(
          ns("hits_binding_chart"),
          label = "Show Binding Bars",
          choices = c("Binding [%]", "Tot. Binding [%]"),
          selected = "Tot. Binding [%]",
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        shiny::div(
          class = "hits-table-export",
          shiny::tags$label(class = "control-label", "Export Table"),
          table_dl_buttons(ns, "hits_unified_tab")
        )
      ),
      shiny::div(
        class = "hits-table-wrapper",
        shinycssloaders::withSpinner(
          DT::DTOutput(ns("hits_unified_tab")),
          type = 1,
          color = "#7777f9"
        )
      )
    ),
    shiny::tags$script(popover_autoclose)
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

  if (samples_status == "confirmed") {
    samples_control_buttons <- shiny::div(
      class = "table-control-buttons sampletable-control-buttons",
      bslib::tooltip(
        shiny::div(
          style = "width: 100%;",
          shinyjs::disabled(shiny::actionButton(
            ns("confirm_samples"),
            label = NULL,
            icon = shiny::icon("check"),
            width = "100%"
          ))
        ),
        "Confirm Sample Table",
        placement = "top"
      ),
      bslib::tooltip(
        shiny::div(
          style = "width: 100%;",
          shinyjs::disabled(shiny::actionButton(
            ns("use_config"),
            label = NULL,
            icon = shiny::icon("wand-magic-sparkles"),
            width = "100%"
          ))
        ),
        "Use Experiment Config",
        placement = "top"
      ),
      bslib::tooltip(
        shiny::div(
          style = "width: 100%;",
          shiny::actionButton(
            ns("edit_samples"),
            label = NULL,
            icon = shiny::icon("pen-to-square"),
            width = "100%"
          )
        ),
        "Edit Sample Table",
        placement = "top"
      ),
      bslib::tooltip(
        shiny::div(
          style = "width: 100%;",
          shinyjs::disabled(shiny::actionButton(
            ns("clear_samples"),
            label = NULL,
            icon = shiny::icon("eraser"),
            width = "100%"
          ))
        ),
        "Clear Sample Table",
        placement = "top"
      )
    )
  } else {
    samples_control_buttons <- shiny::div(
      class = "table-control-buttons sampletable-control-buttons",
      bslib::tooltip(
        shiny::div(
          style = "width: 100%;",
          shinyjs::disabled(shiny::actionButton(
            ns("confirm_samples"),
            label = NULL,
            icon = shiny::icon("bookmark"),
            width = "100%"
          ))
        ),
        "Confirm Sample Table",
        placement = "top"
      ),
      bslib::tooltip(
        shiny::div(
          style = "width: 100%;",
          shinyjs::disabled(shiny::actionButton(
            ns("use_config"),
            label = NULL,
            icon = shiny::icon("wand-magic-sparkles"),
            width = "100%"
          ))
        ),
        "Use Experiment Config",
        placement = "top"
      ),
      bslib::tooltip(
        shiny::div(
          style = "width: 100%;",
          shinyjs::disabled(shiny::actionButton(
            ns("edit_samples"),
            label = NULL,
            icon = shiny::icon("pen-to-square"),
            width = "100%"
          ))
        ),
        "Edit Sample Table",
        placement = "top"
      ),
      bslib::tooltip(
        shiny::div(
          style = "width: 100%;",
          shinyjs::disabled(shiny::actionButton(
            ns("clear_samples"),
            label = NULL,
            icon = shiny::icon("eraser"),
            width = "100%"
          ))
        ),
        "Clear Sample Table",
        placement = "top"
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
            bslib::tooltip(
              shiny::div(
                class = "tooltip-bttn",
                shiny::tags$button(
                  type = "button",
                  class = "btn btn-default fileinput-tooltip-btn",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', Math.random());",
                    ns("fileinput_tooltip_bttn")
                  ),
                  shiny::icon("circle-question")
                )
              ),
              "Help",
              placement = "top"
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
          shiny::div(
            class = "table-hint-anchor",
            shiny::uiOutput(ns("proteins_table_hint"))
          ),
          rhandsontable::rHandsontableOutput(
            ns("proteins_table"),
            width = "99%"
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
            bslib::tooltip(
              shiny::div(
                class = "tooltip-bttn",
                shiny::tags$button(
                  type = "button",
                  class = "btn btn-default fileinput-tooltip-btn",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', Math.random());",
                    ns("fileinput_tooltip_bttn")
                  ),
                  shiny::icon("circle-question")
                )
              ),
              "Help",
              placement = "top"
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
          shiny::div(
            class = "table-hint-anchor",
            shiny::uiOutput(ns("compounds_table_hint"))
          ),
          rhandsontable::rHandsontableOutput(
            ns("compounds_table"),
            width = "99%"
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
      "Samples",
      shiny::div(
        class = "samples-controls",
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::div(
              class = "table-input",
              shinyjs::disabled(
                shiny::fileInput(
                  ns("samples_fileinput"),
                  "Select File",
                  multiple = FALSE,
                  accept = c(".db")
                )
              )
            )
          ),
          shiny::column(
            width = 1,
            align = "left",
            shiny::div(
              class = "sample-declaration-info-ui",
              bslib::tooltip(
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("resultinput_tooltip_bttn"),
                    label = NULL,
                    icon = shiny::icon("circle-question")
                  )
                ),
                "Help",
                placement = "top"
              ),
            )
          ),
          shiny::column(
            width = 2,
            shiny::textOutput(ns("samples_table_info"))
          ),
          shiny::column(
            width = 4,
            samples_control_buttons
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
          shiny::div(
            class = "table-hint-anchor",
            shiny::uiOutput(ns("samples_table_hint"))
          ),
          rhandsontable::rHandsontableOutput(ns("samples_table"), width = "99%")
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
