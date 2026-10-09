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
      unit_symbol,
      stats_histogram,
      stats_boxplot,
      stats_scatter,
      stats_violin,
      show_preferred_column,
      is_complex_row,
      overview_choices,
      overview_status_note,
      overview_selection,
      overview_subset,
    ],
  app / logic / helper_functions[config_icon],
  app /
    logic /
    plot_download[
      card_settings_popover,
      plot_dl_popover,
      table_dl_buttons,
      table_dl_popover
    ],
)

# Concentrations tab of the kinact/Ki results interface: one concentration at a
# time, picked like a sample in the Samples View of the Relative Binding
# interface. `concentrations` are the fitted concentration keys in the declared
# unit; the picker labels follow the unit view (updated by the server).
# `selected` is the concentration picked first (NULL: the first one).
#' @export
kinact_ki_concentrations_panel <- function(
  ns,
  concentrations,
  conc_unit = NULL,
  selected = NULL
) {
  stat_card <- function(title, help_id, output_id) {
    shiny::div(
      class = "card-custom",
      bslib::card(
        bslib::card_header(
          class = "bg-dark help-header",
          title,
          bslib::tooltip(
            shiny::div(
              class = "tooltip-bttn",
              shiny::actionButton(
                ns(help_id),
                label = NULL,
                icon = shiny::icon("circle-question")
              )
            ),
            "Help",
            placement = "top"
          )
        ),
        shiny::uiOutput(ns(output_id))
      )
    )
  }

  bslib::nav_panel(
    title = "Concentrations",
    shiny::div(
      class = "conversion-result-wrapper",
      shiny::div(
        class = "conversion-samples-wrapper conc-tab-wrapper",
        # Top left: concentration picker and the per-concentration fit values
        shiny::div(
          class = "conversion-samples-control",
          shiny::div(
            class = "sample-cmp-prot-picker",
            shinyWidgets::pickerInput(
              ns("conc_tab_select"),
              "Select Concentration",
              choices = stats::setNames(
                concentrations,
                paste(concentrations, conc_unit)
              ),
              selected = selected,
              options = shinyWidgets::pickerOptions(
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search concentrations ..."
              )
            )
          ),
          shiny::div(
            class = "conversion-samples-stats",
            stat_card(
              htmltools::tagList(
                shiny::div("k", htmltools::tags$sub("obs"))
              ),
              "kobs_value_tooltip_bttn",
              "conc_tab_kobs_value"
            ),
            stat_card(
              "Binding Plateau",
              "binding_plateau_tooltip_bttn",
              "conc_tab_plateau_value"
            ),
            stat_card("Velocity v", "v_value_tooltip_bttn", "conc_tab_v_value")
          )
        ),
        # Top right: hits of the concentration
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
                      ns("conc_tab_table_view_binding_bar"),
                      label = "Binding [%] Bar",
                      value = TRUE,
                      right = TRUE
                    ),
                    shinyWidgets::materialSwitch(
                      ns("conc_tab_table_view_tot_binding_bar"),
                      label = "Tot. Binding [%] Bar",
                      value = TRUE,
                      right = TRUE
                    ),
                    style = "margin-right: 20px;"
                  )
                ),
                table_dl_popover(ns, "conc_tab_hits"),
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
                DT::DTOutput(ns("conc_tab_hits")),
                type = 1,
                color = "#7777f9"
              )
            )
          )
        ),
        # Bottom left: spectra of the concentration's samples
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
                        ns("conc_tab_kind"),
                        choices = c("Cubic", "Planar")
                      )
                    ),
                    style = "margin-right: 20px;"
                  )
                ),
                plot_dl_popover(ns, "conc_tab_spectra"),
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
              plotly::plotlyOutput(ns("conc_tab_spectra"), height = "100%"),
              type = 1,
              color = "#7777f9"
            )
          )
        ),
        # Bottom right: binding curve of the concentration
        shiny::div(
          class = "card-custom binding",
          bslib::card(
            bslib::card_header(
              class = "bg-dark help-header d-flex justify-content-between",
              "Binding Curve",
              shiny::div(
                class = "box-header-settings-help",
                card_settings_popover(shiny::div(
                  shiny::div(
                    class = "conversion-tab-items-label",
                    shiny::HTML("Data Points")
                  ),
                  shinyWidgets::radioGroupButtons(
                    ns("conc_tab_binding_points"),
                    label = NULL,
                    choices = c(
                      "Mean ± SD" = "mean",
                      "Samples" = "samples"
                    ),
                    selected = "mean",
                    size = "sm"
                  ),
                  style = "margin-right:20px;"
                )),
                plot_dl_popover(ns, "conc_tab_binding"),
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
                ns("conc_tab_binding_plot"),
                height = "100%"
              ),
              type = 1,
              color = "#7777f9"
            )
          )
        )
      )
    ),
    shiny::tags$script(popover_autoclose)
  )
}

# kinact/Ki results interface. With `selected_conc`, one of the fitted
# `concentrations`, it opens on that concentration in the Concentrations tab.
#' @export
kinact_ki_results_ui <- function(
  ns,
  hits_summary,
  concentrations,
  units = NULL,
  proteoforms = FALSE,
  paired_limits = FALSE,
  selected_conc = NULL
) {
  # Declared concentration unit, e.g. "µM" from "Concentration [µM]"
  conc_unit <- if (!is.null(units[["Concentration"]])) {
    gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]])
  }

  if (!length(selected_conc) || !selected_conc[1] %in% concentrations) {
    selected_conc <- NULL
  }

  # One tab for all fitted concentrations, which are picked inside it
  concentration_panels <- if (length(concentrations)) {
    list(kinact_ki_concentrations_panel(
      ns,
      concentrations,
      conc_unit,
      selected = selected_conc
    ))
  }

  static_panels <- list(
    bslib::nav_panel(
      title = "Kinetics",
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
                  card_settings_popover(shiny::div(
                    shiny::div(
                      class = "conversion-tab-items-label",
                      shiny::HTML("Data Points")
                    ),
                    shinyWidgets::radioGroupButtons(
                      ns("binding_points"),
                      label = NULL,
                      choices = c(
                        "Mean ± SD" = "mean",
                        "Samples" = "samples"
                      ),
                      selected = "mean",
                      size = "sm"
                    ),
                    style = "margin-right:20px;"
                  )),
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
                  card_settings_popover(htmltools::tagList(
                    shiny::div(
                      shinyWidgets::materialSwitch(
                        ns("kobs_show_extrapolation"),
                        label = "Show Extrapolation",
                        value = FALSE,
                        right = TRUE
                      ),
                      style = "margin-right:20px;"
                    ),
                    # Offered only when a protein carries several masses
                    if (isTRUE(proteoforms)) {
                      shiny::div(
                        shinyWidgets::materialSwitch(
                          ns("kobs_show_proteoforms"),
                          label = "Show Proteoforms",
                          value = TRUE,
                          right = TRUE
                        ),
                        style = "margin-right:20px;"
                      )
                    }
                  )),
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
                      "k",
                      htmltools::tags$sub("inact"),
                      "/ K",
                      htmltools::tags$sub("i"),
                    )
                  ),
                  bslib::tooltip(
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("Kinact_Ki_tooltip_bttn"),
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
                    shiny::uiOutput(ns("Kinact_Ki")),
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

  # Per-proteoform kinetics next to the pooled fit, only offered when a protein
  # was declared with several masses
  proteoform_panels <- if (isTRUE(proteoforms)) {
    list(proteoform_results_panel(ns, paired_limits))
  }

  # Fit diagnostics: how well the data support the global kinact/KI fit
  diagnostics_card <- function(title, id, help_id) {
    shiny::div(
      class = "card-custom",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark help-header d-flex justify-content-between",
          title,
          shiny::div(
            class = "box-header-settings-help",
            plot_dl_popover(ns, id),
            bslib::tooltip(
              shiny::div(
                class = "tooltip-bttn",
                shiny::actionButton(
                  ns(help_id),
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
            plotly::plotlyOutput(ns(paste0(id, "_plot")), height = "100%"),
            type = 1,
            color = "#7777f9"
          )
        )
      )
    )
  }

  diagnostics_panel <- bslib::nav_panel(
    title = "Fit",
    shiny::div(
      class = "conversion-result-wrapper",
      shiny::div(
        class = "kinetics-diagnostics-tab",
        diagnostics_card(
          "Global Fit Residuals",
          "diag_residuals",
          "diag_residuals_tooltip_bttn"
        ),
        diagnostics_card(
          "Plateau per Concentration",
          "diag_plateaus",
          "diag_plateaus_tooltip_bttn"
        ),
        diagnostics_card(
          htmltools::tagList(shiny::div(
            "k",
            htmltools::tags$sub("inact"),
            "/K",
            htmltools::tags$sub("i"),
            " by Replicate Series"
          )),
          "diag_series",
          "diag_series_tooltip_bttn"
        ),
        diagnostics_card(
          "Saturation Coverage",
          "diag_saturation",
          "diag_saturation_tooltip_bttn"
        )
      )
    ),
    shiny::tags$script(popover_autoclose)
  )

  all_tabs <- c(
    static_panels,
    list(diagnostics_panel),
    proteoform_panels,
    concentration_panels
  )

  do.call(
    bslib::navset_card_tab,
    c(
      # Kept distinct from the declaration/binding navsets: the result
      # interfaces now coexist in the DOM, so their tab ids have to be unique.
      list(
        id = ns("kinetics_tabs"),
        selected = if (!is.null(selected_conc)) "Concentrations"
      ),
      all_tabs,
      list(
        bslib::nav_item(
          class = "conversion-tab-item-wrapper",
          shiny::div(
            class = "unit-inputs",
            shiny::div("Unit View"),
            conc_unit_input_ui(
              ns,
              id = "conc_unit_results",
              label = FALSE,
              selected = if (!is.null(units)) {
                unit_symbol(units[["Concentration"]])
              }
            ),
            time_unit_input_ui(
              ns,
              id = "time_unit_results",
              label = FALSE,
              selected = if (!is.null(units)) unit_symbol(units[["Time"]])
            )
          )
        )
      )
    )
  )
}

# Proteoforms tab of the kinetics interface
# `paired_limits`: whether the Paired Binding plot has values pinned at a
# detection limit. They are shown by default; without any the switch has
# nothing to show and is disabled.
proteoform_results_panel <- function(ns, paired_limits = FALSE) {
  limits_switch <- shinyWidgets::materialSwitch(
    ns("paired_show_limits"),
    label = "Show Limit Values",
    value = paired_limits,
    right = TRUE
  )
  if (!paired_limits) {
    limits_switch <- shinyjs::disabled(limits_switch)
  }

  # Every card of the tab opens the same help, so the button sets the input
  # rather than being one: an actionButton would repeat its id per card
  help_button <- function(id) {
    bslib::tooltip(
      shiny::div(
        class = "tooltip-bttn",
        shiny::tags$button(
          type = "button",
          class = "btn btn-default",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Math.random());",
            ns(id)
          ),
          shiny::icon("circle-question")
        )
      ),
      "Help",
      placement = "top"
    )
  }

  result_card <- function(title, download, output, style = NULL) {
    shiny::div(
      class = "card-custom",
      style = style,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark help-header d-flex justify-content-between",
          title,
          shiny::div(
            class = "box-header-settings-help",
            download,
            help_button("proteoform_tooltip_bttn")
          )
        ),
        bslib::card_body(
          shinycssloaders::withSpinner(
            output,
            type = 1,
            color = "#7777f9"
          )
        )
      )
    )
  }

  bslib::nav_panel(
    title = "Proteoforms",
    shiny::div(
      class = "conversion-result-wrapper",
      shiny::div(
        class = "binding-analysis-tab",
        result_card(
          htmltools::tagList(shiny::div(
            "k",
            htmltools::tags$sub("obs"),
            " per Proteoform"
          )),
          plot_dl_popover(ns, "proteoform_kobs"),
          plotly::plotlyOutput(ns("proteoform_kobs_plot"), height = "100%")
        ),
        result_card(
          "Paired Binding",
          htmltools::tagList(
            card_settings_popover(
              shiny::div(
                limits_switch,
                style = "margin-right: 20px;"
              )
            ),
            plot_dl_popover(ns, "proteoform_paired")
          ),
          plotly::plotlyOutput(ns("proteoform_paired_plot"), height = "100%")
        ),
        result_card(
          "Proteoform Kinetics",
          table_dl_popover(ns, "proteoform_table"),
          DT::DTOutput(ns("proteoform_table")),
          style = "grid-column: span 2; min-height: 0;"
        )
      )
    ),
    shiny::tags$script(popover_autoclose)
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
                  bslib::tooltip(
                    shiny::actionButton(
                      ns("protocol_scroll_top"),
                      NULL,
                      icon = shiny::icon("arrow-up")
                    ),
                    "Jump to top",
                    placement = "left"
                  ),
                  bslib::tooltip(
                    shiny::actionButton(
                      ns("protocol_scroll_bot"),
                      NULL,
                      icon = shiny::icon("arrow-down")
                    ),
                    "Jump to bottom",
                    placement = "left"
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
                            ns("pstat_correct_stat_help"),
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
                            ns("pstat_unmatched_stat_help"),
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

# Overview tab of the binding results interface: one protein with any of the
# compounds declared with it, picked above the two Mass Shifts cards.
# `selected` is an overview_selection() of `choices`.
overview_results_panel <- function(
  ns,
  hits_summary,
  choices,
  selected,
  sort_binding_switch
) {
  proteins <- choices$proteins
  compounds <- if (is.na(selected$protein)) {
    data.frame(compound = character(0), hit = logical(0))
  } else {
    choices$compounds[[selected$protein]]
  }

  # Spectrum labels and peak symbols get unreadable once many spectra are
  # stacked, so they start off for large selections. The labels are the short
  # sample IDs the interface starts with.
  subset <- overview_subset(hits_summary, selected$protein, selected$compounds)
  sample_ids <- unique(as.character(
    if ("truncSample_ID" %in% names(subset)) {
      subset$truncSample_ID
    } else {
      subset$`Sample ID`
    }
  ))
  sample_ids <- sample_ids[!is.na(sample_ids)]
  labels_show <- length(sample_ids) < 2 ||
    (length(sample_ids) <= 8 && max(nchar(sample_ids)) <= 20)
  symbols_show <- length(sample_ids) <= 20

  help_button <- function(id) {
    bslib::tooltip(
      shiny::div(
        class = "tooltip-bttn",
        shiny::tags$button(
          type = "button",
          class = "btn btn-default",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Math.random());",
            ns(id)
          ),
          shiny::icon("circle-question")
        )
      ),
      "Help",
      placement = "top"
    )
  }

  shift_card <- function(title, output_id) {
    shiny::div(
      class = "card-custom",
      bslib::card(
        bslib::card_header(
          class = "bg-dark help-header",
          title,
          help_button("overview_mass_shifts_tooltip_bttn")
        ),
        shiny::div(
          class = "kobs-val",
          shinycssloaders::withSpinner(
            shiny::uiOutput(ns(output_id)),
            type = 1,
            color = "#7777f9"
          )
        )
      )
    )
  }

  bslib::nav_panel(
    title = "Overview",
    shiny::div(
      class = "conversion-result-wrapper",
      shiny::div(
        class = "conversion-samples-wrapper",
        shiny::div(
          class = "conversion-samples-control",
          # Each picker sits above the card it fills
          shiny::div(
            class = "overview-pickers",
            shiny::div(
              class = "sample-cmp-prot-picker",
              shinyWidgets::pickerInput(
                ns("overview_protein_picker"),
                "Select Protein",
                choices = proteins$protein,
                selected = selected$protein,
                choicesOpt = list(
                  subtext = overview_status_note(proteins$status)
                ),
                options = shinyWidgets::pickerOptions(
                  liveSearch = TRUE,
                  liveSearchPlaceholder = "Search proteins ..."
                ),
                width = "100%"
              )
            ),
            shiny::div(
              class = "sample-cmp-prot-picker",
              shinyWidgets::pickerInput(
                ns("overview_compound_picker"),
                "Select Compounds",
                choices = compounds$compound,
                selected = selected$compounds,
                multiple = TRUE,
                choicesOpt = list(
                  subtext = ifelse(compounds$hit, "", "No hits")
                ),
                options = overview_compound_picker_options(
                  nrow(compounds) > 0
                ),
                width = "100%"
              )
            )
          ),
          shiny::div(
            class = "conversion-samples-stats",
            shift_card(
              "Protein Mass Shifts",
              "overview_protein_mass_shifts"
            ),
            shift_card(
              "Compound Mass Shifts",
              "overview_compound_mass_shifts"
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
                      ns("overview_table_view_binding_bar"),
                      label = "Binding [%] Bar",
                      value = TRUE,
                      right = TRUE
                    ),
                    shinyWidgets::materialSwitch(
                      ns("overview_table_view_tot_binding_bar"),
                      label = "Tot. Binding [%] Bar",
                      value = TRUE,
                      right = TRUE
                    ),
                    style = "margin-right: 20px;"
                  )
                ),
                table_dl_popover(ns, "overview_table_view"),
                help_button("table_view_tooltip_bttn")
              )
            ),
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("overview_table_view")),
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
                      inputId = ns("overview_distribution_scale"),
                      label = "Range",
                      choices = c("Maximum", "100")
                    ),
                    shinyWidgets::materialSwitch(
                      ns("overview_distribution_labels"),
                      label = "Show Labels",
                      value = TRUE,
                      right = TRUE
                    ),
                    style = "margin-right: 20px;"
                  )
                ),
                plot_dl_popover(ns, "overview_cmp_dist"),
                help_button("cmp_distribution_tooltip_bttn")
              )
            ),
            shinycssloaders::withSpinner(
              shiny::uiOutput(ns("overview_distribution_ui")),
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
                        ns("overview_spectrum_kind"),
                        choices = c("Cubic", "Planar")
                      )
                    ),
                    sort_binding_switch("overview_spectrum_sort_binding"),
                    shinyWidgets::materialSwitch(
                      ns("overview_spectrum_labels"),
                      label = "Show Labels",
                      value = labels_show,
                      right = TRUE
                    ),
                    shinyWidgets::materialSwitch(
                      ns("overview_spectrum_symbols"),
                      label = "Show Symbols",
                      value = symbols_show,
                      right = TRUE
                    ),
                    shinyWidgets::materialSwitch(
                      ns("overview_spectrum_unmatched"),
                      label = "Show Unmatched",
                      value = FALSE,
                      right = TRUE
                    ),
                    shinyWidgets::materialSwitch(
                      ns("overview_spectrum_legend"),
                      label = "Show Legend",
                      value = TRUE,
                      right = TRUE
                    ),
                    style = "margin-right: 20px;"
                  )
                ),
                plot_dl_popover(ns, "overview_spectrum"),
                help_button("annotated_spectrum_tooltip_bttn")
              )
            ),
            shiny::uiOutput(ns("overview_spectrum_container")),
            full_screen = TRUE
          )
        )
      )
    ),
    shiny::tags$script(popover_autoclose)
  )
}

# Options of the Overview compound picker; without any compound declared for
# the protein there is nothing to pick
#' @export
overview_compound_picker_options <- function(has_compounds = TRUE) {
  shinyWidgets::pickerOptions(
    actionsBox = TRUE,
    liveSearch = TRUE,
    liveSearchPlaceholder = "Search compounds ...",
    selectedTextFormat = "count > 2",
    countSelectedText = "{0} of {1} compounds",
    noneSelectedText = if (has_compounds) {
      "No compound selected"
    } else {
      "No compounds declared"
    }
  )
}

# Binding results interface. `overview` is the overview_selection() the
# Overview tab opens with.
#' @export
binding_results_ui <- function(
  ns,
  hits_summary,
  show_sort_binding = TRUE,
  overview = NULL
) {
  # The switch only has something to undo while the spectra are grouped by
  # concentration, so it is left out entirely when there is no concentration
  # to group by.
  sort_binding_switch <- function(id) {
    if (!isTRUE(show_sort_binding)) {
      return(NULL)
    }
    shinyWidgets::materialSwitch(
      ns(id),
      label = "Sort by Binding",
      value = TRUE,
      right = TRUE
    )
  }

  choices <- overview_choices(hits_summary)
  if (is.null(overview)) {
    overview <- overview_selection(choices)
  }

  bslib::navset_card_tab(
    id = ns("tabs"),
    overview_results_panel(
      ns,
      hits_summary,
      choices,
      overview,
      sort_binding_switch
    ),
    bslib::nav_panel(
      title = "Sample View",
      shiny::div(
        class = "conversion-result-wrapper",
        shiny::div(
          class = "conversion-samples-wrapper",
          shiny::div(
            class = "conversion-samples-control",
            shiny::div(
              class = "sample-cmp-prot-picker",
              local({
                # One ungrouped list, a sample without any binding event
                # marked next to its name: "Not measured" when no peak of the
                # protein was found at all, neither unbound nor complex (its
                # binding is NA), "No hits" otherwise.
                # A sample keeps rows without an adduct beside its hits
                # (other proteoforms or compounds), so a complex on any of
                # its rows counts.
                sample_col <- as.character(hits_summary$`Sample ID`)
                named <- !is.na(sample_col) & nzchar(trimws(sample_col))
                samples <- unique(sample_col[named])
                hit <- samples %in%
                  sample_col[named & is_complex_row(hits_summary)]
                measured <- samples %in%
                  sample_col[named & !is.na(hits_summary$`Tot. Binding [%]`)]

                shinyWidgets::pickerInput(
                  ns("conversion_sample_picker"),
                  "Select Sample",
                  choices = samples,
                  choicesOpt = list(
                    subtext = ifelse(
                      hit,
                      "",
                      ifelse(measured, "No hits", "Not measured")
                    )
                  ),
                  options = shinyWidgets::pickerOptions(
                    liveSearch = TRUE,
                    liveSearchPlaceholder = "Search samples ..."
                  )
                )
              })
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
                    "Quality",
                    bslib::tooltip(
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::tags$button(
                          type = "button",
                          class = "btn btn-default",
                          onclick = sprintf(
                            "Shiny.setInputValue('%s', Math.random());",
                            ns("samples_quality_tooltip_bttn")
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
                        "samples_quality"
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
                        value = TRUE,
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
                          ns("table_view_tooltip_bttn")
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
                          ns("cmp_distribution_tooltip_bttn")
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
                        label = "Annotate Mass",
                        value = FALSE,
                        right = TRUE
                      ),
                      shinyWidgets::materialSwitch(
                        ns("sample_view_spectrum_unmatched"),
                        label = "Show Unmatched",
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
                          ns("annotated_spectrum_tooltip_bttn")
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
  # Percentage columns offered as bars; Prot. Binding [%] exists only when a
  # protein was declared with several masses
  bar_cols <- c(
    "Binding [%]",
    "Tot. Binding [%]",
    if ("Prot. Binding [%]" %in% names(hits_summary)) "Prot. Binding [%]",
    "Int. Prot. [%]",
    "Int. Cmp [%]",
    "Unmatched [%]",
    "Correct [%]"
  )

  bslib::card(
    class = "hits-unified-card",
    bslib::card_body(
      class = "conversion-result-wrapper hits-tab",
      shiny::div(
        class = "hits-controls input-panel",
        shiny::radioButtons(
          ns("hits_per_adduct"),
          label = "Display",
          choices = c("Adduct View", "Sample View"),
          selected = "Adduct View"
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
              c(
                "Sample ID",
                "Protein",
                "Cmp Name",
                "truncSample_ID",
                "Tot. Binding [%]",
                if ("Concentration" %in% names(units)) {
                  units[["Concentration"]]
                } else {
                  NULL
                },
                if ("Time" %in% names(units)) units[["Time"]] else NULL
              )
          ],
          selected = names(hits_summary)[
            !names(hits_summary) %in%
              c(
                "Sample ID",
                "Protein",
                "Cmp Name",
                "truncSample_ID",
                "Tot. Binding [%]",
                if ("Concentration" %in% names(units)) {
                  units[["Concentration"]]
                } else {
                  NULL
                },
                if ("Time" %in% names(units)) units[["Time"]] else NULL,
                "Well",
                "Replicate",
                "Unmatched [%]",
                # Shown when a peak had a reading that was not preferred
                if (!show_preferred_column(hits_summary)) "Preferred",
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
          label = "Show % Bar",
          choices = bar_cols,
          selected = bar_cols,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        shiny::div(
          class = "hits-table-export",
          shiny::div(
            class = "label-tooltip",
            shiny::tags$label(class = "control-label", "Export Table"),
            bslib::tooltip(
              shiny::div(
                class = "tooltip-bttn",
                shiny::actionButton(
                  ns("hits_tooltip_bttn"),
                  label = NULL,
                  icon = shiny::icon("circle-question")
                )
              ),
              "Help",
              placement = "top"
            )
          ),
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
  samples_status = "",
  conc_unit = NULL,
  time_unit = NULL
) {
  # Protein/compound file input; a saved table renders it locked, matching
  # what confirm_ui_changes() applies to an already rendered input
  declaration_fileinput <- function(id, status) {
    file_input <- shiny::fileInput(
      ns(id),
      "",
      multiple = FALSE,
      accept = c(".csv", ".tsv", ".xlsx", ".xls", ".txt")
    )
    if (status != "confirmed") {
      return(file_input)
    }
    shinyjs::disabled(
      htmltools::tagQuery(file_input)$find(".btn-file")$addClass(
        "custom-disable"
      )$resetSelected()$find(".input-group > .form-control")$addClass(
        "custom-disable"
      )$allTags()
    )
  }

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
            icon = config_icon(apply = TRUE),
            width = "100%"
          ))
        ),
        "Apply Experiment Config to Samples",
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
            icon = config_icon(apply = TRUE),
            width = "100%"
          ))
        ),
        "Apply Experiment Config to Samples",
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
              declaration_fileinput("proteins_fileinput", proteins_status)
            )
          ),
          shiny::column(
            width = 3,
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
              declaration_fileinput("compounds_fileinput", compounds_status)
            )
          ),
          shiny::column(
            width = 3,
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
            width = 3,
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
              conc_unit_input_ui(
                ns,
                id = "conc_unit",
                selected = conc_unit,
                allow_empty = TRUE
              )
            )
          ),
          shiny::column(
            width = 1,
            shiny::div(
              class = "unit-selectors",
              time_unit_input_ui(
                ns,
                id = "time_unit",
                selected = time_unit,
                allow_empty = TRUE
              )
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

# A plain <select> always reports its first option, so a picker rendered
# without a selection silently hands back the first unit. Prepending an empty
# placeholder keeps "nothing picked yet" distinguishable from a real choice.
unit_placeholder_label <- "—"

unit_placeholder_content <- paste0(
  "<span style='display: inline-block; font-style: italic; opacity: 0.6;'>",
  "Select unit</span>"
)

# Time unit choices
#' @export
time_unit_input_ui <- function(
  ns,
  id,
  label = TRUE,
  selected = NULL,
  allow_empty = FALSE
) {
  units <- c("s", "min")
  names <- c("seconds", "minutes")
  content <- {
    w_col1 <- "45px"
    w_col2 <- "95px"
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

  choices <- units
  if (allow_empty) {
    choices <- c(stats::setNames("", unit_placeholder_label), choices)
    content <- c(unit_placeholder_content, content)
    if (is.null(selected)) {
      selected <- ""
    }
  }

  shinyWidgets::pickerInput(
    inputId = ns(id),
    label = if (label) "Time Unit" else NULL,
    choices = choices,
    selected = selected,
    choicesOpt = list(content = content),
    options = shinyWidgets::pickerOptions(
      size = 10,
      showContent = FALSE,
      alignRight = TRUE
    )
  )
}

# Concentration unit choices
#' @export
conc_unit_input_ui <- function(
  ns,
  id,
  label = TRUE,
  selected = NULL,
  allow_empty = FALSE
) {
  units <- c("M", "mM", "μM", "nM", "pM")
  content <- {
    w_col1 <- "50px"
    w_col2 <- "95px"
    w_col3 <- "40px"
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

  choices <- units
  if (allow_empty) {
    choices <- c(stats::setNames("", unit_placeholder_label), choices)
    content <- c(unit_placeholder_content, content)
    if (is.null(selected)) {
      selected <- ""
    }
  }

  shinyWidgets::pickerInput(
    inputId = ns(id),
    label = if (label) "Conc. Unit" else NULL,
    choices = choices,
    selected = selected,
    choicesOpt = list(content = content),
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
      "= masses of one entry within 2 × peak tolerance"
    )
  ),
  shiny::div(
    class = "table-legend-element",
    shiny::div(
      class = "cell duplicated-mass-between"
    ),
    shiny::div(
      class = "table-legend-desc",
      "= masses of different entries within 2 × peak tolerance"
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
