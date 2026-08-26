# app/logic/plot_download.R

box::use(
  bslib,
  htmlwidgets[saveWidget],
  openxlsx[
    addStyle,
    addWorksheet,
    createStyle,
    createWorkbook,
    saveWorkbook,
    setColWidths,
    writeData
  ],
  plotly[as_widget, layout, plotly_json],
  shiny,
  shinyWidgets[materialSwitch, show_toast],
)

box::use(
  rlang[`%||%`],
)

# Wraps a gear-icon popover in a "Settings" hover tooltip.
#' @export
card_settings_popover <- function(content) {
  bslib::tooltip(
    shiny::div(
      bslib::popover(
        shiny::icon("gear"),
        content,
        title = NULL
      )
    ),
    "Settings",
    placement = "top"
  )
}

#' @export
plot_dl_popover <- function(ns, prefix) {
  bslib::tooltip(
    shiny::div(
      bslib::popover(
        shiny::icon("arrow-up-from-bracket"),
        shiny::div(
          class = "plot-dl-popover",
          shiny::radioButtons(
            ns(paste0(prefix, "_dl_theme")),
            label = "Theme",
            # Label and value used to be crossed over ("Dark" produced the
            # light theme), so the picker promised the opposite of what it
            # exported. The default is unchanged - still the light theme, the
            # one this first entry always produced - it just says so now.
            choices = c("Light" = "light", "Dark" = "dark"),
            selected = "light",
            inline = TRUE
          ),
          # Same call shape as the plot settings switches so it picks up the
          # existing .popover .material-switch styling rather than the
          # status-coloured variant.
          materialSwitch(
            ns(paste0(prefix, "_dl_transparent")),
            label = "Transparent Background",
            value = TRUE,
            right = TRUE
          ),
          shiny::radioButtons(
            ns(paste0(prefix, "_dl_quality")),
            label = "Quality (PNG)",
            choices = c("Low" = "low", "Normal" = "normal", "High" = "high"),
            selected = "normal",
            inline = TRUE
          ),
          shiny::radioButtons(
            ns(paste0(prefix, "_dl_context")),
            label = "Label Size",
            choices = c(
              "Small" = "small",
              "Medium" = "normal",
              "Large" = "large",
              "Very Large" = "xlarge"
            ),
            selected = "large",
            inline = TRUE
          ),
          shiny::div(class = "plot-dl-label", "File Format"),
          shiny::div(
            class = "plot-dl-buttons",
            shiny::downloadButton(
              ns(paste0("dl_", prefix, "_html")),
              "HTML",
              class = "btn-sm btn-default",
              icon = NULL
            ),
            shiny::actionButton(
              ns(paste0("dl_", prefix, "_png")),
              "PNG",
              class = "btn-sm btn-default"
            ),
            shiny::actionButton(
              ns(paste0("dl_", prefix, "_svg")),
              "SVG",
              class = "btn-sm btn-default"
            )
          )
        ),
        title = "Export Plot"
      )
    ),
    "Export",
    placement = "top"
  )
}

# Resolves the background an export should be drawn on. NULL keeps the plot
# transparent, which is what every plot builder produces on its own and what
# the switch leaves alone by default.
#' @export
export_bg_color <- function(theme, transparent) {
  if (!isFALSE(transparent)) {
    return(NULL)
  }
  if (identical(theme, "light")) "#ffffff" else "#121212"
}

# Builds a figure with the export palette active: brighten_hex() turns into a
# no-op for the duration, so an exported figure carries the palette's true
# colours instead of the lifted variants the dark in-app background needs to
# keep the darkest end of a scale legible.
#
# A global option rather than an argument threaded through every plot builder:
# the palette is built in a dozen different places, some of them several calls
# deep inside the plot functions, and a few of them behind cached reactives.
# Nothing here is concurrent - a Shiny process runs one session's reactive
# flush at a time and build_fn() is synchronous - so the option cannot leak
# into another session's render.
#' @export
with_export_palette <- function(expr) {
  old <- options(kiwims.export_palette = TRUE)
  on.exit(options(old), add = TRUE)
  force(expr)
}

# Registers HTML/PNG/SVG download handlers for a plot card.
# build_fn(theme) must return a plotly figure.
# filename_fn() must return a string (no extension).
#
# Several plot cards are created inside observers that re-run whenever new
# results arrive (the conversion results renderer, the deconvolution run
# handler), so setup_plot_dl() gets called again for a prefix that is already
# wired up. The HTML handler is an output assignment and simply replaces its
# predecessor, but PNG and SVG go through observeEvent() on the button: a
# second registration makes one click fire two observers, send two
# downloadPlot messages and start two browser downloads. That is what makes
# the browser ask whether the site may download multiple files, and why
# allowing it opens one save dialog per registration. Keep a single
# registration per namespaced prefix and only swap in the newest callbacks.
#' @export
setup_plot_dl <- function(
  input,
  output,
  session,
  prefix,
  build_fn,
  filename_fn,
  available_fn = NULL
) {
  registry <- session$userData$plot_dl_registry
  if (is.null(registry)) {
    registry <- new.env(parent = emptyenv())
    session$userData$plot_dl_registry <- registry
  }

  key <- session$ns(prefix)
  entry <- registry[[key]]

  if (!is.null(entry)) {
    entry$build_fn <- build_fn
    entry$filename_fn <- filename_fn
    entry$available_fn <- available_fn
    # Re-runs the export-state observer against the swapped callbacks.
    entry$version(shiny::isolate(entry$version()) + 1L)
    return(invisible(NULL))
  }

  entry <- new.env(parent = emptyenv())
  entry$build_fn <- build_fn
  entry$filename_fn <- filename_fn
  entry$available_fn <- available_fn
  entry$version <- shiny::reactiveVal(0L)
  registry[[key]] <- entry

  shiny::observe({
    entry$version()
    current_available_fn <- entry$available_fn
    if (is.null(current_available_fn)) {
      return(invisible(NULL))
    }
    session$sendCustomMessage("setExportState", list(
      prefix = prefix,
      enabled = isTRUE(current_available_fn())
    ))
  })

  no_plot_toast <- function() {
    shinyWidgets::show_toast(
      "No plot to export",
      text = "Generate a plot first before exporting.",
      type = "warning",
      timer = 4000,
      timerProgressBar = TRUE
    )
  }

  try_build <- function(theme) {
    tryCatch(
      shiny::isolate(with_export_palette(entry$build_fn(theme))),
      error = function(e) NULL
    )
  }

  # The theme the user picked for this export, and the background that goes
  # with it - NULL while the transparency switch is on.
  dl_theme <- function() {
    input[[paste0(prefix, "_dl_theme")]] %||% "light"
  }
  dl_background <- function() {
    export_bg_color(
      dl_theme(),
      input[[paste0(prefix, "_dl_transparent")]]
    )
  }

  output[[paste0("dl_", prefix, "_html")]] <- shiny::downloadHandler(
    filename = function() paste0(entry$filename_fn(), ".html"),
    content = function(file) {
      p <- try_build(dl_theme())
      if (is.null(p)) {
        no_plot_toast()
        shiny::req(FALSE)
      }
      show_toast(
        "Exporting as HTML",
        text = NULL,
        type = "info",
        timer = 3000,
        timerProgressBar = TRUE
      )
      # PNG and SVG have the background applied browser-side, in the
      # downloadPlot handler; the HTML export never goes through that, so it
      # is applied to the figure here instead.
      background <- dl_background()
      if (!is.null(background)) {
        p <- layout(p, paper_bgcolor = background, plot_bgcolor = background)
      }
      saveWidget(as_widget(p), file, selfcontained = TRUE)
    }
  )

  shiny::observeEvent(input[[paste0("dl_", prefix, "_png")]], {
    p <- try_build(dl_theme())
    if (is.null(p)) {
      no_plot_toast()
      return(invisible(NULL))
    }
    show_toast(
      "Exporting as PNG",
      text = NULL,
      type = "info",
      timer = 3000,
      timerProgressBar = TRUE
    )
    session$sendCustomMessage(
      "downloadPlot",
      list(
        json = plotly_json(p, jsonedit = FALSE),
        format = "png",
        quality = input[[paste0(prefix, "_dl_quality")]] %||% "normal",
        context = input[[paste0(prefix, "_dl_context")]] %||% "normal",
        # "" rather than NULL: a NULL list element would drop out of the
        # message entirely, and the handler reads this as "leave it alone".
        background = dl_background() %||% "",
        filename = entry$filename_fn()
      )
    )
  })

  shiny::observeEvent(input[[paste0("dl_", prefix, "_svg")]], {
    p <- try_build(dl_theme())
    if (is.null(p)) {
      no_plot_toast()
      return(invisible(NULL))
    }
    show_toast(
      "Exporting as SVG",
      text = NULL,
      type = "info",
      timer = 3000,
      timerProgressBar = TRUE
    )
    session$sendCustomMessage(
      "downloadPlot",
      list(
        json = plotly_json(p, jsonedit = FALSE),
        format = "svg",
        quality = input[[paste0(prefix, "_dl_quality")]] %||% "normal",
        context = input[[paste0(prefix, "_dl_context")]] %||% "normal",
        background = dl_background() %||% "",
        filename = entry$filename_fn()
      )
    )
  })
}

# Export popover for DT tables inside card headers (same icon as plot exports).
#' @export
table_dl_popover <- function(ns, prefix) {
  bslib::tooltip(
    shiny::div(
      bslib::popover(
        shiny::icon("arrow-up-from-bracket"),
        shiny::div(
          class = "plot-dl-popover",
          shiny::div(class = "plot-dl-label", "File Format"),
          shiny::div(
            class = "plot-dl-buttons",
            shiny::downloadButton(
              ns(paste0("dl_", prefix, "_csv")),
              "CSV",
              class = "btn-sm btn-default",
              icon = NULL
            ),
            shiny::downloadButton(
              ns(paste0("dl_", prefix, "_xlsx")),
              "Excel",
              class = "btn-sm btn-default",
              icon = NULL
            )
          )
        ),
        title = "Export Table"
      )
    ),
    "Export",
    placement = "top"
  )
}

# Renders CSV/Excel export buttons for a DT table.
#' @export
table_dl_buttons <- function(ns, prefix) {
  shiny::div(
    class = "table-dl-buttons",
    shiny::downloadButton(
      ns(paste0("dl_", prefix, "_csv")),
      "CSV",
      class = "btn-sm btn-default",
      icon = NULL
    ),
    shiny::downloadButton(
      ns(paste0("dl_", prefix, "_xlsx")),
      "Excel",
      class = "btn-sm btn-default",
      icon = NULL
    )
  )
}

# Columns that stay text on export even when every value happens to parse as a
# number (well IDs, replicate group labels and sample names can look numeric).
export_text_cols <- c(
  "Sample ID",
  "Protein",
  "Cmp Name",
  "Well",
  "Replicate",
  "Preferred"
)

# Restores the numeric type of every fully numeric column of an export table.
#
# The result tables are built for DT display, which stringifies some columns
# and leaves others as doubles — transform_hits() keeps concentration, time,
# the binding percentages and the theoretical protein mass numeric while
# turning the rest into text, and the kobs table hands over its standard error
# as text next to numeric rate constants. Exporting that mix writes half the
# numbers as text and half as real numbers, so a spreadsheet localises only
# the latter — the reason percentages showed up with a decimal comma while the
# neighbouring columns kept the dot.
#
# Idempotent, so it is safe to run on a table that already went through
# prepare_hits_export().
#' @export
normalize_export_types <- function(table) {
  if (is.null(table) || !ncol(table)) {
    return(table)
  }

  for (col in setdiff(names(table), export_text_cols)) {
    values <- table[[col]]
    if (!is.character(values)) {
      next
    }
    blank <- is.na(values) | trimws(values) %in% c("", "N/A")
    numbers <- suppressWarnings(as.numeric(values[!blank]))
    if (!length(numbers) || anyNA(numbers)) {
      next
    }
    table[[col]] <- replace(rep(NA_real_, length(values)), !blank, numbers)
  }

  # Missing values keep reading as "N/A" in the text columns; numeric columns
  # keep a real NA so the type survives (the writers spell it "N/A" again).
  for (col in names(table)) {
    if (is.character(table[[col]])) {
      table[[col]][is.na(table[[col]])] <- "N/A"
    }
  }

  table
}

# Prepares a hits table data.frame for export: drops the columns that only
# exist to drive the DT rendering, then normalises the column types.
#' @export
prepare_hits_export <- function(table) {
  table <- table[,
    !names(table) %in%
      c(
        "truncSample_ID",
        "label_color",
        "col_var",
        "trunc_label",
        "Mass Shift"
      ),
    drop = FALSE
  ]

  normalize_export_types(table)
}

# Registers CSV/Excel download handlers for a DT table.
# data_fn() must return a plain data.frame to export.
# filename_fn() must return a string (no extension).
#' @export
setup_table_dl <- function(
  input,
  output,
  session,
  prefix,
  data_fn,
  filename_fn
) {
  output[[paste0("dl_", prefix, "_csv")]] <- shiny::downloadHandler(
    filename = function() paste0(filename_fn(), ".csv"),
    content = function(file) {
      show_toast(
        "Exporting as CSV",
        text = NULL,
        type = "info",
        timer = 3000,
        timerProgressBar = TRUE
      )
      write_export_csv(normalize_export_types(data_fn()), file)
    }
  )

  output[[paste0("dl_", prefix, "_xlsx")]] <- shiny::downloadHandler(
    filename = function() paste0(filename_fn(), ".xlsx"),
    content = function(file) {
      show_toast(
        "Exporting as Excel",
        text = NULL,
        type = "info",
        timer = 3000,
        timerProgressBar = TRUE
      )
      write_export_xlsx(normalize_export_types(data_fn()), file)
    }
  )
}

# Writes a table as UTF-8 CSV with a byte order mark. Without the BOM Excel
# falls back to the system code page, which turns the "Δ" of the mass delta
# headers into mojibake.
#' @export
write_export_csv <- function(table, path) {
  chr <- vapply(table, is.character, logical(1))
  table[chr] <- lapply(table[chr], enc2utf8)
  names(table) <- enc2utf8(names(table))

  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con)
  utils::write.table(
    table,
    file = con,
    sep = ",",
    dec = ".",
    qmethod = "double",
    quote = TRUE,
    row.names = FALSE,
    col.names = TRUE,
    na = "N/A"
  )
}

# Writes a table as .xlsx with display formats attached to the numeric columns.
# Excel renders unformatted (General) cells in scientific notation as soon as
# the value carries more digits than the column is wide, which is what turned
# full precision percentages into "1,05E+01".
#' @export
write_export_xlsx <- function(table, path) {
  wb <- createWorkbook()
  addWorksheet(wb, "Table")
  writeData(wb, 1, table, keepNA = TRUE, na.string = "N/A")

  # Percentages and masses. Not every one of them is labelled with a unit in
  # brackets: the table views call their total column "Total %", and the adduct
  # view's "Mass 1", "Mass 2", … hold compound masses with no [Da] suffix at
  # all. Match those by name rather than leaving them on the General format.
  numeric_cols <- which(vapply(table, is.numeric, logical(1)))
  decimal_cols <- intersect(
    numeric_cols,
    grep("\\[%\\]|\\[Da\\]|%$|^Mass [0-9]+$", names(table))
  )
  if (length(decimal_cols) && nrow(table)) {
    addStyle(
      wb,
      1,
      style = createStyle(numFmt = "0.00"),
      rows = seq_len(nrow(table)) + 1L,
      cols = decimal_cols,
      gridExpand = TRUE
    )
  }

  # Concentration and time keep the General format — a molar concentration is
  # legitimately scientific — but every column gets a width that fits its
  # content so nothing collapses into an exponent for want of space.
  setColWidths(wb, 1, cols = seq_along(table), widths = "auto")
  saveWorkbook(wb, path, overwrite = TRUE)
}
