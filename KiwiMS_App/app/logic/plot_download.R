# app/logic/plot_download.R

box::use(
  base64enc[base64encode],
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

# Writes a widget to a single HTML file with every local dependency folded in.
# `background` paints the page body the figure sits on, and callers pass the
# same colour they gave the figure so the two cannot come apart.
#
# htmlwidgets does this through saveWidget(selfcontained = TRUE), which shells
# out to pandoc, and the installed app has no pandoc: R-Portable ships without
# one and nothing sets RSTUDIO_PANDOC, so rmarkdown finds nothing on any of
# the three paths it searches and saveWidget() aborts. That killed every HTML
# export outside a dev session - the content function errored, Shiny answered
# the download request with a 500, and the browser reported a failed download
# named after the output id rather than after filename_fn(). A dev session
# works only because RStudio exports RSTUDIO_PANDOC into the R process.
#
# So the widget goes out with its library folder instead, and each
# <script src> and <link href> is folded into the document here. Assets are
# inlined verbatim, which keeps the export the size pandoc produced; one that
# cannot be (it carries a sequence that would close its own tag) goes in as a
# data URI. Anything still pointing at the library folder afterwards throws -
# a loud failure beats an export that opens with pieces missing.
#' @export
save_widget_selfcontained <- function(widget, file, background = "white") {
  staging <- tempfile("kiwims-export-")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)

  staged <- file.path(staging, "widget.html")
  saveWidget(
    widget,
    staged,
    selfcontained = FALSE,
    libdir = "lib",
    background = background
  )

  doc <- readChar(staged, file.size(staged), useBytes = TRUE)
  Encoding(doc) <- "UTF-8"

  doc <- inline_widget_tags(
    doc,
    '<script[^>]*\\ssrc="(lib/[^"]+)"[^>]*>\\s*</script>',
    function(path, tag) inline_widget_script(staging, path)
  )
  doc <- inline_widget_tags(
    doc,
    '<link[^>]*\\shref="(lib/[^"]+)"[^>]*>',
    function(path, tag) inline_widget_style(staging, path, tag)
  )

  leftovers <- regmatches(
    doc,
    gregexpr('(?:src|href)="lib/[^"]*"', doc, perl = TRUE)
  )[[1]]
  if (length(leftovers)) {
    stop(
      "Export still references its library folder after inlining: ",
      paste(unique(leftovers), collapse = ", ")
    )
  }

  # The splicing works on offsets into a string that grows by megabytes as it
  # goes, and a document that lost its tail to a bad offset still passes every
  # check above - the references are gone because the text holding them is.
  # The closing tag is the cheap end-to-end proof that it is all still here.
  if (!grepl("</html>\\s*$", doc)) {
    stop("Export was truncated while inlining: the document has no closing tag")
  }

  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(doc), con, useBytes = TRUE)

  invisible(file)
}

# Replaces every match of `pattern` with builder(path, tag), `path` being the
# pattern's single capture group and `tag` the whole element.
#
# Splices from the last match backwards so the earlier offsets stay valid, and
# so an inlined asset that happens to contain something matching the pattern
# is never rescanned - a forwards pass would have to re-search the document
# after each replacement and could walk into the three megabytes of plotly.js
# it had just pasted in.
inline_widget_tags <- function(doc, pattern, builder) {
  matches <- gregexpr(pattern, doc, perl = TRUE)[[1]]
  if (matches[[1]] == -1L) {
    return(doc)
  }

  starts <- as.integer(matches)
  lengths <- attr(matches, "match.length")
  capture_starts <- as.integer(attr(matches, "capture.start"))
  capture_lengths <- as.integer(attr(matches, "capture.length"))

  for (i in rev(seq_along(starts))) {
    tag <- substring(doc, starts[i], starts[i] + lengths[i] - 1L)
    path <- substring(
      doc,
      capture_starts[i],
      capture_starts[i] + capture_lengths[i] - 1L
    )
    # substring() defaults `last` to 1000000L, so the tail has to be asked for
    # explicitly: left to the default it silently cuts the document at a
    # million characters, which is a third of the way into plotly.js.
    doc <- paste0(
      substring(doc, 1L, starts[i] - 1L),
      builder(path, tag),
      substring(doc, starts[i] + lengths[i], nchar(doc))
    )
  }

  doc
}

# Reads one dependency file out of the staging area as UTF-8 text.
read_widget_asset <- function(staging, path) {
  full <- file.path(staging, path)
  if (!file.exists(full)) {
    stop("Widget dependency missing from the export: ", path)
  }
  text <- readChar(full, file.size(full), useBytes = TRUE)
  Encoding(text) <- "UTF-8"
  text
}

inline_widget_script <- function(staging, path) {
  js <- read_widget_asset(staging, path)
  # An inline <script> ends at the first "</script" the parser sees, and an
  # "<!--" opens a comment the rest of the file disappears into. Neither
  # appears anywhere in the dependencies shipped today, so this costs nothing
  # now; it is here because a later build of any of them could carry one.
  if (grepl("</script|<!--", js, ignore.case = TRUE)) {
    return(paste0(
      '<script src="data:application/javascript;base64,',
      base64encode(charToRaw(js)),
      '"></script>'
    ))
  }
  paste0("<script>\n", js, "\n</script>")
}

inline_widget_style <- function(staging, path, tag) {
  if (!grepl('rel="stylesheet"', tag, fixed = TRUE)) {
    stop("Unsupported <link> in a plot export, not a stylesheet: ", tag)
  }

  css <- read_widget_asset(staging, path)

  # A url() would still point into the library folder the export does not
  # carry. Nothing in the current dependency set has one - the check is here
  # so a widget that does fails now, at export time, instead of opening later
  # with a missing icon and no explanation.
  urls <- regmatches(css, gregexpr("url\\([^)]*\\)", css, perl = TRUE))[[1]]
  targets <- gsub("^['\"]|['\"]$", "", trimws(gsub("^url\\(|\\)$", "", urls)))
  external <- targets[!grepl("^(data:|https?://|#)", targets)]
  if (length(external)) {
    stop(
      "Stylesheet ",
      path,
      " references files this export cannot carry: ",
      paste(unique(external), collapse = ", ")
    )
  }

  if (grepl("</style", css, ignore.case = TRUE)) {
    return(paste0(
      '<link rel="stylesheet" href="data:text/css;base64,',
      base64encode(charToRaw(css)),
      '" />'
    ))
  }
  paste0("<style>\n", css, "\n</style>")
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
    session$sendCustomMessage(
      "setExportState",
      list(
        prefix = prefix,
        enabled = isTRUE(current_available_fn())
      )
    )
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
      #
      # The transparency switch deliberately does not reach this export. It is
      # an affordance for the image formats, which get composited onto
      # whatever they are dropped into, so "no background at all" is a
      # meaningful answer there. An HTML export is a page and always paints
      # something: honouring the switch here left the figure transparent over
      # the white body saveWidget() writes, which is merely redundant under
      # the light theme but puts dark-theme plots on white. So HTML takes the
      # picked theme's own background either way, and takes it twice - once on
      # the figure, once on the page behind it, so the two cannot disagree.
      background <- export_bg_color(dl_theme(), transparent = FALSE)
      p <- layout(p, paper_bgcolor = background, plot_bgcolor = background)
      # save_widget_selfcontained() throws rather than write a half-inlined
      # file, and an error escaping a content function reaches the user only
      # as a failed browser download named after the output id - the symptom
      # that made the missing pandoc so hard to place. Name the reason in a
      # toast before letting the request fail.
      failure <- tryCatch(
        {
          save_widget_selfcontained(as_widget(p), file, background = background)
          NULL
        },
        error = function(e) conditionMessage(e)
      )
      if (!is.null(failure)) {
        show_toast(
          "HTML export failed",
          text = failure,
          type = "error",
          timer = 8000,
          timerProgressBar = TRUE
        )
        shiny::req(FALSE)
      }
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
