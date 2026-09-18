# app/logic/conversion_functions.R

box::use(
  app / logic / deconvolution_functions[spectrum_plot, process_plot_data, ],
  app /
    logic /
    conversion_constants[
      symbols,
      warning_sym,
      chart_js,
      sequential_scales,
      qualitative_scales,
      gradient_scales,
      paste_hook_js,
      hits_col_full_names,
      kinetics_settings
    ],
)

# Concentration conversion
#' @export
get_conversion_factor <- function(from, to) {
  scales <- c("M" = 0, "mM" = -3, "μM" = -6, "nM" = -9, "pM" = -12)

  if (!all(from %in% names(scales)) || !all(to %in% names(scales))) {
    stop(
      "Invalid unit. Supported units: ",
      paste(names(scales), collapse = ", ")
    )
  }

  unname(10^(scales[from] - scales[to]))
}

# Time conversion
#' @export
get_time_factor <- function(from, to) {
  scales <- c("s" = 1, "min" = 60)

  if (!all(from %in% names(scales)) || !all(to %in% names(scales))) {
    stop(
      "Invalid unit. Supported units: ",
      paste(names(scales), collapse = ", ")
    )
  }

  unname(scales[from] / scales[to])
}

# Extract the bare unit out of a column label such as "Conc. [M]"
#' @export
unit_symbol <- function(label) {
  gsub(".*\\[(.+)\\].*", "\\1", label)
}

# Replace the unit inside a column label, keeping the label prefix
swap_unit_symbol <- function(label, unit) {
  sub("\\[.*\\]", paste0("[", unit, "]"), label)
}

# Describes how the results are displayed relative to the units the samples
# were declared in. `units` are the source column labels, e.g.
# c(Concentration = "Conc. [M]", Time = "Time [min]"); `conc_unit`/`time_unit`
# are the units picked in the results interface (NULL = keep source unit).
#' @export
make_unit_view <- function(units, conc_unit = NULL, time_unit = NULL) {
  source_conc <- unit_symbol(units[["Concentration"]])
  source_time <- unit_symbol(units[["Time"]])

  if (is.null(conc_unit) || !nzchar(conc_unit)) {
    conc_unit <- source_conc
  }
  if (is.null(time_unit) || !nzchar(time_unit)) {
    time_unit <- source_time
  }

  conc_factor <- get_conversion_factor(from = source_conc, to = conc_unit)
  time_factor <- get_time_factor(from = source_time, to = time_unit)

  list(
    source_units = units,
    units = c(
      Concentration = swap_unit_symbol(units[["Concentration"]], conc_unit),
      Time = swap_unit_symbol(units[["Time"]], time_unit)
    ),
    conc_unit = conc_unit,
    time_unit = time_unit,
    conc_factor = conc_factor,
    time_factor = time_factor,
    identity = conc_factor == 1 && time_factor == 1
  )
}

# Concentrations double as list names, factor levels and row names across the
# result objects — convert them the same way everywhere so the keys stay in
# sync with the converted concentration column of the hits table.
#' @export
convert_conc_keys <- function(keys, view) {
  # Nothing to convert: hand back the keys verbatim so they keep matching the
  # untouched result objects, even when they are not in canonical numeric form
  if (view$identity) {
    return(stats::setNames(keys, keys))
  }

  stats::setNames(
    as.character(as.numeric(keys) * view$conc_factor),
    keys
  )
}

# Rescale and relabel the concentration/time columns of a hits table
#' @export
convert_hits_units <- function(hits, units, view) {
  if (is.null(hits) || view$identity) {
    return(hits)
  }

  conc_col <- units[["Concentration"]]
  time_col <- units[["Time"]]

  hits[[conc_col]] <- as.numeric(hits[[conc_col]]) * view$conc_factor
  hits[[time_col]] <- as.numeric(hits[[time_col]]) * view$time_factor

  names(hits)[names(hits) == conc_col] <- view$units[["Concentration"]]
  names(hits)[names(hits) == time_col] <- view$units[["Time"]]

  hits
}

# Rescale the binding/kobs result object. k_obs and v are rates (per unit
# time), the plateau (100 * v / kobs) is dimensionless and stays untouched.
#' @export
convert_kobs_result_units <- function(binding_kobs_result, view) {
  if (is.null(binding_kobs_result) || view$identity) {
    return(binding_kobs_result)
  }

  time_factor <- view$time_factor

  conc_names <- setdiff(
    names(binding_kobs_result),
    c(
      "binding_table",
      "binding_points",
      "binding_plot",
      "kobs_result_table",
      "skipped"
    )
  )

  # Concentrations that were left out of the fit are reported to the user, so
  # their labels follow the selected unit view like every other concentration
  if (
    !is.null(binding_kobs_result$skipped) &&
      nrow(binding_kobs_result$skipped) > 0
  ) {
    binding_kobs_result$skipped$concentration <- unname(convert_conc_keys(
      binding_kobs_result$skipped$concentration,
      view
    ))
  }

  for (i in conc_names) {
    entry <- binding_kobs_result[[i]]
    entry$kobs <- entry$kobs / time_factor
    entry$kobs_se <- entry$kobs_se / time_factor
    entry$v <- entry$v / time_factor
    if (!is.null(entry$predictions)) {
      entry$predictions$time <- entry$predictions$time * time_factor
    }
    if (!is.null(entry$hits)) {
      entry$hits$time <- entry$hits$time * time_factor
    }
    binding_kobs_result[[i]] <- entry
  }

  binding_points <- binding_kobs_result$binding_points
  if (!is.null(binding_points) && nrow(binding_points) > 0) {
    binding_points$time <- binding_points$time * time_factor
    levels(binding_points$concentration) <- unname(convert_conc_keys(
      levels(binding_points$concentration),
      view
    ))
    binding_kobs_result$binding_points <- binding_points
  }

  # Rename the per-concentration entries to the converted keys
  nms <- names(binding_kobs_result)
  nms[nms %in% conc_names] <- unname(convert_conc_keys(
    nms[nms %in% conc_names],
    view
  ))
  names(binding_kobs_result) <- nms

  binding_table <- binding_kobs_result$binding_table
  if (!is.null(binding_table) && nrow(binding_table) > 0) {
    binding_table$time <- binding_table$time * time_factor
    binding_table$kobs <- binding_table$kobs / time_factor
    binding_table$kobs_se <- binding_table$kobs_se / time_factor

    if (is.factor(binding_table$concentration)) {
      levels(binding_table$concentration) <- unname(convert_conc_keys(
        levels(binding_table$concentration),
        view
      ))
    } else {
      binding_table$concentration <- unname(convert_conc_keys(
        as.character(binding_table$concentration),
        view
      ))
    }

    binding_kobs_result$binding_table <- binding_table
  }

  kobs_result_table <- binding_kobs_result$kobs_result_table
  if (!is.null(kobs_result_table) && nrow(kobs_result_table) > 0) {
    kobs_result_table$kobs <- kobs_result_table$kobs / time_factor
    kobs_result_table$kobs_se <- kobs_result_table$kobs_se / time_factor
    kobs_result_table$v <- kobs_result_table$v / time_factor
    rownames(kobs_result_table) <- unname(convert_conc_keys(
      rownames(kobs_result_table),
      view
    ))

    binding_kobs_result$kobs_result_table <- kobs_result_table
  }

  binding_kobs_result
}

# Rescale the kinact/Ki parameter matrix: kinact is a rate, KI a
# concentration; t value and Pr(>|t|) are dimensionless.
#' @export
convert_kinact_ki_params <- function(params, view) {
  if (is.null(params) || view$identity) {
    return(params)
  }

  params[1, 1:2] <- params[1, 1:2] / view$time_factor
  params[2, 1:2] <- params[2, 1:2] * view$conc_factor

  params
}

# Rescale the complete kinact/Ki result (parameters and fitted kobs curve)
#' @export
convert_kinact_ki_units <- function(kinact_ki_result, view) {
  if (is.null(kinact_ki_result) || view$identity) {
    return(kinact_ki_result)
  }

  kinact_ki_result$Params <- convert_kinact_ki_params(
    kinact_ki_result$Params,
    view
  )

  # kinact/KI is per concentration per time; its t and p values are unitless
  ratio_factor <- view$time_factor * view$conc_factor
  ratio_cols <- c("Estimate", "Std. Error", "CI 2.5%", "CI 97.5%")
  if (!is.null(kinact_ki_result$Ratio)) {
    kinact_ki_result$Ratio[ratio_cols] <- kinact_ki_result$Ratio[ratio_cols] /
      ratio_factor
  }
  if (!is.null(kinact_ki_result$Params_CI)) {
    kinact_ki_result$Params_CI["kinact", ] <-
      kinact_ki_result$Params_CI["kinact", ] / view$time_factor
    kinact_ki_result$Params_CI["KI", ] <-
      kinact_ki_result$Params_CI["KI", ] * view$conc_factor
  }
  if (!is.null(kinact_ki_result$Series)) {
    series <- kinact_ki_result$Series
    series$ratio <- series$ratio / ratio_factor
    series$ratio_se <- series$ratio_se / ratio_factor
    series$kinact <- series$kinact / view$time_factor
    series$KI <- series$KI * view$conc_factor
    kinact_ki_result$Series <- series
  }
  if (!is.null(kinact_ki_result$Fit)) {
    kinact_ki_result$Fit$max_concentration <-
      kinact_ki_result$Fit$max_concentration * view$conc_factor
    kinact_ki_result$Fit$KI_hyperbolic <-
      kinact_ki_result$Fit$KI_hyperbolic * view$conc_factor
    kinact_ki_result$Fit$ratio_hyperbolic <-
      kinact_ki_result$Fit$ratio_hyperbolic / ratio_factor
    kinact_ki_result$Fit$ratio_linear <-
      kinact_ki_result$Fit$ratio_linear / ratio_factor
  }
  if (!is.null(kinact_ki_result$Points)) {
    kinact_ki_result$Points$conc <- kinact_ki_result$Points$conc *
      view$conc_factor
    kinact_ki_result$Points$time <- kinact_ki_result$Points$time *
      view$time_factor
  }
  if (!is.null(kinact_ki_result$Plateaus)) {
    kinact_ki_result$Plateaus$conc <- kinact_ki_result$Plateaus$conc *
      view$conc_factor
  }
  if (!is.null(kinact_ki_result$Bootstrap)) {
    kinact_ki_result$Bootstrap$ratio <- kinact_ki_result$Bootstrap$ratio /
      ratio_factor
    kinact_ki_result$Bootstrap$kinact <- kinact_ki_result$Bootstrap$kinact /
      view$time_factor
    kinact_ki_result$Bootstrap$KI <- kinact_ki_result$Bootstrap$KI *
      view$conc_factor
  }

  kobs_data <- kinact_ki_result$Kobs_Data
  if (!is.null(kobs_data) && nrow(kobs_data) > 0) {
    kobs_data$conc <- kobs_data$conc * view$conc_factor
    kobs_data$kobs <- kobs_data$kobs / view$time_factor
    kobs_data$kobs_se <- kobs_data$kobs_se / view$time_factor
    if ("predicted_kobs" %in% names(kobs_data)) {
      kobs_data$predicted_kobs <- kobs_data$predicted_kobs / view$time_factor
    }

    kinact_ki_result$Kobs_Data <- kobs_data
  }

  kinact_ki_result
}

# Rescale every unit-bearing part of a result list
#' @export
convert_result_list_units <- function(result_list, view) {
  if (is.null(result_list) || view$identity) {
    return(result_list)
  }

  result_list$binding_kobs_result <- convert_kobs_result_units(
    result_list$binding_kobs_result,
    view
  )
  result_list$kinact_ki_result <- convert_kinact_ki_units(
    result_list$kinact_ki_result,
    view
  )

  result_list
}

# Empty default tables
#' @export
empty_prot_comp_tbl <- function(type) {
  na_num <- matrix(NA_real_, nrow = 9, ncol = 9) |>
    as.data.frame() |>
    stats::setNames(paste("Mass", 1:9))

  if (type == "Protein") {
    cbind(Protein = as.character(rep(NA, 9)), na_num)
  } else {
    cbind(Compound = as.character(rep(NA, 9)), na_num)
  }
}

# Helper function to process uploaded table
#' @export
process_uploaded_table <- function(df, type) {
  # Check if first column contains values
  if (anyNA(df[, 1])) {
    return("First column (name) must contain values.")
  }

  # Skip header row if first row is non-numeric
  if (all(is.na(suppressWarnings(as.numeric(as.character(df[1, ])))))) {
    message(
      "Table appears to contain header text. Skipping first row conversion."
    )
    df <- df[-1, , drop = FALSE]
  }

  # Check if table has at least two columns (name and mass)
  if (is.null(df) || nrow(df) == 0 || ncol(df) < 2) {
    return("Table must contain at least two columns: name and mass.")
  }

  # Define expected column names based on type
  expected_cols <- c(
    ifelse(type == "Protein", "Protein", "Compound"),
    paste("Mass", 1:9)
  )

  # Take first up to 10 columns
  num_cols <- min(ncol(df), 10)
  df <- df[, 1:num_cols, drop = FALSE]

  # Rename columns to expected
  colnames(df) <- expected_cols[1:num_cols]

  # Add missing columns with NAs if less than 10
  if (num_cols < 10) {
    for (i in (num_cols + 1):10) {
      df[[expected_cols[i]]] <- NA
    }
  }

  # Convert mass columns to numeric (strip trailing unit suffixes like " Da", " kDa")
  converted_df <- suppressWarnings(dplyr::mutate_all(df[, -1], function(x) {
    x_stripped <- gsub("\\s+[A-Za-z].*$", "", trimws(as.character(x)))
    as.numeric(x_stripped)
  }))

  # Check if conversion resulted in NAs only where original had NAs
  if (identical(which(is.na(df[, -1])), which(is.na(converted_df)))) {
    df[, -1] <- converted_df
  } else {
    # If there are NAs in converted_df that were not NAs in original df, return error
    return("Mass fields require numeric values.")
  }

  return(df)
}

# Validate mandatory tables in a deconvolution result DB.
# Returns NULL if valid, or a character error message.
#' @export
validate_decon_db <- function(db_path) {
  if (!file.exists(db_path)) {
    return("File not found.")
  }

  tryCatch(
    {
      con <- DBI::dbConnect(
        RSQLite::SQLite(),
        db_path,
        flags = RSQLite::SQLITE_RO
      )
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      if (!DBI::dbExistsTable(con, "metadata")) {
        return("Invalid result file: metadata table missing.")
      }
      if (DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM metadata")$n == 0L) {
        return("Invalid result file: no samples in metadata.")
      }

      if (!DBI::dbExistsTable(con, "status")) {
        return("Invalid result file: status table missing.")
      }
      n_done <- DBI::dbGetQuery(
        con,
        "SELECT COUNT(*) AS n FROM status WHERE state = 'done'"
      )$n
      if (n_done == 0L) {
        return(
          "Result file contains no completed samples (no entries with state = 'done')."
        )
      }

      for (tbl in c("mass_data", "error", "config")) {
        if (!DBI::dbExistsTable(con, tbl)) {
          return(sprintf(
            "Invalid result file: required table '%s' is missing.",
            tbl
          ))
        }
        if (
          DBI::dbGetQuery(
            con,
            sprintf("SELECT COUNT(*) AS n FROM \"%s\"", tbl)
          )$n ==
            0L
        ) {
          return(sprintf("Invalid result file: table '%s' is empty.", tbl))
        }
      }

      if (!DBI::dbExistsTable(con, "peaks")) {
        return("Invalid result file: required table 'peaks' is missing.")
      }

      NULL
    },
    error = function(e) {
      paste("Could not read result file:", conditionMessage(e))
    }
  )
}

# Helper: query done sample names from a status table
.done_samples <- function(con) {
  DBI::dbGetQuery(con, "SELECT sample FROM status WHERE state = 'done'")[[
    "sample"
  ]]
}

# Read only metadata (sample names, session, output) from a result SQLite DB.
# Only samples with state = 'done' in the status table are returned.
# session and output_log are written only on completed runs; older or interrupted
# DBs may not have them.
#' @export
read_decon_metadata <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  read_if_exists <- function(tbl, query) {
    if (DBI::dbExistsTable(con, tbl)) {
      DBI::dbGetQuery(con, query)[[1L]]
    } else {
      NULL
    }
  }

  list(
    samples = .done_samples(con),
    session = read_if_exists(
      "session",
      "SELECT line FROM session ORDER BY line_num"
    ),
    output = read_if_exists(
      "output_log",
      "SELECT line FROM output_log ORDER BY line_num"
    )
  )
}

# Read max peak mass per sample from the peaks table (lightweight, for heatmap)
#' @export
read_decon_peaks_max <- function(db_path, samples = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  where <- if (!is.null(samples) && length(samples) > 0) {
    quoted <- paste(
      sapply(samples, function(s) DBI::dbQuoteLiteral(con, s)),
      collapse = ", "
    )
    sprintf("WHERE sample IN (%s)", quoted)
  } else {
    ""
  }
  DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT sample, MAX(mass) AS max_mass FROM peaks %s GROUP BY sample",
      where
    )
  )
}

# Read full result from a result SQLite DB, reconstructing the nested list structure.
# Only samples with state = 'done' in the status table are included unless an
# explicit samples vector is supplied (which is then intersected with done samples).
#' @export
read_decon_result <- function(db_path, samples = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  done <- .done_samples(con)
  all_samples <- if (!is.null(samples)) intersect(done, samples) else done

  q <- function(tbl, s) {
    if (!DBI::dbExistsTable(con, tbl)) {
      return(data.frame())
    }
    DBI::dbGetQuery(
      con,
      sprintf("SELECT * FROM %s WHERE sample = ?", tbl),
      params = list(s)
    )
  }

  deconvolution <- lapply(
    stats::setNames(all_samples, all_samples),
    function(s) {
      config_long <- q("config", s)
      config_wide <- if (nrow(config_long) > 0) {
        tidyr::pivot_wider(
          config_long[, c("key", "value")],
          names_from = "key",
          values_from = "value"
        )
      } else {
        data.frame()
      }

      raw <- q("rawdata", s)
      raw$sample <- NULL
      inp <- q("input_dat", s)
      inp$sample <- NULL
      peaks <- q("peaks", s)
      peaks$sample <- NULL
      mass <- q("mass_data", s)
      mass$sample <- NULL
      err <- q("error", s)
      err$sample <- NULL

      list(
        config = config_wide,
        peaks = peaks,
        error = err,
        rawdata = raw,
        mass = mass,
        input = inp
      )
    }
  )

  read_if_exists <- function(tbl, query) {
    if (DBI::dbExistsTable(con, tbl)) {
      DBI::dbGetQuery(con, query)[[1L]]
    } else {
      NULL
    }
  }

  list(
    deconvolution = deconvolution,
    session = read_if_exists(
      "session",
      "SELECT line FROM session ORDER BY line_num"
    ),
    output = read_if_exists(
      "output_log",
      "SELECT line FROM output_log ORDER BY line_num"
    )
  )
}

# Helper function to read uploaded files
#' @export
read_uploaded_file <- function(file_path, ext) {
  if (ext %in% c("csv")) {
    df <- utils::read.csv(
      file_path,
      stringsAsFactors = FALSE,
      sep = ",",
      header = FALSE
    )

    # If only one column read, try semicolon separator
    if (ncol(df) == 1) {
      df <- utils::read.csv(
        file_path,
        stringsAsFactors = FALSE,
        sep = ";",
        header = FALSE
      )
    }
  } else if (ext == "tsv") {
    df <- suppressMessages(readr::read_tsv(
      file_path,
      show_col_types = FALSE,
      col_names = FALSE
    ))
  } else if (ext == "txt") {
    df <- utils::read.delim(
      file_path,
      stringsAsFactors = FALSE,
      header = FALSE
    )
  } else if (ext %in% c("xlsx", "xls")) {
    df <- suppressMessages(readxl::read_excel(file_path, col_names = FALSE))
  } else {
    stop("Unsupported file format")
  }

  # Ensure column names are trimmed of whitespace
  colnames(df) <- trimws(colnames(df))

  return(df)
}

# Function to set the selected tab
#' @export
set_selected_tab <- function(tab_name, session, id = "tabs") {
  bslib::nav_select(
    id = id,
    selected = tab_name,
    session = session
  )
}

# Render function for protein/compound declaration tables
#' @export
prot_comp_handsontable <- function(
  tab,
  tolerance = NULL,
  disabled = FALSE,
  proteins = NULL,
  compounds = NULL
) {
  if (is.null(tab) || nrow(tab) == 0) {
    return(NULL)
  }
  js_tolerance_value <- if (is.null(tolerance) || is.na(tolerance)) {
    "null"
  } else {
    tolerance
  }

  # Original renderer (no edit detection needed)
  renderer_js <- sprintf(
    "function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      
      td.style.background = ''; // Clear existing background
      td.style.color = '';      // Clear existing text color
      
      var GLOBAL_TOLERANCE = %s; 
      
      var getNormalizedValue = function(val) {
        if (val == null || val === '') {
          return { val: '', is_numeric: false };
        }
        var floatVal = parseFloat(val);
        if (isNaN(floatVal)) {
          return { val: String(val).trim(), is_numeric: false }; 
        } else {
          return { val: floatVal, is_numeric: true }; 
        }
      };

      var cellData = getNormalizedValue(value);
      
      if (cellData.val === '') return;

      var isNameDuplicated = false;
      if (col === 0) {
        var colData = instance.getDataAtCol(0); 
        var valueCounts = {};
        for (var i = 0; i < colData.length; i++) {
          var cVal = colData[i];
          var tVal = cVal == null ? '' : String(cVal).trim();
          if (tVal !== '') valueCounts[tVal] = (valueCounts[tVal] || 0) + 1;
        }
        if (valueCounts[cellData.val] > 1) isNameDuplicated = true;
      }
      
      var isSameRowProximate = false;
      var isDiffRowProximate = false;
      
      if (col >= 1 && cellData.is_numeric && GLOBAL_TOLERANCE !== null) {
        var totalRows = instance.countRows();
        var totalCols = instance.countCols();
        var current_val = cellData.val;
        
        for (var r = 0; r < totalRows; r++) {
          if (isSameRowProximate && isDiffRowProximate) break;
          for (var c = 1; c < totalCols; c++) {
            if (r === row && c === col) continue; 
            var other_value = instance.getDataAtCell(r, c);
            var other_data = getNormalizedValue(other_value);
            if (other_data.is_numeric) {
              var diff = Math.abs(current_val - other_data.val);
              if (diff < GLOBAL_TOLERANCE) {
                if (r === row) isSameRowProximate = true;
                else isDiffRowProximate = true;
              }
            }
          }
        }
      }
      
      if (isSameRowProximate) {
        td.style.background = 'repeating-linear-gradient(-45deg, #fbfbe7, #fbfbe7 5px, #ffa50000 5px, #ffa50073 10px)';
      } 
      else if (isDiffRowProximate) {
        td.style.background = 'rgb(251 251 231)';
      } 
      else if (isNameDuplicated) {
        td.style.background = 'orange';
        td.style.color = 'white';
      }
    }",
    js_tolerance_value
  )

  # Build the table
  table <- rhandsontable::rhandsontable(
    tab,
    rowHeaders = NULL,
    height = 28 + 23 * ifelse(nrow(tab > 14), 14, nrow(tab)),
    stretchH = ifelse(disabled, "none", "all")
  ) |>
    rhandsontable::hot_cols(fixedColumnsLeft = 1, renderer = renderer_js) |>
    rhandsontable::hot_cols(
      cols = 2:ncol(tab),
      format = "0.##########",
    ) |>
    rhandsontable::hot_validate_numeric(
      cols = 2:ncol(tab),
      min = 1,
      allowInvalid = TRUE
    ) |>
    rhandsontable::hot_table(
      contextMenu = TRUE,
      highlightCol = TRUE,
      highlightRow = TRUE,
      stretchH = ifelse(disabled, "none", "all")
    ) |>
    htmlwidgets::onRender(paste_hook_js) |>
    htmlwidgets::onRender(
      "function(el, x) {
        this.hot.updateSettings({
          contextMenu: { items: { row_above: {}, row_below: {} } }
        });
      }"
    )

  if (disabled) {
    table <- rhandsontable::hot_cols(table, readOnly = TRUE)
  }

  return(table)
}

#' @export
sample_handsontable <- function(
  tab,
  proteins = NULL,
  compounds = NULL,
  disabled = FALSE
) {
  cmp_cols <- grep("Compound", colnames(tab))

  # Identify Concentration/Time columns (grepl to handle unit-suffixed names)
  conc_time_idx <- grep("^Concentration|^Time", colnames(tab))

  # Identify Replicate column (read-only, excluded from validation)
  replicate_idx <- grep("^Replicate$", colnames(tab))
  has_replicate <- length(replicate_idx) > 0

  # All columns that must be skipped by the JS renderer (no validation/duplicate check)
  skip_cols <- c(
    conc_time_idx,
    if (has_replicate) replicate_idx else integer(0)
  )

  # Protein and compound column 0-indexed JS positions shift when Replicate is present
  protein_col_js <- if (has_replicate) 2L else 1L
  compound_col_js <- if (has_replicate) 3L else 2L

  # Allowed protein and compound values
  if (!is.null(proteins) && !is.null(compounds)) {
    allowed_per_col <- list(
      NULL,
      proteins,
      compounds
    )

    # Custom renderer
    renderer_js <- sprintf(
      "function(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);

    td.style.background = ''; // Clear existing background for new rendering

    // Skipped columns: Concentration, Time, and Replicate
    var concTimeCols = %s;
    if (concTimeCols.indexOf(col) !== -1) { return; }

    var allowedPerCol = instance.params ? instance.params.allowed_per_col : null;
    var normalizedValue = value == null ? '' : String(value).trim();

    var allowedRaw;
    if (col === %d) {
      allowedRaw = allowedPerCol ? allowedPerCol[1] : null;
    } else if (col >= %d) {
      allowedRaw = allowedPerCol ? allowedPerCol[2] : null;
    } else {
      return;
    }

    // --- 1. Prepare allowed list ---
    var allowedList = [];
    if (Array.isArray(allowedRaw)) {
      allowedList = allowedRaw;
    } else if (typeof allowedRaw === 'string' && allowedRaw.length > 0) {
      allowedList = [allowedRaw];
    } else if (allowedRaw && Array.isArray(allowedRaw) === false) {
      allowedList = [allowedRaw];
    }

    // --- 2. Check Validity (Red Highlight Logic) ---
    var isValid = true;
    if (allowedList.length > 0) {
      isValid = allowedList.includes(normalizedValue) || normalizedValue === '';
    }

    // --- 3. Check Duplication (Orange Highlight Logic) ---
    // Exclude col 0 (Sample) and skipped columns from duplicate scan
    var isDuplicated = false;
    if (normalizedValue !== '') {
      var rowData = instance.getDataAtRow(row);
      var valueCounts = {};
      for (var i = 1; i < rowData.length; i++) {
          if (concTimeCols.indexOf(i) !== -1) { continue; }
          var cellValue = rowData[i];
          var trimmedValue = cellValue == null ? '' : String(cellValue).trim();
          if (trimmedValue !== '') {
              valueCounts[trimmedValue] = (valueCounts[trimmedValue] || 0) + 1;
          }
      }
      if (valueCounts[normalizedValue] > 1) {
          isDuplicated = true;
      }
    }

    // --- 4. Apply Styles based on Priority ---
    if (!isValid) {
      td.style.background = 'red';
    } else if (isDuplicated) {
      td.style.background = 'orange';
    }
  }",
      paste0("[", paste(skip_cols - 1L, collapse = ","), "]"),
      protein_col_js,
      compound_col_js
    )
  } else {
    allowed_per_col <- list(NULL)
    renderer_js <- ""
  }

  # Pre-convert concentration/time columns to character to prevent handsontable
  # numeric type from rounding values in the data source via numeral.js
  display_tab <- tab
  if (length(conc_time_idx) == 2) {
    for (idx in conc_time_idx) {
      display_tab[[idx]] <- as.character(display_tab[[idx]])
    }
  }

  handsontable <- rhandsontable::rhandsontable(
    display_tab,
    rowHeaders = NULL,
    allowed_per_col = allowed_per_col,
    height = 28 + 23 * ifelse(nrow(tab > 15), 15, nrow(tab)),
    stretchH = "all"
  ) |>
    rhandsontable::hot_cols(
      fixedColumnsLeft = if (has_replicate) 3L else 2L,
      type = "text",
      readOnly = ifelse(disabled, TRUE, FALSE)
    ) |>
    rhandsontable::hot_col(
      col = "Protein",
      type = "autocomplete",
      source = proteins,
      strict = FALSE
    ) |>
    rhandsontable::hot_col(col = 2:max(cmp_cols), renderer = renderer_js) |>
    rhandsontable::hot_col("Sample", readOnly = TRUE) |>
    rhandsontable::hot_col(
      col = min(cmp_cols):max(cmp_cols),
      type = "autocomplete",
      source = compounds,
      strict = FALSE
    ) |>
    rhandsontable::hot_table(
      contextMenu = FALSE,
      stretchH = "all"
    )

  if (has_replicate) {
    handsontable <- rhandsontable::hot_col(
      handsontable,
      "Replicate",
      readOnly = TRUE
    )
  }

  if (length(conc_time_idx) == 2) {
    handsontable <- rhandsontable::hot_validate_numeric(
      handsontable,
      cols = conc_time_idx,
      min = 0
    )
  }

  return(handsontable)
}

# Function to fill missing columns in sample table
#' @export
fill_sample_table <- function(sample_table, kinact_ki) {
  # Stash Replicate (not part of the 7-col standard) to avoid count mismatch
  has_rep <- "Replicate" %in% names(sample_table)
  rep_col <- if (has_rep) sample_table[["Replicate"]] else NULL
  if (has_rep) {
    sample_table <- sample_table[,
      names(sample_table) != "Replicate",
      drop = FALSE
    ]
  }

  if (kinact_ki) {
    conc_time <- sample_table[, sapply(
      c("Concentration", "Time"),
      grep,
      names(sample_table)
    )]
    names(conc_time) <- c("Concentration", "Time")

    sample_table <- sample_table[,
      -sapply(
        c("Concentration", "Time"),
        grep,
        names(sample_table)
      )
    ]
  }

  col_diff <- abs(ncol(sample_table) - 7)

  if (col_diff != 0) {
    sample_table <- cbind(
      sample_table,
      (data.frame(rep(list(rep("", nrow(sample_table))), col_diff)))
    )
    names(sample_table) <- c("Sample", "Protein", paste("Compound", 1:5))
  }

  # Re-attach Concentration/Time outside the col_diff block so they are
  # preserved regardless of whether padding was needed.
  if (kinact_ki) {
    sample_table <- cbind(sample_table, conc_time)
  }

  # Reattach Replicate right after Sample
  if (has_rep) {
    sample_table <- cbind(
      sample_table[, "Sample", drop = FALSE],
      Replicate = rep_col,
      sample_table[, setdiff(names(sample_table), "Sample"), drop = FALSE]
    )
  }

  return(sample_table)
}

# Construct cleaned-up sample table with only consecutive non-NA entries
#' @export
clean_sample_table <- function(sample_table, units = NULL) {
  # Use grepl so unit-suffixed names like "Concentration [M]" / "Time [s]"
  # are detected correctly alongside plain "Concentration" / "Time"
  conc_col <- grep("^Concentration", names(sample_table), value = TRUE)
  time_col <- grep("^Time", names(sample_table), value = TRUE)
  has_conc_time <- length(conc_col) == 1 && length(time_col) == 1

  no_cmp_cols <- grepl("^Sample$|^Protein$|^Replicate$", names(sample_table)) |
    (has_conc_time & names(sample_table) %in% c(conc_col, time_col))

  extra_cmp_section <- sample_table[,
    which(!no_cmp_cols),
    drop = FALSE
  ]

  df <- extra_cmp_section[,
    colSums(as.matrix(is.na(extra_cmp_section) | extra_cmp_section == "")) !=
      nrow(extra_cmp_section),
    drop = FALSE
  ]

  # Rebuild data frame with consecutive values
  if (isTRUE(ncol(extra_cmp_section) > 0)) {
    df <- data.frame()
    for (i in seq_len(nrow(extra_cmp_section))) {
      # Extract vector from input table
      row_noNA <- unlist(extra_cmp_section[i, ])[
        !is.na(unlist(extra_cmp_section[i, ])) &
          unlist(extra_cmp_section[i, ]) != ""
      ]

      if (!length(row_noNA)) {
        row_noNA <- ""
      }

      # Adjust column differences
      if (i != 1) {
        col_diff <- ncol(df) - length(row_noNA)
        if (col_diff > 0) {
          row_noNA <- c(row_noNA, rep("", col_diff))
        } else if (col_diff < 0) {
          df <- cbind(df, rep(list(""), abs(col_diff)))
        }
      }

      df <- rbind(df, row_noNA)
    }

    # Correct mass columns to be character
    df <- as.data.frame(apply(df, c(1, 2), as.character))
  }

  # Reattach Sample, Replicate (if present), and Protein; then compound columns
  header_cols <- intersect(
    c("Sample", "Replicate", "Protein"),
    names(sample_table)
  )
  df <- cbind(sample_table[, header_cols, drop = FALSE], df)
  if (has_conc_time) {
    df <- cbind(df, sample_table[, c(conc_col, time_col), drop = FALSE])
  }

  # Number of compound columns: total minus header cols and optional Conc/Time
  n_header <- length(header_cols)
  n_cmp <- ncol(df) - n_header - ifelse(has_conc_time, 2L, 0L)

  # Rename columns — preserve or apply units to Conc/Time names
  names(df) <- c(
    header_cols,
    if (n_cmp > 0) paste("Compound", seq_len(n_cmp)) else character(0),
    if (has_conc_time) {
      c(
        paste0(
          "Concentration",
          if (!is.null(units)) paste0(" [", units$conc, "]")
        ),
        paste0("Time", if (!is.null(units)) paste0(" [", units$time, "]"))
      )
    }
  )

  return(df)
}

# Construct cleaned-up prot/cmp table with only consecutive non-NA entries
#' @export
clean_prot_comp_table <- function(tab, table, full = FALSE) {
  # Keep only rows without NAs
  table <- table[rowSums(is.na(table) | table == "") != ncol(table), ]

  # If empty return empty table
  if (!nrow(table) | all(is.na(table) | table == "")) {
    return(table)
  }

  # Rebuild data frame with consecutive values
  df <- data.frame()
  for (i in 1:nrow(table)) {
    # Extract vector from input table
    row_noNA <- unlist(table[i, ])[!is.na(unlist(table[i, ]))]

    # Case name column is NA
    if (!tab %in% names(row_noNA)) {
      row_noNA <- c(as.character(NA), row_noNA)
      names(row_noNA)[1] <- tab
    }

    # Adjust column differences
    col_diff <- ncol(df) - length(row_noNA)
    if (i != 1 && col_diff > 0) {
      row_noNA <- c(row_noNA, rep(as.numeric(NA), col_diff))
    } else if (i != 1 && col_diff < 0) {
      df <- cbind(df, rep(list(NA), abs(col_diff)))
    }

    df <- rbind(df, row_noNA)
  }

  # Correct mass columns to be numeric and name column character
  df[, -1] <- as.data.frame(apply(
    df[, -1, drop = FALSE],
    c(1, 2),
    as.numeric
  ))
  df[, 1] <- as.character(df[, 1])

  if (nrow(df) > 0 && ncol(df) > 1) {
    if (full) {
      # Get missing columns and rows to achieve target dimension (9, 10)
      missing_cols <- 10 - ncol(df)
      missing_rows <- ifelse(nrow(df) > 9, 0, 9 - nrow(df))

      if (missing_cols != 0 & missing_rows != 0) {
        # Fill up cols with NAs
        df <- cbind(df, rep(list(as.numeric(NA)), missing_cols))

        # Fill up rows with NAs
        df_add_miss_rows <- data.frame(c(
          list(rep(as.character(NA), missing_rows)),
          rep(list(rep(as.numeric(NA), missing_rows)), 9)
        ))

        suppressWarnings({
          df[, -1] <- as.data.frame(apply(
            df[, -1, drop = FALSE],
            c(1, 2),
            as.numeric
          ))
        })

        # Equalize names before merge
        names(df_add_miss_rows) <- names(df)

        # Merge on rows
        df <- rbind(df, df_add_miss_rows)
      } else if (missing_cols != 0 & missing_rows == 0) {
        # Fill up cols with NAs
        df <- cbind(df, rep(list(as.numeric(NA)), missing_cols))
      } else if (missing_cols == 0 & missing_rows != 0) {
        # Fill up rows with NAs
        df_add_miss_rows <- data.frame(c(
          list(rep(as.character(NA), missing_rows)),
          rep(list(rep(as.numeric(NA), missing_rows)), 9)
        ))

        # Equalize names before merge
        names(df_add_miss_rows) <- names(df)

        # Merge on rows
        df <- rbind(df, df_add_miss_rows)
      }
    }
  }

  # Rename columns
  if (ncol(df) == 1 & class(df[1, ]) == "character") {
    names(df) <- tab
  } else {
    names(df) <- c(tab, paste("Mass", 1:(ncol(df) - 1)))
  }

  return(df)
}

# Slice declaration tables column-wise
#' @export
slice_rows <- function(tab) {
  row_contain <- which(rowSums(is.na(tab) | tab == "") != ncol(tab))
  return(tab[row_contain, ])
}

# Slice sample declaration table row-wise
#' @export
slice_cols <- function(sample_table) {
  non_empty <- which(
    colSums(is.na(sample_table) | sample_table == "") != nrow(sample_table)
  )
  return(sample_table[, non_empty])
}

# Validate sample table
#' @export
check_sample_table <- function(sample_table, proteins, compounds) {
  conc_col <- grep("^Concentration", names(sample_table), value = TRUE)
  time_col <- grep("^Time", names(sample_table), value = TRUE)
  has_conc_time <- length(conc_col) == 1 && length(time_col) == 1

  if (has_conc_time) {
    conc_time_tbl <- sample_table[, c(conc_col, time_col), drop = FALSE]
    sample_table <- sample_table[,
      !names(sample_table) %in% c(conc_col, time_col),
      drop = FALSE
    ]
  }

  # Strip Replicate so positional checks below remain Sample | Protein | Compounds
  sample_table <- sample_table[,
    names(sample_table) != "Replicate",
    drop = FALSE
  ]

  # Check if protein and compound names present
  if (is.null(proteins) || is.null(compounds)) {
    return("Declare Proteins and Compounds")
  }

  # Check if protein names valid
  proteins_input <- sample_table[, 2][
    !is.na(sample_table[, 2]) & sample_table[, 2] != ""
  ]
  if (length(proteins_input) & any(!proteins_input %in% proteins)) {
    return("Protein name not declared")
  }

  # Check if compound names valid
  compounds_input <- sample_table[, -(1:2)][
    !is.na(sample_table[, -(1:2)]) & sample_table[, -(1:2)] != ""
  ]
  if (length(compounds_input) & any(!compounds_input %in% compounds)) {
    return("Compound name not declared")
  }

  # If all proteins empty
  if (any(sample_table[, 2] == "" | is.na(sample_table[, 2]))) {
    return("Assign proteins")
  }

  # If all compounds empty
  if (
    any(
      rowSums(
        sample_table[, -(1:2), drop = FALSE] != "" &
          !is.na(sample_table[, -(1:2), drop = FALSE])
      ) <
        1
    )
  ) {
    return("Assign compounds")
  }

  # Check for duplicated compounds
  if (
    any(t(apply(
      sample_table[, -(1:2), drop = FALSE],
      1,
      duplicated,
      incomparables = ""
    )))
  ) {
    return("Duplicated compounds")
  }

  if (has_conc_time) {
    conc_vals <- conc_time_tbl[[conc_col]]
    time_vals <- conc_time_tbl[[time_col]]

    # Check for missing values
    if (any(is.na(conc_vals))) {
      return("Fill Concentrations")
    }
    if (any(is.na(time_vals))) {
      return("Fill Time")
    }

    # At least 3 and at most 10 distinct non-zero concentrations required
    # (zero is allowed but does not count toward these limits)
    n_conc <- length(unique(conc_vals[!is.na(conc_vals) & conc_vals != 0]))
    if (n_conc < 3) {
      return(paste0(
        "At least 3 different non-zero concentrations required (",
        n_conc,
        " present)"
      ))
    }
    if (n_conc > 10) {
      return(paste0(
        "At most 10 different non-zero concentrations allowed (",
        n_conc,
        " present)"
      ))
    }

    # For each unique non-zero concentration, require at least 3 distinct non-zero time points
    # (concentration = 0 is excluded from this check — only one sample is allowed there)
    unique_concs <- unique(conc_vals[!is.na(conc_vals) & conc_vals != 0])
    for (uc in unique_concs) {
      times_for_conc <- time_vals[!is.na(conc_vals) & conc_vals == uc]
      n_time <- length(unique(times_for_conc[
        !is.na(times_for_conc) & times_for_conc != 0
      ]))
      if (n_time < 3) {
        return(paste0(
          "At least 3 different non-zero time points required per concentration (concentration ",
          uc,
          " has only ",
          n_time,
          ")"
        ))
      }
    }
  }

  return(TRUE)
}

### Check duplicated masses
check_mass_duplicates <- function(tab, tolerance) {
  numeric_part <- tab[, -1, drop = FALSE]

  # Flatten the data frame
  all_values <- as.vector(as.matrix(numeric_part))

  # Calculate absolute difference matrix
  diff_matrix <- abs(outer(all_values, all_values, FUN = "-"))

  # Create boolean matrix for proximity
  is_close_matrix <- diff_matrix < tolerance

  # Set all NA values in the boolean matrix to FALSE
  is_close_matrix[is.na(is_close_matrix)] <- FALSE

  # Remove diagonal
  diag(is_close_matrix) <- FALSE

  # Determine indices close to any other value
  close_to_any_vector <- rowSums(is_close_matrix) > 0

  # Transform resulting Boolean vector back into original data frame structure
  result_matrix <- matrix(
    close_to_any_vector,
    nrow = nrow(numeric_part),
    ncol = ncol(numeric_part),
    byrow = FALSE
  )

  # Convert matrix to data frame and restore names
  result_df <- as.data.frame(result_matrix)
  colnames(result_df) <- colnames(numeric_part)

  # Read protein/compound column
  result_df <- dplyr::mutate(
    result_df,
    !!colnames(tab)[1] := tab[, 1],
    .before = 1
  )

  return(result_df)
}

# Validate protein/compound table
#' @export
check_table <- function(tab, tolerance) {
  if (!nrow(tab) || ncol(tab) < 2) {
    return("Fill name and mass fields.")
  }

  # Check variable types
  tab_variables <- sapply(tab, class)
  if (tab_variables[1] != "character") {
    return(paste("Only text characters are allowed as name IDs"))
  }
  if (
    !all(tab_variables[-1] == "numeric") |
      !all(rowSums(!is.na(tab[, -1, drop = FALSE])) > 0)
  ) {
    return(paste("Mass fields require numeric values"))
  }

  # Check missing names
  if (any(is.na(tab[, 1]) | tab[, 1] == "")) {
    return(paste("Missing name ID values"))
  }

  # Check duplicated names
  if (any(duplicated(tab[, 1], incomparables = c("", NA)))) {
    return(paste("Duplicated names"))
  }

  # Check mass shift duplicates
  # TODO
  # duplicate_check <- check_mass_duplicates(tab = tab, tolerance = tolerance)
  #   if (
  #   sum(!is.na(tab[, -1])) > 1 &&
  #     any(rowSums(duplicate_check[, -1, drop = FALSE]) > 1)
  # ) {
  #   return("Mass shifts are duplicated in peak tolerance range")
  # }

  # If all checks passed return TRUE
  return(TRUE)
}


# # Parse filename according to nomenclature of test files
# parse_filename <- function(s) {
#   # Remove file extension if present
#   s <- sub("\\.[^\\.]+$", "", s)

#   # Split on + (corrected escaping for fixed=TRUE)
#   parts <- strsplit(s, "+", fixed = TRUE)[[1]]

#   if (length(parts) != 2) {
#     stop("String does not contain exactly one +")
#   }

#   before <- parts[1]

#   after <- parts[2]

#   # Now split after on _
#   after_parts <- strsplit(after, "_", fixed = TRUE)[[1]]

#   # Combine into a vector
#   result <- c(before, after_parts)

#   return(result)
# }

# Read in file containing the peaks picked from spectrum
get_peaks <- function(peak_file = NULL, result_sample, results) {
  if (
    is.null(peak_file) &
      is.character(result_sample) &
      is.list(results)
  ) {
    peaks <- results$deconvolution[[which(
      result_sample == names(results$deconvolution)
    )]]$peaks
  } else {
    # Check if path valid
    if (!file.exists(peak_file)) {
      warning("File does not exist.")
      return(NULL)
    }

    # Read peaks.dat file
    tryCatch(
      {
        peaks <- read.delim(peak_file, header = F, sep = " ")
      },
      error = function(e) {
        warning("Error reading peaks file: ", e$message)
        return(NULL)
      }
    )
  }

  # Check if data frame valid
  if (ncol(peaks) < 2) {
    warning(
      "Peaks file contains less than 2 fields. Expected: Mass, Intensity."
    )
    return(NULL)
  } else if (ncol(peaks) > 2) {
    warning(
      "Peaks file contains ",
      ncol(peaks),
      " fields. Expected are two fields: Mass, Intensity."
    )
    peaks <- peaks[, 1:2]
  }

  if (!all(sapply(peaks, class) == "numeric")) {
    warning(
      "Wrong data type(s) detected: ",
      paste(sapply(peaks, class), collapse = ", "),
      ". Only numeric is allowed."
    )
    return(NULL)
  }

  # Set names
  names(peaks) <- c("mass", "intensity")

  # Normalize peaks
  peaks$intensity <- peaks$intensity / max(peaks$intensity) * 100

  # Message information
  log_status(nrow(peaks), peaks$mass)

  return(peaks)
}

# Read file containing protein Mw
get_protein_mw <- function(mw_file) {
  # Check if path valid
  if (!file.exists(mw_file)) {
    warning("File does not exist.")
    return(NULL)
  }

  # Read Protein MW file
  protein_mw <- readLines(mw_file)

  if (length(protein_mw)) {
    message(
      "-> Protein Mw file contains ",
      length(protein_mw),
      " molecular weight value(s)"
    )
  } else {
    warning("Protein Mw file is empty")
    return(NULL)
  }

  # Return protein mw
  return(as.numeric(protein_mw))
}

# Read file containing compound mass and mass shifts
# Read in compound files in different formats (CSV, TSV, Excel)
# Specify header = TRUE if file contains header and header = FALSE if file has no header
get_compound_matrix <- function(compound_file, header = TRUE) {
  # Check if path valid
  if (!file.exists(compound_file)) {
    warning("File does not exist.")
    return(NULL)
  }

  # Skip header row
  skip <- ifelse(header, 1, 0)

  # Determine file extension
  file_ext <- tolower(tools::file_ext(compound_file))

  # Read file based on extension
  tryCatch(
    {
      if (file_ext == "csv") {
        compounds <- readr::read_csv(
          compound_file,
          col_names = FALSE,
          skip = skip,
          show_col_types = FALSE
        )
      } else if (file_ext == "tsv" || file_ext == "txt") {
        compounds <- readr::read_tsv(
          compound_file,
          col_names = FALSE,
          skip = skip,
          show_col_types = FALSE
        )
      } else if (file_ext %in% c("xls", "xlsx")) {
        compounds <- readxl::read_excel(
          compound_file,
          col_names = FALSE,
          skip = skip
        )
      } else {
        warning("Unsupported file format: ", file_ext)
        return(NULL)
      }
    },
    error = function(e) {
      warning("Error reading compounds file: ", e$message)
      return(NULL)
    }
  )

  # Check if data frame valid
  if (ncol(compounds) < 2) {
    warning(
      "Compounds file contains just one field. Expected at least two: Compound_Name, Compound_Mass."
    )
    return(NULL)
  } else if (ncol(compounds) > 10) {
    warning(
      "Compounds file contains ",
      ncol(compounds),
      " fields. Only the compound name and nine mass shifts are allowed."
    )
    return(NULL)
  }

  # Check if data types correct
  if (!is.character(compounds[[1]])) {
    warning(
      "First field (compound name) has the data type: ",
      class(compounds[[1]]),
      ". Allowed are only characters."
    )
    return(NULL)
  }

  if (!all(sapply(compounds[-1], function(x) is.numeric(x) || all(is.na(x))))) {
    warning(
      "Mass fields have the data type(s): ",
      paste(unique(sapply(compounds[-1], class)), collapse = ", "),
      ". Allowed are only numeric."
    )
    return(NULL)
  }

  # Fill mass shift names
  field_names <- c("compound")
  for (i in 1:(ncol(compounds) - 1)) {
    field_names[i + 1] <- paste0("mass_", letters[i])
  }
  names(compounds) <- field_names

  # Make matrix
  compounds_matrix <- as.matrix(compounds[, -1])
  row.names(compounds_matrix) <- compounds$compound

  # Inform compound list dimensions
  message(
    "-> ",
    nrow(compounds),
    " compounds with up to ",
    ncol(compounds) - 1,
    " mass shifts imported"
  )
  return(compounds_matrix)
}

check_hits <- function(
  sample_table,
  protein_mw,
  compound_mw,
  peaks,
  peak_tolerance,
  max_multiples,
  sample,
  well = NA
) {
  # Get protein name and mass
  prot_name <- as.character(protein_mw[, 1])
  prot_mass <- as.numeric(protein_mw[, 2])

  # Find protein peak
  protein_peak <- peaks$mass >= prot_mass - peak_tolerance &
    peaks$mass <= prot_mass + peak_tolerance

  prot_intensity <- ifelse(
    !any(protein_peak),
    0,
    peaks$intensity[which(protein_peak)]
  )

  # Keep only peaks above protein mw
  peaks_valid <- peaks$mass >= prot_mass - peak_tolerance

  if (any(peaks_valid)) {
    peaks_filtered <- as.data.frame(peaks[peaks_valid, ])
  } else {
    hits_df <- data.frame(
      well = well,
      sample = sample,
      protein = prot_name,
      theor_prot = prot_mass,
      measured_prot = NA,
      delta_prot = NA,
      prot_intensity = NA,
      peak = NA,
      intensity = NA,
      compound = NA,
      cmp_mass = NA,
      delta_cmp = NA,
      multiple = NA,
      preferred = NA,
      unmatched = NA,
      correct = NA
    )

    hits_df$unmatched <- unmatched <- sum(!peaks$mass %in% hits_df$peak) /
      nrow(peaks) *
      100
    hits_df$correct <- 100 - unmatched

    return(hits_df)
  }

  # Transform compounds to matrix
  cmp_mat <- as.matrix(compound_mw[, -1])
  rownames(cmp_mat) <- compound_mw[, 1]

  # Fill multiples matrix
  for (i in 1:max_multiples) {
    if (i == 1) {
      mat <- cmp_mat * i
      colnames(mat) <- paste0(colnames(cmp_mat), "*", i)
    } else {
      multiple <- cmp_mat * i
      colnames(multiple) <- paste0(colnames(multiple), "*", i)
      mat <- cbind(mat, multiple)
    }
  }

  # Addition of protein mw with multiples matrix
  complex_mat <- mat + prot_mass

  # Initiate empty hits data frame
  hits_df <- data.frame()

  # Fill hits_df
  for (j in 1:nrow(peaks_filtered)) {
    upper <- peaks_filtered$mass[j] + peak_tolerance
    lower <- peaks_filtered$mass[j] - peak_tolerance

    hits <- complex_mat >= lower & complex_mat <= upper

    if (any(hits, na.rm = TRUE)) {
      indices <- which(hits, arr.ind = TRUE)

      hits_add <- data.frame()

      for (k in 1:nrow(indices)) {
        # Retrieve compound mass from hit on complex
        multiple <- as.integer(sub(".*\\*", "", colnames(hits)[indices[k, 2]]))
        cmp_mass <- mat[
          indices[k, 1],
          indices[k, 2] - (ncol(hits) / max_multiples) * (multiple - 1)
        ]

        # Construct new entry for hits_df data frame
        hit <- data.frame(
          well = well,
          sample = sample,
          protein = prot_name,
          theor_prot = prot_mass,
          measured_prot = if (any(protein_peak)) {
            peaks$mass[which(protein_peak)]
          } else {
            NA
          },
          delta_prot = if (any(protein_peak)) {
            abs(
              prot_mass - peaks$mass[which(protein_peak)]
            )
          } else {
            NA
          },
          prot_intensity = prot_intensity,
          peak = peaks_filtered[j, "mass"],
          intensity = peaks_filtered[j, "intensity"],
          compound = rownames(hits)[indices[1]],
          cmp_mass = cmp_mass,
          delta_cmp = abs(
            (as.numeric(cmp_mass) * multiple) -
              (peaks_filtered[j, "mass"] - prot_mass)
          ),
          multiple = multiple,
          preferred = TRUE,
          unmatched = NA,
          correct = NA
        )

        hits_add <- rbind(hits_add, hit)
      }

      # Case multiple matching
      if (nrow(hits_add) > 1) {
        # Hit with highest compound mass is preferred to add to total binding
        hits_add <- hits_add |>
          dplyr::group_by(compound) |>
          dplyr::mutate(
            preferred = dplyr::row_number() == 1
          )

        # Log duplication event
        log_duplicated_hits(hits_add)
      }

      hits_df <- rbind(hits_df, hits_add)
    }
  }

  # If no hits detected in peaks
  if (nrow(hits_df) == 0) {
    hits_df <- data.frame(
      well = well,
      sample = sample,
      protein = prot_name,
      theor_prot = prot_mass,
      measured_prot = if (any(protein_peak)) {
        peaks$mass[which(protein_peak)]
      } else {
        NA
      },
      delta_prot = if (any(protein_peak)) {
        abs(
          prot_mass - peaks$mass[which(protein_peak)]
        )
      } else {
        NA
      },
      prot_intensity = if (any(protein_peak)) prot_intensity else NA,
      peak = if (any(protein_peak)) {
        peaks$mass[which(protein_peak)]
      } else {
        NA
      },
      intensity = NA,
      compound = NA,
      cmp_mass = NA,
      delta_cmp = NA,
      multiple = NA,
      preferred = NA,
      unmatched = NA,
      correct = NA
    )
  }

  # Calculate % unmatched and % correct
  # hits_df$unmatched <- unmatched <- sum(!peaks$mass %in% hits_df$peak) /
  #   nrow(peaks) *
  #   100
  hits_df$unmatched <- unmatched <- sum(
    !peaks$mass %in% c(hits_df$peak, hits_df$measured_prot)
  ) /
    nrow(peaks) *
    100
  hits_df$correct <- correct <- 100 - unmatched

  log_result(nrow(hits_df), unmatched, correct)
  return(hits_df)
}

###################################################
# intensitäten aufsummieren -> 100 %
# prot signal intenstität (einzeln) / gesamtintensität

# Compounds
# 1. Unterschiedliche massenshifts
# 2. multiple bindungen -> vielfache von compound MW (! jeweils pro massenshift)

# Protein MW = 1000
# Compound MW = 10|11

conversion <- function(hits) {
  # Check 'hits' argument validity
  if (!is.data.frame(hits) || nrow(hits) < 1) {
    log_err_no_df()
    return(NULL)
  } else if (ncol(hits) != 16) {
    log_err_cols(ncol(hits))
    return(NULL)
  } else if (nrow(hits) == 1 && is.na(hits$intensity)) {
    # Case only protein detected no hits
    I_total <- hits$prot_intensity # Total intensity
    hits <- dplyr::mutate(hits, `%binding` = 0)
    hits <- dplyr::mutate(
      hits,
      `%binding_tot` = 0,
      .before = peak
    )
  } else {
    # Total intensity (only preferred)
    I_total <- sum(hits$intensity[hits$preferred]) +
      ifelse(anyNA(hits$prot_intensity), 0, unique(hits$prot_intensity))

    # Protein only binding
    perc_bind_prot <- ifelse(
      anyNA(hits$prot_intensity),
      0,
      unique(hits$prot_intensity) / I_total
    )

    # Adding %Binding values to hit data frame
    hits <- dplyr::mutate(hits, `%binding` = intensity / I_total)
    hits <- dplyr::mutate(
      hits,
      `%binding_tot` = sum(unique(hits$`%binding`)),
      .before = peak
    )

    # Plausibility check
    total_relBinding <- hits$`%binding_tot`[1] + perc_bind_prot
    if (!isTRUE(all.equal(total_relBinding, 1))) {
      log_err_binding()
      return(NULL)
    }

    # Log computed relative binding values
    log_intensities(
      I_total,
      unique(hits$prot_intensity),
      sum(unique(hits$intensity))
    )

    # Normalize peak intensity
    # max_intensity <- max(c(hits$intensity, hits$prot_intensity))
    # hits$intensity <- hits$intensity / max_intensity * 100
    # hits$prot_intensity <- hits$prot_intensity / max_intensity * 100
  }

  # Change column names
  colnames(hits) <- c(
    "Well",
    "Sample",
    "Protein",
    "Mw Protein [Da]",
    "Measured Mw Protein [Da]",
    "Delta Mw Protein [Da]",
    "Protein Intensity",
    "Total % Binding",
    "Peak [Da]",
    "Intensity",
    "Compound",
    "Compound Mw [Da]",
    "Delta Mw Compound [Da]",
    "Binding Stoichiometry",
    "Preferred",
    "% Unmatched",
    "% Correct",
    "% Binding"
  )

  return(hits)
}

# Header: The Sample Name
log_start <- function(sample_name) {
  message(sprintf("Hit Screening: %s\n  │", sample_name))
}

# Status: Peak Info
log_status <- function(n_peaks, mass = NULL) {
  if (n_peaks > 0 && length(mass) > 0) {
    message(sprintf(
      "  ├─ Status: %s peaks detected [%.2f - %.2f Da]",
      n_peaks,
      min(mass),
      max(mass)
    ))
  } else {
    message(sprintf("  ├─ Status: %s peaks detected", n_peaks))
  }
}

log_duplicated_hits <- function(hits_add) {
  message(sprintf(
    "  ├─ %s Hit duplicates at %s Da",
    .col_warn(warning_sym),
    hits_add[1, "peak"]
  ))
  for (i in 1:nrow(hits_add)) {
    message(sprintf(
      "  │  └─ Compound %s - %s%s",
      hits_add[i, "compound"],
      paste0("[", hits_add[i, "cmp_mass"], "]x", hits_add[i, "multiple"]),
      paste0(" - Preferred: ", hits_add[i, "preferred"])
    ))
  }
}

.col_warn <- function(x) {
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    paste0('<span style="color: darkorange; font-weight: bold;">', x, "</span>")
  } else {
    paste0("\033[33m", x, "\033[39m")
  }
}
.col_err <- function(x) {
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    paste0('<span style="color: #e53935; font-weight: bold;">', x, "</span>")
  } else {
    paste0("\033[31m", x, "\033[39m")
  }
}

# Log conversion result per sample
log_result <- function(n_hits, unmatched, correct) {
  message(sprintf("  ├─ Result: %s hits detected", n_hits))
  message(sprintf("  │  ├─ Unmatched: %.2f%%", unmatched))
  message(sprintf("  │  ├─ Correct: %.2f%%", correct))
}

log_intensities <- function(total, unbound, binding) {
  # Calculate percentages
  perc_unbound <- (unbound / total) * 100
  perc_binding <- (binding / total) * 100

  message(paste0(
    sprintf("  │  ├─ Unbound Protein: %.2f%%\n", perc_unbound),
    sprintf("  │  └─ Total Binding:   %.2f%%", perc_binding)
  ))
}

# No Peaks
log_alert <- function(msg = "No protein peak detected") {
  message(sprintf("  ├─ %s %s.  ", .col_warn(warning_sym), msg))
}

# Footer: Closing a successful sample
log_done <- function() {
  message(paste0("  │\n", "  └─ ☑ Sample completed.\n  "))
}

# Alert: empty hits argument
log_err_no_df <- function() {
  msg <- sprintf(
    "  │  └─ %s ALERT: 'hits' argument must be a data frame with at least one row. Skipping.\n",
    .col_err(warning_sym)
  )
  message(msg)
}

# Alert: discrepancy in expected hits columns
log_err_cols <- function(current_cols) {
  msg <- sprintf(
    "  │  └─ %s ALERT: 'hits' data frame has %s columns, but 16 are required.\n",
    .col_err(warning_sym),
    current_cols
  )
  message(msg)
}

# Alert: 100% Tot. Binding [%] plausibility check
log_err_binding <- function() {
  msg <- sprintf(
    "  │  └─ %s ALERT: Total relative binding is not 100%%. Check data integrity.\n",
    .col_err(warning_sym)
  )
  message(msg)
}

# Log hits summary
log_hits_summary <- function(hits_summarized) {
  unmatched_vals <- as.numeric(hits_summarized[["% Unmatched"]])
  correct_vals <- as.numeric(hits_summarized[["% Correct"]])
  mean_unmatched <- mean(unmatched_vals, na.rm = TRUE)
  sd_unmatched <- stats::sd(unmatched_vals, na.rm = TRUE)
  mean_correct <- mean(correct_vals, na.rm = TRUE)
  sd_correct <- stats::sd(correct_vals, na.rm = TRUE)

  message(sprintf(
    "SUMMARIZING HITS\n  │\n  ├─ %s sample(s) screened\n  ├─ %s hit(s) detected in total\n  ├─ Unmatched: %.2f%% (SD: %.2f%%)\n  └─ Correct:   %.2f%% (SD: %.2f%%)\n",
    length(unique(hits_summarized$Sample)),
    sum(!is.na(hits_summarized$Compound)),
    mean_unmatched,
    sd_unmatched,
    mean_correct,
    sd_correct
  ))
}

# Plain-text number formatter for log messages: scientific notation for
# very large (|exp| >= 4) or very small (exp <= -3) values, otherwise signif.
fmt_log <- function(x, digits = 4) {
  if (is.na(x) || !is.finite(x)) {
    return(as.character(x))
  }
  abs_x <- abs(x)
  if (abs_x == 0) {
    return("0")
  }
  exp <- floor(log10(abs_x))
  if (exp >= 4 || exp <= -3) {
    formatC(x, format = "e", digits = 2)
  } else {
    as.character(signif(x, digits))
  }
}

# Log binding kinetics analysis initiation
#' @export
log_binding_kinetics <- function(concentrations, times, units) {
  message(paste(
    sprintf(
      "  ├─ %s concentrations present from %s to %s [%s]\n",
      length(unique(concentrations)),
      fmt_log(min(concentrations)),
      fmt_log(max(concentrations)),
      units[1]
    ),
    sprintf(
      " ├─ %s time points present from %s to %s [%s]\n",
      length(unique(times)),
      fmt_log(min(times)),
      fmt_log(max(times)),
      units[2]
    ),
    " ├─ Infer observed first-order rate constant k_obs\n  │  │"
  ))
}

# Log filtered samples
#' @export
log_filtered_samples <- function(diff) {
  if (diff > 0) {
    message(paste(
      sprintf(
        "  │  ├─ %s %s sample(s) ignored due to missing hits",
        .col_warn(warning_sym),
        diff
      )
    ))
  }
}

# Log filtered concentrations
#' @export
log_filtered_concentrations <- function(initial_tbl, filtered_tbl, conc_time) {
  conc_diff <- unique(initial_tbl[[conc_time[1]]]) %in%
    unique(filtered_tbl[[conc_time[1]]])

  not_present_conc <- unique(initial_tbl[[conc_time[1]]])[!conc_diff]

  if (length(not_present_conc)) {
    n <- length(not_present_conc)
    connectors <- c(rep("├─", max(n - 1, 0)), "└─")
    sub_lines <- paste0(
      sprintf("  │  │  %s %s\n", connectors, not_present_conc),
      collapse = ""
    )
    message(sprintf(
      "  │  ├─ %s Omitted concentrations after filtering\n%s  │  │",
      .col_warn(warning_sym),
      sub_lines
    ))
  }
}

# Log concentrations
log_concentration <- function(concentration, unit, last) {
  message(paste(
    sprintf(
      ifelse(
        last,
        "  │  └─ Computing k_obs for %s %s",
        "  │  ├─ Computing k_obs for %s %s"
      ),
      fmt_log(concentration),
      unit
    )
  ))
}

# Log nonlinear fit failure
log_fit_failed <- function(last, reason) {
  message(sprintf(
    ifelse(
      last,
      "  │     └─ %s Skipped: %s",
      "  │  │  └─ %s Skipped: %s"
    ),
    .col_warn(warning_sym),
    reason
  ))
}

# Pre-flight check: returns NULL if data is suitable for fitting, or a reason string if not
can_fit_kobs <- function(data) {
  real <- data[data$time > 0 & !is.na(data$binding), ]

  # No binding anywhere is a result rather than a fit failure: it is reported
  # before the time point rule below so such a concentration still reaches the
  # k_obs = 0 branch and stays visible in the result table and plot, however
  # few time points it was measured at.
  if (nrow(real) > 0 && all(real$binding == 0)) {
    return("no response detected (binding = 0% at all time points)")
  }

  # Count distinct time points, not rows: replicates of the same condition
  # share a time and add no information about the shape of the curve, so two
  # rows at one time point cannot identify the two-parameter model.
  n <- length(unique(real$time))
  if (n < 2) {
    tp_label <- if (n == 1) "1 time point" else "0 time points"
    return(sprintf("only %s available (minimum 2 required)", tp_label))
  }

  if (length(unique(real$binding)) < 2) {
    vals <- real$binding
    if (all(vals >= 100)) {
      return("immediate saturation (binding = 100% at all time points)")
    }
    return(sprintf(
      "flat response (binding = %s%% at all time points)",
      round(vals[1], 1)
    ))
  }
  NULL
}

# Log timepoints
log_timepoints <- function(data, unit, last) {
  # Distinct times, matching what can_fit_kobs() counts — replicates sharing a
  # time point must not be reported as separate time points.
  n <- length(unique(data$time))
  tmin <- min(data$time)
  tmax <- max(data$time)
  tp_label <- if (n == 1) "time point" else "time points"
  range_str <- if (tmin == tmax) {
    sprintf("%s %s", fmt_log(tmin), unit)
  } else {
    sprintf("%s - %s %s", fmt_log(tmin), fmt_log(tmax), unit)
  }
  message(paste(
    sprintf(
      ifelse(
        last,
        "  │     ├─ %s %s included (%s)",
        "  │  │  ├─ %s %s included (%s)"
      ),
      n,
      tp_label,
      range_str
    )
  ))
}

# Log kobs result
log_kobs_result <- function(result, last, unit) {
  p <- if (last) "  │     " else "  │  │  "
  message(sprintf("%s├─ Nonlinear regression model fitted", p))
  message(sprintf("%s├─ k_obs   = %s %s⁻¹", p, fmt_log(result$kobs), unit))
  message(sprintf("%s├─ v       = %s", p, fmt_log(result$v)))
  message(sprintf("%s└─ Plateau = %s%%", p, fmt_log(result$plateau)))
}

# Longest log row we keep on a single line. The Protocol log renders with
# white-space: pre, so anything past this would run off the side of the card
# instead of wrapping. Long text is broken here and the continuation rows are
# indented to stay lined up under the tree glyphs.
log_wrap_width <- 74

# Break `text` into rows: the first carries `head` (the branch glyph), the
# rest carry `cont`. `head_width` is given separately because a head may hold
# markup (the coloured warning symbol) that takes no visible width.
wrap_log_line <- function(
  head,
  cont,
  text,
  head_width = nchar(head, type = "width")
) {
  parts <- strwrap(text, width = max(28, log_wrap_width - head_width))
  if (length(parts) == 0) parts <- ""
  paste0(c(head, rep(cont, length(parts) - 1L)), parts)
}

# Log kinact/Ki warning. The headline sits on the branch, the explanation
# hangs off it one level deeper — the same shape the kobs steps use — so no
# single row grows past the width of the log card.
log_kinact_ki_warning <- function(msg, detail = NULL) {
  head <- sprintf("     ├─ %s ", .col_warn(warning_sym))
  lines <- wrap_log_line(head, "     │    ", msg, head_width = 10)
  if (!is.null(detail) && nzchar(detail)) {
    lines <- c(lines, wrap_log_line("     │  └─ ", "     │     ", detail))
  }
  message(paste(lines, collapse = "\n"))
}

# Log (Kᵢ/kᵢₙₐ꜀ₜ) analysis initiation
log_kinact_ki_analysis <- function() {
  message(paste("  │\n", " └─ Infer second-order rate constant Kᵢ/kᵢₙₐ꜀ₜ"))
}

# Log kinact/Ki results
log_kinact_ki_results <- function(results, units) {
  fit <- results$Fit
  model_label <- if (results$Model == "hyperbolic") {
    "hyperbolic"
  } else {
    "linear (no saturation)"
  }
  ratio <- results$Ratio
  ci <- if (all(is.na(ratio[c("CI 2.5%", "CI 97.5%")]))) {
    ""
  } else {
    sprintf(
      " [95%% CI %s – %s]",
      fmt_log(ratio[["CI 2.5%"]]),
      fmt_log(ratio[["CI 97.5%"]])
    )
  }

  lines <- c(
    sprintf(
      "     ├─ Global fit: %d samples, %d concentrations, %d plateau(s)",
      fit$n_points,
      fit$n_concentrations,
      max(fit$plateau_groups)
    ),
    sprintf(
      "     │  └─ own plateau where ≥ %d %% of it is reached",
      round(100 * kinetics_settings$plateau_min_reached)
    ),
    sprintf(
      "     ├─ Model: %s (curvature p = %s)",
      model_label,
      fmt_log(fit$p_curvature, 2)
    ),
    if (results$Status == "saturated") {
      c(
        sprintf(
          "     ├─ kᵢₙₐ꜀ₜ = %s ± %s %s⁻¹",
          fmt_log(results$Params[1, 1]),
          fmt_log(results$Params[1, 2]),
          units[["Time"]]
        ),
        sprintf(
          "     ├─ Kᵢ     = %s ± %s %s",
          fmt_log(results$Params[2, 1]),
          fmt_log(results$Params[2, 2]),
          units[["Concentration"]]
        )
      )
    } else {
      "     ├─ kᵢₙₐ꜀ₜ, Kᵢ = not determinable"
    },
    sprintf(
      "     └─ kᵢₙₐ꜀ₜ/Kᵢ = %s ± %s %s⁻¹ %s⁻¹%s",
      fmt_log(ratio[["Estimate"]]),
      fmt_log(ratio[["Std. Error"]]),
      units[["Concentration"]],
      units[["Time"]],
      ci
    )
  )

  message(paste(lines, collapse = "\n"))
}

# Add screened hits to result list
#' @export
add_hits <- function(
  results,
  sample_table,
  protein_table,
  compound_table,
  peak_tolerance,
  max_multiples,
  session,
  ns,
  kinact_ki = FALSE,
  config = NULL
) {
  samples <- names(results$deconvolution)
  protein_mw <- protein_table$`Mass 1`
  compound_mw <- as.matrix(compound_table[, -1])
  rownames(compound_mw) <- compound_table[, 1]

  hits_max <- if (kinact_ki) 80 else 100

  for (i in seq_along(samples)) {
    shinyWidgets::updateProgressBar(
      session = session,
      id = ns("conversion_progress"),
      value = ifelse(i == 1, 0, (i - 1) / length(samples) * hits_max),
      title = paste(
        "[",
        i,
        "/",
        length(samples),
        "] Checking hits for",
        samples[i]
      )
    )

    log_start(samples[i])

    st_key <- gsub("\\.raw$", "", sample_table$Sample, ignore.case = TRUE)
    s_key <- gsub("\\.raw$", "", samples[i], ignore.case = TRUE)
    present_protein <- sample_table$Protein[st_key == s_key]
    present_cmp <- sample_table[
      st_key == s_key,
      grep("Compound", names(sample_table))
    ]

    # Determine well position if config is present
    sample_well <- NA
    if (
      !is.null(config) &&
        "Well" %in% names(config) &&
        "Sample" %in% names(config)
    ) {
      cfg_key <- gsub("\\.raw$", "", config[["Sample"]], ignore.case = TRUE)
      idx <- match(s_key, cfg_key)
      if (!is.na(idx)) {
        raw_well <- as.character(config[["Well"]][idx])
        if (!is.na(raw_well) && nzchar(trimws(raw_well))) {
          sample_well <- sub("^.*:", "", raw_well)
        }
      }
    }

    results$deconvolution[[samples[i]]][["hits"]] <- check_hits(
      sample_table = sample_table,
      protein_mw = protein_table[protein_table$Protein == present_protein, ],
      compound_mw = compound_table[compound_table$Compound == present_cmp, ],
      peaks = get_peaks(result_sample = samples[i], results = results),
      peak_tolerance = peak_tolerance,
      max_multiples = max_multiples,
      sample = samples[i],
      well = sample_well
    )

    # Conversion of relative intensities to Binding [%]
    # Add resulting hits data frame to sample
    results$deconvolution[[samples[i]]][[
      "hits"
    ]] <- conversion(results$deconvolution[[samples[i]]][[
      "hits"
    ]])

    log_done()
  }

  shinyWidgets::updateProgressBar(
    session = session,
    id = ns("conversion_progress"),
    value = hits_max,
    title = paste0(
      "Hit screening completed for ",
      length(samples),
      " sample(s).",
      if (kinact_ki) " Computing binding kinetics..." else ""
    )
  )

  return(results)
}

# Concatenate and extract all hits data frames from all samples
#' @export
summarize_hits <- function(result_list, sample_table) {
  # Get samples from result list without session and output elements
  samples <- names(result_list$deconvolution)

  # Prepare empty hits data frame
  hits_summarized <- data.frame()

  for (i in samples) {
    hits_summarized <- rbind(
      hits_summarized,
      result_list$deconvolution[[i]]$hits
    )
  }

  conc_time <- names(sample_table)[unlist(sapply(
    c("Concentration", "Time"),
    grep,
    names(sample_table)
  ))]

  if (length(conc_time) == 2) {
    sample_table_join <- sample_table[, c("Sample", conc_time)]
    sample_table_join$Sample <- gsub(
      "\\.raw$",
      "",
      sample_table_join$Sample,
      ignore.case = TRUE
    )
    hits_summarized$Sample <- gsub(
      "\\.raw$",
      "",
      hits_summarized$Sample,
      ignore.case = TRUE
    )

    hits_summarized <- hits_summarized |>
      dplyr::left_join(sample_table_join, by = "Sample") |>
      dplyr::mutate(binding = `Total % Binding` * 100) |>
      dplyr::arrange(dplyr::across(all_of(conc_time)))
  }

  # Join Replicate from sample_table if available
  if ("Replicate" %in% names(sample_table)) {
    rep_join <- sample_table[, c("Sample", "Replicate"), drop = FALSE]
    rep_join$Sample <- gsub("\\.raw$", "", rep_join$Sample, ignore.case = TRUE)
    hs_key <- gsub("\\.raw$", "", hits_summarized$Sample, ignore.case = TRUE)
    hits_summarized$Replicate <- rep_join$Replicate[match(
      hs_key,
      rep_join$Sample
    )]
  }

  # Log hits summary
  log_hits_summary(hits_summarized)

  return(hits_summarized)
}

# Function to extract minutes information from sample names
extract_minutes <- function(strings) {
  # Find pattern: one or more digits followed by "min"
  minutes <- regmatches(strings, regexpr("\\d+(?=min)", strings, perl = TRUE))

  # Convert to numeric, replace empty matches with NA
  minutes <- ifelse(minutes == "", NA, minutes)
  as.numeric(minutes)
}

# Function perform checks for kinact/Ki analysis prerequisites
#' @export
check_filter_hits <- function(result_list) {
  # Check if hits summary is present and contains hits
  if (
    is.null(result_list$hits_summary) || nrow(result_list$hits_summary) == 0
  ) {
    message(sprintf(
      "  └─ %s No hits detected in any sample. Skipping binding kinetics analysis.",
      .col_err(warning_sym)
    ))
    return(NULL)
  }

  # Filter NA
  hits_summary <- result_list$hits_summary |>
    dplyr::filter(!is.na(binding))

  # Summarize filtered hits by concentration
  time_col <- grep("^Time", names(hits_summary), value = TRUE)
  time_col <- if (length(time_col) == 1) time_col else NA_character_

  tab <- hits_summary |>
    dplyr::group_by(dplyr::pick(dplyr::contains("Concentration"))) |>
    dplyr::summarise(
      count = dplyr::n(),
      # Distinct non-zero incubation times behind those hits. Counting hit
      # rows alone overstates the data available to the k_obs fit, because a
      # single sample contributes one row per matched peak — three "hits" can
      # all sit at the same time point.
      timepoints = if (is.na(time_col)) {
        NA_integer_
      } else {
        dplyr::n_distinct(.data[[time_col]][
          !is.na(.data[[time_col]]) & .data[[time_col]] > 0
        ])
      },
      .groups = "drop"
    )

  # Assign concentration column
  conc_col <- names(tab)[1]

  # Check if >= 3 non-zero concentrations are present
  nonzero_conc <- !is.na(tab[[conc_col]]) & tab[[conc_col]] != 0
  if (sum(nonzero_conc) < 3) {
    message(
      "  │  ├─ At least 3 different non-zero concentrations are required.\n",
      "  │  └─ Skipping binding kinetics analysis."
    )
    return(NULL)
  }

  # Check if concentrations have enough data points
  # Requirement: At least 3 non-zero concentrations must have >= 3 hits
  valid_concs <- sum(tab$count[nonzero_conc] >= 3)

  if (valid_concs < 3) {
    message(
      "  │  ├─ 3 hits per concentration are required.\n",
      "  │  ├─ Only ",
      valid_concs,
      " non-zero concentrations meet this threshold.\n",
      "  │  └─ Skipping binding kinetics analysis."
    )
    return(NULL)
  }

  # Check if concentrations cover enough distinct time points
  # Requirement: k_obs is fitted against time with two free parameters, so at
  # least 3 non-zero concentrations must carry >= 2 distinct non-zero times.
  # Without this, a single-time-point design passes the hit count check above
  # and then fails silently for every concentration inside compute_kobs().
  if (!is.na(time_col)) {
    valid_time_concs <- sum(tab$timepoints[nonzero_conc] >= 2, na.rm = TRUE)

    if (valid_time_concs < 3) {
      message(
        "  │  ├─ 2 distinct non-zero time points per concentration are required.\n",
        "  │  ├─ Only ",
        valid_time_concs,
        " non-zero concentrations meet this threshold.\n",
        "  │  └─ Skipping binding kinetics analysis."
      )
      return(NULL)
    }
  }

  return(hits_summary)
}

# Function to add binding/kobs results to result list
#' @export
add_kobs_binding_result <- function(
  hits_summary,
  concentrations_select = NULL,
  units,
  conc_time
) {
  # Optional concentration filter
  if (!is.null(concentrations_select)) {
    hits_summary <- dplyr::filter(
      hits_summary,
      !!rlang::sym(gsub(
        "Conc.",
        "Concentration",
        conc_time[["Concentration"]]
      )) %in%
        concentrations_select
    )
  }

  # Compute kobs
  binding_kobs_result <- compute_kobs(hits_summary, units = units)

  # Make kobs result table
  # Get measured concentrations
  concentrations <- which(
    !names(binding_kobs_result) %in%
      c("binding_table", "binding_points", "binding_plot", "skipped")
  )

  # Get concentration names
  conc_names <- names(binding_kobs_result[concentrations])

  # Fill kobs result table. Start from a typed zero-row frame so the four
  # columns exist even when no concentration could be fitted — assigning
  # colnames() to a 0x0 data.frame raised
  # "'names' attribute [4] must be the same length as the vector [0]".
  kobs_result_table <- data.frame(
    kobs = numeric(0),
    kobs_se = numeric(0),
    v = numeric(0),
    plateau = numeric(0)
  )
  for (i in conc_names) {
    kobs_result_table <- rbind(
      kobs_result_table,
      data.frame(
        kobs = binding_kobs_result[[i]]$kobs,
        kobs_se = binding_kobs_result[[i]]$kobs_se,
        v = binding_kobs_result[[i]]$v,
        plateau = binding_kobs_result[[i]]$plateau
      )
    )
  }
  rownames(kobs_result_table) <- conc_names

  # Add kobs result table
  binding_kobs_result$kobs_result_table <- kobs_result_table

  return(binding_kobs_result)
}

# Function to add kinact/Ki results to result list
#' @export
add_kinact_ki_result <- function(result_list, units) {
  # Log (Kᵢ/kᵢₙₐ꜀ₜ) analysis initiation
  log_kinact_ki_analysis()

  # Calculcate kinact/Ki from binding/kobs result
  kinact_ki_result <- compute_kinact_ki(
    result_list[["binding_kobs_result"]],
    units = units
  )

  # Log kinact/Ki results
  if (!is.null(kinact_ki_result)) {
    log_kinact_ki_results(results = kinact_ki_result, units = units)
  }

  return(kinact_ki_result)
}

# Tick formatting shared by the kinetics plot axes. Plotly's default renders
# magnitudes as SI prefixes ("5µ", "3.6k"), which reads as a unit on axes that
# already carry one. Powers of ten instead, matching how the result cards
# render values via format_scientific().
sci_axis_ticks <- list(
  exponentformat = "power",
  showexponent = "all"
)

# Marker symbol per concentration, highest concentration first. Built once
# from all analysed concentrations and handed to every plot that shows
# concentrations, so a concentration keeps its shape whatever subset a plot
# draws (a single concentration tab, excluded concentrations).
#' @export
concentration_symbol_map <- function(concentrations) {
  conc <- unique(as.character(concentrations))
  conc <- conc[order(as.numeric(conc), decreasing = TRUE)]
  stats::setNames(rep_len(symbols, length(conc)), conc)
}

# The given symbol map, extended by any concentration it does not know;
# without a map, symbols follow the plotted concentrations
resolve_symbol_map <- function(symbol_map, concentrations) {
  if (is.null(symbol_map)) {
    return(concentration_symbol_map(concentrations))
  }
  missing <- setdiff(unique(as.character(concentrations)), names(symbol_map))
  if (length(missing) > 0) {
    extra <- rep_len(symbols, length(symbol_map) + length(missing))
    symbol_map <- c(
      symbol_map,
      stats::setNames(extra[length(symbol_map) + seq_along(missing)], missing)
    )
  }
  symbol_map
}

# Function to generate and display binding plot
#' @export
make_binding_plot <- function(
  kobs_result,
  filter_conc = NULL,
  colors = NULL,
  units = NULL,
  theme = "dark",
  points = c("mean", "samples", "both"),
  symbol_map = NULL
) {
  # Which observations to draw: mean ± SD per time point, the individual
  # samples, or both (both clutters the full plot; single concentrations cope)
  points <- match.arg(points)
  show_means <- points != "samples"

  # Individual sample points (every sample enters the fit on its own)
  sample_points <- if (points != "mean") kobs_result$binding_points

  # Filter for specified concentration
  if (!is.null(filter_conc)) {
    kobs_result$binding_table <- dplyr::filter(
      kobs_result$binding_table,
      concentration == filter_conc
    )
    if (!is.null(sample_points)) {
      sample_points <- dplyr::filter(
        sample_points,
        concentration == filter_conc
      )
    }
  }

  # Mean per time point (rows of the curve table that carry observations)
  df_points <- kobs_result$binding_table[
    kobs_result$binding_table$time > 0 &
      !is.na(kobs_result$binding_table$binding),
  ]

  # Identify concentrations with no observed data (e.g. kobs=0 no-response)
  all_conc_chr <- unique(as.character(kobs_result$binding_table$concentration))
  obs_conc_chr <- unique(as.character(df_points$concentration))
  missing_conc <- setdiff(all_conc_chr, obs_conc_chr)

  # Sort df_points so legend order follows descending concentration
  all_conc_sorted <- sort(as.numeric(all_conc_chr), decreasing = TRUE)
  df_points$concentration <- factor(
    as.character(df_points$concentration),
    levels = as.character(all_conc_sorted)
  )
  df_points <- df_points[order(df_points$concentration), ]

  # Symbols fixed per concentration (see concentration_symbol_map())
  symbol_map <- resolve_symbol_map(symbol_map, all_conc_chr)

  font_color <- if (theme == "light") "black" else "white"
  grid_color <- if (theme == "light") {
    "rgba(0,0,0,0.1)"
  } else {
    "rgba(255,255,255,0.2)"
  }
  zeroline_color <- if (theme == "light") {
    "rgba(0,0,0,0.5)"
  } else {
    "rgba(255,255,255,0.5)"
  }

  has_replicates <- "binding_sd" %in%
    names(df_points) &&
    !all(is.na(df_points$binding_sd))

  df_points$sd_label <- ifelse(
    is.na(df_points$binding_sd),
    "n.a.",
    sprintf("%.2f", df_points$binding_sd)
  )

  time_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])

  # Generate plot
  binding_plot <- plotly::plot_ly() |>
    # Predicted/modeled binding
    plotly::add_lines(
      data = kobs_result$binding_table,
      x = ~time,
      y = ~predicted_binding,
      color = ~concentration,
      legendgroup = ~concentration,
      colors = colors,
      symbols = symbol_map,
      line = list(width = 2, opacity = 0.6),
      hovertemplate = paste(
        "<b>Predicted</b><br>",
        paste(
          "Time: %{x}",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
          "<br>"
        ),
        "Binding [%]: %{y:.2f}<br>",
        paste0(
          "k<sub>obs</sub>: %{customdata:.3~g} ",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
          "⁻¹"
        ),
        "<extra></extra>"
      ),
      customdata = ~kobs,
      showlegend = FALSE
    )

  if (show_means) binding_plot <- binding_plot |>
    # Mean ± SD per time point
    plotly::add_markers(
      data = dplyr::filter(df_points, !is.na(kobs)),
      x = ~time,
      y = ~binding,
      color = ~concentration,
      legendgroup = ~concentration,
      colors = colors,
      symbol = ~concentration,
      marker = list(
        size = 12,
        opacity = 0.8,
        line = list(width = 1, color = "white")
      ),
      legendgroup = ~concentration,
      error_y = if (has_replicates) {
        list(
          type = "data",
          array = ~binding_sd,
          visible = TRUE,
          thickness = 1.5,
          width = 4
        )
      } else {
        list(visible = FALSE)
      },
      text = ~ paste0("n = ", n, " · SD ", sd_label),
      hovertemplate = ~ paste(
        "<b>Mean ± SD</b> (%{text})<br>",
        paste(
          "Time: %{x}",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
          "<br>"
        ),
        "Binding [%]: %{y:.2f}<br>",
        paste0(
          "k<sub>obs</sub>: %{customdata:.3~g} ",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
          "⁻¹"
        ),
        "<extra></extra>"
      ),
      customdata = ~kobs,
      showlegend = ifelse(is.null(filter_conc), TRUE, FALSE)
    )

  # Individual samples: these are the points the curves were fitted to.
  if (!is.null(sample_points) && nrow(sample_points) > 0) {
    sample_points$concentration <- factor(
      as.character(sample_points$concentration),
      levels = as.character(all_conc_sorted)
    )
    sample_points <- sample_points[!is.na(sample_points$concentration), ]
    sample_points <- sample_points[order(sample_points$concentration), ]
    ring_colors <- unname(colors[as.character(sample_points$concentration)])
    ring_colors[is.na(ring_colors)] <- font_color
    sample_hover <- paste0(
      "<b>Sample</b><br>%{text}<br>",
      "Time: %{x} ",
      time_unit,
      "<br>",
      "Binding [%]: %{y:.2f}",
      "<extra></extra>"
    )
  }

  if (!is.null(sample_points) && nrow(sample_points) > 0 && !show_means) {
    # Samples alone: filled in the concentration colour with an outline in
    # the theme's text colour, carrying the legend in place of the means
    binding_plot <- binding_plot |>
      plotly::add_markers(
        data = sample_points,
        x = ~time,
        y = ~binding,
        color = ~concentration,
        colors = colors,
        symbol = ~concentration,
        symbols = symbol_map,
        legendgroup = ~concentration,
        marker = list(
          size = 10,
          opacity = 1,
          line = list(width = 1.5, color = font_color)
        ),
        text = ~Sample,
        hovertemplate = sample_hover,
        showlegend = is.null(filter_conc)
      )
  } else if (!is.null(sample_points) && nrow(sample_points) > 0) {
    # Samples on top of the means: solid in the text colour of the theme
    # (white on dark, black on light) with a ring in the concentration colour,
    # so they read against the coloured means in both export themes.
    binding_plot <- binding_plot |>
      plotly::add_markers(
        data = sample_points,
        x = ~time,
        y = ~binding,
        legendgroup = ~concentration,
        marker = list(
          size = 7,
          opacity = 1,
          color = font_color,
          symbol = unname(symbol_map[as.character(sample_points$concentration)]),
          line = list(width = 1.5, color = ring_colors)
        ),
        text = ~Sample,
        hovertemplate = paste0(
          "<b>Sample</b><br>%{text}<br>",
          "Time: %{x} ",
          time_unit,
          "<br>",
          "Binding [%]: %{y:.2f}",
          "<extra></extra>"
        ),
        showlegend = FALSE,
        inherit = FALSE
      )
  }

  # Concentrations that were measured but could not be fitted: their means are
  # drawn in a neutral open marker with no curve behind them, so real data
  # never disappears from the plot without trace. The hover carries the reason.
  unfitted <- if ("skip_reason" %in% names(df_points)) {
    df_points[!is.na(df_points$skip_reason), ]
  } else {
    df_points[0, ]
  }
  if (nrow(unfitted) > 0) {
    binding_plot <- binding_plot |>
      plotly::add_markers(
        data = unfitted,
        x = ~time,
        y = ~binding,
        name = ~ paste0(concentration, " (not fitted)"),
        legendgroup = ~concentration,
        marker = list(
          size = 11,
          color = "rgba(0,0,0,0)",
          symbol = unname(symbol_map[as.character(unfitted$concentration)]),
          line = list(width = 2, color = font_color)
        ),
        text = ~skip_reason,
        hovertemplate = paste0(
          "<b>Measured, not fitted</b><br>",
          "Time: %{x} ",
          time_unit,
          "<br>",
          "Binding [%]: %{y:.2f}<br>",
          "%{text}",
          "<extra></extra>"
        ),
        # In Mean ± SD mode this trace is the concentration's only legend
        # entry, because the coloured mean markers exclude it. In Samples mode
        # the sample trace already carries one, so a second would duplicate it.
        showlegend = is.null(filter_conc) && show_means,
        inherit = FALSE
      )
  }

  # Add explicit legend-only entries for no-response concentrations (no observed points)
  if (is.null(filter_conc) && length(missing_conc) > 0) {
    for (conc_name in as.character(sort(
      as.numeric(missing_conc),
      decreasing = TRUE
    ))) {
      binding_plot <- binding_plot |>
        plotly::add_markers(
          x = 0,
          y = 0,
          name = conc_name,
          legendgroup = conc_name,
          marker = list(
            size = 12,
            color = unname(colors[conc_name]),
            symbol = unname(symbol_map[conc_name]),
            opacity = 0,
            line = list(width = 0)
          ),
          showlegend = TRUE,
          hoverinfo = "skip",
          inherit = FALSE
        )
    }
  }

  binding_plot <- binding_plot |>
    plotly::layout(
      hovermode = "closest",
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(size = 14, color = font_color),
      legend = list(
        title = list(
          text = paste0(
            "Concentration [",
            gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
            "]  "
          ),
          font = list(color = font_color)
        ),
        bgcolor = "rgba(0,0,0,0)",
        bordercolor = "rgba(0,0,0,0)",
        font = list(color = font_color)
      ),
      xaxis = c(
        list(
          title = paste0(
            "Time [",
            gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
            "]"
          ),
          color = font_color,
          showgrid = TRUE,
          gridcolor = grid_color,
          zerolinecolor = zeroline_color
        ),
        sci_axis_ticks
      ),
      yaxis = list(
        title = "Binding [%]",
        color = font_color,
        showgrid = TRUE,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color
      )
    )

  # Return plot
  return(binding_plot)
}

# Function to generate and display kobs plot
#' @export
make_kobs_plot <- function(
  kinact_ki_result,
  colors,
  units,
  theme = "dark",
  show_extrapolation = FALSE,
  symbol_map = NULL
) {
  # Get predicted/modeled kobs
  df <- kinact_ki_result$Kobs_Data[
    !is.na(kinact_ki_result$Kobs_Data$predicted_kobs),
  ]

  # Get observed kobs data points (include kobs=0 for no-response; exclude dummy anchor at conc=0)
  df_points <- kinact_ki_result$Kobs_Data[
    !is.na(kinact_ki_result$Kobs_Data$kobs) &
      kinact_ki_result$Kobs_Data$conc > 0,
  ]
  ordered_levels <- sort(
    unique(as.numeric(as.character(df_points$conc))),
    decreasing = TRUE
  )
  df_points$conc <- factor(
    as.character(df_points$conc),
    levels = as.character(ordered_levels)
  )
  # Sort so legend order matches factor level order (first appearance drives plotly legend)
  df_points <- df_points[order(df_points$conc), ]
  df_points$kobs_se_label <- ifelse(
    is.na(df_points$kobs_se),
    "N/A",
    formatC(df_points$kobs_se, digits = 3, format = "g")
  )

  # Set symbols to corresponding concentration (descending, matching binding curve)
  ordered_conc <- as.character(ordered_levels)

  symbol_map <- resolve_symbol_map(symbol_map, ordered_conc)

  font_color <- if (theme == "light") "black" else "white"
  grid_color <- if (theme == "light") {
    "rgba(0,0,0,0.1)"
  } else {
    "rgba(255,255,255,0.2)"
  }
  zeroline_color <- if (theme == "light") {
    "rgba(0,0,0,0.5)"
  } else {
    "rgba(255,255,255,0.5)"
  }

  # Generate plot
  kobs_plot <- plotly::plot_ly() |>
    # kobs curve of the global fit (linear or hyperbolic model)
    plotly::add_lines(
      data = df,
      x = ~conc,
      y = ~predicted_kobs,
      colors = colors,
      symbols = symbol_map,
      line = list(width = 1.5, color = font_color),
      hovertemplate = paste(
        paste0("<b>Global fit (", kinact_ki_result$Model, ")</b><br>"),
        paste0(
          "Concentration: %{x} ",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
          "<br>"
        ),
        paste0(
          "k<sub>obs</sub>: %{y:.3~g} ",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
          "⁻¹"
        ),
        "<extra></extra>"
      ),
      showlegend = FALSE
    )

  conc_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]])
  time_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])

  for (conc_name in ordered_conc) {
    sub <- df_points[as.character(df_points$conc) == conc_name, , drop = FALSE]
    kobs_plot <- plotly::add_markers(
      kobs_plot,
      data = sub,
      x = ~ as.numeric(as.character(conc)),
      y = ~kobs,
      name = conc_name,
      legendgroup = conc_name,
      marker = list(
        size = 12,
        opacity = 1,
        color = unname(colors[conc_name]),
        symbol = unname(symbol_map[conc_name]),
        line = list(width = 1, color = font_color)
      ),
      customdata = ~kobs_se_label,
      error_y = list(
        type = "data",
        array = ~kobs_se,
        visible = TRUE,
        thickness = 1.5,
        width = 4,
        color = font_color
      ),
      hovertemplate = paste0(
        "<b>Calculated</b><br>",
        "Concentration: ",
        conc_name,
        " ",
        conc_unit,
        "<br>",
        "k<sub>obs</sub>: %{y:.3~g} ± %{customdata} ",
        time_unit,
        "⁻¹",
        "<extra></extra>"
      ),
      showlegend = TRUE,
      inherit = FALSE
    )
  }

  # Optional view past the measured range: the fitted curve continued as a
  # dashed line, the alternative model as a dotted one, and the measured range
  # shaded. Inside the shade the two models are indistinguishable when the
  # data do not saturate; outside it they part — which is the reason kinact
  # and KI are then not reported.
  shapes <- list()
  annotations <- list()
  if (isTRUE(show_extrapolation) && nrow(df) > 0) {
    max_conc <- max(df$conc, na.rm = TRUE)
    fit <- kinact_ki_result$Fit
    ratio <- kinact_ki_result$Ratio[["Estimate"]]
    ki <- fit$KI_hyperbolic
    extrap_max <- 5 * max_conc
    if (kinact_ki_result$Model == "hyperbolic" && is.finite(ki)) {
      extrap_max <- max(extrap_max, 3 * ki)
    }
    hyperbolic_ok <- is.finite(ki) &&
      !isTRUE(fit$KI_hyperbolic_at_bound) &&
      is.finite(fit$ratio_hyperbolic)

    kobs_at <- function(conc, model) {
      if (model == "hyperbolic") {
        r <- if (kinact_ki_result$Model == "hyperbolic") {
          ratio
        } else {
          fit$ratio_hyperbolic
        }
        r * conc / (1 + conc / ki)
      } else {
        r <- if (kinact_ki_result$Model == "linear") ratio else fit$ratio_linear
        r * conc
      }
    }
    unit_suffix <- paste0(" ", time_unit, "⁻¹")
    line_hover <- function(label) {
      paste0(
        "<b>",
        label,
        "</b><br>Concentration: %{x:.3~g} ",
        conc_unit,
        "<br>k<sub>obs</sub>: %{y:.3~g}",
        unit_suffix,
        "<extra></extra>"
      )
    }

    grid_out <- seq(max_conc, extrap_max, length.out = 200)
    kobs_plot <- kobs_plot |>
      plotly::add_lines(
        x = grid_out,
        y = kobs_at(grid_out, kinact_ki_result$Model),
        line = list(width = 1.5, color = font_color, dash = "dash"),
        hovertemplate = line_hover(paste0(
          "Global fit (",
          kinact_ki_result$Model,
          "), extrapolated"
        )),
        showlegend = FALSE,
        inherit = FALSE
      )

    other <- if (kinact_ki_result$Model == "linear") "hyperbolic" else "linear"
    other_ok <- if (other == "hyperbolic") {
      hyperbolic_ok
    } else {
      is.finite(fit$ratio_linear)
    }
    if (other_ok) {
      grid_all <- seq(0, extrap_max, length.out = 300)
      kobs_plot <- kobs_plot |>
        plotly::add_lines(
          x = grid_all,
          y = kobs_at(grid_all, other),
          line = list(width = 1.2, color = zeroline_color, dash = "dot"),
          hovertemplate = line_hover(paste0(
            "Alternative model (",
            other,
            "), not selected"
          )),
          showlegend = FALSE,
          inherit = FALSE
        )
    }

    shapes <- list(list(
      type = "rect",
      xref = "x",
      yref = "paper",
      x0 = 0,
      x1 = max_conc,
      y0 = 0,
      y1 = 1,
      fillcolor = if (theme == "light") {
        "rgba(0,0,0,0.07)"
      } else {
        "rgba(255,255,255,0.09)"
      },
      line = list(width = 0),
      layer = "below"
    ))
    annotations <- list(list(
      x = max_conc,
      y = 1,
      xref = "x",
      yref = "paper",
      text = "measured",
      showarrow = FALSE,
      xanchor = "right",
      yanchor = "top",
      font = list(size = 12, color = font_color)
    ))
  }

  kobs_plot <- kobs_plot |>
    plotly::layout(
      shapes = shapes,
      annotations = annotations,
      hovermode = "closest",
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(size = 14, color = font_color),
      legend = list(
        title = list(
          text = paste0(
            "Concentration [",
            gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
            "]  "
          ),
          font = list(color = font_color)
        ),
        bgcolor = "rgba(0,0,0,0)",
        bordercolor = "rgba(0,0,0,0)",
        font = list(color = font_color)
      ),
      xaxis = c(
        list(
          title = paste0(
            "Compound [",
            gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
            "]"
          ),
          color = font_color,
          showgrid = TRUE,
          gridcolor = grid_color,
          zerolinecolor = zeroline_color
        ),
        sci_axis_ticks
      ),
      yaxis = c(
        list(
          title = paste0("k<sub>obs</sub> [", time_unit, "⁻¹]"),
          color = font_color,
          showgrid = TRUE,
          gridcolor = grid_color,
          zerolinecolor = zeroline_color
        ),
        sci_axis_ticks
      )
    )

  # Return plot
  return(kobs_plot)
}

## Kinetics diagnostics plots ----
#
# Four views on how well the data support the global fit. All of them take
# the theme so exports stay readable on light and dark backgrounds.

kinetics_plot_colors <- function(theme) {
  light <- theme == "light"
  list(
    font = if (light) "black" else "white",
    grid = if (light) "rgba(0,0,0,0.1)" else "rgba(255,255,255,0.2)",
    zero = if (light) "rgba(0,0,0,0.5)" else "rgba(255,255,255,0.5)",
    shade = if (light) "rgba(0,0,0,0.07)" else "rgba(255,255,255,0.09)",
    muted = if (light) "rgba(0,0,0,0.45)" else "rgba(255,255,255,0.55)"
  )
}

kinetics_axis <- function(title, pal, extra = list()) {
  c(
    list(
      title = title,
      color = pal$font,
      showgrid = TRUE,
      gridcolor = pal$grid,
      zerolinecolor = pal$zero
    ),
    extra
  )
}

kinetics_layout <- function(p, pal, units, xaxis, yaxis, legend = TRUE, ...) {
  plotly::layout(
    p,
    hovermode = "closest",
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    font = list(size = 14, color = pal$font),
    showlegend = legend,
    legend = list(
      title = list(
        text = paste0(
          "Concentration [",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
          "]  "
        ),
        font = list(color = pal$font)
      ),
      bgcolor = "rgba(0,0,0,0)",
      bordercolor = "rgba(0,0,0,0)",
      font = list(color = pal$font)
    ),
    xaxis = xaxis,
    yaxis = yaxis,
    ...
  )
}

# Horizontal legend above the plot area, so it never runs into the x axis title
kinetics_top_legend <- function(p, pal) {
  plotly::layout(
    p,
    legend = list(
      title = list(text = ""),
      orientation = "h",
      x = 0,
      xanchor = "left",
      y = 1.02,
      yanchor = "bottom",
      font = list(color = pal$font)
    )
  )
}

# Empty plot carrying a message (e.g. when a diagnostic does not apply)
kinetics_message_plot <- function(message, theme = "dark") {
  pal <- kinetics_plot_colors(theme)
  plotly::plot_ly() |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(
        text = message,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        x = 0.5,
        y = 0.5,
        font = list(size = 14, color = pal$font)
      ))
    )
}

# Residuals of the global fit against time, per concentration. Scatter around
# zero without a trend means the model describes the time courses; a run of
# same-signed residuals (e.g. early points all below zero) points at
# something the model does not capture.
#' @export
make_kinetics_residual_plot <- function(
  kinact_ki_result,
  colors,
  units,
  theme = "dark",
  symbol_map = NULL
) {
  pts <- kinact_ki_result$Points
  if (is.null(pts) || nrow(pts) == 0) {
    return(kinetics_message_plot("No global fit available", theme))
  }
  pal <- kinetics_plot_colors(theme)
  time_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])
  conc_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]])

  levels_desc <- as.character(sort(unique(pts$conc), decreasing = TRUE))
  pts$concentration <- factor(as.character(pts$conc), levels = levels_desc)
  pts$series[is.na(pts$series) | pts$series == ""] <- "–"
  pts <- pts[order(pts$concentration, pts$series, pts$time), ]
  symbol_map <- resolve_symbol_map(symbol_map, levels_desc)
  spread <- 2 * stats::sd(pts$residual)
  time_max <- max(pts$time)

  # Replicate series share the concentration's colour and shape and differ in
  # fill: filled, open, dotted, open with dot. The thin line joins the points
  # of one series over time, so a replicate that drifts away shows as a run.
  series_levels <- sort(unique(pts$series))
  variants <- c("", "-open", "-dot", "-open-dot")
  series_variant <- stats::setNames(
    variants[(seq_along(series_levels) - 1) %% length(variants) + 1],
    series_levels
  )
  multi_series <- length(series_levels) > 1

  pts$hover <- paste0(
    "<b>", pts$sample, "</b>",
    if (multi_series) paste0("<br>Series: ", pts$series) else "",
    "<br>Concentration: ", pts$concentration, " ", conc_unit,
    "<br>Time: ", signif(pts$time, 4), " ", time_unit,
    "<br>Observed: ", sprintf("%.2f", pts$binding), " %",
    "<br>Fitted: ", sprintf("%.2f", pts$fitted), " %",
    "<br>Residual: ", sprintf("%+.2f", pts$residual), " %"
  )

  p <- plotly::plot_ly()
  for (conc_name in levels_desc) {
    conc_color <- unname(colors[conc_name])
    if (is.na(conc_color)) conc_color <- pal$muted
    first <- TRUE
    for (s in series_levels) {
      sub <- pts[
        as.character(pts$concentration) == conc_name & pts$series == s,
        ,
        drop = FALSE
      ]
      if (nrow(sub) == 0) next
      variant <- series_variant[[s]]
      open <- grepl("open", variant)

      if (multi_series && nrow(sub) > 1) {
        p <- plotly::add_lines(
          p,
          x = sub$time,
          y = sub$residual,
          line = list(color = conc_color, width = 1),
          opacity = 0.45,
          legendgroup = conc_name,
          showlegend = FALSE,
          hoverinfo = "skip",
          inherit = FALSE
        )
      }

      p <- plotly::add_markers(
        p,
        x = sub$time,
        y = sub$residual,
        name = conc_name,
        legendgroup = conc_name,
        showlegend = first,
        marker = list(
          size = 10,
          color = conc_color,
          symbol = paste0(symbol_map[[conc_name]], variant),
          line = if (open) {
            list(width = 2)
          } else {
            list(width = 1, color = pal$font)
          }
        ),
        text = sub$hover,
        hovertemplate = "%{text}<extra></extra>",
        inherit = FALSE
      )
      first <- FALSE
    }
  }

  # Legend entries explaining the fill of each series (shape-neutral circles
  # in the text colour)
  if (multi_series) {
    for (i in seq_along(series_levels)) {
      s <- series_levels[i]
      variant <- series_variant[[s]]
      p <- plotly::add_markers(
        p,
        x = numeric(0),
        y = numeric(0),
        name = s,
        legendgroup = "series",
        legendgrouptitle = if (i == 1) {
          list(text = "Replicate series", font = list(color = pal$font))
        },
        marker = list(
          size = 10,
          color = pal$font,
          symbol = paste0("circle", variant),
          line = if (grepl("open", variant)) list(width = 2) else list(width = 0)
        ),
        hoverinfo = "skip",
        inherit = FALSE
      )
    }
  }

  guide <- function(y, dash) {
    list(
      type = "line",
      xref = "paper",
      x0 = 0,
      x1 = 1,
      y0 = y,
      y1 = y,
      line = list(color = pal$zero, width = 1, dash = dash)
    )
  }

  kinetics_layout(
    p,
    pal,
    units,
    xaxis = kinetics_axis(
      paste0("Time [", time_unit, "]"),
      pal,
      c(list(range = c(-0.03 * time_max, 1.03 * time_max)), sci_axis_ticks)
    ),
    yaxis = kinetics_axis("Residual [% binding]", pal),
    shapes = list(guide(0, "solid"), guide(spread, "dot"), guide(-spread, "dot")),
    annotations = list(list(
      x = 1,
      y = spread,
      xref = "paper",
      yref = "y",
      text = "±2 SD",
      showarrow = FALSE,
      xanchor = "right",
      yanchor = "bottom",
      font = list(size = 11, color = pal$muted)
    ))
  ) |>
    plotly::layout(legend = list(tracegroupgap = 4))
}

# Plateau per concentration, all in % binding. For each concentration the
# binding observed at its last time point (the concentration's symbol) is
# joined by a dotted stem to the plateau of its single-concentration fit (the
# tick, as in the Binding Analysis table): a long stem means the curve was
# still rising when measurement stopped, so that plateau is extrapolated. The
# bar is the plateau the global fit used; one bar across several
# concentrations means they share it because none of them got close enough to
# define its own.
#' @export
make_kinetics_plateau_plot <- function(
  kinact_ki_result,
  colors,
  units,
  theme = "dark",
  symbol_map = NULL
) {
  tab <- kinact_ki_result$Plateaus
  if (is.null(tab) || nrow(tab) == 0) {
    return(kinetics_message_plot("No global fit available", theme))
  }
  pal <- kinetics_plot_colors(theme)
  conc_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]])
  time_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])

  tab <- tab[order(tab$conc), ]
  tab$label <- as.character(tab$conc)
  tab$x <- seq_len(nrow(tab))
  symbol_map <- resolve_symbol_map(symbol_map, tab$label)

  # Mean observed binding at each concentration's last time point
  pts <- kinact_ki_result$Points
  last_obs <- vapply(
    seq_len(nrow(tab)),
    function(i) {
      sub <- pts[pts$conc == tab$conc[i], , drop = FALSE]
      if (nrow(sub) == 0) return(c(NA_real_, NA_real_))
      t_last <- max(sub$time)
      c(mean(sub$binding[sub$time == t_last]), t_last)
    },
    numeric(2)
  )
  tab$last_binding <- last_obs[1, ]
  tab$last_time <- last_obs[2, ]

  point_colors <- unname(colors[tab$label])
  point_colors[is.na(point_colors)] <- pal$muted

  # NA-separated segments drawn as one trace each
  segments <- function(x0, x1, y0, y1) {
    list(
      x = as.vector(rbind(x0, x1, NA)),
      y = as.vector(rbind(y0, y1, NA))
    )
  }

  # Stems from the observed value to the single-fit plateau
  stems <- segments(tab$x, tab$x, tab$last_binding, tab$plateau_single)

  # Global plateau: one bar per plateau group across its concentrations
  groups <- split(tab, tab$group)
  group_tab <- data.frame(
    x0 = vapply(groups, function(g) min(g$x) - 0.38, numeric(1)),
    x1 = vapply(groups, function(g) max(g$x) + 0.38, numeric(1)),
    plateau = vapply(groups, function(g) g$plateau[1], numeric(1)),
    text = vapply(
      groups,
      function(g) {
        if (nrow(g) == 1) {
          "Own plateau"
        } else {
          paste0(
            "Shared by ",
            paste(g$label, collapse = ", "),
            " ",
            conc_unit
          )
        }
      },
      character(1)
    )
  )
  bars <- segments(
    group_tab$x0,
    group_tab$x1,
    group_tab$plateau,
    group_tab$plateau
  )

  hover_head <- paste0("<b>", tab$label, " ", conc_unit, "</b><br>")

  p <- plotly::plot_ly() |>
    plotly::add_lines(
      x = bars$x,
      y = bars$y,
      line = list(color = pal$muted, width = 5),
      text = as.vector(rbind(group_tab$text, group_tab$text, NA)),
      hovertemplate = "<b>Global fit plateau</b>: %{y:.2f} %<br>%{text}<extra></extra>",
      name = "Plateau, global fit",
      legendrank = 3,
      inherit = FALSE
    ) |>
    plotly::add_lines(
      x = stems$x,
      y = stems$y,
      line = list(color = pal$font, width = 1.5, dash = "dot"),
      hoverinfo = "skip",
      name = "Rise still to come",
      legendrank = 4,
      inherit = FALSE
    ) |>
    plotly::add_markers(
      x = tab$x,
      y = tab$plateau_single,
      marker = list(
        symbol = "line-ew-open",
        size = 24,
        color = pal$font,
        line = list(width = 3, color = pal$font)
      ),
      text = paste0(
        hover_head,
        "Single-concentration fit plateau: ",
        sprintf("%.2f", tab$plateau_single),
        " %"
      ),
      hovertemplate = "%{text}<extra></extra>",
      name = "Plateau, single-concentration fit",
      legendrank = 2,
      inherit = FALSE
    ) |>
    plotly::add_markers(
      x = tab$x,
      y = tab$last_binding,
      marker = list(
        symbol = unname(symbol_map[tab$label]),
        size = 13,
        color = point_colors,
        opacity = 1,
        line = list(width = 1, color = pal$font)
      ),
      text = paste0(
        hover_head,
        "Observed at ",
        signif(tab$last_time, 4),
        " ",
        time_unit,
        ": ",
        sprintf("%.2f", tab$last_binding),
        " %<br>Model: curve reached ",
        sprintf("%.2f", 100 * tab$reached),
        " % of its plateau"
      ),
      hovertemplate = "%{text}<extra></extra>",
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    # Legend entry for the observed values in neutral colour; the points
    # themselves carry the concentration's colour and symbol
    plotly::add_markers(
      x = numeric(0),
      y = numeric(0),
      marker = list(
        symbol = "circle",
        size = 11,
        color = pal$muted,
        line = list(width = 1, color = pal$font)
      ),
      name = "Observed at last time point",
      legendrank = 1,
      inherit = FALSE
    )

  kinetics_layout(
    p,
    pal,
    units,
    xaxis = kinetics_axis(
      paste0("Compound [", conc_unit, "]"),
      pal,
      list(
        tickmode = "array",
        tickvals = tab$x,
        ticktext = tab$label,
        range = c(0.4, nrow(tab) + 0.6),
        showgrid = FALSE,
        zeroline = FALSE
      )
    ),
    yaxis = kinetics_axis("Binding [%]", pal, list(range = c(0, 105)))
  ) |>
    kinetics_top_legend(pal)
}

# kinact/KI of each replicate series fitted on its own next to the result of
# all samples. Series that agree within their error bars mean a repeat of the
# experiment gives the same answer.
#' @export
make_kinetics_series_plot <- function(
  kinact_ki_result,
  units,
  theme = "dark"
) {
  series <- kinact_ki_result$Series
  if (is.null(series) || nrow(series) < 2) {
    return(kinetics_message_plot(
      "Fewer than two replicate series with ≥ 3 concentrations",
      theme
    ))
  }
  pal <- kinetics_plot_colors(theme)
  conc_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]])
  time_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])
  ratio_unit <- paste0(time_unit, "⁻¹ ", conc_unit, "⁻¹")
  ratio <- kinact_ki_result$Ratio

  all_label <- "All samples"
  lvls <- c(rev(series$series), all_label)
  has_ci <- !any(is.na(ratio[c("CI 2.5%", "CI 97.5%")]))

  p <- plotly::plot_ly() |>
    plotly::add_markers(
      x = series$ratio,
      y = factor(series$series, levels = lvls),
      name = "Series fit ± SE",
      marker = list(
        size = 12,
        color = pal$font,
        line = list(width = 1, color = pal$font)
      ),
      error_x = list(
        type = "data",
        array = series$ratio_se,
        color = pal$font,
        thickness = 1.5,
        width = 6
      ),
      text = paste0("n = ", series$n, " samples"),
      hovertemplate = paste0(
        "<b>%{y}</b><br>k<sub>inact</sub>/K<sub>i</sub>: %{x:.4~g} ",
        ratio_unit,
        "<br>%{text}<extra></extra>"
      ),
      inherit = FALSE
    ) |>
    plotly::add_markers(
      x = ratio[["Estimate"]],
      y = factor(all_label, levels = lvls),
      name = if (has_ci) "All samples, 95 % CI" else "All samples ± SE",
      marker = list(
        size = 14,
        symbol = "diamond",
        color = "#7777f9",
        line = list(width = 1.5, color = pal$font)
      ),
      error_x = if (has_ci) {
        list(
          type = "data",
          symmetric = FALSE,
          array = ratio[["CI 97.5%"]] - ratio[["Estimate"]],
          arrayminus = ratio[["Estimate"]] - ratio[["CI 2.5%"]],
          color = pal$font,
          thickness = 1.5,
          width = 6
        )
      } else {
        list(
          type = "data",
          array = ratio[["Std. Error"]],
          color = pal$font,
          thickness = 1.5,
          width = 6
        )
      },
      hovertemplate = paste0(
        "<b>All samples</b><br>k<sub>inact</sub>/K<sub>i</sub>: %{x:.4~g} ",
        ratio_unit,
        "<extra></extra>"
      ),
      inherit = FALSE
    )

  kinetics_layout(
    p,
    pal,
    units,
    xaxis = kinetics_axis(
      paste0("k<sub>inact</sub>/K<sub>i</sub> [", ratio_unit, "]"),
      pal,
      sci_axis_ticks
    ),
    yaxis = kinetics_axis("", pal, list(type = "category", showgrid = FALSE)),
    shapes = list(list(
      type = "line",
      yref = "paper",
      y0 = 0,
      y1 = 1,
      x0 = ratio[["Estimate"]],
      x1 = ratio[["Estimate"]],
      line = list(color = pal$zero, width = 1, dash = "dot")
    ))
  ) |>
    kinetics_top_legend(pal)
}

# Saturation coverage: the fraction of the maximal rate, [I] / (KI + [I]),
# reached at each measured concentration according to the curved fit's KI.
# Values far below 50 % mean the bend of the curve — and with it kinact and
# KI separately — lies outside the measured range.
#' @export
make_kinetics_saturation_plot <- function(
  kinact_ki_result,
  colors,
  units,
  theme = "dark",
  symbol_map = NULL
) {
  fit <- kinact_ki_result$Fit
  pal <- kinetics_plot_colors(theme)
  ki <- fit$KI_hyperbolic
  if (is.null(fit) || !is.finite(ki) || isTRUE(fit$KI_hyperbolic_at_bound)) {
    return(kinetics_message_plot(
      paste(
        "The curved fit finds no KI within reach:",
        "k_obs rises linearly over the whole range",
        sep = "<br>"
      ),
      theme
    ))
  }
  conc_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]])
  max_conc <- fit$max_concentration
  x_max <- max(5 * max_conc, 3 * ki)
  frac <- function(c) 100 * c / (ki + c)

  tab <- kinact_ki_result$Plateaus
  concs <- sort(tab$conc)
  labels <- as.character(concs)
  point_colors <- unname(colors[labels])
  point_colors[is.na(point_colors)] <- pal$font
  symbol_map <- resolve_symbol_map(symbol_map, labels)

  grid <- seq(0, x_max, length.out = 400)
  selected_label <- if (kinact_ki_result$Status == "saturated") {
    "KI determined"
  } else {
    "KI not determinable — curved fit estimate"
  }

  p <- plotly::plot_ly() |>
    plotly::add_lines(
      x = grid,
      y = frac(grid),
      line = list(width = 2, color = pal$font),
      hovertemplate = paste0(
        "Concentration: %{x:.3~g} ",
        conc_unit,
        "<br>%{y:.2f} % of k<sub>inact</sub><extra></extra>"
      ),
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::add_markers(
      x = concs,
      y = frac(concs),
      marker = list(
        size = 12,
        color = point_colors,
        symbol = unname(symbol_map[labels]),
        line = list(width = 1, color = pal$font)
      ),
      text = labels,
      hovertemplate = paste0(
        "<b>%{text} ",
        conc_unit,
        "</b><br>%{y:.2f} % of the maximal rate<extra></extra>"
      ),
      showlegend = FALSE,
      inherit = FALSE
    )

  kinetics_layout(
    p,
    pal,
    units,
    legend = FALSE,
    xaxis = kinetics_axis(
      paste0("Compound [", conc_unit, "]"),
      pal,
      c(list(range = c(0, x_max)), sci_axis_ticks)
    ),
    yaxis = kinetics_axis(
      "k<sub>obs</sub> / k<sub>inact</sub> [%]",
      pal,
      list(range = c(0, 100))
    ),
    shapes = list(
      list(
        type = "rect",
        xref = "x",
        yref = "paper",
        x0 = 0,
        x1 = max_conc,
        y0 = 0,
        y1 = 1,
        fillcolor = pal$shade,
        line = list(width = 0),
        layer = "below"
      ),
      list(
        type = "line",
        xref = "x",
        yref = "y",
        x0 = ki,
        x1 = ki,
        y0 = 0,
        y1 = 50,
        line = list(color = pal$zero, width = 1, dash = "dot")
      ),
      list(
        type = "line",
        xref = "x",
        yref = "y",
        x0 = 0,
        x1 = ki,
        y0 = 50,
        y1 = 50,
        line = list(color = pal$zero, width = 1, dash = "dot")
      )
    ),
    annotations = list(
      list(
        x = max_conc,
        y = 1,
        xref = "x",
        yref = "paper",
        text = sprintf("measured: up to %.0f %%", frac(max_conc)),
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "top",
        font = list(size = 12, color = pal$font)
      ),
      list(
        x = ki,
        y = 50,
        xref = "x",
        yref = "y",
        text = sprintf("K<sub>i</sub> ≈ %s %s<br>%s", signif(ki, 3), conc_unit, selected_label),
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "top",
        xshift = 6,
        font = list(size = 11, color = pal$muted)
      )
    )
  )
}

## Binding kinetics ----
#
# Every sample is one measurement. Samples measured at the same concentration
# and time are independent incubations (separate wells, separately quenched),
# so they all enter the fits as individual points; nothing is averaged before
# fitting and the Replicate label never decides what gets merged. Means and
# standard deviations per time point are computed for display only.
#
# Neither model needs an artificial (0, 0) anchor: both pass through the origin
# by construction, so such a row always fits perfectly, changes no estimate and
# only inflates the degrees of freedom (standard errors too small).

# Collapse the hits table to one row per sample. A sample contributes one row
# per matched peak, all carrying the same total binding; those rows are copies
# of a single measurement, not repeats of the experiment.
kinetic_sample_points <- function(raw_data) {
  if (!"Sample" %in% names(raw_data)) {
    raw_data$Sample <- as.character(seq_len(nrow(raw_data)))
  }
  if (!"Replicate" %in% names(raw_data)) {
    raw_data$Replicate <- NA_character_
  }

  raw_data |>
    dplyr::group_by(Sample, time) |>
    dplyr::summarise(
      Replicate = dplyr::first(as.character(Replicate)),
      binding = mean(binding, na.rm = TRUE),
      .groups = "drop"
    ) |>
    as.data.frame()
}

# Replicate series a sample belongs to, used for the per-series fits. A config
# Replicate value names the series directly (e.g. "R1"). A label KiwiMS derived
# from the file name names the condition instead (identical for R1 and R2), so
# for those the _R<n> suffix of the sample name is used. NA when neither exists.
kinetic_series_labels <- function(samples, replicate = NULL) {
  stem <- sub("\\.raw$", "", as.character(samples), ignore.case = TRUE)
  has_suffix <- grepl("_[Rr][0-9]+$", stem)
  suffix <- rep(NA_character_, length(stem))
  suffix[has_suffix] <- toupper(sub("^.*_([Rr][0-9]+)$", "\\1", stem[has_suffix]))

  if (is.null(replicate)) {
    return(suffix)
  }

  rep_chr <- trimws(as.character(replicate))
  derived <- is.na(rep_chr) |
    rep_chr == "" |
    rep_chr == sub("_[Rr][0-9]+$", "", stem)
  ifelse(derived, suffix, rep_chr)
}

# Standard errors and covariance of a minpack.lm::nls.lm() fit. Parameters
# sitting on a bound are held fixed: they get no standard error and are left
# out of the covariance of the free ones.
nls_lm_covariance <- function(fit, n, lower, upper) {
  p <- length(fit$par)
  vc <- matrix(NA_real_, p, p)
  at_bound <- abs(fit$par - lower) <= 1e-8 * pmax(1, abs(lower)) |
    (is.finite(upper) & abs(fit$par - upper) <= 1e-8 * pmax(1, abs(upper)))
  free <- which(!at_bound)
  df <- n - length(free)

  if (df > 0 && length(free) > 0) {
    inv <- tryCatch(
      solve(fit$hessian[free, free, drop = FALSE]),
      error = function(e) NULL
    )
    if (!is.null(inv)) {
      vc[free, free] <- inv * fit$deviance / df
    }
  }

  list(
    vcov = vc,
    se = sqrt(pmax(diag(vc), 0)),
    df = df,
    at_bound = at_bound
  )
}

# Starting values for the single-exponential binding curve, read off the data
# (so they work in any unit): the highest binding for the plateau and the first
# time the mean binding reaches half of it for the half-life.
binding_start_values <- function(data) {
  real <- data[data$time > 0 & !is.na(data$binding), ]
  # Kept inside the 0-100 % bounds: a start on a bound (e.g. samples reading
  # 100 %) can leave the optimiser stuck there
  plateau <- min(95, max(5, max(real$binding)))

  means <- stats::aggregate(binding ~ time, real, mean)
  means <- means[order(means$time), ]
  reached <- means$time[means$binding >= plateau / 2]
  t_half <- if (length(reached)) reached[1] else max(means$time)

  c(plateau = plateau, kobs = log(2) / t_half)
}

# Fit Binding(t) = plateau * (1 - exp(-kobs * t)) to all sample points of one
# concentration. The plateau is free (bounded to 0-100 %): measured binding
# often levels off below 100 %, and a fixed 100 % plateau misfits such data.
fit_binding_curve <- function(data) {
  start <- binding_start_values(data)
  lower <- c(0, 0)
  upper <- c(100, Inf)

  # A curve that has not levelled off yet leaves plateau and kobs loosely
  # coupled, so the fit is started from a few plateau/half-life combinations
  # and the best one is kept
  starts <- list(
    start,
    c(start[1] * 0.75, start[2]),
    c(min(95, start[1] * 1.2), start[2] / 2)
  )
  fits <- lapply(starts, function(s) {
    tryCatch(
      minpack.lm::nls.lm(
        par = s,
        fn = function(p) data$binding - p[1] * (1 - exp(-p[2] * data$time)),
        lower = lower,
        upper = upper,
        control = minpack.lm::nls.lm.control(maxiter = 500)
      ),
      error = function(e) NULL
    )
  })
  fits <- Filter(Negate(is.null), fits)
  if (length(fits) == 0) {
    stop("the binding curve fit did not converge")
  }
  fit <- fits[[which.min(vapply(fits, function(f) f$deviance, numeric(1)))]]
  cov <- nls_lm_covariance(fit, nrow(data), lower, upper)

  plateau <- unname(fit$par[1])
  kobs <- unname(fit$par[2])

  list(
    kobs = kobs,
    kobs_se = unname(cov$se[2]),
    v = plateau / 100 * kobs,
    plateau = plateau,
    n = nrow(data)
  )
}

# Mean, SD and n of the sample points per time point (display only)
binding_time_summary <- function(data) {
  data |>
    dplyr::group_by(time) |>
    dplyr::summarise(
      binding_sd = if (dplyr::n() > 1) stats::sd(binding) else NA_real_,
      n = dplyr::n(),
      binding = mean(binding),
      .groups = "drop"
    ) |>
    as.data.frame()
}

compute_kobs <- function(hits, units) {
  # Prepare empty objects
  concentration_list <- list()
  binding_table <- data.frame()
  binding_points <- data.frame()
  # Concentrations that carry data but could not be fitted. Collected here so
  # the result interface can say so; without this the only trace of them is
  # the protocol log, and a concentration silently missing from the Binding
  # Curve is easy to overlook.
  skipped <- data.frame(
    concentration = character(0),
    reason = character(0),
    stringsAsFactors = FALSE
  )

  # Observed points of a concentration that is shown but not fitted: the
  # measured means land on the curve grid, with no predicted curve behind them
  unfitted_rows <- function(obs_summary, conc, reason, grid) {
    dplyr::left_join(
      data.frame(time = grid, predicted_binding = NA_real_),
      obs_summary,
      by = "time"
    ) |>
      dplyr::mutate(
        concentration = conc,
        kobs = NA_real_,
        kobs_se = NA_real_,
        skip_reason = reason
      )
  }

  # Concentration and time columns
  conc <- names(hits)[grep("Concentration", names(hits))]
  names(hits)[grep("Time", names(hits))] <- "time"

  max_time <- max(hits$time, na.rm = TRUE)
  time_grid <- seq(0, max_time, length.out = kinetics_settings$curve_points + 1)

  concentrations <- as.character(unique(hits[[conc]]))

  # Loop over each unique concentration
  for (i in concentrations) {
    last <- i == utils::tail(concentrations, 1)

    # Filter rows for this concentration
    raw_data <- hits |>
      dplyr::filter(!!rlang::sym(conc) == i)

    # Nothing measured at this concentration (e.g. NA concentration slipped
    # through) — there is nothing to fit, so move on instead of building a
    # dummy row out of an empty frame.
    if (nrow(raw_data) == 0) {
      next
    }

    # One point per sample
    data <- kinetic_sample_points(raw_data)
    data$concentration <- i
    data$series <- kinetic_series_labels(data$Sample, data$Replicate)
    obs_summary <- binding_time_summary(data)

    # The untreated control (0 concentration) has no rate to fit; noting it
    # as a warning would flag every correctly designed experiment. It is still
    # drawn in the Binding Curve as the experiment's baseline — a control that
    # does not sit flat at 0 % points at background adduct or carry-over, and
    # the Binding Curve is where that is noticed. It gets no entry in
    # concentration_list, so it stays out of the Binding Analysis table, the
    # concentration tabs and the global fit.
    if (isTRUE(as.numeric(i) == 0)) {
      message(sprintf(
        "  │  %s %s %s: untreated control, not fitted (shown as baseline)",
        if (last) "└─" else "├─",
        fmt_log(0),
        units["Concentration"]
      ))

      binding_table <- rbind(
        binding_table,
        dplyr::left_join(
          data.frame(
            time = sort(unique(c(time_grid, obs_summary$time))),
            predicted_binding = 0
          ),
          obs_summary,
          by = "time"
        ) |>
          dplyr::mutate(
            concentration = i,
            kobs = 0,
            kobs_se = NA_real_,
            skip_reason = NA_character_
          )
      )
      binding_points <- rbind(binding_points, data)
      next
    }

    # Pre-flight: verify data is identifiable before attempting fit
    # Check before logging so skipped concentrations emit a single compact line
    fit_check <- can_fit_kobs(data)
    if (!is.null(fit_check)) {
      branch <- if (last) "└─" else "├─"
      if (grepl("^no response", fit_check)) {
        message(sprintf(
          "  │  %s %s %s %s: no response → k_obs = 0",
          branch,
          .col_warn(warning_sym),
          fmt_log(i),
          units["Concentration"]
        ))
        concentration_list[[i]] <- list(
          kobs = 0,
          kobs_se = NA_real_,
          v = 0,
          plateau = 0,
          hits = data
        )

        binding_table <- rbind(
          binding_table,
          dplyr::left_join(
            data.frame(time = time_grid, predicted_binding = 0),
            obs_summary,
            by = "time"
          ) |>
            dplyr::mutate(
              concentration = i,
              kobs = 0,
              kobs_se = NA_real_,
              skip_reason = NA_character_
            )
        )
        binding_points <- rbind(binding_points, data)
      } else {
        message(sprintf(
          "  │  %s %s %s %s: skipped (%s)",
          branch,
          .col_warn(warning_sym),
          fmt_log(i),
          units["Concentration"],
          fit_check
        ))
        # The measurements are real; only the fit is impossible. Keep the
        # points visible so the concentration does not vanish from the plot
        # without trace, but draw no curve behind them.
        skipped <- rbind(
          skipped,
          data.frame(
            concentration = i,
            reason = fit_check,
            stringsAsFactors = FALSE
          )
        )
        binding_table <- rbind(
          binding_table,
          unfitted_rows(
            obs_summary,
            i,
            fit_check,
            sort(unique(c(time_grid, obs_summary$time)))
          )
        )
        binding_points <- rbind(binding_points, data)
      }
      next
    }

    # Only log the verbose header + timepoints when actually fitting
    log_concentration(
      concentration = i,
      unit = units["Concentration"],
      last = last
    )
    log_timepoints(data = data, unit = units["Time"], last = last)

    fit_error <- NULL
    result <- tryCatch(
      fit_binding_curve(data),
      error = function(e) {
        log_fit_failed(last, conditionMessage(e))
        fit_error <<- conditionMessage(e)
        NULL
      }
    )
    if (is.null(result)) {
      skipped <- rbind(
        skipped,
        data.frame(
          concentration = i,
          reason = if (is.null(fit_error)) {
            "the curve fit did not converge"
          } else {
            fit_error
          },
          stringsAsFactors = FALSE
        )
      )
      binding_table <- rbind(
        binding_table,
        unfitted_rows(
          obs_summary,
          i,
          "the curve fit did not converge",
          sort(unique(c(time_grid, obs_summary$time)))
        )
      )
      binding_points <- rbind(binding_points, data)
      next
    }

    # Predictions on an even grid plus the measured times, so every observed
    # mean lands on a row of the curve table
    grid <- sort(unique(c(time_grid, obs_summary$time)))
    predictions <- data.frame(
      time = grid,
      predicted_binding = result$plateau * (1 - exp(-result$kobs * grid))
    )

    result$predictions <- predictions
    result$hits <- data
    concentration_list[[i]] <- result

    binding_table <- rbind(
      binding_table,
      dplyr::left_join(predictions, obs_summary, by = "time") |>
        dplyr::mutate(
          concentration = i,
          kobs = result$kobs,
          kobs_se = result$kobs_se,
          skip_reason = NA_character_
        )
    )
    binding_points <- rbind(binding_points, data)

    # Log kobs result
    log_kobs_result(
      result = result,
      last = last,
      unit = units["Time"]
    )
  }

  # Reorder concentrations as factor (skip if no concentration was fitted,
  # otherwise binding_table is a zero-column data.frame without `concentration`)
  if ("concentration" %in% names(binding_table)) {
    conc_levels <- sort(
      as.numeric(unique(binding_table$concentration)),
      decreasing = TRUE
    )
    binding_table$concentration <- factor(
      binding_table$concentration,
      levels = conc_levels
    )
    binding_points$concentration <- factor(
      binding_points$concentration,
      levels = conc_levels
    )
  }
  concentration_list[["binding_table"]] <- binding_table
  concentration_list[["binding_points"]] <- binding_points
  concentration_list[["skipped"]] <- skipped

  return(concentration_list)
}

## kinact / KI ----
#
# One global fit of all sample points of all concentrations:
#
#   Binding = P[group] * (1 - exp(-kobs([I]) * t))
#   hyperbolic: kobs = (kinact/KI) * [I] / (1 + [I]/KI)
#   linear:     kobs = (kinact/KI) * [I]
#
# Plateau groups: a concentration whose curve gets close to its plateau within
# the measured time has its own plateau; all other concentrations share one
# (see compute_kinact_ki()). kinact/KI is a fit parameter of its own, so it gets
# a standard error even when kinact and KI cannot be separated. Concentration
# and time are scaled to their maxima inside the fit, which keeps the
# parameters of order one whatever the declared units.

fit_global_kinetics <- function(points, model, groups, start) {
  conc_levels <- sort(unique(points$conc_n))
  np <- max(groups)
  pi_idx <- groups[match(points$conc_n, conc_levels)]
  hyperbolic <- model == "hyperbolic"

  kobs_n <- if (hyperbolic) {
    function(p) p[np + 1] * points$conc_n / (1 + points$conc_n / p[np + 2])
  } else {
    function(p) p[np + 1] * points$conc_n
  }

  lower <- c(rep(0, np), 0, if (hyperbolic) 1e-3)
  upper <- c(rep(100, np), Inf, if (hyperbolic) 1e3)

  fit <- minpack.lm::nls.lm(
    par = start,
    fn = function(p) {
      points$binding - p[pi_idx] * (1 - exp(-kobs_n(p) * points$time_n))
    },
    lower = lower,
    upper = upper,
    control = minpack.lm::nls.lm.control(maxiter = 1000)
  )
  cov <- nls_lm_covariance(fit, nrow(points), lower, upper)

  list(
    model = model,
    groups = groups,
    par = fit$par,
    vcov = cov$vcov,
    se = cov$se,
    df = cov$df,
    at_bound = cov$at_bound,
    deviance = fit$deviance,
    n = nrow(points),
    np = np,
    conc_levels = conc_levels
  )
}

# Fit a model from several starting points and keep the best one.
# - Plateaus: the per-concentration fits averaged per group, plus the plateau
#   of the highest concentration (its curve is the most complete) and a lower
#   variant of it. Low concentrations rarely level off within the measured
#   time, so their own plateau estimates tend to run to 100 %, and a start
#   there can leave the global fit stuck on that bound.
# - kinact/KI: a third of, equal to and three times each initial estimate.
#   Plateau and rate trade off against each other at concentrations whose
#   curve has not levelled off, so a single start can end in a local minimum.
# - KI (hyperbolic model): below, near and above the highest concentration,
#   because the error surface is flat along kinact/KI when the data does not
#   saturate.
fit_global_best <- function(points, model, groups, plateau_start, k2_start) {
  np <- max(groups)
  top <- min(95, max(5, plateau_start[length(plateau_start)]))
  per_group <- vapply(
    seq_len(np),
    function(g) min(95, max(5, mean(plateau_start[groups == g]))),
    numeric(1)
  )
  plateau_starts <- unique(list(per_group, rep(top, np), rep(0.8 * top, np)))
  k2_starts <- unique(as.vector(outer(k2_start, c(1 / 3, 1, 3))))
  ki_starts <- if (model == "hyperbolic") c(0.5, 2, 10) else NA
  combos <- expand.grid(
    p = seq_along(plateau_starts),
    k2 = k2_starts,
    ki = ki_starts
  )

  fits <- lapply(seq_len(nrow(combos)), function(j) {
    start <- c(
      plateau_starts[[combos$p[j]]],
      combos$k2[j],
      if (model == "hyperbolic") combos$ki[j]
    )
    tryCatch(
      fit_global_kinetics(points, model, groups, start),
      error = function(e) NULL
    )
  })
  fits <- Filter(Negate(is.null), fits)
  if (length(fits) == 0) {
    return(NULL)
  }
  fits[[which.min(vapply(fits, function(f) f$deviance, numeric(1)))]]
}

# Extra-sum-of-squares F-test of a nested model pair: p value for the simpler
# fit describing the data as well as the more flexible one
nested_f_test <- function(simple, complex) {
  if (is.null(simple) || is.null(complex)) {
    return(NA_real_)
  }
  extra <- simple$df - complex$df
  if (complex$df <= 0 || extra <= 0) {
    return(NA_real_)
  }
  gain <- max(simple$deviance - complex$deviance, 0)
  f_value <- (gain / extra) / (complex$deviance / complex$df)
  stats::pf(f_value, extra, complex$df, lower.tail = FALSE)
}

# Linear and hyperbolic global fits for one plateau grouping, and the selected
# one: hyperbolic only when it fits significantly better (F-test) and its KI is
# not below the lowest concentration — such a KI would mean every
# concentration is saturated, which a rising kobs contradicts
fit_global_models <- function(points, groups, plateau_start, k2_start) {
  linear <- fit_global_best(points, "linear", groups, plateau_start, k2_start)
  if (is.null(linear)) {
    return(NULL)
  }
  hyperbolic <- fit_global_best(
    points,
    "hyperbolic",
    groups,
    plateau_start,
    c(linear$par[linear$np + 1], k2_start)
  )
  p_curvature <- nested_f_test(linear, hyperbolic)
  ki_n <- if (is.null(hyperbolic)) {
    NA_real_
  } else {
    unname(hyperbolic$par[hyperbolic$np + 2])
  }
  significant <- isTRUE(p_curvature < kinetics_settings$curvature_alpha)
  ki_in_range <- isTRUE(ki_n >= min(points$conc_n))

  list(
    linear = linear,
    hyperbolic = hyperbolic,
    p_curvature = p_curvature,
    ki_n = ki_n,
    significant = significant,
    ki_in_range = ki_in_range,
    selected = if (significant && ki_in_range) hyperbolic else linear
  )
}

# Estimates in declared units from a (scaled) global fit
global_fit_estimates <- function(fit, conc_max, time_max) {
  np <- fit$np
  k2_n <- fit$par[np + 1]
  out <- list(
    ratio = k2_n / (conc_max * time_max),
    ratio_se = fit$se[np + 1] / (conc_max * time_max),
    plateaus = stats::setNames(
      fit$par[fit$groups],
      fit$conc_levels * conc_max
    )
  )

  if (fit$model == "hyperbolic") {
    ki_n <- fit$par[np + 2]
    # kinact = (kinact/KI) * KI; standard error by the delta method
    grad <- c(ki_n, k2_n)
    idx <- c(np + 1, np + 2)
    kinact_var <- as.numeric(t(grad) %*% fit$vcov[idx, idx] %*% grad)
    out$kinact <- k2_n * ki_n / time_max
    out$kinact_se <- sqrt(kinact_var) / time_max
    out$KI <- ki_n * conc_max
    out$KI_se <- fit$se[np + 2] * conc_max
    out$KI_n <- ki_n
    out$KI_at_bound <- fit$at_bound[np + 2]
  }

  out
}

# Fitted binding values of a global fit at the sample points
global_fitted <- function(points, fit) {
  np <- fit$np
  p <- fit$par
  kobs_n <- if (fit$model == "hyperbolic") {
    p[np + 1] * points$conc_n / (1 + points$conc_n / p[np + 2])
  } else {
    p[np + 1] * points$conc_n
  }
  plateau <- p[fit$groups[match(points$conc_n, fit$conc_levels)]]
  plateau * (1 - exp(-kobs_n * points$time_n))
}

# Residual bootstrap: add resampled residuals of the selected fit to its fitted
# values and refit; returns the refitted estimates (one row per resample).
# Residuals are pooled over all samples rather than resampled within each
# concentration x time cell: with two replicates per cell, resampling within
# the cell loses about half of the scatter and gives intervals that are too
# narrow. They are centred and rescaled for the fitted parameters.
bootstrap_global_kinetics <- function(points, fit, conc_max, time_max) {
  n_boot <- kinetics_settings$bootstrap_n
  fitted <- global_fitted(points, fit)
  residuals <- points$binding - fitted
  residuals <- (residuals - mean(residuals)) *
    sqrt(nrow(points) / max(fit$df, 1))

  with_kinetics_seed(kinetics_settings$bootstrap_seed, {
    rows <- lapply(seq_len(n_boot), function(b) {
      boot_points <- points
      boot_points$binding <- fitted +
        residuals[sample.int(length(residuals), replace = TRUE)]
      refit <- tryCatch(
        fit_global_kinetics(boot_points, fit$model, fit$groups, fit$par),
        error = function(e) NULL
      )
      if (is.null(refit)) {
        return(NULL)
      }
      est <- global_fit_estimates(refit, conc_max, time_max)
      data.frame(
        ratio = est$ratio,
        kinact = if (is.null(est$kinact)) NA_real_ else est$kinact,
        KI = if (is.null(est$KI)) NA_real_ else est$KI
      )
    })
    do.call(rbind, rows)
  })
}

# Run expr with a fixed seed without disturbing the session's random stream
with_kinetics_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  })
  set.seed(seed)
  expr
}

# Early time points that read exactly 0 % while later ones at the same
# concentration show binding: small adduct peaks below the deconvolution peak
# threshold are not reported and turn into 0 %, pulling the early curve down.
early_zero_points <- function(points) {
  flagged <- lapply(split(points, points$conc), function(d) {
    first_positive <- suppressWarnings(min(d$time[d$binding > 0]))
    if (!is.finite(first_positive)) {
      return(NULL)
    }
    d[d$binding == 0 & d$time < first_positive, c("conc", "time")]
  })
  do.call(rbind, flagged)
}

kinact_ki_warning <- function(code, title, detail) {
  list(code = code, title = title, detail = detail)
}

compute_kinact_ki <- function(kobs_result, units = units) {
  conc_names <- setdiff(
    names(kobs_result),
    c(
      "binding_table",
      "binding_points",
      "binding_plot",
      "kobs_result_table",
      "skipped"
    )
  )
  warnings <- list()
  add_warning <- function(w) {
    log_kinact_ki_warning(w$title, w$detail)
    warnings[[length(warnings) + 1]] <<- w
  }

  # Sample points of all fitted concentrations. Concentrations without any
  # response carry no information on the rate (their plateau fits 0 %, which
  # makes any kobs fit equally well) and are left out of the global fit.
  points <- do.call(rbind, lapply(conc_names, function(i) {
    entry <- kobs_result[[i]]
    if (is.null(entry$hits) || isTRUE(entry$kobs == 0)) {
      return(NULL)
    }
    data.frame(
      conc = as.numeric(i),
      time = entry$hits$time,
      binding = entry$hits$binding,
      series = entry$hits$series,
      sample = entry$hits$Sample
    )
  }))

  n_conc <- if (is.null(points)) 0 else length(unique(points$conc))
  if (n_conc < 3) {
    log_kinact_ki_warning(sprintf(
      "Fit skipped: %d concentration(s) with a response, at least 3 required",
      n_conc
    ))
    return(NULL)
  }

  conc_max <- max(points$conc)
  time_max <- max(points$time)
  points$conc_n <- points$conc / conc_max
  points$time_n <- points$time / time_max

  # Starting values from the per-concentration fits: their plateaus, and the
  # slope through the origin of kobs against concentration
  conc_levels <- sort(unique(points$conc))
  # (kept inside the 0-100 % bounds, see binding_start_values())
  plateau_start <- vapply(
    conc_levels,
    function(c0) {
      p <- kobs_result[[as.character(c0)]]$plateau
      if (is.null(p) || !is.finite(p)) 50 else min(95, max(5, p))
    },
    numeric(1)
  )
  kobs_n <- vapply(
    conc_levels,
    function(c0) kobs_result[[as.character(c0)]]$kobs * time_max,
    numeric(1)
  )
  cn <- conc_levels / conc_max
  k2_start <- max(sum(cn * kobs_n) / sum(cn^2), 1e-6)

  # A second start that does not rely on the per-concentration fits (which are
  # unreliable when no curve levels off): kobs from the log-linearised binding
  # assuming a 100 % plateau, then the slope through the origin
  early <- points[points$binding > 0 & points$binding < 90, ]
  if (nrow(early) > 0) {
    kobs_lin <- -log(1 - early$binding / 100) / early$time_n
    k2_data <- sum(early$conc_n * kobs_lin) / sum(early$conc_n^2)
    if (is.finite(k2_data) && k2_data > 0) {
      k2_start <- c(k2_start, k2_data)
    }
  }

  # 1) Plateau groups. A plateau can only be estimated from a curve that gets
  #    close to it within the measured time; for a curve that is still rising,
  #    plateau and rate trade off against each other, and free plateaus there
  #    can fake curvature or bias kinact/KI. So: fit with one shared plateau,
  #    then give every concentration whose curve reaches at least
  #    plateau_min_reached of its plateau by the last time point a plateau of
  #    its own; all others keep sharing one.
  alpha <- kinetics_settings$curvature_alpha
  nc <- length(conc_levels)
  shared_models <- fit_global_models(
    points,
    rep(1L, nc),
    plateau_start,
    k2_start
  )
  if (is.null(shared_models)) {
    log_kinact_ki_warning("Fit failed: the global model did not converge")
    return(NULL)
  }

  shared_est <- global_fit_estimates(shared_models$selected, conc_max, time_max)
  kobs_shared <- if (shared_models$selected$model == "hyperbolic") {
    shared_est$ratio * conc_levels / (1 + conc_levels / shared_est$KI)
  } else {
    shared_est$ratio * conc_levels
  }
  reached <- 1 - exp(-kobs_shared * time_max)
  resolved <- reached >= kinetics_settings$plateau_min_reached
  groups <- if (any(resolved)) {
    # Unresolved concentrations share group 1 (when there are any), resolved
    # ones get consecutive groups of their own. (Tying the unresolved ones to
    # the plateau of the lowest resolved concentration instead biased
    # kinact/KI in simulations where the plateau truly rises with
    # concentration.)
    g <- integer(nc)
    offset <- if (any(!resolved)) 1L else 0L
    g[!resolved] <- 1L
    g[resolved] <- offset + seq_len(sum(resolved))
    g
  } else {
    rep(1L, nc)
  }

  models <- if (max(groups) == 1L) {
    shared_models
  } else {
    fit_global_models(points, groups, plateau_start, k2_start)
  }
  if (is.null(models)) {
    models <- shared_models
    groups <- rep(1L, nc)
  }

  linear_fit <- models$linear
  hyperbolic_fit <- models$hyperbolic
  p_curvature <- models$p_curvature
  ki_n_hyp <- models$ki_n
  significant_curvature <- models$significant
  ki_in_range <- models$ki_in_range
  curved <- identical(models$selected, hyperbolic_fit) && !is.null(hyperbolic_fit)

  selected <- models$selected
  est <- global_fit_estimates(selected, conc_max, time_max)

  # Separate kinact and KI only when KI is actually pinned down by the data
  ki_rel_se <- NA_real_
  determinable <- FALSE
  if (curved) {
    ki_rel_se <- est$KI_se / est$KI
    determinable <- !isTRUE(est$KI_at_bound) &&
      is.finite(ki_rel_se) &&
      ki_rel_se <= kinetics_settings$ki_max_rel_se &&
      est$KI_n <= kinetics_settings$ki_max_over_conc
  }

  status <- if (determinable) {
    "saturated"
  } else if (curved) {
    "ki_undetermined"
  } else {
    "linear"
  }

  conc_unit <- units[["Concentration"]]
  time_unit <- units[["Time"]]

  if (significant_curvature && !ki_in_range) {
    add_warning(kinact_ki_warning(
      "curvature_implausible",
      "Curvature not interpretable",
      sprintf(
        paste(
          "the hyperbolic model fits better (p = %s) only with KI (%s %s)",
          "below the lowest concentration — treated as linear."
        ),
        fmt_log(p_curvature, 2),
        fmt_log(ki_n_hyp * conc_max),
        conc_unit
      )
    ))
  }

  if (status == "linear") {
    add_warning(kinact_ki_warning(
      "no_saturation",
      "Saturation not reached",
      sprintf(
        paste(
          "k_obs rises linearly up to the highest concentration (%s %s;",
          "curvature p = %s). kinact and KI cannot be separated —",
          "only kinact/KI is determined."
        ),
        fmt_log(conc_max),
        conc_unit,
        fmt_log(p_curvature, 2)
      )
    ))
  } else if (status == "ki_undetermined") {
    add_warning(kinact_ki_warning(
      "ki_undetermined",
      "KI not determinable",
      sprintf(
        paste(
          "k_obs curves (p = %s), but KI (%s %s) is %s —",
          "kinact and KI are extrapolated, only kinact/KI is reported."
        ),
        fmt_log(p_curvature, 2),
        fmt_log(est$KI),
        conc_unit,
        if (isTRUE(est$KI_at_bound)) {
          "at the edge of the search range"
        } else if (est$KI_n > kinetics_settings$ki_max_over_conc) {
          sprintf(
            "%s× the highest concentration",
            fmt_log(est$KI_n, 2)
          )
        } else {
          sprintf("uncertain by ±%s %%", round(100 * ki_rel_se))
        }
      )
    ))
  }

  # Bootstrap confidence intervals
  boot <- bootstrap_global_kinetics(points, selected, conc_max, time_max)
  n_boot_ok <- if (is.null(boot)) 0L else nrow(boot)
  ci <- function(x) {
    if (n_boot_ok < kinetics_settings$bootstrap_min_success *
      kinetics_settings$bootstrap_n) {
      return(c(NA_real_, NA_real_))
    }
    unname(stats::quantile(x, c(0.025, 0.975), na.rm = TRUE))
  }
  ratio_ci <- if (n_boot_ok > 0) ci(boot$ratio) else c(NA_real_, NA_real_)
  if (all(is.na(ratio_ci))) {
    add_warning(kinact_ki_warning(
      "bootstrap",
      "No confidence interval",
      sprintf(
        "only %d of %d bootstrap refits converged",
        n_boot_ok,
        kinetics_settings$bootstrap_n
      )
    ))
  }

  # Plateaus that differ between concentrations. The kinetic model assumes one
  # maximum occupancy; clearly different plateaus point at effects it does not
  # describe (inhibitor depletion or instability, protein degradation), and
  # kinact/KI can be biased by them.
  group_plateaus <- tapply(unname(est$plateaus), groups, mean)
  if (length(group_plateaus) >= 2 &&
    diff(range(group_plateaus)) > kinetics_settings$plateau_max_spread) {
    add_warning(kinact_ki_warning(
      "plateau_spread",
      "Plateaus differ",
      sprintf(
        paste(
          "binding levels off between %s %% and %s %% depending on the",
          "concentration. The model assumes one maximum occupancy; check",
          "inhibitor depletion or stability — kinact/KI may be biased."
        ),
        round(min(group_plateaus)),
        round(max(group_plateaus))
      )
    ))
  }

  # Low-intensity adducts reported as 0 %
  zeros <- early_zero_points(points)
  if (!is.null(zeros) && nrow(zeros) > 0) {
    add_warning(kinact_ki_warning(
      "early_zero",
      "Early 0 % readings",
      sprintf(
        paste(
          "%d early sample(s) at %d concentration(s) read 0 %% before binding",
          "appears — adduct peaks below the deconvolution peak threshold are",
          "recorded as 0 %% and pull the early curve down."
        ),
        nrow(zeros),
        length(unique(zeros$conc))
      )
    ))
  }

  # Parameter table (kinact, KI) — NA where not determinable
  t_p <- function(estimate, se, df) {
    t_value <- estimate / se
    c(t_value, 2 * stats::pt(-abs(t_value), df))
  }
  params <- matrix(
    NA_real_,
    nrow = 2,
    ncol = 4,
    dimnames = list(
      c("kinact", "KI"),
      c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    )
  )
  if (status == "saturated") {
    params["kinact", ] <- c(
      est$kinact,
      est$kinact_se,
      t_p(est$kinact, est$kinact_se, selected$df)
    )
    params["KI", ] <- c(est$KI, est$KI_se, t_p(est$KI, est$KI_se, selected$df))
  }

  ratio <- c(
    Estimate = est$ratio,
    `Std. Error` = est$ratio_se,
    `t value` = NA_real_,
    `Pr(>|t|)` = NA_real_,
    `CI 2.5%` = ratio_ci[1],
    `CI 97.5%` = ratio_ci[2]
  )
  ratio[c("t value", "Pr(>|t|)")] <- t_p(est$ratio, est$ratio_se, selected$df)

  kinact_ci <- c(NA_real_, NA_real_)
  ki_ci <- c(NA_real_, NA_real_)
  if (status == "saturated" && n_boot_ok > 0) {
    kinact_ci <- ci(boot$kinact)
    ki_ci <- ci(boot$KI)
  }

  # Per-series fits: the same model on each replicate series on its own,
  # showing how much a complete repeat of the experiment varies
  series <- NULL
  labels <- unique(stats::na.omit(points$series))
  if (length(labels) >= 2) {
    series <- do.call(rbind, lapply(sort(labels), function(s) {
      sp <- points[!is.na(points$series) & points$series == s, ]
      if (length(unique(sp$conc)) < 3) {
        return(NULL)
      }
      levels_s <- sort(unique(sp$conc_n))
      pos <- match(levels_s, selected$conc_levels)
      groups_s <- match(groups[pos], sort(unique(groups[pos])))
      sfit <- fit_global_best(
        sp,
        selected$model,
        groups_s,
        unname(est$plateaus[pos]),
        c(selected$par[selected$np + 1], k2_start)
      )
      if (is.null(sfit)) {
        return(NULL)
      }
      sest <- global_fit_estimates(sfit, conc_max, time_max)
      data.frame(
        series = s,
        ratio = sest$ratio,
        ratio_se = sest$ratio_se,
        kinact = if (status == "saturated") sest$kinact else NA_real_,
        KI = if (status == "saturated") sest$KI else NA_real_,
        n = nrow(sp)
      )
    }))
    if (!is.null(series) && nrow(series) < 2) {
      series <- NULL
    }
  }

  # Fitted kobs curve of the selected model plus the per-concentration kobs
  conc_grid <- seq(0, conc_max, length.out = kinetics_settings$curve_points + 1)
  predicted_kobs <- if (selected$model == "hyperbolic") {
    est$ratio * conc_grid / (1 + conc_grid / est$KI)
  } else {
    est$ratio * conc_grid
  }
  kobs_table <- kobs_result$kobs_result_table
  kobs_points <- data.frame(
    conc = as.numeric(rownames(kobs_table)),
    kobs = kobs_table$kobs,
    kobs_se = kobs_table$kobs_se
  )
  kobs_data <- dplyr::full_join(
    data.frame(conc = conc_grid, predicted_kobs = predicted_kobs),
    kobs_points,
    by = "conc"
  ) |>
    dplyr::arrange(conc)

  # Per-sample fitted values and residuals of the selected global fit, and the
  # plateau of every concentration (for the diagnostics plots)
  fitted_points <- data.frame(
    conc = points$conc,
    time = points$time,
    binding = points$binding,
    fitted = global_fitted(points, selected),
    series = points$series,
    sample = points$sample
  )
  fitted_points$residual <- fitted_points$binding - fitted_points$fitted

  group_sizes <- tabulate(groups, nbins = max(groups))
  plateau_table <- data.frame(
    conc = conc_levels,
    plateau = unname(est$plateaus),
    plateau_single = vapply(
      conc_levels,
      function(c0) {
        p <- kobs_result[[as.character(c0)]]$plateau
        if (is.null(p)) NA_real_ else p
      },
      numeric(1)
    ),
    reached = reached,
    group = groups,
    # A concentration fits its own plateau when it is alone in its group
    own = group_sizes[groups] == 1L
  )

  hyperbolic_est <- if (is.null(hyperbolic_fit)) {
    NULL
  } else {
    global_fit_estimates(hyperbolic_fit, conc_max, time_max)
  }

  list(
    Params = params,
    Ratio = ratio,
    Params_CI = rbind(kinact = kinact_ci, KI = ki_ci),
    Status = status,
    Model = selected$model,
    Fit = list(
      n_points = nrow(points),
      n_concentrations = n_conc,
      max_concentration = conc_max,
      plateau_groups = stats::setNames(groups, conc_levels),
      plateau_reached = stats::setNames(reached, conc_levels),
      p_curvature = p_curvature,
      KI_hyperbolic = ki_n_hyp * conc_max,
      KI_hyperbolic_at_bound = isTRUE(hyperbolic_est$KI_at_bound),
      ratio_hyperbolic = if (is.null(hyperbolic_est)) {
        NA_real_
      } else {
        hyperbolic_est$ratio
      },
      ratio_linear = global_fit_estimates(linear_fit, conc_max, time_max)$ratio,
      KI_rel_se = ki_rel_se,
      df = selected$df,
      rss_linear = linear_fit$deviance,
      rss_hyperbolic = if (is.null(hyperbolic_fit)) {
        NA_real_
      } else {
        hyperbolic_fit$deviance
      },
      plateaus = est$plateaus,
      bootstrap_n = kinetics_settings$bootstrap_n,
      bootstrap_ok = n_boot_ok
    ),
    Series = series,
    Warnings = warnings,
    Points = fitted_points,
    Plateaus = plateau_table,
    Bootstrap = boot,
    Kobs_Data = kobs_data
  )
}

# Function to format number in scientific
#' @export
format_scientific <- function(number, digits = 2) {
  # Calculate the absolute value and the exponent (log10)
  abs_num <- abs(number)
  exponent <- ifelse(abs_num > 0, floor(log10(abs_num)), 0)

  # Determine if scientific notation is required
  use_scientific <- exponent <= -3 | exponent >= 4

  if (use_scientific) {
    sci_str <- format(number, scientific = TRUE, digits = digits)

    # Split the string at the 'e' or 'E'
    parts <- strsplit(sci_str, "[eE]")[[1]]

    # Extract base number and power
    mantissa <- parts[1]
    # Convert to integer to clean up leading '+' or '0'
    exponent_val <- as.integer(parts[2])

    # Construct the HTML with superscript
    return(
      htmltools::tagList(
        mantissa,
        " \u00D7 10",
        htmltools::tags$sup(exponent_val)
      )
    )
  } else {
    # Determine how many decimal
    if (abs_num < 1 && abs_num > 0) {
      decimals_to_show <- abs(exponent) + digits
    } else {
      decimals_to_show <- digits
    }

    format_string <- paste0("%.", decimals_to_show, "f")
    formatted_num <- sprintf(format_string, number)

    return(formatted_num)
  }
}

# Smart truncation helper function
#' @export
label_smart_clean <- function(files) {
  if (!is.character(files) || length(files) == 0) {
    stop("Input must be a non-empty character vector.")
  }

  n <- length(files)
  if (n == 1) {
    return(files)
  }

  common_prefix <- function(strings) {
    if (length(strings) <= 1) {
      return(strings[1])
    }
    min_len <- min(nchar(strings))
    for (i in 1:min_len) {
      chars <- substr(strings, i, i)
      if (length(unique(chars)) > 1) return(substr(strings[1], 1, i - 1))
    }
    return(substr(strings[1], 1, min_len))
  }

  common_suffix <- function(strings) {
    if (length(strings) <= 1) {
      return(strings[1])
    }
    rev_strings <- sapply(strings, function(s) {
      paste(rev(strsplit(s, "")[[1]]), collapse = "")
    })
    cp <- common_prefix(rev_strings)
    paste(rev(strsplit(cp, "")[[1]]), collapse = "")
  }

  # Detect common extension
  extensions <- character(n)
  bases <- character(n)
  for (i in 1:n) {
    f <- files[i]
    dot_pos <- gregexpr("\\.", f)[[1]]
    if (length(dot_pos) > 0) {
      last_dot <- utils::tail(dot_pos, 1)
      ext <- substr(f, last_dot, nchar(f))
      if (
        nchar(ext) <= 5 && nchar(ext) >= 2 && grepl("^\\.[a-zA-Z0-9]+$", ext)
      ) {
        extensions[i] <- ext
        bases[i] <- substr(f, 1, last_dot - 1)
      } else {
        extensions[i] <- ""
        bases[i] <- f
      }
    } else {
      extensions[i] <- ""
      bases[i] <- f
    }
  }

  common_ext <- NULL
  if (all(extensions != "") && length(unique(extensions)) == 1) {
    common_ext <- unique(extensions)
  } else {
    bases <- files
  }

  # Now, common_prefix and common_base_suffix on bases
  prefix <- common_prefix(bases)
  base_suffix <- common_suffix(bases)
  pre_len <- nchar(prefix)
  suf_len <- nchar(base_suffix)

  pattern <- "[-._+# ]|[^-._+# ]+"
  prefix_parts <- if (nchar(prefix) > 0) {
    regmatches(prefix, gregexpr(pattern, prefix))[[1]]
  } else {
    character(0)
  }

  # Shorten prefix if long
  if (length(prefix_parts) > 6) {
    first_kept <- 2
    last_kept <- 1
    if (
      nchar(prefix_parts[length(prefix_parts)]) != 1 ||
        !grepl("[-._+# ]", prefix_parts[length(prefix_parts)])
    ) {
      last_kept <- 2
    }
    prefix <- paste0(
      paste0(prefix_parts[1:first_kept], collapse = ""),
      "...",
      paste0(
        prefix_parts[
          (length(prefix_parts) - last_kept + 1):length(prefix_parts)
        ],
        collapse = ""
      )
    )
  }

  base_suffix_parts <- if (nchar(base_suffix) > 0) {
    regmatches(base_suffix, gregexpr(pattern, base_suffix))[[1]]
  } else {
    character(0)
  }

  # Shorten base_suffix if long
  if (length(base_suffix_parts) > 6) {
    first_kept <- 1
    if (
      nchar(base_suffix_parts[1]) != 1 ||
        !grepl("[-._+# ]", base_suffix_parts[1])
    ) {
      first_kept <- 2
    }
    last_kept <- 2
    base_suffix <- paste0(
      paste0(base_suffix_parts[1:first_kept], collapse = ""),
      "...",
      paste0(
        base_suffix_parts[
          (length(base_suffix_parts) - last_kept + 1):length(base_suffix_parts)
        ],
        collapse = ""
      )
    )
  }

  middles <- substr(bases, pre_len + 1, nchar(bases) - suf_len)

  middle_parts_list <- lapply(middles, function(m) {
    if (nchar(m) > 0) regmatches(m, gregexpr(pattern, m))[[1]] else character(0)
  })

  long_middle_parts <- middle_parts_list
  left_m_vec <- rep(1, n)
  right_m_vec <- rep(1, n)

  max_loop <- max(0, sapply(long_middle_parts, length))
  loop_count <- 0
  found <- FALSE
  result <- files

  while (loop_count < max_loop) {
    loop_count <- loop_count + 1

    long_labels <- character(n)
    for (j in 1:n) {
      parts <- long_middle_parts[[j]]
      np <- length(parts)
      if (np == 0) {
        long_labels[j] <- ""
        next
      }
      left_m <- left_m_vec[j]
      right_m <- right_m_vec[j]
      if (np <= left_m + right_m) {
        long_labels[j] <- paste0(parts, collapse = "")
      } else {
        left <- paste0(parts[1:left_m], collapse = "")
        right <- paste0(parts[(np - right_m + 1):np], collapse = "")
        long_labels[j] <- paste0(left, "...", right)
      }
    }

    all_labels <- paste0(
      prefix,
      long_labels,
      base_suffix,
      if (is.null(common_ext)) "" else common_ext
    )

    if (length(unique(all_labels)) == n) {
      found <- TRUE
      break
    }

    dup_mask <- duplicated(all_labels) | duplicated(all_labels, fromLast = TRUE)

    dup_labels <- unique(all_labels[dup_mask])
    for (d in dup_labels) {
      group_idx <- which(all_labels == d)
      if (length(group_idx) < 2) {
        next
      }
      group_j <- group_idx
      group_parts <- long_middle_parts[group_j]
      np <- length(group_parts[[1]])
      min_pos <- np + 1
      for (p in 1:np) {
        ps <- sapply(group_parts, function(gp) gp[p])
        if (length(unique(ps)) > 1) {
          min_pos <- p
          break
        }
      }
      if (min_pos > np) {
        next
      }
      is_prev_sep <- min_pos > 1 &&
        nchar(group_parts[[1]][min_pos - 1]) == 1 &&
        grepl("[-._+# ]", group_parts[[1]][min_pos - 1])
      is_next_sep <- min_pos < np &&
        nchar(group_parts[[1]][min_pos + 1]) == 1 &&
        grepl("[-._+# ]", group_parts[[1]][min_pos + 1])
      curr_left <- left_m_vec[group_j[1]]
      curr_right <- right_m_vec[group_j[1]]
      add_left <- max(0, min_pos - curr_left)
      add_right <- max(0, (np - min_pos + 1) - curr_right)
      if (is_next_sep) {
        add_left_next <- max(0, (min_pos + 1) - curr_left)
        add_left <- max(add_left, add_left_next)
      }
      if (is_prev_sep) {
        add_right_prev <- max(0, (np - (min_pos - 1) + 1) - curr_right)
        add_right <- max(add_right, add_right_prev)
      }
      if (add_left == 0 && add_right == 0) {
        next
      }
      if (add_left <= add_right) {
        left_m_vec[group_j] <- curr_left + add_left
      } else {
        right_m_vec[group_j] <- curr_right + add_right
      }
    }
  }

  if (found) {
    # Adjust for right separator
    for (j in 1:n) {
      parts <- long_middle_parts[[j]]
      np <- length(parts)
      left_m <- left_m_vec[j]
      right_m <- right_m_vec[j]
      if (np <= left_m + right_m) {
        next
      }
      first_right <- np - right_m + 1
      if (
        !grepl("[-._+# ]", parts[first_right]) &&
          first_right > 1 &&
          grepl("[-._+# ]", parts[first_right - 1])
      ) {
        right_m_vec[j] <- right_m + 1
      }
    }
    # Adjust for left separator
    for (j in 1:n) {
      parts <- long_middle_parts[[j]]
      np <- length(parts)
      left_m <- left_m_vec[j]
      right_m <- right_m_vec[j]
      if (np <= left_m + right_m) {
        next
      }
      last_left <- left_m
      if (
        !grepl("[-._+# ]", parts[last_left]) &&
          last_left < (np - right_m + 1 - 1) &&
          grepl("[-._+# ]", parts[last_left + 1])
      ) {
        left_m_vec[j] <- left_m + 1
      }
    }
    # Build long_labels
    long_labels <- character(n)
    for (j in 1:n) {
      parts <- long_middle_parts[[j]]
      np <- length(parts)
      if (np == 0) {
        long_labels[j] <- ""
        next
      }
      left_m <- left_m_vec[j]
      right_m <- right_m_vec[j]
      if (np <= left_m + right_m) {
        long_labels[j] <- paste0(parts, collapse = "")
      } else {
        left <- paste0(parts[1:left_m], collapse = "")
        right <- paste0(parts[(np - right_m + 1):np], collapse = "")
        long_labels[j] <- paste0(left, "...", right)
      }
    }
    # Build result conditionally excluding common_ext
    result <- character(n)
    for (j in 1:n) {
      temp <- paste0(prefix, long_labels[j], base_suffix)
      if (
        grepl("...", long_labels[j], fixed = TRUE) && nchar(base_suffix) > 0
      ) {
        result[j] <- temp
      } else {
        result[j] <- paste0(temp, if (is.null(common_ext)) "" else common_ext)
      }
    }
  } else {
    result <- files
  }

  return(result)
}

# Marker traces (the peak symbols and the legend rows that name them) carry
# this tag in their `meta` attribute so a plotlyProxy can find them again in
# the rendered figure without rebuilding it.
#' @export
peaks_trace_tag <- "kiwims-peak-symbols"

# 0-based indices of the tagged traces in a built plotly figure, ready to hand
# to plotlyProxyInvoke("restyle", ...), which indexes traces the JS way.
#' @export
peaks_trace_indices <- function(built_plot) {
  traces <- built_plot$x$data

  if (is.null(traces) || length(traces) == 0) {
    return(integer(0))
  }

  # plotly treats `meta` as arrayOk and recycles a scalar along the trace's
  # points, so the tag comes back as a vector on multi-point traces.
  tagged <- vapply(
    traces,
    function(trace) {
      tag <- as.character(trace$meta)
      length(tag) > 0 && all(tag == peaks_trace_tag)
    },
    logical(1)
  )

  which(tagged) - 1L
}

# A 3D scene keeps its full domain when a legend is shown — the legend simply
# overlays the right-hand side — so the camera is nudged left to clear it.
#' @export
spectrum_legend_camera_x <- 0.33

# The three settings below only change how the figure is presented, never the
# data behind it, so they are pushed to the figure that is already on screen
# with plotlyProxy rather than costing a full rebuild and re-serialization.

# Show or hide the peak symbols and the legend rows that name them. The traces
# are always built, so this only flips their `visible` flag.
#' @export
restyle_peak_symbols <- function(session, output_id, plot_reactive, show) {
  # The figure is behind bindEvent(), so this returns the last built value and
  # never kicks off a build of its own; before the first render there is no
  # value and nothing to patch.
  indices <- tryCatch(
    peaks_trace_indices(shiny::isolate(plot_reactive())),
    error = function(e) integer(0)
  )

  if (length(indices) == 0) {
    return(invisible(NULL))
  }

  plotly::plotlyProxyInvoke(
    plotly::plotlyProxy(output_id, session),
    "restyle",
    list(visible = isTRUE(show)),
    as.list(indices)
  )

  invisible(NULL)
}

# Show or hide the legend, recentring a cubic spectrum once nothing needs
# dodging. Patching center.x alone leaves whatever rotation the user has
# dialled in untouched.
#' @export
relayout_spectrum_legend <- function(session, output_id, show, cubic = TRUE) {
  update <- list(showlegend = isTRUE(show))

  if (isTRUE(cubic)) {
    update[["scene.camera.center.x"]] <- if (isTRUE(show)) {
      spectrum_legend_camera_x
    } else {
      0
    }
  }

  plotly::plotlyProxyInvoke(
    plotly::plotlyProxy(output_id, session),
    "relayout",
    update
  )

  invisible(NULL)
}

# Show or hide the sample tick labels. Only the cubic view has a sample axis;
# the planar view plots every sample against the same two axes.
#' @export
relayout_spectrum_labels <- function(session, output_id, show, cubic = TRUE) {
  if (!isTRUE(cubic)) {
    return(invisible(NULL))
  }

  plotly::plotlyProxyInvoke(
    plotly::plotlyProxy(output_id, session),
    "relayout",
    list("scene.zaxis.showticklabels" = isTRUE(show))
  )

  invisible(NULL)
}

# Reduce one spectrum to at most `max_points` points before plotting.
#
# Min/max decimation: the trace is cut into buckets and only the lowest and
# highest intensity point of each bucket survives. Unlike plain thinning this
# never shaves a peak apex — the maximum of every bucket is kept by
# construction — so the envelope on screen is the same one the full trace
# would draw, while the baseline points that overplot each other anyway are
# dropped. Peak markers are annotated from `highlight_peaks` and are never
# downsampled, so hit positions stay exact.
downsample_spectrum <- function(df, max_points = 4000) {
  n <- nrow(df)

  if (
    !is.finite(max_points) ||
      max_points <= 0 ||
      n <= max_points ||
      !all(c("mass", "intensity") %in% names(df))
  ) {
    return(df)
  }

  # Two points survive per bucket, so ask for half as many buckets.
  n_buckets <- max(1L, as.integer(max_points %/% 2))
  bucket <- as.integer(cut(seq_len(n), breaks = n_buckets, labels = FALSE))

  keep <- unlist(
    lapply(split(seq_len(n), bucket), function(idx) {
      intensity <- df$intensity[idx]
      c(idx[which.min(intensity)], idx[which.max(intensity)])
    }),
    use.names = FALSE
  )

  # Keep the original mass order and both endpoints so the trace still spans
  # the full mass range.
  df[sort(unique(c(1L, keep, n))), , drop = FALSE]
}

# Order hit rows so that their samples run from least to most bound
#
# This is the order the Compound Distribution plots put their samples in, and
# the one the sample colour scale is built from, so total binding stays the
# single source of truth for which sample gets which colour. `by_mean` picks
# the Proteins View variant: a sample there carries several compounds, so its
# rank comes from the mean of its total binding rather than from a single row.
#' @export
binding_ordered_hits <- function(
  tbl,
  truncate_names = FALSE,
  by_mean = FALSE
) {
  if (nrow(tbl) == 0) {
    return(tbl)
  }

  if (!isTRUE(by_mean)) {
    return(dplyr::arrange(
      tbl,
      `Tot. Binding [%]`,
      `Binding [%]`,
      `Sample ID`
    ))
  }

  sid <- if (isTRUE(truncate_names) && "truncSample_ID" %in% names(tbl)) {
    tbl$truncSample_ID
  } else {
    tbl$`Sample ID`
  }

  mean_tb <- tapply(
    as.numeric(as.character(tbl$`Tot. Binding [%]`)),
    sid,
    mean,
    na.rm = TRUE
  )
  sample_order <- names(mean_tb)[order(mean_tb, names(mean_tb))]

  tbl[order(match(sid, sample_order)), , drop = FALSE]
}

# Sample order for the annotated spectrum traces
#
# `hits_summary` is arranged by concentration and time whenever both units are
# available, so the plain row order groups the spectra by concentration. With
# `sort_by_binding = TRUE` that grouping is dropped and the samples are ordered
# the way the Compound Distribution plot and the Table View order them, by
# total and per-hit binding.
#' @export
spectrum_sample_ids <- function(
  hits_summary,
  column,
  value,
  sort_by_binding = FALSE,
  by_mean = FALSE
) {
  keep <- hits_summary[[column]] == value
  keep[is.na(keep)] <- FALSE
  tbl <- hits_summary[keep, , drop = FALSE]

  if (isTRUE(sort_by_binding)) {
    tbl <- binding_ordered_hits(tbl, by_mean = by_mean)
  }

  unique(tbl$`Sample ID`)
}

# Generate spectrum with multiple traces
#' @export
multiple_spectra <- function(
  results_list,
  samples,
  cubic = TRUE,
  labels_show = NULL,
  symbols_show = TRUE,
  legend_show = TRUE,
  time = FALSE,
  color_cmp = NULL,
  truncated = FALSE,
  color_variable = NULL,
  hits_summary = NULL,
  units = NULL,
  time_factor = 1,
  max_points = 4000,
  theme = "dark"
) {
  # Omit NA in samples
  samples <- samples[!is.na(samples)]

  # Treat unset toggles as enabled
  symbols_show <- !isFALSE(symbols_show)
  legend_show <- !isFALSE(legend_show)

  # Get spectrum and peaks data. process_plot_data() parses the whole sample,
  # so call it once and take both pieces from the same result. Collecting the
  # per-sample frames in a list and binding once at the end avoids the
  # quadratic reallocation of growing a data.frame with rbind() in a loop.
  spectrum_parts <- vector("list", length(samples))
  peaks_parts <- vector("list", length(samples))

  for (i in seq_along(samples)) {
    plot_data <- process_plot_data(
      results_list$deconvolution[[samples[i]]],
      result_path = NULL
    )

    z_value <- if (time) {
      extract_minutes(samples[i]) * time_factor
    } else {
      samples[i]
    }

    mass_df <- plot_data$mass
    if (!is.null(mass_df) && nrow(mass_df) > 0) {
      spectrum_parts[[i]] <- dplyr::mutate(
        downsample_spectrum(mass_df, max_points = max_points),
        z = z_value
      )
    }

    peaks_df <- plot_data$highlight_peaks
    if (!is.null(peaks_df) && nrow(peaks_df) > 0) {
      peaks_parts[[i]] <- dplyr::mutate(peaks_df, z = z_value)
    }
  }

  spectrum_data <- as.data.frame(dplyr::bind_rows(spectrum_parts))
  peaks_data <- as.data.frame(dplyr::bind_rows(peaks_parts))

  if (nrow(spectrum_data) == 0 || !("mass" %in% names(spectrum_data))) {
    return(
      plotly::plot_ly() |>
        plotly::layout(
          annotations = list(list(
            text = "No spectrum data available",
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            font = list(
              size = 14,
              color = if (theme == "light") "black" else "white"
            )
          )),
          paper_bgcolor = if (theme == "light") "white" else "rgba(0,0,0,0)",
          plot_bgcolor = if (theme == "light") "white" else "rgba(0,0,0,0)"
        )
    )
  }

  # If truncated active adapt z variable
  if (!isFALSE(truncated)) {
    spectrum_data$z <- truncated$truncated[match(
      spectrum_data$z,
      truncated$original
    )]
  }

  if (time) {
    lvls <- rev(sort(unique(spectrum_data$z)))
  } else {
    lvls <- rev(unique(spectrum_data$z))
  }

  spectrum_data$z <- factor(
    spectrum_data$z,
    levels = if (time) {
      rev(sort(unique(spectrum_data$z)))
    } else {
      rev(unique(spectrum_data$z))
    }
  )

  # If truncated active adapt z variable
  if (!isFALSE(truncated)) {
    peaks_data$z <- truncated$truncated[match(
      peaks_data$z,
      truncated$original
    )]
  }

  # Transform z variable to factor
  peaks_data$z <- factor(
    peaks_data$z,
    levels = if (time) {
      rev(sort(unique(peaks_data$z)))
    } else {
      rev(unique(peaks_data$z))
    }
  )

  font_color <- if (theme == "light") "black" else "white"
  inv_color <- if (theme == "light") "white" else "black"
  grid_color <- if (theme == "light") "rgba(0,0,0,0.1)" else "#7f7f7fff"
  zeroline_color <- if (theme == "light") {
    "rgba(0,0,0,0.5)"
  } else {
    "rgba(255,255,255,0.5)"
  }

  # Prepare hit marker symbols
  if (!all(is.na(peaks_data$mass))) {
    prot_peaks <- hits_summary$`Meas. Prot. [Da]`[
      if (time) {
        hits_summary$`Sample ID` %in% samples
      } else if (!isFALSE(truncated)) {
        hits_summary$truncSample_ID %in% peaks_data$z
      } else {
        hits_summary$`Sample ID` %in% peaks_data$z
      }
    ]
    prot_peaks <- prot_peaks[!is.na(prot_peaks)]

    prot_names <- unique(peaks_data$name[peaks_data$mass %in% prot_peaks])

    peaks_data <- dplyr::mutate(
      peaks_data,
      symbol = ifelse(mass %in% prot_peaks, "diamond", "circle"),
      linecolor = font_color
    )
  } else {
    peaks_data <- dplyr::mutate(
      peaks_data,
      symbol = "circle",
      linecolor = font_color
    )
  }

  color_cmp <- color_cmp[!is.na(names(color_cmp))]

  # Prepare compound marker colors and symbols
  if (!is.null(color_cmp) && !is.null(color_variable)) {
    if (color_variable == "Compounds") {
      if (length(color_cmp)) {
        # Adding protein peak marker
        prot_colors <- rep(font_color, length(prot_names))
        names(prot_colors) <- prot_names
        color_cmp <- c(prot_colors, color_cmp)

        # Match colors to peaks data
        peaks_data$color <- color_cmp[match(
          as.character(peaks_data$name),
          names(color_cmp)
        )]

        marker_color <- ~ I(color)
      } else {
        marker_color <- font_color
      }

      peaks_data$mk_color <- if (is.character(marker_color)) {
        marker_color
      } else {
        peaks_data$color
      }

      # Declare coloring variables for graph elements
      color <- NULL
      line <- list(color = font_color, width = 1)
      z_linecolor <- list(color = font_color, width = 1)
    } else if (color_variable == "Samples") {
      # Match colors to peaks and spectrum data
      peaks_data$z_color <- color_cmp[match(peaks_data$z, names(color_cmp))]
      spectrum_data$z_color <- color_cmp[match(
        spectrum_data$z,
        names(color_cmp)
      )]

      # Protein diamonds use theme fill; compound circles use sample color
      peaks_data <- dplyr::mutate(
        peaks_data,
        z_color = ifelse(symbol == "diamond", font_color, z_color)
      )

      peaks_data$mk_color <- peaks_data$z_color

      # Declare coloring variables for graph elements
      color <- ~ I(z_color)
      line <- list(width = 1)
      marker_color <- ~ I(z_color)
      z_linecolor <- list(width = 1)
    }
  } else {
    # Make color palette
    color_cmp <- brighten_hex(
      viridisLite::viridis(length(unique(spectrum_data$z))),
      factor = 1.5
    )
    names(color_cmp) <- levels(spectrum_data$z)

    # Adding protein peak marker
    peaks_data <- dplyr::mutate(
      peaks_data,
      color = ifelse(symbol == "diamond", font_color, inv_color)
    )
    peaks_data$mk_color <- peaks_data$color
    marker_color <- ~ I(color)

    # Match colors to spectrum data
    spectrum_data$z_color <- color_cmp[match(
      spectrum_data$z,
      names(color_cmp)
    )]

    # Declare coloring variables for graph elements
    color <- ~ I(z_color)
    line <- list(width = 1)
    z_linecolor <- list(width = 1)
  }

  # Condition on data size
  if (is.null(labels_show)) {
    z_chr <- as.character(peaks_data$z)
    labels_show <- (length(unique(z_chr)) <= 8 &
      (length(z_chr) == 0 || max(nchar(z_chr)) <= 20)) |
      isTRUE(time)
  }

  # Remove NA peaks
  peaks_data <- peaks_data[!is.na(peaks_data$mass), ]

  if (cubic) {
    plot <- plotly::plot_ly(
      data = spectrum_data,
      x = ~mass,
      y = ~intensity,
      z = ~z,
      split = ~z,
      legendgroup = ~z,
      color = color,
      line = z_linecolor,
      type = "scatter3d",
      mode = "lines",
      showlegend = TRUE,
      hoverinfo = "text",
      text = ~ paste0(
        "Mass: ",
        mass,
        " Da\nIntensity: ",
        round(intensity, 2),
        "%",
        ifelse(
          time == TRUE,
          "\nTime: ",
          "\nSample: "
        ),
        z,
        ifelse(
          time == TRUE,
          paste0(" ", gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])),
          ""
        )
      )
    )

    # Add hit markers. marker.color and marker.symbol are both arrayOk, so a
    # whole sample's peaks fit into one trace instead of the one-trace-per-peak
    # fan-out this used to build. Splitting at the sample level rather than
    # collapsing to a single trace keeps the legendgroup link intact, so
    # hiding a sample in the legend still hides its markers.
    if (nrow(peaks_data) > 0) {
      for (lvl in levels(peaks_data$z)) {
        peaks_lvl <- peaks_data[
          !is.na(peaks_data$z) & peaks_data$z == lvl, ,
          drop = FALSE
        ]

        if (nrow(peaks_lvl) == 0) {
          next
        }

        plot <- plot |>
          plotly::add_markers(
            data = peaks_lvl,
            x = ~mass,
            y = ~intensity,
            z = ~z,
            legendgroup = lvl,
            mode = "markers",
            inherit = FALSE,
            visible = symbols_show,
            meta = peaks_trace_tag,
            marker = list(
              color = peaks_lvl$mk_color,
              symbol = peaks_lvl$symbol,
              size = 5,
              zindex = 100,
              line = list(color = inv_color, width = 3)
            ),
            hoverinfo = "text",
            text = ~ paste0(
              "Name: ",
              name,
              "\nMeasured: ",
              mass,
              " Da\nIntensity: ",
              round(intensity, 2),
              ifelse(time, "%\nTime: ", "%\nSample: "),
              z,
              ifelse(
                time,
                paste0(" ", gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])),
                ""
              ),
              "\nTheor. Mw: ",
              mw
            ),
            showlegend = FALSE
          )
      }
    }

    if (nrow(peaks_data) > 0 && !time) {
      name_entries <- peaks_data |>
        dplyr::filter(!is.na(name)) |>
        dplyr::distinct(name, symbol) |>
        dplyr::arrange(dplyr::desc(symbol == "diamond"), name)

      protein_seen <- FALSE
      compound_seen <- FALSE

      for (i in seq_len(nrow(name_entries))) {
        entry_name <- name_entries$name[i]
        sym <- name_entries$symbol[i]
        first_peak <- peaks_data[peaks_data$name == entry_name, ][1, ]
        is_protein <- sym == "diamond"
        lg <- if (is_protein) "proteins" else "compounds"
        add_lgt <- (is_protein && !protein_seen) ||
          (!is_protein && !compound_seen)

        args <- list(
          p = plot,
          inherit = FALSE,
          type = "scatter",
          mode = "markers",
          x = 0,
          y = 0,
          name = entry_name,
          legendgroup = lg,
          marker = list(
            color = font_color,
            symbol = paste0(sym, "-open"),
            size = 8
          ),
          visible = symbols_show,
          meta = peaks_trace_tag,
          showlegend = TRUE,
          legendrank = i,
          hoverinfo = "skip"
        )
        if (add_lgt) {
          args$legendgrouptitle <- list(
            text = if (is_protein) "Proteins" else "Compounds",
            font = list(color = font_color)
          )
        }
        plot <- do.call(plotly::add_trace, args)

        if (is_protein) protein_seen <- TRUE else compound_seen <- TRUE
      }

      plot <- plotly::style(
        plot,
        legendgrouptitle = list(
          text = "Samples",
          font = list(color = font_color)
        ),
        traces = 1
      )
    }

    plot |>
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(size = 14, color = font_color),
        showlegend = legend_show,
        xaxis = list(
          visible = FALSE,
          showgrid = FALSE,
          zeroline = FALSE,
          range = c(1, 2)
        ),
        yaxis = list(
          visible = FALSE,
          showgrid = FALSE,
          zeroline = FALSE,
          range = c(1, 2)
        ),
        legend = list(
          bgcolor = "rgba(0,0,0,0)",
          bordercolor = "rgba(0,0,0,0)",
          font = list(size = 11, color = font_color),
          itemsizing = "constant",
          tracegroupgap = 4,
          title = list(
            text = if (time) {
              paste0(
                "<b> Time [",
                gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
                "]  </b>"
              )
            } else {
              ""
            },
            color = font_color
          )
        ),
        # 3D Scene Styling
        scene = list(
          aspectmode = "manual",
          aspectratio = list(
            x = 1,
            y = 1,
            z = ifelse(length(unique(peaks_data$z)) <= 3, 0.3, 1.0)
          ),
          xaxis = list(
            title = "Mass [Da]",
            titlefont = list(size = 14, color = font_color),
            tickfont = list(size = 12, color = font_color),
            gridcolor = grid_color,
            showgrid = TRUE,
            showline = FALSE,
            linecolor = "rgba(0,0,0,0)",
            showzeroline = FALSE,
            zerolinecolor = "rgba(0,0,0,0)",
            showticklabels = TRUE,
            showspikes = FALSE,
            showbackground = FALSE
          ),
          yaxis = list(
            title = "Intensity [%]",
            titlefont = list(size = 14, color = font_color),
            tickfont = list(size = 12, color = font_color),
            gridcolor = grid_color,
            showgrid = TRUE,
            showline = FALSE,
            linecolor = "rgba(0,0,0,0)",
            showzeroline = FALSE,
            zerolinecolor = "rgba(0,0,0,0)",
            showticklabels = TRUE,
            showspikes = FALSE,
            showbackground = FALSE
          ),
          zaxis = list(
            title = ifelse(
              time,
              paste0(
                "Time [",
                gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
                "]"
              ),
              ""
            ),
            titlefont = list(size = 14, color = font_color),
            tickfont = list(size = 12, color = font_color),
            gridcolor = grid_color,
            showgrid = ifelse(time, TRUE, FALSE),
            showline = FALSE,
            linecolor = "rgba(0,0,0,0)",
            showzeroline = FALSE,
            zerolinecolor = "rgba(0,0,0,0)",
            showticklabels = labels_show,
            showspikes = FALSE,
            showbackground = FALSE,
            type = 'category',
            tickvals = levels(spectrum_data$z)
          ),
          camera = list(
            # center = list(x = 0.33, y = -0.05, z = 0.05),
            center = list(
              # With no legend there is nothing to dodge and the spectrum sits
              # centred; see spectrum_legend_camera_x.
              x = if (legend_show) spectrum_legend_camera_x else 0,
              y = ifelse(length(unique(peaks_data$z)) < 4, 0.075, -0.05),
              z = 0.05
            ),
            # eye = if (length(unique(peaks_data$z)) <= 8) {
            eye = if (length(unique(peaks_data$z)) <= 3) {
              list(
                x = 1 +
                  length(unique(peaks_data$z)) / 20 +
                  ifelse(labels_show, 0.2, 0),
                y = 0.7 +
                  length(unique(peaks_data$z)) / 20 +
                  ifelse(labels_show, 0.2, 0),
                z = 1 +
                  length(unique(peaks_data$z)) / 20 +
                  ifelse(labels_show, 0.2, 0)
              )
            } else {
              list(x = 1.13, y = 0.74, z = 1.58)
            },
            up = list(x = -0.28, y = 0.9, z = -0.33)
          )
        )
      )
  } else {
    planar_colors <- if (
      !is.null(color_variable) && color_variable == "Samples"
    ) {
      color_cmp
    } else if (!is.null(color_variable) && color_variable == "Compounds") {
      # Lines all same color; compound markers handle the coloring
      z_levels <- levels(spectrum_data$z)
      stats::setNames(rep(font_color, length(z_levels)), z_levels)
    } else {
      brighten_hex(
        viridisLite::viridis(length(unique(peaks_data$z))),
        factor = 1.5
      )
    }

    plot_2d <- plotly::plot_ly(
      data = spectrum_data,
      x = ~mass,
      y = ~intensity,
      color = ~z,
      colors = planar_colors,
      legendgroup = ~z,
      # WebGL rather than SVG: a spectrum runs to thousands of points per
      # sample, which the SVG renderer draws one path node at a time. The
      # single-sample spectrum_plot() already uses scattergl for the same
      # reason. Downloads go out via saveWidget(), so WebGL traces export fine.
      type = "scattergl",
      mode = "lines",
      hoverinfo = "text",
      text = ~ paste0(
        "Mass: ",
        mass,
        " Da\nIntensity: ",
        round(intensity, 2),
        "%",
        ifelse(
          time == TRUE,
          "\nTime: ",
          "\nSample: "
        ),
        z,
        ifelse(
          time == TRUE,
          paste(" ", gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])),
          ""
        )
      ),
      showlegend = TRUE
    )

    # One marker trace per sample rather than one per peak — see the cubic
    # branch above for why the split stays at the sample level.
    if (nrow(peaks_data) > 0) {
      for (lvl in levels(peaks_data$z)) {
        peaks_lvl <- peaks_data[
          !is.na(peaks_data$z) & peaks_data$z == lvl, ,
          drop = FALSE
        ]

        if (nrow(peaks_lvl) == 0) {
          next
        }

        plot_2d <- plot_2d |>
          plotly::add_markers(
            data = peaks_lvl,
            x = ~mass,
            y = ~intensity,
            legendgroup = lvl,
            # SVG, not WebGL, even though the lines above are scattergl:
            # plotly.js draws the WebGL canvas underneath the SVG layers, so an
            # SVG marker trace is always in front of the spectrum lines no
            # matter how the traces are ordered. There are only a handful of
            # peaks per sample, so nothing is lost by drawing them as SVG.
            type = "scatter",
            mode = "markers",
            inherit = FALSE,
            visible = symbols_show,
            meta = peaks_trace_tag,
            marker = list(
              color = peaks_lvl$mk_color,
              symbol = peaks_lvl$symbol,
              size = 10,
              zindex = 100,
              line = list(color = inv_color, width = 1.5)
            ),
            hoverinfo = "text",
            text = ~ paste0(
              "Name: ",
              name,
              "\nMeasured: ",
              mass,
              " Da\nIntensity: ",
              round(intensity, 2),
              ifelse(
                time,
                "%\nTime: ",
                "%\nSample: "
              ),
              z,
              ifelse(
                time,
                paste0(" ", gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])),
                ""
              ),
              "\nTheor. Mw: ",
              mw
            ),
            showlegend = FALSE
          )
      }
    }

    if (nrow(peaks_data) > 0) {
      name_entries <- peaks_data |>
        dplyr::filter(!is.na(name)) |>
        dplyr::distinct(name, symbol) |>
        dplyr::arrange(dplyr::desc(symbol == "diamond"), name)

      protein_seen <- FALSE
      compound_seen <- FALSE

      for (i in seq_len(nrow(name_entries))) {
        entry_name <- name_entries$name[i]
        sym <- name_entries$symbol[i]
        first_peak <- peaks_data[peaks_data$name == entry_name, ][1, ]
        is_protein <- sym == "diamond"
        lg <- if (is_protein) "proteins" else "compounds"
        add_lgt <- (is_protein && !protein_seen) ||
          (!is_protein && !compound_seen)

        args <- list(
          p = plot_2d,
          inherit = FALSE,
          type = "scatter",
          mode = "markers",
          x = 0,
          y = 0,
          xaxis = "x2",
          yaxis = "y2",
          name = entry_name,
          legendgroup = lg,
          marker = list(
            color = font_color,
            symbol = if (is_protein) sym else paste0(sym, "-open"),
            size = 8
          ),
          visible = symbols_show,
          meta = peaks_trace_tag,
          showlegend = TRUE,
          legendrank = i,
          hoverinfo = "skip"
        )
        if (add_lgt) {
          args$legendgrouptitle <- list(
            text = if (is_protein) "Proteins" else "Compounds",
            font = list(color = font_color)
          )
        }
        plot_2d <- do.call(plotly::add_trace, args)

        if (is_protein) protein_seen <- TRUE else compound_seen <- TRUE
      }

      plot_2d <- plotly::style(
        plot_2d,
        legendgrouptitle = list(
          text = "Samples",
          font = list(color = font_color)
        ),
        traces = 1
      )
    }

    plot_2d |>
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(size = 14, color = font_color),
        showlegend = legend_show,
        xaxis = list(
          title = "Mass [Da]",
          color = font_color,
          gridcolor = grid_color,
          zerolinecolor = zeroline_color
        ),
        yaxis = list(
          title = "Intensity [%]",
          color = font_color,
          gridcolor = grid_color,
          zerolinecolor = zeroline_color
        ),
        xaxis2 = list(
          visible = FALSE,
          showgrid = FALSE,
          zeroline = FALSE,
          range = c(1, 2),
          overlaying = "x"
        ),
        yaxis2 = list(
          visible = FALSE,
          showgrid = FALSE,
          zeroline = FALSE,
          range = c(1, 2),
          overlaying = "y"
        ),
        legend = list(
          bgcolor = "rgba(0,0,0,0)",
          bordercolor = "rgba(0,0,0,0)",
          font = list(size = 11, color = font_color),
          itemsizing = "constant",
          tracegroupgap = 4,
          title = list(
            text = if (time) {
              paste0(
                "<b> Time [",
                gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
                "]  </b>"
              )
            } else {
              ""
            },
            color = font_color
          )
        )
      )
  }
}

# Filter function for table view
#' @export
filter_table_view <- function(table, colors, inputs, units) {
  # Replace NA in color names
  names(colors)[is.na(names(colors))] <- "N/A"

  # Get optional concentration and time cols
  optional_cols <- if (length(units) == 2) {
    c(units[["Concentration"]], units[["Time"]])
  } else {
    NULL
  }

  # Merge non-preferred hits per peak into their preferred counterpart
  table <- table |>
    dplyr::arrange(
      `Sample ID`,
      `Cmp Name`,
      `Peak Signal [Da]`,
      dplyr::desc(Preferred == "TRUE"),
      dplyr::desc(suppressWarnings(as.numeric(`Theor. Cmp [Da]`)))
    ) |>
    dplyr::group_by(`Sample ID`, `Cmp Name`, `Peak Signal [Da]`) |>
    dplyr::reframe(
      truncSample_ID = `truncSample_ID`[1],
      dplyr::across(dplyr::any_of(optional_cols), ~ .x[1]),
      mass_stoich_html = {
        theor <- `Theor. Cmp [Da]`
        stoich <- `Bind. Stoich.`
        valid <- !is.na(theor) & theor != "N/A"
        if (!any(valid)) {
          "N/A"
        } else {
          paste(
            paste0(
              "[",
              theor[valid],
              "]&thinsp;",
              sapply(stoich[valid], function(x) {
                as.character(htmltools::tags$sub(x))
              })
            ),
            collapse = " + "
          )
        }
      },
      `Theor. Cmp [Da]` = {
        theor <- `Theor. Cmp [Da]`
        valid <- !is.na(theor) & theor != "N/A"
        if (any(valid)) theor[valid][1] else NA_character_
      },
      `Bind. Stoich.` = {
        theor <- `Theor. Cmp [Da]`
        stoich <- `Bind. Stoich.`
        valid <- !is.na(theor) & theor != "N/A"
        if (any(valid)) stoich[valid][1] else NA_character_
      },
      `Binding [%]` = {
        pref <- `Binding [%]`[Preferred == "TRUE"]
        if (length(pref) > 0) pref[1] else `Binding [%]`[1]
      },
      `Tot. Binding [%]` = `Tot. Binding [%]`[1]
    ) |>
    dplyr::select(-`Peak Signal [Da]`)

  table <- if (length(units) == 2) {
    dplyr::arrange(
      table,
      `Cmp Name`,
      as.numeric(.data[[units[["Concentration"]]]]),
      as.numeric(.data[[units[["Time"]]]])
    )
  } else {
    dplyr::arrange(
      table,
      `Cmp Name`,
      `Tot. Binding [%]`,
      `Binding [%]`,
      `Sample ID`
    )
  }

  # Prepare data frame for table
  tbl <- table |>
    dplyr::ungroup() |>
    dplyr::select(
      `Sample ID` = `Sample ID`,
      `Cmp Name` = `Cmp Name`,
      dplyr::any_of(optional_cols),
      `Mass Shift` = mass_stoich_html,
      `Theor. Cmp [Da]` = `Theor. Cmp [Da]`,
      `Bind. Stoich.` = `Bind. Stoich.`,
      `Binding [%]` = `Binding [%]`,
      `Total %` = `Tot. Binding [%]`
    ) |>
    dplyr::mutate(
      # Truncated label used only for color matching / display; original Sample ID kept
      trunc_label = if (inputs$truncate_names) {
        table$`truncSample_ID`
      } else {
        `Sample ID`
      },
      `Cmp Name` = ifelse(is.na(`Cmp Name`), "N/A", `Cmp Name`),
      label_color = get_contrast_color(colors[match(
        if (
          length(units) == 2 && inputs$color_variable == units["Concentration"]
        ) {
          table[[units["Concentration"]]]
        } else if (inputs$color_variable == "Compounds") {
          `Cmp Name`
        } else if (inputs$color_variable == "Samples") {
          trunc_label
        },
        names(colors)
      )]),
      `Binding [%]` = `Binding [%]`,
      `Total %` = `Total %`,
      # As.character, because the row colour is applied browser side by
      # DT::styleEqual(): a numeric cell is compared against the level string,
      # which JavaScript parses back into a double. After a unit conversion
      # that round trip loses the last bit (0.9375 μM -> 9.375e-07 M reads back
      # one ULP off) and the row silently loses its colour. Comparing the
      # canonical strings on both sides sidesteps the float entirely.
      col_var = as.character(!!rlang::sym(
        if (
          length(units) == 2 &&
            inputs$color_variable == units[["Concentration"]]
        ) {
          units[["Concentration"]]
        } else if (inputs$color_variable == "Compounds") {
          "Cmp Name"
        } else if (inputs$color_variable == "Samples") {
          "trunc_label"
        }
      ))
    )

  # Show the concentration in the same canonical form. Besides matching the
  # colour key it keeps the column short: the raw double reaches the browser at
  # full precision and renders as 9.374999999999999e-7.
  if (length(units) == 2) {
    conc_col <- units[["Concentration"]]
    if (conc_col %in% names(tbl)) {
      tbl[[conc_col]] <- as.character(tbl[[conc_col]])
    }
  }

  return(tbl)
}

# Rendering function for relative binding table view
#' @export
render_table_view <- function(table, colors, tab, inputs, units) {
  # Replace NA in color names
  names(colors)[is.na(names(colors))] <- "N/A"

  # Apply bar renderer to binding column
  if (
    is.null(inputs$binding_bar) ||
      isTRUE(inputs$binding_bar)
  ) {
    render_binding <- htmlwidgets::JS(chart_js)
  } else {
    render_binding <- NULL
  }

  # Apply bar renderer to total binding column
  if (
    is.null(inputs$tot_binding_bar) ||
      isTRUE(inputs$tot_binding_bar)
  ) {
    render_tot_binding <- htmlwidgets::JS(chart_js)
  } else {
    render_tot_binding <- NULL
  }

  # Determine grouped row variable
  if (tab == "Compounds") {
    group_variable <- "Sample ID"
  } else if (any(tab %in% c("Samples", "Proteins"))) {
    group_variable <- "Cmp Name"
  } else {
    group_variable <- NULL
  }

  if (
    is.null(group_variable) ||
      length(unique(table[[group_variable]])) == nrow(table)
  ) {
    row_group <- NULL
  } else {
    row_group <- list(dataSrc = which(names(table) == group_variable) - 1)
  }

  # Display-only transforms: truncate Sample ID, format Mass Shift, coerce
  # binding columns to character when bar renderer is off
  if (isTRUE(inputs$truncate_names) && "trunc_label" %in% names(table)) {
    table[["Sample ID"]] <- table[["trunc_label"]]
  }
  if (!is.null(inputs$binding_bar) && !isTRUE(inputs$binding_bar)) {
    table[["Binding [%]"]] <- sprintf("%.2f", table[["Binding [%]"]])
  }
  if (!is.null(inputs$tot_binding_bar) && !isTRUE(inputs$tot_binding_bar)) {
    table[["Total %"]] <- sprintf("%.2f", table[["Total %"]])
  }

  # Add  prefix to group rows
  if (!is.null(row_group)) {
    if (tab == "Compounds") {
      table$`Sample ID` <- paste("Sample:", table$`Sample ID`)
    } else if (any(tab %in% c("Samples", "Proteins"))) {
      table$`Cmp Name` <- paste("Compound:", table$`Cmp Name`)
    }
  }

  DT::datatable(
    data = table,
    escape = FALSE,
    extensions = "RowGroup",
    rownames = FALSE,
    class = "order-column",
    selection = "none",
    options = list(
      dom = 't',
      paging = FALSE,
      ordering = FALSE,
      scrollY = TRUE,
      scrollCollapse = TRUE,
      rowGroup = row_group,
      columnDefs = list(
        list(
          visible = ifelse(
            is.null(group_variable) ||
              length(unique(table[[group_variable]])) == nrow(table),
            TRUE,
            FALSE
          ),
          targets = group_variable
        ),
        list(
          visible = FALSE,
          targets = c(
            "col_var",
            "label_color",
            "trunc_label",
            "Theor. Cmp [Da]",
            "Bind. Stoich.",
            if (tab == "Concentration") "Cmp Name"
          )
        ),
        list(className = 'dt-center', targets = "_all"),
        list(className = 'dt-nowrap', targets = "Mass Shift"),
        list(
          targets = "Binding [%]",
          type = "num",
          className = if (!is.null(render_binding)) "bar-chart-col" else NULL,
          render = render_binding
        ),
        list(
          targets = "Total %",
          type = "num",
          className = if (!is.null(render_tot_binding)) {
            "bar-chart-col"
          } else {
            NULL
          },
          render = render_tot_binding
        ),
        list(
          targets = -1,
          className = 'dt-last-col'
        )
      )
    )
  ) |>
    DT::formatStyle(
      columns = "col_var",
      target = 'row',
      backgroundColor = DT::styleEqual(
        levels = names(colors),
        values = colors
      ),
      color = DT::styleEqual(
        levels = names(colors),
        values = get_contrast_color(colors)
      )
    ) |>
    (\(dt) {
      if (tab == "Proteins") {
        na_label <- if (!is.null(row_group)) "Compound: N/A" else "N/A"
        dt |>
          DT::formatStyle(
            "Cmp Name",
            target = "row",
            backgroundColor = DT::styleEqual(na_label, "transparent"),
            color = DT::styleEqual(na_label, "inherit")
          )
      } else {
        dt
      }
    })()
}

# Selection and filtering of hits table
#' @export
filter_hits_table <- function(
  hits_table,
  selected_cols = NULL,
  compounds = NULL,
  samples = NULL,
  units
) {
  # Filter compounds
  if (!is.null(compounds) && length(compounds) > 0) {
    hits_table <- dplyr::filter(
      hits_table,
      `Cmp Name` %in% compounds | is.na(`Cmp Name`)
    )
  }

  # Filter samples
  if (!is.null(samples) && length(samples) > 0) {
    hits_table <- dplyr::filter(hits_table, `Sample ID` %in% samples)
  }

  # Filter columns
  if (!is.null(selected_cols)) {
    selected_cols <- selected_cols[selected_cols %in% names(hits_table)]
  }

  std_cols <- c(
    "Sample ID",
    "Protein",
    "Cmp Name",
    if ("Concentration" %in% names(units)) units[["Concentration"]] else NULL,
    if ("Time" %in% names(units)) units[["Time"]] else NULL,
    "truncSample_ID"
  )

  hits_table <- hits_table |>
    dplyr::select(
      dplyr::any_of(std_cols),
      all_of(selected_cols)
    )

  return(hits_table)
}

# Transform per hit table to display per-adduct entries
#' @export
transform_per_adduct <- function(
  hits_table,
  proteins_table,
  compounds_table,
  samples_table
) {
  # Get distinct adducts
  distinct_adducts <- dplyr::distinct(hits_table, `Sample ID`, `Cmp Name`)

  # Get colnames of retained columns subset
  col_names <- names(hits_table)[
    !names(hits_table) %in%
      c(
        "Peak Signal [Da]",
        "Int. Cmp [%]",
        "Theor. Cmp [Da]",
        "Δ Cmp [Da]",
        "Bind. Stoich.",
        "Preferred",
        "Binding [%]"
      )
  ]

  # Initiate list object to store table intermediates
  entries <- vector("list", nrow(distinct_adducts))

  # Iterate generating one table per distinct adduct
  for (i in seq_len(nrow(distinct_adducts))) {
    sample <- distinct_adducts$`Sample ID`[i] # Current sample
    cmp <- distinct_adducts$`Cmp Name`[i] # Current compound

    if (is.na(cmp)) {
      hits_per_adduct <- hits_table[
        hits_table$`Sample ID` == sample,
        col_names
      ][1, ]
    } else {
      # Build hits df per adduct
      hits_table_subset <- hits_table[
        hits_table$`Sample ID` == sample & hits_table$`Cmp Name` == cmp,
      ]
      hits_per_adduct <- hits_table_subset[, col_names][1, ]

      # Append mass-shift columns
      mass_shifts <- unique(hits_table_subset$`Theor. Cmp [Da]`)
      cmp_mass_shifts <- as.numeric(compounds_table[
        compounds_table$Compound == cmp,
        -1
      ])

      for (mass_shift in mass_shifts) {
        hits_table_subset_mass_shift <- hits_table_subset[
          hits_table_subset$`Theor. Cmp [Da]` == mass_shift,
        ]

        mass_no <- which(cmp_mass_shifts %in% as.numeric(mass_shift))
        if (length(mass_no) == 0) {
          next
        }

        binding_vals <- as.list(hits_table_subset_mass_shift$`Binding [%]`)
        names(binding_vals) <- paste0(
          "Binding (Mass ",
          mass_no,
          ifelse(hits_table_subset_mass_shift$Preferred == "FALSE", "*", ""),
          ")x",
          hits_table_subset_mass_shift$`Bind. Stoich.`,
          " [%]"
        )
        binding_vals[[paste0("Total Binding (Mass ", mass_no, ") [%]")]] <-
          sum(hits_table_subset_mass_shift$`Binding [%]`)

        binding_vals[[paste("Mass", mass_no)]] <- cmp_mass_shifts[[mass_no]]

        hits_per_adduct <- dplyr::mutate(hits_per_adduct, !!!binding_vals)
      }

      # Attach mass shifts for respective compound
      # hits_per_adduct <- dplyr::bind_cols(
      #   hits_per_adduct,
      #   compounds_table[compounds_table$Compound == cmp, -1]
      # )
    }

    ## TODO
    # # Optionally adding concentration and time columns
    # if ("Concentration" %in% names(units)) {
    #   hits_per_adduct <- dplyr::mutate(
    #     hits_per_adduct,
    #     !!rlang::sym(
    #       units[[
    #         "Concentration"
    #       ]]
    #     ) := hits_table_subset$concentration,
    #     !!rlang::sym(units[["Time"]]) := hits_table_subset$time,
    #     .after = Protein
    #   )
    # }

    entries[[i]] <- hits_per_adduct
  }

  # Join all hits_per_adduct tables
  hits_table <- suppressMessages(Reduce(dplyr::full_join, entries))

  ### Sort mass-shift columns by mass
  all_cols <- names(hits_table)

  # 1. Define helper to extract numbers from strings
  get_number <- function(string, pattern) {
    matches <- regmatches(string, regexpr(pattern, string, perl = TRUE))
    as.numeric(gsub("[^0-9]", "", matches))
  }

  # 2. Identify and define the raw mass columns
  is_raw_mass <- grepl("^Mass [0-9]+$", all_cols)
  raw_mass_names <- all_cols[is_raw_mass]
  is_binding <- grepl("^(Binding|Total Binding) \\(", all_cols)
  binding_cols <- all_cols[is_binding]

  # 3. Calculate Stoichiometry for binding_cols
  # We look for the "x" followed by a number
  stoich_vals <- suppressWarnings(as.numeric(sub(
    ".*x([0-9]+).*",
    "\\1",
    binding_cols
  )))
  stoich_vals[is.na(stoich_vals)] <- Inf # Totals get Inf to move to the end

  # 4. Get the Mass ID for all components
  all_relevant_cols <- c(raw_mass_names, binding_cols)
  all_mass_ids <- c(
    get_number(raw_mass_names, "[0-9]+"),
    get_number(binding_cols, "(?<=Mass )[0-9]+")
  )

  # 5. Define hierarchy for sorting
  # Raw Mass = 1, Binding = 2, Total = 3
  type_rank <- ifelse(
    grepl("^Mass [0-9]+$", all_relevant_cols),
    1,
    ifelse(grepl("^Total", all_relevant_cols), 3, 2)
  )

  # 6. Perform the sort
  # Order by: Mass ID -> Type Rank -> Stoichiometry
  # Note: For raw mass columns, we assign a stoich of 0 to ensure they come first
  all_stoich <- c(rep(0, length(raw_mass_names)), stoich_vals)

  final_sorted_relevant <- all_relevant_cols[order(
    all_mass_ids,
    type_rank,
    all_stoich
  )]

  # 7. Final full vector
  fixed_cols <- all_cols[
    !grepl("^Mass [0-9]+$", all_cols) &
      !grepl("^(Binding|Total Binding) \\(", all_cols)
  ]
  final_column_order <- c(fixed_cols, final_sorted_relevant)

  return(hits_table[, final_column_order])
}

# Rendering function of hits table
#' @export
render_hits_table <- function(
  hits_table,
  concentration_colors,
  single_conc = NULL,
  bar_chart = character(),
  colors = NULL,
  color_variable = NULL,
  truncated = NULL,
  clickable = FALSE,
  valid_concentrations = NULL,
  per_adduct = FALSE,
  units
) {
  # Adapt table layout
  if (!is.null(single_conc)) {
    menu_length <- list(c(25, -1), c('25', 'All'))
    dom_value <- "t"
  } else {
    menu_length <- list(
      c(15, 25, 50, 100, -1),
      c('15', '25', '50', '100', 'All')
    )
    dom_value <- "fti"
  }

  # 0-based indices of hidden columns (truncSample_ID); used to align
  # td:eq() visible-index with the data-array index in rowCallback
  hidden_col_json <- jsonlite::toJSON(
    which(names(hits_table) == "truncSample_ID") - 1L
  )

  # Determine clickable cells
  rowCallback <- NULL
  if (!isFALSE(clickable)) {
    if (any(names(hits_table) %in% clickable)) {
      clickable_targets <- which(
        names(hits_table) %in% clickable
      ) -
        1

      valid_conc_js <- if (!is.null(valid_concentrations)) {
        jsonlite::toJSON(as.numeric(valid_concentrations))
      } else {
        "null"
      }

      rowCallback <- c(
        "function(row, data){",
        "  var targets = ",
        jsonlite::toJSON(clickable_targets),
        ";",
        "  var validConc = ",
        valid_conc_js,
        ";",
        "  var hiddenCols = ",
        hidden_col_json,
        ";",
        "  var visOffset = 0;",
        "  for(var i=0; i<data.length; i++){",
        "    if(hiddenCols.includes(i)){ visOffset++; continue; }",
        "    var vi = i - visOffset;",
        "    if(data[i] === null){",
        "      $('td:eq('+vi+')', row).html('N/A').css({'color': 'inherit'});",
        "    }",
        "    if(targets.includes(i) && data[i] !== null){",
        "      var isValid = validConc === null || validConc.includes(parseFloat(data[i]));",
        "      if(isValid) $('td:eq('+vi+')', row).addClass('clickable-column');",
        "    }",
        "  }",
        "}"
      )
    } else {
      clickable_targets <- NULL
    }
  }

  if (is.null(rowCallback)) {
    rowCallback <- c(
      "function(row, data){",
      "  var hiddenCols = ",
      hidden_col_json,
      ";",
      "  var visOffset = 0;",
      "  for(var i=0; i<data.length; i++){",
      "    if(hiddenCols.includes(i)){ visOffset++; continue; }",
      "    if(data[i] === null){",
      "      $('td:eq('+(i-visOffset)+')', row).html('N/A').css({'color': 'inherit'});",
      "    }",
      "  }",
      "}"
    )
  }

  # Percentage columns formatted to 2 d.p. via columnDefs render (not formatRound)
  # so null cells survive rowCallback as "N/A" without post-draw interference.
  binding_fmt_cols <- setdiff(
    intersect(
      c(
        "Binding [%]",
        "Tot. Binding [%]",
        "Unmatched [%]",
        "Correct [%]",
        "Int. Prot. [%]",
        "Int. Cmp [%]"
      ),
      names(hits_table)
    ),
    bar_chart
  )

  # Generate datatable
  hits_datatable <- DT::datatable(
    data = hits_table,
    rownames = FALSE,
    class = "order-column",
    selection = list(
      mode = ifelse(!isFALSE(clickable), "single", "none"),
      target = 'cell'
    ),
    options = list(
      rowCallback = htmlwidgets::JS(rowCallback),
      headerCallback = htmlwidgets::JS({
        visible_cols <- names(hits_table)[names(hits_table) != "truncSample_ID"]
        tooltip_names <- ifelse(
          visible_cols %in% names(hits_col_full_names),
          hits_col_full_names[visible_cols],
          visible_cols
        )
        paste0(
          "function(thead, data, start, end, display) {",
          "  var names = ",
          jsonlite::toJSON(tooltip_names),
          ";",
          "  $(thead).find('th').each(function(i) {",
          "    if (i < names.length) $(this).attr('title', names[i]);",
          "    var h = $(this).html().replace(/Mass (\\d)/g, 'Mass\\u00a0$1');",
          "    var n = 0;",
          "    h = h.replace(/ /g, function(m) { return ++n <= 2 ? m : '\\u00a0'; });",
          "    $(this).html(h);",
          "  });",
          "}"
        )
      }),
      scrollX = TRUE,
      scrollY = TRUE,
      scrollCollapse = TRUE,
      stripe = FALSE,
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        "  var api = this.api();",
        "  setTimeout(function() { api.columns.adjust(); }, 50);",
        "}"
      ),
      dom = dom_value,
      paging = ifelse(!is.null(single_conc), TRUE, FALSE),
      columnDefs = list(
        list(className = 'dt-center', targets = "_all"),
        list(
          targets = 0,
          className = 'dt-left',
          createdCell = htmlwidgets::JS(
            "function(td) { td.style.textAlign = 'left'; }"
          )
        ),
        if (length(bar_chart) > 0 & any(bar_chart %in% names(hits_table))) {
          list(
            targets = bar_chart[bar_chart %in% names(hits_table)],
            render = htmlwidgets::JS(chart_js),
            type = "num",
            className = "bar-chart-col"
          )
        } else {
          list()
        },
        if ("truncSample_ID" %in% names(hits_table)) {
          list(
            visible = FALSE,
            targets = "truncSample_ID"
          )
        } else {
          list()
        },
        if (length(binding_fmt_cols) > 0) {
          list(
            targets = binding_fmt_cols,
            render = htmlwidgets::JS(
              "function(data, type, row, meta) {",
              "  if (type !== 'display') return data;",
              "  if (data === null || data === undefined) return null;",
              "  var n = parseFloat(data);",
              "  return isNaN(n) ? data : n.toFixed(2);",
              "}"
            )
          )
        } else {
          list()
        },
        list(
          targets = -1,
          className = 'dt-last-col'
        )
      )
    )
  )

  adduct_fmt_cols <- setdiff(
    grep("^(Binding|Total Binding) \\(", names(hits_table), value = TRUE),
    bar_chart
  )
  if (length(adduct_fmt_cols) > 0) {
    hits_datatable <- DT::formatRound(
      hits_datatable,
      columns = which(names(hits_table) %in% adduct_fmt_cols),
      digits = 2
    )
  }

  if (!is.null(concentration_colors)) {
    if (!is.null(single_conc)) {
      conc_color <- concentration_colors[which(
        names(concentration_colors) == single_conc
      )]

      hits_datatable <- hits_datatable |>
        DT::formatStyle(
          columns = 2,
          target = 'row',
          backgroundColor = conc_color
        )
    } else {
      hits_datatable <- hits_datatable |>
        DT::formatStyle(
          columns = units[["Concentration"]],
          target = 'row',
          backgroundColor = DT::styleEqual(
            levels = names(concentration_colors),
            values = concentration_colors
          ),
          color = DT::styleEqual(
            levels = names(concentration_colors),
            values = get_contrast_color(concentration_colors)
          )
        )
    }
  } else if (!is.null(colors)) {
    if (color_variable == "Compounds" & anyNA(hits_table$`Cmp Name`)) {
      names(colors)[
        names(colors) %in% c("NA", "N/A", as.character(NA))
      ] <- as.character(NA)
    }

    hits_datatable <- hits_datatable |>
      DT::formatStyle(
        columns = ifelse(
          color_variable == "Compounds",
          "Cmp Name",
          ifelse(truncated, "truncSample_ID", "Sample ID")
        ),
        target = 'row',
        backgroundColor = DT::styleEqual(
          levels = names(colors),
          values = colors
        ),
        color = DT::styleEqual(
          levels = names(colors),
          values = get_contrast_color(colors)
        )
      )
  }

  return(hits_datatable)
}

# Define JS to fetch checkbox inputs from table
#' @export
js_code_gen <- function(dtid, cols, ns = identity) {
  code <- vector("list", length(cols))
  for (i in seq_along(cols)) {
    col <- cols[i]
    code[[i]] <- c(
      sprintf(
        "$('body').on('click', '[id^=checkb_%d_]', function() {",
        col
      ),
      "  var id = this.getAttribute('id');",
      sprintf("  var i = parseInt(/checkb_%d_(\\d+)/.exec(id)[1]);", col),
      "  var value = $(this).prop('checked');",
      sprintf("  var info = [{row: i, col: %d, value: value}];", col),
      sprintf(
        "  Shiny.setInputValue('%s', info);",
        ns(sprintf("%s_cell_edit:DT.cellInfo", dtid))
      ),
      "});"
    )
  }
  do.call(c, code)
}

# Define checkbox generator
#' @export
checkboxColumn <- function(len, col, value = TRUE, ...) {
  # Recycled so a per-row state (e.g. included concentrations) survives a
  # re-render of the table
  value <- rep_len(value, len)

  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(shiny::checkboxInput(
      paste0("checkb_", col, "_", i),
      label = NULL,
      value = value[i],
      ...
    ))
  }
  inputs
}

# Compute replicate group labels for a vector of sample names.
# Priority is resolved per sample (not globally), so a config that only
# covers some of the current samples doesn't blind the fallbacks for the
# rest: (1) config Replicate value for that sample, (2) _R<n> filename
# suffix detection, (3) a unique R<n> placeholder for samples with no
# detectable suffix at all.
#' @export
compute_replicate_labels <- function(sample_names, config = NULL) {
  labels <- rep(NA_character_, length(sample_names))

  # Priority 1: config supplies a non-empty Replicate value for this sample
  if (!is.null(config) && "Replicate" %in% names(config)) {
    cfg_key <- gsub("\\.raw$", "", config$Sample, ignore.case = TRUE)
    samp_key <- gsub("\\.raw$", "", sample_names, ignore.case = TRUE)
    matched <- config$Replicate[match(samp_key, cfg_key)]
    non_empty <- !is.na(matched) & trimws(matched) != ""
    labels[non_empty] <- trimws(matched[non_empty])
  }

  # Priority 2: filename suffix detection (_R<n> before optional .raw), for
  # any sample not already labeled from config. Assign the shared base name
  # to every sample carrying an _R<n> suffix, even when its partner
  # replicate(s) are missing (e.g. a failed deconvolution dropped one file
  # from the set). A singleton still gets a label derived from its own
  # filename instead of falling through to the arbitrary global counter
  # below, which would otherwise hand out meaningless, order-dependent
  # "R1"/"R2"/... tags with no relation to the sample's actual _R<n> suffix
  # or to which other rows it belongs with.
  remaining <- is.na(labels)
  if (any(remaining)) {
    has_rn <- grepl("_[Rr]\\d+(\\.raw)?$", sample_names)
    base_names <- gsub("_[Rr]\\d+(\\.raw)?$", "", sample_names)
    base_names <- gsub("\\.raw$", "", base_names, ignore.case = TRUE)
    fill_idx <- remaining & has_rn
    labels[fill_idx] <- base_names[fill_idx]
  }

  # Fill remaining NAs (samples with no config value and no _R<n> suffix)
  # with R<n>, avoiding clashes with existing R<n>-shaped labels
  existing <- labels[!is.na(labels)]
  used_ints <- suppressWarnings(stats::na.omit(as.integer(
    regmatches(
      existing,
      regexpr("(?<=[Rr])\\d+$", existing, perl = TRUE)
    )
  )))
  ctr <- 1L
  for (i in which(is.na(labels))) {
    while (ctr %in% used_ints) {
      ctr <- ctr + 1L
    }
    labels[i] <- paste0("R", ctr)
    used_ints <- c(used_ints, ctr)
    ctr <- ctr + 1L
  }
  labels
}

# Empty sample declaration table generator function
#' @export
new_sample_table <- function(
  result,
  protein_table,
  compound_table,
  kinact_ki = FALSE
) {
  sample_names <- sort(paste0(
    result$samples %||% names(result$deconvolution),
    ".raw"
  ))
  sample_tab <- data.frame(
    Sample = sample_names,
    Protein = ifelse(
      length(protein_table$Protein) == 1,
      protein_table$Protein,
      ""
    ),
    Compound = ifelse(
      length(compound_table$Compound) == 1,
      compound_table$Compound,
      ""
    ),
    rep(list(""), 4)
  )

  if (!is.null(kinact_ki) && kinact_ki) {
    sample_tab <- cbind(
      sample_tab,
      Concentration = as.numeric(NA),
      Time = as.numeric(NA)
    )
  }

  colnames(sample_tab) <- c(
    "Sample",
    "Protein",
    paste("Compound", 1:5),
    if (isTRUE(kinact_ki)) c("Concentration", "Time")
  )

  return(sample_tab)
}

# Re-apply Concentration/Time values from an old sample table into a newly
# rebuilt one, matched by Sample name. Rows not present in the old table keep
# their NA values.
#' @export
restore_conc_time <- function(new_table, old_table) {
  if (is.null(old_table)) {
    return(new_table)
  }
  conc_col_old <- grep("^Concentration", names(old_table), value = TRUE)
  time_col_old <- grep("^Time", names(old_table), value = TRUE)
  if (length(conc_col_old) != 1 || length(time_col_old) != 1) {
    return(new_table)
  }
  conc_col_new <- grep("^Concentration", names(new_table), value = TRUE)
  time_col_new <- grep("^Time", names(new_table), value = TRUE)
  if (length(conc_col_new) != 1 || length(time_col_new) != 1) {
    return(new_table)
  }
  idx <- match(new_table$Sample, old_table$Sample)
  new_table[[conc_col_new]] <- old_table[[conc_col_old]][idx]
  new_table[[time_col_new]] <- old_table[[time_col_old]][idx]
  new_table
}

# UI changes when conversion declaration tab is confirmed
#' @export
confirm_ui_changes <- function(
  tab,
  session,
  output
) {
  tab_low <- tolower(tab)

  # Show toast
  shinyWidgets::show_toast(
    "Table saved!",
    text = NULL,
    type = "success",
    timer = 3000,
    timerProgressBar = TRUE
  )

  # Update confirm button
  shiny::updateActionButton(
    session = session,
    paste0("confirm_", tab_low),
    label = ifelse(tab != "Samples", "Saved", ""),
    icon = shiny::icon("check")
  )

  # Disable confirm button
  shinyjs::disable(paste0("confirm_", tab_low))

  # Enable edit button
  shinyjs::enable(paste0("edit_", tab_low))

  # Disable clear button
  shinyjs::disable(paste0("clear_", tab_low))

  # Disable unit selectors (Samples tab only)
  if (tab_low == "samples") {
    shinyjs::disable("conc_unit")
    shinyjs::addClass(
      selector = ".shiny-input-container:has(#app-conversion_main-conc_unit) .bootstrap-select",
      class = "custom-disable"
    )
    shinyjs::disable("time_unit")
    shinyjs::addClass(
      selector = ".shiny-input-container:has(#app-conversion_main-time_unit) .bootstrap-select",
      class = "custom-disable"
    )
  }

  # Disable file upload
  shinyjs::disable(paste0(tab_low, "_fileinput"))
  shinyjs::addClass(
    selector = paste0(
      ".btn-file:has(#app-conversion_main-",
      tab_low,
      "_fileinput"
    ),
    class = "custom-disable"
  )
  shinyjs::addClass(
    selector = paste0(
      ".input-group:has(#app-conversion_main-",
      tab_low,
      "_fileinput) > .form-control"
    ),
    class = "custom-disable"
  )

  # Show table message
  output[[paste0(tab_low, "_table_info")]] <- shiny::renderText("Table saved!")

  # Mark tab as done
  shinyjs::runjs(paste0(
    'document.querySelector(".nav-link[data-value=\'',
    tab,
    '\']").classList.add("done");'
  ))
}

# UI changes when conversion declaration tab is edited
#' @export
edit_ui_changes <- function(
  tab,
  session,
  output
) {
  tab_low <- tolower(tab)

  # Update confirm button
  shiny::updateActionButton(
    session = session,
    paste0("confirm_", tab_low),
    label = ifelse(tab != "Samples", "Save", ""),
    icon = shiny::icon("bookmark")
  )

  # Enable confirm button
  shinyjs::enable(paste0("confirm_", tab_low))

  # Disable edit button
  shinyjs::disable(paste0("edit_", tab_low))

  # Enable clear button
  shinyjs::enable(paste0("clear_", tab_low))

  # Enable unit selectors (Samples tab only)
  if (tab_low == "samples") {
    shinyjs::enable("conc_unit")
    shinyjs::removeClass(
      selector = ".shiny-input-container:has(#app-conversion_main-conc_unit) .bootstrap-select",
      class = "custom-disable"
    )
    shinyjs::enable("time_unit")
    shinyjs::removeClass(
      selector = ".shiny-input-container:has(#app-conversion_main-time_unit) .bootstrap-select",
      class = "custom-disable"
    )
  }

  # Enable file upload
  shinyjs::enable(paste0(tab_low, "_fileinput"))
  shinyjs::removeClass(
    selector = paste0(
      ".btn-file:has(#app-conversion_main-",
      tab_low,
      "_fileinput"
    ),
    class = "custom-disable"
  )
  shinyjs::removeClass(
    selector = paste0(
      ".input-group:has(#app-conversion_main-",
      tab_low,
      "_fileinput) > .form-control"
    ),
    class = "custom-disable"
  )

  # Mark tab as undone
  shinyjs::runjs(paste0(
    'document.querySelector(".nav-link[data-value=\'',
    tab,
    '\']").classList.remove("done");'
  ))
}

# Slice sample declaration table row-wise
#' @export
table_observe <- function(
  table,
  tab,
  output,
  ns,
  proteins,
  compounds,
  tolerance = 3
) {
  # Show waiter with 0.25 seconds minimum runtime; on.exit ensures hide always runs
  waiter::waiter_show(
    id = ns(paste0(tab, "_table_info")),
    html = waiter::spin_throbber()
  )
  on.exit(
    waiter::waiter_hide(id = ns(paste0(tab, "_table_info"))),
    add = TRUE
  )
  Sys.sleep(0.25)

  # If table non-empty check for correctness
  if (nrow(table) < 1) {
    # Table info UI changes
    shinyjs::removeClass(
      paste0(tab, "_table_info"),
      "table-info-green"
    )
    shinyjs::removeClass(
      paste0(tab, "_table_info"),
      "table-info-red"
    )
    output[[paste0(tab, "_table_info")]] <- shiny::renderText(
      "Fill table ..."
    )
    output[[paste0(tab, "_table_hint")]] <- shiny::renderUI(NULL)

    # Disable confirm button
    shinyjs::disable(paste0("confirm_", tab))

    # Set status variable to FALSE
    table_status <- FALSE
  } else {
    # Validate correct input
    if (tab == "samples") {
      check_function <- "check_sample_table"
      args <- list(
        sample_table = table,
        proteins = proteins,
        compounds = compounds
      )
    } else {
      check_function <- "check_table"
      args <- list(tab = table, tolerance = tolerance)
    }

    table_check <- do.call(what = check_function, args = args)

    if (isTRUE(table_check)) {
      # Table info UI changes
      shinyjs::removeClass(
        paste0(tab, "_table_info"),
        "table-info-red"
      )
      shinyjs::addClass(
        paste0(tab, "_table_info"),
        "table-info-green"
      )
      output[[paste0(tab, "_table_info")]] <- shiny::renderText(
        "Table can be saved"
      )
      output[[paste0(tab, "_table_hint")]] <- shiny::renderUI(NULL)

      # Enable confirm button
      shinyjs::enable(paste0("confirm_", tab))

      # Set status variable to TRUE
      table_status <- TRUE
    } else {
      # Table info UI changes — show short status, detail goes to hint below table
      shinyjs::removeClass(
        paste0(tab, "_table_info"),
        "table-info-green"
      )
      shinyjs::addClass(
        paste0(tab, "_table_info"),
        "table-info-red"
      )
      output[[paste0(tab, "_table_info")]] <- shiny::renderText(
        "Fix table issues"
      )
      local({
        msg <- table_check
        hint_class <- if (grepl("^Duplicated", msg)) {
          "table-hint table-hint-orange"
        } else {
          "table-hint table-hint-red"
        }
        output[[paste0(tab, "_table_hint")]] <- shiny::renderUI(
          shiny::div(
            class = hint_class,
            shiny::icon("triangle-exclamation"),
            msg
          )
        )
      })

      # Disable confirm button
      shinyjs::disable(paste0("confirm_", tab))

      # Set status variable to FALSE
      table_status <- FALSE
    }
  }

  return(table_status)
}

# Generalized function to handle file uploads for proteins or compounds
#' @export
handle_file_upload <- function(
  file_input,
  type,
  output,
  declaration_vars
) {
  tryCatch(
    {
      # Read in file
      table_upload <- read_uploaded_file(
        file_input$datapath,
        tolower(tools::file_ext(file_input$name))
      )

      # Process table and check for errors
      table_upload_processed <- process_uploaded_table(table_upload, type)

      # Update UI and status variable based on processing result
      if (is.data.frame(table_upload_processed)) {
        shinyWidgets::show_toast(
          paste0(tools::toTitleCase(type), " table loaded!"),
          type = "success",
          timer = 3000
        )
        return(table_upload_processed)
      } else {
        shinyWidgets::show_toast(
          table_upload_processed,
          type = "error",
          timer = 3000
        )
        return(NULL)
      }
    },
    error = function(e) {
      shinyWidgets::show_toast(
        paste0("Failed to load ", type, " file. Please check the file format."),
        type = "error",
        timer = 4000
      )
      return(NULL)
    }
  )
}

# Transform summarized hits into readable table
#' @export
transform_hits <- function(hits_summary) {
  # Stash Replicate before the positional colnames() assignment
  replicate_col <- if ("Replicate" %in% names(hits_summary)) {
    tmp <- hits_summary[["Replicate"]]
    hits_summary <- hits_summary[,
      names(hits_summary) != "Replicate",
      drop = FALSE
    ]
    tmp
  } else {
    NULL
  }

  # Shared transformations
  summary_table <- hits_summary |>
    dplyr::mutate(
      # Convert binding cols to exact percentage — rounding only happens in DT display
      dplyr::across(
        c(`% Binding`, `Total % Binding`),
        ~ dplyr::if_else(is.na(.x), 0, .x * 100)
      ),
      # Round protein mass columns (kept numeric for sorting/export)
      dplyr::across(
        dplyr::any_of(c("Mw Protein [Da]", "Measured Mw Protein [Da]")) &
          where(is.numeric),
        ~ round(.x, 1)
      ),
      # Format remaining [Da] columns only if numeric
      dplyr::across(
        dplyr::ends_with("[Da]") &
          where(is.numeric) &
          !dplyr::any_of(c("Mw Protein [Da]", "Measured Mw Protein [Da]")),
        ~ dplyr::if_else(
          is.na(.x),
          "N/A",
          format(.x, nsmall = 1, trim = TRUE)
        )
      ),
      # Global NA cleanup (convert to character, exclude compound and binding cols)
      dplyr::across(
        !dplyr::any_of(c(
          "Compound",
          "% Binding",
          "Total % Binding",
          "Mw Protein [Da]"
        )),
        ~ tidyr::replace_na(as.character(.x), "N/A")
      )
    ) |>
    dplyr::relocate(dplyr::any_of("Total % Binding"), .after = "% Binding") |>
    dplyr::relocate(
      dplyr::any_of(c("% Unmatched", "% Correct")),
      .after = "Total % Binding"
    )

  # Define column names
  col_names <- c(
    "Well",
    "Sample ID",
    "Protein",
    "Theor. Prot. [Da]",
    "Meas. Prot. [Da]",
    "Δ Prot. [Da]",
    "Int. Prot. [%]",
    "Peak Signal [Da]",
    "Int. Cmp [%]",
    "Cmp Name",
    "Theor. Cmp [Da]",
    "Δ Cmp [Da]",
    "Bind. Stoich.",
    "Preferred",
    "Binding [%]",
    "Tot. Binding [%]",
    "Unmatched [%]",
    "Correct [%]"
  )

  # Interface dependent logic
  conc_time <- sapply(
    c("Concentration", "Time", "binding"),
    grepl,
    names(summary_table)
  )

  if (sum(conc_time) == 3) {
    conc_col <- names(summary_table)[which(conc_time[,
      "Concentration"
    ])]
    time_col <- names(summary_table)[which(conc_time[,
      "Time"
    ])]

    summary_table <- summary_table |>
      dplyr::select(-c("binding")) |>
      dplyr::relocate(
        c(
          !!rlang::sym(conc_col),
          !!rlang::sym(time_col)
        ),
        .before = `Mw Protein [Da]`
      )

    col_names <- append(
      col_names,
      c(gsub("Concentration", "Conc.", conc_col), time_col),
      after = 3
    )
  }

  colnames(summary_table) <- col_names

  # Reattach Replicate right after "Sample ID" with consistent NA handling
  if (!is.null(replicate_col)) {
    summary_table[["Replicate"]] <- tidyr::replace_na(
      as.character(replicate_col),
      "N/A"
    )
    summary_table <- dplyr::relocate(
      summary_table,
      "Replicate",
      .after = "Sample ID"
    )
  }

  return(summary_table)
}

# Calculate black/white font color depending on background brightness
#' @export
get_contrast_color <- function(hex_codes) {
  rgb_vals <- grDevices::col2rgb(hex_codes)
  # Brightness formula (YIQ)
  brightness <- (299 *
    rgb_vals[1, ] +
    587 * rgb_vals[2, ] +
    114 * rgb_vals[3, ]) /
    1000
  # If brightness > 128 (bright), use Black text, else White
  ifelse(brightness > 128, "#000000", "#ffffff")
}

# Adjust brightness.
#
# The brightening exists so the dark end of a palette stays legible against
# the app's dark plot background. An exported figure is looked at on its own -
# on white paper, in a slide deck - where that lift only distorts the palette,
# so exports opt out via the option below (see with_export_palette() in
# app/logic/plot_download.R). It is deliberately keyed on "is this an export"
# rather than on the theme argument: a dark-themed export should still carry
# the palette's true colours.
brighten_hex <- function(hex_colors, factor = 1.2) {
  # Neutralise the factor rather than returning early, so the round trip
  # through col2rgb()/hsv() still happens: it normalises viridisLite's 8-digit
  # "#RRGGBBFF" down to "#RRGGBB", and callers downstream compare and index
  # these strings.
  if (isTRUE(getOption("kiwims.export_palette", FALSE))) {
    factor <- 1
  }

  # Convert Hex to HSV
  rgb_vals <- grDevices::col2rgb(hex_colors)
  hsv_vals <- grDevices::rgb2hsv(rgb_vals)

  # Multiply the 'Value' (brightness) channel
  # We use pmin to ensure we don't exceed the maximum value of 1
  hsv_vals[3, ] <- pmin(hsv_vals[3, ] * factor, 1)

  # Convert back to Hex
  grDevices::hsv(hsv_vals[1, ], hsv_vals[2, ], hsv_vals[3, ])
}

# Sample n colors from a sequential brewer palette, cutting off the darkest end.
# cutoff = 0.85 means only the lightest 85% of the palette is used.
brewer_seq_colors <- function(n, scale, max_colors, cutoff = 0.85) {
  avail <- max(floor(max_colors * cutoff), 3)
  raw <- RColorBrewer::brewer.pal(avail, scale)
  if (n == 1) {
    return(raw[1])
  }
  raw[round(seq(1, avail, length.out = n))]
}

# Make uniform color scale for compounds
#' @export
get_cmp_colorScale <- function(
  filtered_table,
  scale,
  variable,
  trunc,
  conc_col = NULL
) {
  if (variable == "Compounds") {
    # cmp_levels <- unique(filtered_table[["Theor. Cmp"]])
    cmp_levels <- unique(filtered_table[["Cmp Name"]])
  } else if (variable == "Samples") {
    if (trunc) {
      cmp_levels <- unique(filtered_table[["truncSample_ID"]])
    } else {
      cmp_levels <- unique(filtered_table[["Sample ID"]])
    }
  } else if (variable == "Concentration") {
    cmp_levels <- unique(filtered_table[[conc_col]])
  }

  n <- length(cmp_levels)

  # Initialize output
  colors <- NULL

  for (i in 1:2) {
    # RColorBrewer Scales
    if (scale %in% c(qualitative_scales, sequential_scales)) {
      # Check max colors available for this specific Brewer palette
      max_colors <- RColorBrewer::brewer.pal.info[scale, "maxcolors"]

      # Shift to gradient scale if n exceeds the palette's max limit
      if (n > max_colors) {
        message(paste(
          "N =",
          n,
          "exceeds max colors (",
          max_colors,
          ") for palette",
          scale
        ))

        scale <- "viridis"
      } else {
        if (scale %in% sequential_scales) {
          colors <- brewer_seq_colors(n, scale, max_colors)
        } else {
          # Qualitative: keep existing subsetting
          n_request <- max(n, 3)
          raw_colors <- RColorBrewer::brewer.pal(n_request, scale)
          if (n == 2) {
            colors <- raw_colors[c(1, 2)]
          } else if (n == 1) {
            colors <- raw_colors[1]
          } else {
            colors <- raw_colors[1:n]
          }
        }
        break
      }

      # ViridisLite Scales
    } else if (scale %in% gradient_scales) {
      vir_func <- getExportedValue("viridisLite", scale)
      dark_begin_scales <- c("magma", "inferno", "rocket", "mako")
      begin <- if (scale %in% dark_begin_scales) 0.15 else 0
      colors <- vir_func(n, begin = begin, end = 0.9)
    } else {
      stop(paste("Scale", scale, "not recognized in provided lists."))
    }
  }

  # Adjust brightness
  colors <- brighten_hex(colors, factor = 1.5)

  # Assign names mapping the colors to the specific variable levels
  names(colors) <- cmp_levels

  return(colors)
}

# JS function for conversion process tracking
#' @export
conversion_tracking_js <- "
    var el = document.getElementById('%s');
    var btn = document.getElementById('%s');
    
    el.innerHTML = '';
    btn.style.setProperty('display', 'none', 'important'); 
    var isAutoScrolling = false;

    el.doAutoScroll = function() {
      // Check if user is near bottom
      var isAtBottom = (el.scrollHeight - el.scrollTop - el.clientHeight) <= 150;
      
      if (isAtBottom) {
        isAutoScrolling = true;
        // Instant scroll is more reliable for high-frequency logs, 
        // but if you want smooth, we use a 'double-tap' to ensure it hits bottom.
        el.scrollTo({top: el.scrollHeight, behavior: 'auto'}); 
        
        // Brief timeout to prevent the onscroll event from flickering the button
        setTimeout(function() { isAutoScrolling = false; }, 50);
      }
    };

    el.onscroll = function() {
      if (isAutoScrolling) return;

      // Only scrollable if content is significantly larger than box (e.g. > 30px)
      var isScrollable = el.scrollHeight > (el.clientHeight + 30);
      var isAtBottom = (el.scrollHeight - el.scrollTop - el.clientHeight) <= 50;
      
      if (isScrollable && !isAtBottom) {
        btn.style.display = 'block';
      } else {
        btn.style.display = 'none';
      }
    };

    btn.onclick = function() {
      isAutoScrolling = true;
      el.scrollTo({top: el.scrollHeight, behavior: 'smooth'});
      btn.style.display = 'none';
      setTimeout(function() { isAutoScrolling = false; }, 500);
    };
  "

# Filter RColorBrewer scales by number of distinct colors
#' @export
filter_color_list <- function(color_list, min_n) {
  # Get the RColorBrewer metadata table
  info <- RColorBrewer::brewer.pal.info

  # Process each sub-list (Qualitative, Sequential, etc.)
  filtered_list <- lapply(color_list, function(subgroup) {
    # Filter the names within each subgroup
    Filter(
      function(pal_name) {
        if (pal_name %in% rownames(info)) {
          # If it's a Brewer palette, check maxcolors
          return(info[pal_name, "maxcolors"] >= min_n)
        } else {
          # If it's a Gradient/Viridis palette, they are usually
          # continuous and can support any n. We'll keep them.
          return(TRUE)
        }
      },
      subgroup
    )
  })

  # Drop entirely-empty groups so they don't appear as orphan headers
  Filter(function(g) length(g) > 0, filtered_list)
}

# Palette groups offered for a variable with n distinct levels. Brewer scales
# that cannot supply n colors are dropped; the gradient scales are continuous
# and stay available regardless of n.
#' @export
color_scale_choices <- function(n) {
  scales <- filter_color_list(
    list(
      Qualitative = qualitative_scales,
      Sequential = sequential_scales
    ),
    n
  )
  scales[["Gradient"]] <- gradient_scales
  scales
}

# Palette used until the user picks one: Set3 while it still holds enough
# distinct colors, a continuous scale beyond that.
#' @export
default_color_scale <- function(n) {
  if (n <= RColorBrewer::brewer.pal.info["Set3", "maxcolors"]) {
    "Set3"
  } else {
    "turbo"
  }
}

# Palette a color-scale input should currently use: the user's pick while it
# is still offered for n levels, otherwise the default.
#' @export
resolve_color_scale <- function(scale, n) {
  if (
    length(scale) == 1 &&
      nzchar(scale) &&
      scale %in% unlist(color_scale_choices(n))
  ) {
    scale
  } else {
    default_color_scale(n)
  }
}

# Make compound distribution plot for proteins tab
#' @export
prot_compound_distribution <- function(
  hits_summary,
  protein,
  color_variable,
  truncate_names,
  color_scale,
  distribution_scale,
  distribution_labels = NULL,
  theme = "dark"
) {
  tbl <- hits_summary |>
    dplyr::filter(
      `Protein` == protein &
        !is.na(`Cmp Name`)
    )

  if (color_variable == "Compounds") {
    color <- ~`Cmp Name`
  } else if (color_variable == "Samples") {
    color <- ~`Sample ID`
  }

  # Merge non-preferred hits (same peak) into their preferred counterpart
  tbl <- tbl |>
    # dplyr::arrange(
    #   `Cmp Name`,
    #   `Sample ID`,
    #   `Peak Signal [Da]`,
    #   dplyr::desc(Preferred == "TRUE"),
    #   dplyr::desc(suppressWarnings(as.numeric(`Theor. Cmp [Da]`)))
    # ) |>
    dplyr::group_by(`Cmp Name`, `Sample ID`, `Peak Signal [Da]`) |>
    dplyr::reframe(
      `Protein` = `Protein`[1],
      `Tot. Binding [%]` = `Tot. Binding [%]`[1],
      `truncSample_ID` = `truncSample_ID`[1],
      mass_stoich_raw = paste(
        paste0(
          "[",
          `Theor. Cmp [Da]`,
          "]",
          sapply(`Bind. Stoich.`, function(x) {
            as.character(htmltools::tags$sub(x))
          })
        ),
        collapse = " + "
      ),
      `Theor. Cmp [Da]` = `Theor. Cmp [Da]`[Preferred == "TRUE"][1],
      `Bind. Stoich.` = `Bind. Stoich.`[Preferred == "TRUE"][1],
      `Binding [%]` = {
        pref <- `Binding [%]`[Preferred == "TRUE"]
        if (length(pref) > 0) pref[1] else `Binding [%]`[1]
      }
    ) |>
    dplyr::select(-`Peak Signal [Da]`) |>
    dplyr::arrange(`Cmp Name`, `Tot. Binding [%]`, `Binding [%]`, `Sample ID`)

  .sid_raw <- if (truncate_names) tbl$truncSample_ID else tbl$`Sample ID`
  global_sample_order <- tbl |>
    dplyr::mutate(.sid = .sid_raw) |>
    dplyr::group_by(.sid) |>
    dplyr::summarize(
      .mean_tb = mean(
        as.numeric(as.character(`Tot. Binding [%]`)),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(.mean_tb, .sid) |>
    dplyr::pull(.sid)

  colors <- get_cmp_colorScale(
    filtered_table = if (color_variable == "Compounds") {
      tbl
    } else {
      tbl[order(match(.sid_raw, global_sample_order)), ]
    },
    scale = color_scale,
    variable = color_variable,
    trunc = truncate_names
  )

  tbl <- tbl |>
    dplyr::group_by(`Cmp Name`) |>
    dplyr::mutate(
      `Sample ID` = if (truncate_names) {
        `truncSample_ID`
      } else {
        `Sample ID`
      },
      Group = match(as.character(`Sample ID`), global_sample_order),
      `Cmp Name` = factor(
        `Cmp Name`,
        levels = if (color_variable == "Compounds") {
          names(colors)[names(colors) %in% unique(`Cmp Name`)]
        } else {
          unique(`Cmp Name`)
        }
      ),
      `Sample ID` = factor(
        `Sample ID`,
        levels = global_sample_order
      ),
      `Binding [%]` = factor(
        `Binding [%]`,
        levels = unique(`Binding [%]`)
      ),
      bg_hex = if (color_variable == "Compounds") {
        colors[as.character(`Cmp Name`)]
      } else if (color_variable == "Samples") {
        colors[as.character(`Sample ID`)]
      },
      label_color = get_contrast_color(bg_hex),
      mass_stoich = paste0(
        "<span style='color:",
        label_color,
        "'>",
        mass_stoich_raw,
        "</span>"
      )
    ) |>
    dplyr::ungroup()

  range <- c(
    0,
    max(tbl$`Tot. Binding [%]`) + 10
  )

  if (!is.null(distribution_scale) && distribution_scale == "100") {
    range <- c(0, 101)
  }

  condition <- ifelse(
    length(levels(tbl$`Cmp Name`)) > 1,
    max(nchar(levels(tbl$`Cmp Name`))) <= 22,
    length(levels(tbl$`Sample ID`)) <= 50 |
      max(nchar(levels(tbl$`Sample ID`))) <= 22
  )

  showticklabels <- ifelse(
    !is.null(distribution_labels),
    distribution_labels,
    condition
  )

  axis_color <- if (theme == "light") "black" else "#ffffff"
  grid_color <- if (theme == "light") "rgba(0,0,0,0.2)" else "#7f7f7fff"

  if (length(unique(tbl$`Cmp Name`)) > 1) {
    layout_list <- list(
      barmode = "relative",
      font = list(size = 12, color = axis_color),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      xaxis = list(
        type = "category",
        categoryorder = "array",
        categoryarray = levels(tbl$`Cmp Name`),
        showgrid = FALSE,
        zeroline = FALSE,
        color = axis_color,
        showticklabels = showticklabels
      ),
      yaxis = list(
        range = range,
        title = list(text = "Binding [%]"),
        zeroline = FALSE,
        gridcolor = grid_color,
        color = axis_color,
        dtick = 20,
        tick0 = 0
      )
    )

    groups <- unique(tbl$Group)
    n_groups <- length(groups)
    group_map <- stats::setNames(0:(n_groups - 1), groups)

    compound_local_n <- tbl |>
      dplyr::group_by(`Cmp Name`) |>
      dplyr::summarize(local_n = dplyr::n_distinct(Group), .groups = "drop")
    compound_local_n_map <- stats::setNames(
      compound_local_n$local_n,
      compound_local_n$`Cmp Name`
    )

    max_local_n <- max(compound_local_n$local_n)
    bar_width <- min(0.3, 0.85 / (max_local_n + max(0, max_local_n - 1) * 0.15))
    group_gap <- bar_width * 0.15

    local_i_map <- tbl |>
      dplyr::distinct(`Cmp Name`, Group) |>
      dplyr::arrange(`Cmp Name`, Group) |>
      dplyr::group_by(`Cmp Name`) |>
      dplyr::mutate(local_i = dplyr::row_number() - 1L) |>
      dplyr::ungroup()

    if (n_groups > 1) {
      for (i in 2:n_groups) {
        axis_name <- paste0("yaxis", i)
        layout_list[[axis_name]] <- list(
          visible = FALSE,
          matches = "y",
          overlaying = "y",
          anchor = "x",
          range = range,
          dtick = 20,
          tick0 = 0
        )
      }
    }

    bar_chart <- plotly::plot_ly(showlegend = FALSE)
    bar_chart <- do.call(
      plotly::layout,
      c(list(bar_chart), layout_list)
    )

    for (i in 1:nrow(tbl)) {
      row <- tbl[i, ]
      g <- row$Group[[1]]
      i_group <- group_map[[g]]
      yax <- ifelse(i_group == 0, "y", paste0("y", i_group + 1))
      local_n <- compound_local_n_map[[as.character(row$`Cmp Name`[[1]])]]
      local_cluster_width <- local_n *
        bar_width +
        max(0, local_n - 1) * group_gap
      local_i <- local_i_map$local_i[
        local_i_map$`Cmp Name` == as.character(row$`Cmp Name`[[1]]) &
          local_i_map$Group == g
      ]
      off <- -local_cluster_width / 2 + local_i * (bar_width + group_gap)

      if (color_variable == "Compounds") {
        var <- as.character(row$`Cmp Name`[[1]])
      } else if (color_variable == "Samples") {
        var <- as.character(row$`Sample ID`[[1]])
      }

      col <- colors[[var]]
      y_val <- row$`Binding [%]`[[1]]

      hover_text <- paste0(
        "<span style='opacity: 0.8'>Mass Shift:</span> <b>",
        row$mass_stoich_raw[[1]],
        "</b><br>",
        "<span style='opacity: 0.8'>Binding [%]:</span> <b>",
        sprintf("%.2f", as.numeric(as.character(row$`Binding [%]`[[1]]))),
        "%</b>",
        "<extra><div style='text-align: left;'>",
        "<span style='opacity: 0.8;;'>Cmp Name: </span><b>",
        row$`Cmp Name`[[1]],
        "</b><br>",
        "<span style='opacity: 0.8;'>Sample ID: </span><b>",
        row$`Sample ID`[[1]],
        "</b>",
        "</div></extra>"
      )

      bar_chart <- plotly::add_bars(
        bar_chart,
        x = row$`Cmp Name`[[1]],
        y = as.numeric(as.character(y_val)),
        offsetgroup = local_i,
        offset = off,
        width = bar_width,
        text = row$mass_stoich[[1]],
        textposition = 'inside',
        insidetextanchor = 'middle',
        textfont = list(size = 12),
        hovertemplate = hover_text,
        hoverlabel = list(align = "left", valign = "middle"),
        marker = list(
          color = col,
          line = list(color = axis_color, width = 1)
        ),
        yaxis = yax,
        showlegend = FALSE
      )
    }

    # Total % binding annotation above each (Cmp Name, Sample) bar stack
    totals_cmp_grp <- tbl |>
      dplyr::group_by(`Cmp Name`, Group) |>
      dplyr::summarize(
        total_val = sum(as.numeric(as.character(`Binding [%]`))),
        .groups = "drop"
      )

    cmp_levels <- levels(tbl$`Cmp Name`)

    if (nrow(totals_cmp_grp) <= 30) {
      annots <- vector("list", nrow(totals_cmp_grp))
      for (j in seq_len(nrow(totals_cmp_grp))) {
        tot_row <- totals_cmp_grp[j, ]
        g <- tot_row$Group[[1]]
        local_n <- compound_local_n_map[[as.character(tot_row$`Cmp Name`[[1]])]]
        local_cluster_width <- local_n *
          bar_width +
          max(0, local_n - 1) * group_gap
        local_i <- local_i_map$local_i[
          local_i_map$`Cmp Name` == as.character(tot_row$`Cmp Name`[[1]]) &
            local_i_map$Group == g
        ]
        off <- -local_cluster_width / 2 + local_i * (bar_width + group_gap)
        cmp_idx <- which(cmp_levels == as.character(tot_row$`Cmp Name`[[1]])) -
          1L
        annots[[j]] <- list(
          x = cmp_idx + off + bar_width / 2,
          y = tot_row$total_val[[1]],
          text = paste0(sprintf("%.2f", tot_row$total_val[[1]]), "%"),
          xref = "x",
          yref = "y",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(color = axis_color, size = 12),
          yshift = 4
        )
      }
      bar_chart <- bar_chart |>
        plotly::layout(
          xaxis = list(title = list(text = NULL)),
          annotations = annots
        )
    } else {
      bar_chart <- bar_chart |>
        plotly::layout(xaxis = list(title = list(text = NULL)))
    }
  } else {
    bar_chart <- plotly::plot_ly(data = tbl) |>
      plotly::add_trace(
        x = ~`Sample ID`,
        y = ~ as.numeric(as.character(`Binding [%]`)),
        color = color,
        colors = colors,
        type = 'bar',
        name = ~mass_stoich,
        hovertemplate = ~ paste0(
          "<span style='opacity: 0.8'>Mass Shift / Stoich.:</span> <b>",
          mass_stoich_raw,
          "</b><br>",
          "<span style='opacity: 0.8'>Binding [%]:</span> <b>",
          sprintf("%.2f", as.numeric(as.character(`Binding [%]`))),
          "%</b>",
          "<extra><div style='text-align: left;'>",
          "<span style='opacity: 0.8;;'>Cmp Name: </span><b>",
          `Cmp Name`,
          "</b><br>",
          "<span style='opacity: 0.8;'>Sample ID: </span><b>",
          `Sample ID`,
          "</b>",
          "</div></extra>"
        ),
        hoverlabel = list(align = "left", valign = "middle"),
        text = ~mass_stoich,
        textposition = 'inside',
        textfont = list(size = 12),
        marker = list(line = list(color = axis_color, width = 1)),
        showlegend = FALSE
      )

    if (length(tbl$`Sample ID`) <= 20) {
      totals <- dplyr::group_by(tbl, `Sample ID`) |>
        dplyr::summarize(
          total_val = sum(as.numeric(as.character(`Binding [%]`)))
        )
      bar_chart <- bar_chart |>
        plotly::add_trace(
          data = totals,
          x = ~`Sample ID`,
          y = ~total_val,
          type = 'scatter',
          mode = 'text',
          text = ~ paste0(sprintf("%.2f", total_val), "%"),
          textposition = 'top center',
          showlegend = FALSE,
          hoverinfo = 'none',
          inherit = FALSE,
          textfont = list(
            color = axis_color,
            size = if (length(tbl$`Sample ID`) <= 8) {
              16
            } else if (length(tbl$`Sample ID`) <= 16) {
              14
            } else {
              12
            }
          )
        )
    }

    bar_chart <- bar_chart |>
      plotly::layout(
        barmode = 'stack',
        bargap = 0.5,
        font = list(size = 12, color = axis_color),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        xaxis = list(
          title = list(text = NULL),
          showgrid = FALSE,
          zeroline = FALSE,
          color = axis_color,
          showticklabels = showticklabels
        ),
        yaxis = list(
          range = range,
          title = list(text = "Binding [%]"),
          zeroline = FALSE,
          gridcolor = grid_color,
          color = axis_color
        )
      )
  }

  return(bar_chart)
}

# Make compound distribution plot for compounds tab
#' @export
cmp_compound_distribution <- function(
  hits_summary,
  compound,
  color_variable,
  truncate_names,
  color_scale,
  distribution_scale,
  distribution_labels = NULL,
  theme = "dark"
) {
  tbl <- hits_summary |>
    dplyr::filter(`Cmp Name` == compound)

  # Merge non-preferred hits (same peak) into their preferred counterpart
  tbl <- tbl |>
    dplyr::arrange(
      `Sample ID`,
      `Peak Signal [Da]`,
      dplyr::desc(Preferred == "TRUE"),
      dplyr::desc(suppressWarnings(as.numeric(`Theor. Cmp [Da]`)))
    ) |>
    dplyr::group_by(`Sample ID`, `Peak Signal [Da]`) |>
    dplyr::reframe(
      `Cmp Name` = `Cmp Name`[1],
      `Tot. Binding [%]` = `Tot. Binding [%]`[1],
      `truncSample_ID` = `truncSample_ID`[1],
      mass_stoich_raw = paste(
        paste0(
          "[",
          `Theor. Cmp [Da]`,
          "]",
          sapply(`Bind. Stoich.`, function(x) {
            as.character(htmltools::tags$sub(x))
          })
        ),
        collapse = " + "
      ),
      `Theor. Cmp [Da]` = `Theor. Cmp [Da]`[Preferred == "TRUE"][1],
      `Bind. Stoich.` = `Bind. Stoich.`[Preferred == "TRUE"][1],
      `Binding [%]` = {
        pref <- `Binding [%]`[Preferred == "TRUE"]
        if (length(pref) > 0) pref[1] else `Binding [%]`[1]
      }
    ) |>
    dplyr::select(-`Peak Signal [Da]`) |>
    dplyr::arrange(`Tot. Binding [%]`, `Binding [%]`, `Sample ID`) |>
    dplyr::mutate(
      `Sample ID` = if (truncate_names) `truncSample_ID` else `Sample ID`
    )

  tbl$`Sample ID` <- factor(
    tbl$`Sample ID`,
    levels = unique(tbl$`Sample ID`)
  )

  colors <- get_cmp_colorScale(
    filtered_table = tbl,
    scale = color_scale,
    variable = color_variable,
    trunc = truncate_names
  )

  tbl <- tbl |>
    dplyr::mutate(
      bg_hex = if (color_variable == "Compounds") {
        colors[as.character(`Cmp Name`)]
      } else if (color_variable == "Samples") {
        colors[as.character(`Sample ID`)]
      },
      label_color = get_contrast_color(bg_hex),
      mass_stoich = paste0(
        "<span style='color:",
        label_color,
        "'>",
        mass_stoich_raw,
        "</span>"
      )
    )

  if (color_variable == "Compounds") {
    color <- ~`Cmp Name`
  } else if (color_variable == "Samples") {
    color <- ~`Sample ID`
  }

  axis_color <- if (theme == "light") "black" else "#ffffff"
  grid_color <- if (theme == "light") "rgba(0,0,0,0.2)" else "#7f7f7fff"

  bar_chart <- plotly::plot_ly(data = tbl) |>
    plotly::add_trace(
      x = ~`Sample ID`,
      y = ~`Binding [%]`,
      color = color,
      colors = colors,
      type = 'bar',
      name = ~mass_stoich,
      hovertemplate = ~ paste0(
        "<span style='opacity: 0.8'>Mass Shift:</span> <b>",
        mass_stoich_raw,
        "</b><br>",
        "<span style='opacity: 0.8'>Binding [%]:</span> <b>",
        sprintf("%.2f", `Binding [%]`),
        "%</b>",
        "<extra><div style='text-align: left;'>",
        "<span style='opacity: 0.8;;'>Cmp Name: </span><b>",
        `Cmp Name`,
        "</b><br>",
        "<span style='opacity: 0.8;'>Sample ID: </span><b>",
        `Sample ID`,
        "</b>",
        "</div></extra>"
      ),
      hoverlabel = list(align = "left", valign = "middle"),
      text = ~mass_stoich,
      textposition = 'inside',
      textfont = list(size = 12),
      marker = list(line = list(color = axis_color, width = 1)),
      showlegend = FALSE
    )

  if (length(tbl$`Sample ID`) <= 20) {
    totals <- dplyr::group_by(tbl, `Sample ID`) |>
      dplyr::summarize(
        total_val = sum(`Binding [%]`)
      )
    bar_chart <- bar_chart |>
      plotly::add_trace(
        data = totals,
        x = ~`Sample ID`,
        y = ~total_val,
        type = 'scatter',
        mode = 'text',
        text = ~ paste0(round(total_val, 2), "%"),
        textposition = 'top center',
        showlegend = FALSE,
        hoverinfo = 'none',
        inherit = FALSE,
        textfont = list(
          color = axis_color,
          size = if (length(tbl$`Sample ID`) <= 8) {
            16
          } else if (length(tbl$`Sample ID`) <= 16) {
            14
          } else {
            12
          }
        )
      )
  }

  range <- c(
    0,
    max(tbl$`Tot. Binding [%]`) + 10
  )

  if (!is.null(distribution_scale) && distribution_scale == "100") {
    range <- c(0, 101)
  }

  bar_chart |>
    plotly::layout(
      barmode = 'stack',
      bargap = 0.5,
      font = list(size = 12, color = axis_color),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      xaxis = list(
        title = list(text = NULL),
        showgrid = FALSE,
        zeroline = FALSE,
        color = axis_color,
        showticklabels = if (!is.null(distribution_labels)) {
          distribution_labels
        } else {
          TRUE
        }
      ),
      yaxis = list(
        range = range,
        title = list(text = "Binding [%]"),
        zeroline = FALSE,
        gridcolor = grid_color,
        color = axis_color
      )
    )
}

# Make compound distribution pie chart for samples tab
#' @export
smpl_compound_distribution <- function(
  hits_summary,
  sample,
  color_variable,
  truncate_names,
  color_scale,
  theme = "dark"
) {
  tbl <- hits_summary |>
    dplyr::filter(`Sample ID` == sample)

  if (anyNA(tbl)) {
    return(NULL)
  }

  # Group by compound + peak: multiple stoichiometry interpretations of the
  # same peak are merged into one slice with a combined [x]xN + [y]xM label.
  # Only the Preferred hit's binding value counts for the slice size.
  cmp_table <- tbl |>
    dplyr::arrange(
      `Cmp Name`,
      `Peak Signal [Da]`,
      dplyr::desc(Preferred == "TRUE"),
      dplyr::desc(suppressWarnings(as.numeric(`Theor. Cmp [Da]`)))
    ) |>
    dplyr::group_by(`Cmp Name`, `Peak Signal [Da]`) |>
    dplyr::reframe(
      `Cmp Name` = `Cmp Name`[1],
      `Sample ID` = if (truncate_names) `truncSample_ID`[1] else `Sample ID`[1],
      total_bind = `Tot. Binding [%]`[1],
      mass_stoich = paste(
        paste0(
          "[",
          `Theor. Cmp [Da]`,
          "]",
          sapply(`Bind. Stoich.`, function(x) {
            as.character(htmltools::tags$sub(x))
          })
        ),
        collapse = " + "
      ),
      relBinding = {
        pref <- `Binding [%]`[Preferred == "TRUE"]
        (if (length(pref) > 0) pref[1] else `Binding [%]`[1]) / 100
      }
    ) |>
    dplyr::select(-`Peak Signal [Da]`) |>
    dplyr::mutate(
      `Binding [%]` = paste0(sprintf("%.2f", relBinding * 100), "%")
    ) |>
    rbind(
      data.frame(
        "Cmp Name" = "Unbound",
        "Sample ID" = "Unbound",
        total_bind = 100 - tbl$`Tot. Binding [%]`[1],
        mass_stoich = "Unbound Protein",
        relBinding = 1 - tbl$`Tot. Binding [%]`[1] / 100,
        "Binding [%]" = paste0(
          sprintf("%.2f", 100 - tbl$`Tot. Binding [%]`[1]),
          "%"
        ),
        check.names = FALSE
      )
    )

  colors <- c(
    "#e5e5e5",
    get_cmp_colorScale(
      filtered_table = tbl,
      scale = color_scale,
      variable = color_variable,
      trunc = truncate_names
    )
  )
  names(colors) <- c("empty", names(colors)[-1])

  if (color_variable == "Compounds") {
    cmp_table$color <- colors[match(cmp_table$`Cmp Name`, names(colors))]
  } else {
    cmp_table$color <- colors[match(cmp_table$`Sample ID`, names(colors))]
  }
  cmp_table$color[cmp_table$`Cmp Name` == "Unbound"] <- "#333338"

  font_color <- if (theme == "light") "black" else "white"

  plotly::plot_ly(
    data = cmp_table,
    labels = ~mass_stoich,
    values = ~relBinding,
    sort = FALSE,
    type = 'pie',
    hole = 0.4,
    text = ~`Binding [%]`,
    texttemplate = "%{label}<br>%{text}",
    textposition = 'outside',
    hovertemplate = ~ paste0(
      "<span style='opacity: 0.8'>Compound:</span> <b>",
      `Cmp Name`,
      "</b><br>",
      "<span style='opacity: 0.8'>Mass Shift:</span> <b>",
      `mass_stoich`,
      "</b><br>",
      "<span style='opacity: 0.8'>Binding [%]:</span> <b>",
      `Binding [%]`,
      "<extra></extra>"
    ),
    outsidetextfont = list(color = font_color, size = 12),
    marker = list(
      colors = ~ I(color),
      line = list(color = '#e5e5e5', width = 1)
    )
  ) |>
    plotly::layout(
      showlegend = FALSE,
      autosize = TRUE,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      uniformtext = list(minsize = 8, mode = "hide"),
      margin = list(l = 90, r = 90, t = 60, b = 60),
      annotations = list(
        list(
          x = 0.5,
          y = 0.5,
          text = paste0(
            "<b>",
            sprintf("%.2f", cmp_table$total_bind[1]),
            "%</b><br>Bound"
          ),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "middle",
          showarrow = FALSE,
          font = list(
            size = 15,
            color = font_color
          )
        )
      )
    )
}

stats_palette <- function(n, scale) {
  if (scale %in% unlist(c(qualitative_scales, sequential_scales))) {
    max_colors <- RColorBrewer::brewer.pal.info[scale, "maxcolors"]
    if (n > max_colors) {
      brighten_hex(viridisLite::viridis(n), factor = 1.5)
    } else if (scale %in% sequential_scales) {
      brewer_seq_colors(n, scale, max_colors)
    } else {
      # Qualitative
      n_req <- max(n, 3)
      raw <- RColorBrewer::brewer.pal(n_req, scale)
      if (n == 1) {
        raw[1]
      } else if (n == 2) {
        raw[c(1, 3)]
      } else {
        raw[seq_len(n)]
      }
    }
  } else {
    vir_func <- tryCatch(
      getExportedValue("viridisLite", scale),
      error = function(e) viridisLite::viridis
    )
    dark_begin_scales <- c("magma", "inferno", "rocket", "mako")
    begin <- if (scale %in% dark_begin_scales) 0.15 else 0
    brighten_hex(vir_func(n, begin = begin), factor = 1.5)
  }
}

hex_to_rgba <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

#' @export
stats_histogram <- function(
  hits_summary,
  theme = "dark",
  show = "Correct"
) {
  df <- dplyr::distinct(hits_summary, Sample, .keep_all = TRUE)

  x_col <- if (show == "Unmatched") df$`% Unmatched` else df$`% Correct`
  bin_counts <- tabulate(floor(x_col) + 1L)
  max_count <- if (length(bin_counts) > 0) max(bin_counts) else 1L
  y_dtick <- max(1L, ceiling(max_count / 8))

  font_color <- if (theme == "light") "black" else "white"
  grid_color <- if (theme == "light") {
    "rgba(0,0,0,0.1)"
  } else {
    "rgba(255,255,255,0.2)"
  }
  zeroline_color <- if (theme == "light") {
    "rgba(0,0,0,0.5)"
  } else {
    "rgba(255,255,255,0.5)"
  }

  hex_correct <- "#4daf4a"
  hex_unmatched <- "#e41a1c"
  col <- hex_to_rgba(
    if (show == "Unmatched") hex_unmatched else hex_correct,
    0.7
  )
  colb <- hex_to_rgba(
    if (show == "Unmatched") hex_unmatched else hex_correct,
    1
  )

  p <- plotly::plot_ly()
  if (show == "Unmatched") {
    p <- plotly::add_histogram(
      p,
      data = df,
      x = ~`% Unmatched`,
      name = "Unmatched [%]",
      marker = list(color = col, line = list(color = colb, width = 0.5)),
      xbins = list(start = 0, end = 101, size = 1)
    )
  } else {
    p <- plotly::add_histogram(
      p,
      data = df,
      x = ~`% Correct`,
      name = "Correct [%]",
      marker = list(color = col, line = list(color = colb, width = 0.5)),
      xbins = list(start = 0, end = 101, size = 1)
    )
  }
  p |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(size = 14, color = font_color),
      legend = list(
        bgcolor = "rgba(0,0,0,0)",
        font = list(color = font_color)
      ),
      xaxis = list(
        title = paste(show, "[%]"),
        range = c(-2, 102),
        color = font_color,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color,
        hoverformat = ".0f"
      ),
      yaxis = list(
        title = "Count",
        color = font_color,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color,
        tick0 = 0,
        dtick = y_dtick,
        hoverformat = "d"
      )
    )
}

#' @export
stats_boxplot <- function(
  hits_summary,
  theme = "dark",
  show_points = TRUE,
  show = "Correct",
  fixed_range = TRUE
) {
  df <- dplyr::distinct(hits_summary, Sample, .keep_all = TRUE)

  font_color <- if (theme == "light") "black" else "white"
  grid_color <- if (theme == "light") {
    "rgba(0,0,0,0.1)"
  } else {
    "rgba(255,255,255,0.2)"
  }
  zeroline_color <- if (theme == "light") {
    "rgba(0,0,0,0.5)"
  } else {
    "rgba(255,255,255,0.5)"
  }
  dot_border_color <- if (theme == "light") {
    "rgba(0,0,0,0.5)"
  } else {
    "rgba(255,255,255,0.5)"
  }

  hex_base <- if (show == "Unmatched") "#e41a1c" else "#4daf4a"
  show_unmatched <- show == "Unmatched"
  show_correct <- show == "Correct"
  box_col <- hex_to_rgba(hex_base, 1)
  box_fill <- hex_to_rgba(hex_base, 0.15)
  box_y <- if (show_unmatched) ~`% Unmatched` else ~`% Correct`
  box_name <- if (show_unmatched) "Unmatched [%]" else "Correct [%]"
  hover_tmpl <- if (show_unmatched) {
    "<b>%{text}</b><br>Unmatched [%]: %{y:.2f}<extra></extra>"
  } else {
    "<b>%{text}</b><br>Correct [%]: %{y:.2f}<extra></extra>"
  }

  # box_half_width = 0.3 gives box width = 0.6, which is exactly 50% of x range c(-0.4, 0.8)
  box_half_width <- 0.3
  metric_vals_bp <- if (show_unmatched) df$`% Unmatched` else df$`% Correct`

  # Pre-compute with R's quantile() for the annotations and box shapes
  q1_bp <- unname(stats::quantile(metric_vals_bp, 0.25, na.rm = TRUE))
  med_bp <- stats::median(metric_vals_bp, na.rm = TRUE)
  mean_bp <- mean(metric_vals_bp, na.rm = TRUE)
  q3_bp <- unname(stats::quantile(metric_vals_bp, 0.75, na.rm = TRUE))
  iqr_bp <- q3_bp - q1_bp
  lo_fence_bp <- min(metric_vals_bp[metric_vals_bp >= q1_bp - 1.5 * iqr_bp])
  hi_fence_bp <- max(metric_vals_bp[metric_vals_bp <= q3_bp + 1.5 * iqr_bp])

  # Box is drawn as layout shapes so Plotly's quartile algorithm is bypassed entirely
  whisk_half <- box_half_width * 0.5
  box_shapes <- list(
    list(
      type = "rect",
      xref = "x",
      yref = "y",
      x0 = -box_half_width,
      x1 = box_half_width,
      y0 = q1_bp,
      y1 = q3_bp,
      fillcolor = box_fill,
      line = list(color = box_col, width = 1.5)
    ),
    list(
      type = "line",
      xref = "x",
      yref = "y",
      x0 = -box_half_width,
      x1 = box_half_width,
      y0 = med_bp,
      y1 = med_bp,
      line = list(color = box_col, width = 2)
    ),
    list(
      type = "line",
      xref = "x",
      yref = "y",
      x0 = -box_half_width,
      x1 = box_half_width,
      y0 = mean_bp,
      y1 = mean_bp,
      line = list(color = box_col, width = 1.5, dash = "dot")
    ),
    list(
      type = "line",
      xref = "x",
      yref = "y",
      x0 = 0,
      x1 = 0,
      y0 = q3_bp,
      y1 = hi_fence_bp,
      line = list(color = box_col, width = 1.5)
    ),
    list(
      type = "line",
      xref = "x",
      yref = "y",
      x0 = -whisk_half,
      x1 = whisk_half,
      y0 = hi_fence_bp,
      y1 = hi_fence_bp,
      line = list(color = box_col, width = 1.5)
    ),
    list(
      type = "line",
      xref = "x",
      yref = "y",
      x0 = 0,
      x1 = 0,
      y0 = lo_fence_bp,
      y1 = q1_bp,
      line = list(color = box_col, width = 1.5)
    ),
    list(
      type = "line",
      xref = "x",
      yref = "y",
      x0 = -whisk_half,
      x1 = whisk_half,
      y0 = lo_fence_bp,
      y1 = lo_fence_bp,
      line = list(color = box_col, width = 1.5)
    )
  )

  if (show_points) {
    x_jit <- stats::runif(nrow(df), -box_half_width, box_half_width)
    p <- plotly::plot_ly(
      type = "scatter",
      mode = "markers",
      x = x_jit,
      y = metric_vals_bp,
      text = df$Sample,
      customdata = df$Sample,
      hovertemplate = hover_tmpl,
      showlegend = FALSE,
      hoverlabel = list(
        bgcolor = if (theme == "dark") {
          "rgba(50,52,60,0.95)"
        } else {
          "rgba(255,255,255,0.9)"
        },
        bordercolor = if (theme == "dark") {
          "rgba(200,200,200,0.4)"
        } else {
          "rgba(0,0,0,0.2)"
        },
        font = list(
          color = if (theme == "dark") "#ffffff" else "#000000",
          size = 12
        )
      ),
      marker = list(
        color = "rgba(0,0,0,0)",
        size = 8,
        line = list(color = dot_border_color, width = 1.5)
      )
    )
  } else {
    p <- plotly::plot_ly(
      type = "scatter",
      mode = "markers",
      x = numeric(0),
      y = numeric(0),
      showlegend = FALSE,
      hoverinfo = "none"
    )
  }

  correct_sd_bp <- stats::sd(df$`% Correct`, na.rm = TRUE)
  unmatched_sd_bp <- stats::sd(df$`% Unmatched`, na.rm = TRUE)
  correct_mean_bp <- mean(df$`% Correct`, na.rm = TRUE)
  unmatched_mean_bp <- mean(df$`% Unmatched`, na.rm = TRUE)

  bp_annots <- list()
  if (show_correct && !is.na(correct_sd_bp) && correct_sd_bp < 0.005) {
    bp_annots <- c(
      bp_annots,
      list(list(
        x = 0,
        xref = "x",
        y = correct_mean_bp - 4,
        yref = "y",
        text = sprintf("All values: %.2f%%", correct_mean_bp),
        showarrow = FALSE,
        yanchor = "top",
        font = list(color = font_color, size = 11)
      ))
    )
  }
  if (show_unmatched && !is.na(unmatched_sd_bp) && unmatched_sd_bp < 0.005) {
    bp_annots <- c(
      bp_annots,
      list(list(
        x = 0,
        xref = "x",
        y = unmatched_mean_bp - 4,
        yref = "y",
        text = sprintf("All values: %.2f%%", unmatched_mean_bp),
        showarrow = FALSE,
        yanchor = "top",
        font = list(color = font_color, size = 11)
      ))
    )
  }

  sa_font <- list(color = font_color, size = 10)
  # Spread all four label y-positions by actual value with a minimum gap
  spread_ys <- local({
    min_gap <- 5
    raw <- c(q1_bp, med_bp, mean_bp, q3_bp)
    idx <- order(raw)
    s <- raw[idx]
    for (iter in seq_len(50)) {
      changed <- FALSE
      for (i in seq_len(3)) {
        if (s[i + 1] - s[i] < min_gap) {
          mid <- (s[i] + s[i + 1]) / 2
          s[i] <- mid - min_gap / 2
          s[i + 1] <- mid + min_gap / 2
          changed <- TRUE
        }
      }
      if (!changed) break
    }
    result <- raw
    result[idx] <- s
    result
  })
  q1_y <- spread_ys[1]
  med_y <- spread_ys[2]
  mean_y <- spread_ys[3]
  q3_y <- spread_ys[4]

  bp_annots <- c(
    bp_annots,
    list(
      list(
        x = 0.65,
        xref = "paper",
        y = q1_y,
        yref = "y",
        text = sprintf("Q1: %.2f%%", q1_bp),
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "middle",
        font = sa_font
      ),
      list(
        x = 0.65,
        xref = "paper",
        y = med_y,
        yref = "y",
        text = sprintf("Median: %.2f%%", med_bp),
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "middle",
        font = sa_font
      ),
      list(
        x = 0.65,
        xref = "paper",
        y = mean_y,
        yref = "y",
        text = sprintf("Mean: %.2f%%", mean_bp),
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "middle",
        font = sa_font
      ),
      list(
        x = 0.65,
        xref = "paper",
        y = q3_y,
        yref = "y",
        text = sprintf("Q3: %.2f%%", q3_bp),
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "middle",
        font = sa_font
      )
    )
  )

  p |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(size = 14, color = font_color),
      legend = list(bgcolor = "rgba(0,0,0,0)", font = list(color = font_color)),
      hoverlabel = if (theme == "dark") {
        list(
          bgcolor = "rgba(50,52,60,0.95)",
          bordercolor = "rgba(200,200,200,0.4)",
          font = list(color = "#ffffff", size = 12)
        )
      } else {
        list()
      },
      xaxis = list(
        showticklabels = FALSE,
        color = font_color,
        range = c(-0.4, 0.8),
        gridcolor = "rgba(0,0,0,0)",
        zerolinecolor = "rgba(0,0,0,0)"
      ),
      yaxis = list(
        title = paste(show, "[%]"),
        range = if (fixed_range) c(-5, 105) else NULL,
        color = font_color,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color,
        hoverformat = ".2f"
      ),
      annotations = if (length(bp_annots) > 0) bp_annots else NULL,
      shapes = box_shapes
    )
}

#' @export
stats_scatter <- function(
  hits_summary,
  full_scale = FALSE,
  group_by = NULL,
  color_scale = "plasma",
  theme = "dark",
  show = "Correct"
) {
  metric_col <- if (show == "Unmatched") "% Unmatched" else "% Correct"
  metric_label <- if (show == "Unmatched") "Unmatched [%]" else "Correct [%]"

  df <- dplyr::distinct(hits_summary, Sample, .keep_all = TRUE)
  df$`Total % Binding` <- ifelse(
    is.na(df$`Total % Binding`),
    0,
    df$`Total % Binding`
  ) *
    100

  font_color <- if (theme == "light") "black" else "white"
  grid_color <- if (theme == "light") {
    "rgba(0,0,0,0.1)"
  } else {
    "rgba(255,255,255,0.2)"
  }
  zeroline_color <- if (theme == "light") {
    "rgba(0,0,0,0.5)"
  } else {
    "rgba(255,255,255,0.5)"
  }
  dot_border_color <- if (theme == "light") {
    "rgba(0,0,0,0.5)"
  } else {
    "rgba(255,255,255,0.5)"
  }

  metric_vals <- df[[metric_col]]
  correct_mean <- mean(metric_vals, na.rm = TRUE)
  correct_sd <- stats::sd(metric_vals, na.rm = TRUE)
  correct_upper <- correct_mean + correct_sd
  correct_lower <- correct_mean - correct_sd
  show_sd <- !is.na(correct_sd) && correct_sd >= 0.005

  y_range <- if (full_scale) {
    c(-5, 105)
  } else {
    pad <- max(
      (max(metric_vals, na.rm = TRUE) - min(metric_vals, na.rm = TRUE)) *
        0.08,
      3
    )
    c(
      max(-5, floor(min(metric_vals, na.rm = TRUE) - pad)),
      min(105, ceiling(max(metric_vals, na.rm = TRUE) + pad))
    )
  }

  group_by <- if (
    is.null(group_by) || !nzchar(group_by) || !group_by %in% names(df)
  ) {
    if ("Protein" %in% names(df)) "Protein" else names(df)[1]
  } else {
    group_by
  }

  df <- df[!is.na(df[[group_by]]), ]
  groups <- as.character(unique(df[[group_by]]))
  df[[group_by]] <- as.character(df[[group_by]])
  color_map <- stats::setNames(
    stats_palette(length(groups), color_scale),
    groups
  )

  p <- plotly::plot_ly()
  for (grp in groups) {
    sub_df <- dplyr::filter(df, .data[[group_by]] == grp)
    sub_df <- sub_df |>
      dplyr::mutate(
        x_plot = `Total % Binding` + stats::runif(dplyr::n(), -0.6, 0.6),
        y_plot = .data[[metric_col]] + stats::runif(dplyr::n(), -0.6, 0.6),
        tooltip = paste0(
          Sample,
          "<br>",
          "Tot. Binding [%]: ",
          round(`Total % Binding`, 2),
          "<br>",
          metric_label,
          ": ",
          round(.data[[metric_col]], 2)
        )
      )
    p <- plotly::add_markers(
      p,
      data = sub_df,
      x = ~x_plot,
      y = ~y_plot,
      name = grp,
      showlegend = TRUE,
      customdata = I(sub_df$Sample),
      text = I(sub_df$tooltip),
      hovertemplate = "%{text}<extra></extra>",
      marker = list(
        color = color_map[[grp]],
        size = 8,
        opacity = 0.8,
        line = list(color = dot_border_color, width = 1)
      )
    )
  }

  ref_color <- if (theme == "light") {
    "rgba(0,0,0,0.7)"
  } else {
    "rgba(255,255,255,0.7)"
  }
  sd_color <- if (theme == "light") {
    "rgba(0,0,0,0.7)"
  } else {
    "rgba(255,255,255,0.7)"
  }

  p |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(size = 14, color = font_color),
      showlegend = TRUE,
      legend = list(
        bgcolor = "rgba(0,0,0,0)",
        font = list(color = font_color)
      ),
      xaxis = list(
        title = "Tot. Binding [%]",
        color = font_color,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color,
        hoverformat = ".2f"
      ),
      yaxis = list(
        title = metric_label,
        range = y_range,
        color = font_color,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color,
        hoverformat = ".2f"
      ),
      shapes = c(
        list(list(
          type = "line",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = correct_mean,
          y1 = correct_mean,
          line = list(color = ref_color, width = 1.5, dash = "dash")
        )),
        if (show_sd) {
          list(
            list(
              type = "line",
              x0 = 0,
              x1 = 1,
              xref = "paper",
              y0 = correct_upper,
              y1 = correct_upper,
              line = list(color = sd_color, width = 1, dash = "dot")
            ),
            list(
              type = "line",
              x0 = 0,
              x1 = 1,
              xref = "paper",
              y0 = correct_lower,
              y1 = correct_lower,
              line = list(color = sd_color, width = 1, dash = "dot")
            )
          )
        } else {
          list()
        }
      ),
      annotations = c(
        list(list(
          x = 1,
          xref = "paper",
          y = correct_mean,
          yref = "y",
          text = sprintf("<b>Mean</b> %.2f%%", correct_mean),
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "bottom",
          font = list(color = font_color, size = 11)
        )),
        if (show_sd) {
          list(
            list(
              x = 1,
              xref = "paper",
              y = correct_upper,
              yref = "y",
              text = sprintf("+1 SD  %.2f%%", correct_upper),
              showarrow = FALSE,
              xanchor = "right",
              yanchor = "bottom",
              font = list(color = font_color, size = 10)
            ),
            list(
              x = 1,
              xref = "paper",
              y = correct_lower,
              yref = "y",
              text = sprintf("− 1 SD  %.2f%%", correct_lower),
              showarrow = FALSE,
              xanchor = "right",
              yanchor = "top",
              font = list(color = font_color, size = 10)
            )
          )
        } else {
          list()
        }
      )
    )
}

#' @export
stats_violin <- function(
  hits_summary,
  group_by = "Protein",
  full_scale = FALSE,
  theme = "dark",
  color_scale = "plasma",
  inner = "box",
  show = "Correct"
) {
  metric_col <- if (show == "Unmatched") "% Unmatched" else "% Correct"
  metric_label <- if (show == "Unmatched") "Unmatched [%]" else "Correct [%]"
  hover_label <- if (show == "Unmatched") "Unmatched [%]" else "Correct [%]"

  df <- dplyr::distinct(hits_summary, Sample, .keep_all = TRUE)
  df[[group_by]] <- ifelse(
    is.na(df[[group_by]]),
    "Unknown",
    as.character(df[[group_by]])
  )

  font_color <- if (theme == "light") "black" else "white"
  grid_color <- if (theme == "light") {
    "rgba(0,0,0,0.1)"
  } else {
    "rgba(255,255,255,0.2)"
  }
  zeroline_color <- if (theme == "light") {
    "rgba(0,0,0,0.5)"
  } else {
    "rgba(255,255,255,0.5)"
  }
  dot_border_color <- if (theme == "light") {
    "rgba(0,0,0,0.5)"
  } else {
    "rgba(255,255,255,0.5)"
  }
  box_line_color <- if (theme == "light") {
    "rgba(0,0,0,1)"
  } else {
    "rgba(255,255,255,1)"
  }

  show_box <- inner != "Points"
  show_points <- inner == "Points"

  y_range <- if (full_scale) c(-5, 105) else NULL

  groups <- unique(df[[group_by]])
  pal <- stats_palette(length(groups), color_scale)
  color_map <- stats::setNames(pal, groups)

  p <- plotly::plot_ly()
  for (i in seq_along(groups)) {
    grp <- groups[[i]]
    cat_idx <- i - 1L
    sub_df <- dplyr::filter(df, .data[[group_by]] == grp)
    col <- hex_to_rgba(color_map[[grp]], 1)
    col_f <- hex_to_rgba(color_map[[grp]], 0.2)
    col_b <- hex_to_rgba(color_map[[grp]], 0.25)
    grp_sd <- stats::sd(sub_df[[metric_col]], na.rm = TRUE)
    is_degenerate <- is.na(grp_sd) || grp_sd < 0.005
    hover_tmpl_v <- paste0(
      "<b>%{text}</b><br>",
      hover_label,
      ": %{y:.2f}<extra></extra>"
    )
    if (is_degenerate) {
      # Single-member groups stay centered; multi-member degenerate groups get jitter
      x_jit <- if (nrow(sub_df) == 1) {
        cat_idx
      } else {
        cat_idx + stats::runif(nrow(sub_df), -0.25, 0.25)
      }
      p <- plotly::add_trace(
        p,
        type = "scatter",
        mode = "markers",
        x = x_jit,
        y = sub_df[[metric_col]],
        text = I(sub_df$Sample),
        customdata = I(sub_df$Sample),
        hovertemplate = hover_tmpl_v,
        name = grp,
        showlegend = FALSE,
        marker = list(
          color = col,
          size = 8,
          opacity = 0.9,
          line = list(color = dot_border_color, width = 1)
        )
      )
    } else {
      # Violin shape: visual only, excluded from hit detection
      p <- suppressWarnings(plotly::add_trace(
        p,
        data = sub_df,
        type = "violin",
        x = rep(cat_idx, nrow(sub_df)),
        y = sub_df[[metric_col]],
        name = grp,
        hoverinfo = "skip",
        fillcolor = col_f,
        line = list(color = col, width = 1),
        box = list(
          visible = show_box,
          fillcolor = col_b,
          line = list(color = box_line_color, width = 1)
        ),
        meanline = list(visible = show_box, color = box_line_color, width = 1),
        points = FALSE,
        showlegend = FALSE
      ))
      if (show_points) {
        # Scatter: manually jittered — visual AND hit targets
        x_jit <- cat_idx + stats::runif(nrow(sub_df), -0.25, 0.25)
        p <- plotly::add_trace(
          p,
          type = "scatter",
          mode = "markers",
          x = x_jit,
          y = sub_df[[metric_col]],
          text = sub_df$Sample,
          customdata = sub_df$Sample,
          hovertemplate = hover_tmpl_v,
          showlegend = FALSE,
          marker = list(
            color = col,
            size = 7,
            opacity = 0.9,
            line = list(color = dot_border_color, width = 1)
          )
        )
      }
    }
  }

  p |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(size = 14, color = font_color),
      xaxis = list(
        title = group_by,
        tickmode = "array",
        tickvals = as.list(seq_along(groups) - 1L),
        ticktext = as.list(groups),
        range = c(-0.5, length(groups) - 0.5),
        color = font_color,
        gridcolor = grid_color,
        zeroline = FALSE
      ),
      yaxis = list(
        title = metric_label,
        range = y_range,
        color = font_color,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color,
        hoverformat = ".2f"
      )
    )
}

# batch_plate_heatmap(): Interactive well plate heatmap for batch control ----
#' @export
batch_plate_heatmap <- function(
  hits_summary,
  variable = "Total % Binding",
  color_scale = "plasma",
  scale_mode = "minmax",
  theme = "dark"
) {
  # Only Total % Binding is stored as 0-1 fraction; % Correct / % Unmatched are 0-100
  fraction_vars <- c("Total % Binding")
  pct_vars <- c("Total % Binding", "% Correct", "% Unmatched")
  numeric_vars <- c(
    "Total % Binding",
    "% Correct",
    "% Unmatched",
    "Concentration"
  )

  font_color <- if (theme == "light") "black" else "white"
  paper_bg <- if (theme == "light") "#f0f0f5" else "#23252e"
  tile_bg <- "rgba(190,192,200,0.38)"

  df <- dplyr::distinct(hits_summary, Sample, .keep_all = TRUE)
  if (!variable %in% names(df)) {
    return(plotly::plotly_empty())
  }

  if (
    !"Well" %in% names(df) ||
      all(is.na(df[["Well"]])) ||
      all(trimws(as.character(df[["Well"]])) %in% c("", "NA", "N/A"))
  ) {
    return(plotly::plotly_empty())
  }

  # Classify each sample's well state (use df = one row per sample)
  no_prot_flag <- is.na(df$`Measured Mw Protein [Da]`) &
    is.na(df$Compound)
  no_hit_flag <- !is.na(df$`Measured Mw Protein [Da]`) &
    is.na(df$Compound)
  well_state <- dplyr::case_when(
    no_prot_flag ~ "no_prot",
    no_hit_flag ~ "no_hit",
    TRUE ~ "normal"
  )

  raw_vals <- df[[variable]]
  is_numeric_var <- variable %in% numeric_vars

  cat_levels <- NULL
  if (!is_numeric_var) {
    cat_levels <- sort(unique(stats::na.omit(as.character(raw_vals))))
    values <- as.numeric(factor(as.character(raw_vals), levels = cat_levels))
    display_vals <- as.character(raw_vals)
  } else {
    values <- as.numeric(raw_vals)
    if (variable %in% fraction_vars) {
      values <- values * 100
    }
    # No-hit wells have 0 binding for numeric variables
    values[no_hit_flag & is.na(values)] <- 0
    display_vals <- if (variable %in% pct_vars) {
      sprintf("%.2f%%", values)
    } else {
      sprintf("%.2f", values)
    }
  }

  # Normalize well IDs: "A01" → "A1", "B12" → "B12"
  raw_wells <- toupper(as.character(df[["Well"]]))
  norm_wells <- gsub("^([A-Z]+)0*(\\d+)$", "\\1\\2", raw_wells)

  data <- data.frame(
    well_id = norm_wells,
    value = values,
    display = display_vals,
    sample = df[["Sample"]],
    well_state = well_state,
    stringsAsFactors = FALSE
  )

  rows <- LETTERS[1:16]
  cols <- 1:24
  plate_layout <- expand.grid(
    row = rows,
    col = cols,
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(well_id = paste0(row, col))
  plate_data <- dplyr::left_join(plate_layout, data, by = "well_id")

  z_mat <- matrix(
    NA_real_,
    nrow = 16,
    ncol = 24,
    dimnames = list(rows, as.character(cols))
  )
  text_mat <- matrix(
    "",
    nrow = 16,
    ncol = 24,
    dimnames = list(rows, as.character(cols))
  )
  presence_mat <- matrix(
    FALSE,
    nrow = 16,
    ncol = 24,
    dimnames = list(rows, as.character(cols))
  )
  no_prot_col_vals <- integer(0)
  no_prot_row_vals <- character(0)
  no_hit_col_vals <- integer(0)
  no_hit_row_vals <- character(0)

  for (r in rows) {
    for (c in cols) {
      wid <- paste0(r, c)
      d <- plate_data[plate_data$well_id == wid, ]
      ws <- if (!is.na(d$well_state[1])) d$well_state[1] else "empty"

      if (ws != "empty") {
        presence_mat[r, as.character(c)] <- TRUE
      }

      if (!is.na(d$value[1])) {
        if (ws != "no_prot") {
          z_mat[r, as.character(c)] <- d$value[1]
        }
        hover_txt <- paste0(
          "Well: ",
          wid,
          "<br>Sample: ",
          d$sample[1],
          "<br>",
          variable,
          ": ",
          d$display[1]
        )
        if (ws == "no_prot") {
          hover_txt <- paste0(hover_txt, "<br>No protein peak detected")
          no_prot_col_vals <- c(no_prot_col_vals, c)
          no_prot_row_vals <- c(no_prot_row_vals, r)
        } else if (ws == "no_hit") {
          hover_txt <- paste0(hover_txt, "<br>No compound binding")
          no_hit_col_vals <- c(no_hit_col_vals, c)
          no_hit_row_vals <- c(no_hit_row_vals, r)
        }
        text_mat[r, as.character(c)] <- hover_txt
      } else if (ws == "no_prot") {
        hover_txt <- paste0(
          "Well: ",
          wid,
          "<br>Sample: ",
          d$sample[1],
          "<br>No protein peak detected"
        )
        text_mat[r, as.character(c)] <- hover_txt
        no_prot_col_vals <- c(no_prot_col_vals, c)
        no_prot_row_vals <- c(no_prot_row_vals, r)
      } else if (ws == "no_hit") {
        hover_txt <- paste0(
          "Well: ",
          wid,
          "<br>Sample: ",
          d$sample[1],
          "<br>No compound binding"
        )
        text_mat[r, as.character(c)] <- hover_txt
        no_hit_col_vals <- c(no_hit_col_vals, c)
        no_hit_row_vals <- c(no_hit_row_vals, r)
      } else {
        text_mat[r, as.character(c)] <- paste0("Well: ", wid, "<br>Empty")
      }
    }
  }

  # Trim to bounding rectangle of all wells present in dataset
  has_empty_in_rect <- FALSE
  used_row_idx <- which(rowSums(presence_mat) > 0)
  used_col_idx <- which(colSums(presence_mat) > 0)
  if (length(used_row_idx) > 0 && length(used_col_idx) > 0) {
    row_range <- seq(min(used_row_idx), max(used_row_idx))
    col_range <- seq(min(used_col_idx), max(used_col_idx))
    display_rows <- rows[row_range]
    display_cols <- cols[col_range]
    z_mat <- z_mat[row_range, col_range, drop = FALSE]
    text_mat <- text_mat[row_range, col_range, drop = FALSE]
    has_empty_in_rect <- any(!presence_mat[row_range, col_range])
    keep_np <- no_prot_row_vals %in%
      display_rows &
      no_prot_col_vals %in% display_cols
    no_prot_row_vals <- no_prot_row_vals[keep_np]
    no_prot_col_vals <- no_prot_col_vals[keep_np]
    keep_nh <- no_hit_row_vals %in%
      display_rows &
      no_hit_col_vals %in% display_cols
    no_hit_row_vals <- no_hit_row_vals[keep_nh]
    no_hit_col_vals <- no_hit_col_vals[keep_nh]
    rows <- display_rows
    cols <- display_cols
  }

  tick_vals <- NULL
  tick_text <- NULL

  if (is_numeric_var) {
    full_scale <- variable %in% pct_vars && scale_mode == "min100"
    zmin <- if (full_scale) 0 else min(z_mat, na.rm = TRUE)
    zmax <- if (full_scale) 100 else max(z_mat, na.rm = TRUE)
    if (!is.finite(zmin) || !is.finite(zmax) || zmin == zmax) {
      zmin <- 0
      zmax <- 1
    }
    n_stops <- 9
    pal <- stats_palette(n_stops, color_scale)
    cs <- lapply(seq_len(n_stops), function(i) {
      list((i - 1) / (n_stops - 1), pal[i])
    })
    show_scale <- TRUE
    cb_title <- variable
  } else {
    n_cats <- length(cat_levels)
    pal <- stats_palette(max(n_cats, 2), color_scale)
    if (n_cats <= 1) {
      cs <- list(list(0, pal[1]), list(1, pal[1]))
    } else {
      cs <- lapply(seq_len(n_cats), function(i) {
        list((i - 1) / (n_cats - 1), pal[i])
      })
    }
    zmin <- 1
    zmax <- max(n_cats, 2)
    show_scale <- FALSE
    cb_title <- variable
  }

  # Position colorbar below the Well States legend group when it has entries
  n_ws_items <- as.integer(length(no_prot_row_vals) > 0) +
    as.integer(length(no_hit_row_vals) > 0) +
    as.integer(has_empty_in_rect)
  cb_y <- if (is_numeric_var && n_ws_items > 0L) {
    max(0.05, 1.0 - (n_ws_items + 1L) * 0.08 - 0.04)
  } else {
    0.5
  }

  p <- plotly::plot_ly(
    z = z_mat,
    x = cols,
    y = rows,
    type = "heatmap",
    colorscale = cs,
    showscale = show_scale,
    zmin = zmin,
    zmax = zmax,
    xgap = 3,
    ygap = 3,
    text = text_mat,
    hoverinfo = "text",
    colorbar = list(
      title = list(text = cb_title, font = list(color = font_color, size = 13)),
      tickfont = list(color = font_color, size = 11),
      outlinecolor = font_color,
      y = cb_y,
      yanchor = "top"
    )
  )

  if (!is_numeric_var && !is.null(cat_levels) && length(cat_levels) > 0) {
    n_cats <- length(cat_levels)
    pal <- stats_palette(max(n_cats, 2), color_scale)
    for (i in seq_len(n_cats)) {
      p <- p |>
        plotly::add_trace(
          type = "scatter",
          x = min(cols) - 10000,
          y = rows[1],
          mode = "markers",
          visible = TRUE,
          marker = list(
            symbol = "square",
            color = pal[i],
            size = 10,
            line = list(width = 0)
          ),
          name = cat_levels[i],
          showlegend = TRUE,
          inherit = FALSE,
          hoverinfo = "skip",
          legendrank = 200,
          legendgroup = "categories",
          legendgrouptitle = list(
            text = cb_title,
            font = list(color = font_color, size = 13)
          )
        )
    }
  }

  has_no_prot <- length(no_prot_row_vals) > 0
  tile_px_est <- min(480 / length(cols), 360 / length(rows))
  d_np <- as.integer(max(4L, min(11L, round(tile_px_est * 0.24))))
  d_nh <- as.integer(max(4L, min(12L, round(tile_px_est * 0.27))))
  np_shapes <- if (has_no_prot) {
    d <- d_np
    unlist(
      lapply(seq_along(no_prot_col_vals), function(i) {
        cx <- no_prot_col_vals[i]
        cy <- which(rows == no_prot_row_vals[i]) - 1L
        list(
          list(
            type = "line",
            xref = "x",
            yref = "y",
            xsizemode = "pixel",
            ysizemode = "pixel",
            xanchor = cx,
            yanchor = cy,
            x0 = -d,
            y0 = -d,
            x1 = d,
            y1 = d,
            line = list(color = "black", width = 2.5)
          ),
          list(
            type = "line",
            xref = "x",
            yref = "y",
            xsizemode = "pixel",
            ysizemode = "pixel",
            xanchor = cx,
            yanchor = cy,
            x0 = -d,
            y0 = d,
            x1 = d,
            y1 = -d,
            line = list(color = "black", width = 2.5)
          )
        )
      }),
      recursive = FALSE
    )
  } else {
    list()
  }
  if (has_no_prot) {
    p <- p |>
      plotly::add_trace(
        type = "scatter",
        mode = "markers",
        x = min(cols) - 10000,
        y = rows[1],
        visible = TRUE,
        marker = list(
          symbol = "x-thin",
          size = 10,
          color = "rgba(0,0,0,0)",
          line = list(color = font_color, width = 2.5)
        ),
        name = "No Protein Peak",
        showlegend = TRUE,
        inherit = FALSE,
        hoverinfo = "skip",
        legendrank = 100,
        legendgroup = "well_states",
        legendgrouptitle = list(
          text = "Well States",
          font = list(color = font_color, size = 13)
        )
      )
  }

  has_no_hit <- length(no_hit_col_vals) > 0
  nh_shapes <- if (has_no_hit) {
    d <- d_nh
    lapply(seq_along(no_hit_col_vals), function(i) {
      cx <- no_hit_col_vals[i]
      cy <- which(rows == no_hit_row_vals[i]) - 1L
      list(
        type = "circle",
        xref = "x",
        yref = "y",
        xsizemode = "pixel",
        ysizemode = "pixel",
        xanchor = cx,
        yanchor = cy,
        x0 = -d,
        y0 = -d,
        x1 = d,
        y1 = d,
        fillcolor = "rgba(0,0,0,0)",
        line = list(color = "black", width = 2)
      )
    })
  } else {
    list()
  }
  if (has_no_hit) {
    p <- p |>
      plotly::add_trace(
        type = "scatter",
        mode = "markers",
        x = min(cols) - 10000,
        y = rows[1],
        visible = TRUE,
        marker = list(
          symbol = "circle-open",
          size = 10,
          color = font_color,
          line = list(color = font_color, width = 2)
        ),
        name = "No Compound Binding",
        showlegend = TRUE,
        inherit = FALSE,
        hoverinfo = "skip",
        legendrank = 100,
        legendgroup = "well_states",
        legendgrouptitle = list(
          text = "Well States",
          font = list(color = font_color, size = 13)
        )
      )
  }

  # Empty well: legend swatch
  if (has_empty_in_rect) {
    p <- p |>
      plotly::add_trace(
        type = "scatter",
        mode = "markers",
        x = min(cols) - 10000,
        y = rows[1],
        visible = TRUE,
        marker = list(
          symbol = "square",
          size = 10,
          color = tile_bg,
          line = list(color = "rgba(130,132,140,0.6)", width = 1)
        ),
        name = "Empty",
        showlegend = TRUE,
        inherit = FALSE,
        hoverinfo = "skip",
        legendrank = 100,
        legendgroup = "well_states",
        legendgrouptitle = list(
          text = "Well States",
          font = list(color = font_color, size = 13)
        )
      )
  }

  show_legend_any <- !is_numeric_var ||
    has_no_prot ||
    has_no_hit ||
    has_empty_in_rect

  p_out <- p |>
    plotly::layout(
      shapes = c(np_shapes, nh_shapes),
      dragmode = "zoom",
      showlegend = show_legend_any,
      legend = list(
        font = list(color = font_color, size = 11),
        bgcolor = "rgba(0,0,0,0)",
        y = 1,
        yanchor = "top"
      ),
      hoverlabel = list(
        bgcolor = "rgba(56, 56, 124, 0.86)",
        font = list(size = 14, color = "white"),
        bordercolor = "white"
      ),
      xaxis = list(
        side = "top",
        tickmode = "array",
        tickvals = cols,
        ticktext = as.character(cols),
        tickfont = list(color = font_color, size = 12),
        tickangle = 0,
        ticklen = 0,
        showgrid = FALSE,
        zeroline = FALSE,
        automargin = FALSE,
        range = c(min(cols) - 0.5, max(cols) + 0.5)
      ),
      yaxis = list(
        range = c(length(rows) - 0.5, -0.5),
        tickfont = list(color = font_color, size = 12),
        ticklen = 0,
        showgrid = FALSE,
        zeroline = FALSE,
        scaleanchor = "x",
        scaleratio = 1,
        automargin = FALSE
      ),
      margin = list(t = 25, r = if (is_numeric_var) 0 else 10, b = 0, l = 30),
      plot_bgcolor = tile_bg,
      paper_bgcolor = "rgba(0,0,0,0)"
    ) |>
    plotly::config(
      displayModeBar = "hover",
      scrollZoom = FALSE,
      modeBarButtons = list(list(
        "zoom2d",
        "toImage",
        "autoScale2d",
        "resetScale2d",
        "zoomIn2d",
        "zoomOut2d"
      )),
      toImageButtonOptions = list(
        filename = paste0(Sys.Date(), "_Batch_Heatmap")
      )
    )
  p_out
}
