# app/logic/conversion_functions.R

box::use(
  app /
    logic /
    deconvolution_functions[
      bin_spectrum,
      downsample_spectrum,
      parse_wells,
      plate_format,
      slim_spectrum,
      spectrum_plot,
      process_plot_data,
      unmatched_marker,
      unmatched_trace_tag,
    ],
  app /
    logic /
    conversion_constants[
      warning_sym,
      chart_js,
      paste_hook_js,
      sample_include_js,
      hits_col_full_names,
      kinetics_settings,
      run_limits,
      hit_preference_rules,
      shift_multiple_limits
    ],
  app / logic / ms_formats[ms_sample_base],
  app /
    logic /
    palette[
      categorical_colors,
      categorical_palette,
      color_alpha,
      concentration_symbols,
      control_symbol,
      level_tier,
      ramp_at,
      ramp_colorscale,
      sample_colors,
      status_colors,
      tier_pattern,
      tier_symbol,
      tint,
    ],
)

box::use(
  rlang[`%||%`],
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

# Concentration as shown on screen. Keys stay plain numbers because they are
# parsed back; the ones R writes as "8e-05" (molar units) read as 8×10⁻⁵, the
# style of the plot axes. Everything else passes through unchanged.
#' @export
conc_display <- function(x) {
  x <- as.character(x)
  sci <- !is.na(x) & grepl("^-?[0-9.]+e[-+]?[0-9]+$", x)
  if (!any(sci)) {
    return(x)
  }
  mantissa <- sub("e.*$", "", x[sci])
  exponent <- as.character(as.integer(sub("^.*e", "", x[sci])))
  x[sci] <- paste0(
    mantissa,
    "×10",
    chartr("0123456789-", "⁰¹²³⁴⁵⁶⁷⁸⁹⁻", exponent)
  )
  x
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

# Rescale the kinact/KI parameter matrix: kinact is a rate, KI a
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

# Rescale the complete kinact/KI result (parameters and fitted kobs curve)
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
  if (!is.null(kinact_ki_result$Two_Step)) {
    two_step <- kinact_ki_result$Two_Step
    two_step$ratio <- two_step$ratio / ratio_factor
    two_step$ratio_se <- two_step$ratio_se / ratio_factor
    two_step$kinact <- two_step$kinact / view$time_factor
    two_step$kinact_se <- two_step$kinact_se / view$time_factor
    two_step$KI <- two_step$KI * view$conc_factor
    two_step$KI_se <- two_step$KI_se * view$conc_factor
    kinact_ki_result$Two_Step <- two_step
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
  # Files saved without a trailing newline (common for hand-edited tables)
  # read fine, but read.table() warns about the incomplete final line. The
  # message is translated, so match the (untranslated) function name
  muffle_final_line <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
      if (grepl("readTableHeader", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    })
  }

  if (ext %in% c("csv")) {
    df <- muffle_final_line(utils::read.csv(
      file_path,
      stringsAsFactors = FALSE,
      sep = ",",
      header = FALSE
    ))

    # If only one column read, try semicolon separator
    if (ncol(df) == 1) {
      df <- muffle_final_line(utils::read.csv(
        file_path,
        stringsAsFactors = FALSE,
        sep = ";",
        header = FALSE
      ))
    }
  } else if (ext == "tsv") {
    df <- suppressMessages(readr::read_tsv(
      file_path,
      show_col_types = FALSE,
      col_names = FALSE
    ))
  } else if (ext == "txt") {
    df <- muffle_final_line(utils::read.delim(
      file_path,
      stringsAsFactors = FALSE,
      header = FALSE
    ))
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
      // A peak matches a mass within the tolerance, so two masses can claim
      // the same peak once they are no more than twice the tolerance apart -
      // the window the declaration check uses (small margin for rounding)
      var PROXIMITY = GLOBAL_TOLERANCE === null ? null : 2 * GLOBAL_TOLERANCE + 1e-9;
      
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
              if (diff <= PROXIMITY) {
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

  # Include checkboxes: a cleared one leaves the sample out of the conversion
  include_idx <- match("Include", colnames(tab))
  has_include <- !is.na(include_idx)

  # All columns that must be skipped by the JS renderer (no validation/duplicate check)
  skip_cols <- c(
    conc_time_idx,
    if (has_replicate) replicate_idx else integer(0)
  )

  # 0-indexed JS positions of the protein, first compound and Include columns
  protein_col_js <- match("Protein", colnames(tab)) - 1L
  compound_col_js <- min(cmp_cols) - 1L
  include_col_js <- if (has_include) include_idx - 1L else -1L

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

    // Samples left out of the conversion are not checked
    var includeCol = %d;
    if (includeCol >= 0 && instance.getDataAtCell(row, includeCol) === false) {
      return;
    }

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
    // Scan from the protein column on (Include, Sample and Replicate lie
    // before it) and leave out the skipped columns
    var isDuplicated = false;
    if (normalizedValue !== '') {
      var rowData = instance.getDataAtRow(row);
      var valueCounts = {};
      for (var i = %d; i < rowData.length; i++) {
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
      include_col_js,
      paste0("[", paste(skip_cols - 1L, collapse = ","), "]"),
      protein_col_js,
      compound_col_js,
      protein_col_js
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
      # Everything up to the protein stays in view
      fixedColumnsLeft = protein_col_js + 1L,
      type = "text",
      readOnly = ifelse(disabled, TRUE, FALSE)
    ) |>
    rhandsontable::hot_col(
      col = "Protein",
      type = "autocomplete",
      source = proteins,
      strict = FALSE
    ) |>
    rhandsontable::hot_col(
      col = (match("Sample", colnames(tab)) + 1L):max(cmp_cols),
      renderer = renderer_js
    ) |>
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

  # Include as checkboxes (hot_cols above made every column text), with a
  # select-all box in the header and the left-out rows greyed
  if (has_include) {
    handsontable <- handsontable |>
      rhandsontable::hot_col(
        "Include",
        type = "checkbox",
        halign = "htCenter",
        width = 40
      ) |>
      htmlwidgets::onRender(sample_include_js)
  }

  return(handsontable)
}

# Samples of the Samples table that enter the conversion: all but those whose
# Include box is cleared. Only an explicit FALSE leaves a sample out, so a
# table without the column (as built outside the declaration interface) keeps
# every sample.
#' @export
sample_inclusion <- function(sample_table) {
  if (!"Include" %in% names(sample_table)) {
    return(rep(TRUE, nrow(sample_table)))
  }
  !(as.logical(sample_table$Include) %in% FALSE)
}

# The included rows of a Samples table, without the Include column: the table
# the checks and the conversion work on
#' @export
included_sample_rows <- function(sample_table) {
  out <- sample_table[
    sample_inclusion(sample_table),
    names(sample_table) != "Include",
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
}

# Carry the Include state of an old Samples table over to a rebuilt one,
# matched by sample name. Samples new to the table are included.
#' @export
restore_inclusion <- function(new_table, old_table) {
  if (
    is.null(old_table) ||
      !"Include" %in% names(old_table) ||
      !"Include" %in% names(new_table)
  ) {
    return(new_table)
  }
  old <- sample_inclusion(old_table)[match(new_table$Sample, old_table$Sample)]
  new_table$Include <- is.na(old) | old
  new_table
}

# The tables a confirmed Samples table yields: `shown` keeps every sample, the
# left-out ones unticked so they can be included again, and `converted` holds
# the included samples only and is what the conversion works on. `excluded`
# names the samples left out. Needs at least one included sample.
#' @export
confirm_sample_table <- function(sample_table, units = NULL) {
  included <- sample_inclusion(sample_table)
  list(
    shown = clean_sample_table(sample_table, units = units),
    converted = clean_sample_table(
      included_sample_rows(sample_table),
      units = units
    ),
    excluded = as.character(sample_table$Sample[!included])
  )
}

# The deconvolution result of the samples a conversion screens: read from its
# database, which leaves the other samples unread, or cut from a result held
# in memory. With `samples` NULL every sample is kept.
#' @export
conversion_input_result <- function(result, samples = NULL) {
  if (!is.null(result$.db_path)) {
    return(read_decon_result(result$.db_path, samples = samples))
  }
  if (!is.null(samples) && !is.null(result$deconvolution)) {
    result$deconvolution <- result$deconvolution[
      names(result$deconvolution) %in% samples
    ]
  }
  result
}

# Columns of a Samples table in display order: Include, Sample and Replicate
# lead, the others keep their order
#' @export
order_sample_cols <- function(sample_table) {
  lead <- intersect(c("Include", "Sample", "Replicate"), names(sample_table))
  sample_table[, c(lead, setdiff(names(sample_table), lead)), drop = FALSE]
}

# Function to fill missing columns in sample table
#' @export
fill_sample_table <- function(sample_table, kinact_ki) {
  # Stash Include and Replicate (not part of the 7-col standard) to avoid
  # count mismatch
  lead <- intersect(c("Include", "Replicate"), names(sample_table))
  lead_cols <- sample_table[, lead, drop = FALSE]
  sample_table <- sample_table[, !names(sample_table) %in% lead, drop = FALSE]

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

  # Reattach Include before and Replicate right after Sample
  if (length(lead)) {
    sample_table <- order_sample_cols(cbind(sample_table, lead_cols))
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

  no_cmp_cols <- grepl(
    "^Include$|^Sample$|^Protein$|^Replicate$",
    names(sample_table)
  ) |
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

  # Reattach Include, Sample, Replicate (if present), and Protein; then
  # compound columns
  header_cols <- intersect(
    c("Include", "Sample", "Replicate", "Protein"),
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
#
# With the protein and compound tables, the peak tolerance and the maximum
# stoichiometry it also checks every protein and compound combination for mass
# assignments a peak cannot tell apart (see mass_ambiguities()). Two compounds
# of one sample that are indistinguishable fail the check - the analysis could
# not attribute their binding. The other ambiguities are resolved by a fixed
# rule during the run, so they pass with the reason attached as a
# "warning" attribute.
#' @export
check_sample_table <- function(
  sample_table,
  proteins,
  compounds,
  protein_table = NULL,
  compound_table = NULL,
  tolerance = NULL,
  max_multiples = NULL
) {
  # Samples left out of the conversion are not checked: every rule below
  # applies to the samples that are converted
  if ("Include" %in% names(sample_table)) {
    if (!any(sample_inclusion(sample_table))) {
      return(structure(
        "No sample included",
        note = "Tick the samples to convert in the first column of the table."
      ))
    }
    sample_table <- included_sample_rows(sample_table)
  }

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

  # The replicate series of each sample (see compute_replicate_labels()),
  # empty where nothing names it
  replicate_col <- if ("Replicate" %in% names(sample_table)) {
    sample_table$Replicate
  }
  series <- kinetic_series_labels(sample_table$Sample, replicate_col)

  # Strip Replicate so positional checks below remain Sample | Protein | Compounds
  sample_table <- sample_table[,
    names(sample_table) != "Replicate",
    drop = FALSE
  ]

  # Hard cap: one 384-well plate per run
  if (nrow(sample_table) > run_limits$max_samples) {
    return(structure(
      paste0(
        "At most ",
        run_limits$max_samples,
        " samples per run (",
        nrow(sample_table),
        " present)"
      ),
      note = paste(
        "Split the samples over several deconvolution databases and convert",
        "them one by one."
      )
    ))
  }

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

    # kinact/KI describes one compound reacting with one protein, and the
    # concentration column holds a single value per sample: a sample listing
    # several compounds has no concentration for each, and a second compound
    # competing for the site changes k_obs
    declared <- sample_compounds(sample_table)
    several <- lengths(declared) > 1
    if (any(several)) {
      return(structure(
        paste0(
          "One compound per sample for kinact/KI (",
          sum(several),
          if (sum(several) == 1) " sample lists" else " samples list",
          " several)"
        ),
        details = format_listed(sprintf(
          "%s: %s",
          ms_sample_base(sample_table[several, 1]),
          vapply(declared[several], paste, character(1), collapse = ", ")
        )),
        note = paste(
          "k_obs and kinact/KI are fitted per protein-compound complex.",
          "Screen compound mixtures with kinact/KI switched off."
        )
      ))
    }

    # k_obs and kinact/KI are fitted per complex, over its own samples, so the
    # design rules hold for every complex: at least 3 non-zero concentrations,
    # each with at least 3 distinct non-zero time points (concentration 0 is
    # the untreated control and is exempt)
    complex <- paste(
      sample_table[, 2],
      vapply(declared, function(x) x[1], character(1)),
      sep = " + "
    )
    prefix <- if (length(unique(complex)) > 1) {
      function(k) paste0(k, ": ")
    } else {
      function(k) ""
    }
    for (k in unique(complex)) {
      in_k <- complex == k
      k_conc <- conc_vals[in_k]
      k_time <- time_vals[in_k]
      unique_concs <- unique(k_conc[k_conc != 0])
      if (length(unique_concs) < 3) {
        return(paste0(
          prefix(k),
          "At least 3 different non-zero concentrations required (",
          length(unique_concs),
          " present)"
        ))
      }
      for (uc in unique_concs) {
        times_for_conc <- k_time[k_conc == uc]
        n_time <- length(unique(times_for_conc[times_for_conc != 0]))
        if (n_time < 3) {
          return(paste0(
            prefix(k),
            "At least 3 different non-zero time points required per concentration (concentration ",
            uc,
            " has only ",
            n_time,
            ")"
          ))
        }
      }
    }

    # Hard cap on replicate series: each one is fitted on its own and drawn
    # in its own marker fill, and the plots have four of them
    n_series <- sort_series(series[!is.na(series)])
    if (length(n_series) > run_limits$max_series) {
      return(structure(
        paste0(
          "At most ",
          run_limits$max_series,
          " replicate series (",
          length(n_series),
          " present)"
        ),
        details = format_listed(vapply(
          n_series,
          function(s) sprintf("%s: %d samples", s, sum(series %in% s)),
          character(1)
        )),
        note = paste(
          "A replicate series is one complete repeat of the experiment (all R1",
          "samples, all R2 samples, ...). It comes from the Replicate column",
          "of the config or the _R<n> ending of the file names."
        )
      ))
    }
  }

  # Hard cap on replicates per condition. With kinact/KI a condition is the
  # declared protein, compounds, concentration and time (the untreated
  # controls at concentration 0 are exempt, like in the design rules);
  # without, it is the samples named alike up to _R<n>.
  condition <- if (has_conc_time) {
    key <- paste0(
      sample_table[, 2],
      " + ",
      vapply(
        sample_compounds(sample_table),
        function(x) paste(sort(x), collapse = ", "),
        character(1)
      ),
      ", concentration ",
      conc_time_tbl[[1]],
      ", time ",
      conc_time_tbl[[2]]
    )
    key[conc_time_tbl[[1]] == 0] <- NA
    key
  } else {
    stem <- ms_sample_base(sample_table[, 1])
    ifelse(grepl("_[Rr][0-9]+$", stem), sub("_[Rr][0-9]+$", "", stem), NA)
  }
  counts <- table(condition[!is.na(condition)])
  crowded <- counts[counts > run_limits$max_replicates]
  if (length(crowded)) {
    return(structure(
      paste0(
        "At most ",
        run_limits$max_replicates,
        " replicates per condition (",
        length(crowded),
        if (length(crowded) == 1) " condition has" else " conditions have",
        " up to ",
        max(crowded),
        ")"
      ),
      details = format_listed(sprintf(
        "%s: %d samples",
        names(crowded),
        as.integer(crowded)
      )),
      note = if (has_conc_time) {
        paste(
          "Replicates are samples with the same protein, compounds,",
          "concentration and time. Check for a mistyped concentration or time."
        )
      } else {
        "Replicates are samples named alike up to their _R<n> ending."
      }
    ))
  }

  # Warnings of a passed check, shown together in the orange hint
  warnings <- list()

  if (
    !is.null(protein_table) &&
      !is.null(compound_table) &&
      !is.null(tolerance) &&
      !is.null(max_multiples)
  ) {
    amb <- declaration_ambiguities(
      sample_table,
      protein_table,
      compound_table,
      tolerance,
      max_multiples
    )

    # The hint above the table holds one short line; the pairs and what the
    # screening does with them go to its tooltip (attributes "details", "note")
    window <- paste0("within 2 \u00d7 peak tolerance (", 2 * tolerance, " Da)")

    blocking <- amb[amb$kind == "compounds", , drop = FALSE]
    if (nrow(blocking)) {
      return(structure(
        paste0(
          "Compounds of one sample are not distinguishable ",
          window,
          ": ",
          format_compound_pairs(blocking)
        ),
        details = format_ambiguities(blocking),
        note = "Assign them to separate samples or revise the mass shifts."
      ))
    }

    if (nrow(amb)) {
      pairs <- length(format_ambiguities(amb, max_listed = Inf))
      warnings$ambiguities <- list(
        warning = paste0(
          "Ambiguous masses ",
          window,
          ": ",
          pairs,
          if (pairs == 1) " pair" else " pairs"
        ),
        details = format_ambiguities(amb),
        note = paste(
          "Unbound readings are kept. A complex shared by proteoforms counts",
          "once in the binding; the per-proteoform binding of those samples is",
          "not resolvable."
        )
      )
    }
  }

  replicates <- replicate_mismatches(
    sample_table,
    if (has_conc_time) conc_time_tbl
  )
  if (length(replicates)) {
    warnings$replicates <- list(
      warning = paste0(
        "Replicates declared differently: ",
        length(replicates),
        if (length(replicates) == 1) " group" else " groups"
      ),
      details = format_listed(replicates),
      note = paste(
        "Samples named alike up to _R<n> are read as replicates of one",
        "condition. Each is analysed as declared, so a mismatch moves it to",
        "another complex, concentration or time point."
      )
    )
  }

  conflicts <- replicate_conflicts(sample_table[, 1], replicate_col)
  if (length(conflicts)) {
    warnings$conflicts <- list(
      warning = paste0(
        "Replicate differs from the file name: ",
        length(conflicts),
        if (length(conflicts) == 1) " sample" else " samples"
      ),
      details = format_listed(conflicts),
      note = paste(
        "The Replicate value from the config is used. Check whether the",
        "config or the file name is wrong."
      )
    )
  }

  if (!length(warnings)) {
    return(TRUE)
  }

  structure(
    TRUE,
    warning = paste(
      vapply(warnings, `[[`, character(1), "warning"),
      collapse = " \u00b7 "
    ),
    details = unlist(lapply(warnings, `[[`, "details"), use.names = FALSE),
    note = paste(vapply(warnings, `[[`, character(1), "note"), collapse = " ")
  )
}

# Tooltip lines of a declaration hint, at most `max_listed` of them
format_listed <- function(lines, max_listed = 12) {
  more <- length(lines) - max_listed
  c(
    utils::head(lines, max_listed),
    if (more > 0) sprintf("and %d more", more)
  )
}

# Compounds declared per row of a sample table (Sample | Protein | Compound ...)
sample_compounds <- function(sample_table) {
  cmp <- sample_table[, -(1:2), drop = FALSE]
  lapply(seq_len(nrow(cmp)), function(i) {
    x <- trimws(as.character(unlist(cmp[i, ])))
    unique(x[!is.na(x) & nzchar(x)])
  })
}

# Samples whose Replicate value (from a config) names another series number
# than the _R<n> ending of their file name, e.g. "R2" or "Rep2" for
# "..._R1.raw". A value without a number is a naming of its own, not a
# contradiction. One line per sample.
replicate_conflicts <- function(sample_names, replicate) {
  if (is.null(replicate)) {
    return(character(0))
  }
  suffix <- replicate_suffix(sample_names)
  value <- trimws(as.character(replicate))
  value_n <- suppressWarnings(
    as.integer(sub("^.*?([0-9]+)$", "\\1", value))
  )
  value_n[!grepl("[0-9]$", value)] <- NA
  suffix_n <- as.integer(sub("^R", "", suffix))
  clash <- !is.na(suffix_n) & !is.na(value_n) & value_n != suffix_n
  stem <- ms_sample_base(sample_names)
  sprintf("%s: Replicate %s", stem[clash], value[clash])
}

# Replicate groups whose samples are declared differently. A group is the
# samples whose names differ only in their _R<n> suffix (the Replicate label
# KiwiMS derives from the file name); they are expected to share protein,
# compounds and, with kinact/KI, concentration and time. One line per group,
# naming the first column that differs.
replicate_mismatches <- function(sample_table, conc_time_tbl = NULL) {
  stem <- ms_sample_base(sample_table[, 1])
  group <- ifelse(
    grepl("_[Rr][0-9]+$", stem),
    sub("_[Rr][0-9]+$", "", stem),
    NA_character_
  )

  fields <- list(
    Protein = as.character(sample_table[, 2]),
    Compound = vapply(
      sample_compounds(sample_table),
      function(x) paste(sort(x), collapse = ", "),
      character(1)
    )
  )
  if (!is.null(conc_time_tbl)) {
    fields$Concentration <- as.character(conc_time_tbl[[1]])
    fields$Time <- as.character(conc_time_tbl[[2]])
  }

  groups <- unique(group[!is.na(group) & duplicated(group)])
  out <- lapply(groups, function(g) {
    idx <- which(group == g)
    for (f in names(fields)) {
      values <- unique(fields[[f]][idx])
      if (length(values) > 1) {
        return(sprintf(
          "%s: %s %s",
          g,
          f,
          paste(values, collapse = " \u2194 ")
        ))
      }
    }
    NULL
  })
  unlist(out)
}

# Mass shifts of one compound that are whole multiples of each other ----
#
# One row per pair of shifts in one row of the compound table (name column,
# then Mass 1 ..) where the larger is `factor` times the smaller, within
# shift_multiple_limits$precision, for factors 1 (the same shift twice) up to
# shift_multiple_limits$max_factor. Shifts of opposite sign are never
# multiples; a zero shift only pairs with another zero.
#' @export
shift_multiples <- function(compound_table) {
  empty <- data.frame(
    compound = character(0),
    smaller = character(0),
    larger = character(0),
    factor = integer(0),
    smaller_mass = numeric(0),
    larger_mass = numeric(0)
  )
  if (
    !is.data.frame(compound_table) ||
      !nrow(compound_table) ||
      ncol(compound_table) < 3
  ) {
    return(empty)
  }

  precision <- shift_multiple_limits$precision + 1e-9 # floating-point margin
  cols <- names(compound_table)[-1]

  out <- lapply(seq_len(nrow(compound_table)), function(r) {
    m <- suppressWarnings(as.numeric(unlist(
      compound_table[r, -1, drop = FALSE]
    )))
    idx <- which(!is.na(m))
    if (length(idx) < 2) {
      return(NULL)
    }
    do.call(
      rbind,
      lapply(utils::combn(idx, 2, simplify = FALSE), function(p) {
        # Smaller magnitude first, then declared order
        p <- p[order(abs(m[p]), p)]
        a <- m[p[1]]
        b <- m[p[2]]
        n <- if (abs(a) <= precision) {
          if (abs(b) <= precision) 1 else NA
        } else {
          round(b / a)
        }
        if (
          is.na(n) ||
            n < 1 ||
            n > shift_multiple_limits$max_factor ||
            abs(b - n * a) > precision
        ) {
          return(NULL)
        }
        data.frame(
          compound = as.character(compound_table[[1]][r]),
          smaller = cols[p[1]],
          larger = cols[p[2]],
          factor = as.integer(n),
          smaller_mass = a,
          larger_mass = b
        )
      })
    )
  })

  out <- do.call(rbind, out)
  if (is.null(out)) empty else out
}

# The refusal for shift_multiples(): one line naming at most `max_listed`
# pairs, every pair in the "details" attribute and what to do in "note". The
# Compounds table shows it as its hint, the start of a conversion as a toast.
#' @export
shift_multiples_message <- function(multiples, max_listed = 2) {
  fmt <- function(x) {
    format(x, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
  }
  pair <- ifelse(
    multiples$factor == 1,
    sprintf("%s = %s", fmt(multiples$larger_mass), fmt(multiples$smaller_mass)),
    sprintf(
      "%s = %d × %s",
      fmt(multiples$larger_mass),
      multiples$factor,
      fmt(multiples$smaller_mass)
    )
  )
  short <- sprintf("%s (%s)", multiples$compound, pair)
  more <- length(short) - max_listed

  structure(
    paste0(
      "Mass shifts that are multiples of each other: ",
      paste(utils::head(short, max_listed), collapse = ", "),
      if (more > 0) sprintf(" and %d more", more) else ""
    ),
    details = sprintf(
      "%s: %s (%s Da) = %s%s (%s Da)",
      multiples$compound,
      multiples$larger,
      fmt(multiples$larger_mass),
      ifelse(
        multiples$factor == 1,
        "",
        paste0(multiples$factor, " × ")
      ),
      multiples$smaller,
      fmt(multiples$smaller_mass)
    ),
    note = paste(
      "Stoichiometry already covers multiples of a mass shift and no",
      "spectrum tells them apart. Remove the multiple."
    )
  )
}

# Show the Preferred column of the hits table by default only when it tells
# something: when at least one peak had a reading that was not preferred
#' @export
show_preferred_column <- function(hits) {
  "Preferred" %in% names(hits) && any(as.character(hits$Preferred) %in% "FALSE")
}

# Validate protein/compound table. `type` is "proteins" or "compounds"; the
# compound table also refuses mass shifts that are multiples of each other.
#' @export
check_table <- function(tab, tolerance, type = NULL) {
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

  # Protein masses and compound mass shifts are both added onto the peaks
  # looked for, so a mass of zero or below declares nothing a spectrum shows
  masses <- as.matrix(tab[, -1, drop = FALSE])
  not_positive <- which(!is.na(masses) & masses <= 0, arr.ind = TRUE)
  if (nrow(not_positive)) {
    not_positive <- not_positive[
      order(not_positive[, 1], not_positive[, 2]),
      ,
      drop = FALSE
    ]
    return(structure(
      "Mass values must be greater than 0",
      details = sprintf(
        "%s: %s (%s Da)",
        tab[[1]][not_positive[, 1]],
        colnames(masses)[not_positive[, 2]],
        format(
          masses[not_positive],
          trim = TRUE,
          scientific = FALSE,
          drop0trailing = TRUE
        )
      ),
      note = "Declare protein masses and compound mass shifts as positive values."
    ))
  }

  # Exact multiples among the shifts of one compound
  if (identical(type, "compounds")) {
    multiples <- shift_multiples(tab)
    if (nrow(multiples)) {
      return(shift_multiples_message(multiples))
    }
  }

  # Masses within the tolerance window of each other are not refused here: the
  # tables colour them (prot_comp_handsontable()) and the Samples-table check
  # reports what they mean for the screening (mass_ambiguities()); within one
  # compound the preferred assignment rule resolves them

  # If all checks passed return TRUE
  return(TRUE)
}

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

  if (nrow(peaks) != 0) {
    # Normalize peaks
    peaks$intensity <- peaks$intensity / max(peaks$intensity) * 100
  }

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

# Check if hits present in spectrum i.e. peaks belonging to declared species or adducts
# Peaks the declaration predicts ----
#
# One row per interpretation of a mass: every protein species unbound, and
# every species with every mass shift of every compound at stoichiometry
# 1 .. max_multiples. Ordered species, stoichiometry, mass shift, compound;
# `shift` is the declared position of the mass shift (Mass 1, Mass 2 ..), which
# prefer_hits() sorts on. `compound_mw` is the compound table (name column,
# then Mass 1 ..) restricted to the compounds in question.
#' @export
predict_peaks <- function(prot_masses, compound_mw, max_multiples) {
  prot_masses <- suppressWarnings(as.numeric(prot_masses))
  prot_masses <- unique(prot_masses[!is.na(prot_masses)])

  unbound <- data.frame(
    mass = prot_masses,
    species = prot_masses,
    type = rep("unbound", length(prot_masses)),
    compound = NA_character_,
    shift = NA_integer_,
    cmp_mass = NA_real_,
    multiple = 0L
  )

  max_multiples <- suppressWarnings(as.integer(max_multiples))
  if (
    is.null(compound_mw) ||
      !nrow(compound_mw) ||
      ncol(compound_mw) < 2 ||
      !length(prot_masses) ||
      is.na(max_multiples) ||
      max_multiples < 1
  ) {
    return(unbound)
  }

  cmp <- suppressWarnings(apply(
    as.matrix(compound_mw[, -1, drop = FALSE]),
    2,
    as.numeric
  ))
  cmp <- matrix(cmp, nrow = nrow(compound_mw))

  # Compound fastest, then mass shift, then stoichiometry
  grid <- expand.grid(
    row = seq_len(nrow(cmp)),
    shift = seq_len(ncol(cmp)),
    multiple = seq_len(max_multiples)
  )
  cmp_mass <- cmp[cbind(grid$row, grid$shift)]
  keep <- !is.na(cmp_mass)
  grid <- grid[keep, , drop = FALSE]
  cmp_mass <- cmp_mass[keep]

  complexes <- do.call(
    rbind,
    lapply(prot_masses, function(m) {
      data.frame(
        mass = m + cmp_mass * grid$multiple,
        species = m,
        type = rep("complex", nrow(grid)),
        compound = as.character(compound_mw[[1]])[grid$row],
        shift = grid$shift,
        cmp_mass = cmp_mass,
        multiple = grid$multiple
      )
    })
  )

  rbind(unbound, complexes)
}

# Plain-text name of one predicted interpretation
interpretation_label <- function(peaks) {
  species <- paste0(fmt_species_mass(peaks$species), " Da")
  ifelse(
    peaks$type == "unbound",
    paste(species, "unbound"),
    sprintf(
      "%s + %s (%s Da ×%d)",
      species,
      peaks$compound,
      format(peaks$cmp_mass, nsmall = 1, trim = TRUE),
      as.integer(peaks$multiple)
    )
  )
}

# Mass assignments a peak cannot tell apart ----
#
# check_hits() assigns a peak to every interpretation whose predicted mass lies
# within the peak tolerance of it, so two interpretations can claim one peak
# once their predicted masses are no more than twice the tolerance apart. One
# row per such pair, classified by what the screening does with it:
#   compounds  - one species with two different compounds. Nothing in the
#                spectrum tells the compounds apart, so the declaration check
#                refuses such a sample.
#   species    - an unbound species and a complex, or two unbound species. The
#                unbound reading wins; the run logs it.
#   proteoform - complexes of two different species. The peak counts once in
#                the pooled binding; per species it is not resolvable (see
#                proteoform_binding()).
# Pairs within one compound on one species - two of its mass shifts or
# stoichiometries - are left out: the preferred assignment resolves those
# (prefer_hits()).
#' @export
mass_ambiguities <- function(
  prot_masses,
  compound_mw,
  max_multiples,
  tolerance
) {
  empty <- data.frame(
    kind = character(0),
    first = character(0),
    second = character(0),
    delta = numeric(0),
    first_compound = character(0),
    second_compound = character(0)
  )

  tolerance <- suppressWarnings(as.numeric(tolerance))
  if (!length(tolerance) || is.na(tolerance) || tolerance < 0) {
    return(empty)
  }

  pred <- predict_peaks(prot_masses, compound_mw, max_multiples)
  if (nrow(pred) < 2) {
    return(empty)
  }
  pred <- pred[order(pred$mass), , drop = FALSE]

  # Neighbours within the window, found on the sorted masses rather than on
  # the full pairwise matrix, which grows with the square of the
  # interpretations (9 masses x 9 shifts x stoichiometry per compound)
  window <- 2 * tolerance + 1e-9 # margin for floating-point sums
  last <- findInterval(pred$mass + window, pred$mass)
  i <- rep(seq_len(nrow(pred)), pmax(last - seq_len(nrow(pred)), 0))
  if (!length(i)) {
    return(empty)
  }
  j <- unlist(lapply(
    seq_len(nrow(pred)),
    function(k) if (last[k] > k) (k + 1):last[k] else integer(0)
  ))

  a <- pred[i, , drop = FALSE]
  b <- pred[j, , drop = FALSE]
  both_complex <- a$type == "complex" & b$type == "complex"
  same_species <- a$species == b$species

  kind <- ifelse(
    !both_complex,
    "species",
    ifelse(
      !same_species,
      "proteoform",
      ifelse(a$compound != b$compound, "compounds", NA_character_)
    )
  )
  keep <- !is.na(kind)

  if (!any(keep)) {
    return(empty)
  }

  out <- data.frame(
    kind = kind[keep],
    first = interpretation_label(a[keep, , drop = FALSE]),
    second = interpretation_label(b[keep, , drop = FALSE]),
    delta = abs(b$mass[keep] - a$mass[keep]),
    # Compound of each side, NA for an unbound reading
    first_compound = a$compound[keep],
    second_compound = b$compound[keep]
  )
  out[!duplicated(out[, c("kind", "first", "second")]), , drop = FALSE]
}

# The same for every protein and compound combination of a sample table ----
#
# Samples sharing a protein and a set of compounds are checked once. `samples`
# counts how many samples a pair concerns.
#' @export
declaration_ambiguities <- function(
  sample_table,
  protein_table,
  compound_table,
  tolerance,
  max_multiples
) {
  empty <- data.frame(
    protein = character(0),
    kind = character(0),
    first = character(0),
    second = character(0),
    delta = numeric(0),
    first_compound = character(0),
    second_compound = character(0),
    samples = integer(0)
  )

  if (
    !is.data.frame(sample_table) ||
      !nrow(sample_table) ||
      !is.data.frame(protein_table) ||
      !is.data.frame(compound_table) ||
      !"Protein" %in% names(sample_table)
  ) {
    return(empty)
  }

  cmp_cols <- grep("^Compound", names(sample_table), value = TRUE)
  compounds <- apply(
    sample_table[, cmp_cols, drop = FALSE],
    1,
    function(x) {
      x <- trimws(as.character(x))
      sort(unique(x[!is.na(x) & nzchar(x)]))
    },
    simplify = FALSE
  )
  key <- paste(
    sample_table$Protein,
    vapply(compounds, paste, character(1), collapse = "\u0001"),
    sep = "\u0002"
  )

  out <- lapply(unique(key), function(k) {
    idx <- which(key == k)
    protein <- as.character(sample_table$Protein[idx[1]])
    prot_row <- protein_table[protein_table[[1]] %in% protein, , drop = FALSE]
    if (!nrow(prot_row) || is.na(protein)) {
      return(NULL)
    }
    cmp_rows <- compound_table[
      compound_table[[1]] %in% compounds[[idx[1]]],
      ,
      drop = FALSE
    ]

    amb <- mass_ambiguities(
      unlist(prot_row[1, -1, drop = FALSE]),
      cmp_rows,
      max_multiples,
      tolerance
    )
    if (!nrow(amb)) {
      return(NULL)
    }
    cbind(protein = protein, amb, samples = length(idx))
  })

  out <- do.call(rbind, out)
  if (is.null(out)) empty else out
}

# Ambiguous pairs for the tooltip of the declaration hints, one line each,
# listing at most `max_listed` pairs
format_ambiguities <- function(amb, max_listed = 12) {
  pairs <- unique(sprintf(
    "%s ↔ %s (Δ %.1f Da)",
    amb$first,
    amb$second,
    amb$delta
  ))
  more <- length(pairs) - max_listed
  c(
    utils::head(pairs, max_listed),
    if (more > 0) sprintf("and %d more", more)
  )
}

# The compounds that cannot be told apart, for the one-line declaration hint
format_compound_pairs <- function(amb, max_listed = 2) {
  pairs <- unique(vapply(
    seq_len(nrow(amb)),
    function(k) {
      compounds <- c(amb$first_compound[k], amb$second_compound[k])
      paste(sort(compounds), collapse = " ↔ ")
    },
    character(1)
  ))
  more <- length(pairs) - max_listed
  paste0(
    paste(utils::head(pairs, max_listed), collapse = ", "),
    if (more > 0) sprintf(" and %d more", more) else ""
  )
}

# Preferred assignment of an ambiguous peak ----
#
# The rule a setting names (see hit_preference_rules), or the default one for a
# missing or unknown name: a setting saved by another release must not stop a
# run.
#' @export
hit_preference_rule <- function(rule) {
  rule <- as.character(rule)[1]
  if (!is.na(rule) && rule %in% names(hit_preference_rules)) {
    rule
  } else {
    names(hit_preference_rules)[1]
  }
}

# The rules as select-input choices, labels naming values
#' @export
hit_preference_choices <- function() {
  stats::setNames(
    names(hit_preference_rules),
    vapply(hit_preference_rules, `[[`, character(1), "label")
  )
}

# Which readings of one peak are preferred. `hits` holds one row per reading
# (theor_prot, compound, multiple, delta_cmp) and `shift` the declared position
# of each reading's mass shift. Per protein species and compound exactly one
# reading is TRUE: the first after sorting by the criteria of the rule in turn.
# Mass errors are compared rounded to 1e-6 Da, so floating-point noise in the
# predicted sums cannot decide between readings that are equally close, and the
# row order settles whatever the criteria leave tied.
#' @export
prefer_hits <- function(
  hits,
  shift = hits$shift,
  rule = names(hit_preference_rules)[1]
) {
  n <- nrow(hits)
  if (!n) {
    return(logical(0))
  }
  if (length(shift) != n) {
    shift <- rep(NA_real_, n)
  }

  criteria <- list(
    multiple = suppressWarnings(as.numeric(hits$multiple)),
    error = round(suppressWarnings(as.numeric(hits$delta_cmp)), 6),
    shift = suppressWarnings(as.numeric(shift))
  )[hit_preference_rules[[hit_preference_rule(rule)]]$keys]

  group <- paste(hits$theor_prot, hits$compound, sep = "\r")
  ord <- do.call(
    order,
    c(list(group), unname(criteria), list(seq_len(n)))
  )
  preferred <- logical(n)
  preferred[ord] <- !duplicated(group[ord])
  preferred
}

# A protein may be declared with more than one mass (Mass 1 .. Mass 9), e.g. the
# plain protein plus a modified form such as a gluconoylated His-tag. Every
# declared mass is treated as an independent species: it carries its own unbound
# signal and compounds bind to it exactly like they bind to the base mass. Rows
# of the returned hits frame carry the species a hit belongs to in theor_prot /
# measured_prot / delta_prot, while prot_intensity stays a sample-level value -
# the summed intensity of all detected unbound species. `preference` names the
# rule picking the preferred reading of an ambiguous peak (hit_preference_rules).
check_hits <- function(
  sample_table,
  protein_mw,
  compound_mw,
  peaks,
  peak_tolerance,
  max_multiples,
  sample,
  well = NA,
  preference = names(hit_preference_rules)[1]
) {
  # Get protein name and its declared mass species
  prot_name <- as.character(protein_mw[1, 1])
  prot_masses <- suppressWarnings(as.numeric(unlist(
    protein_mw[1, -1, drop = FALSE]
  )))
  prot_masses <- unique(prot_masses[!is.na(prot_masses)])

  # Locate each species: closest peak within tolerance, NA if the species has
  # no unbound signal (fully converted or simply absent)
  species_row <- vapply(
    prot_masses,
    function(m) {
      dist <- abs(peaks$mass - m)
      within <- which(dist <= peak_tolerance)
      if (!length(within)) NA_integer_ else within[which.min(dist[within])]
    },
    integer(1)
  )

  species <- data.frame(
    theor = prot_masses,
    measured = peaks$mass[species_row],
    intensity = peaks$intensity[species_row]
  )
  species$delta <- abs(species$theor - species$measured)

  # A peak claimed by more than one species - declared masses closer to each
  # other than the tolerance - is counted for the first species claiming it, so
  # the unbound signal is not inflated. A species without signal contributes
  # nothing but can still carry complexes (fully converted protein).
  species$intensity[duplicated(species_row) & !is.na(species_row)] <- 0
  species$intensity[is.na(species$intensity)] <- 0

  # Every peak the declaration predicts: each species unbound and with every
  # mass shift of every compound at every stoichiometry. The declaration check
  # reads the same table, so both see the same interpretations.
  predicted <- predict_peaks(prot_masses, compound_mw, max_multiples)
  complexes <- predicted[predicted$type == "complex", , drop = FALSE]

  # Keep only peaks at or above the lightest predicted mass: usually the
  # lightest species, lower when a negative mass shift (a compound whose
  # binding loses more than it adds) makes a complex lighter than it
  peaks_valid <- if (length(prot_masses)) {
    peaks$mass >= min(predicted$mass) - peak_tolerance
  } else {
    logical(nrow(peaks))
  }

  # Compounds declared for the sample - the compound table arrives narrowed
  # to them
  declared <- if (is.null(compound_mw) || !nrow(compound_mw)) {
    character(0)
  } else {
    unique(as.character(compound_mw[[1]]))
  }

  # Rows without an adduct, one per given species and declared compound the
  # species carries no complex of (`complexed`, "<species> <compound>"), so
  # every declared protein-compound pair of the sample is listed. A sample
  # declaring no compound - a control - gets rows that name none.
  no_adduct_rows <- function(sp, complexed = character(0)) {
    pairs <- expand.grid(
      s = seq_len(nrow(sp)),
      compound = if (length(declared)) declared else NA_character_,
      stringsAsFactors = FALSE
    )
    pairs <- pairs[
      !paste(sp$theor[pairs$s], pairs$compound) %in% complexed,
      ,
      drop = FALSE
    ]
    if (!nrow(pairs)) {
      return(NULL)
    }
    sp <- sp[pairs$s, , drop = FALSE]

    data.frame(
      well = well,
      sample = sample,
      protein = prot_name,
      theor_prot = sp$theor,
      measured_prot = sp$measured,
      delta_prot = sp$delta,
      prot_intensity = ifelse(is.na(sp$measured), NA_real_, sp$intensity),
      # The species' own signal
      peak = sp$measured,
      intensity = NA,
      compound = pairs$compound,
      cmp_mass = NA,
      delta_cmp = NA,
      multiple = NA,
      preferred = NA
    )
  }

  if (!any(peaks_valid)) {
    return(no_adduct_rows(data.frame(
      theor = if (length(prot_masses)) prot_masses[1] else NA_real_,
      measured = NA_real_,
      delta = NA_real_,
      intensity = NA_real_
    )))
  }

  # A peak read as an unbound species can equally be a complex of another
  # species (its mass offset matches a compound). The species reading wins -
  # there is nothing in one spectrum to decide it otherwise - so it is logged.
  for (s in which(!is.na(species_row) & !duplicated(species_row))) {
    peak_mass <- peaks$mass[species_row[s]]
    alt <- complexes[
      abs(complexes$mass - peak_mass) <= peak_tolerance &
        complexes$species != species$theor[s],
      ,
      drop = FALSE
    ]
    if (nrow(alt)) {
      log_species_complex_overlap(peak_mass, species$theor[s], alt)
    }
  }

  # Peaks already explained as an unbound species must not be re-interpreted as
  # a complex of a lighter species
  peaks_filtered <- as.data.frame(peaks[
    peaks_valid & !(seq_len(nrow(peaks)) %in% stats::na.omit(species_row)),
  ])

  # Initiate empty hits data frame
  hits_df <- data.frame()

  # Fill hits_df
  for (j in seq_len(nrow(peaks_filtered))) {
    upper <- peaks_filtered$mass[j] + peak_tolerance
    lower <- peaks_filtered$mass[j] - peak_tolerance

    # A peak can be a complex of any of the declared species
    matched <- complexes[
      complexes$mass >= lower & complexes$mass <= upper,
      ,
      drop = FALSE
    ]

    if (!nrow(matched)) {
      next
    }

    s <- match(matched$species, species$theor)

    hits_add <- data.frame(
      well = well,
      sample = sample,
      protein = prot_name,
      theor_prot = species$theor[s],
      measured_prot = species$measured[s],
      delta_prot = species$delta[s],
      prot_intensity = species$intensity[s],
      peak = peaks_filtered[j, "mass"],
      intensity = peaks_filtered[j, "intensity"],
      compound = matched$compound,
      cmp_mass = matched$cmp_mass,
      delta_cmp = abs(
        matched$cmp_mass *
          matched$multiple -
          (peaks_filtered[j, "mass"] - species$theor[s])
      ),
      multiple = matched$multiple,
      preferred = TRUE
    )

    # Case multiple matching
    if (nrow(hits_add) > 1) {
      # One reading per species and compound names the peak, picked by the
      # chosen rule. Readings of different species (proteoforms) each stay
      # preferred: nothing tells them apart. conversion() counts the peak once
      # for the sample; proteoform_binding() leaves it unresolved per species.
      hits_add$preferred <- prefer_hits(
        hits_add,
        shift = matched$shift,
        rule = preference
      )

      log_ambiguous_peak(hits_add, preference)
    }

    hits_df <- rbind(hits_df, hits_add)
  }

  # Every species in the spectrum - detected unbound or carrying a complex -
  # lists each declared compound it carries no complex of, so a pair whose
  # adduct was not found is annotated and counted too
  present <- species[
    !is.na(species$measured) | species$theor %in% hits_df$theor_prot,
    ,
    drop = FALSE
  ]
  if (!nrow(present)) {
    # No species detected and no complex found
    present <- species[1, , drop = FALSE]
    present$measured <- NA
    present$delta <- NA
  }

  hits_df <- rbind(
    hits_df,
    no_adduct_rows(present, paste(hits_df$theor_prot, hits_df$compound))
  )

  return(hits_df)
}

# Unbound protein species of one sample, one row per declared mass that carries
# unbound signal. Hits are one row per assignment, so a species with several
# complexes repeats and one without any complex appears as its own
# compound-less row - both collapse to the same single entry here. Accepts the
# hits frame in either its raw or its display column naming.
#' @export
unbound_species <- function(hits) {
  cols <- if ("Mw Protein [Da]" %in% names(hits)) {
    c("Mw Protein [Da]", "Protein Intensity")
  } else if ("Theor. Prot. [Da]" %in% names(hits)) {
    c("Theor. Prot. [Da]", "Int. Prot. [%]")
  } else {
    c("theor_prot", "prot_intensity")
  }

  species <- data.frame(
    theor_prot = suppressWarnings(as.numeric(hits[[cols[1]]])),
    prot_intensity = suppressWarnings(as.numeric(hits[[cols[2]]]))
  )

  species <- species[
    !is.na(species$prot_intensity) & species$prot_intensity > 0,
    ,
    drop = FALSE
  ]
  species <- species[!duplicated(species$theor_prot), , drop = FALSE]

  species[order(species$theor_prot), , drop = FALSE]
}

# Whether each hits row is a complex. A row without an adduct still names the
# compound declared for its sample (see check_hits()), so the name alone does
# not tell - the assigned compound mass does. Accepts the hits frame in its raw
# or its display column naming; a frame without the mass reads a named row as a
# complex.
#' @export
is_complex_row <- function(hits) {
  cols <- if ("Compound" %in% names(hits)) {
    c("Compound", "Compound Mw [Da]")
  } else {
    c("Cmp Name", "Theor. Cmp [Da]")
  }
  named <- !is.na(hits[[cols[1]]])
  if (!cols[2] %in% names(hits)) {
    return(named)
  }
  cmp_mass <- as.character(hits[[cols[2]]])
  named & !is.na(cmp_mass) & cmp_mass != "N/A"
}

# Format a protein species mass for use in a label
#' @export
fmt_species_mass <- function(x) {
  vapply(
    suppressWarnings(as.numeric(x)),
    function(v) {
      if (is.na(v)) {
        "N/A"
      } else {
        format(round(v, 1), big.mark = ",", scientific = FALSE)
      }
    },
    character(1)
  )
}

# Mass-shift label of one peak, e.g. "[266.0]&#8321;". When the protein was
# declared with more than one mass the label is prefixed with the proteoform the
# adduct sits on - without it two species carrying the same compound produce the
# same text, which reads as a duplicate in a table and is silently merged into
# one sector by a plotly pie.
#' @export
mass_shift_label <- function(
  theor_cmp,
  stoich,
  species_mass = NULL,
  multi_species = FALSE,
  sep = ""
) {
  valid <- !is.na(theor_cmp) & theor_cmp != "N/A"

  label <- if (!any(valid)) {
    "N/A"
  } else {
    paste(
      paste0(
        "[",
        theor_cmp[valid],
        "]",
        sep,
        sapply(stoich[valid], function(x) {
          as.character(htmltools::tags$sub(x))
        })
      ),
      collapse = " + "
    )
  }

  if (isTRUE(multi_species)) {
    # A literal separator, not an HTML entity: plotly text rendering resolves
    # only a small set of entities, a DT cell resolves them all
    paste0(fmt_species_mass(species_mass[1]), " Da · ", label)
  } else {
    label
  }
}

# Unbound-slice label, naming the proteoform when there is more than one
#' @export
unbound_label <- function(species_mass, multi_species = FALSE) {
  if (isTRUE(multi_species)) {
    paste0("Unbound ", fmt_species_mass(species_mass), " Da")
  } else {
    rep("Unbound Protein", length(species_mass))
  }
}

# Binding of every protein species on its own ----
#
# Total % Binding is the occupancy of the whole protein, all declared masses
# pooled. Split by species it is the complex signal of one species over that
# species' unbound plus complex signal. Proteoforms that react alike show the
# same binding, so the split is the check on the pooled value.
#
# `limit` flags a value pinned to 0 or 100 % because one form of the species was
# not detected: a minor proteoform's peaks drop below the deconvolution peak
# threshold well before the main species' do, and its binding then snaps to an
# end of the scale instead of being measured. Accepts the hits frame in its raw
# or its display column naming.
#
# A complex peak that fits complexes of several species (their masses plus the
# mass shifts coincide) cannot be attributed to any one of them: nothing in the
# spectrum says how much of it belongs to which. The binding of every species
# claiming it is then not resolvable in that sample - NA, `limit` "shared" -
# rather than resting on an assumed share. The pooled Total % Binding counts
# the peak once either way.
#' @export
proteoform_binding <- function(hits) {
  cols <- if ("Mw Protein [Da]" %in% names(hits)) {
    c(
      sample = "Sample",
      species = "Mw Protein [Da]",
      unbound = "Protein Intensity",
      peak = "Peak [Da]",
      complex = "Intensity",
      compound = "Compound"
    )
  } else {
    c(
      sample = "Sample ID",
      species = "Theor. Prot. [Da]",
      unbound = "Int. Prot. [%]",
      peak = "Peak Signal [Da]",
      complex = "Int. Cmp [%]",
      compound = "Cmp Name"
    )
  }

  empty <- data.frame(
    sample = character(0),
    protein = character(0),
    species = numeric(0),
    unbound = numeric(0),
    complex = numeric(0),
    binding = numeric(0),
    limit = character(0),
    shared = logical(0)
  )

  if (!is.data.frame(hits) || !nrow(hits) || !all(cols %in% names(hits))) {
    return(empty)
  }

  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  compound <- as.character(hits[[cols[["compound"]]]])

  df <- data.frame(
    sample = as.character(hits[[cols[["sample"]]]]),
    protein = if ("Protein" %in% names(hits)) {
      as.character(hits$Protein)
    } else {
      NA_character_
    },
    species = num(hits[[cols[["species"]]]]),
    unbound = num(hits[[cols[["unbound"]]]]),
    peak = num(hits[[cols[["peak"]]]]),
    complex = num(hits[[cols[["complex"]]]]),
    is_complex = is_complex_row(hits)
  )
  df <- df[!is.na(df$species), , drop = FALSE]

  if (!nrow(df)) {
    return(empty)
  }

  # Complex peaks claimed by more than one species: not attributable
  claimed <- df$is_complex & !is.na(df$complex)
  peak_key <- paste(df$sample, df$peak)
  n_species <- tapply(
    df$species[claimed],
    peak_key[claimed],
    function(x) length(unique(x))
  )
  df$shared <- FALSE
  df$shared[claimed] <- n_species[peak_key[claimed]] > 1

  out <- df |>
    dplyr::group_by(sample, protein, species) |>
    dplyr::summarise(
      # Repeated on every row of the species; 0 when it was not detected
      unbound = {
        u <- unbound[!is.na(unbound)]
        if (length(u)) u[1] else 0
      },
      # The species' own complex signal. A peak carrying several readings of
      # the species (compounds, mass shifts) contributes its intensity once; a
      # peak shared with another species is left out (see `shared`).
      complex = {
        keep <- is_complex & !is.na(complex) & !shared
        x <- complex[keep]
        sum(x[!duplicated(peak[keep])])
      },
      shared = any(shared),
      .groups = "drop"
    ) |>
    as.data.frame()

  total <- out$unbound + out$complex
  out$binding <- ifelse(
    out$shared | total <= 0,
    NA_real_,
    100 * out$complex / total
  )
  out$limit <- ifelse(
    out$shared,
    "shared",
    ifelse(
      total <= 0,
      NA_character_,
      ifelse(
        out$unbound <= 0,
        "unbound",
        ifelse(out$complex <= 0, "complex", NA_character_)
      )
    )
  )

  out[, c(names(empty)), drop = FALSE]
}

# Card text for a list of per-species values ----
#
# The summary cards have room for two proteoforms. Beyond that the values are
# cut after `max_shown` and the rest is named in the hover text of a trailing
# ellipsis. `values` may carry HTML; the hover text is its plain-text form.
strip_html <- function(x) {
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&ndash;", "–", x, fixed = TRUE)
  trimws(gsub("\\s+", " ", x))
}

ellipsis_html <- function(hidden, sep) {
  sprintf(
    "<span data-bs-toggle=\"tooltip\" title=\"%s\" style=\"cursor: help;\">…</span>",
    htmltools::htmlEscape(paste(strip_html(hidden), collapse = sep), TRUE)
  )
}

#' @export
collapse_species <- function(values, max_shown = 2, sep = " | ") {
  if (length(values) <= max_shown) {
    return(paste(values, collapse = sep))
  }
  paste(
    c(
      values[seq_len(max_shown)],
      ellipsis_html(values[-seq_len(max_shown)], "\n")
    ),
    collapse = sep
  )
}

# One line per declared species: theoretical mass and, next to it, the range of
# the unbound signals detected for it. Returns the lines as HTML and the matching
# label column, since both columns of a card have to stay line-aligned.
#' @export
species_mw_lines <- function(theor, measured, max_shown = 2) {
  theor <- suppressWarnings(as.numeric(as.character(theor)))
  measured <- suppressWarnings(as.numeric(as.character(measured)))
  species <- sort(unique(theor[!is.na(theor)]))

  fmt <- function(x) {
    format(round(x, 1), big.mark = ",", nsmall = 1, scientific = FALSE)
  }

  lines <- vapply(
    species,
    function(mw) {
      signals <- measured[theor %in% mw & !is.na(measured)]
      range <- if (!length(signals)) {
        "no signal"
      } else if (min(signals) == max(signals)) {
        fmt(signals[1])
      } else {
        paste0(fmt(min(signals)), " &ndash; ", fmt(max(signals)))
      }
      title <- if (length(signals) > 1) {
        " data-bs-toggle=\"tooltip\" title=\"Range of the detected signals\""
      } else if (length(signals) == 1) {
        " data-bs-toggle=\"tooltip\" title=\"Detected signal\""
      } else {
        ""
      }
      sprintf(
        "%s Da <span class=\"conversion-sample-protein-names\"%s>(%s)</span>",
        fmt(mw),
        title,
        range
      )
    },
    character(1)
  )

  if (length(lines) > max_shown) {
    lines <- c(
      lines[seq_len(max_shown)],
      ellipsis_html(lines[-seq_len(max_shown)], "\n")
    )
  }

  list(
    html = paste(lines, collapse = "<br>"),
    labels = paste(
      c("Mw", rep("&nbsp;", max(length(lines) - 1, 0))),
      collapse = "<br>"
    )
  )
}

# Declared masses of one protein or compound, read from its declaration table
# row (name column first, then "Mass 1" ...)
#' @export
declared_masses <- function(table, name) {
  if (is.null(table) || !nrow(table)) {
    return(numeric(0))
  }
  row <- table[!is.na(table[[1]]) & table[[1]] == name, -1, drop = FALSE]
  masses <- suppressWarnings(as.numeric(as.character(unlist(row))))
  unique(masses[!is.na(masses)])
}

# Whether each mass in `x` matches one in `ref`. The hits carry masses as
# formatted text, so an exact comparison may miss on rounding.
mass_matched <- function(x, ref, tol = 0.05) {
  vapply(x, function(v) any(abs(ref - v) < tol), logical(1))
}

# Entries of a Mass Shifts card: label, whether it was assigned to a peak and
# in how many samples
mass_entries <- function(
  label = character(0),
  present = logical(0),
  count = integer(0)
) {
  data.frame(label = label, present = present, count = count)
}

# Compound masses for the Mass Shifts card, one line per mass whatever its
# stoichiometry or proteoform: the masses assigned to a peak, followed by the
# declared ones that were not. A peak counts for its preferred hit only, as for
# the binding values - an alternative interpretation of a peak is not an
# occurrence of its mass. Masses only the hits know of are kept, for results
# loaded without their declaration.
#' @export
mass_shift_entries <- function(hits, declared = numeric(0)) {
  theor <- suppressWarnings(as.numeric(as.character(hits$`Theor. Cmp [Da]`)))
  preferred <- !is.na(hits$`Cmp Name`) &
    !is.na(theor) &
    as.character(hits$Preferred) %in% "TRUE"

  masses <- unique(c(declared, theor[preferred]))
  masses <- masses[!duplicated(round(masses, 1))]
  count <- vapply(
    masses,
    function(m) {
      length(unique(hits$`Sample ID`[preferred & abs(theor - m) < 0.05]))
    },
    integer(1)
  )
  order <- order(count == 0, masses)

  mass_entries(
    vapply(
      masses[order],
      function(m) paste(format(m, nsmall = 1, scientific = FALSE), "Da"),
      character(1)
    ),
    count[order] > 0,
    count[order]
  )
}

# Protein masses (its proteoforms) for the Mass Shifts card: the declared
# masses whose species shows a peak, followed by the ones without. Masses only
# the hits know of are kept, for results loaded without their declaration.
#' @export
protein_mass_entries <- function(hits, declared = numeric(0)) {
  theor <- suppressWarnings(as.numeric(as.character(hits$`Theor. Prot. [Da]`)))
  measured <- suppressWarnings(as.numeric(as.character(
    hits$`Meas. Prot. [Da]`
  )))
  detected <- unique(theor[!is.na(theor) & !is.na(measured)])

  masses <- unique(c(declared, theor[!is.na(theor)]))
  masses <- masses[!duplicated(round(masses, 1))]
  present <- mass_matched(masses, detected)
  shows <- !is.na(theor) & !is.na(measured)
  count <- vapply(
    masses,
    function(m) {
      length(unique(hits$`Sample ID`[shows & abs(theor - m) < 0.05]))
    },
    integer(1)
  )
  order <- order(!present, masses)

  mass_entries(
    paste(
      format(
        round(masses[order], 1),
        big.mark = ",",
        nsmall = 1,
        scientific = FALSE
      ),
      "Da"
    ),
    unname(present[order]),
    count[order]
  )
}

# Mass shifts of several compounds for one Mass Shifts card: the entries of
# every compound (see mass_shift_entries()), each label naming its compound
# once there is more than one. The masses assigned to a peak come first, so the
# lines the card has room for show what was found.
#' @export
compound_mass_entries <- function(hits, compounds, compound_table = NULL) {
  compounds <- unique(compounds[!is.na(compounds)])
  entries <- lapply(compounds, function(cmp) {
    e <- mass_shift_entries(
      hits[hits$`Cmp Name` %in% cmp, , drop = FALSE],
      declared = declared_masses(compound_table, cmp)
    )
    if (length(compounds) > 1 && nrow(e)) {
      e$label <- paste(
        htmltools::htmlEscape(cmp),
        "&middot;",
        e$label
      )
    }
    e
  })

  entries <- do.call(rbind, c(list(mass_entries()), entries))
  entries <- entries[order(!entries$present), , drop = FALSE]
  rownames(entries) <- NULL
  entries
}

# Mass Shifts card content from mass_entries(): the first `max_lines` entries,
# then a "N more" hint listing the rest on hover, up to `max_tooltip_lines` of
# them. Entries without a peak are greyed out and say so on hover. The hover
# lists are styled inline like the Protocol tab's warning tooltip, as bslib
# reparents them into a popup where stylesheet classes are unreliable.
#' @export
mass_shift_card <- function(
  entries,
  max_lines = 3,
  max_tooltip_lines = 20,
  accent = "#7777f9"
) {
  if (!nrow(entries)) {
    return(shiny::div("N/A", class = "na-placeholder"))
  }

  absent_hint <- "Declared, but not assigned to any peak"
  shown <- seq_len(min(nrow(entries), max_lines))
  hidden <- setdiff(seq_len(nrow(entries)), shown)

  count_hint <- function(n) {
    sprintf("Found in %d sample%s", n, if (n == 1) "" else "s")
  }

  line <- function(i) {
    if (entries$present[i]) {
      return(shiny::div(
        class = "mass-shift-line",
        shiny::span(class = "mass-shift-label", shiny::HTML(entries$label[i])),
        shiny::span(
          class = "mass-shift-count",
          `data-bs-toggle` = "tooltip",
          title = count_hint(entries$count[i]),
          paste0("×", entries$count[i])
        )
      ))
    }
    bslib::tooltip(
      shiny::div(
        class = "mass-shift-line mass-shift-absent",
        shiny::span(class = "mass-shift-label", shiny::HTML(entries$label[i]))
      ),
      absent_hint,
      placement = "auto"
    )
  }

  # The hover list shows one small line per entry and stops after
  # `max_tooltip_lines`, naming how many are left out - a tooltip cannot scroll
  listed <- utils::head(hidden, max_tooltip_lines)
  unlisted <- length(hidden) - length(listed)

  tooltip_items <- lapply(listed, function(i) {
    color <- if (entries$present[i]) accent else "#9a9a9a"
    shiny::div(
      style = paste(
        "display:flex; justify-content:space-between; align-items:baseline;",
        "gap:0.6rem; padding-left:0.4rem; border-left:2px solid",
        paste0(color, ";"),
        "color:",
        paste0(color, ";"),
        "font-weight:",
        if (entries$present[i]) "700;" else "400;",
        "white-space:nowrap;"
      ),
      shiny::span(
        style = "min-width:0; overflow:hidden; text-overflow:ellipsis;",
        shiny::HTML(entries$label[i])
      ),
      if (entries$present[i]) {
        shiny::span(
          style = paste(
            "flex-shrink:0; font-weight:400; opacity:0.8;",
            "font-variant-numeric:tabular-nums;"
          ),
          paste0("×", entries$count[i])
        )
      }
    )
  })

  tooltip_footer <- shiny::div(
    style = paste(
      "margin-top:0.25rem; color:#fff; opacity:0.85;",
      "white-space:nowrap;"
    ),
    if (unlisted) {
      shiny::tagList(
        shiny::div("…"),
        shiny::div(sprintf("%d more not listed", unlisted))
      )
    },
    if (any(!entries$present[listed])) {
      shiny::div(
        style = "color:#9a9a9a;",
        "Grey: declared, but not assigned to any peak"
      )
    }
  )

  shiny::div(
    class = "mass-shift-list",
    lapply(shown, line),
    if (length(hidden)) {
      bslib::tooltip(
        shiny::div(
          class = "mass-shift-more",
          shiny::icon("circle-info"),
          sprintf("%d more", length(hidden)),
          if (any(!entries$present[hidden])) {
            sprintf("(%d without peak)", sum(!entries$present[hidden]))
          }
        ),
        shiny::div(
          style = paste(
            "display:flex; flex-direction:column; gap:0.1rem;",
            "max-width:22rem; width:max-content; text-align:left;",
            "font-size:0.7rem; line-height:1.25;"
          ),
          tooltip_items,
          tooltip_footer
        ),
        placement = "auto"
      )
    }
  )
}

# Whether any protein of the proteoform binding table carries several species
#' @export
has_proteoforms <- function(binding) {
  if (!nrow(binding)) {
    return(FALSE)
  }
  protein <- ifelse(is.na(binding$protein), "", binding$protein)
  any(tapply(binding$species, protein, function(x) length(unique(x))) > 1)
}

# Hover note for a proteoform binding value pinned by an undetected peak, or
# not resolvable because its complex peak also fits another proteoform
#' @export
proteoform_limit_note <- function(limit) {
  ifelse(
    is.na(limit),
    NA_character_,
    ifelse(
      limit == "shared",
      "Complex peak also fits another proteoform: binding not resolvable",
      ifelse(
        limit == "unbound",
        "Unbound peak not detected: binding may be overestimated",
        "No complex detected: binding may be underestimated"
      )
    )
  )
}

# Colours of the proteoforms in the comparison plots, keyed by species mass;
# the pooled fit is drawn in the font colour. The categorical colours are
# taken cool ones first: the proteoforms are drawn over the k_obs curve,
# whose concentrations run from violet to orange.
#' @export
proteoform_colors <- function(species, theme = "light") {
  cool_first <- c(1L, 6L, 4L, 9L, 11L, 8L, 7L, 3L, 10L, 12L, 5L, 2L)
  palette <- categorical_palette[[
    if (identical(theme, "dark")) {
      "dark"
    } else {
      "light"
    }
  ]][cool_first]
  species <- sort(unique(species))
  stats::setNames(
    palette[(seq_along(species) - 1) %% length(palette) + 1],
    as.character(species)
  )
}

# Main species of each protein, the reference of the paired comparisons: the
# one carrying the most signal on average. The declared mass order does not
# survive the hit screening, and the native form is the dominant one in any
# usable preparation. Only species whose binding is measured (not pinned to a
# detection limit) in at least one sample qualify, as long as one does: when an
# unbound mass coincides with a complex, the complex peak makes it the
# strongest species, yet its binding sits at 0 % in every sample and every
# pair with it would be pinned.
#' @export
main_proteoform <- function(binding) {
  binding$total <- binding$unbound + binding$complex
  binding$protein[is.na(binding$protein)] <- ""
  share <- binding |>
    dplyr::group_by(sample, protein) |>
    dplyr::mutate(share = total / sum(total)) |>
    dplyr::group_by(protein, species) |>
    dplyr::summarise(
      share = mean(share, na.rm = TRUE),
      measured = sum(is.na(limit) & !is.na(binding)),
      .groups = "drop"
    ) |>
    dplyr::group_by(protein) |>
    dplyr::mutate(
      main = seq_along(species) == order(-(measured > 0), -share)[1]
    ) |>
    dplyr::ungroup() |>
    as.data.frame()
  share
}

# Binding kinetics of every proteoform on its own ----
#
# Runs the pooled kinetics - k_obs per concentration, then kinact/KI - on the
# binding of one species at a time, over the same concentrations, so each fit
# compares like for like with the pooled one. Expects the raw hits summary the
# pooled kinetics were fitted on. NULL when there is only one species.
#' @export
proteoform_kinetics <- function(
  hits_summary,
  units,
  conc_time,
  concentrations_select = NULL
) {
  binding <- proteoform_binding(hits_summary)

  if (!has_proteoforms(binding)) {
    return(NULL)
  }

  conc_col <- gsub("Conc.", "Concentration", conc_time[["Concentration"]])
  species <- sort(unique(binding$species))

  lapply(stats::setNames(species, as.character(species)), function(s) {
    b <- binding[binding$species == s, , drop = FALSE]

    # One row per sample carrying the species, with its own binding in place of
    # the pooled one
    rows <- hits_summary[
      !is.na(hits_summary$`Mw Protein [Da]`) &
        hits_summary$`Mw Protein [Da]` == s,
      ,
      drop = FALSE
    ]
    rows <- rows[!duplicated(rows$Sample), , drop = FALSE]
    rows$binding <- b$binding[match(rows$Sample, b$sample)]

    # Samples whose complex peak also fits another species have no binding of
    # their own (see proteoform_binding()); they leave the fit but are counted
    shared <- rows[
      b$limit[match(rows$Sample, b$sample)] %in% "shared",
      ,
      drop = FALSE
    ]
    if (!is.null(concentrations_select) && conc_col %in% names(shared)) {
      shared <- shared[
        shared[[conc_col]] %in% concentrations_select,
        ,
        drop = FALSE
      ]
    }
    n_shared <- nrow(shared)

    rows <- rows[!is.na(rows$binding), , drop = FALSE]

    if (!is.null(concentrations_select) && conc_col %in% names(rows)) {
      used <- rows[rows[[conc_col]] %in% concentrations_select, , drop = FALSE]
    } else {
      used <- rows
    }
    limit <- b$limit[match(used$Sample, b$sample)]

    # The pooled fit logs every step; for the comparison only the result counts
    kobs <- tryCatch(
      suppressWarnings(suppressMessages(add_kobs_binding_result(
        rows,
        concentrations_select = concentrations_select,
        units = units,
        conc_time = conc_time
      ))),
      error = function(e) NULL
    )
    if (!is.null(kobs) && nrow(kobs$kobs_result_table) == 0) {
      kobs <- NULL
    }

    kinact_ki <- if (!is.null(kobs)) {
      tryCatch(
        suppressWarnings(suppressMessages(compute_kinact_ki(
          kobs,
          units = units
        ))),
        error = function(e) NULL
      )
    }

    # A negative kinact or KI has no physical meaning - it comes from k_obs
    # values too few or too scattered to show the hyperbola - so such a fit
    # is dropped rather than drawn and tabulated. kinact and KI are NA when the
    # data do not saturate; kinact/KI is then the only estimate to check.
    nonphysical <- !is.null(kinact_ki) &&
      any(
        c(kinact_ki$Params[1:2, 1], kinact_ki$Ratio[["Estimate"]]) <= 0,
        na.rm = TRUE
      )
    if (isTRUE(nonphysical)) {
      kinact_ki <- NULL
    }

    # Why a species has no kinact/KI, for the hover of its N/A cells. A
    # species without binding anywhere (e.g. an unbound mass that coincides
    # with a complex, or a form whose complex is never detected) is common
    # enough to be named on its own; its plots leave out the row of zeros.
    n_response <- if (is.null(kobs)) {
      0L
    } else {
      sum(kobs$kobs_result_table$kobs > 0, na.rm = TRUE)
    }
    # Too little data left because samples were not resolvable: that is the
    # reason to name, not the shortage it causes
    short_of_data <- is.null(kobs) || n_response < 3
    reason <- if (!is.null(kinact_ki)) {
      NULL
    } else if (isTRUE(nonphysical)) {
      "The fit gave a negative parameter"
    } else if (n_shared > 0 && short_of_data) {
      sprintf(
        paste(
          "Its complex peak also fits another proteoform in %d sample%s:",
          "binding not resolvable there"
        ),
        n_shared,
        if (n_shared == 1) "" else "s"
      )
    } else if (is.null(kobs)) {
      "k_obs could not be fitted at any concentration"
    } else if (n_response == 0) {
      "No binding at any concentration"
    } else if (n_response < 3) {
      sprintf(
        "Binding at %d concentration%s only, at least 3 are needed",
        n_response,
        if (n_response == 1) "" else "s"
      )
    } else {
      "The kinact/KI fit did not converge"
    }

    list(
      species = s,
      binding_kobs_result = kobs,
      kinact_ki_result = kinact_ki,
      nonphysical = isTRUE(nonphysical),
      no_response = !is.null(kobs) && n_response == 0 && n_shared == 0,
      reason = reason,
      n_samples = nrow(used),
      n_limit = sum(!is.na(limit)),
      # Samples left out because the binding is not resolvable
      n_shared = n_shared
    )
  })
}

# Comparison table of the pooled and the per-proteoform kinetics ----
#
# `kinetics` from proteoform_kinetics(), `pooled` the pooled kinact/KI
# result, both already in the displayed units; `binding` from
# proteoform_binding(). Δ Binding is the mean paired difference to the main
# species over the samples where both values are measured, which isolates a
# difference in reactivity from the detection-limit artefacts of the fits.
#' @export
proteoform_comparison_table <- function(kinetics, pooled, binding, view) {
  fmt <- function(x) {
    if (is.null(x) || !length(x) || is.na(x)) {
      return("N/A")
    }
    as.character(format_scientific(x))
  }
  # The global fit reports kinact/KI on its own (Ratio), also when the data do
  # not saturate and kinact and KI are not determinable
  ratio_of <- function(result) {
    if (is.null(result)) {
      return(NA_real_)
    }
    if (!is.null(result$Ratio)) {
      return(unname(result$Ratio[["Estimate"]]))
    }
    result$Params[1, 1] / result$Params[2, 1]
  }

  pooled_ratio <- ratio_of(pooled)
  pooled_params <- pooled$Params
  shares <- main_proteoform(binding)
  main <- shares$species[shares$main][1]
  main_b <- binding[binding$species == main, , drop = FALSE]

  species_rows <- lapply(kinetics, function(k) {
    params <- k$kinact_ki_result$Params
    ratio <- ratio_of(k$kinact_ki_result)
    share <- shares$share[shares$species == k$species][1]

    # Fitted mostly on values pinned by an undetected peak, the parameters
    # describe the detection limit rather than the proteoform
    unreliable <- k$n_samples > 0 && k$n_limit / k$n_samples > 0.5
    # The fitted value is not shown at all: it would describe the detection
    # limit (e.g. a form read at 100 % from the first time point gets an
    # arbitrarily high rate), and a number invites reading it anyway
    flag <- function(x) {
      if (x == "N/A" || !unreliable) {
        return(x)
      }
      sprintf(
        "<span class=\"protocol-stat-warn\" data-bs-toggle=\"tooltip\" title=\"%s\">not meaningful</span>",
        paste(
          "More than half of the values sit at a detection limit, so the fit",
          "describes the limit, not the proteoform"
        )
      )
    }
    # Say why a proteoform has no fit when the reason is known
    not_fitted <- function(x) {
      if (x != "N/A" || is.null(k$reason)) {
        return(x)
      }
      sprintf(
        "<span data-bs-toggle=\"tooltip\" title=\"%s\">N/A</span>",
        k$reason
      )
    }

    delta <- if (identical(k$species, main)) {
      "Reference"
    } else {
      b <- binding[binding$species == k$species, , drop = FALSE]
      paired <- merge(
        b[, c("sample", "binding", "limit")],
        main_b[, c("sample", "binding", "limit")],
        by = "sample",
        suffixes = c("", "_main")
      )
      paired <- paired[
        is.na(paired$limit) &
          is.na(paired$limit_main) &
          !is.na(paired$binding) &
          !is.na(paired$binding_main),
        ,
        drop = FALSE
      ]
      if (!nrow(paired)) {
        sprintf(
          "<span data-bs-toggle=\"tooltip\" title=\"%s\">N/A</span>",
          "No sample where both species are measured"
        )
      } else {
        d <- paired$binding - paired$binding_main
        sprintf(
          "%+.2f%s (n = %d)",
          mean(d),
          if (length(d) > 1) sprintf(" ± %.2f", stats::sd(d)) else "",
          length(d)
        )
      }
    }

    data.frame(
      Proteoform = paste0(fmt_species_mass(k$species), " Da"),
      Signal = sprintf("%.1f", 100 * share),
      kinact = not_fitted(flag(fmt(if (!is.null(params)) params[1, 1]))),
      KI = not_fitted(flag(fmt(if (!is.null(params)) params[2, 1]))),
      ratio = not_fitted(flag(fmt(ratio))),
      vs_pooled = if (is.na(ratio) || is.na(pooled_ratio)) {
        "N/A"
      } else {
        flag(sprintf("%+.1f", 100 * (ratio / pooled_ratio - 1)))
      },
      delta = delta,
      # Flagged on its own as well, so the warning also shows when there is
      # no fitted value to flag
      limit = paste0(
        if (unreliable) {
          sprintf(
            "<span class=\"protocol-stat-warn\" data-bs-toggle=\"tooltip\" title=\"%s\">%d / %d</span>",
            "More than half of the values sit at a detection limit",
            k$n_limit,
            k$n_samples
          )
        } else {
          sprintf("%d / %d", k$n_limit, k$n_samples)
        },
        # Samples left out of the fit as not resolvable
        if (isTRUE(k$n_shared > 0)) {
          sprintf(
            " · <span class=\"protocol-stat-warn\" data-bs-toggle=\"tooltip\" title=\"%s\">%d not resolvable</span>",
            "Samples whose complex peak also fits another proteoform: left out of this fit",
            k$n_shared
          )
        }
      )
    )
  })

  pooled_row <- data.frame(
    Proteoform = "Pooled",
    Signal = "100.0",
    kinact = fmt(if (!is.null(pooled_params)) pooled_params[1, 1]),
    KI = fmt(if (!is.null(pooled_params)) pooled_params[2, 1]),
    ratio = fmt(pooled_ratio),
    vs_pooled = "",
    delta = "",
    limit = ""
  )

  # The reference species first, right under the pooled fit
  is_main <- vapply(
    kinetics,
    function(k) identical(k$species, main),
    logical(1)
  )
  species_rows <- c(species_rows[is_main], species_rows[!is_main])

  tbl <- do.call(rbind, c(list(pooled_row), unname(species_rows)))

  names(tbl) <- c(
    "Proteoform",
    "Signal [%]",
    paste0("k<sub> inact</sub> [", view$time_unit, "⁻¹]"),
    paste0("K<sub>&thinsp;I</sub> [", view$conc_unit, "]"),
    paste0(
      "k<sub> inact</sub>/K<sub>&thinsp;I</sub> [",
      view$conc_unit,
      "⁻¹ ",
      view$time_unit,
      "⁻¹]"
    ),
    paste0(
      "<span data-bs-toggle=\"tooltip\" title=\"Relative difference of the proteoform's kinact/KI ",
      "to the pooled kinact/KI\">Δ vs Pooled [%]</span>"
    ),
    paste0(
      "<span data-bs-toggle=\"tooltip\" title=\"Mean difference of the proteoform's binding to the ",
      "binding of the reference proteoform, in percentage points, over the ",
      "samples where both are measured (± SD, n = samples). Negative: it ",
      "binds less.\">Δ Binding vs Reference [pp]</span>"
    ),
    "Limit Values"
  )

  tbl
}

# k_obs per proteoform ----
#
# Observed k_obs with standard errors and the fitted hyperbola of every
# proteoform over the pooled fit. `entries` is a named list, the pooled entry
# named "Pooled" and the others by species mass, each holding `kobs` (the
# k_obs result table) and `kinact_ki` (the kinact/KI result), in the displayed
# units. The k_obs points come from the table, so a proteoform whose hyperbola
# could not be fitted still shows them.
#' @export
proteoform_kobs_plot <- function(entries, colors, units, theme = "light") {
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
  conc_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]])
  time_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])

  plotly::plot_ly() |>
    add_proteoform_kobs_traces(
      entries,
      colors = colors,
      font_color = font_color,
      conc_unit = conc_unit,
      time_unit = time_unit,
      subtle = "Pooled"
    ) |>
    ensure_trace() |>
    plotly::layout(
      hovermode = "closest",
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(size = 14, color = font_color),
      legend = list(
        title = list(text = "Proteoform  ", font = list(color = font_color)),
        bgcolor = "rgba(0,0,0,0)",
        bordercolor = "rgba(0,0,0,0)",
        font = list(color = font_color)
      ),
      xaxis = c(
        list(
          title = paste0("Compound [", conc_unit, "]"),
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
}

# k_obs points and fitted hyperbola of every entry of `entries` (see
# proteoform_kobs_plot()). Shared by the Proteoforms tab and the k_obs curve of
# the Binding tab; `group_title` puts the proteoform rows under a legend
# heading of their own when they join another legend.
#
# Entries named in `subtle` are context rather than the subject of the plot and
# are drawn in the background: thin dotted line, small open markers, no error
# bars, faded. The Binding tab passes the proteoforms, the Proteoforms tab the
# pooled fit.
#
# With `extrapolate_to` the fitted curve of every entry is continued as a
# dashed line from its highest concentration to that concentration.
add_proteoform_kobs_traces <- function(
  plot,
  entries,
  colors,
  font_color,
  conc_unit,
  time_unit,
  group_title = NULL,
  subtle = character(0),
  extrapolate_to = NULL
) {
  # Entries with k_obs points, and of those the ones without binding at any
  # concentration (drawn hidden, see below)
  has_points <- function(label) {
    tbl <- entries[[label]]$kobs
    !is.null(tbl) && nrow(tbl) > 0
  }
  no_binding <- function(label) {
    tbl <- entries[[label]]$kobs
    k <- tbl$kobs[as.numeric(rownames(tbl)) > 0 & !is.na(tbl$kobs)]
    label != "Pooled" && length(k) > 0 && all(k == 0)
  }
  # A species with more than half of its values at a detection limit (its
  # `limit_share`) has k_obs that describe the limit, not the proteoform: a
  # form read at 100 % from the first time point gets a k_obs only bounded by
  # that time point, often far above all others, and the axis it stretches
  # flattens every real curve. It is drawn hidden as well.
  pinned <- function(label) {
    label != "Pooled" && isTRUE(entries[[label]]$limit_share > 0.5)
  }
  hidden <- function(label) no_binding(label) || pinned(label)
  drawn <- Filter(has_points, names(entries))
  # Hidden species come last: plotly draws a legend group title in the style
  # of the group's first row, and a hidden first row greys it out. The title
  # goes on the first visible trace.
  ordered <- c(
    Filter(Negate(hidden), names(entries)),
    Filter(hidden, names(entries))
  )
  title_label <- c(Filter(Negate(hidden), drawn), drawn)[1]

  for (label in ordered) {
    kobs_tbl <- entries[[label]]$kobs
    if (is.null(kobs_tbl) || !nrow(kobs_tbl)) {
      next
    }
    background <- label %in% subtle
    color <- if (label == "Pooled") font_color else unname(colors[label])
    name <- if (label == "Pooled") {
      "Pooled"
    } else {
      paste0(fmt_species_mass(label), " Da")
    }
    group <- if (is.null(group_title)) name else "proteoforms"
    symbol <- if (label == "Pooled") "diamond" else "circle"

    points <- data.frame(
      conc = as.numeric(rownames(kobs_tbl)),
      kobs = kobs_tbl$kobs,
      kobs_se = kobs_tbl$kobs_se
    )
    points <- points[!is.na(points$kobs) & points$conc > 0, , drop = FALSE]
    points$kobs_se[is.na(points$kobs_se)] <- 0

    # A species without binding at any concentration would draw a row of
    # zeros, exactly on top of any other such species, so that all but the
    # last stay hidden. Its legend row says so, and its zeros start hidden: a
    # click on the row shows them.
    if (no_binding(label)) {
      args <- list(
        p = plot,
        data = points,
        x = ~conc,
        y = ~kobs,
        visible = "legendonly",
        name = paste0(name, " · no binding"),
        legendgroup = group,
        opacity = if (background) 0.5 else 1,
        marker = list(
          size = if (background) 6 else 10,
          color = "rgba(0,0,0,0)",
          symbol = paste0(symbol, "-open"),
          line = list(width = 1, color = color)
        ),
        hovertemplate = paste0(
          "<b>",
          name,
          "</b><br>No binding at ",
          "%{x} ",
          conc_unit,
          "<extra></extra>"
        ),
        showlegend = TRUE,
        inherit = FALSE
      )
      if (!is.null(group_title) && identical(label, title_label)) {
        args$legendgrouptitle <- list(
          text = group_title,
          font = list(color = font_color)
        )
      }
      plot <- do.call(plotly::add_markers, args)
      next
    }

    # Mostly limit values: hidden until its legend row is clicked
    visible <- if (pinned(label)) "legendonly" else TRUE
    legend_name <- if (pinned(label)) {
      paste0(name, " · limit values")
    } else {
      name
    }

    kinact_ki <- entries[[label]]$kinact_ki
    kd <- kinact_ki$Kobs_Data
    if (!is.null(kd) && nrow(kd) > 0) {
      kd <- kd[!is.na(kd$predicted_kobs), , drop = FALSE]
      plot <- plotly::add_lines(
        plot,
        data = kd,
        x = ~conc,
        y = ~predicted_kobs,
        visible = visible,
        name = name,
        legendgroup = group,
        opacity = if (background) 0.5 else 1,
        line = list(
          width = if (background) 1 else 1.5,
          color = color,
          dash = if (background) "dot" else "solid"
        ),
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      )

      fit_end <- if (nrow(kd)) max(kd$conc) else NA_real_
      if (
        !is.null(extrapolate_to) &&
          is.finite(fit_end) &&
          extrapolate_to > fit_end
      ) {
        grid_out <- seq(fit_end, extrapolate_to, length.out = 200)
        kobs_out <- kobs_model_at(kinact_ki, grid_out)
        if (length(kobs_out) == length(grid_out) && all(is.finite(kobs_out))) {
          plot <- plotly::add_lines(
            plot,
            x = grid_out,
            y = kobs_out,
            visible = visible,
            name = name,
            legendgroup = group,
            opacity = if (background) 0.5 else 1,
            line = list(
              width = if (background) 1 else 1.5,
              color = color,
              dash = "dash"
            ),
            hoverinfo = "skip",
            showlegend = FALSE,
            inherit = FALSE
          )
        }
      }
    }

    args <- list(
      p = plot,
      data = points,
      x = ~conc,
      y = ~kobs,
      visible = visible,
      name = legend_name,
      legendgroup = group,
      opacity = if (background) 0.5 else 1,
      marker = if (background) {
        list(
          size = 6,
          color = color,
          symbol = paste0(symbol, "-open"),
          line = list(width = 1, color = color)
        )
      } else {
        list(
          size = 10,
          color = color,
          symbol = symbol,
          line = list(width = 1, color = font_color)
        )
      },
      hovertemplate = paste0(
        "<b>",
        name,
        "</b><br>Concentration: %{x} ",
        conc_unit,
        "<br>k<sub>obs</sub>: %{y:.4g} ",
        time_unit,
        "⁻¹<extra></extra>"
      ),
      showlegend = TRUE,
      inherit = FALSE
    )
    if (!background) {
      args$error_y <- list(
        type = "data",
        array = ~kobs_se,
        visible = TRUE,
        thickness = 1.5,
        width = 4,
        color = color
      )
    }
    if (!is.null(group_title) && identical(label, title_label)) {
      args$legendgrouptitle <- list(
        text = group_title,
        font = list(color = font_color)
      )
    }
    plot <- do.call(plotly::add_markers, args)
  }

  plot
}

# Paired binding of the proteoforms ----

# Binding of species `s` against the main species' (`main_b`) in every sample
# where both are measured. A pair is pinned when either value sits at a
# detection limit; `note` then says which.
proteoform_pairs <- function(binding, s, main_b) {
  paired <- merge(
    binding[binding$species == s, c("sample", "binding", "limit")],
    main_b[, c("sample", "binding", "limit")],
    by = "sample",
    suffixes = c("", "_main")
  )
  paired <- paired[
    !is.na(paired$binding) & !is.na(paired$binding_main),
    ,
    drop = FALSE
  ]

  paired$note <- ifelse(
    !is.na(paired$limit),
    proteoform_limit_note(paired$limit),
    proteoform_limit_note(paired$limit_main)
  )
  paired$pinned <- !is.na(paired$note)
  paired
}

# Whether proteoform_paired_plot() has any pinned pair, i.e. anything for its
# `show_limits` to show
#' @export
proteoform_paired_has_limits <- function(binding) {
  shares <- main_proteoform(binding)
  main <- shares$species[shares$main][1]
  main_b <- binding[binding$species == main, , drop = FALSE]
  others <- setdiff(unique(binding$species), main)

  any(vapply(
    others,
    function(s) any(proteoform_pairs(binding, s, main_b)$pinned),
    logical(1)
  ))
}

# Every other species' binding against the main species', one point per sample.
# Proteoforms that react alike sit on the diagonal.
#
# Values pinned to 0 or 100 % by an undetected peak are left out unless
# `show_limits`: they say nothing about reactivity, and with several minor
# proteoforms they stack on the 0 and 100 % lines and hide the measured points.
# The species are named in the legend; the y axis only names the mass when a
# single minor proteoform has points, as more would not fit on it.
#' @export
proteoform_paired_plot <- function(
  binding,
  colors,
  theme = "light",
  show_limits = FALSE
) {
  font_color <- if (theme == "light") "black" else "white"
  grid_color <- if (theme == "light") {
    "rgba(0,0,0,0.1)"
  } else {
    "rgba(255,255,255,0.2)"
  }

  shares <- main_proteoform(binding)
  main <- shares$species[shares$main][1]
  main_b <- binding[binding$species == main, , drop = FALSE]
  main_label <- paste0(fmt_species_mass(main), " Da")
  others <- setdiff(sort(unique(binding$species)), main)

  plot <- plotly::plot_ly() |>
    plotly::add_lines(
      x = c(0, 100),
      y = c(0, 100),
      line = list(color = font_color, width = 1, dash = "dot"),
      hovertemplate = paste(
        "Same binding on both species:<br>points here react alike,",
        "<br>below it the other proteoform binds less<extra></extra>"
      ),
      showlegend = FALSE,
      inherit = FALSE
    )

  any_pinned <- FALSE
  # Species that end up with points; one with only limit values drops out
  shown <- numeric(0)
  # Species left without points because all their pairs are pinned
  hidden_limits <- 0

  for (s in others) {
    paired <- proteoform_pairs(binding, s, main_b)
    label <- paste0(fmt_species_mass(s), " Da")
    color <- unname(colors[as.character(s)])

    # A species with no measured pair keeps its legend row and says why it
    # has no points (or, with the limits shown, only open ones that may lie
    # under another species' open points)
    limits_only <- nrow(paired) > 0 && all(paired$pinned)
    if (limits_only) {
      label <- paste0(label, " · limits only")
    }
    if (!show_limits) {
      paired <- paired[!paired$pinned, , drop = FALSE]
    }
    if (!nrow(paired)) {
      if (limits_only) {
        hidden_limits <- hidden_limits + 1
        plot <- plotly::add_markers(
          plot,
          x = -100,
          y = -100,
          name = label,
          legendgroup = legend_key_group(label),
          marker = list(
            size = 7,
            color = "rgba(0,0,0,0)",
            symbol = "circle",
            line = list(width = 1, color = color)
          ),
          hoverinfo = "skip",
          inherit = FALSE
        )
      }
      next
    }
    shown <- c(shown, s)
    paired$hover <- paste0(
      paired$sample,
      "<br>",
      main_label,
      ": ",
      sprintf("%.2f", paired$binding_main),
      "%<br>",
      label,
      ": ",
      sprintf("%.2f", paired$binding),
      "%",
      ifelse(paired$pinned, paste0("<br><i>", paired$note, "</i>"), "")
    )

    # Measured points first, so the legend row of the species shows its fill
    for (pinned in c(FALSE, TRUE)) {
      pts <- paired[paired$pinned == pinned, , drop = FALSE]
      if (!nrow(pts)) {
        next
      }
      any_pinned <- any_pinned || pinned
      plot <- plotly::add_markers(
        plot,
        data = pts,
        x = ~binding_main,
        y = ~binding,
        name = label,
        legendgroup = label,
        # A species with only limit values still needs its legend row
        showlegend = !pinned || all(paired$pinned),
        # Translucent, so that points of several proteoforms lying on top of
        # each other still show through
        opacity = if (pinned) 0.6 else 0.8,
        marker = list(
          size = 7,
          color = if (pinned) "rgba(0,0,0,0)" else color,
          symbol = "circle",
          line = list(width = 1, color = if (pinned) color else font_color)
        ),
        text = ~hover,
        hoverinfo = "text",
        inherit = FALSE
      )
    }
  }

  # One legend row explains the open symbol for every proteoform
  if (any_pinned) {
    plot <- plotly::add_markers(
      plot,
      x = -100,
      y = -100,
      name = "Detection limit",
      legendgroup = legend_key_group("limit"),
      marker = list(
        size = 7,
        color = "rgba(0,0,0,0)",
        symbol = "circle",
        line = list(width = 1, color = font_color)
      ),
      hoverinfo = "skip",
      inherit = FALSE
    )
  }

  axis <- function(title) {
    list(
      title = title,
      range = c(-3, 103),
      color = font_color,
      showgrid = TRUE,
      gridcolor = grid_color,
      zeroline = FALSE
    )
  }

  plot |>
    plotly::layout(
      hovermode = "closest",
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(size = 14, color = font_color),
      legend = list(
        title = list(text = "Proteoform  ", font = list(color = font_color)),
        bgcolor = "rgba(0,0,0,0)",
        bordercolor = "rgba(0,0,0,0)",
        font = list(color = font_color)
      ),
      # x: the reference species, y: each other proteoform, in the same sample
      xaxis = axis(paste0("Binding of ", main_label, " (reference) [%]")),
      yaxis = axis(
        if (length(shown) == 1) {
          paste0("Binding of ", fmt_species_mass(shown), " Da [%]")
        } else {
          "Binding of other proteoform [%]"
        }
      ),
      # Without any point the plot would be an unexplained diagonal
      annotations = if (!length(shown)) {
        list(list(
          text = paste0(
            "No sample where another proteoform and the<br>",
            "reference are both measured",
            if (hidden_limits > 0 && !show_limits) {
              "<br><i>Show Limit Values (settings) shows the pinned values</i>"
            } else {
              ""
            }
          ),
          x = 50,
          y = 50,
          xref = "x",
          yref = "y",
          showarrow = FALSE,
          bgcolor = if (theme == "light") {
            "rgba(255,255,255,0.8)"
          } else {
            "rgba(0,0,0,0.6)"
          },
          font = list(size = 13, color = font_color)
        ))
      }
    )
}

# Conversion of intensities to fractional % binding
conversion <- function(hits) {
  # Check 'hits' argument validity
  if (!is.data.frame(hits) || nrow(hits) < 1) {
    log_err_no_df()
    return(NULL)
  } else if (ncol(hits) != 16) {
    log_err_cols(ncol(hits))
    return(NULL)
  } else if (all(is.na(hits$intensity))) {
    # Case no complex hits. With an unbound species detected the binding is a
    # measured 0 % (below the adduct detection limit). Without any peak of the
    # declared protein - neither unbound nor complex: a failed acquisition, or
    # a converted protein whose adduct is not declared - it is 0 / 0, not
    # measured. NA rather than 0, which would pull the kinetic fits down (a
    # failed sample is "no data", Resnick et al. 2019, JACS 141, 8951).
    I_total <- sum(unbound_species(hits)$prot_intensity) # Total intensity
    measured <- I_total > 0
    if (!measured) {
      log_alert("No protein or complex peak found, binding not measurable")
    }
    hits <- dplyr::mutate(hits, `%binding` = if (measured) 0 else NA_real_)
    hits <- dplyr::mutate(
      hits,
      `%binding_tot` = if (measured) 0 else NA_real_,
      .before = peak
    )
  } else {
    # Complex signal. Counted per peak, not per row: one peak can carry several
    # interpretations (different compounds, mass shifts or protein species) and
    # must contribute its intensity only once.
    complex_peaks <- hits[!is.na(hits$intensity), c("peak", "intensity")]
    complex_peaks <- complex_peaks[!duplicated(complex_peaks$peak), ]
    I_complex <- sum(complex_peaks$intensity)

    # Unbound signal, summed over all detected protein species. Every declared
    # mass is a proteoform of the same protein, so all of their apo peaks belong
    # to the unbound pool the occupancy is measured against.
    I_prot <- sum(unbound_species(hits)$prot_intensity)

    # Total intensity
    I_total <- I_complex + I_prot

    # Protein only binding
    perc_bind_prot <- I_prot / I_total

    # Adding %Binding values to hit data frame. Rows of an unbound species carry
    # no complex signal and therefore no binding. A peak claimed by several
    # preferred interpretations - complexes of different proteoforms, or of
    # different compounds - is split evenly between them, so the per-row
    # values still add up to the total instead of counting the peak twice.
    # This is bookkeeping for the row values only: the binding of a single
    # proteoform treats such a peak as not resolvable (proteoform_binding()).
    n_claims <- stats::ave(
      as.numeric(hits$preferred %in% TRUE),
      hits$peak,
      FUN = sum
    )
    n_claims[is.na(n_claims) | n_claims < 1] <- 1
    hits <- dplyr::mutate(
      hits,
      `%binding` = dplyr::if_else(
        is.na(intensity),
        0,
        intensity / n_claims / I_total
      )
    )
    hits <- dplyr::mutate(
      hits,
      `%binding_tot` = I_complex / I_total,
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
      I_prot,
      I_complex
    )
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
    "% Correct",
    "% Unmatched",
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

# A peak with more than one reading. Readings of one compound on one species
# are resolved by the preference rule, readings of different species share the
# peak (counted once, not resolvable per species); the header says which
# happened and every reading follows,
# the preferred ones ticked. The header is a warning: the Warnings card of the
# Protocol tab lists it, folded per resolution.
log_ambiguous_peak <- function(hits, preference) {
  group <- paste(hits$theor_prot, hits$compound, sep = "\r")
  preferred <- hits$preferred %in% TRUE
  n_split <- sum(preferred)
  n_species <- length(unique(hits$theor_prot[preferred]))

  how <- c(
    if (any(duplicated(group))) {
      paste(
        tolower(hit_preference_rules[[hit_preference_rule(preference)]]$label),
        "preferred"
      )
    },
    if (n_split > 1) {
      sprintf(
        "shared by %d %s",
        n_split,
        if (n_species == n_split) "proteoforms" else "readings"
      )
    }
  )
  message(sprintf(
    "  ├─ %s Ambiguous assignment at %.2f Da (%s)",
    .col_warn(warning_sym),
    hits$peak[1],
    paste(how, collapse = "; ")
  ))

  labels <- interpretation_label(data.frame(
    type = "complex",
    species = hits$theor_prot,
    compound = hits$compound,
    cmp_mass = hits$cmp_mass,
    multiple = hits$multiple
  ))
  for (i in seq_len(nrow(hits))) {
    message(sprintf(
      "  │  %s %s %s, Δ %.2f Da",
      if (i == nrow(hits)) "└─" else "├─",
      if (preferred[i]) "✓" else "·",
      labels[i],
      hits$delta_cmp[i]
    ))
  }
}

# A peak read as an unbound species that also fits complexes of other species
log_species_complex_overlap <- function(peak_mass, species_mass, alt) {
  message(sprintf(
    "  ├─ %s Peak %s Da read as unbound %s Da, also fits:",
    .col_warn(warning_sym),
    peak_mass,
    fmt_species_mass(species_mass)
  ))
  for (label in interpretation_label(alt)) {
    message(sprintf("  │  └─ %s", label))
  }
}

# Declaration-level ambiguities, logged once at the start of a run
#' @export
log_mass_ambiguities <- function(amb, tolerance) {
  if (!nrow(amb)) {
    return(invisible(NULL))
  }
  kinds <- c(
    species = "read as unbound",
    proteoform = "not resolvable per form",
    compounds = "not distinguishable"
  )
  # Found from the declared masses; each rule only acts on a detected peak
  # that matches both readings of its pair
  message(sprintf(
    "AMBIGUOUS MASS ASSIGNMENTS (within 2 × %s Da)\n  │  rule applied when a peak matches both\n  │",
    tolerance
  ))
  # One short branch per pair, its two readings below it
  for (k in seq_len(nrow(amb))) {
    last <- k == nrow(amb)
    message(sprintf(
      "  %s %s %s (Δ %.1f Da)\n  %s  ├─ %s\n  %s  └─ %s%s",
      if (last) "└─" else "├─",
      .col_warn(warning_sym),
      kinds[[amb$kind[k]]],
      amb$delta[k],
      if (last) " " else "│",
      amb$first[k],
      if (last) " " else "│",
      amb$second[k],
      # Blank line before the first sample's log
      if (last) "\n" else ""
    ))
  }
}

.col_warn <- function(x) {
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    # Colour by class (see .log-warn in main.scss): the Protocol tab and the
    # sidebar console show the log on different backgrounds, and the Alerts
    # and Warnings counts find the lines by this class
    paste0('<span class="log-warn" style="font-weight: bold;">', x, "</span>")
  } else {
    paste0("\033[33m", x, "\033[39m")
  }
}
.col_err <- function(x) {
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    paste0('<span class="log-err" style="font-weight: bold;">', x, "</span>")
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
  # Quality metrics are sample-level values repeated on every hit row, so they
  # are averaged per sample - otherwise samples with more rows would weigh more
  per_sample <- hits_summarized[!duplicated(hits_summarized$Sample), ]
  unmatched_vals <- as.numeric(per_sample[["% Unmatched"]])
  correct_vals <- as.numeric(per_sample[["% Correct"]])
  mean_unmatched <- mean(unmatched_vals, na.rm = TRUE)
  sd_unmatched <- stats::sd(unmatched_vals, na.rm = TRUE)
  mean_correct <- mean(correct_vals, na.rm = TRUE)
  sd_correct <- stats::sd(correct_vals, na.rm = TRUE)

  message(sprintf(
    "SUMMARIZING HITS\n  │\n  ├─ %s sample(s) screened\n  ├─ %s hit(s) detected in total\n  ├─ Unmatched: %.2f%% (SD: %.2f%%)\n  └─ Correct:   %.2f%% (SD: %.2f%%)\n",
    length(unique(hits_summarized$Sample)),
    sum(is_complex_row(hits_summarized)),
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
      " ├─ %s time points present from %s to %s [%s]",
      length(unique(times)),
      fmt_log(min(times)),
      fmt_log(max(times)),
      units[2]
    )
  ))
}

# Log k_obs analysis initiation, the first step of the kinetics of a complex
log_kobs_analysis <- function() {
  message("  ├─ Infer observed first-order rate constant k_obs\n  │  │")
}

# Tree lines of the protocol log ----
#
# The kinetics loggers draw one complex's steps as a tree rooted at column 2
# ("  ├─ ..."). These helpers close a branch whose last step was cut short (a
# failed fit, skipped prerequisites) and hang the tree of each complex under
# its own branch when a run has several.

# Node line at column `col`: `col` characters of indentation, then a connector
is_log_node <- function(lines, col) {
  substr(lines, col + 1, col + 2) %in%
    c("├─", "└─") &
    !grepl("[^ │]", substr(lines, 1, col))
}

# Turns the last node at column `col` into a closing one, clearing the
# vertical line it would have continued below
close_log_node <- function(lines, col) {
  nodes <- which(is_log_node(lines, col))
  if (!length(nodes)) {
    return(lines)
  }
  last <- max(nodes)
  substr(lines[last], col + 1, col + 1) <- "└"
  below <- seq_along(lines) > last
  continued <- below & substr(lines, col + 1, col + 1) == "│"
  substr(lines[continued], col + 1, col + 1) <- " "
  lines
}

# One complex's log block, closed at its root and in the last root step
#' @export
close_log_block <- function(lines) {
  # A vertical separator after the last step leads nowhere
  while (length(lines) && grepl("^[ │]*$", lines[length(lines)])) {
    lines <- lines[-length(lines)]
  }
  lines <- close_log_node(lines, 2)
  last <- max(c(0, which(is_log_node(lines, 2))))
  if (last > 0) {
    tail <- seq(last, length(lines))
    lines[tail] <- close_log_node(lines[tail], 5)
  }
  lines
}

# The block hung under a branch of its own, named `title`. The first branch
# needs no separator: the section heading ends in one.
#' @export
nest_log_block <- function(lines, title, last = FALSE, first = FALSE) {
  branch <- if (last) "  └─ " else "  ├─ "
  cont <- if (last) "   " else "│  "
  c(
    if (!first) "  │",
    paste0(branch, title),
    paste0("  ", cont, "│"),
    paste0("  ", cont, substr(lines, 3, nchar(lines)))
  )
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
  if (length(parts) == 0) {
    parts <- ""
  }
  paste0(c(head, rep(cont, length(parts) - 1L)), parts)
}

# Log kinact/KI warning. The headline sits on the branch, the explanation
# hangs off it one level deeper — the same shape the kobs steps use — so no
# single row grows past the width of the log card.
log_kinact_ki_warning <- function(msg, detail = NULL) {
  head <- sprintf("     ├─ %s ", .col_warn(warning_sym))
  lines <- wrap_log_line(head, "     │    ", trimws(msg), head_width = 10)
  if (!is.null(detail) && nzchar(detail)) {
    lines <- c(lines, wrap_log_line("     │  └─ ", "     │     ", detail))
  }
  message(paste(lines, collapse = "\n"))
}

# Log (kinact/KI) analysis initiation
log_kinact_ki_analysis <- function() {
  message(paste("  │\n", " └─ Infer second-order rate constant kinact/KI"))
}

# Log kinact/KI results
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
          "     ├─ kinact = %s ± %s %s⁻¹",
          fmt_log(results$Params[1, 1]),
          fmt_log(results$Params[1, 2]),
          units[["Time"]]
        ),
        sprintf(
          "     ├─ KI     = %s ± %s %s",
          fmt_log(results$Params[2, 1]),
          fmt_log(results$Params[2, 2]),
          units[["Concentration"]]
        )
      )
    } else {
      "     ├─ kinact, KI = not determinable"
    },
    sprintf(
      "     %s kinact/KI = %s ± %s %s⁻¹ %s⁻¹%s",
      if (is.null(results$Two_Step)) "└─" else "├─",
      fmt_log(ratio[["Estimate"]]),
      fmt_log(ratio[["Std. Error"]]),
      units[["Concentration"]],
      units[["Time"]],
      ci
    ),
    # The classic result, for comparison only
    if (!is.null(results$Two_Step)) {
      sprintf(
        "     └─ Two-step (classic, comparison only): kinact/KI = %s%s",
        fmt_log(results$Two_Step$ratio),
        if (isFALSE(results$Two_Step$within_ci)) {
          " (outside the 95 % CI)"
        } else {
          ""
        }
      )
    }
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
  config = NULL,
  preference = names(hit_preference_rules)[1]
) {
  samples <- names(results$deconvolution)
  preference <- hit_preference_rule(preference)
  compound_mw <- as.matrix(compound_table[, -1])
  rownames(compound_mw) <- compound_table[, 1]

  for (i in seq_along(samples)) {
    log_start(samples[i])

    st_key <- ms_sample_base(sample_table$Sample)
    s_key <- ms_sample_base(samples[i])
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
      cfg_key <- ms_sample_base(config[["Sample"]])
      idx <- match(s_key, cfg_key)
      if (!is.na(idx)) {
        raw_well <- as.character(config[["Well"]][idx])
        if (!is.na(raw_well) && nzchar(trimws(raw_well))) {
          sample_well <- sub("^.*:", "", raw_well)
        }
      }
    }

    peaks <- get_peaks(result_sample = samples[i], results = results)

    # Hit search
    hits_df <- check_hits(
      sample_table = sample_table,
      protein_mw = protein_table[protein_table$Protein == present_protein, ],
      # %in%, not ==: a sample can list several compounds, and comparing the
      # compound column against that one-row frame picked none or a wrong one
      compound_mw = compound_table[
        compound_table$Compound %in% unlist(present_cmp),
      ],
      peaks = peaks,
      peak_tolerance = peak_tolerance,
      max_multiples = max_multiples,
      sample = samples[i],
      well = sample_well,
      preference = preference
    )

    # Calculate quality metrics unmatched[%] and correct[%]
    hits_df <- assess_quality(hits = hits_df, peaks = peaks)

    # Log hit search result
    log_result(
      sum(!is.na(hits_df$cmp_mass)),
      hits_df$unmatched[1],
      hits_df$correct[1]
    )

    # Conversion of relative intensities to Binding [%]
    # Add resulting hits data frame to sample
    results$deconvolution[[samples[i]]][[
      "hits"
    ]] <- conversion(hits_df)

    log_done()
  }

  return(results)
}

# Calculate quality metrics unmatched[%] and correct[%]
assess_quality <- function(hits, peaks) {
  if (nrow(peaks) != 0) {
    total_peak_intensity <- sum(peaks$intensity)
    detected_species <- c(hits$measured_prot, hits$peak)
    known_peak_intensity <- sum(peaks$intensity[
      peaks$mass %in% detected_species
    ])

    # Ratio first: 100 * a / a can land one ulp off 100, which plots that
    # autoscale to the data then zoom into as if it were a real spread
    hits$correct <- correct <- 100 *
      (known_peak_intensity / total_peak_intensity)
    hits$unmatched <- 100 - correct
  } else {
    # Assigned separately so correct precedes unmatched, the column order
    # conversion() relies on when naming the columns
    hits$correct <- NA
    hits$unmatched <- NA
  }

  return(hits)
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
    sample_table_join$Sample <- ms_sample_base(sample_table_join$Sample)
    hits_summarized$Sample <- ms_sample_base(hits_summarized$Sample)

    hits_summarized <- hits_summarized |>
      dplyr::left_join(sample_table_join, by = "Sample") |>
      dplyr::mutate(binding = `Total % Binding` * 100) |>
      dplyr::arrange(dplyr::across(all_of(conc_time)))
  }

  # Join Replicate from sample_table if available
  if ("Replicate" %in% names(sample_table)) {
    rep_join <- sample_table[, c("Sample", "Replicate"), drop = FALSE]
    rep_join$Sample <- ms_sample_base(rep_join$Sample)
    hs_key <- ms_sample_base(hits_summarized$Sample)
    hits_summarized$Replicate <- rep_join$Replicate[match(
      hs_key,
      rep_join$Sample
    )]
  }

  # Log hits summary
  log_hits_summary(hits_summarized)

  return(hits_summarized)
}

# Function perform checks for kinact/KI analysis prerequisites
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

  # Summarize filtered hits by concentration. Counted per sample, not per row:
  # one sample yields one hit row per compound, mass shift and protein species,
  # which would otherwise let a single measurement satisfy the count below.
  tab <- hits_summary |>
    dplyr::group_by(dplyr::pick(dplyr::contains("Concentration"))) |>
    dplyr::summarise(
      count = if ("Sample" %in% names(hits_summary)) {
        dplyr::n_distinct(Sample)
      } else {
        dplyr::n()
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
  time_col <- grep("^Time", names(hits_summary), value = TRUE)[1]
  if (!is.na(time_col)) {
    timepoints <- hits_summary |>
      dplyr::group_by(dplyr::pick(dplyr::contains("Concentration"))) |>
      dplyr::summarise(
        timepoints = {
          t <- as.numeric(.data[[time_col]])
          dplyr::n_distinct(t[!is.na(t) & t != 0])
        },
        .groups = "drop"
      )
    tab$timepoints <- timepoints$timepoints[match(
      tab[[conc_col]],
      timepoints[[conc_col]]
    )]
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

  # Rows without a binding value are left out, as check_filter_hits() does
  # before the first fit; the refit after excluding concentrations and the
  # proteoform fits pass unfiltered hits, where such a row would turn its
  # sample point into NaN and fail the whole concentration
  if ("binding" %in% names(hits_summary)) {
    hits_summary <- hits_summary[!is.na(hits_summary$binding), , drop = FALSE]
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

# Function to add kinact/KI results to result list
#' @export
add_kinact_ki_result <- function(result_list, units) {
  # Log (kinact/KI) analysis initiation
  log_kinact_ki_analysis()

  # Calculcate kinact/KI from binding/kobs result
  kinact_ki_result <- compute_kinact_ki(
    result_list[["binding_kobs_result"]],
    units = units
  )

  # Log kinact/KI results
  if (!is.null(kinact_ki_result)) {
    log_kinact_ki_results(results = kinact_ki_result, units = units)
  }

  return(kinact_ki_result)
}

# Binding kinetics per protein-compound complex ----
#
# k_obs and kinact/KI describe one compound reacting with one protein, so the
# kinetics are fitted per complex, over the samples declared with both. A
# sample declaring a single compound contributes its total binding, as before;
# one declaring several contributes the binding of the complex's compound only,
# so a second compound binding alongside does not inflate the curve.

# Picker value of a complex
#' @export
complex_key <- function(protein, compound) {
  paste(protein, compound, sep = " + ")
}

# Rows of the raw hits summary belonging to one complex, with `binding` set to
# the binding of its compound
#' @export
complex_hits <- function(hits_summary, sample_table, protein, compound) {
  strip <- ms_sample_base

  cmp_cols <- grep("^Compound", names(sample_table), value = TRUE)
  declared <- lapply(seq_len(nrow(sample_table)), function(i) {
    x <- trimws(as.character(unlist(sample_table[i, cmp_cols])))
    unique(x[!is.na(x) & nzchar(x)])
  })
  has_compound <- vapply(declared, function(x) compound %in% x, logical(1))
  samples <- strip(sample_table$Sample[
    sample_table$Protein %in% protein & has_compound
  ])
  single <- strip(sample_table$Sample[lengths(declared) == 1])

  sample_key <- strip(hits_summary$Sample)
  rows <- hits_summary[
    sample_key %in% samples & hits_summary$Protein %in% protein,
    ,
    drop = FALSE
  ]

  # Complexes of the other compounds of a sample are not this complex's
  # binding; the unbound rows stay
  rows <- rows[is.na(rows$Compound) | rows$Compound == compound, , drop = FALSE]
  if (!nrow(rows) || !"binding" %in% names(rows)) {
    return(rows)
  }

  # The preferred rows of the compound add up to its share of the total, as a
  # peak claimed by several interpretations is split between them
  own <- is_complex_row(rows) & rows$Preferred %in% TRUE
  shares <- tapply(
    ifelse(own, rows$`% Binding`, 0),
    rows$Sample,
    sum,
    na.rm = TRUE
  )
  # A sample without any protein or complex peak stays unmeasured (NA) either way
  rows$binding <- ifelse(
    strip(rows$Sample) %in% single | is.na(rows$binding),
    rows$binding,
    100 * unname(shares[rows$Sample])
  )
  rows
}

# The complexes of a run: every protein-compound pair with a hit and, given the
# sample table, every declared pair
#' @export
run_complexes <- function(hits_summary, sample_table = NULL) {
  hits <- hits_summary[is_complex_row(hits_summary), , drop = FALSE]
  pairs <- data.frame(
    protein = as.character(hits$Protein),
    compound = as.character(hits$Compound)
  )

  # With the sample table, also the declared pairs without any hit, so their
  # samples do not drop out of the kinetics unnoticed
  if (!is.null(sample_table)) {
    st <- sample_table[, names(sample_table) != "Replicate", drop = FALSE]
    cmp_cols <- grep("^Compound", names(st), value = TRUE)
    declared <- sample_compounds(st[, c("Sample", "Protein", cmp_cols)])
    pairs <- rbind(
      pairs,
      data.frame(
        protein = rep(as.character(st$Protein), lengths(declared)),
        compound = unlist(declared)
      )
    )
  }

  pairs <- unique(pairs)
  pairs <- pairs[order(pairs$protein, pairs$compound), , drop = FALSE]
  pairs$key <- complex_key(pairs$protein, pairs$compound)
  rownames(pairs) <- NULL
  pairs
}

# k_obs and kinact/KI of every complex. `reason` says why a complex has no
# k_obs: the prerequisites of check_filter_hits() failed, or no concentration
# could be fitted.
#' @export
complex_kinetics <- function(hits_summary, sample_table, conc_time, units) {
  complexes <- run_complexes(hits_summary, sample_table)
  several <- nrow(complexes) > 1

  out <- lapply(seq_len(nrow(complexes)), function(k) {
    hits <- complex_hits(
      hits_summary,
      sample_table,
      complexes$protein[k],
      complexes$compound[k]
    )

    # The steps log into a block of their own, closed where they stop early
    # and, with several complexes, hung under the complex's branch
    log <- character(0)
    entry <- withCallingHandlers(
      fit_complex(
        hits,
        complexes$protein[k],
        complexes$compound[k],
        conc_time,
        units
      ),
      message = function(m) {
        log <<- c(log, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    lines <- close_log_block(strsplit(paste(log, collapse = ""), "\n")[[1]])
    if (several) {
      lines <- nest_log_block(
        lines,
        sprintf(
          "%s (%d samples)",
          complexes$key[k],
          length(unique(hits$Sample))
        ),
        last = k == nrow(complexes),
        first = k == 1
      )
    }
    message(paste(lines, collapse = "\n"))

    entry
  })

  stats::setNames(out, complexes$key)
}

# k_obs and kinact/KI of one complex, logging every step
fit_complex <- function(hits, protein, compound, conc_time, units) {
  # The design of this complex's own samples
  log_binding_kinetics(
    concentrations = hits[[conc_time[1]]],
    times = hits[[conc_time[2]]],
    units = units
  )
  log_kobs_analysis()

  entry <- list(
    protein = protein,
    compound = compound,
    hits = hits,
    binding_kobs_result = NULL,
    kinact_ki_result = NULL,
    reason = NULL
  )

  # A declared complex whose compound was found in none of its samples
  if (!any(is_complex_row(hits) & hits$Compound %in% compound)) {
    message(sprintf(
      "  │  └─ %s No hits of %s in its samples. Skipping binding kinetics analysis.",
      .col_warn(warning_sym),
      compound
    ))
    entry$reason <- paste("No hits of", compound, "in its samples")
    return(entry)
  }

  filtered <- check_filter_hits(list(hits_summary = hits))
  if (!is.data.frame(filtered)) {
    entry$reason <- "Too few concentrations, hits or time points for a k_obs fit"
    return(entry)
  }

  log_filtered_samples(diff = nrow(hits) - nrow(filtered))
  log_filtered_concentrations(
    initial_tbl = hits,
    filtered_tbl = filtered,
    conc_time = conc_time
  )

  kobs <- add_kobs_binding_result(
    filtered,
    conc_time = conc_time,
    units = units
  )
  if (nrow(kobs$kobs_result_table) == 0) {
    message(paste(
      "  │  └─ No concentration could be fitted.",
      "Skipping binding kinetics analysis."
    ))
    entry$reason <- "No concentration could be fitted"
    return(entry)
  }

  entry$binding_kobs_result <- kobs
  entry$kinact_ki_result <- add_kinact_ki_result(
    list(binding_kobs_result = kobs),
    units = units
  )
  entry
}

# The result list with the kinetics of one complex in the places the kinetics
# interface reads them from. Without a valid `key` the first complex with a
# k_obs fit is taken.
#' @export
select_complex_kinetics <- function(result_list, key = NULL) {
  kinetics <- result_list$kinetics
  if (!length(kinetics)) {
    return(result_list)
  }

  if (is.null(key) || !key %in% names(kinetics)) {
    fitted <- vapply(
      kinetics,
      function(k) !is.null(k$binding_kobs_result),
      logical(1)
    )
    key <- names(kinetics)[if (any(fitted)) which(fitted)[1] else 1]
  }

  k <- kinetics[[key]]
  result_list$binding_kobs_result <- k$binding_kobs_result
  result_list$kinact_ki_result <- k$kinact_ki_result
  result_list$kinetics_hits <- k$hits
  result_list$kinetics_complex <- k[c("protein", "compound", "reason")]
  result_list$kinetics_complex$key <- key
  result_list
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
  key_symbols(
    list(concentrations = suppressWarnings(as.numeric(conc))),
    conc
  )
}

# The given symbol map, extended by any concentration it does not know;
# without a map, symbols follow the plotted concentrations
resolve_symbol_map <- function(symbol_map, concentrations) {
  if (is.null(symbol_map)) {
    return(concentration_symbol_map(concentrations))
  }
  missing <- setdiff(unique(as.character(concentrations)), names(symbol_map))
  if (length(missing) > 0) {
    # The control keeps its own symbol; other unknown concentrations take
    # the symbols that follow the map's
    is_control <- suppressWarnings(as.numeric(missing)) %in% 0
    used <- sum(names(symbol_map) != "0" & symbol_map != control_symbol)
    extra <- concentration_symbols[
      (used + cumsum(!is_control) - 1L) %% length(concentration_symbols) + 1L
    ]
    extra[is_control] <- control_symbol
    symbol_map <- c(symbol_map, stats::setNames(extra, missing))
  }
  symbol_map
}

# Replicate series of single samples ----
#
# Shared by the plots that draw every sample (Global Fit Residuals, Binding
# Curve, its 3D view): colour and shape give the concentration, the fill the
# replicate series - filled, open, dotted, open with dot. 3D markers know
# fewer symbols, so there it is filled and open circle, filled and open square.
series_fill_variants <- function(series) {
  levels <- sort_series(series)
  variants <- c("", "-open", "-dot", "-open-dot")
  stats::setNames(variants[(seq_along(levels) - 1) %% length(variants) + 1], levels)
}

series_symbols_3d <- function(series) {
  levels <- sort_series(series)
  symbols <- c("circle", "circle-open", "square", "square-open")
  stats::setNames(symbols[(seq_along(levels) - 1) %% length(symbols) + 1], levels)
}

# Legend group naming the series by their fill, in shape-neutral circles of
# the text colour. `symbols` maps series to the full symbol name. Each entry
# carries one missing point: plotly.js leaves traces without any point out of
# the legend, and a missing value draws nothing. It is wrapped in a list since
# plotly_build() drops plain NA values together with the whole trace.
add_series_legend <- function(p, symbols, font_color, scene = FALSE) {
  # A filled dotted marker draws its dot in the line colour, which therefore
  # has to stand off the filled text colour
  contrast <- if (mean(grDevices::col2rgb(font_color)) < 128) "white" else "black"
  for (i in seq_along(symbols)) {
    s <- names(symbols)[i]
    open <- grepl("open", symbols[[s]])
    filled_dot <- !open && grepl("dot", symbols[[s]])
    marker <- list(
      size = if (scene) 6 else 10,
      color = font_color,
      symbol = symbols[[s]],
      line = if (filled_dot) {
        list(width = 1, color = contrast)
      } else {
        list(width = if (open) 2 else 0, color = font_color)
      }
    )
    group_title <- if (i == 1) {
      list(text = "Replicate series", font = list(color = font_color))
    }
    p <- if (scene) {
      plotly::add_trace(
        p,
        type = "scatter3d",
        mode = "markers",
        x = list(NA),
        y = list(NA),
        z = list(NA),
        name = s,
        legendgroup = legend_key_group("series"),
        legendgrouptitle = group_title,
        marker = marker,
        hoverinfo = "skip",
        inherit = FALSE
      )
    } else {
      plotly::add_markers(
        p,
        x = list(NA),
        y = list(NA),
        name = s,
        legendgroup = legend_key_group("series"),
        legendgrouptitle = group_title,
        marker = marker,
        hoverinfo = "skip",
        inherit = FALSE
      )
    }
  }
  p
}

# Curves of the global fit for the Binding Curve: per fitted concentration
# Plateau * (1 - exp(-kobs * t)) with the plateau and k_obs([I]) of the global
# kinact/KI fit, keyed by the concentration labels of `binding_table` (matched
# numerically, the labels follow the unit view). NULL without a global fit.
#' @export
global_binding_curves <- function(kinact_ki_result, binding_table, n = 201) {
  plateaus <- kinact_ki_result$Plateaus
  if (is.null(plateaus) || !nrow(plateaus) || !nrow(binding_table)) {
    return(NULL)
  }
  keys <- unique(as.character(binding_table$concentration))
  key_num <- suppressWarnings(as.numeric(keys))
  grid <- seq(0, max(binding_table$time, na.rm = TRUE), length.out = n)

  curves <- lapply(seq_len(nrow(plateaus)), function(i) {
    conc <- plateaus$conc[i]
    hit <- which(abs(key_num - conc) <= 1e-9 * max(abs(conc), 1e-300))
    if (!length(hit)) {
      return(NULL)
    }
    kobs <- kobs_model_at(kinact_ki_result, conc)
    data.frame(
      time = grid,
      predicted_binding = plateaus$plateau[i] * (1 - exp(-kobs * grid)),
      concentration = keys[hit[1]],
      kobs = kobs,
      plateau = plateaus$plateau[i]
    )
  })
  curves <- do.call(rbind, curves)
  if (is.null(curves)) {
    return(NULL)
  }
  curves$concentration <- factor(curves$concentration, levels = keys)
  curves
}

# Function to generate and display binding plot
#
# With `kinact_ki_result` the curves are those of the global fit, the model
# kinact/KI and KI come from; concentrations outside it (untreated control,
# excluded or without response) keep their own fit. `show_single` adds the fit
# of every concentration on its own as a dashed line, for comparison.
#' @export
make_binding_plot <- function(
  kobs_result,
  filter_conc = NULL,
  colors = NULL,
  units = NULL,
  theme = "light",
  points = c("samples", "mean", "both"),
  symbol_map = NULL,
  kinact_ki_result = NULL,
  show_single = FALSE
) {
  # Which observations to draw: the individual samples (what the global fit
  # sees), mean ± SD per time point, or both
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

  curve_hover <- paste(
    "<b>%{text}</b><br>",
    paste("Time: %{x}", time_unit, "<br>"),
    "Binding [%]: %{y:.2f}<br>",
    paste0("k<sub>obs</sub>: %{customdata:.3~g} ", time_unit, "⁻¹"),
    "<extra></extra>"
  )

  # Curves: the global fit where it covers a concentration, otherwise the
  # concentration's own fit (untreated control, excluded, no response)
  global <- if (!is.null(kinact_ki_result)) {
    global_binding_curves(kinact_ki_result, kobs_result$binding_table)
  }
  if (!is.null(global) && !is.null(filter_conc)) {
    global <- global[global$concentration == filter_conc, , drop = FALSE]
  }
  if (!is.null(global) && !nrow(global)) {
    global <- NULL
  }
  in_global <- if (is.null(global)) {
    character(0)
  } else {
    unique(as.character(global$concentration))
  }
  single <- kobs_result$binding_table
  own <- single[!as.character(single$concentration) %in% in_global, ]
  own$label <- ifelse(
    suppressWarnings(as.numeric(as.character(own$concentration))) %in% 0,
    "Untreated control",
    if (is.null(global)) "Fit" else "Fitted alone (not in the global fit)"
  )

  binding_plot <- plotly::plot_ly()
  if (!is.null(global)) {
    global$label <- "Global fit"
    binding_plot <- plotly::add_lines(
      binding_plot,
      data = global,
      x = ~time,
      y = ~predicted_binding,
      color = ~concentration,
      legendgroup = ~concentration,
      colors = colors,
      symbols = symbol_map,
      line = list(width = 2),
      text = ~label,
      customdata = ~kobs,
      hovertemplate = curve_hover,
      showlegend = FALSE
    )
  }
  if (nrow(own)) {
    binding_plot <- plotly::add_lines(
      binding_plot,
      data = own,
      x = ~time,
      y = ~predicted_binding,
      color = ~concentration,
      legendgroup = ~concentration,
      colors = colors,
      symbols = symbol_map,
      line = list(width = 2, dash = if (is.null(global)) "solid" else "dot"),
      text = ~label,
      customdata = ~kobs,
      hovertemplate = curve_hover,
      showlegend = FALSE
    )
  }
  # Each concentration fitted on its own, for comparison with the global fit
  if (isTRUE(show_single) && !is.null(global)) {
    alone <- single[as.character(single$concentration) %in% in_global, ]
    alone$label <- "Fitted alone (for comparison)"
    binding_plot <- plotly::add_lines(
      binding_plot,
      data = alone,
      x = ~time,
      y = ~predicted_binding,
      color = ~concentration,
      legendgroup = ~concentration,
      colors = colors,
      symbols = symbol_map,
      line = list(width = 1.2, dash = "dash"),
      text = ~label,
      customdata = ~kobs,
      hovertemplate = curve_hover,
      showlegend = FALSE
    )
  }

  if (show_means) {
    binding_plot <- binding_plot |>
      # Mean ± SD per time point
      plotly::add_markers(
        data = dplyr::filter(df_points, !is.na(kobs)),
        x = ~time,
        y = ~binding,
        color = ~concentration,
        legendgroup = ~concentration,
        # Legend label as shown on screen, in the order of the levels
        name = ~ factor(
          conc_display(concentration),
          levels = unique(conc_display(
            if (is.factor(concentration)) levels(concentration) else concentration
          ))
        ),
        colors = colors,
        symbol = ~concentration,
        marker = list(
          size = 12,
          opacity = 0.8,
          line = list(width = 1, color = font_color)
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
          "<b>Mean ± SD</b> (%{text}) · display only, the fit uses every sample<br>",
          paste(
            "Time: %{x}",
            gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
            "<br>"
          ),
          "Binding [%]: %{y:.2f}",
          "<extra></extra>"
        ),
        showlegend = ifelse(is.null(filter_conc), TRUE, FALSE)
      )
  }

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
    # Samples alone, as the global fit sees them: colour and shape give the
    # concentration, the fill the replicate series (as in Global Fit
    # Residuals). The first series of a concentration carries its legend
    # entry in place of the means.
    series <- if ("series" %in% names(sample_points)) {
      as.character(sample_points$series)
    } else {
      rep(NA_character_, nrow(sample_points))
    }
    series[is.na(series) | series == ""] <- "–"
    sample_points$series_label <- series
    variants <- series_fill_variants(series)
    multi_series <- length(variants) > 1

    for (conc_name in levels(droplevels(sample_points$concentration))) {
      conc_color <- unname(colors[conc_name])
      if (is.na(conc_color)) {
        conc_color <- font_color
      }
      first <- TRUE
      for (s in names(variants)) {
        sub <- sample_points[
          as.character(sample_points$concentration) == conc_name &
            sample_points$series_label == s,
          ,
          drop = FALSE
        ]
        if (!nrow(sub)) {
          next
        }
        open <- grepl("open", variants[[s]])
        binding_plot <- plotly::add_markers(
          binding_plot,
          x = sub$time,
          y = sub$binding,
          name = conc_display(conc_name),
          legendgroup = conc_name,
          showlegend = first && is.null(filter_conc),
          marker = list(
            size = 10,
            opacity = 1,
            color = conc_color,
            symbol = paste0(symbol_map[[conc_name]], variants[[s]]),
            line = if (open) {
              list(width = 2)
            } else {
              list(width = 1.5, color = font_color)
            }
          ),
          text = paste0(
            sub$Sample,
            if (multi_series) paste0("<br>Series: ", s) else ""
          ),
          hovertemplate = sample_hover,
          inherit = FALSE
        )
        first <- FALSE
      }
    }
    if (multi_series) {
      binding_plot <- add_series_legend(
        binding_plot,
        stats::setNames(paste0("circle", variants), names(variants)),
        font_color
      )
    }
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
          symbol = unname(symbol_map[as.character(
            sample_points$concentration
          )]),
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
        name = ~ paste0(conc_display(concentration), " (not fitted)"),
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

  # Add explicit legend-only entries for no-response concentrations (no observed
  # points). In Samples mode a concentration whose samples are drawn (the
  # untreated control at t = 0) already has its entry.
  if (!show_means && !is.null(sample_points)) {
    missing_conc <- setdiff(
      missing_conc,
      unique(as.character(sample_points$concentration))
    )
  }
  if (is.null(filter_conc) && length(missing_conc) > 0) {
    for (conc_name in as.character(sort(
      as.numeric(missing_conc),
      decreasing = TRUE
    ))) {
      binding_plot <- binding_plot |>
        plotly::add_markers(
          x = 0,
          y = 0,
          name = conc_display(conc_name),
          legendgroup = legend_key_group(conc_name),
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
        # One concentration (Concentrations tab) has no concentration entries,
        # so no title over them either
        title = if (is.null(filter_conc)) {
          list(
            text = paste0(
              "Concentration [",
              gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
              "]  "
            ),
            font = list(color = font_color)
          )
        },
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

# k_obs of the selected model (linear or hyperbolic) of a kinact/KI result at
# `conc`, with the reported kinact/KI
#' @export
kobs_model_at <- function(kinact_ki_result, conc) {
  ratio <- kinact_ki_result$Ratio[["Estimate"]]
  if (identical(kinact_ki_result$Model, "hyperbolic")) {
    ratio * conc / (1 + conc / kinact_ki_result$Fit$KI_hyperbolic)
  } else {
    ratio * conc
  }
}

# Function to generate and display kobs plot
#
# `proteoforms` optionally overlays the per-proteoform k_obs and fits on the
# pooled ones: entries as in proteoform_kobs_plot(), without the pooled entry,
# coloured by `proteoform_palette`.
#' @export
make_kobs_plot <- function(
  kinact_ki_result,
  colors,
  units,
  theme = "light",
  show_extrapolation = FALSE,
  show_constants = FALSE,
  show_two_step = FALSE,
  symbol_map = NULL,
  proteoforms = NULL,
  proteoform_palette = NULL,
  kobs_table = NULL,
  log_conc = FALSE
) {
  # Without a kinact/KI fit the k_obs values of `kobs_table` are drawn alone
  fit_failed <- is.null(kinact_ki_result)
  if (fit_failed) {
    if (is.null(kobs_table) || !nrow(kobs_table)) {
      return(NULL)
    }
    kinact_ki_result <- list(
      Kobs_Data = data.frame(
        conc = as.numeric(rownames(kobs_table)),
        predicted_kobs = NA_real_,
        kobs = kobs_table$kobs,
        kobs_se = kobs_table$kobs_se
      )
    )
  }

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

  conc_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]])
  time_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])

  # Log concentration axis: the origin is not on it, so lines start at the
  # left edge, a bit below the lowest concentration. Shapes and annotations
  # take log10 positions there; lx() places them on either axis.
  log_conc <- isTRUE(log_conc) && nrow(df_points) > 0
  conc_measured <- sort(unique(as.numeric(as.character(df_points$conc))))
  x_left <- if (log_conc) min(conc_measured) / 1.6 else 0
  lx <- function(x) if (log_conc) log10(pmax(x, x_left)) else x

  # Line styles, one per kind of line so none is mistaken for another: the
  # global fit solid, continued dashed beyond the measured range; the two-step
  # fit dash-dot; the guides to the constants thin, faded and dotted. The
  # proteoform fits are coloured.
  extrap_line <- list(width = 1.5, color = font_color, dash = "dash")

  # Legend in two titled groups (plus the proteoforms' own): the fits and the
  # concentrations of the points. The title goes on the group's first row.
  fit_group <- "kobs_fits"
  conc_group <- "kobs_concentrations"
  group_title <- function(text) {
    list(text = text, font = list(color = font_color))
  }

  two_step <- kinact_ki_result$Two_Step
  draw_two_step <- isTRUE(show_two_step) && !fit_failed &&
    !is.null(two_step) && nrow(df_points) > 0

  # Generate plot
  kobs_plot <- plotly::plot_ly()
  if (nrow(df)) {
    # kobs curve of the global fit (linear or hyperbolic model)
    kobs_plot <- plotly::add_lines(
      kobs_plot,
      data = df,
      x = ~conc,
      y = ~predicted_kobs,
      colors = colors,
      symbols = symbol_map,
      line = list(width = 2.5, color = font_color),
      name = "Global fit",
      legendgroup = fit_group,
      legendgrouptitle = group_title("Fit"),
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
      showlegend = TRUE
    )
  }

  # Collected here and set in the single layout() call at the end, so the
  # fit-failed note and the extrapolation shading cannot overwrite each other
  shapes <- list()
  annotations <- list()
  if (fit_failed) {
    annotations <- c(
      annotations,
      list(list(
        text = "k<sub>inact</sub>/K<sub>I</sub> fit failed",
        xref = "paper",
        yref = "paper",
        x = 0,
        y = 1,
        xanchor = "left",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(color = font_color, size = 13)
      ))
    )
  }

  for (conc_name in ordered_conc) {
    sub <- df_points[as.character(df_points$conc) == conc_name, , drop = FALSE]
    kobs_plot <- plotly::add_markers(
      kobs_plot,
      data = sub,
      x = ~ as.numeric(as.character(conc)),
      y = ~kobs,
      name = conc_display(conc_name),
      legendgroup = conc_group,
      legendgrouptitle = if (identical(conc_name, ordered_conc[1])) {
        group_title(paste0("Concentration [", conc_unit, "]"))
      },
      marker = list(
        size = 10,
        opacity = 0.85,
        color = unname(colors[conc_name]),
        symbol = unname(symbol_map[conc_name]),
        line = list(width = 1, color = font_color)
      ),
      customdata = ~kobs_se_label,
      error_y = list(
        type = "data",
        array = ~kobs_se,
        visible = TRUE,
        thickness = 1.2,
        width = 3,
        color = font_color
      ),
      hovertemplate = paste0(
        "<b>Fitted alone</b> (for comparison, not used by the fit)<br>",
        "Concentration: ",
        conc_display(conc_name),
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

  # Classic two-step fit through the k_obs points, for comparison only. Drawn
  # only when switched on in the settings, so the global fit stays the curve
  # the plot shows.
  if (draw_two_step) {
    conc_top <- max(as.numeric(as.character(df_points$conc)), na.rm = TRUE)
    grid_classic <- if (log_conc) {
      exp(seq(log(x_left), log(conc_top), length.out = 200))
    } else {
      seq(0, conc_top, length.out = 200)
    }
    kobs_plot <- plotly::add_lines(
      kobs_plot,
      x = grid_classic,
      y = two_step$kinact * grid_classic / (two_step$KI + grid_classic),
      name = "Two-step (classic)",
      legendgroup = fit_group,
      legendgrouptitle = if (!nrow(df)) group_title("Fit"),
      line = list(width = 1.5, color = font_color, dash = "dashdot"),
      hovertemplate = paste0(
        "<b>Two-step (classic), for comparison</b><br>",
        "Unweighted hyperbola through the k<sub>obs</sub> points<br>",
        "Concentration: %{x:.3~g} ",
        conc_unit,
        "<br>k<sub>obs</sub>: %{y:.3~g} ",
        time_unit,
        "⁻¹<extra></extra>"
      ),
      showlegend = TRUE,
      inherit = FALSE
    )
  }

  # Optional view past the measured range: the selected model continued as a
  # dashed line and the measured range shaded. The proteoform fits are
  # continued to the same end.
  extrapolate <- isTRUE(show_extrapolation) && nrow(df) > 0
  if (extrapolate) {
    max_conc <- max(df$conc, na.rm = TRUE)
    ki <- kinact_ki_result$Fit$KI_hyperbolic
    extrap_max <- 5 * max_conc
    if (kinact_ki_result$Model == "hyperbolic" && is.finite(ki)) {
      extrap_max <- max(extrap_max, 3 * ki)
    }
  }

  if (length(proteoforms)) {
    kobs_plot <- add_proteoform_kobs_traces(
      kobs_plot,
      proteoforms,
      colors = proteoform_palette,
      font_color = font_color,
      conc_unit = conc_unit,
      time_unit = time_unit,
      group_title = "Proteoforms",
      subtle = names(proteoforms),
      extrapolate_to = if (extrapolate) extrap_max
    )
  }

  if (extrapolate) {
    grid_out <- if (log_conc) {
      exp(seq(log(max_conc), log(extrap_max), length.out = 200))
    } else {
      seq(max_conc, extrap_max, length.out = 200)
    }
    kobs_plot <- kobs_plot |>
      plotly::add_lines(
        x = grid_out,
        y = kobs_model_at(kinact_ki_result, grid_out),
        line = extrap_line,
        hovertemplate = paste0(
          "<b>Global fit (",
          kinact_ki_result$Model,
          "), extrapolated</b><br>Concentration: %{x:.3~g} ",
          conc_unit,
          "<br>k<sub>obs</sub>: %{y:.3~g} ",
          time_unit,
          "⁻¹<extra></extra>"
        ),
        showlegend = FALSE,
        inherit = FALSE
      )

    shapes <- c(
      shapes,
      list(list(
        type = "rect",
        xref = "x",
        yref = "paper",
        x0 = lx(0),
        x1 = lx(max_conc),
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
    )
    annotations <- c(
      annotations,
      list(list(
        x = lx(max_conc),
        y = 1,
        xref = "x",
        yref = "paper",
        text = "measured",
        showarrow = FALSE,
        xanchor = "right",
        yanchor = "top",
        font = list(size = 12, color = font_color)
      ))
    )
  }

  x_end <- NULL
  # Optional guide to the constants: kinact as the level the curve approaches,
  # KI where it reaches half of it, kinact/KI as its initial slope. Without
  # saturation only the slope is defined, and a note says so.
  if (isTRUE(show_constants) && !fit_failed && nrow(df)) {
    fmt3 <- function(x) trimws(formatC(x, digits = 3, format = "g"))
    ratio <- kinact_ki_result$Ratio[["Estimate"]]
    saturated <- identical(kinact_ki_result$Status, "saturated")
    measured_max <- max(as.numeric(as.character(df_points$conc)), na.rm = TRUE)
    kinact <- if (saturated) kinact_ki_result$Params["kinact", 1] else NA_real_
    ki <- if (saturated) kinact_ki_result$Params["KI", 1] else NA_real_
    x_end <- if (extrapolate) {
      extrap_max
    } else if (saturated) {
      max(measured_max, 1.25 * ki)
    } else {
      measured_max
    }
    # Reference lines, set apart from the curves by being thin and faded; the
    # labels tell them apart
    guide <- list(color = font_color, width = 1, dash = "dot")
    guide_opacity <- 0.7

    # The model beyond the measured range, dashed, when KI lies beyond it
    if (saturated && !extrapolate && x_end > measured_max) {
      grid_beyond <- if (log_conc) {
        exp(seq(log(measured_max), log(x_end), length.out = 100))
      } else {
        seq(measured_max, x_end, length.out = 100)
      }
      kobs_plot <- plotly::add_lines(
        kobs_plot,
        x = grid_beyond,
        y = kobs_model_at(kinact_ki_result, grid_beyond),
        line = extrap_line,
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      )
    }

    # Initial slope: kobs = (kinact/KI) * [I] from the origin. It meets the
    # kinact level exactly at [I] = KI, which the drawing shows.
    # A linear fit is that slope itself, and the guide on top of it (and on
    # its dashed extrapolation) would only blur the line.
    # On the log axis the straight slope bends, so it is drawn as a line.
    if (!identical(kinact_ki_result$Model, "linear")) {
      x_slope <- if (saturated) ki else x_end
      if (log_conc) {
        grid_slope <- exp(seq(log(x_left), log(x_slope), length.out = 100))
        kobs_plot <- plotly::add_lines(
          kobs_plot,
          x = grid_slope,
          y = ratio * grid_slope,
          line = guide,
          opacity = guide_opacity,
          hoverinfo = "skip",
          showlegend = FALSE,
          inherit = FALSE
        )
      } else {
        shapes <- c(shapes, list(list(
          type = "line", xref = "x", yref = "y",
          x0 = 0, y0 = 0, x1 = x_slope, y1 = ratio * x_slope,
          line = guide, opacity = guide_opacity
        )))
      }
    }
    slope_text <- paste0(
      "initial slope k<sub>inact</sub>/K<sub>I</sub> = ", fmt3(ratio), " ",
      conc_unit, "⁻¹ ", time_unit, "⁻¹"
    )

    if (saturated) {
      # Labelled below the kinact line at the left, where the plot is empty
      annotations <- c(annotations, list(list(
        x = lx(0), y = kinact, xref = "x", yref = "y",
        text = slope_text,
        showarrow = FALSE, xanchor = "left", yanchor = "top", xshift = 4,
        font = list(size = 12, color = font_color)
      )))
      shapes <- c(
        shapes,
        list(
          # kinact: the level k_obs approaches at saturation
          list(
            type = "line", xref = "x", yref = "y",
            x0 = lx(0), x1 = lx(x_end), y0 = kinact, y1 = kinact,
            line = guide, opacity = guide_opacity
          ),
          # KI: the concentration at half of kinact
          list(
            type = "line", xref = "x", yref = "y",
            x0 = lx(ki), x1 = lx(ki), y0 = 0, y1 = kinact / 2,
            line = guide, opacity = guide_opacity
          ),
          list(
            type = "line", xref = "x", yref = "y",
            x0 = lx(0), x1 = lx(ki), y0 = kinact / 2, y1 = kinact / 2,
            line = guide, opacity = guide_opacity
          )
        )
      )
      annotations <- c(
        annotations,
        list(
          list(
            x = lx(x_end), y = kinact, xref = "x", yref = "y",
            text = paste0("k<sub>inact</sub> = ", fmt3(kinact), " ", time_unit, "⁻¹"),
            showarrow = FALSE, xanchor = "right", yanchor = "bottom",
                        font = list(size = 12, color = font_color)
          ),
          list(
            x = lx(ki), y = 0, xref = "x", yref = "y",
            text = paste0("K<sub>I</sub> = ", fmt3(ki), " ", conc_unit),
            showarrow = FALSE, xanchor = "left", yanchor = "bottom",
            xshift = 4, font = list(size = 12, color = font_color)
          ),
          list(
            x = lx(0), y = kinact / 2, xref = "x", yref = "y",
            text = "½ k<sub>inact</sub>",
            showarrow = FALSE, xanchor = "left", yanchor = "bottom",
            xshift = 4, font = list(size = 11, color = font_color)
          )
        )
      )
      # The half-maximum point
      kobs_plot <- plotly::add_markers(
        kobs_plot,
        x = ki,
        y = kinact / 2,
        marker = list(
          size = 9,
          symbol = "circle-open",
          color = font_color,
          line = list(width = 2, color = font_color)
        ),
        hovertemplate = paste0(
          "K<sub>I</sub>: %{x:.3~g} ", conc_unit,
          "<br>½ k<sub>inact</sub>: %{y:.3~g} ", time_unit, "⁻¹<extra></extra>"
        ),
        showlegend = FALSE,
        inherit = FALSE
      )
      # Invisible point keeping kinact and its label inside the axis range
      kobs_plot <- plotly::add_markers(
        kobs_plot,
        x = x_end,
        y = 1.08 * kinact,
        marker = list(size = 1, opacity = 0),
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      )
    } else {
      # Slope and note in the top left corner, which a curve without
      # saturation leaves empty (its points rise to the right). Two labels
      # with fixed gaps, below the "measured" label of the shading, which
      # also sits at the top; the note set apart in italics.
      top <- if (extrapolate) -26 else -2
      corner_label <- function(text, yshift, size) {
        list(
          text = text,
          xref = "paper", yref = "paper", x = 0, y = 1,
          xanchor = "left", yanchor = "top", align = "left",
          xshift = 6, yshift = yshift,
          showarrow = FALSE,
          font = list(size = size, color = font_color)
        )
      }
      annotations <- c(annotations, list(
        corner_label(slope_text, top, 12),
        corner_label(
          paste0(
            "<i>k<sub>inact</sub> and K<sub>I</sub> not determinable: ",
            "the data define the initial slope only</i>"
          ),
          top - 24,
          11
        )
      ))
    }
  }

  # A failed fit without any usable kobs point leaves no trace
  kobs_plot <- ensure_trace(kobs_plot) |>
    plotly::layout(
      shapes = shapes,
      annotations = annotations,
      hovermode = "closest",
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(size = 14, color = font_color),
      legend = list(
        bgcolor = "rgba(0,0,0,0)",
        bordercolor = "rgba(0,0,0,0)",
        font = list(color = font_color),
        # Hiding single traces breaks the curve/point pairing of this plot
        itemclick = FALSE,
        itemdoubleclick = FALSE
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
        if (log_conc) {
          # The measured concentrations as ticks, labelled like the legend
          x_right <- max(conc_measured, if (extrapolate) extrap_max, x_end)
          list(
            type = "log",
            tickmode = "array",
            tickvals = conc_measured,
            ticktext = conc_display(conc_measured),
            range = log10(c(x_left, 1.15 * x_right))
          )
        } else {
          sci_axis_ticks
        }
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

# Binding surface: the global fit as one model over time and concentration ----
#
# Binding(t, [I]) = P([I]) * (1 - exp(-kobs([I]) * t)), with kobs([I]) of the
# fitted model, drawn as a surface; the curves at the measured concentrations
# (the Binding Curve) lie on it, the samples around it. Between measured
# concentrations the plateau is interpolated linearly (held constant beyond
# them), which is display only: the fit has plateaus at the measured
# concentrations alone. `log_conc` puts the concentration axis on a log scale
# and leaves out the untreated control.
#' @export
make_binding_surface_plot <- function(
  kinact_ki_result,
  kobs_result,
  colors,
  units,
  theme = "light",
  log_conc = FALSE
) {
  pl <- kinact_ki_result$Plateaus
  if (is.null(pl) || !nrow(pl) || is.null(kobs_result$binding_points)) {
    return(kinetics_message_plot("No global fit to show", theme))
  }
  pl <- pl[order(pl$conc), , drop = FALSE]
  pal <- kinetics_plot_colors(theme)
  conc_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]])
  time_unit <- gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])

  pts <- kobs_result$binding_points
  pts$conc_num <- suppressWarnings(as.numeric(as.character(pts$concentration)))
  pts <- pts[!is.na(pts$binding) & !is.na(pts$conc_num), , drop = FALSE]
  if (isTRUE(log_conc)) {
    pts <- pts[pts$conc_num > 0, , drop = FALSE]
  }

  conc_grid <- if (isTRUE(log_conc)) {
    exp(seq(log(min(pl$conc)), log(max(pl$conc)), length.out = 60))
  } else {
    seq(0, max(pl$conc), length.out = 60)
  }
  time_grid <- seq(0, max(pts$time, na.rm = TRUE), length.out = 80)
  plateau_at <- stats::approx(pl$conc, pl$plateau, xout = conc_grid, rule = 2)$y
  kobs_at <- kobs_model_at(kinact_ki_result, conc_grid)
  z <- plateau_at * (1 - exp(-outer(kobs_at, time_grid)))

  # Neutral surface, so the concentration colours of curves and samples stand
  # out on it
  scale <- if (theme == "light") {
    list(c(0, "#e3e6eb"), c(1, "#3f4b63"))
  } else {
    list(c(0, "#3f4b63"), c(1, "#e3e6eb"))
  }

  p <- plotly::plot_ly() |>
    plotly::add_surface(
      x = time_grid,
      y = conc_grid,
      z = z,
      colorscale = scale,
      showscale = FALSE,
      opacity = 0.55,
      name = "Global fit",
      hovertemplate = paste0(
        "<b>Global fit</b><br>Time: %{x:.3~g} ",
        time_unit,
        "<br>Compound: %{y:.3~g} ",
        conc_unit,
        "<br>Binding: %{z:.1f} %<extra></extra>"
      )
    )

  # Label of a measured concentration as the plots key it (the points' level)
  key_of <- function(conc) {
    levels_chr <- unique(as.character(pts$concentration))
    num <- suppressWarnings(as.numeric(levels_chr))
    hit <- which(abs(num - conc) <= 1e-9 * max(abs(conc), 1e-300))
    if (length(hit)) levels_chr[hit[1]] else as.character(conc)
  }

  # The global curve of every fitted concentration, on the surface
  for (i in seq_len(nrow(pl))) {
    key <- key_of(pl$conc[i])
    curve <- pl$plateau[i] *
      (1 - exp(-kobs_model_at(kinact_ki_result, pl$conc[i]) * time_grid))
    p <- plotly::add_trace(
      p,
      type = "scatter3d",
      mode = "lines",
      x = time_grid,
      y = rep(pl$conc[i], length(time_grid)),
      z = curve,
      line = list(width = 6, color = unname(colors[key])),
      name = conc_display(key),
      legendgroup = key,
      showlegend = FALSE,
      hovertemplate = paste0(
        "<b>Global fit, ",
        conc_display(key),
        " ",
        conc_unit,
        "</b><br>Time: %{x:.3~g} ",
        time_unit,
        "<br>Binding: %{z:.1f} %<extra></extra>"
      )
    )
  }

  # The samples, highest concentration first in the legend; the symbol tells
  # the replicate series apart (filled / open circle, filled / open square)
  series <- if ("series" %in% names(pts)) as.character(pts$series) else NA
  series[is.na(series) | series == ""] <- "–"
  pts$series_label <- series
  symbols <- series_symbols_3d(series)
  multi_series <- length(symbols) > 1
  keys <- unique(as.character(pts$concentration[order(-pts$conc_num)]))
  for (key in keys) {
    first <- TRUE
    for (s in names(symbols)) {
      d <- pts[
        as.character(pts$concentration) == key & pts$series_label == s,
        ,
        drop = FALSE
      ]
      if (!nrow(d)) {
        next
      }
      p <- plotly::add_trace(
        p,
        type = "scatter3d",
        mode = "markers",
        x = d$time,
        y = d$conc_num,
        z = d$binding,
        marker = list(
          size = 3.5,
          color = unname(colors[key]),
          symbol = symbols[[s]],
          line = list(width = 0.5, color = pal$font)
        ),
        text = paste0(d$Sample, if (multi_series) paste0("<br>Series: ", s) else ""),
        name = conc_display(key),
        legendgroup = key,
        showlegend = first,
        hovertemplate = paste0(
          "<b>Sample</b><br>%{text}<br>Time: %{x:.3~g} ",
          time_unit,
          "<br>Binding: %{z:.1f} %<extra></extra>"
        )
      )
      first <- FALSE
    }
  }
  if (multi_series) {
    p <- add_series_legend(p, symbols, pal$font, scene = TRUE)
  }

  # Scene axes as the cubic spectrum sets them (multiple_spectra()): explicit
  # tick and title sizes, which the plot export reads to scale 3D labels on
  # their own, shallower ladder and to keep them clear of each other; no
  # backgrounds or spikes
  axis <- function(title, extra = list()) {
    c(
      list(
        title = list(text = title, font = list(size = 14, color = pal$font)),
        tickfont = list(size = 12, color = pal$font),
        color = pal$font,
        gridcolor = pal$grid,
        showgrid = TRUE,
        showline = FALSE,
        zerolinecolor = pal$zero,
        showspikes = FALSE,
        showbackground = FALSE
      ),
      extra
    )
  }

  p |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(size = 14, color = pal$font),
      legend = list(
        title = list(
          text = paste0("Concentration [", conc_unit, "]  "),
          font = list(color = pal$font)
        ),
        bgcolor = "rgba(0,0,0,0)",
        bordercolor = "rgba(0,0,0,0)",
        font = list(size = 11, color = pal$font),
        itemsizing = "constant",
        tracegroupgap = 4
      ),
      scene = list(
        xaxis = axis(paste0("Time [", time_unit, "]")),
        # On the log axis plotly's own ticks (digits between the decades)
        # crowd and overlap; tick the measured concentrations instead,
        # labelled like the legend
        yaxis = axis(
          paste0("Compound [", conc_unit, "]"),
          if (isTRUE(log_conc)) {
            ticks <- unique(pts[, c("conc_num", "concentration")])
            ticks <- ticks[order(ticks$conc_num), , drop = FALSE]
            list(
              type = "log",
              tickmode = "array",
              tickvals = ticks$conc_num,
              ticktext = conc_display(ticks$concentration)
            )
          } else {
            list()
          }
        ),
        zaxis = axis("Binding [%]", list(range = c(0, 105))),
        aspectmode = "manual",
        aspectratio = list(x = 1.15, y = 1, z = 0.7),
        camera = list(eye = list(x = 1.95, y = -2.05, z = 1.05))
      )
    )
}

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

# plotly_build() warns "No trace type specified and no positional attributes
# specified" for a plot_ly() root that never got a trace, as in message-only
# placeholders or a trace loop over empty data. An empty, typed trace keeps
# such plots quiet without drawing anything.
#' @export
ensure_trace <- function(p) {
  attrs <- p$x$attrs
  if (length(attrs) > 1 || !is.null(attrs[[1]]$type)) {
    return(p)
  }
  plotly::add_trace(
    p,
    type = "scatter",
    mode = "markers",
    x = numeric(0),
    y = numeric(0),
    showlegend = FALSE,
    hoverinfo = "skip"
  )
}

# Empty plot carrying a message (e.g. when a diagnostic does not apply)
kinetics_message_plot <- function(message, theme = "light") {
  pal <- kinetics_plot_colors(theme)
  plotly::plot_ly() |>
    ensure_trace() |>
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
  theme = "light",
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
  series_levels <- sort_series(pts$series)
  variants <- c("", "-open", "-dot", "-open-dot")
  series_variant <- stats::setNames(
    variants[(seq_along(series_levels) - 1) %% length(variants) + 1],
    series_levels
  )
  multi_series <- length(series_levels) > 1

  pts$hover <- paste0(
    "<b>",
    pts$sample,
    "</b>",
    if (multi_series) paste0("<br>Series: ", pts$series) else "",
    "<br>Concentration: ",
    conc_display(pts$concentration),
    " ",
    conc_unit,
    "<br>Time: ",
    signif(pts$time, 4),
    " ",
    time_unit,
    "<br>Observed: ",
    sprintf("%.2f", pts$binding),
    " %",
    "<br>Fitted: ",
    sprintf("%.2f", pts$fitted),
    " %",
    "<br>Residual: ",
    sprintf("%+.2f", pts$residual),
    " %"
  )

  p <- plotly::plot_ly()
  for (conc_name in levels_desc) {
    conc_color <- unname(colors[conc_name])
    if (is.na(conc_color)) {
      conc_color <- pal$muted
    }
    first <- TRUE
    for (s in series_levels) {
      sub <- pts[
        as.character(pts$concentration) == conc_name & pts$series == s,
        ,
        drop = FALSE
      ]
      if (nrow(sub) == 0) {
        next
      }
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
        name = conc_display(conc_name),
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

  # Legend entries explaining the fill of each series
  if (multi_series) {
    p <- add_series_legend(
      p,
      stats::setNames(paste0("circle", series_variant), names(series_variant)),
      pal$font
    )
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
    yaxis = kinetics_axis("Residual [% Binding]", pal),
    shapes = list(
      guide(0, "solid"),
      guide(spread, "dot"),
      guide(-spread, "dot")
    ),
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
# tick, as in the Per-Concentration Fits table): a long stem means the curve was
# still rising when measurement stopped, so that plateau is extrapolated. The
# bar is the plateau the global fit used; one bar across several
# concentrations means they share it because none of them got close enough to
# define its own.
#' @export
make_kinetics_plateau_plot <- function(
  kinact_ki_result,
  colors,
  units,
  theme = "light",
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
      if (nrow(sub) == 0) {
        return(c(NA_real_, NA_real_))
      }
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

  hover_head <- paste0("<b>", conc_display(tab$label), " ", conc_unit, "</b><br>")

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
      legendgroup = legend_key_group("observed"),
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
        ticktext = conc_display(tab$label),
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
  theme = "light"
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
        "<b>%{y}</b><br>k<sub>inact</sub>/K<sub>I</sub>: %{x:.4~g} ",
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
        "<b>All samples</b><br>k<sub>inact</sub>/K<sub>I</sub>: %{x:.4~g} ",
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
      paste0("k<sub>inact</sub>/K<sub>I</sub> [", ratio_unit, "]"),
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
  theme = "light",
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
        text = sprintf(
          "K<sub>I</sub> ≈ %s %s<br>%s",
          conc_display(signif(ki, 3)),
          conc_unit,
          selected_label
        ),
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
# per compound, mass shift and protein species, all carrying the same total
# binding; those rows are copies of a single measurement, not repeats of the
# experiment. Without this a sample with more hits would weigh more in the
# fits and would shrink the per-timepoint SD.
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

# Replicate series a sample belongs to, used for the per-series fits: its
# Replicate value (see compute_replicate_labels()), else the _R<n> suffix of
# its name, else NA. Tables saved before 0.7.5 held the condition name in the
# Replicate column (the sample name without _R<n>, the same for R1 and R2);
# such a value is read as missing, so the suffix names the series.
kinetic_series_labels <- function(samples, replicate = NULL) {
  suffix <- replicate_suffix(samples)

  if (is.null(replicate)) {
    return(suffix)
  }

  stem <- ms_sample_base(samples)
  rep_chr <- trimws(as.character(replicate))
  missing <- is.na(rep_chr) |
    rep_chr == "" |
    rep_chr == sub("_[Rr][0-9]+$", "", stem)
  ifelse(missing, suffix, rep_chr)
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
    # concentration_list, so it stays out of the Per-Concentration Fits table, the
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

# Classic two-step fit, shown next to the global fit as a cross-check ----
#
# The textbook route KiwiMS used until September 2026 and most users know from
# general curve-fitting programs: the k_obs of each concentration (the per-concentration
# fits of compute_kobs()) fitted to kobs = kinact * [I] / (KI + [I]),
# unweighted. kinact/KI is the quotient of the two estimates, its standard
# error from the delta method with their covariance. It does not feed any
# result: unweighted, it gives the least precise k_obs (high concentrations,
# curves nearly complete at the first time point) as much say as the precise
# ones, which is why the global fit replaced it. NULL with fewer than three
# concentrations or when the fit fails.
#' @export
classic_two_step <- function(kobs_result) {
  tbl <- kobs_result$kobs_result_table
  if (is.null(tbl) || !nrow(tbl)) {
    return(NULL)
  }
  d <- data.frame(conc = as.numeric(rownames(tbl)), kobs = tbl$kobs)
  d <- d[is.finite(d$conc) & d$conc > 0 & is.finite(d$kobs), , drop = FALSE]
  if (nrow(d) < 3 || max(d$kobs) <= 0) {
    return(NULL)
  }

  # Fitted on scaled values (concentration and k_obs over their maxima), so
  # the parameters are of order one whatever the units
  conc_max <- max(d$conc)
  kobs_max <- max(d$kobs)
  x <- d$conc / conc_max
  y <- d$kobs / kobs_max
  lower <- c(0, 1e-6)
  upper <- c(Inf, 1e6)

  fits <- lapply(c(0.5, 2, 10), function(ki) {
    tryCatch(
      minpack.lm::nls.lm(
        par = c(kinact = 1 + 1 / ki, KI = ki),
        fn = function(p) y - p[1] * x / (p[2] + x),
        lower = lower,
        upper = upper,
        control = minpack.lm::nls.lm.control(maxiter = 1000)
      ),
      error = function(e) NULL
    )
  })
  fits <- Filter(Negate(is.null), fits)
  if (!length(fits)) {
    return(NULL)
  }
  fit <- fits[[which.min(vapply(fits, function(f) f$deviance, numeric(1)))]]
  cov <- nls_lm_covariance(fit, nrow(d), lower, upper)

  kinact_n <- unname(fit$par[1])
  ki_n <- unname(fit$par[2])
  # d(kinact/KI) / d(kinact, KI)
  grad <- c(1 / ki_n, -kinact_n / ki_n^2)
  ratio_var <- as.numeric(t(grad) %*% cov$vcov %*% grad)

  list(
    kinact = kinact_n * kobs_max,
    kinact_se = cov$se[1] * kobs_max,
    KI = ki_n * conc_max,
    KI_se = cov$se[2] * conc_max,
    ratio = kinact_n / ki_n * kobs_max / conc_max,
    ratio_se = if (is.finite(ratio_var)) {
      sqrt(ratio_var) * kobs_max / conc_max
    } else {
      NA_real_
    },
    n_concentrations = nrow(d)
  )
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
  points <- do.call(
    rbind,
    lapply(conc_names, function(i) {
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
    })
  )

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
  curved <- identical(models$selected, hyperbolic_fit) &&
    !is.null(hyperbolic_fit)

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
    if (
      n_boot_ok <
        kinetics_settings$bootstrap_min_success *
          kinetics_settings$bootstrap_n
    ) {
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
  if (
    length(group_plateaus) >= 2 &&
      diff(range(group_plateaus)) > kinetics_settings$plateau_max_spread
  ) {
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
    series <- do.call(
      rbind,
      lapply(sort_series(labels), function(s) {
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
      })
    )
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

  # The classic two-step result for comparison, and whether it lies inside
  # the 95 % interval of the global fit (NA without an interval)
  two_step <- classic_two_step(kobs_result)
  if (!is.null(two_step)) {
    two_step$within_ci <- if (all(is.finite(ratio_ci))) {
      two_step$ratio >= ratio_ci[1] && two_step$ratio <= ratio_ci[2]
    } else {
      NA
    }
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
    Two_Step = two_step,
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
  # No value, e.g. the parameters of a kinact/KI fit that failed
  if (!is.numeric(number) || length(number) != 1 || !is.finite(number)) {
    return("N/A")
  }

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

# Legend group of a legend row that only explains a symbol or colour and draws
# no data of its own. A click on such a row would hide nothing but the row, and
# a double click would hide every trace with data, leaving an empty figure, so
# the app-wide legend guard (main.R) ignores clicks on rows in these groups.
# The prefix is what the guard checks.
#' @export
legend_key_group <- function(id) {
  paste0("kiwims-key-", id)
}

# 0-based indices of the tagged traces in a built plotly figure, ready to hand
# to plotlyProxyInvoke("restyle", ...), which indexes traces the JS way. The
# hit markers carry peaks_trace_tag, the unmatched peaks unmatched_trace_tag.
#' @export
peaks_trace_indices <- function(built_plot, tag = peaks_trace_tag) {
  traces <- built_plot$x$data

  if (is.null(traces) || length(traces) == 0) {
    return(integer(0))
  }

  # plotly treats `meta` as arrayOk and recycles a scalar along the trace's
  # points, so the tag comes back as a vector on multi-point traces.
  tagged <- vapply(
    traces,
    function(trace) {
      meta <- as.character(trace$meta)
      length(meta) > 0 && all(meta == tag)
    },
    logical(1)
  )

  which(tagged) - 1L
}

# A 3D scene keeps its full domain when a legend is shown — the legend simply
# overlays the right-hand side — so the camera is nudged left to clear it.
#' @export
spectrum_legend_camera_x <- 0.33

# The marker rows of a spectra legend (name, symbol): each protein by name,
# then each compound by name when the compounds carry the colours. Coloured
# by anything else, every compound's row would show the same uncoloured
# shape - a list as long as the compounds that tells them apart by nothing -
# so compounds sharing a shape share one "Compounds" row instead. A shape
# only one compound has still names it.
spectrum_marker_entries <- function(peaks_data, color_variable) {
  entries <- dplyr::distinct(
    peaks_data[!is.na(peaks_data$name), c("name", "symbol"), drop = FALSE]
  )
  # radix: the C-locale order dplyr::arrange() gives, as elsewhere
  entries <- entries[order(entries$name, method = "radix"), , drop = FALSE]
  proteins <- entries[entries$symbol == "diamond", , drop = FALSE]
  compounds <- entries[entries$symbol != "diamond", , drop = FALSE]
  if (!identical(color_variable, "Compounds") && nrow(compounds)) {
    shared <- names(which(table(compounds$symbol) > 1))
    collapsed <- if (length(shared)) {
      data.frame(
        name = if (length(unique(compounds$symbol)) == 1) {
          "Compounds"
        } else {
          paste0("Compounds (", gsub("-", " ", shared), ")")
        },
        symbol = shared
      )
    }
    compounds <- rbind(
      compounds[!compounds$symbol %in% shared, , drop = FALSE],
      collapsed
    )
  }
  rbind(proteins, compounds)
}

# The three settings below only change how the figure is presented, never the
# data behind it, so they are pushed to the figure that is already on screen
# (plotlyProxy, or plain JS where the legend state matters) rather than costing
# a full rebuild and re-serialization.

# Show or hide the peak symbols and the legend rows that name them. The traces
# are always built, so this only flips their `visible` flag. `tag` picks the
# hit markers or the unmatched peaks.
#
# Markers grouped with a sample follow that sample's line when shown: a sample
# hidden in the legend (or every other one, after a double-click) keeps its
# markers hidden. Only the browser knows the legend state, so the visibility
# is resolved there rather than sent as a flat `visible = TRUE`.
#' @export
restyle_peak_symbols <- function(
  session,
  output_id,
  plot_reactive,
  show,
  tag = peaks_trace_tag
) {
  # The figure is behind bindEvent(), so this returns the last built value and
  # never kicks off a build of its own; before the first render there is no
  # value and nothing to patch.
  indices <- tryCatch(
    peaks_trace_indices(shiny::isolate(plot_reactive()), tag = tag),
    error = function(e) integer(0)
  )

  if (length(indices) == 0) {
    return(invisible(NULL))
  }

  shinyjs::runjs(sprintf(
    paste0(
      "(function() {",
      "  var gd = document.getElementById('%s');",
      "  if (!gd || !gd.data) return;",
      "  var idx = [%s].filter(function(i) { return i < gd.data.length; });",
      "  if (!idx.length) return;",
      "  var show = %s;",
      "  var vis = idx.map(function(i) {",
      "    if (!show) return false;",
      "    var g = gd.data[i].legendgroup;",
      "    if (!g) return true;",
      "    for (var j = 0; j < gd.data.length; j++) {",
      "      var t = gd.data[j];",
      "      if (t.mode === 'lines' && t.legendgroup === g) {",
      "        return (t.visible === 'legendonly' || t.visible === false) ?",
      "          'legendonly' : true;",
      "      }",
      "    }",
      "    return true;",
      "  });",
      "  Plotly.restyle(gd, {visible: vis}, idx);",
      "})();"
    ),
    session$ns(output_id),
    paste(indices, collapse = ","),
    tolower(isTRUE(show))
  ))

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

# Order hit rows so that their samples run from least to most bound
#
# This is the order the Compound Distribution plots put their samples in (their
# colours are fixed by the result's colour key, see color_key()). `by_mean` picks
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

# Overview tab of the Relative Binding interface ----
#
# The Overview shows one protein with any of the compounds it was declared
# with. The hits list every declared protein-compound pair of a sample, a pair
# without an adduct on a row of its own (see check_hits()), so the pairs are
# read off the hits: the declaration is not needed and results loaded without
# it work the same.

# The proteins of the run and the compounds declared with each, for the
# Overview pickers. `proteins$status` is "hits" when a complex of the protein
# has a hit, "no_hits" when it has complexes but none with a hit, and "none"
# when it was declared without any compound (controls only). The proteins come
# in that order, keeping the order of the hits within each group, so the
# default pick is a protein with hits. The compounds of a protein list those
# with a hit first.
#' @export
overview_choices <- function(hits_summary) {
  prot <- as.character(hits_summary$Protein)
  cmp <- as.character(hits_summary$`Cmp Name`)
  named_prot <- !is.na(prot) & nzchar(trimws(prot))
  named_cmp <- !is.na(cmp) & nzchar(trimws(cmp)) & cmp != "N/A"
  complex <- is_complex_row(hits_summary)

  proteins <- unique(prot[named_prot])
  compounds <- lapply(stats::setNames(proteins, proteins), function(p) {
    rows <- named_prot & prot == p & named_cmp
    cmps <- unique(cmp[rows])
    hit <- cmps %in% cmp[rows & complex]
    data.frame(
      compound = c(cmps[hit], cmps[!hit]),
      hit = c(rep(TRUE, sum(hit)), rep(FALSE, sum(!hit)))
    )
  })

  status <- vapply(
    compounds,
    function(d) {
      if (!nrow(d)) {
        "none"
      } else if (any(d$hit)) {
        "hits"
      } else {
        "no_hits"
      }
    },
    character(1)
  )
  ord <- order(match(status, c("hits", "no_hits", "none")))

  list(
    proteins = data.frame(
      protein = proteins[ord],
      status = unname(status[ord])
    ),
    compounds = compounds
  )
}

# Picker subtext of an Overview protein or compound: empty for one with hits
#' @export
overview_status_note <- function(status) {
  unname(c(hits = "", no_hits = "No hits", none = "No compounds")[status])
}

# A valid Overview selection from overview_choices(): the protein asked for,
# else the default one, with the asked compounds it was declared with, else all
# of them
#' @export
overview_selection <- function(choices, protein = NULL, compounds = NULL) {
  proteins <- choices$proteins$protein
  if (!length(protein) || !protein[1] %in% proteins) {
    protein <- proteins[1]
  }
  protein <- protein[1]

  declared <- if (is.na(protein)) {
    character(0)
  } else {
    choices$compounds[[protein]]$compound
  }
  compounds <- if (is.null(compounds)) {
    declared
  } else {
    declared[declared %in% compounds]
  }

  list(protein = protein, compounds = compounds)
}

# The hits rows of the Overview selection: one protein with the picked
# compounds. Samples declared without any of them (controls) belong to no
# complex and are left out.
#' @export
overview_subset <- function(hits_summary, protein, compounds) {
  hits_summary[
    hits_summary$Protein %in% protein & hits_summary$`Cmp Name` %in% compounds,
    ,
    drop = FALSE
  ]
}

# Colours of the Overview selection, shared by its distribution plot, spectra
# and table: the result's fixed colours (see color_key()) of the picked
# compounds ("Compounds"), or of the selection's samples - their own colour
# ("Samples") or that of their concentration, time or protein (named by the
# samples either way). The protein's other compounds are muted, as their
# peaks still show in the spectra of samples carrying several compounds.
#' @export
overview_colors <- function(
  hits_summary,
  protein,
  compounds,
  variable,
  key = NULL,
  trunc = FALSE,
  theme = "light"
) {
  key <- key %||% color_key(hits_summary)

  if (identical(variable, "Compounds")) {
    cmps <- hits_summary$`Cmp Name`[
      hits_summary$Protein %in% protein & !is.na(hits_summary$`Cmp Name`)
    ]
    cmps <- sort(unique(as.character(cmps)), method = "radix")
    picked <- cmps[cmps %in% compounds]
    rest <- cmps[!cmps %in% compounds]
  } else {
    sub <- binding_ordered_hits(
      overview_subset(hits_summary, protein, compounds),
      truncate_names = trunc,
      by_mean = TRUE
    )
    picked <- unique(as.character(
      if (isTRUE(trunc)) sub$truncSample_ID else sub$`Sample ID`
    ))
    rest <- character(0)
  }

  c(
    if (identical(variable, "Compounds")) {
      key_colors(key, variable, picked, theme = theme, trunc = trunc)
    } else {
      sample_level_colors(key, variable, picked, theme = theme, trunc = trunc)
    },
    stats::setNames(rep(status_colors(theme)$muted, length(rest)), rest)
  )
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
  unmatched_show = FALSE,
  time = FALSE,
  color_cmp = NULL,
  symbol_cmp = NULL,
  truncated = FALSE,
  color_variable = NULL,
  hits_summary = NULL,
  units = NULL,
  # Declared to displayed time unit, for the colour key's times (the
  # spectra's own times come in the displayed unit with `hits_summary`)
  time_factor = 1,
  max_points = NULL,
  theme = "light",
  plot_data = NULL,
  key = NULL
) {
  # Omit NA in samples
  samples <- samples[!is.na(samples)]

  # Treat unset toggles as enabled
  symbols_show <- !isFALSE(symbols_show)
  legend_show <- !isFALSE(legend_show)

  # Points per spectrum, after the 1 Da display bins: at most 1500, fewer the
  # more spectra share the figure, about 40k in all. Min/max decimation keeps
  # every peak apex. A full plate used to send hundreds of thousands of
  # points (some 8 MB for 108 spectra), slow to build, send and draw.
  if (is.null(max_points)) {
    max_points <- max(250, min(1500, floor(40000 / max(1, length(samples)))))
  }

  # Get spectrum and peaks data. process_plot_data() parses the whole sample,
  # so call it once and take both pieces from the same result. Collecting the
  # per-sample frames in a list and binding once at the end avoids the
  # quadratic reallocation of growing a data.frame with rbind() in a loop.
  spectrum_parts <- vector("list", length(samples))
  peaks_parts <- vector("list", length(samples))
  unmatched_parts <- vector("list", length(samples))

  # Time view: each spectrum's time and replicate as declared in the sample
  # table, from the hits table the caller passes (its time column already in
  # the displayed unit). Nothing is read from sample names.
  if (time) {
    declared <- match(samples, hits_summary$`Sample ID`)
    time_of <- suppressWarnings(
      as.numeric(hits_summary[[units[["Time"]]]][declared])
    )
    replicate_of <- if ("Replicate" %in% names(hits_summary)) {
      as.character(hits_summary$Replicate[declared])
    } else {
      rep(NA_character_, length(samples))
    }
  }

  for (i in seq_along(samples)) {
    # A spectrum without a declared time has no place on the time axis
    if (time && is.na(time_of[i])) {
      next
    }

    # `plot_data` hands over the spectra ready-made, one per sample and named
    # by it (the deconvolution page reads them from the run database), in
    # place of parsing them from results_list.
    sample_data <- if (!is.null(plot_data)) {
      plot_data[[samples[i]]]
    } else {
      process_plot_data(
        results_list$deconvolution[[samples[i]]],
        result_path = NULL
      )
    }

    z_value <- if (time) time_of[i] else samples[i]

    mass_df <- sample_data$mass
    if (!is.null(mass_df) && nrow(mass_df) > 0) {
      # The per-trace budget first, then at most one point per Da (display
      # resolution): binning the few points left is cheap, binning the full
      # spectrum first was not. Columns are set directly; dplyr::mutate() per
      # sample cost more than the rest of the loop. The peaks the markers
      # mark stay on the line, at their exact mass and height.
      peaks <- c(
        sample_data$highlight_peaks$mass,
        sample_data$unmatched_peaks$mass
      )
      part <- bin_spectrum(
        downsample_spectrum(mass_df, max_points = max_points, peaks = peaks),
        peaks = peaks
      )
      part$z <- z_value
      part$sample_key <- samples[i]
      spectrum_parts[[i]] <- part
    }

    peaks_df <- sample_data$highlight_peaks
    if (!is.null(peaks_df) && nrow(peaks_df) > 0) {
      peaks_df$z <- z_value
      peaks_df$sample_key <- samples[i]
      peaks_parts[[i]] <- peaks_df
    }

    unmatched_df <- sample_data$unmatched_peaks
    if (!is.null(unmatched_df) && nrow(unmatched_df) > 0) {
      unmatched_df$z <- z_value
      unmatched_df$sample_key <- samples[i]
      unmatched_parts[[i]] <- unmatched_df
    }
  }

  spectrum_data <- as.data.frame(dplyr::bind_rows(spectrum_parts))
  peaks_data <- as.data.frame(dplyr::bind_rows(peaks_parts))
  unmatched_data <- as.data.frame(dplyr::bind_rows(unmatched_parts))

  # Peaks straight from a deconvolution carry no assignment yet
  if (nrow(peaks_data) > 0) {
    if (!"name" %in% names(peaks_data)) {
      peaks_data$name <- NA_character_
    }
    if (!"mw" %in% names(peaks_data)) peaks_data$mw <- NA_real_
  }

  if (nrow(spectrum_data) == 0 || !("mass" %in% names(spectrum_data))) {
    return(
      plotly::plot_ly() |>
        ensure_trace() |>
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
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
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

  # Unmatched peaks share the spectrum's sample levels, so their traces land on
  # the same category of the sample axis as the spectrum they belong to
  if (nrow(unmatched_data) > 0) {
    if (!isFALSE(truncated)) {
      unmatched_data$z <- truncated$truncated[match(
        unmatched_data$z,
        truncated$original
      )]
    }
    unmatched_data$z <- factor(
      unmatched_data$z,
      levels = levels(spectrum_data$z)
    )
  }

  # The cubic view plots each spectrum at its position on the sample axis and
  # names the positions with tick labels. As a category axis, every point
  # carried its sample's name, which made up most of a large figure.
  # A level is a sample, or in the time view a time point; its spectra (the
  # replicates of that time) sit side by side around the level's position,
  # closer to each other than to the next level, in the order of their
  # declared replicates. Each spectrum is a trace of its own.
  spectra <- unique(spectrum_data$sample_key)
  spectrum_level <- as.integer(spectrum_data$z[
    match(spectra, spectrum_data$sample_key)
  ])
  replicate_label <- if (time) {
    replicate_of[match(spectra, samples)]
  } else {
    rep(NA_character_, length(spectra))
  }
  spectrum_order <- order(
    spectrum_level,
    replicate_label,
    match(spectra, samples),
    na.last = TRUE
  )
  spectra <- spectra[spectrum_order]
  spectrum_level <- spectrum_level[spectrum_order]
  replicate_label <- replicate_label[spectrum_order]
  spectrum_pos <- stats::setNames(numeric(length(spectra)), spectra)
  for (lvl in unique(spectrum_level)) {
    in_level <- spectrum_level == lvl
    m <- sum(in_level)
    gap <- if (m > 1) min(0.25, 0.7 / (m - 1)) else 0
    spectrum_pos[in_level] <- lvl + (seq_len(m) - (m + 1) / 2) * gap
  }
  # Named in hovers by their replicate, or their sample without one
  spectrum_suffix <- stats::setNames(
    ifelse(
      !is.na(replicate_label) & nzchar(trimws(replicate_label)),
      paste0(" (", replicate_label, ")"),
      ""
    ),
    spectra
  )
  shared <- spectrum_level %in% spectrum_level[duplicated(spectrum_level)]
  unnamed <- shared & !nzchar(spectrum_suffix)
  spectrum_suffix[unnamed] <- paste0(" (", spectra[unnamed], ")")

  spectrum_data$z_pos <- unname(spectrum_pos[spectrum_data$sample_key])
  if (nrow(peaks_data) > 0) {
    peaks_data$z_pos <- unname(spectrum_pos[peaks_data$sample_key])
  }
  if (nrow(unmatched_data) > 0) {
    unmatched_data$z_pos <- unname(spectrum_pos[unmatched_data$sample_key])
  }
  unmatched_show <- isTRUE(unmatched_show)
  um <- unmatched_marker(theme)

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

    # Compounds in the shape of their colour round when one is given; the
    # 3D view draws only circles, squares, diamonds and crosses
    cmp_symbol <- if (is.null(symbol_cmp)) {
      rep("circle", nrow(peaks_data))
    } else {
      s <- unname(symbol_cmp[as.character(peaks_data$name)])
      ifelse(is.na(s), "circle", s)
    }
    if (cubic) {
      cmp_symbol <- ifelse(cmp_symbol == "circle", "circle", "square")
    }
    peaks_data <- dplyr::mutate(
      peaks_data,
      symbol = ifelse(mass %in% prot_peaks, "diamond", cmp_symbol),
      linecolor = font_color
    )
  } else {
    # No peak of the samples was assigned: no protein species to mark
    prot_names <- character(0)
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
    }
  } else {
    # Time colours, placed among all times of the result when its colour
    # key is given (z is in the displayed time unit, the key in the declared
    # one), so a time point keeps its colour across concentrations
    color_cmp <- key_colors(
      list(
        times = if (is.null(key)) {
          as.numeric(levels(spectrum_data$z))
        } else {
          key$times * time_factor
        }
      ),
      "Time",
      levels(spectrum_data$z),
      theme = theme
    )

    # Protein peaks in the font colour, compound peaks in the colour of their
    # time point: an inverse (white or black) fill would vanish on the
    # export background of the same shade
    peaks_data <- dplyr::mutate(
      peaks_data,
      color = ifelse(
        symbol == "diamond",
        font_color,
        unname(color_cmp[match(as.character(z), names(color_cmp))])
      )
    )
    peaks_data$mk_color <- peaks_data$color
    marker_color <- ~ I(color)

    # Match colors to spectrum data
    spectrum_data$z_color <- color_cmp[match(
      spectrum_data$z,
      names(color_cmp)
    )]
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

  # Hover of the spectrum lines as a template, not a text string per point:
  # those strings made up most of the figure sent to the browser. The cubic
  # view reads the sample from its z axis, the planar one from the trace name.
  line_hover <- function(sample_ref, suffix = "") {
    paste0(
      "Mass: %{x:.1f} Da<br>Intensity: %{y:.2f}%<br>",
      if (time) {
        paste0(
          "Time: ",
          sample_ref,
          " ",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])
        )
      } else {
        paste0("Sample: ", sample_ref)
      },
      suffix,
      "<extra></extra>"
    )
  }

  # The per-sample traces -- each sample's spectrum line, then each sample's
  # peak markers -- ready-made, in sample order. They are appended to the
  # figure after plotly_build() (end of function): plotly spends some 20 ms
  # on every trace it builds, two per sample, so a full plate took seconds.
  # They are what plotly would have built; I() keeps the data arrays arrays.
  # The markers carry peaks_trace_tag and stay grouped with their sample, so
  # hiding a sample in the legend still hides its markers.
  # `line_color(lvl, rows)` gives a sample's colour; `marker` the marker trace
  # type, size and outline width.
  time_unit <- if (time) {
    paste0(" ", gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]))
  } else {
    ""
  }
  sample_traces <- function(type, line_color, line_width, marker) {
    # One legend row per level (a time point's replicates share it, and a
    # click on it hides them all)
    lines <- list()
    rows_by_spectrum <- split(
      seq_len(nrow(spectrum_data)),
      spectrum_data$sample_key
    )
    shown_levels <- character(0)
    for (s in spectra) {
      rows <- spectrum_data[rows_by_spectrum[[s]], , drop = FALSE]
      if (nrow(rows) == 0) {
        next
      }
      lvl <- as.character(rows$z[1])
      trace <- list(
        type = type,
        mode = "lines",
        x = I(round(rows$mass, 2)),
        y = I(round(rows$intensity, 2)),
        name = lvl,
        legendgroup = lvl,
        showlegend = !lvl %in% shown_levels,
        line = c(
          list(color = line_color(lvl, rows)),
          if (!is.null(line_width)) list(width = line_width)
        ),
        hovertemplate = line_hover("%{fullData.name}", spectrum_suffix[[s]])
      )
      shown_levels <- c(shown_levels, lvl)
      if (type == "scatter3d") {
        trace$z <- I(rows$z_pos)
      }
      lines[[length(lines) + 1]] <- trace
    }

    markers <- list()
    if (nrow(peaks_data) > 0) {
      peaks_by_spectrum <- split(
        seq_len(nrow(peaks_data)),
        peaks_data$sample_key
      )
      for (s in intersect(spectra, names(peaks_by_spectrum))) {
        peaks_lvl <- peaks_data[peaks_by_spectrum[[s]], , drop = FALSE]
        peaks_lvl <- peaks_lvl[!is.na(peaks_lvl$z), , drop = FALSE]
        if (nrow(peaks_lvl) == 0) {
          next
        }
        lvl <- as.character(peaks_lvl$z[1])
        trace <- list(
          type = marker$type,
          mode = "markers",
          x = I(peaks_lvl$mass),
          y = I(peaks_lvl$intensity),
          legendgroup = lvl,
          showlegend = FALSE,
          visible = symbols_show,
          meta = peaks_trace_tag,
          marker = list(
            color = I(peaks_lvl$mk_color),
            symbol = I(peaks_lvl$symbol),
            size = marker$size,
            zindex = 100,
            line = list(color = inv_color, width = marker$line_width)
          ),
          hoverinfo = "text",
          text = I(paste0(
            ifelse(
              is.na(peaks_lvl$name),
              "",
              paste0("Name: ", peaks_lvl$name, "<br>")
            ),
            "Measured: ",
            peaks_lvl$mass,
            " Da<br>Intensity: ",
            round(peaks_lvl$intensity, 2),
            if (time) "%<br>Time: " else "%<br>Sample: ",
            lvl,
            time_unit,
            spectrum_suffix[[s]],
            ifelse(
              is.na(peaks_lvl$mw),
              "",
              paste0("<br>Theor. Mw: ", peaks_lvl$mw)
            )
          ))
        )
        if (marker$type == "scatter3d") {
          trace$z <- I(peaks_lvl$z_pos)
        }
        markers[[length(markers) + 1]] <- trace
      }
    }

    c(lines, markers)
  }

  # Built and trimmed for the browser at the end (slim_spectrum())
  group_title <- NULL
  fig <- if (cubic) {
    # The sample's colour, or the font colour when the compounds carry it.
    # 3D markers are sized on a different scale than 2D ones.
    per_sample <- sample_traces(
      "scatter3d",
      function(lvl, rows) {
        if ("z_color" %in% names(rows)) rows$z_color[1] else font_color
      },
      line_width = 1,
      marker = list(type = "scatter3d", size = 5, line_width = 3)
    )
    plot <- plotly::plot_ly()

    per_sample <- c(
      per_sample,
      unmatched_sample_traces(
        unmatched_data,
        spectra = spectra,
        suffix = spectrum_suffix,
        cubic = TRUE,
        visible = unmatched_show,
        um = um,
        inv_color = inv_color,
        time = time,
        units = units
      )
    )
    plot <- add_unmatched_traces(
      plot,
      unmatched_data,
      cubic = TRUE,
      visible = unmatched_show,
      um = um,
      legend = !time
    )

    if (nrow(peaks_data) > 0 && !time) {
      name_entries <- spectrum_marker_entries(peaks_data, color_variable)

      # Marker symbols are listed without group titles: the symbol and name
      # say what each one marks, and a "Compounds" title would also seem to
      # claim the "Unmatched" row listed after it
      for (i in seq_len(nrow(name_entries))) {
        entry_name <- name_entries$name[i]
        sym <- name_entries$symbol[i]
        is_protein <- sym == "diamond"
        lg <- legend_key_group(if (is_protein) "proteins" else "compounds")
        # Coloured by compound, a compound's row shows its colour and shape:
        # the legend is where the colours are named. Otherwise it shows the
        # shape in the font colour, filled and outlined like the peaks.
        chip_color <- if (
          !is_protein &&
            identical(color_variable, "Compounds") &&
            entry_name %in% names(color_cmp)
        ) {
          unname(color_cmp[[entry_name]])
        }

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
            color = chip_color %||% font_color,
            symbol = sym,
            size = 8,
            line = list(color = inv_color, width = 1)
          ),
          visible = symbols_show,
          meta = peaks_trace_tag,
          showlegend = TRUE,
          legendrank = i,
          hoverinfo = "skip"
        )
        plot <- do.call(plotly::add_trace, args)
      }

      # The spectra are the time points of one concentration in time
      # mode; the marker rows above them need no title of their own.
      # Set on the first trace after the final build (end of function).
      group_title <- list(
        text = if (time) {
          paste0(
            "Time [",
            gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
            "]"
          )
        } else {
          "Samples"
        },
        font = list(color = font_color)
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
          title = list(text = "", color = font_color)
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
            # Sample positions (z_pos), named by their samples
            tickvals = seq_along(levels(spectrum_data$z)),
            ticktext = levels(spectrum_data$z)
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
      color_cmp
    }

    # Lines as WebGL rather than SVG: a spectrum runs to thousands of points
    # per sample, which the SVG renderer draws one path node at a time. The
    # single-sample spectrum_plot() already uses scattergl for the same
    # reason. Downloads go out via saveWidget(), so WebGL traces export fine.
    # Markers as SVG, though: plotly.js draws the WebGL canvas underneath the
    # SVG layers, so an SVG marker trace is always in front of the spectrum
    # lines. There are only a handful of peaks per sample, so nothing is lost.
    per_sample <- sample_traces(
      "scattergl",
      function(lvl, rows) {
        col <- unname(planar_colors[lvl])
        if (length(col) == 1 && !is.na(col)) col else font_color
      },
      line_width = NULL,
      marker = list(type = "scatter", size = 10, line_width = 1.5)
    )
    plot_2d <- plotly::plot_ly()

    per_sample <- c(
      per_sample,
      unmatched_sample_traces(
        unmatched_data,
        spectra = spectra,
        suffix = spectrum_suffix,
        cubic = FALSE,
        visible = unmatched_show,
        um = um,
        inv_color = inv_color,
        time = time,
        units = units
      )
    )
    plot_2d <- add_unmatched_traces(
      plot_2d,
      unmatched_data,
      cubic = FALSE,
      visible = unmatched_show,
      um = um,
      legend = TRUE
    )

    if (nrow(peaks_data) > 0) {
      name_entries <- spectrum_marker_entries(peaks_data, color_variable)

      # Marker symbols are listed without group titles: the symbol and name
      # say what each one marks, and a "Compounds" title would also seem to
      # claim the "Unmatched" row listed after it
      for (i in seq_len(nrow(name_entries))) {
        entry_name <- name_entries$name[i]
        sym <- name_entries$symbol[i]
        is_protein <- sym == "diamond"
        lg <- legend_key_group(if (is_protein) "proteins" else "compounds")
        # As in the cubic branch: the compound's colour when it has one,
        # otherwise the font colour, always filled and outlined like the peaks
        chip_color <- if (
          !is_protein &&
            identical(color_variable, "Compounds") &&
            entry_name %in% names(color_cmp)
        ) {
          unname(color_cmp[[entry_name]])
        }

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
            color = chip_color %||% font_color,
            symbol = sym,
            size = 8,
            line = list(color = inv_color, width = 1)
          ),
          visible = symbols_show,
          meta = peaks_trace_tag,
          showlegend = TRUE,
          legendrank = i,
          hoverinfo = "skip"
        )
        plot_2d <- do.call(plotly::add_trace, args)
      }

      # As in the cubic branch: set after the final build
      group_title <- list(
        text = if (time) {
          paste0(
            "Time [",
            gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
            "]"
          )
        } else {
          "Samples"
        },
        font = list(color = font_color)
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
          title = list(text = "", color = font_color)
        )
      )
  }

  # One build for the rest of the figure (legend rows, unmatched peaks,
  # layout), then the per-sample traces in front of it, as plotly would have
  # ordered them. The first spectrum titles the legend group (plotly::style()
  # built the whole figure once more just for that).
  # Without legend rows or unmatched peaks the rest has no trace of its own,
  # and plotly would build a placeholder one (and warn) from the bare
  # plot_ly(); only that bare entry is left in attrs then.
  if (length(fig$x$attrs) <= 1) {
    fig$x$attrs <- fig$x$attrs[0]
  }
  fig <- slim_spectrum(fig)
  # A build without traces counts as parcoords to plotly, which drops the top
  # margin then; plotly.js falls back to 100 px, and the legend hanging from
  # the plot area lost that much height and scrolled
  if (is.null(fig$x$layout$margin$t)) {
    fig$x$layout$margin$t <- 25
  }
  if (length(per_sample) > 0 && !is.null(group_title)) {
    per_sample[[1]]$legendgrouptitle <- group_title
  }
  fig$x$data <- c(per_sample, fig$x$data)
  fig
}

# Unmatched peak markers of multiple_spectra() ----
#
# One trace per sample, grouped with that sample's spectrum so hiding it in the
# legend hides its unmatched peaks too, plus a single legend row naming the
# symbol. Every trace carries unmatched_trace_tag, so the peaks are toggled on
# the rendered figure rather than by a rebuild.
#
# unmatched_sample_traces() gives the per-sample traces ready-made, appended
# with the other per-sample traces after plotly_build() (see multiple_spectra()
# for why); add_unmatched_traces() adds the legend row through plotly.
unmatched_sample_traces <- function(
  unmatched_data,
  spectra,
  suffix,
  cubic,
  visible,
  um,
  inv_color,
  time = FALSE,
  units = NULL
) {
  if (is.null(unmatched_data) || nrow(unmatched_data) == 0) {
    return(list())
  }

  time_unit <- if (time) {
    paste0(" ", gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]))
  } else {
    ""
  }

  # One trace per spectrum (`spectra`, in their order), grouped with its
  # level; `suffix` names a spectrum among the replicates of its level
  traces <- list()
  by_spectrum <- split(seq_len(nrow(unmatched_data)), unmatched_data$sample_key)
  for (s in intersect(spectra, names(by_spectrum))) {
    um_lvl <- unmatched_data[by_spectrum[[s]], , drop = FALSE]
    um_lvl <- um_lvl[!is.na(um_lvl$z), , drop = FALSE]

    if (nrow(um_lvl) == 0) {
      next
    }
    lvl <- as.character(um_lvl$z[1])

    trace <- list(
      # SVG like the hit markers, so they draw above the WebGL spectrum lines
      type = if (cubic) "scatter3d" else "scatter",
      mode = "markers",
      x = I(um_lvl$mass),
      y = I(um_lvl$intensity),
      legendgroup = lvl,
      visible = visible,
      meta = unmatched_trace_tag,
      # 3D markers are sized in a different scale than 2D ones. The 2D cross
      # is line-only and drawn by its line colour; the solid 3D one gets an
      # outline like the hit markers of that view.
      marker = list(
        color = um$color,
        symbol = if (cubic) um$symbol_3d else um$symbol,
        size = if (cubic) 3 else 8,
        line = list(
          color = if (cubic) inv_color else um$color,
          width = if (cubic) 2 else um$width
        )
      ),
      hoverinfo = "text",
      text = I(paste0(
        "Unmatched Peak",
        "<br>Measured: ",
        um_lvl$mass,
        " Da<br>Intensity: ",
        round(um_lvl$intensity, 2),
        if (time) "%<br>Time: " else "%<br>Sample: ",
        lvl,
        time_unit,
        suffix[[s]]
      )),
      showlegend = FALSE
    )
    if (cubic) {
      trace$z <- I(um_lvl$z_pos)
    }
    traces[[length(traces) + 1]] <- trace
  }
  traces
}

add_unmatched_traces <- function(
  plot,
  unmatched_data,
  cubic,
  visible,
  um,
  legend = TRUE
) {
  if (is.null(unmatched_data) || nrow(unmatched_data) == 0 || !legend) {
    return(plot)
  }

  args <- list(
    p = plot,
    inherit = FALSE,
    type = "scatter",
    mode = "markers",
    x = 0,
    y = 0,
    name = "Unmatched",
    legendgroup = legend_key_group("unmatched"),
    marker = list(
      color = um$color,
      symbol = um$symbol,
      size = 8,
      line = list(color = um$color, width = um$width)
    ),
    visible = visible,
    meta = unmatched_trace_tag,
    showlegend = TRUE,
    # After the protein and compound rows (ranked 1..n), ahead of the
    # sample rows (plotly's default rank of 1000)
    legendrank = 999,
    hoverinfo = "skip"
  )

  if (!cubic) {
    args$xaxis <- "x2"
    args$yaxis <- "y2"
  }

  do.call(plotly::add_trace, args)
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

  # Adducts of different proteoforms land on different peaks and stay separate
  # rows, so the mass-shift label has to name the proteoform - otherwise the
  # rows are indistinguishable
  multi_species <- length(unique(table$`Theor. Prot. [Da]`)) > 1

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
      mass_stoich_html = mass_shift_label(
        `Theor. Cmp [Da]`,
        `Bind. Stoich.`,
        species_mass = `Theor. Prot. [Da]`[1],
        multi_species = multi_species,
        sep = "&thinsp;"
      ),
      # Hidden in the table, but the proteoform has to reach the export: the
      # visible Mass Shift column is HTML and is dropped on the way out
      `Theor. Prot. [Da]` = `Theor. Prot. [Da]`[1],
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
      `Theor. Prot. [Da]` = `Theor. Prot. [Da]`,
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
      label_color = get_contrast_color(tint(colors)[match(
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
      col_var = as.character(
        !!rlang::sym(
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
        )
      )
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
  # Concentrations as the plots write them (8×10⁻⁵ in molar units); the row
  # colours key on col_var, not on this column
  conc_col <- if ("Concentration" %in% names(units)) units[["Concentration"]]
  if (!is.null(conc_col) && conc_col %in% names(table)) {
    table[[conc_col]] <- conc_display(table[[conc_col]])
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
      # A table wider than its card scrolls sideways with its header
      scrollX = TRUE,
      scrollY = TRUE,
      scrollCollapse = TRUE,
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        "  var api = this.api();",
        "  setTimeout(function() { api.columns.adjust(); }, 50);",
        "}"
      ),
      rowGroup = row_group,
      columnDefs = Filter(
        Negate(is.null),
        list(
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
              "Theor. Prot. [Da]",
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
    )
  ) |>
    # Rows in a light tint of the plots' colours, read in black
    DT::formatStyle(
      columns = "col_var",
      target = 'row',
      backgroundColor = DT::styleEqual(
        levels = names(colors),
        values = tint(colors)
      ),
      color = DT::styleEqual(
        levels = names(colors),
        values = get_contrast_color(tint(colors))
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

# Percentage columns of a hits table, drawn as bars in their cells: those of
# the Adduct View and the binding columns of the Sample View
#' @export
hits_bar_cols <- function(hits) {
  grep("\\[%\\]$", names(hits), value = TRUE)
}

# Columns of the hits table to pick from, and those picked by default. The
# sample, protein, compound, concentration and time are always shown, and so
# is Tot. Binding [%], which follows Binding [%] (see hits_view_table()).
#' @export
hits_col_choices <- function(hits, units) {
  fixed <- c(
    "Sample ID",
    "Protein",
    "Cmp Name",
    "truncSample_ID",
    "Tot. Binding [%]",
    if ("Concentration" %in% names(units)) units[["Concentration"]],
    if ("Time" %in% names(units)) units[["Time"]]
  )
  choices <- setdiff(names(hits), fixed)
  hidden <- c(
    "Well",
    "Replicate",
    "Unmatched [%]",
    # Shown when a peak had a reading that was not preferred
    if (!show_preferred_column(hits)) "Preferred",
    "Meas. Prot. [Da]",
    "Δ Prot. [Da]",
    "Int. Prot. [%]",
    "Int. Cmp [%]",
    "Δ Cmp [Da]"
  )

  list(choices = choices, selected = setdiff(choices, hidden))
}

# Columns of the two Table Views to pick from (those of the hits table), and
# those picked by default: Binding [%], and Preferred when the rows shown
# (`shown`, a subset of `hits`) include a reading that was not preferred
#' @export
table_view_col_choices <- function(hits, shown, units) {
  choices <- hits_col_choices(hits, units)$choices
  list(
    choices = choices,
    selected = intersect(
      c("Binding [%]", if (show_preferred_column(shown)) "Preferred"),
      choices
    )
  )
}

# Hits in the order of the hits table: by protein, then by concentration and
# time
#' @export
sort_hits_table <- function(hits, units) {
  num_sort_cols <- unname(units[intersect(
    c("Concentration", "Time"),
    names(units)
  )])

  dplyr::arrange(
    hits,
    Protein,
    dplyr::across(dplyr::all_of(num_sort_cols), as.numeric)
  )
}

# The hits table, one row per assignment of a peak, at the picked columns with
# Tot. Binding [%] right after Binding [%]. Every view of the hits (the Hits
# interface, the Table View of the Overview and Sample View) is a subset of it.
#' @export
hits_view_table <- function(
  hits,
  selected_cols,
  units,
  compounds = NULL,
  samples = NULL
) {
  binding_pos <- match("Binding [%]", selected_cols)
  selected_cols <- if (is.na(binding_pos)) {
    union(selected_cols, "Tot. Binding [%]")
  } else {
    append(selected_cols, "Tot. Binding [%]", after = binding_pos)
  }

  filter_hits_table(
    sort_hits_table(hits, units),
    selected_cols = selected_cols,
    compounds = compounds,
    samples = samples,
    units = units
  )
}

# Table View of the Relative Binding interface: a hits_view_table() subset in
# the colours of the plots, with all percentages as bars
#' @export
render_hits_view <- function(
  hits_table,
  colors,
  color_variable,
  truncate_names = FALSE,
  units,
  empty_message = NULL
) {
  if (isTRUE(truncate_names)) {
    hits_table$`Sample ID` <- hits_table$truncSample_ID
  }

  render_hits_table(
    hits_table,
    concentration_colors = NULL,
    bar_chart = hits_bar_cols(hits_table),
    colors = colors,
    color_variable = color_variable,
    truncated = FALSE,
    dom = "t",
    empty_message = empty_message,
    units = units
  )
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

# Columns of the hits table that describe a protein species (proteoform)
sample_view_species_cols <- c(
  "Theor. Prot. [Da]",
  "Meas. Prot. [Da]",
  "Δ Prot. [Da]",
  "Int. Prot. [%]"
)

# Column names of the Sample View without their proteoform, so "Int. Prot.
# (21,816.8 Da) [%]" reads as "Int. Prot. [%]": the column pickers, bars and
# number formats of the hits table name the columns this way.
#' @export
hits_base_col <- function(cols) {
  sub(" \\([0-9][0-9,.]* Da\\) \\[", " [", cols)
}

# Header tooltip of a hits table column: its full name, with the proteoform,
# compound and mass shift a Sample View column belongs to
#' @export
hits_col_tooltip <- function(cols) {
  vapply(
    cols,
    function(col) {
      if (col %in% names(hits_col_full_names)) {
        return(hits_col_full_names[[col]])
      }
      base <- hits_base_col(col)
      if (base != col && base %in% names(hits_col_full_names)) {
        species <- sub("^.* \\(([0-9][0-9,.]* Da)\\) \\[.*$", "\\1", col)
        return(paste0(hits_col_full_names[[base]], ", proteoform ", species))
      }
      binding <- regmatches(
        col,
        regexec("^Binding \\((.*?)(\\*?)\\)x([0-9]+) \\[%\\]$", col)
      )[[1]]
      if (length(binding)) {
        return(paste0(
          "Binding [%] of ",
          binding[2],
          " at stoichiometry ",
          binding[4],
          if (nzchar(binding[3])) {
            ". *: not the preferred reading of its peak, not counted in the totals"
          }
        ))
      }
      total <- regmatches(
        col,
        regexec("^Total Binding \\((.*)\\) \\[%\\]$", col)
      )[[1]]
      if (length(total)) {
        return(paste0(
          "Total binding of ",
          total[2],
          ": its preferred readings over all proteoforms and stoichiometries"
        ))
      }
      complex <- regmatches(
        col,
        regexec("^Tot\\. Binding \\((.*)\\) \\[%\\]$", col)
      )[[1]]
      if (length(complex)) {
        return(paste0(
          "Total binding of the complex with ",
          complex[2],
          ", all proteoforms and mass shifts pooled"
        ))
      }
      mass <- regmatches(
        col,
        regexec("^Mass ([0-9]+)( \\((.*)\\))?$", col)
      )[[1]]
      if (length(mass)) {
        return(paste0(
          "Declared mass shift ",
          mass[2],
          if (nzchar(mass[4])) paste0(" of ", mass[4]),
          " [Da]"
        ))
      }
      col
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# Sample View of the hits table: one row per sample. The values of a
# proteoform carry its mass in the column name ("Int. Prot. (21,816.8 Da)
# [%]") once a sample holds several proteoforms; Theor. Prot. [Da] is then
# left out, the name says it. A sample declaring several compounds lists them
# in Cmp Name, its binding columns carry the compound and each complex gets its
# own Tot. Binding (compound) [%]. Every complex gets a column per declared
# mass shift of its compound (Mass n of the Compounds table), proteoform and
# stoichiometry, e.g. "Binding (21,816.8 Da, Mass 1)x1 [%]"; a reading that is
# not the preferred one of its peak is marked with an asterisk, "Binding
# (Mass 2*)x1 [%]". Total Binding (Mass n) sums the preferred readings of the
# shift over all proteoforms and stoichiometries, so the totals of a sample add
# up to its Tot. Binding [%] as the rows of the Adduct View do.
#' @export
transform_per_adduct <- function(
  hits_table,
  proteins_table,
  compounds_table,
  samples_table
) {
  if (!nrow(hits_table)) {
    return(hits_table)
  }

  complex_cols <- c(
    "Peak Signal [Da]",
    "Int. Cmp [%]",
    "Theor. Cmp [Da]",
    "Δ Cmp [Da]",
    "Bind. Stoich.",
    "Preferred",
    "Binding [%]"
  )
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))

  sample <- as.character(hits_table$`Sample ID`)
  samples <- unique(sample)
  species <- if ("Theor. Prot. [Da]" %in% names(hits_table)) {
    num(hits_table$`Theor. Prot. [Da]`)
  } else {
    rep(NA_real_, nrow(hits_table))
  }
  cmp <- as.character(hits_table$`Cmp Name`)

  # Qualified only when one sample holds several, so a table of one
  # proteoform and compound per sample keeps its plain column names
  several <- function(x) {
    known <- !is.na(x)
    any(tapply(x[known], sample[known], function(v) length(unique(v))) > 1)
  }
  by_species <- several(species)
  by_cmp <- several(cmp)

  # Proteoforms by mass; more decimals only if one place cannot tell them
  # apart
  species_set <- sort(unique(species[!is.na(species)]))
  species_lab <- paste(fmt_species_mass(species_set), "Da")
  if (anyDuplicated(species_lab)) {
    species_lab <- paste(
      formatC(species_set, format = "f", digits = 3, big.mark = ","),
      "Da"
    )
  }
  with_species <- function(col, lab) {
    vapply(
      lab,
      function(l) sub("^(.*) (\\[[^]]*\\])$", paste0("\\1 (", l, ") \\2"), col),
      character(1),
      USE.NAMES = FALSE
    )
  }

  # Compounds in the order of the Compounds table
  cmp_rank <- function(x) {
    rank <- match(x, compounds_table$Compound)
    rank[is.na(rank)] <- length(compounds_table$Compound) +
      match(x[is.na(rank)], sort(unique(x)))
    rank
  }

  # Values of the sample, from its first row
  sample_cols <- setdiff(
    names(hits_table),
    c(complex_cols, if (by_species) sample_view_species_cols)
  )
  out <- hits_table[!duplicated(sample), sample_cols, drop = FALSE]
  cmp_lists <- split(cmp, factor(sample, levels = samples))
  out$`Cmp Name` <- vapply(
    cmp_lists,
    function(x) {
      x <- unique(x[!is.na(x)])
      if (!length(x)) {
        NA_character_
      } else {
        paste(x[order(cmp_rank(x))], collapse = ", ")
      }
    },
    character(1),
    USE.NAMES = FALSE
  )

  # Values of each proteoform, expanded in place of the plain column
  col_order <- names(hits_table)[!names(hits_table) %in% complex_cols]
  if (by_species) {
    first_of_species <- !duplicated(paste(sample, species)) & !is.na(species)
    species_cols <- intersect(
      setdiff(sample_view_species_cols, "Theor. Prot. [Da]"),
      names(hits_table)
    )
    col_order <- unlist(lapply(col_order, function(col) {
      if (col == "Theor. Prot. [Da]") {
        return(character(0))
      }
      if (!col %in% species_cols) {
        return(col)
      }
      with_species(col, species_lab)
    }))
    for (k in seq_along(species_set)) {
      rows <- which(first_of_species & species == species_set[k])
      at <- match(samples, sample[rows])
      for (col in species_cols) {
        out[[with_species(col, species_lab[k])]] <- hits_table[[col]][
          rows[at]
        ]
      }
    }
  }

  # A sample declaring several compounds gets the binding of each complex -
  # the protein, all its proteoforms pooled, with one compound - next to its
  # Tot. Binding [%], which pools the compounds too. The kinetics of a complex
  # are fitted to this value (complex_hits()).
  is_cx <- is_complex_row(hits_table)
  is_preferred <- if ("Preferred" %in% names(hits_table)) {
    !as.character(hits_table$Preferred) %in% "FALSE"
  } else {
    rep(TRUE, nrow(hits_table))
  }
  if (by_cmp && "Binding [%]" %in% names(hits_table)) {
    named <- which(!is.na(cmp))
    # Rows without a complex carry 0 % (NA for an unmeasured sample)
    share <- ifelse(
      is_cx[named] & !is_preferred[named],
      0,
      num(hits_table$`Binding [%]`[named])
    )
    complexes <- unique(cmp[named])
    complexes <- complexes[order(cmp_rank(complexes))]
    complex_tot_cols <- paste0("Tot. Binding (", complexes, ") [%]")
    summed <- tapply(share, paste(sample[named], cmp[named], sep = "\r"), sum)
    for (k in seq_along(complexes)) {
      out[[complex_tot_cols[k]]] <- as.vector(
        summed[paste(samples, complexes[k], sep = "\r")]
      )
    }
    at <- match("Tot. Binding [%]", col_order)
    col_order <- append(
      col_order,
      complex_tot_cols,
      after = if (is.na(at)) length(col_order) else at
    )
  }

  # Readings of the complexes, one column each
  cx <- which(is_cx)
  binding_cols <- character(0)
  if (length(cx)) {
    shift <- num(hits_table$`Theor. Cmp [Da]`[cx])
    # Position of the shift among the declared shifts of its compound. The
    # Compounds table refuses shifts within 0.01 Da of each other, so the
    # closest one within that is the one assigned.
    # Without its declaration (results loaded on their own) the shifts of a
    # compound are numbered by size
    declared_shifts <- lapply(
      stats::setNames(nm = unique(cmp[cx])),
      function(compound) {
        row <- match(compound, compounds_table$Compound)
        declared <- if (is.na(row)) {
          numeric(0)
        } else {
          num(unlist(compounds_table[row, -1, drop = FALSE]))
        }
        if (!any(!is.na(declared))) {
          declared <- sort(unique(round(shift[cmp[cx] %in% compound], 4)))
        }
        declared
      }
    )
    mass_no <- mapply(
      function(compound, m) {
        declared <- declared_shifts[[compound]]
        hit <- which(abs(declared - m) < 0.01)
        if (length(hit)) hit[which.min(abs(declared[hit] - m))] else NA_integer_
      },
      cmp[cx],
      shift,
      USE.NAMES = FALSE
    )
    keep <- !is.na(mass_no)
    cx <- cx[keep]
    shift <- shift[keep]
    mass_no <- mass_no[keep]
  }

  if (length(cx)) {
    stoich <- as.character(hits_table$`Bind. Stoich.`[cx])
    preferred <- is_preferred[cx]
    binding <- num(hits_table$`Binding [%]`[cx])
    sp_k <- match(species[cx], species_set)

    # The totals pool the proteoforms, as Tot. Binding [%] and the kinetics
    # do: they are forms of one protein
    total_qualifier <- paste0(
      if (by_cmp) paste0(cmp[cx], ", ") else "",
      "Mass ",
      mass_no
    )
    qualifier <- paste0(
      if (by_species) paste0(species_lab[sp_k], ", ") else "",
      total_qualifier
    )
    mass_col <- if (by_cmp) {
      paste0("Mass ", mass_no, " (", cmp[cx], ")")
    } else {
      paste("Mass", mass_no)
    }

    # Column order by what the names tell apart. Without the compound (or the
    # proteoform) in the name, the samples of different compounds share a
    # column, which must not take the rank of whichever compound came first.
    rank_cmp <- if (by_cmp) cmp_rank(cmp[cx]) else 0
    sp_k <- if (by_species) sp_k else rep(0, length(cx))

    readings <- rbind(
      data.frame(
        sample = sample[cx],
        col = mass_col,
        value = shift,
        stat = "first",
        rank_cmp = rank_cmp,
        rank_mass = mass_no,
        rank_species = 0,
        rank_kind = 0,
        rank_stoich = 0,
        rank_star = 0
      ),
      data.frame(
        sample = sample[cx],
        col = paste0(
          "Binding (",
          qualifier,
          ifelse(preferred, "", "*"),
          ")x",
          stoich,
          " [%]"
        ),
        value = binding,
        stat = "sum",
        rank_cmp = rank_cmp,
        rank_mass = mass_no,
        rank_species = sp_k,
        rank_kind = 1,
        rank_stoich = num(stoich),
        rank_star = as.numeric(!preferred)
      ),
      data.frame(
        sample = sample[cx],
        col = paste0("Total Binding (", total_qualifier, ") [%]"),
        value = ifelse(preferred, binding, 0),
        stat = "sum",
        rank_cmp = rank_cmp,
        rank_mass = mass_no,
        rank_species = Inf,
        rank_kind = 2,
        rank_stoich = 0,
        rank_star = 0
      )
    )

    first <- !duplicated(readings$col)
    cols <- readings[first, ]
    cols <- cols[
      order(
        cols$rank_cmp,
        cols$rank_mass,
        cols$rank_kind > 0,
        cols$rank_species,
        cols$rank_kind,
        cols$rank_stoich,
        cols$rank_star
      ),
    ]
    binding_cols <- cols$col

    key <- paste(readings$sample, readings$col, sep = "\r")
    summed <- tapply(readings$value, key, sum)
    first_value <- readings$value[!duplicated(key)]
    names(first_value) <- key[!duplicated(key)]
    for (i in seq_len(nrow(cols))) {
      at <- paste(samples, cols$col[i], sep = "\r")
      out[[cols$col[i]]] <- as.vector(
        if (cols$stat[i] == "sum") {
          summed[at]
        } else {
          first_value[at]
        }
      )
    }
  }

  out[, c(col_order, binding_cols), drop = FALSE]
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
  dom = NULL,
  empty_message = NULL,
  units
) {
  # Bars and number formats are picked by column; a proteoform's column of
  # the Sample View takes those of its plain column (see hits_base_col())
  base_cols <- hits_base_col(names(hits_table))
  bar_chart <- names(hits_table)[
    base_cols %in% bar_chart | names(hits_table) %in% bar_chart
  ]

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
  dom_value <- dom %||% dom_value

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
    names(hits_table)[
      base_cols %in%
        c(
          "Binding [%]",
          "Tot. Binding [%]",
          "Unmatched [%]",
          "Correct [%]",
          "Int. Prot. [%]",
          "Int. Cmp [%]"
        )
    ],
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
        tooltip_names <- hits_col_tooltip(visible_cols)
        # Breaks only after the first two words, never inside "Mass 1", a
        # proteoform's "21,816.8 Da" or at the hyphen of a compound name
        paste0(
          "function(thead, data, start, end, display) {",
          "  var names = ",
          jsonlite::toJSON(tooltip_names),
          ";",
          "  $(thead).find('th').each(function(i) {",
          "    if (i < names.length) $(this).attr('title', names[i]);",
          "    var h = $(this).html().replace(/Mass (\\d)/g, 'Mass\\u00a0$1')",
          "      .replace(/(\\d) Da\\b/g, '$1\\u00a0Da').replace(/-/g, '\\u2011');",
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

  if (!is.null(empty_message)) {
    hits_datatable$x$options$language <- list(emptyTable = empty_message)
  }

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

  # An empty colour map (e.g. no compound selected) colours nothing: given to
  # DT::styleEqual() it renders a callback that is not valid JavaScript, and
  # the browser then drops the rest of the update it came with
  if (!length(concentration_colors)) {
    concentration_colors <- NULL
  }
  if (!length(colors)) {
    colors <- NULL
  }

  # Rows in a light tint of the plots' colours, read in black
  if (!is.null(concentration_colors)) {
    concentration_colors <- tint(concentration_colors)
  }
  if (!is.null(colors)) {
    colors <- tint(colors)
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

# Replicate series of each sample: the repeat of the whole experiment it
# belongs to (R1 = first repeat, R2 = second, ...). Resolved per sample, so a
# config that only covers some samples doesn't blind the fallback for the
# rest: (1) the config's Replicate value, as given; (2) the _R<n> suffix of
# the file name, as "R<n>"; (3) otherwise empty - nothing says which repeat
# the sample is, and a made-up label would suggest a series that isn't there.
#' @export
compute_replicate_labels <- function(sample_names, config = NULL) {
  labels <- replicate_suffix(sample_names)
  labels[is.na(labels)] <- ""

  if (!is.null(config) && "Replicate" %in% names(config)) {
    cfg_key <- ms_sample_base(config$Sample)
    samp_key <- ms_sample_base(sample_names)
    matched <- trimws(as.character(config$Replicate[match(samp_key, cfg_key)]))
    from_config <- !is.na(matched) & matched != ""
    labels[from_config] <- matched[from_config]
  }

  labels
}

# "R<n>" from an _R<n> suffix of a sample name (extension ignored), NA without
# one. Leading zeros are dropped, so "_R01" and "_R1" name the same series.
replicate_suffix <- function(sample_names) {
  stem <- ms_sample_base(sample_names)
  has_suffix <- grepl("_[Rr][0-9]+$", stem)
  out <- rep(NA_character_, length(stem))
  out[has_suffix] <- paste0(
    "R",
    as.integer(sub("^.*_[Rr]([0-9]+)$", "\\1", stem[has_suffix]))
  )
  out
}

# Series labels in natural order: R2 before R10, labels without a number
# after those with one
#' @export
sort_series <- function(series) {
  series <- unique(as.character(series))
  number <- suppressWarnings(as.numeric(sub("^.*?([0-9]+)$", "\\1", series)))
  number[!grepl("[0-9]$", series)] <- NA
  series[order(sub("[0-9]+$", "", series), is.na(number), number, series)]
}

# Empty sample declaration table generator function
#' @export
new_sample_table <- function(
  result,
  protein_table,
  compound_table,
  kinact_ki = FALSE
) {
  # Sample names are stored without an extension, and appending ".raw" here
  # would label a Thermo, mzML or mzXML sample as a Waters folder. Every lookup
  # that matches these against a config runs both sides through ms_sample_base(),
  # so leaving them bare matches exactly as before.
  sample_names <- sort(result$samples %||% names(result$deconvolution))
  sample_tab <- data.frame(
    Include = rep(TRUE, length(sample_names)),
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
    "Include",
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
      "_fileinput)"
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
      "_fileinput)"
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

# One-line hint above a declaration table. A long explanation (`details`, one
# line each, then `note`) goes into the tooltip of an info icon, so the hint
# itself stays within the table width.
table_hint <- function(class, msg, details = NULL, note = NULL) {
  has_details <- length(details) || length(note)
  hint <- shiny::div(
    class = paste(class, if (has_details) "table-hint-has-details"),
    tabindex = if (has_details) "0",
    shiny::icon("triangle-exclamation"),
    shiny::span(class = "table-hint-text", as.character(msg)),
    # Cue that the hint has details; the whole hint shows them on hover
    if (has_details) {
      shiny::span(class = "table-hint-details", shiny::icon("circle-info"))
    }
  )
  if (!has_details) {
    return(hint)
  }
  bslib::tooltip(
    hint,
    shiny::tagList(
      lapply(details, shiny::div),
      if (length(note)) shiny::div(class = "table-hint-note", note)
    ),
    placement = "bottom",
    options = list(customClass = "table-hint-tooltip")
  )
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
  tolerance = 3,
  protein_table = NULL,
  compound_table = NULL,
  max_multiples = NULL
) {
  # Loading state on the info panel itself, shown for at least 0.25 seconds;
  # on.exit ensures it is always cleared. (A waiter overlay here switches the
  # panel to position: static and briefly renders it with scrollbars.)
  shinyjs::addClass(paste0(tab, "_table_info"), "table-info-loading")
  on.exit(
    shinyjs::removeClass(paste0(tab, "_table_info"), "table-info-loading"),
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
        compounds = compounds,
        protein_table = protein_table,
        compound_table = compound_table,
        tolerance = tolerance,
        max_multiples = max_multiples
      )
    } else {
      check_function <- "check_table"
      args <- list(tab = table, tolerance = tolerance, type = tab)
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
      # A passed check can still carry a warning (e.g. ambiguous masses)
      local({
        check <- table_check
        output[[paste0(tab, "_table_hint")]] <- shiny::renderUI(
          if (!is.null(attr(check, "warning"))) {
            table_hint(
              "table-hint table-hint-orange",
              attr(check, "warning"),
              details = attr(check, "details"),
              note = attr(check, "note")
            )
          }
        )
      })

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
          table_hint(
            hint_class,
            msg,
            details = attr(msg, "details"),
            note = attr(msg, "note")
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
      # Convert binding cols to exact percentage — rounding only happens in DT
      # display. A sample without any protein or complex peak stays NA: not
      # measured.
      dplyr::across(
        c(`% Binding`, `Total % Binding`),
        ~ .x * 100
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

# Colour key of a conversion result.
#
# The levels of every variable the result views are coloured by, taken once
# from the whole result, so a level keeps its colour however the views filter
# it. Colours are looked up with key_colors(), marker symbols of the
# concentrations with key_symbols(); see app/logic/palette.R for the palettes.
#
# - Proteins and compounds share one sequence (proteins first, then the
#   compounds, each sorted), so a protein and a compound never share a colour
#   where they are shown side by side (batch heatmaps, spectra).
# - Samples follow `sample_order`, the order the result table lists them in
#   (protein, compound, then concentration and time or binding), so a sample
#   gradient runs along the table and the plots ordered like it. Without it,
#   their natural name order ("S2" before "S10").
# - Concentrations and times are numeric and ordered.
# - sample_info holds each sample's protein, compound, concentration and
#   time, so a sample can be coloured by any of them (sample_level_colors()).
#
# Works on the result table as stored (Sample, Compound, Concentration [..])
# and on its display form (Sample ID, Cmp Name, Conc. [..]) alike, which hold
# the same values.
#' @export
color_key <- function(hits_summary, sample_order = NULL) {
  nms <- names(hits_summary)
  pick <- function(cands) intersect(cands, nms)[1]
  col_values <- function(col) {
    if (is.na(col)) {
      return(character(0))
    }
    x <- as.character(hits_summary[[col]])
    unique(x[!is.na(x) & nzchar(x) & x != "N/A"])
  }
  num_values <- function(pattern) {
    col <- nms[grepl(pattern, nms)][1]
    if (is.na(col)) {
      return(numeric(0))
    }
    x <- suppressWarnings(as.numeric(as.character(hits_summary[[col]])))
    sort(unique(x[!is.na(x)]))
  }

  proteins <- sort(col_values(pick("Protein")), method = "radix")
  compounds <- sort(
    col_values(pick(c("Cmp Name", "Compound"))),
    method = "radix"
  )
  sample_col <- pick(c("Sample ID", "Sample"))
  samples <- natural_sort(col_values(sample_col))
  if (length(sample_order)) {
    sample_order <- unique(as.character(sample_order))
    samples <- c(
      sample_order[sample_order %in% samples],
      samples[!samples %in% sample_order]
    )
  }

  # Per sample: its protein, its (first) compound, concentration and time
  value_col <- function(col) {
    if (is.na(col)) {
      return(rep(NA_character_, length(samples)))
    }
    x <- as.character(hits_summary[[col]])
    x[!is.na(x) & x == "N/A"] <- NA
    ids <- as.character(hits_summary[[sample_col]])
    first <- tapply(x, ids, function(v) v[!is.na(v)][1])
    unname(first[samples])
  }
  sample_info <- data.frame(
    sample = samples,
    protein = value_col(pick("Protein")),
    compound = value_col(pick(c("Cmp Name", "Compound"))),
    concentration = suppressWarnings(as.numeric(
      value_col(nms[grepl("^Conc", nms)][1])
    )),
    time = suppressWarnings(as.numeric(value_col(nms[grepl("^Time", nms)][1]))),
    stringsAsFactors = FALSE
  )

  # Short sample IDs, when the table carries them
  sample_labels <- if (!is.na(sample_col) && "truncSample_ID" %in% nms) {
    keep <- !duplicated(hits_summary[[sample_col]]) &
      !is.na(hits_summary[[sample_col]])
    stats::setNames(
      as.character(hits_summary$truncSample_ID[keep]),
      as.character(hits_summary[[sample_col]][keep])
    )
  } else {
    stats::setNames(character(0), character(0))
  }

  list(
    entities = unique(c(proteins, compounds)),
    proteins = proteins,
    compounds = compounds,
    samples = samples,
    sample_labels = sample_labels,
    sample_info = sample_info,
    concentrations = num_values("^Conc"),
    times = num_values("^Time")
  )
}

# Sorts strings with their digit runs compared as numbers
natural_sort <- function(x) {
  if (length(x) < 2) {
    return(x)
  }
  padded <- vapply(
    x,
    function(s) {
      parts <- regmatches(s, gregexpr("[0-9]+|[^0-9]+", s))[[1]]
      num <- grepl("^[0-9]+$", parts)
      parts[num] <- formatC(
        parts[num],
        width = 20,
        flag = "0",
        format = "s"
      )
      parts[num] <- gsub(" ", "0", parts[num], fixed = TRUE)
      paste(parts, collapse = "")
    },
    character(1),
    USE.NAMES = FALSE
  )
  x[order(padded, x, method = "radix")]
}

# Which key variable a view's colour variable or grouping column refers to
#' @export
key_variable <- function(variable) {
  v <- tolower(as.character(variable)[1])
  if (is.na(v)) {
    return(NA_character_)
  }
  if (v %in% c("compounds", "compound", "cmp name")) {
    "compound"
  } else if (v %in% c("proteins", "protein")) {
    "protein"
  } else if (v %in% c("samples", "sample", "sample id", "truncsample_id")) {
    "sample"
  } else if (grepl("^conc", v)) {
    "concentration"
  } else if (grepl("^time", v)) {
    "time"
  } else {
    NA_character_
  }
}

# Position of each level in a key sequence; levels the key does not know are
# appended after it, in order, so they still get colours of their own
key_index <- function(levels, known) {
  idx <- match(levels, known)
  unknown <- unique(levels[is.na(idx) & !is.na(levels)])
  idx[is.na(idx) & !is.na(levels)] <- length(known) +
    match(levels[is.na(idx) & !is.na(levels)], unknown)
  idx
}

# Rank positions (0 to 1) of numeric levels among the key's values
key_ramp_position <- function(values, known) {
  scale <- sort(unique(c(known, values[!is.na(values)])))
  if (length(scale) < 2) {
    return(ifelse(is.na(values), NA_real_, 0.5))
  }
  (match(values, scale) - 1) / (length(scale) - 1)
}

# Colours of the levels of one variable, named by the levels as given
# (as.character). levels defaults to all levels of the key. For samples,
# trunc = TRUE takes and returns the short sample IDs.
#
# - compound / protein: categorical while the result has at most twelve of
#   them together, otherwise spread over the hue gradient in key order.
# - sample: the same, over the samples of the result in key order.
# - concentration: the 0 control in the control grey, the others over the
#   concentration ramp, low to high.
# - time: over the time ramp, early to late.
# Missing levels, and variables the key does not cover, are muted.
#' @export
key_colors <- function(
  key,
  variable,
  levels = NULL,
  theme = "light",
  trunc = FALSE
) {
  var <- key_variable(variable)
  status <- status_colors(theme)

  if (is.null(levels)) {
    levels <- switch(
      if (is.na(var)) "none" else var,
      compound = key$compounds,
      protein = key$proteins,
      sample = if (isTRUE(trunc) && length(key$sample_labels)) {
        unname(key$sample_labels[key$samples])
      } else {
        key$samples
      },
      concentration = key$concentrations,
      time = key$times,
      character(0)
    )
  }
  lvl_names <- as.character(levels)
  out <- rep(status$muted, length(levels))

  if (is.na(var) || !length(levels)) {
    return(stats::setNames(out, lvl_names))
  }

  if (var %in% c("compound", "protein")) {
    lv <- as.character(levels)
    lv[lv %in% "N/A"] <- NA
    idx <- key_index(lv, key$entities)
    ok <- !is.na(idx)
    n_all <- max(c(length(key$entities), idx[ok]))
    out[ok] <- sample_colors(n_all, theme)[idx[ok]]
  } else if (var == "sample") {
    lv <- as.character(levels)
    if (isTRUE(trunc) && length(key$sample_labels)) {
      full <- names(key$sample_labels)[match(lv, key$sample_labels)]
      lv <- ifelse(is.na(full), lv, full)
    }
    idx <- key_index(lv, key$samples)
    ok <- !is.na(idx)
    n_all <- max(c(length(key$samples), idx[ok]))
    out[ok] <- sample_colors(n_all, theme)[idx[ok]]
  } else if (var == "concentration") {
    v <- suppressWarnings(as.numeric(as.character(levels)))
    known <- key$concentrations[key$concentrations != 0]
    control <- !is.na(v) & v == 0
    dosed <- !is.na(v) & v != 0
    out[control] <- status$control
    out[dosed] <- ramp_at(
      key_ramp_position(v[dosed], known),
      "concentration",
      theme
    )
  } else if (var == "time") {
    v <- suppressWarnings(as.numeric(as.character(levels)))
    ok <- !is.na(v)
    out[ok] <- ramp_at(key_ramp_position(v[ok], key$times), "time", theme)
  }

  stats::setNames(out, lvl_names)
}

# Colours of samples by a variable of theirs, named by the samples as given:
# their own colour ("Samples"), or the colour of their protein, compound,
# concentration or time. With trunc = TRUE the samples are short sample IDs.
# This is how a view colours its per-sample marks (bars, spectra, table rows)
# by a property of the sample.
#' @export
sample_level_colors <- function(
  key,
  variable,
  samples,
  theme = "light",
  trunc = FALSE
) {
  var <- key_variable(variable)
  samples <- as.character(samples)
  if (is.na(var) || var == "sample" || is.null(key$sample_info)) {
    return(key_colors(key, "Samples", samples, theme = theme, trunc = trunc))
  }
  ids <- samples
  if (isTRUE(trunc) && length(key$sample_labels)) {
    full <- names(key$sample_labels)[match(ids, key$sample_labels)]
    ids <- ifelse(is.na(full), ids, full)
  }
  values <- key$sample_info[[var]][match(ids, key$sample_info$sample)]
  stats::setNames(
    unname(key_colors(key, variable, values, theme = theme)),
    samples
  )
}

# Colours of the Sample View, one sample: its compounds' colours
# ("Compounds"), or the sample's colour by the variable picked - its own, its
# concentration's, time's or protein's - named by its (short) sample ID
#' @export
sample_view_colors <- function(
  hits_summary,
  key,
  variable,
  sample,
  trunc = FALSE,
  theme = "light"
) {
  rows <- hits_summary[hits_summary$`Sample ID` %in% sample, , drop = FALSE]
  if (identical(variable, "Compounds")) {
    cmps <- as.character(rows$`Cmp Name`)
    cmps <- unique(cmps[!is.na(cmps) & cmps != "N/A"])
    return(key_colors(key, "Compounds", cmps, theme = theme))
  }
  label <- if (isTRUE(trunc) && "truncSample_ID" %in% names(rows)) {
    unique(as.character(rows$truncSample_ID))
  } else {
    as.character(sample)
  }
  sample_level_colors(key, variable, label, theme = theme, trunc = trunc)
}

# The colour variable a view opens with: the experiment's main factor among
# the `roles` it offers ("concentration", "compound", "time", "protein",
# "sample"): the concentration when several were dosed, else the compound
# when there are several, else time, protein - whatever varies - and the
# sample when nothing else does. Returns the role.
#' @export
default_color_role <- function(key, roles) {
  varies <- c(
    concentration = sum(key$concentrations != 0) > 1,
    compound = length(key$compounds) > 1,
    time = sum(key$times != 0) > 1,
    protein = length(key$proteins) > 1
  )
  for (role in names(varies)) {
    if (role %in% roles && varies[[role]]) {
      return(role)
    }
  }
  if ("sample" %in% roles) "sample" else roles[1]
}

# Levels sorted the way the key orders them: numerically for concentrations
# and times, in key order otherwise, with levels the key does not know last
#' @export
key_order <- function(key, variable, levels) {
  var <- key_variable(variable)
  lv <- unique(levels)
  if (var %in% c("concentration", "time")) {
    v <- suppressWarnings(as.numeric(as.character(lv)))
    return(lv[order(is.na(v), v)])
  }
  known <- switch(
    if (is.na(var)) "none" else var,
    compound = key$entities,
    protein = key$entities,
    sample = key$samples,
    character(0)
  )
  lv[order(is.na(lv), key_index(as.character(lv), known))]
}

# Round of twelve of each level (0 within the first twelve),
# for the plots that switch symbol, pattern or dash past the palette.
# Ordered variables and ramp-coloured samples are always round 0.
#' @export
key_tiers <- function(key, variable, levels) {
  var <- key_variable(variable)
  lv <- as.character(levels)
  tiers <- rep(0L, length(lv))
  if (identical(var, "compound") || identical(var, "protein")) {
    idx <- key_index(lv, key$entities)
    tiers[!is.na(idx)] <- level_tier(max(c(idx, 0L), na.rm = TRUE))[
      idx[!is.na(idx)]
    ]
  }
  stats::setNames(tiers, lv)
}

# Marker symbol of each level, named by the levels as given: a circle in the
# first round of colours, another shape in each later one (see level_tier)
#' @export
key_marker_symbols <- function(key, variable, levels) {
  stats::setNames(
    tier_symbol(unname(key_tiers(key, variable, levels))),
    as.character(levels)
  )
}

# Marker symbol of each concentration, named by the levels as given: the
# highest concentration of the result first (circle), the 0 control apart.
# Fixed from all concentrations of the result, so a concentration has the
# same shape in every plot and for every complex.
#' @export
key_symbols <- function(key, levels = NULL) {
  if (is.null(levels)) {
    levels <- key$concentrations
  }
  v <- suppressWarnings(as.numeric(as.character(levels)))
  known <- sort(
    unique(c(key$concentrations, v[!is.na(v)])),
    decreasing = TRUE
  )
  known <- known[known != 0]
  rank <- match(v, known)
  out <- ifelse(
    is.na(rank),
    control_symbol,
    concentration_symbols[(rank - 1L) %% length(concentration_symbols) + 1L]
  )
  stats::setNames(out, as.character(levels))
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

# Make compound distribution plot for proteins tab
#' @export
prot_compound_distribution <- function(
  hits_summary,
  protein,
  color_variable,
  truncate_names,
  key = NULL,
  distribution_scale,
  distribution_labels = NULL,
  theme = "light",
  colors = NULL,
  sample_order = NULL
) {
  tbl <- hits_summary[
    hits_summary$Protein %in% protein & is_complex_row(hits_summary),
  ]

  # Adducts of different proteoforms are separate bar segments and need labels
  # that tell them apart
  multi_species <- length(unique(tbl$`Theor. Prot. [Da]`)) > 1

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
      mass_stoich_raw = mass_shift_label(
        `Theor. Cmp [Da]`,
        `Bind. Stoich.`,
        species_mass = `Theor. Prot. [Da]`[1],
        multi_species = multi_species
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

  # A given sample order (the Overview's) replaces the ranking by binding
  if (length(sample_order)) {
    given <- if (truncate_names) {
      tbl$truncSample_ID[match(sample_order, tbl$`Sample ID`)]
    } else {
      sample_order
    }
    given <- unique(given[!is.na(given) & given %in% global_sample_order])
    global_sample_order <- c(given, setdiff(global_sample_order, given))
  }

  # Colours handed in are shared with the other cards of a view
  if (is.null(colors)) {
    colors <- key_colors(
      key %||% color_key(hits_summary),
      color_variable,
      if (color_variable == "Compounds") {
        sort(unique(as.character(tbl$`Cmp Name`)), method = "radix")
      } else {
        global_sample_order
      },
      theme = theme,
      trunc = truncate_names
    )
  }

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

    bar_chart <- bar_chart |>
      plotly::layout(xaxis = list(title = list(text = NULL)))
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
  key = NULL,
  distribution_scale,
  distribution_labels = NULL,
  theme = "light",
  colors = NULL,
  sample_order = NULL
) {
  # A sample without any protein or complex peak has no binding to show
  tbl <- hits_summary |>
    dplyr::filter(`Cmp Name` == compound & !is.na(`Tot. Binding [%]`))

  # A sample the compound was declared for but not found in stays in the plot
  # with one empty bar at 0 %. Next to adducts, its rows without one (unbound
  # proteoforms) would only add empty segments.
  complex <- is_complex_row(tbl)
  bound <- tbl$`Sample ID`[complex]
  tbl <- tbl[
    complex | (!tbl$`Sample ID` %in% bound & !duplicated(tbl$`Sample ID`)),
  ]

  # Adducts of different proteoforms are separate bar segments and need labels
  # that tell them apart
  multi_species <- length(unique(
    tbl$`Theor. Prot. [Da]`[is_complex_row(tbl)]
  )) >
    1

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
      # The empty bar of a sample without an adduct carries no label
      mass_stoich_raw = if (all(`Theor. Cmp [Da]` %in% c(NA, "N/A"))) {
        ""
      } else {
        mass_shift_label(
          `Theor. Cmp [Da]`,
          `Bind. Stoich.`,
          species_mass = `Theor. Prot. [Da]`[1],
          multi_species = multi_species
        )
      },
      `Theor. Cmp [Da]` = `Theor. Cmp [Da]`[Preferred == "TRUE"][1],
      `Bind. Stoich.` = `Bind. Stoich.`[Preferred == "TRUE"][1],
      `Binding [%]` = {
        pref <- `Binding [%]`[Preferred == "TRUE"]
        if (length(pref) > 0) pref[1] else `Binding [%]`[1]
      }
    ) |>
    dplyr::select(-`Peak Signal [Da]`) |>
    dplyr::arrange(`Tot. Binding [%]`, `Binding [%]`, `Sample ID`)

  # A given sample order (the Overview's) replaces the ranking by binding
  if (length(sample_order)) {
    tbl <- tbl[order(match(tbl$`Sample ID`, sample_order)), , drop = FALSE]
  }
  tbl <- dplyr::mutate(
    tbl,
    `Sample ID` = if (truncate_names) `truncSample_ID` else `Sample ID`
  )

  tbl$`Sample ID` <- factor(
    tbl$`Sample ID`,
    levels = unique(tbl$`Sample ID`)
  )

  # Colours handed in are shared with the other cards of a view
  if (is.null(colors)) {
    colors <- key_colors(
      key %||% color_key(hits_summary),
      color_variable,
      if (color_variable == "Compounds") {
        unique(as.character(tbl$`Cmp Name`))
      } else {
        levels(tbl$`Sample ID`)
      },
      theme = theme,
      trunc = truncate_names
    )
  }

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
    max(tbl$`Tot. Binding [%]`, 0, na.rm = TRUE) + 10
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

# Compound distribution of the Overview selection: one protein with the picked
# compounds. A single compound gets the per-sample bars of its samples, the
# ones it was declared for but not found in at 0 %; several are grouped by
# compound. NULL when the selection has no binding event to show.
#' @export
overview_compound_distribution <- function(
  hits_summary,
  protein,
  compounds,
  color_variable,
  truncate_names,
  key = NULL,
  distribution_scale,
  distribution_labels = NULL,
  theme = "light",
  colors = NULL,
  sample_order = NULL
) {
  tbl <- overview_subset(hits_summary, protein, compounds)
  if (!any(is_complex_row(tbl))) {
    return(NULL)
  }

  if (is.null(colors)) {
    colors <- overview_colors(
      hits_summary,
      protein,
      compounds,
      variable = color_variable,
      key = key,
      trunc = truncate_names,
      theme = theme
    )
  }

  # The plots colour compounds or samples; a sample's colour may stand for
  # its concentration, time or protein (see overview_colors())
  mode <- if (identical(color_variable, "Compounds")) "Compounds" else "Samples"

  # Compounds without a hit add nothing to the plot, so one with hits among
  # several picked ones is shown on its own
  hit_cmps <- unique(tbl$`Cmp Name`[is_complex_row(tbl)])

  if (length(hit_cmps) == 1) {
    cmp_compound_distribution(
      hits_summary = tbl,
      compound = hit_cmps,
      color_variable = mode,
      truncate_names = truncate_names,
      key = key,
      distribution_scale = distribution_scale,
      distribution_labels = distribution_labels,
      theme = theme,
      colors = colors,
      sample_order = sample_order
    )
  } else {
    prot_compound_distribution(
      hits_summary = tbl,
      protein = protein,
      color_variable = mode,
      truncate_names = truncate_names,
      key = key,
      distribution_scale = distribution_scale,
      distribution_labels = distribution_labels,
      theme = theme,
      colors = colors,
      sample_order = sample_order
    )
  }
}

# Text in place of the Overview distribution plot when it has nothing to show
#' @export
overview_distribution_note <- function(hits_summary, protein, compounds) {
  if (!length(compounds)) {
    return("No compound selected")
  }
  tbl <- overview_subset(hits_summary, protein, compounds)
  # Without any protein or complex peak there was nothing to measure, which is
  # not the same as no binding
  if (nrow(tbl) && all(is.na(tbl$`Tot. Binding [%]`))) {
    "No protein or complex peak found"
  } else {
    "No binding events"
  }
}

# Make compound distribution pie chart for samples tab
#' @export
smpl_compound_distribution <- function(
  hits_summary,
  sample,
  color_variable,
  truncate_names,
  key = NULL,
  theme = "light",
  colors = NULL
) {
  tbl <- hits_summary |>
    dplyr::filter(`Sample ID` == sample)

  # Binding events only - a protein species without any complex sits in the
  # table as a row without an adduct and is accounted for in the unbound slices
  adducts <- tbl[is_complex_row(tbl), ]

  if (nrow(adducts) == 0) {
    return(NULL)
  }

  # A protein may be declared with several masses (proteoforms). Each of them
  # binds on its own, so every (proteoform, peak) pair is a separate slice and
  # its label has to name the proteoform - plotly merges pie sectors that share
  # a label, which would silently fold the species into one another.
  species <- unbound_species(tbl)
  multi_species <- length(unique(tbl$`Theor. Prot. [Da]`)) > 1

  # Group by protein species + compound + peak: multiple stoichiometry
  # interpretations of the same peak are merged into one slice with a combined
  # [x]xN + [y]xM label. Only the Preferred hit's binding value counts.
  cmp_table <- adducts |>
    dplyr::arrange(
      `Cmp Name`,
      `Theor. Prot. [Da]`,
      `Peak Signal [Da]`,
      dplyr::desc(Preferred == "TRUE"),
      dplyr::desc(suppressWarnings(as.numeric(`Theor. Cmp [Da]`)))
    ) |>
    dplyr::group_by(`Cmp Name`, `Theor. Prot. [Da]`, `Peak Signal [Da]`) |>
    dplyr::reframe(
      `Cmp Name` = `Cmp Name`[1],
      `Sample ID` = if (truncate_names) `truncSample_ID`[1] else `Sample ID`[1],
      total_bind = `Tot. Binding [%]`[1],
      mass_stoich = mass_shift_label(
        `Theor. Cmp [Da]`,
        `Bind. Stoich.`,
        species_mass = `Theor. Prot. [Da]`[1],
        multi_species = multi_species
      ),
      relBinding = {
        pref <- `Binding [%]`[Preferred == "TRUE"]
        (if (length(pref) > 0) pref[1] else `Binding [%]`[1]) / 100
      }
    ) |>
    dplyr::select(-`Peak Signal [Da]`, -`Theor. Prot. [Da]`) |>
    dplyr::mutate(
      `Binding [%]` = paste0(sprintf("%.2f", relBinding * 100), "%")
    )

  # Unbound slice per proteoform, splitting the unbound remainder by the share
  # each apo peak holds of the total unbound signal
  unbound_rel <- 1 - tbl$`Tot. Binding [%]`[1] / 100
  if (nrow(species) > 0 && unbound_rel > 0) {
    shares <- species$prot_intensity / sum(species$prot_intensity)
    cmp_table <- rbind(
      cmp_table,
      data.frame(
        "Cmp Name" = "Unbound",
        "Sample ID" = "Unbound",
        total_bind = 100 - tbl$`Tot. Binding [%]`[1],
        mass_stoich = unbound_label(species$theor_prot, multi_species),
        relBinding = unbound_rel * shares,
        "Binding [%]" = paste0(
          sprintf("%.2f", unbound_rel * shares * 100),
          "%"
        ),
        check.names = FALSE
      )
    )
  }

  colors <- colors %||%
    key_colors(
      key %||% color_key(hits_summary),
      color_variable,
      if (color_variable == "Compounds") {
        unique(as.character(cmp_table$`Cmp Name`))
      } else {
        unique(as.character(cmp_table$`Sample ID`))
      },
      theme = theme,
      trunc = truncate_names
    )

  if (color_variable == "Compounds") {
    cmp_table$color <- colors[match(cmp_table$`Cmp Name`, names(colors))]
  } else {
    cmp_table$color <- colors[match(cmp_table$`Sample ID`, names(colors))]
  }
  cmp_table$color[cmp_table$`Cmp Name` == "Unbound"] <- status_colors(
    theme
  )$unbound

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

hex_to_rgba <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

#' @export
stats_histogram <- function(
  hits_summary,
  theme = "light",
  show = "Correct"
) {
  df <- dplyr::distinct(hits_summary, Sample, .keep_all = TRUE)

  # A sample without any protein or complex peak has no value to count
  # (plotly warns about every NA it is handed)
  metric_col <- if (show == "Unmatched") "% Unmatched" else "% Correct"
  df <- df[!is.na(df[[metric_col]]), , drop = FALSE]

  x_col <- df[[metric_col]]
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

  hex_correct <- status_colors(theme)$correct
  hex_unmatched <- status_colors(theme)$unmatched
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

# Axis range fitted to the data: 5% of the spread on either side, at least half
# a percentage point so a box of identical values is not stretched over noise
data_axis_range <- function(vals) {
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) {
    return(c(-5, 105))
  }
  pad <- max(diff(range(vals)) * 0.05, 0.5)
  range(vals) + c(-pad, pad)
}

# Label heights for values on a vertical axis: each label sits at its value
# unless it would come closer than min_gap to a neighbour, in which case the
# crowded labels are spread evenly around their mean value. Order is kept, and
# the labels stay within lower/upper (half a gap from either end)
spread_label_positions <- function(vals, min_gap, lower = -Inf, upper = Inf) {
  n <- length(vals)
  if (n == 0) {
    return(numeric(0))
  }
  ord <- order(vals)
  target <- vals[ord]
  lo <- lower + min_gap / 2
  hi <- upper - min_gap / 2
  # Clusters of neighbouring labels, each placed as one block
  clusters <- as.list(seq_len(n))
  place <- function(idx) {
    k <- length(idx)
    centre <- mean(target[idx])
    first <- centre - (k - 1) * min_gap / 2
    first <- min(max(first, lo), hi - (k - 1) * min_gap)
    first + (seq_len(k) - 1) * min_gap
  }
  repeat {
    pos <- unlist(lapply(clusters, place))
    tops <- cumsum(lengths(clusters))
    gaps <- diff(pos)[tops[-length(tops)]]
    clash <- which(gaps < min_gap - 1e-9)
    if (length(clash) == 0) {
      break
    }
    i <- clash[1]
    clusters[[i]] <- c(clusters[[i]], clusters[[i + 1]])
    clusters[[i + 1]] <- NULL
  }
  out <- numeric(n)
  out[ord] <- pos
  out
}

#' @export
stats_boxplot <- function(
  hits_summary,
  theme = "light",
  show_points = TRUE,
  show = "Correct",
  fit_box = FALSE
) {
  df <- dplyr::distinct(hits_summary, Sample, .keep_all = TRUE)

  # A sample without any protein or complex peak (NA values) is not drawn, and
  # would turn the whisker fences below into NA
  df <- df[
    !is.na(df[[if (show == "Unmatched") "% Unmatched" else "% Correct"]]),
    ,
    drop = FALSE
  ]

  font_color <- if (theme == "light") "black" else "white"
  grid_color <- if (theme == "light") {
    "rgba(0,0,0,0.1)"
  } else {
    "rgba(255,255,255,0.2)"
  }
  dot_border_color <- if (theme == "light") {
    "rgba(0,0,0,0.5)"
  } else {
    "rgba(255,255,255,0.5)"
  }

  hex_base <- if (show == "Unmatched") {
    status_colors(theme)$unmatched
  } else {
    status_colors(theme)$correct
  }
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

  # Without Show Points the samples beyond the whiskers are still drawn, as
  # outliers on the box's centre line (Tukey's box plot); the axis spans them,
  # and was left empty below the whisker while they were hidden
  is_outlier <- metric_vals_bp < lo_fence_bp | metric_vals_bp > hi_fence_bp
  pt_idx <- if (show_points) seq_len(nrow(df)) else which(is_outlier)
  if (length(pt_idx) > 0) {
    x_pts <- if (show_points) {
      stats::runif(length(pt_idx), -box_half_width, box_half_width)
    } else {
      rep(0, length(pt_idx))
    }
    p <- plotly::plot_ly(
      type = "scatter",
      mode = "markers",
      x = x_pts,
      y = metric_vals_bp[pt_idx],
      # I(): a single outlier would otherwise go out as a scalar, which the
      # hover showed as a literal "%{text}"
      text = I(df$Sample[pt_idx]),
      customdata = I(df$Sample[pt_idx]),
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

  # The axis spans the data (box, whiskers and outliers), or with Fit to Box
  # only the whiskers and the mean; the label spacing below is a share of it
  y_range_bp <- if (fit_box && is.finite(lo_fence_bp)) {
    data_axis_range(c(lo_fence_bp, hi_fence_bp, mean_bp))
  } else {
    data_axis_range(metric_vals_bp)
  }

  # Samples the fitted axis leaves out are counted in the margin above or
  # below the plot, so a zoomed box does not pass for the whole data
  off_scale_annots <- list()
  if (fit_box) {
    below <- metric_vals_bp[metric_vals_bp < y_range_bp[1]]
    above <- metric_vals_bp[metric_vals_bp > y_range_bp[2]]
    off_scale_note <- function(vals, side) {
      if (length(vals) == 0) {
        return(NULL)
      }
      list(
        x = 1,
        xref = "paper",
        xanchor = "right",
        y = if (side == "below") 0 else 1,
        yref = "paper",
        yanchor = if (side == "below") "top" else "bottom",
        text = sprintf(
          "%s %d sample%s %s the axis (%s %.2f%%)",
          if (side == "below") "▾" else "▴",
          length(vals),
          if (length(vals) == 1) "" else "s",
          side,
          if (side == "below") "lowest" else "highest",
          if (side == "below") min(vals) else max(vals)
        ),
        showarrow = FALSE,
        font = list(color = font_color, size = 10)
      )
    }
    off_scale_annots <- Filter(
      Negate(is.null),
      list(off_scale_note(below, "below"), off_scale_note(above, "above"))
    )
  }

  # Statistics beside the box, each at the height of the line it describes.
  # Statistics equal at the shown precision share one label ("Median = Q3").
  # Only labels too close to be read apart are moved, and only those get a
  # short connector back to their line.
  stat_vals <- c(Q1 = q1_bp, Median = med_bp, Mean = mean_bp, Q3 = q3_bp)
  stat_vals <- stat_vals[is.finite(stat_vals)]
  stat_txt <- sprintf("%.2f", stat_vals)
  label_groups <- split(names(stat_vals), factor(stat_txt, unique(stat_txt)))
  label_vals <- vapply(
    label_groups,
    function(nm) mean(stat_vals[nm]),
    numeric(1)
  )
  label_text <- vapply(
    names(label_groups),
    function(txt) {
      nm <- label_groups[[txt]]
      if (length(nm) == 4 && diff(range(metric_vals_bp)) < 0.005) {
        sprintf("All values: %s%%", txt)
      } else {
        sprintf("%s: %s%%", paste(nm, collapse = " = "), txt)
      }
    },
    character(1)
  )
  min_gap_bp <- 0.075 * diff(y_range_bp)
  label_ys <- spread_label_positions(
    label_vals,
    min_gap = min_gap_bp,
    lower = y_range_bp[1],
    upper = y_range_bp[2]
  )
  moved <- abs(label_ys - label_vals) > 0.1 * min_gap_bp

  # The gap between box and label is in pixels (xshift, pixel-sized shapes),
  # which the image export scales along with the label text. Connectors are
  # shapes rather than annotation arrows, whose tails ran into enlarged text.
  label_shift_px <- 16
  connector_shapes <- unname(Map(
    function(val, lab_y) {
      list(
        type = "line",
        xref = "x",
        yref = "y",
        xsizemode = "pixel",
        xanchor = box_half_width,
        x0 = 1,
        x1 = label_shift_px - 4,
        y0 = val,
        y1 = lab_y,
        line = list(color = hex_to_rgba(hex_base, 0.6), width = 1)
      )
    },
    label_vals[moved],
    label_ys[moved]
  ))

  sa_font <- list(color = font_color, size = 10)
  bp_annots <- unname(Map(
    function(lab_y, txt) {
      list(
        x = box_half_width,
        y = lab_y,
        xref = "x",
        yref = "y",
        xshift = label_shift_px,
        text = txt,
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "middle",
        font = sa_font
      )
    },
    label_ys,
    label_text
  ))
  bp_annots <- c(bp_annots, off_scale_annots)

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
        range = y_range_bp,
        color = font_color,
        gridcolor = grid_color,
        # Now that the axis ends at the data, a zero line would sit on the
        # lower whisker of a box reaching 0%
        zeroline = FALSE,
        hoverformat = ".2f"
      ),
      # The x axis has no labels and the statistics sit inside the plot, so
      # plotly's default margins (80-100 px) would only shrink the box
      margin = list(l = 70, r = 10, t = 20, b = 20),
      annotations = if (length(bp_annots) > 0) bp_annots else NULL,
      shapes = c(box_shapes, connector_shapes)
    )
}

#' @export
stats_scatter <- function(
  hits_summary,
  full_scale = FALSE,
  group_by = NULL,
  key = NULL,
  theme = "light",
  show = "Correct"
) {
  metric_col <- if (show == "Unmatched") "% Unmatched" else "% Correct"
  metric_label <- if (show == "Unmatched") "Unmatched [%]" else "Correct [%]"

  # A sample without any protein or complex peak (NA binding) is not drawn
  df <- dplyr::distinct(hits_summary, Sample, .keep_all = TRUE)
  df <- df[
    !is.na(df[[metric_col]]) & !is.na(df$`Total % Binding`),
    ,
    drop = FALSE
  ]
  df$`Total % Binding` <- df$`Total % Binding` * 100

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
  key <- key %||% color_key(hits_summary)
  groups <- as.character(key_order(key, group_by, unique(df[[group_by]])))
  df[[group_by]] <- as.character(df[[group_by]])
  color_map <- key_colors(key, group_by, groups, theme = theme)
  # Past the twelfth colour the marker shape tells groups apart
  symbol_map <- tier_symbol(key_tiers(key, group_by, groups))
  names(symbol_map) <- groups

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
        symbol = symbol_map[[grp]],
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

  # No groups left (e.g. no sample has a value for the metric)
  ensure_trace(p) |>
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
    ) |>
    htmlwidgets::onRender(group_highlight_js)
}

# Fades every other group while a point is hovered, so a group stays easy to
# pick out when the colours repeat. Opacity only: the traces stay as built.
group_highlight_js <- "
function(el) {
  var base = null;
  el.on('plotly_hover', function(d) {
    if (!d || !d.points || !d.points.length || el.data.length < 2) return;
    if (base === null) {
      base = el.data.map(function(t) {
        return (t.marker && t.marker.opacity !== undefined) ? t.marker.opacity : 1;
      });
    }
    var cn = d.points[0].curveNumber;
    Plotly.restyle(el, {'marker.opacity': base.map(function(o, i) {
      return i === cn ? 1 : 0.12;
    })});
  });
  el.on('plotly_unhover', function() {
    if (base !== null) Plotly.restyle(el, {'marker.opacity': base});
  });
}
"

#' @export
stats_violin <- function(
  hits_summary,
  group_by = "Protein",
  theme = "light",
  key = NULL,
  inner = "box",
  show = "Correct"
) {
  metric_col <- if (show == "Unmatched") "% Unmatched" else "% Correct"
  metric_label <- if (show == "Unmatched") "Unmatched [%]" else "Correct [%]"
  hover_label <- if (show == "Unmatched") "Unmatched [%]" else "Correct [%]"

  # A sample without any protein or complex peak (NA value) is not drawn
  df <- dplyr::distinct(hits_summary, Sample, .keep_all = TRUE)
  df <- df[!is.na(df[[metric_col]]), , drop = FALSE]
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

  y_range <- NULL
  # Autoscale would stretch a float-noise spread across the whole axis
  y_vals <- df[[metric_col]]
  if (length(y_vals) > 0 && diff(range(y_vals)) < 0.01) {
    y_range <- mean(range(y_vals)) + c(-1, 1)
  }

  # Groups in key order, the ones without a value ("Unknown") last and muted
  key <- key %||% color_key(hits_summary)
  groups <- unique(df[[group_by]])
  known <- groups[groups != "Unknown"]
  groups <- c(key_order(key, group_by, known), groups[groups == "Unknown"])
  color_map <- key_colors(
    key,
    group_by,
    ifelse(groups == "Unknown", NA, groups),
    theme = theme
  )
  names(color_map) <- groups

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

  # No groups when no sample has a value for the metric
  ensure_trace(p) |>
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
  key = NULL,
  scale_mode = "minmax",
  theme = "light"
) {
  # Only Total % Binding is stored as 0-1 fraction; % Correct / % Unmatched are 0-100
  fraction_vars <- c("Total % Binding")
  pct_vars <- c("Total % Binding", "% Correct", "% Unmatched")
  numeric_vars <- c("Total % Binding", "% Correct", "% Unmatched")
  # Continuous fill of each numeric variable; the others (compound, protein,
  # concentration, time) take the result's fixed colours
  numeric_ramps <- c(
    "Total % Binding" = "binding",
    "% Correct" = "correct",
    "% Unmatched" = "unmatched"
  )
  key <- key %||% color_key(hits_summary)

  font_color <- if (theme == "light") "black" else "white"
  paper_bg <- if (theme == "light") "#f0f0f5" else "#23252e"
  tile_bg <- "rgba(190,192,200,0.38)"

  df <- dplyr::distinct(hits_summary, Sample, .keep_all = TRUE)
  # Concentration and time columns carry their unit in the name
  if (variable %in% c("Concentration", "Time") && !variable %in% names(df)) {
    unit_col <- names(df)[startsWith(names(df), paste0(variable, " ["))][1]
    if (!is.na(unit_col)) {
      df[[variable]] <- df[[unit_col]]
    }
  }
  if (!variable %in% names(df)) {
    return(ensure_trace(plotly::plotly_empty()))
  }

  if (
    !"Well" %in% names(df) ||
      all(is.na(df[["Well"]])) ||
      all(trimws(as.character(df[["Well"]])) %in% c("", "NA", "N/A"))
  ) {
    return(ensure_trace(plotly::plotly_empty()))
  }

  # Classify each sample's well state. Evaluated over all rows of a sample
  # rather than its first one: a sample contributes one row per compound and
  # per protein species, and a row without an adduct is no binding event.
  prot_seen <- tapply(
    !is.na(hits_summary$`Measured Mw Protein [Da]`),
    hits_summary$Sample,
    any
  )
  cmp_seen <- tapply(
    is_complex_row(hits_summary),
    hits_summary$Sample,
    any
  )
  # as.logical() drops the 1D-array dim from tapply(); case_when() rejects arrays
  no_prot_flag <- as.logical(!prot_seen[df$Sample] & !cmp_seen[df$Sample])
  no_hit_flag <- as.logical(prot_seen[df$Sample] & !cmp_seen[df$Sample])
  well_state <- dplyr::case_when(
    no_prot_flag ~ "no_prot",
    no_hit_flag ~ "no_hit",
    TRUE ~ "normal"
  )

  raw_vals <- df[[variable]]
  is_numeric_var <- variable %in% numeric_vars

  cat_levels <- NULL
  if (!is_numeric_var) {
    cat_levels <- as.character(key_order(
      key,
      variable,
      unique(stats::na.omit(as.character(raw_vals)))
    ))
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

  # Normalize well IDs: "A01" → "A1", "B12" → "B12" (as the deconvolution's
  # Sample Distribution reads them)
  norm_wells <- parse_wells(df[["Well"]])$well_id

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

  # The whole plate the wells belong to, 96 or 384 wells (plate_format(), as
  # the deconvolution's Sample Distribution), with all its row and column
  # labels. Cropping to the occupied wells drew a single row for a run that
  # used only a few wells.
  has_empty_in_rect <- FALSE
  if (any(presence_mat)) {
    format <- plate_format(data$well_id)
    row_range <- format$rows
    col_range <- format$cols
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
    cs <- ramp_colorscale(numeric_ramps[[variable]], theme)
    show_scale <- TRUE
    cb_title <- variable
  } else {
    n_cats <- length(cat_levels)
    pal <- unname(key_colors(key, variable, cat_levels, theme = theme))
    zmin <- 1
    zmax <- max(n_cats, 2)
    show_scale <- FALSE
    cb_title <- variable
  }

  # The well states go in a row of their own under the plate (legend2), so
  # beside the plate only the levels or the colorbar take room
  has_well_states <- length(no_prot_row_vals) > 0 ||
    length(no_hit_row_vals) > 0 ||
    has_empty_in_rect

  # Rows by their position (1 = A), labelled by letter: plotly keeps the
  # tiles square (scaleanchor) only between axes of the same type, and a
  # category axis of row letters against the linear columns stretched the
  # tiles to the card's shape
  row_pos <- seq_along(rows)
  big_plate <- length(cols) > 12

  # Text for the tiles of a Batch Control card, about 25 px on a 96-well and
  # 12 px on a 384-well plate (the marks follow the tiles, being in plate units)
  label_size <- if (big_plate) 8 else 10
  tick_size <- if (big_plate) 10 else 12
  x_ticks <- if (big_plate) cols[cols %% 2 == 1] else cols
  tile_codes <- NULL

  # Every well as a tile underneath, in the colour of the "Empty" legend chip,
  # so an empty well looks like a well and the whole plate reads as one (as
  # the deconvolution's Sample Distribution); the gaps between the tiles show
  # the card through. Empty wells used to merge into one grey block.
  tile_gap <- if (big_plate) 2 else 3
  empty_plate <- plotly::plot_ly() |>
    plotly::add_trace(
      type = "heatmap",
      z = matrix(0, nrow = length(rows), ncol = length(cols)),
      x = cols,
      y = row_pos,
      colorscale = list(list(0, tile_bg), list(1, tile_bg)),
      showscale = FALSE,
      xgap = tile_gap,
      ygap = tile_gap,
      hoverinfo = "skip"
    )

  if (is_numeric_var) {
    p <- plotly::add_trace(
      empty_plate,
      z = z_mat,
      x = cols,
      y = row_pos,
      type = "heatmap",
      colorscale = cs,
      showscale = show_scale,
      zmin = zmin,
      zmax = zmax,
      xgap = tile_gap,
      ygap = tile_gap,
      text = text_mat,
      hoverinfo = "text",
      # Level with the plate, its title turned along the bar so the bar takes
      # little width (static/js/plate_heatmap.js fits its length to the plate)
      colorbar = list(
        title = list(
          text = cb_title,
          side = "right",
          font = list(color = font_color, size = 12)
        ),
        tickfont = list(color = font_color, size = 11),
        outlinecolor = font_color,
        thickness = 12,
        y = 1,
        yanchor = "top",
        len = 1
      )
    )
  } else {
    # Labels on the tiles once colour alone no longer tells the levels apart:
    # past the categorical colours a number, named in the legend; concentrations
    # and times by their value, when that is short enough for a tile (a
    # 96-well one: a 384-well tile holds a number, not a value of four digits)
    ordered_var <- key_variable(variable) %in% c("concentration", "time")
    if (n_cats > 0) {
      if (!ordered_var && n_cats > length(categorical_palette$light)) {
        tile_codes <- as.character(seq_len(n_cats))
      } else if (
        ordered_var &&
          !big_plate &&
          n_cats > 8 &&
          all(nchar(cat_levels) <= 4)
      ) {
        tile_codes <- cat_levels
      }
    }
    legend_names <- if (!is.null(tile_codes) && !ordered_var) {
      paste0(tile_codes, " · ", cat_levels)
    } else {
      cat_levels
    }

    # The plate in a faded grey underneath. It carries the hover text and the
    # clicks, and shows where a level is hidden from the legend.
    faded <- color_alpha(status_colors(theme)$muted, 0.3)
    p <- plotly::add_trace(
      empty_plate,
      z = ifelse(is.na(z_mat), NA_real_, 1),
      x = cols,
      y = row_pos,
      type = "heatmap",
      colorscale = list(list(0, faded), list(1, faded)),
      showscale = FALSE,
      xgap = tile_gap,
      ygap = tile_gap,
      text = text_mat,
      hoverinfo = "text"
    )

    # One trace per level, in a legend group with its legend chip: a click on
    # the chip hides the level's wells, a double click shows them alone
    for (i in seq_len(n_cats)) {
      group <- paste0("level-", i)
      is_level <- !is.na(z_mat) & z_mat == i
      p <- p |>
        plotly::add_trace(
          type = "heatmap",
          z = ifelse(is_level, 1, NA_real_),
          x = cols,
          y = row_pos,
          colorscale = list(list(0, pal[i]), list(1, pal[i])),
          showscale = FALSE,
          xgap = tile_gap,
          ygap = tile_gap,
          name = cat_levels[i],
          legendgroup = group,
          showlegend = FALSE,
          hoverinfo = "skip",
          # A literal template would print on every cell of the trace, the
          # gaps included: the label goes into the level's own cells only
          text = if (!is.null(tile_codes)) ifelse(is_level, tile_codes[i], ""),
          texttemplate = if (!is.null(tile_codes)) "%{text}",
          textfont = list(
            size = label_size,
            color = unname(get_contrast_color(pal[i]))
          ),
          inherit = FALSE
        ) |>
        plotly::add_trace(
          type = "scatter",
          x = min(cols) - 10000,
          y = 1,
          mode = "markers",
          visible = TRUE,
          marker = list(
            symbol = "square",
            color = pal[i],
            size = 10,
            line = list(width = 0)
          ),
          name = legend_names[i],
          legendgroup = group,
          showlegend = TRUE,
          inherit = FALSE,
          hoverinfo = "skip",
          legendrank = 200 + i,
          legendgrouptitle = if (i == 1) {
            list(text = cb_title, font = list(color = font_color, size = 12))
          }
        )
    }
  }

  # Well-state marks in black or white, whichever stands out on their tile;
  # a tile without a value shows the light empty-tile grey
  mark_color <- function(row, col) {
    v <- suppressWarnings(as.numeric(z_mat[row, as.character(col)]))
    if (!length(v) || is.na(v)) {
      return(if (theme == "light") "#000000" else "#ffffff")
    }
    tile <- if (is_numeric_var) {
      ramp_at((v - zmin) / (zmax - zmin), numeric_ramps[[variable]], theme)
    } else {
      pal[min(max(1L, as.integer(v)), length(pal))]
    }
    unname(get_contrast_color(tile))
  }

  # The marks in plate units, so they keep to their tile at any card size;
  # sized in pixels for an assumed tile, they spilled over the tiles of a
  # short card and of a 384-well plate. Around a label the ring is wider.
  has_no_prot <- length(no_prot_row_vals) > 0
  mark_width <- if (big_plate) 1.5 else 2
  r_np <- 0.24
  r_nh <- if (is.null(tile_codes)) 0.3 else 0.4
  np_shapes <- if (has_no_prot) {
    unlist(
      lapply(seq_along(no_prot_col_vals), function(i) {
        cx <- no_prot_col_vals[i]
        cy <- which(rows == no_prot_row_vals[i])
        col <- mark_color(no_prot_row_vals[i], cx)
        cross_line <- function(dy) {
          list(
            type = "line",
            xref = "x",
            yref = "y",
            x0 = cx - r_np,
            y0 = cy - dy,
            x1 = cx + r_np,
            y1 = cy + dy,
            line = list(color = col, width = mark_width + 0.5)
          )
        }
        list(cross_line(r_np), cross_line(-r_np))
      }),
      recursive = FALSE
    )
  } else {
    list()
  }
  # A key of the well states in the row under the plate (legend2), with
  # short names: the long ones made the legend beside the plate wide. Each in
  # a group of its own, as a horizontal legend stacks a group's entries.
  well_state_key <- function(p, name, marker, rank) {
    plotly::add_trace(
      p,
      type = "scatter",
      mode = "markers",
      x = min(cols) - 10000,
      y = 1,
      visible = TRUE,
      marker = marker,
      name = name,
      legend = "legend2",
      showlegend = TRUE,
      inherit = FALSE,
      hoverinfo = "skip",
      legendrank = rank,
      legendgroup = legend_key_group(paste0("well_states-", rank))
    )
  }
  if (has_no_prot) {
    p <- well_state_key(
      p,
      "No Protein",
      list(
        symbol = "x-thin",
        size = 9,
        color = "rgba(0,0,0,0)",
        line = list(color = font_color, width = 2.5)
      ),
      rank = 102
    )
  }

  has_no_hit <- length(no_hit_col_vals) > 0
  nh_shapes <- if (has_no_hit) {
    lapply(seq_along(no_hit_col_vals), function(i) {
      cx <- no_hit_col_vals[i]
      cy <- which(rows == no_hit_row_vals[i])
      list(
        type = "circle",
        xref = "x",
        yref = "y",
        x0 = cx - r_nh,
        y0 = cy - r_nh,
        x1 = cx + r_nh,
        y1 = cy + r_nh,
        fillcolor = "rgba(0,0,0,0)",
        line = list(
          color = mark_color(no_hit_row_vals[i], cx),
          width = mark_width
        )
      )
    })
  } else {
    list()
  }
  if (has_no_hit) {
    p <- well_state_key(
      p,
      "No Binding",
      list(
        symbol = "circle-open",
        size = 9,
        color = font_color,
        line = list(color = font_color, width = 2)
      ),
      rank = 101
    )
  }
  if (has_empty_in_rect) {
    p <- well_state_key(
      p,
      "Empty",
      list(
        symbol = "square",
        size = 10,
        color = tile_bg,
        line = list(color = "rgba(130,132,140,0.6)", width = 1)
      ),
      rank = 103
    )
  }

  show_legend_any <- !is_numeric_var || has_well_states
  # Plot margins, the row labels left and the column labels on top; below,
  # the row of well states
  plate_margin <- list(
    l = if (big_plate) 22 else 26,
    t = 20,
    b = if (has_well_states) 28 else 8
  )

  p_out <- p |>
    plotly::layout(
      shapes = c(np_shapes, nh_shapes),
      dragmode = "zoom",
      showlegend = show_legend_any,
      legend = list(
        # Numbered names ("12 · COM-3818744") a size smaller, they set the
        # width of the legend beside the plate
        font = list(
          color = font_color,
          size = if (is.null(tile_codes)) 11 else 10
        ),
        bgcolor = "rgba(0,0,0,0)",
        y = 1,
        yanchor = "top",
        # Every level is a legend group of its own (chip + tiles); keep
        # them packed like one list
        tracegroupgap = 2
      ),
      legend2 = list(
        orientation = "h",
        font = list(color = font_color, size = 11),
        bgcolor = "rgba(0,0,0,0)",
        x = 0,
        xanchor = "left",
        y = 0,
        yanchor = "top",
        itemwidth = 30
      ),
      hoverlabel = list(
        bgcolor = "rgba(56, 56, 124, 0.86)",
        font = list(size = 14, color = "white"),
        bordercolor = "white"
      ),
      # Square tiles, the plot area shrunk to the plate (as the deconvolution's
      # Sample Distribution) and kept at the top, level with the legend. The
      # 384-well plate labels every other column, its numbers ran together.
      # In the app, plate_heatmap.js takes over the plate's geometry: plotly
      # kept the domain of a first drawing made while the tab was hidden,
      # which left the plate a fraction of its card.
      xaxis = list(
        side = "top",
        tickmode = "array",
        tickvals = x_ticks,
        ticktext = as.character(x_ticks),
        tickfont = list(color = font_color, size = tick_size),
        tickangle = 0,
        ticklen = 0,
        showgrid = FALSE,
        zeroline = FALSE,
        automargin = FALSE,
        range = c(min(cols) - 0.5, max(cols) + 0.5),
        constrain = "domain"
      ),
      yaxis = list(
        tickmode = "array",
        tickvals = row_pos,
        ticktext = rows,
        range = c(length(rows) + 0.5, 0.5),
        tickfont = list(color = font_color, size = tick_size),
        ticklen = 0,
        showgrid = FALSE,
        zeroline = FALSE,
        scaleanchor = "x",
        scaleratio = 1,
        constrain = "domain",
        constraintoward = "top",
        automargin = FALSE
      ),
      # Room under the plate for half of the last row's label, which plotly
      # hid once the plate filled the plot's height
      margin = list(
        t = plate_margin$t,
        r = 0,
        b = plate_margin$b,
        l = plate_margin$l
      ),
      # The empty wells are tiles of their own now (empty_plate), so the gaps
      # between all tiles show the card
      plot_bgcolor = "rgba(0,0,0,0)",
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
  # A no-op where the script is not loaded (an exported HTML page): there the
  # constraints above size the plate, drawn once at its final size
  htmlwidgets::onRender(
    p_out,
    "function(el, x, data) {
      if (window.kiwiMS_fitPlate) window.kiwiMS_fitPlate(el, data);
    }",
    data = c(list(cols = length(cols), rows = length(rows)), plate_margin)
  )
}
