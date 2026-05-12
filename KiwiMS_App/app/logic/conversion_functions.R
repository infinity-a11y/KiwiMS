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
      paste_hook_js
    ],
)

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
    sprintf(
      "WHERE sample IN (%s)",
      paste(sprintf("'%s'", samples), collapse = ",")
    )
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
set_selected_tab <- function(tab_name, session) {
  bslib::nav_select(
    id = "tabs",
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

  handsontable <- rhandsontable::rhandsontable(
    tab,
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
    handsontable <- rhandsontable::hot_col(
      handsontable,
      col = conc_time_idx,
      type = "numeric",
      allowInvalid = FALSE,
      format = "0.##########"
    ) |>
      rhandsontable::hot_validate_numeric(
        cols = conc_time_idx,
        min = 0
      )
  }

  return(handsontable)
}

# Function to fill missing columns in sample table
#' @export
fill_sample_table <- function(sample_table, ki_kinact) {
  # Stash Replicate (not part of the 7-col standard) to avoid count mismatch
  has_rep <- "Replicate" %in% names(sample_table)
  rep_col <- if (has_rep) sample_table[["Replicate"]] else NULL
  if (has_rep) {
    sample_table <- sample_table[,
      names(sample_table) != "Replicate",
      drop = FALSE
    ]
  }

  if (ki_kinact) {
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
  if (ki_kinact) {
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
  # Find protein peak
  protein_peak <- peaks$mass >= protein_mw[, -1] - peak_tolerance &
    peaks$mass <= protein_mw[, -1] + peak_tolerance

  # Abort if peaks show invalid peaks
  if (!any(protein_peak)) {
    log_alert()

    hits_df <- data.frame(
      well = well,
      sample = sample,
      protein = protein_mw[, 1],
      theor_prot = as.numeric(protein_mw[, -1]),
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
      unmatched = 100,
      correct = 0
    )

    return(hits_df)
  }

  # Keep only peaks above protein mw
  peaks_valid <- peaks$mass >= protein_mw[, -1] - peak_tolerance
  if (any(peaks_valid) && sum(peaks_valid) > 1) {
    peaks_filtered <- as.data.frame(peaks[peaks_valid, ])
  } else {
    hits_df <- data.frame(
      well = well,
      sample = sample,
      protein = protein_mw[, 1],
      theor_prot = as.numeric(protein_mw[, -1]),
      measured_prot = peaks$mass[which(protein_peak)],
      delta_prot = abs(
        as.numeric(protein_mw[, -1]) - peaks$mass[which(protein_peak)]
      ),
      prot_intensity = peaks$intensity[which(protein_peak)],
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
  complex_mat <- mat + protein_mw[, -1]

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
          protein = protein_mw[, 1],
          theor_prot = as.numeric(protein_mw[, -1]),
          measured_prot = peaks$mass[which(protein_peak)],
          delta_prot = round(
            abs(
              as.numeric(protein_mw[, -1]) - peaks$mass[which(protein_peak)]
            ),
            2
          ),
          prot_intensity = peaks$intensity[which(protein_peak)],
          peak = peaks_filtered[j, "mass"],
          intensity = peaks_filtered[j, "intensity"],
          compound = rownames(hits)[indices[1]],
          cmp_mass = cmp_mass,
          delta_cmp = abs(
            (as.numeric(cmp_mass) * multiple) -
              (peaks_filtered[j, "mass"] - as.numeric(protein_mw[, -1]))
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
      protein = protein_mw[, 1],
      theor_prot = as.numeric(protein_mw[, -1]),
      measured_prot = peaks$mass[which(protein_peak)],
      delta_prot = abs(
        as.numeric(protein_mw[, -1]) - peaks$mass[which(protein_peak)]
      ),
      prot_intensity = peaks$intensity[which(protein_peak)],
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
  }

  # Calculate % unmatched and % correct
  hits_df$unmatched <- unmatched <- sum(!peaks$mass %in% hits_df$peak) /
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
  hits1 <<- hits

  # Check 'hits' argument validity
  if (!is.data.frame(hits) || nrow(hits) < 1) {
    log_err_no_df()
    return(NULL)
  } else if (ncol(hits) != 16) {
    log_err_cols(ncol(hits))
    return(NULL)
  } else if (anyNA(hits[, names(hits) != "well"])) {
    hits <- hits |>
      dplyr::mutate(
        `%binding` = NA
      ) |>
      dplyr::mutate(
        `%binding_tot` = NA,
        .before = peak
      )
  } else {
    # Peaks: 1000(IA), 1010(IB), 1020(IC), 1011(ID)
    # Peaks are in hits data frame

    # Gesamtintensität: IA + IB + IC + ID = Itotal
    I_total <- sum(unique(hits$intensity)) + unique(hits$prot_intensity)
    perc_bind_prot <- unique(hits$prot_intensity) / I_total

    # nicht umgesetztes / ungebundenes Protein: IA / Itotal * 100

    # %BinIB = IB / Itotal * 100
    # %BinIC = IC / Itotal * 100
    # usw ...

    # %Bintotal = %BinIB + %BinIC + %BinID  (alles was nicht freies Prot ist)

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
    max_intensity <- max(c(hits$intensity, hits$prot_intensity))
    hits$intensity <- hits$intensity / max_intensity * 100
    hits$prot_intensity <- hits$prot_intensity / max_intensity * 100
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
    sprintf("  │  ├─ Total Intensity: %.2f (100%%)\n", total),
    sprintf("  │  ├─ Unbound Protein: %.2f (%.1f%%)\n", unbound, perc_unbound),
    sprintf("  │  └─ Total Binding:   %.2f (%.1f%%)", binding, perc_binding)
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

# Log binding kinetics analysis initiation
#' @export
log_binding_kinetics <- function(concentrations, times, units) {
  message(paste(
    sprintf(
      "  ├─ %s concentrations present from %s to %s [%s] \n",
      length(unique(concentrations)),
      min(concentrations),
      max(concentrations),
      units[1]
    ),
    sprintf(
      " ├─ %s time points present from %s to %s [%s] \n",
      length(unique(times)),
      min(times),
      max(times),
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
      concentration,
      unit
    )
  ))
}

# Log timepoints
log_timepoints <- function(data, unit, last) {
  n <- nrow(data)
  tmin <- min(data$time)
  tmax <- max(data$time)
  tp_label <- if (n == 1) "time point" else "time points"
  range_str <- if (tmin == tmax) {
    sprintf("%s %s", tmin, unit)
  } else {
    sprintf("%s - %s %s", tmin, tmax, unit)
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
    ),
    if (n < 3) {
      sprintf(
        ifelse(
          last,
          "\n  │     └─ %s ≥ 3 time points required for nonlinear fit",
          "\n  │  │  └─ %s ≥ 3 time points required for nonlinear fit"
        ),
        .col_warn(warning_sym)
      )
    }
  ))
}

# Log kobs result
log_kobs_result <- function(result, last, unit) {
  message(
    if (last) {
      paste(
        "  │     ├─ Nonlinear regression model fitted\n",
        sprintf(
          " │     ├─ k_obs = %s %s⁻¹\n",
          round(result$kobs, 4),
          unit
        ),
        sprintf(" │     ├─ v = %s\n", round(result$v, 4)),
        sprintf(" │     └─ Plateau = %s\n  │", round(result$kobs, 4))
      )
    } else {
      paste(
        "  │  │  ├─ Nonlinear regression model fitted.\n",
        sprintf(" │  │  ├─ k_obs = %s %s⁻¹\n", round(result$kobs, 4), unit),
        sprintf(" │  │  ├─ v = %s\n", round(result$v, 4)),
        sprintf(" │  │  └─ Plateau = %s\n  │  │", round(result$kobs, 4))
      )
    }
  )
}

# Log (Kᵢ/kᵢₙₐ꜀ₜ) analysis initiation
log_ki_kinact_analysis <- function() {
  message(paste(
    "  └─ Infer second-order rate constant Kᵢ/kᵢₙₐ꜀ₜ"
  ))
}

# Log Ki/kinact results
log_ki_kinact_results <- function(results, units) {
  message(
    paste0(
      sprintf(
        "     ├─ kᵢₙₐ꜀ₜ = %s ± %s %s⁻¹\n",
        round(results$Params[1, 1], 4),
        round(results$Params[1, 2], 4),
        units[["Time"]]
      ),
      sprintf(
        "     ├─ Kᵢ = %s ± %s %s\n",
        round(results$Params[2, 1], 4),
        round(results$Params[2, 2], 4),
        units[["Concentration"]]
      ),
      sprintf(
        "     └─ kᵢₙₐ꜀ₜ/Kᵢ = %s %s⁻¹ %s⁻¹",
        round(results$Params[1, 1] / results$Params[2, 1], 4),
        units[["Concentration"]],
        units[["Time"]]
      )
    )
  )
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
  ki_kinact = FALSE,
  config = NULL
) {
  samples <- names(results$deconvolution)
  protein_mw <- protein_table$`Mass 1`
  compound_mw <- as.matrix(compound_table[, -1])
  rownames(compound_mw) <- compound_table[, 1]

  #TODO
  results <<- results
  sample_table <<- sample_table
  protein_table <<- protein_table
  compound_table <<- compound_table
  peak_tolerance <<- peak_tolerance
  max_multiples <<- max_multiples
  samples <<- samples
  protein_mw <<- protein_mw
  compound_mw <<- compound_mw
  config1 <<- config

  hits_max <- if (ki_kinact) 80 else 100

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
      if (ki_kinact) " Computing binding kinetics..." else ""
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

# Function perform checks for ki/kinact analysis prerequisites
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
  tab <- hits_summary |>
    dplyr::group_by(dplyr::pick(dplyr::contains("Concentration"))) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

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
      c("binding_table", "binding_plot")
  )

  # Get concentration names
  conc_names <- names(binding_kobs_result[concentrations])

  # Fill kobs result table
  kobs_result_table <- data.frame()
  for (i in conc_names) {
    kobs_result_table <- rbind(
      kobs_result_table,
      data.frame(
        binding_kobs_result[[i]]$kobs,
        binding_kobs_result[[i]]$v,
        binding_kobs_result[[i]]$plateau
      )
    )
  }
  rownames(kobs_result_table) <- conc_names
  colnames(kobs_result_table) <- c("kobs", "v", "plateau")

  # Add kobs result table
  binding_kobs_result$kobs_result_table <- kobs_result_table

  return(binding_kobs_result)
}

# Function to add Ki/kinact results to result list
#' @export
add_ki_kinact_result <- function(result_list, units) {
  # Log (Kᵢ/kᵢₙₐ꜀ₜ) analysis initiation
  log_ki_kinact_analysis()

  # Calculcate Ki/kinact from binding/kobs result
  ki_kinact_result <- compute_ki_kinact(
    result_list[["binding_kobs_result"]],
    units = units
  )

  # Log Ki/kinact results
  log_ki_kinact_results(results = ki_kinact_result, units = units)

  return(ki_kinact_result)
}

# Function to generate and display binding plot
#' @export
make_binding_plot <- function(
  kobs_result,
  filter_conc = NULL,
  colors = NULL,
  units = NULL,
  theme = "dark"
) {
  # Filter for specified concentration
  if (!is.null(filter_conc)) {
    kobs_result$binding_table <- dplyr::filter(
      kobs_result$binding_table,
      concentration == filter_conc
    )
  }

  # Keep only non-zero observed data points
  df_points <- kobs_result$binding_table[
    !(kobs_result$binding_table$time == 0 |
      kobs_result$binding_table$binding == 0),
  ]

  # Set symbols to corresponding concentration
  symbol_map <- stats::setNames(
    symbols[1:length(levels(df_points$concentration))],
    levels(df_points$concentration)
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
          "K<sub>obs</sub>: %{customdata:.2f} ",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
          "⁻¹"
        ),
        "<extra></extra>"
      ),
      customdata = ~kobs,
      showlegend = FALSE
    ) |>
    # Observed binding
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
      error_y = list(
        type = "data",
        array = ~binding_sd,
        visible = TRUE,
        thickness = 1.5,
        width = 4
      ),
      hovertemplate = ~ paste(
        "<b>Observed</b><br>",
        paste(
          "Time: %{x}",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
          "<br>"
        ),
        "Binding [%]: %{y:.2f}<br>",
        paste0(
          "K<sub>obs</sub>: %{customdata:.2f} ",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
          "⁻¹"
        ),
        "<extra></extra>"
      ),
      customdata = ~kobs,
      showlegend = ifelse(is.null(filter_conc), TRUE, FALSE)
    ) |>
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
      xaxis = list(
        title = "Time [min]",
        color = font_color,
        showgrid = TRUE,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color
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
make_kobs_plot <- function(ki_kinact_result, colors, units, theme = "dark") {
  # Get predicted/modeled kobs
  df <- ki_kinact_result$Kobs_Data[
    !is.na(ki_kinact_result$Kobs_Data$predicted_kobs),
  ]

  # Get observed kobs data points
  df_points <- ki_kinact_result$Kobs_Data[
    !is.na(ki_kinact_result$Kobs_Data$kobs) &
      ki_kinact_result$Kobs_Data$kobs != 0,
  ]
  df_points$conc <- factor(df_points$conc, levels = sort(df_points$conc))

  # Set symbols to corresponding concentration
  ordered_conc <- df_points |>
    dplyr::arrange(dplyr::desc(conc)) |>
    dplyr::reframe(conc) |>
    unlist()

  symbol_map <- stats::setNames(
    symbols[1:length(ordered_conc)],
    ordered_conc
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

  # Generate plot
  kobs_plot <- plotly::plot_ly() |>
    # Predicted / modeled kobs
    plotly::add_lines(
      data = df,
      x = ~conc,
      y = ~predicted_kobs,
      colors = colors,
      symbols = symbol_map,
      line = list(width = 1.5, color = font_color),
      hovertemplate = paste(
        "<b>Predicted</b><br>",
        paste0(
          "Concentration: %{x:.2f} ",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
          "<br>"
        ),
        paste0(
          "K<sub>obs</sub>: %{y:.2f} ",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
          "⁻¹"
        ),
        "<extra></extra>"
      ),
      showlegend = FALSE
    ) |>
    # Calculated kobs
    plotly::add_markers(
      data = df_points,
      x = ~ as.numeric(as.character(conc)),
      y = ~kobs,
      type = "scatter",
      color = ~conc,
      marker = list(
        size = 12,
        opacity = 1,
        line = list(width = 1, color = font_color)
      ),
      name = ~conc,
      symbols = symbol_map,
      symbol = ~ as.character(conc),
      error_y = list(
        type = "data",
        array = ~kobs_se,
        visible = TRUE,
        thickness = 1.5,
        width = 4,
        color = font_color
      ),
      hovertemplate = paste(
        "<b>Calculated</b><br>",
        paste0(
          "Concentration: %{x:.2f} ",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
          "<br>"
        ),
        paste0(
          "K<sub>obs</sub>: %{y:.2f} ± %{error_y.array:.4f} ",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
          "⁻¹"
        ),
        "<extra></extra>"
      ),
    ) |>
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
      xaxis = list(
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
      yaxis = list(
        title = "k<sub>obs</sub> [s⁻¹]",
        color = font_color,
        showgrid = TRUE,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color
      )
    )

  # Return plot
  return(kobs_plot)
}

# Function to predict binding/kobs values
predict_values <- function(
  data,
  predict,
  x,
  interval,
  fitted_model,
  max = NULL
) {
  # Prepare sequence of predictions
  prediction_df <- data.frame(seq(
    0,
    ifelse(!is.null(max), max, max(data[[x]])),
    by = interval
  ))
  colnames(prediction_df) <- x

  # Predict using the fitted model
  predicted <- stats::predict(
    fitted_model,
    prediction_df
  )

  prediction_df[[paste0("predicted_", predict)]] <- predicted

  return(prediction_df)
}

compute_kobs <- function(hits, units) {
  # Prepare empty objects
  concentration_list <- list()
  binding_table <- data.frame()

  # Concentration and time columns
  conc <- names(hits)[grep("Concentration", names(hits))]
  time <- names(hits)[grep("Time", names(hits))]
  names(hits)[grep("Time", names(hits))] <- "time"

  # Starting values based on units
  # TODO
  if (units["Concentration"] == "M" & units["Time"] == "s") {
    start_vals <- c(v = 1, kobs = 0.0004)
  } else {
    start_vals <- c(v = 1, kobs = 0.001)
  }

  # Loop over each unique concentration
  for (i in as.character(unique(hits[[conc]]))) {
    last <- ifelse(
      i == utils::tail(as.character(unique(hits[[conc]])), 1),
      TRUE,
      FALSE
    )

    # Log concentrations
    log_concentration(
      concentration = i,
      unit = units["Concentration"],
      last = last
    )

    # Filter rows for this concentration
    raw_data <- hits |>
      dplyr::filter(!!rlang::sym(conc) == i)

    # Average technical replicates grouped by Replicate column; fall back to
    # distinct-by-time when the column is absent
    if ("Replicate" %in% names(raw_data) && !all(is.na(raw_data$Replicate))) {
      data <- raw_data |>
        dplyr::group_by(Replicate) |>
        dplyr::summarise(
          time = dplyr::first(time),
          binding = mean(binding, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      data <- dplyr::distinct(raw_data, time, .keep_all = TRUE)
    }

    # Log timepoints
    log_timepoints(data = data, unit = units["Time"], last = last)

    # If less than 3 entries abort and continue with next iteration
    if (sum(!is.na(data$binding)) < 3) {
      next
    }

    # Make dummy row to anchor fitting at 0
    dummy_row <- data[1, ]
    dummy_row$binding <- 0.0
    dummy_row$time <- 0

    # If Well column exists
    if ("Well" %in% colnames(data)) {
      dummy_row$Well <- "XX"
    }
    data <- rbind(data, dummy_row)

    # Nonlinear regression with customized minpack.lm::nlsLM() function
    nonlin_mod <- nlsLM_fixed(
      formula = binding ~ 100 * (v / kobs * (1 - exp(-kobs * time))),
      start = start_vals,
      data = data
    )

    # Extract parameters
    params <- summary(nonlin_mod)$parameters
    result <- list(
      kobs = params[2, 1],
      kobs_se = params[2, 2],
      v = params[1, 1],
      plateau = 100 * (params[1, 1] / params[2, 1]),
      nlm = nonlin_mod
    )
    # Add parameters to concentration list
    concentration_list[[i]] <- result

    # Predict concentration
    predictions <- predict_values(
      data = data,
      fitted_model = nonlin_mod,
      predict = "binding",
      x = "time",
      interval = 1,
      max = max(hits$time)
    )

    # Per-time-point SD from raw replicates (NA when no Replicate column)
    if ("Replicate" %in% names(raw_data) && !all(is.na(raw_data$Replicate))) {
      tp_sd <- raw_data |>
        dplyr::group_by(time) |>
        dplyr::summarise(
          binding_sd = stats::sd(binding, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      tp_sd <- data.frame(time = unique(raw_data$time), binding_sd = NA_real_)
    }

    # Append predictions to predictions data frame
    binding_table <- rbind(
      binding_table,
      dplyr::left_join(
        predictions,
        dplyr::select(data, c("time", "binding")),
        by = "time"
      ) |>
        dplyr::left_join(tp_sd, by = "time") |>
        dplyr::mutate(
          concentration = i,
          kobs = result$kobs,
          kobs_se = result$kobs_se
        )
    )

    # Add predictions specific for concentration
    concentration_list[[i]][["predictions"]] <- predictions

    # Save hits for concentration (averaged) and raw replicates for scatter plots
    concentration_list[[i]][["hits"]] <- data
    concentration_list[[i]][["hits_raw"]] <- raw_data

    # Log kobs result
    log_kobs_result(
      result = concentration_list[[i]],
      last = last,
      unit = units["Time"]
    )
  }

  # Reorder concentrations as factor
  concentration_list[["binding_table"]] <- binding_table |>
    dplyr::mutate(
      concentration = factor(
        concentration,
        levels = sort(
          as.numeric(unique(concentration)),
          decreasing = TRUE
        )
      )
    )

  return(concentration_list)
}

compute_ki_kinact <- function(kobs_result, units = units) {
  # Get kobs subset (one row per concentration; include SE if available)
  has_kobs_se <- "kobs_se" %in% names(kobs_result$binding_table)
  kobs <- kobs_result$binding_table |>
    dplyr::filter(!duplicated(kobs_result$binding_table$kobs)) |>
    dplyr::mutate(conc = as.numeric(as.character(concentration))) |>
    dplyr::select(conc, kobs, dplyr::any_of("kobs_se"))

  # Adjust start values to units
  if (units["Concentration"] == "M" & units["Time"] == "s") {
    start_values <- c(kinact = 0.001, KI = 0.000001)
  } else {
    start_values <- c(kinact = 1000, KI = 10)
  }

  # Add dummy row x,y = 0
  kobs_dummy <- kobs[1, ]
  kobs_dummy$kobs <- 0
  kobs_dummy$conc <- 0
  kobs <- rbind(kobs, kobs_dummy)
  kobs <- kobs[order(kobs$conc), ]

  # Nonlinear regression
  nonlin_mod <- nlsLM_fixed(
    formula = kobs ~ (kinact * conc) / (KI + conc),
    data = kobs,
    start = start_values
  )

  # Predict kobs values with NLM
  kobs_predicted <- predict_values(
    data = kobs,
    predict = "kobs",
    x = "conc",
    interval = 0.1,
    fitted_model = nonlin_mod
  )

  # Join with true data
  kobs_data <- dplyr::full_join(kobs_predicted, kobs, by = "conc")

  # Return complete list
  ki_kinact_result <- list(
    "Params" = summary(nonlin_mod)$parameters,
    "Kobs_Data" = kobs_data
  )

  return(ki_kinact_result)
}

# Modified minpack.lm::nlsLM() function due to namespace issues with stats::model.frame()
nlsLM_fixed <- function(
  formula,
  data = base::parent.frame(),
  start,
  jac = NULL,
  algorithm = "LM",
  control = minpack.lm::nls.lm.control(),
  lower = NULL,
  upper = NULL,
  trace = FALSE,
  subset,
  weights,
  na.action,
  model = FALSE,
  ...
) {
  formula <- stats::as.formula(formula)
  if (!base::is.list(data) && !base::is.environment(data)) {
    base::stop("'data' must be a list or an environment")
  }
  mf <- base::match.call()
  varNames <- base::all.vars(formula)
  if (base::length(formula) == 2L) {
    formula[[3L]] <- formula[[2L]]
    formula[[2L]] <- 0
  }
  form2 <- formula
  form2[[2L]] <- 0
  varNamesRHS <- base::all.vars(form2)
  mWeights <- base::missing(weights)
  if (trace) {
    control$nprint <- 1
  }
  pnames <- if (base::missing(start)) {
    if (!base::is.null(base::attr(data, "parameters"))) {
      base::names(base::attr(data, "parameters"))
    } else {
      cll <- formula[[base::length(formula)]]
      func <- base::get(base::as.character(cll[[1L]]))
      if (!base::is.null(pn <- base::attr(func, "pnames"))) {
        base::as.character(base::as.list(base::match.call(
          func,
          call = cll
        ))[-1L][pn])
      }
    }
  } else {
    base::names(start)
  }
  env <- base::environment(formula)
  if (base::is.null(env)) {
    env <- base::parent.frame()
  }
  if (base::length(pnames)) {
    varNames <- varNames[base::is.na(base::match(varNames, pnames))]
  }
  lenVar <- function(var) {
    base::tryCatch(
      base::length(base::eval(base::as.name(var), data, env)),
      error = function(e) -1
    )
  }
  if (base::length(varNames)) {
    n <- base::sapply(varNames, lenVar)
    if (base::any(not.there <- n == -1)) {
      nnn <- base::names(n[not.there])
      if (base::missing(start)) {
        base::warning(
          "No starting values specified for some parameters.\n",
          "Initializing ",
          base::paste(base::sQuote(nnn), collapse = ", "),
          " to '1.'.\n",
          "Consider specifying 'start' or using a selfStart model"
        )
        start <- base::as.list(base::rep(1, base::length(nnn)))
        base::names(start) <- nnn
        varNames <- varNames[i <- base::is.na(base::match(varNames, nnn))]
        n <- n[i]
      } else {
        base::stop(
          "parameters without starting value in 'data': ",
          base::paste(nnn, collapse = ", ")
        )
      }
    }
  } else {
    if (
      base::length(pnames) &&
        base::any((np <- base::sapply(pnames, lenVar)) == -1)
    ) {
      base::message(
        "fitting parameters ",
        base::paste(base::sQuote(pnames[np == -1]), collapse = ", "),
        " without any variables"
      )
      n <- base::integer()
    } else {
      base::stop("no parameters to fit")
    }
  }
  respLength <- base::length(base::eval(formula[[2L]], data, env))
  if (base::length(n) > 0L) {
    varIndex <- n %% respLength == 0
    if (
      base::is.list(data) &&
        base::diff(base::range(n[base::names(n) %in% base::names(data)])) > 0
    ) {
      mf <- data
      if (!base::missing(subset)) {
        base::warning("argument 'subset' will be ignored")
      }
      if (!base::missing(na.action)) {
        base::warning("argument 'na.action' will be ignored")
      }
      if (base::missing(start)) {
        start <- stats::getInitial(formula, mf)
      }
      startEnv <- base::new.env(
        hash = FALSE,
        parent = base::environment(formula)
      )
      for (i in base::names(start)) {
        base::assign(i, start[[i]], envir = startEnv)
      }
      rhs <- base::eval(formula[[3L]], data, startEnv)
      n <- base::NROW(rhs)
      wts <- if (mWeights) {
        base::rep(1, n)
      } else {
        base::eval(
          base::substitute(weights),
          data,
          base::environment(formula)
        )
      }
    } else {
      mf$formula <- stats::as.formula(
        base::paste("~", base::paste(varNames[varIndex], collapse = "+")),
        env = base::environment(formula)
      )
      mf$start <- mf$control <- mf$algorithm <- mf$trace <- mf$model <- NULL
      mf$lower <- mf$upper <- NULL

      # CHANGE FROM ORIGINAL
      # Using quote(stats::model.frame) to fix the scoping issue
      mf[[1L]] <- quote(stats::model.frame)
      mf <- base::eval.parent(mf)

      n <- base::nrow(mf)
      mf <- base::as.list(mf)
      wts <- if (!mWeights) {
        stats::model.weights(mf)
      } else {
        base::rep(1, n)
      }
    }
    if (base::any(wts < 0 | base::is.na(wts))) {
      base::stop("missing or negative weights not allowed")
    }
  } else {
    varIndex <- base::logical()
    mf <- base::list(0)
    wts <- base::numeric()
  }
  if (base::missing(start)) {
    start <- stats::getInitial(formula, mf)
  }
  for (var in varNames[!varIndex]) {
    mf[[var]] <- base::eval(base::as.name(var), data, env)
  }
  varNamesRHS <- varNamesRHS[varNamesRHS %in% varNames[varIndex]]
  mf <- base::c(mf, start)
  lhs <- base::eval(formula[[2L]], envir = mf)
  m <- base::match(base::names(start), base::names(mf))
  .swts <- if (!base::missing(wts) && base::length(wts)) {
    base::sqrt(wts)
  }
  FCT <- function(par) {
    mf[m] <- par
    rhs <- base::eval(formula[[3L]], envir = mf, base::environment(formula))
    res <- lhs - rhs
    res <- .swts * res
    res
  }
  NLS <- minpack.lm::nls.lm(
    par = start,
    fn = FCT,
    jac = jac,
    control = control,
    lower = lower,
    upper = upper,
    ...
  )
  start <- NLS$par
  m <- minpack.lm:::nlsModel(formula, mf, start, wts)
  if (NLS$info %in% base::c(1, 2, 3, 4)) {
    isConv <- TRUE
  } else {
    isConv <- FALSE
  }
  finIter <- NLS$niter
  finTol <- minpack.lm::nls.lm.control()$ftol
  convInfo <- base::list(
    isConv = isConv,
    finIter = finIter,
    finTol = finTol,
    stopCode = NLS$info,
    stopMessage = NLS$message
  )
  nls.out <- base::list(
    m = m,
    convInfo = convInfo,
    data = base::substitute(data),
    call = base::match.call()
  )
  nls.out$call$algorithm <- algorithm
  nls.out$call$control <- stats::nls.control()
  nls.out$call$trace <- FALSE
  nls.out$call$lower <- lower
  nls.out$call$upper <- upper
  nls.out$na.action <- base::attr(mf, "na.action")
  nls.out$dataClasses <- base::attr(base::attr(mf, "terms"), "dataClasses")[
    varNamesRHS
  ]
  if (model) {
    nls.out$model <- mf
  }
  if (!mWeights) {
    nls.out$weights <- wts
  }
  nls.out$control <- control
  base::class(nls.out) <- "nls"
  nls.out
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

# Generate spectrum with multiple traces
#' @export
multiple_spectra <- function(
  results_list,
  samples,
  cubic = TRUE,
  labels_show = NULL,
  time = FALSE,
  color_cmp = NULL,
  truncated = FALSE,
  color_variable = NULL,
  hits_summary = NULL,
  units = NULL,
  theme = "dark"
) {
  # Omit NA in samples
  samples <- samples[!is.na(samples)]

  # Get spectrum data
  spectrum_data <- data.frame()
  for (i in seq_along(samples)) {
    add_df <- process_plot_data(
      results_list$deconvolution[[samples[i]]],
      result_path = NULL
    )$mass

    if (time) {
      add_df <- dplyr::mutate(add_df, z = extract_minutes(samples[i]))
    } else {
      add_df <- dplyr::mutate(add_df, z = samples[i])
    }

    spectrum_data <- rbind(spectrum_data, add_df)
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

  # Get peaks data
  peaks_data <- data.frame()
  for (i in seq_along(samples)) {
    add_df <- process_plot_data(
      results_list$deconvolution[[samples[i]]],
      result_path = NULL
    )$highlight_peaks

    if (time) {
      add_df <- dplyr::mutate(add_df, z = extract_minutes(samples[i]))
    } else {
      add_df <- dplyr::mutate(add_df, z = samples[i])
    }

    peaks_data <- rbind(peaks_data, add_df)
  }

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

      # Declare coloring variables for graph elements
      color <- NULL
      line <- list(color = font_color, width = 1)
      z_linecolor <- list(color = font_color)
    } else if (color_variable == "Samples") {
      # Match colors to peaks and spectrum data
      peaks_data$z_color <- color_cmp[match(peaks_data$z, names(color_cmp))]
      spectrum_data$z_color <- color_cmp[match(
        spectrum_data$z,
        names(color_cmp)
      )]

      # Declare coloring variables for graph elements
      color <- ~ I(z_color)
      line <- list(width = 1)
      marker_color <- ~ I(z_color)
      z_linecolor <- NULL
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
    marker_color <- ~ I(color)

    # Match colors to spectrum data
    spectrum_data$z_color <- color_cmp[match(
      spectrum_data$z,
      names(color_cmp)
    )]

    # Declare coloring variables for graph elements
    color <- ~ I(z_color)
    line <- list(width = 1)
    z_linecolor <- NULL
  }

  # Condition on data size
  if (is.null(labels_show)) {
    labels_show <- (length(unique(peaks_data$z)) <= 8 &
      max(nchar(as.character(peaks_data$z))) <= 20) |
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

    # Add hit markers
    if (nrow(peaks_data) > 0) {
      if (time) {
        marker_list <- list(
          size = 5,
          zindex = 100,
          line = list(color = font_color, width = 3)
        )
      } else {
        marker_list <- list(
          color = marker_color,
          symbol = ~ I(symbol),
          size = 5,
          zindex = 100,
          line = list(color = font_color, width = 3)
        )
      }

      plot <- plot |>
        plotly::add_markers(
          data = peaks_data,
          x = ~mass,
          y = ~intensity,
          z = ~z,
          split = ~ seq_len(nrow(peaks_data)),
          legendgroup = ~z,
          color = marker_color,
          symbol = ~ I(symbol),
          mode = "markers",
          inherit = FALSE,
          marker = marker_list,
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

    plot |>
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(size = 14, color = font_color),
        legend = list(
          bgcolor = "rgba(0,0,0,0)",
          bordercolor = "rgba(0,0,0,0)",
          font = list(color = font_color),
          title = list(
            text = paste(
              "<b>",
              ifelse(
                time,
                paste0(
                  "Time [",
                  gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
                  "]  "
                ),
                "Sample ID"
              ),
              "</b>"
            ),
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
              x = 0.33,
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

    plotly::plot_ly(
      data = spectrum_data,
      x = ~mass,
      y = ~intensity,
      color = ~z,
      colors = planar_colors,
      legendgroup = ~z,
      type = "scatter",
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
    ) |>
      plotly::add_markers(
        data = dplyr::mutate(
          peaks_data,
          symbol = paste0(symbol, "-open")
        ),
        x = ~mass,
        y = ~intensity,
        split = ~ interaction(z, name),
        legendgroup = ~z,
        mode = "markers",
        color = marker_color,
        symbol = ~ I(symbol),
        inherit = FALSE,
        marker = c(
          list(
            size = 10,
            zindex = 100,
            line = list(color = font_color, width = 1.5)
          ),
          if (is.null(color_variable)) list(color = font_color) else list()
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
      ) |>
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(size = 14, color = font_color),
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
        legend = list(
          bgcolor = "rgba(0,0,0,0)",
          bordercolor = "rgba(0,0,0,0)",
          font = list(color = font_color),
          title = list(
            text = paste(
              "<b>",
              ifelse(
                time,
                paste0(
                  "Time [",
                  gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
                  "]  "
                ),
                "Sample ID"
              ),
              "</b>"
            ),
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
      `Binding [%]` = {
        pref <- `Binding [%]`[Preferred == "TRUE"]
        if (length(pref) > 0) pref[1] else `Binding [%]`[1]
      },
      `Tot. Binding [%]` = `Tot. Binding [%]`[1]
    ) |>
    dplyr::select(-`Peak Signal [Da]`)

  # Prepare data frame for table
  tbl <- table |>
    dplyr::ungroup() |>
    dplyr::select(
      `Sample ID` = `Sample ID`,
      `Cmp Name` = `Cmp Name`,
      dplyr::any_of(optional_cols),
      `Mass Shift` = mass_stoich_html,
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
      col_var = !!rlang::sym(
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

  # Add human-readable prefix to group rows (after truncation so it isn't overwritten)
  if (!is.null(row_group)) {
    if (tab == "Compounds") {
      table$`Sample ID` <- paste("Sample:", table$`Sample ID`)
    } else if (any(tab %in% c("Samples", "Proteins"))) {
      table$`Cmp Name` <- paste("Compound:", table$`Cmp Name`)
    }
  }
  if (!is.null(inputs$binding_bar) && !isTRUE(inputs$binding_bar)) {
    table[["Binding [%]"]] <- sprintf("%.2f", table[["Binding [%]"]])
  }
  if (!is.null(inputs$tot_binding_bar) && !isTRUE(inputs$tot_binding_bar)) {
    table[["Total %"]] <- sprintf("%.2f", table[["Total %"]])
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
            if (tab == "Concentration") "Cmp Name"
          )
        ),
        list(className = 'dt-center', targets = "_all"),
        list(
          targets = "Binding [%]",
          render = render_binding
        ),
        list(
          targets = "Total %",
          render = render_tot_binding
        ),
        list(
          targets = -1,
          className = 'dt-last-col'
        )
      )
    )
  ) |>
    DT::formatRound(
      columns = intersect(c("Binding [%]", "Total %"), names(table)),
      digits = 2
    ) |>
    DT::formatStyle(
      columns = "col_var",
      target = 'row',
      backgroundColor = DT::styleEqual(
        levels = if (
          length(units) == 2 && inputs$color_variable == units["Concentration"]
        ) {
          names(colors)
        } else {
          names(colors)
        },
        values = colors
      ),
      color = DT::styleEqual(
        levels = if (
          length(units) == 2 && inputs$color_variable == units["Concentration"]
        ) {
          names(colors)
        } else {
          names(colors)
        },
        values = get_contrast_color(colors)
      )
    )
}

# Selection and filtering of hits table
#' @export
filter_hits_table <- function(
  hits_table,
  selected_cols = NULL,
  compounds = NULL,
  samples = NULL,
  expand = TRUE,
  na_include = TRUE,
  units
) {
  # Modify if samples are summarized instead of expanded
  if (!expand) {
    if ("Replicate" %in% names(hits_table)) {
      hits_table <- hits_table |>
        dplyr::distinct(
          `Sample ID`,
          Replicate,
          `Protein`,
          `Cmp Name`,
          `Theor. Prot. [Da]`,
          `Tot. Binding [%]`,
          `truncSample_ID`
        )
    } else {
      hits_table <- hits_table |>
        dplyr::distinct(
          `Sample ID`,
          `Protein`,
          `Cmp Name`,
          `Theor. Prot. [Da]`,
          `Tot. Binding [%]`,
          `truncSample_ID`
        )
    }
  }

  # Filter compounds
  if (!is.null(compounds) && length(compounds) > 0 && na_include) {
    hits_table <- dplyr::filter(
      hits_table,
      `Cmp Name` %in% compounds | is.na(`Cmp Name`)
    )
  } else if (!is.null(compounds) && length(compounds) > 0) {
    hits_table <- dplyr::filter(
      hits_table,
      `Cmp Name` %in% compounds
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
        "  for(var i=0; i<data.length; i++){",
        "    if(data[i] === null){",
        "      $('td:eq('+i+')', row).html('N/A').css({'color': 'inherit'});",
        "    }",
        "    if(targets.includes(i) && data[i] !== null){",
        "      var isValid = validConc === null || validConc.includes(parseFloat(data[i]));",
        "      if(isValid) $('td:eq('+i+')', row).addClass('clickable-column');",
        "    }",
        "  }",
        "}"
      )
    } else {
      clickable_targets <- NULL
    }
  }

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
        list(className = 'dt-left', targets = "_all"),
        if (length(bar_chart) > 0 & any(bar_chart %in% names(hits_table))) {
          list(
            targets = bar_chart[bar_chart %in% names(hits_table)],
            render = htmlwidgets::JS(chart_js),
            type = "num"
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
        list(
          targets = -1,
          className = 'dt-last-col'
        )
      )
    )
  )

  # Format binding columns to consistent 2 decimal places
  binding_fmt_cols <- intersect(
    c("Binding [%]", "Tot. Binding [%]", "Unmatched [%]", "Correct [%]"),
    names(hits_table)
  )
  if (length(binding_fmt_cols) > 0) {
    hits_datatable <- DT::formatRound(
      hits_datatable,
      columns = binding_fmt_cols,
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
  } else {
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
checkboxColumn <- function(len, col, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(shiny::checkboxInput(
      paste0("checkb_", col, "_", i),
      label = NULL,
      ...
    ))
  }
  inputs
}

# Compute replicate group labels for a vector of sample names.
# Priority: (1) config Replicate column, (2) _R<n> filename suffix detection,
# (3) unique _R<n> placeholders for samples without a detected group.
#' @export
compute_replicate_labels <- function(sample_names, config = NULL) {
  labels <- rep(NA_character_, length(sample_names))
  config_mode <- FALSE

  # Priority 1: config supplies at least one non-empty Replicate value
  if (!is.null(config) && "Replicate" %in% names(config)) {
    cfg_key <- gsub("\\.raw$", "", config$Sample, ignore.case = TRUE)
    samp_key <- gsub("\\.raw$", "", sample_names, ignore.case = TRUE)
    matched <- config$Replicate[match(samp_key, cfg_key)]
    non_empty <- !is.na(matched) & trimws(matched) != ""
    if (any(non_empty)) {
      config_mode <- TRUE
      labels[non_empty] <- trimws(matched[non_empty])
    }
  }

  # Priority 2: filename suffix detection (_R<n> before optional .raw)
  if (!config_mode) {
    has_rn <- grepl("_[Rr]\\d+(\\.raw)?$", sample_names)
    base_names <- gsub("_[Rr]\\d+(\\.raw)?$", "", sample_names)
    base_names <- gsub("\\.raw$", "", base_names, ignore.case = TRUE)
    for (base in unique(base_names[has_rn])) {
      idx <- which(has_rn & base_names == base)
      if (length(idx) >= 2L) labels[idx] <- base
    }
  }

  # Fill remaining NAs with R<n>, avoiding clashes with existing R<n> labels
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
  ki_kinact = FALSE
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

  if (!is.null(ki_kinact) && ki_kinact) {
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
    if (isTRUE(ki_kinact)) c("Concentration", "Time")
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
      # Format Intensity columns
      dplyr::across(
        dplyr::any_of(c("Intensity", "Protein Intensity")) & where(is.numeric),
        ~ round(.x, 2)
      ),
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

# Adjust brightness
brighten_hex <- function(hex_colors, factor = 1.2) {
  # Convert Hex to HSV
  rgb_vals <- grDevices::col2rgb(hex_colors)
  hsv_vals <- grDevices::rgb2hsv(rgb_vals)

  # Multiply the 'Value' (brightness) channel
  # We use pmin to ensure we don't exceed the maximum value of 1
  hsv_vals[3, ] <- pmin(hsv_vals[3, ] * factor, 1)

  # Convert back to Hex
  grDevices::hsv(hsv_vals[1, ], hsv_vals[2, ], hsv_vals[3, ])
}

# Make uniform color scale for compounds
#' @export
get_cmp_colorScale <- function(filtered_table, scale, variable, trunc) {
  if (variable == "Compounds") {
    # cmp_levels <- unique(filtered_table[["Theor. Cmp"]])
    cmp_levels <- unique(filtered_table[["Cmp Name"]])
  } else if (variable == "Samples") {
    if (trunc) {
      cmp_levels <- unique(filtered_table[["truncSample_ID"]])
    } else {
      cmp_levels <- unique(filtered_table[["Sample ID"]])
    }
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
        # Handle n < 3 (Brewer minimum request is 3)
        n_request <- max(n, 3)
        raw_colors <- RColorBrewer::brewer.pal(n_request, scale)

        # Apply custom subsetting for contrast at low n
        if (n == 2) {
          colors <- raw_colors[c(1, 2)]
        } else if (n == 1) {
          colors <- raw_colors[1]
        } else {
          colors <- raw_colors[1:n]
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

  return(filtered_list)
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

  colors <- get_cmp_colorScale(
    filtered_table = tbl,
    scale = color_scale,
    variable = color_variable,
    trunc = truncate_names
  )

  if (color_variable == "Compounds") {
    color <- ~`Cmp Name`
  } else if (color_variable == "Samples") {
    color <- ~`Sample ID`
  }

  # Merge non-preferred hits (same peak) into their preferred counterpart
  tbl <- tbl |>
    dplyr::arrange(
      `Cmp Name`,
      `Sample ID`,
      `Peak Signal [Da]`,
      dplyr::desc(Preferred == "TRUE"),
      dplyr::desc(suppressWarnings(as.numeric(`Theor. Cmp [Da]`)))
    ) |>
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
    dplyr::select(-`Peak Signal [Da]`)

  tbl <- tbl |>
    dplyr::group_by(`Cmp Name`) |>
    dplyr::mutate(
      `Sample ID` = if (truncate_names) {
        `truncSample_ID`
      } else {
        `Sample ID`
      },
      Group = match(`Sample ID`, unique(`Sample ID`)),
      `Cmp Name` = factor(`Cmp Name`, levels = unique(`Cmp Name`)),
      `Sample ID` = factor(
        `Sample ID`,
        levels = unique(`Sample ID`)
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
        tickson = "boundaries",
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

    bar_width <- min(0.3, 0.85 / (n_groups + max(0, n_groups - 1) * 0.15))
    group_gap <- bar_width * 0.15

    compound_local_n <- tbl |>
      dplyr::group_by(`Cmp Name`) |>
      dplyr::summarize(local_n = dplyr::n_distinct(Group), .groups = "drop")
    compound_local_n_map <- stats::setNames(
      compound_local_n$local_n,
      compound_local_n$`Cmp Name`
    )

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
      off <- -local_cluster_width / 2 + i_group * (bar_width + group_gap)

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
        offsetgroup = i_group,
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
          line = list(color = row$label_color[[1]], width = 1)
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
    annots <- vector("list", nrow(totals_cmp_grp))
    for (j in seq_len(nrow(totals_cmp_grp))) {
      tot_row <- totals_cmp_grp[j, ]
      g <- tot_row$Group[[1]]
      i_group <- group_map[[g]]
      local_n <- compound_local_n_map[[as.character(tot_row$`Cmp Name`[[1]])]]
      local_cluster_width <- local_n *
        bar_width +
        max(0, local_n - 1) * group_gap
      off <- -local_cluster_width / 2 + i_group * (bar_width + group_gap)
      cmp_idx <- which(cmp_levels == as.character(tot_row$`Cmp Name`[[1]])) - 1L
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
        marker = list(line = list(color = ~label_color, width = 1)),
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
      marker = list(line = list(color = ~label_color, width = 1)),
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
        showticklabels = TRUE
        #   ifelse(
        #   !is.null(distribution_labels),
        #   distribution_labels,
        #   max(nchar(levels(tbl$`Sample ID`))) <= 22 | nrow(tbl) < 4
        # )
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
    } else {
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
  color_scale = "plasma"
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

  pal <- stats_palette(2, color_scale)
  col1 <- hex_to_rgba(pal[1], 0.7)
  col1b <- hex_to_rgba(pal[1], 1)
  col2 <- hex_to_rgba(pal[2], 0.7)
  col2b <- hex_to_rgba(pal[2], 1)

  plotly::plot_ly() |>
    plotly::add_histogram(
      data = df,
      x = ~`% Correct`,
      name = "Correct [%]",
      marker = list(color = col1, line = list(color = col1b, width = 0.5)),
      xbins = list(start = 0, end = 100, size = 1)
    ) |>
    plotly::add_histogram(
      data = df,
      x = ~`% Unmatched`,
      name = "Unmatched [%]",
      marker = list(color = col2, line = list(color = col2b, width = 0.5)),
      xbins = list(start = 0, end = 100, size = 1)
    ) |>
    plotly::layout(
      barmode = "overlay",
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(size = 14, color = font_color),
      legend = list(
        bgcolor = "rgba(0,0,0,0)",
        font = list(color = font_color)
      ),
      xaxis = list(
        title = "Value [%]",
        range = c(-2, 102),
        color = font_color,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color,
        hoverformat = ".2f"
      ),
      yaxis = list(
        title = "Count",
        color = font_color,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color,
        hoverformat = ".2f"
      )
    )
}

#' @export
stats_boxplot <- function(
  hits_summary,
  theme = "dark",
  color_scale = "plasma",
  show_points = TRUE
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

  pal <- stats_palette(2, color_scale)
  col1 <- hex_to_rgba(pal[1], 0.7)
  col1b <- hex_to_rgba(pal[1], 1)
  col1f <- hex_to_rgba(pal[1], 0.15)
  col2 <- hex_to_rgba(pal[2], 0.7)
  col2b <- hex_to_rgba(pal[2], 1)
  col2f <- hex_to_rgba(pal[2], 0.15)

  p <- plotly::plot_ly() |>
    # Box shape — no points, default box hover shows statistics
    plotly::add_trace(
      data = df,
      y = ~`% Correct`,
      name = "Correct [%]",
      type = "box",
      boxpoints = FALSE,
      boxmean = TRUE,
      line = list(color = col1b),
      fillcolor = col1f,
      showlegend = TRUE
    ) |>
    plotly::add_trace(
      data = df,
      y = ~`% Unmatched`,
      name = "Unmatched [%]",
      type = "box",
      boxpoints = FALSE,
      boxmean = TRUE,
      line = list(color = col2b),
      fillcolor = col2f,
      showlegend = TRUE
    )

  if (show_points) {
    p <- p |>
      # Points-only overlay: invisible box shape on top so points capture hover
      plotly::add_trace(
        data = df,
        y = ~`% Correct`,
        text = ~Sample,
        name = "Correct [%]",
        type = "box",
        boxpoints = "all",
        jitter = 0.4,
        pointpos = 0,
        hoveron = "points",
        hovertemplate = "<b>%{text}</b><br>Correct [%]: %{y:.2f}<extra></extra>",
        whiskerwidth = 0,
        line = list(color = "rgba(0,0,0,0)", width = 0),
        fillcolor = "rgba(0,0,0,0)",
        showlegend = FALSE,
        marker = list(
          color = "rgba(0,0,0,0)",
          size = 8,
          line = list(color = dot_border_color, width = 1.5)
        )
      ) |>
      plotly::add_trace(
        data = df,
        y = ~`% Unmatched`,
        text = ~Sample,
        name = "Unmatched [%]",
        type = "box",
        boxpoints = "all",
        jitter = 0.4,
        pointpos = 0,
        hoveron = "points",
        hovertemplate = "<b>%{text}</b><br>Unmatched [%]: %{y:.2f}<extra></extra>",
        whiskerwidth = 0,
        line = list(color = "rgba(0,0,0,0)", width = 0),
        fillcolor = "rgba(0,0,0,0)",
        showlegend = FALSE,
        marker = list(
          color = "rgba(0,0,0,0)",
          size = 8,
          line = list(color = dot_border_color, width = 1.5)
        )
      )
  }

  correct_sd_bp <- stats::sd(df$`% Correct`, na.rm = TRUE)
  unmatched_sd_bp <- stats::sd(df$`% Unmatched`, na.rm = TRUE)
  correct_mean_bp <- mean(df$`% Correct`, na.rm = TRUE)
  unmatched_mean_bp <- mean(df$`% Unmatched`, na.rm = TRUE)

  bp_annots <- list()
  if (!is.na(correct_sd_bp) && correct_sd_bp < 0.005) {
    bp_annots <- c(
      bp_annots,
      list(list(
        x = "Correct [%]",
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
  if (!is.na(unmatched_sd_bp) && unmatched_sd_bp < 0.005) {
    bp_annots <- c(
      bp_annots,
      list(list(
        x = "Unmatched [%]",
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
        color = font_color,
        gridcolor = "rgba(0,0,0,0)",
        zerolinecolor = "rgba(0,0,0,0)"
      ),
      yaxis = list(
        title = "Value [%]",
        range = c(-5, 105),
        color = font_color,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color,
        hoverformat = ".2f"
      ),
      annotations = if (length(bp_annots) > 0) bp_annots else NULL
    )
}

#' @export
stats_scatter <- function(
  hits_summary,
  full_scale = FALSE,
  group_by = NULL,
  color_scale = "plasma",
  theme = "dark"
) {
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

  correct_vals <- df$`% Correct`
  correct_mean <- mean(correct_vals, na.rm = TRUE)
  correct_sd <- stats::sd(correct_vals, na.rm = TRUE)
  correct_upper <- correct_mean + correct_sd
  correct_lower <- correct_mean - correct_sd
  show_sd <- !is.na(correct_sd) && correct_sd >= 0.005

  y_range <- if (full_scale) {
    c(-5, 105)
  } else {
    pad <- max(
      (max(correct_vals, na.rm = TRUE) - min(correct_vals, na.rm = TRUE)) *
        0.08,
      3
    )
    c(
      max(-5, floor(min(correct_vals, na.rm = TRUE) - pad)),
      min(105, ceiling(max(correct_vals, na.rm = TRUE) + pad))
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
        y_plot = `% Correct` + stats::runif(dplyr::n(), -0.6, 0.6),
        tooltip = paste0(
          Sample,
          "<br>",
          "Tot. Binding [%]: ",
          round(`Total % Binding`, 2),
          "<br>",
          "Correct [%]: ",
          round(`% Correct`, 2)
        )
      )
    p <- plotly::add_markers(
      p,
      data = sub_df,
      x = ~x_plot,
      y = ~y_plot,
      name = grp,
      showlegend = TRUE,
      customdata = ~tooltip,
      hovertemplate = "%{customdata}<extra></extra>",
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
        title = "Correct [%]",
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
  inner = "box"
) {
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
  for (grp in groups) {
    sub_df <- dplyr::filter(df, .data[[group_by]] == grp)
    col <- hex_to_rgba(color_map[[grp]], 1)
    col_f <- hex_to_rgba(color_map[[grp]], 0.2)
    col_b <- hex_to_rgba(color_map[[grp]], 0.25)
    grp_sd <- stats::sd(sub_df$`% Correct`, na.rm = TRUE)
    is_degenerate <- is.na(grp_sd) || grp_sd < 0.005
    if (is_degenerate) {
      # Degenerate group: strip chart (invisible box carrying jittered points)
      p <- plotly::add_trace(
        p,
        data = sub_df,
        type = "box",
        x = grp,
        y = ~`% Correct`,
        name = grp,
        text = ~Sample,
        boxpoints = "all",
        jitter = 0.5,
        pointpos = 0,
        hoveron = "points",
        hovertemplate = "<b>%{text}</b><br>Correct [%]: %{y:.2f}<extra></extra>",
        whiskerwidth = 0,
        line = list(color = "rgba(0,0,0,0)", width = 0),
        fillcolor = "rgba(0,0,0,0)",
        showlegend = FALSE,
        marker = list(
          color = col,
          size = 8,
          opacity = 0.9,
          line = list(color = dot_border_color, width = 1)
        )
      )
    } else {
      p <- suppressWarnings(plotly::add_trace(
        p,
        data = sub_df,
        type = "violin",
        x = grp,
        y = ~`% Correct`,
        name = grp,
        fillcolor = col_f,
        line = list(color = col, width = 1),
        box = list(
          visible = show_box,
          fillcolor = col_b,
          line = list(color = box_line_color, width = 1)
        ),
        meanline = list(visible = show_box, color = box_line_color, width = 1),
        points = if (show_points) "all" else FALSE,
        pointpos = 0,
        jitter = 0.3,
        marker = list(
          color = "rgba(0,0,0,0)",
          size = 7,
          line = list(color = dot_border_color, width = 1)
        ),
        showlegend = FALSE
      ))
    }
  }

  p |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(size = 14, color = font_color),
      xaxis = list(
        title = group_by,
        color = font_color,
        gridcolor = grid_color,
        zerolinecolor = zeroline_color
      ),
      yaxis = list(
        title = "Correct [%]",
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
  hits_summary3 <<- hits_summary

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

  # Classify each sample's well state
  no_prot_flag <- is.na(hits_summary$`Measured Mw Protein [Da]`)
  no_hit_flag <- !is.na(hits_summary$`Measured Mw Protein [Da]`) &
    is.na(hits_summary$Compound)
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
      sprintf("%.1f%%", values)
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

  p |>
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
}
