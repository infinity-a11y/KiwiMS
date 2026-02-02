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

# Helper function to process uploaded table
#' @export
process_uploaded_table <- function(df, type) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  expected_cols <- if (type == "protein") {
    c("Protein", paste("Mass", 1:9))
  } else {
    c("Compound", paste("Mass", 1:9))
  }

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

  # Convert mass columns to numeric
  mass_cols <- paste("Mass", 1:9)
  for (col in mass_cols) {
    if (col %in% colnames(df)) {
      original <- df[[col]]
      numeric_vals <- suppressWarnings(as.numeric(original))
      if (any(is.na(numeric_vals) & !is.na(original))) {
        shinyWidgets::show_toast(
          "Conversion error",
          text = paste(
            "Column",
            col,
            "contains non-numeric values that cannot be converted."
          ),
          type = "error",
          timer = 5000
        )
        return(NULL)
      }
      df[[col]] <- numeric_vals
    }
  }

  return(df)
}

# Helper function to read uploaded files
#' @export
read_uploaded_file <- function(file_path, ext, has_header) {
  if (ext %in% c("csv", "txt")) {
    df <- utils::read.csv(
      file_path,
      stringsAsFactors = FALSE,
      header = has_header
    )
  } else if (ext == "tsv") {
    df <- readr::read_tsv(
      file_path,
      col_names = has_header,
      show_col_types = FALSE
    )
  } else if (ext == "tsv") {
    df <- utils::read.delim(
      file_path,
      stringsAsFactors = FALSE,
      header = has_header
    )
  } else if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(file_path, col_names = has_header)
  } else {
    stop("Unsupported file format")
  }
  # Ensure column names are standardized
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
    height = 28 + 23 * ifelse(nrow(tab > 16), 16, nrow(tab)),
    stretchH = ifelse(disabled, "none", "all")
  ) |>
    rhandsontable::hot_cols(fixedColumnsLeft = 1, renderer = renderer_js) |>
    rhandsontable::hot_context_menu(
      allowRowEdit = ifelse(disabled, FALSE, TRUE),
      allowColEdit = FALSE
    ) |>
    rhandsontable::hot_cols(
      cols = 2:ncol(tab),
      format = "0.000"
    ) |>
    rhandsontable::hot_validate_numeric(
      cols = 2:ncol(tab),
      min = 1,
      allowInvalid = TRUE
    ) |>
    rhandsontable::hot_table(
      contextMenu = ifelse(disabled, FALSE, TRUE),
      highlightCol = TRUE,
      highlightRow = TRUE,
      stretchH = ifelse(disabled, "none", "all")
    ) |>
    htmlwidgets::onRender(paste_hook_js)

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

  # Allowed protein and compound values
  if (!is.null(proteins) && !is.null(compounds)) {
    allowed_per_col <- list(
      NULL,
      proteins,
      compounds
    )

    # Custom renderer
    renderer_js <- "function(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    
    td.style.background = ''; // Clear existing background for new rendering
    
    var allowedPerCol = instance.params ? instance.params.allowed_per_col : null;
    var normalizedValue = value == null ? '' : String(value).trim();
    
    var allowedRaw;
    if (col === 1) {
      allowedRaw = allowedPerCol ? allowedPerCol[1] : null; 
    } else if (col >= 2) {
      allowedRaw = allowedPerCol ? allowedPerCol[2] : null; 
    } else {
      return;
    }
    
    // --- 1. Prepare allowed list (same as before) ---
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
    var isDuplicated = false;
    if (normalizedValue !== '') {
      var rowData = instance.getDataAtRow(row);
      var valueCounts = {};
      var startCol = 1; // Start from column 1 (Protein) to exclude 'Sample' (col 0)
      
      for (var i = startCol; i < rowData.length; i++) {
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
      td.style.background = 'red'; // Invalid content takes precedence
    } else if (isDuplicated) {
      td.style.background = 'orange'; // Duplicated content
    }
  }"
  } else {
    allowed_per_col <- list(NULL)
    renderer_js <- ""
  }

  handsontable <- rhandsontable::rhandsontable(
    tab,
    rowHeaders = NULL,
    allowed_per_col = allowed_per_col,
    height = 28 + 23 * ifelse(nrow(tab > 16), 16, nrow(tab)),
    stretchH = "all"
  ) |>
    rhandsontable::hot_cols(
      fixedColumnsLeft = 2,
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
      contextMenu = ifelse(disabled, FALSE, TRUE),
      stretchH = "all"
    )

  if (all(c("Concentration", "Time") %in% colnames(tab))) {
    handsontable <- rhandsontable::hot_col(
      handsontable,
      col = which(colnames(tab) %in% c("Concentration", "Time")),
      type = "numeric",
      allowInvalid = FALSE
    ) |>
      rhandsontable::hot_validate_numeric(
        cols = which(colnames(tab) %in% c("Concentration", "Time")),
        min = 0
      )
  }

  return(handsontable)
}

# Function to fill missing columns in sample table
#' @export
fill_sample_table <- function(sample_table, ki_kinact) {
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
  # ifelse(ki_kinact, 9, 7)
  if (col_diff != 0) {
    # Get concentration, time columns if ki_kinact active

    sample_table <- cbind(
      sample_table,
      (data.frame(rep(list(rep("", nrow(sample_table))), col_diff)))
    )

    if (ki_kinact) {
      sample_table <- cbind(sample_table, conc_time)
    }

    names(sample_table) <- c(
      "Sample",
      "Protein",
      paste("Compound", 1:5),
      if (ki_kinact) c("Concentration", "Time")
    )
  }

  return(sample_table)
}

# Construct cleaned-up sample table with only consecutive non-NA entries
#' @export
clean_sample_table <- function(sample_table, units = NULL) {
  has_conc_time <- all(c("Concentration", "Time") %in% names(sample_table))

  no_cmp_cols <- names(sample_table) %in%
    c(
      "Sample",
      "Protein",
      if (has_conc_time) {
        c("Concentration", "Time")
      }
    )

  extra_cmp_section <- sample_table[,
    which(!no_cmp_cols),
    drop = FALSE
  ]

  df <- extra_cmp_section[,
    colSums(is.na(extra_cmp_section) | extra_cmp_section == "") !=
      nrow(extra_cmp_section),
    drop = FALSE
  ]

  # Rebuild data frame with consecutive values
  if (ncol(extra_cmp_section)) {
    df <- data.frame()
    for (i in 1:nrow(extra_cmp_section)) {
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

  # Reattach sample, protein, compound columns
  df <- cbind(sample_table[, 1:2, drop = FALSE], df)
  if (has_conc_time) {
    df <- cbind(
      df,
      sample_table[, names(sample_table) %in% c("Concentration", "Time")]
    )
  }

  # Rename columns
  names(df) <- c(
    "Sample",
    "Protein",
    paste("Compound", 1:(ncol(df) - ifelse(has_conc_time, 4, 2))),
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
  has_conc_time <- all(c("Concentration", "Time") %in% names(sample_table))

  if (has_conc_time) {
    conc_time_tbl <- sample_table[,
      names(sample_table) %in% c("Concentration", "Time")
    ]
    sample_table <- sample_table[,
      !names(sample_table) %in% c("Concentration", "Time")
    ]
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
    return("Protein name not found")
  }

  # Check if compound names valid
  compounds_input <- sample_table[, -(1:2)][
    !is.na(sample_table[, -(1:2)]) & sample_table[, -(1:2)] != ""
  ]
  if (length(compounds_input) & any(!compounds_input %in% compounds)) {
    return("Compound name not found")
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
    # Check for correct concentration input
    if (any(is.na(conc_time_tbl$Concentration))) {
      return("Fill Concentrations")
    }

    # Check for correct time input
    if (any(is.na(conc_time_tbl$Time))) {
      return("Fill Time")
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
  log_status(nrow(peaks), min(peaks$mass), max(peaks$mass))

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
  sample
) {
  # Find protein peak
  protein_peak <- peaks$mass >= protein_mw[, -1] - peak_tolerance &
    peaks$mass <= protein_mw[, -1] + peak_tolerance

  # Abort if peaks show invalid peaks
  if (!any(protein_peak)) {
    log_alert()

    hits_df <- data.frame(
      well = "A1",
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
      multiple = NA
    )

    return(hits_df)
  }

  # Keep only peaks above protein mw
  peaks_valid <- peaks$mass >= protein_mw[, -1] - peak_tolerance
  if (any(peaks_valid) && sum(peaks_valid) > 1) {
    peaks_filtered <- as.data.frame(peaks[peaks_valid, ])
  } else {
    log_alert()

    hits_df <- data.frame(
      well = "A1",
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
      multiple = NA
    )

    return(hits_df)
  }

  # Transform compounds to matrix
  cmp_mat <- as.matrix(compound_mw[, -1])
  colnames(cmp_mat) <- compound_mw[, 1]

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

      for (k in 1:nrow(indices)) {
        # Retrieve compound mass from hit on complex
        multiple <- as.integer(sub(".*\\*", "", colnames(hits)[indices[k, 2]]))
        cmp_mass <- mat[
          indices[k, 1],
          indices[k, 2] - (ncol(hits) / max_multiples) * (multiple - 1)
        ]

        # Construct new entry for hits_df data frame
        hits_add <- data.frame(
          well = "A1",
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
          compound = sub("\\*.*", "", colnames(hits)[indices[k, 2]]),
          cmp_mass = cmp_mass,
          delta_cmp = abs(
            (as.numeric(cmp_mass) * multiple) -
              (peaks_filtered[j, "mass"] - as.numeric(protein_mw[, -1]))
          ),
          multiple = multiple
        )

        hits_df <- rbind(hits_df, hits_add)
      }
    }
  }

  # If no hits detected in peaks
  if (nrow(hits_df) == 0) {
    hits_df <- data.frame(
      well = "A1",
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
      multiple = NA
    )
  }

  log_hits_count(nrow(hits_df))
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
  } else if (ncol(hits) != 13) {
    log_err_cols()
    return(NULL)
  } else if (anyNA(hits)) {
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
    # usw ....

    # %Bintotal = %BinIB + %BinIC + %BinID  (alles was nicht freies Prot ist)

    # Adding %Binding values to hit data frame
    hits <- hits |>
      dplyr::mutate(
        `%binding` = intensity / I_total
      )
    hits <- hits |>
      dplyr::mutate(
        `%binding_tot` = sum(unique(hits$`%binding`)),
        .before = peak
      )

    # Plausibilitätscheck check result < 100 - richtig so?
    total_relBinding <- hits$`%binding_tot`[1] + perc_bind_prot
    if (!all.equal(total_relBinding, 1)) {
      log_err_binding()
      return(NULL)
    }

    log_intensities(
      I_total,
      unique(hits$prot_intensity),
      sum(unique(hits$intensity))
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
    "% Binding"
  )

  return(hits)
}

# Header: The Sample Name
log_start <- function(sample_name) {
  message(sprintf("Hit Screening: %s\n  │", sample_name))
  Sys.sleep(0.05)
}

# Status: Peak Info
log_status <- function(n_peaks, m_min, m_max) {
  message(sprintf(
    "  ├─ Status: %s peaks detected [%.2f - %.2f Da]",
    n_peaks,
    m_min,
    m_max
  ))
  # Sys.sleep(0.05)
}

# 3. The hit count
log_hits_count <- function(n_hits) {
  message(sprintf("  ├─ Result: %s hits detected", n_hits))
  # Sys.sleep(0.05)
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
  # Sys.sleep(0.05)
}

# Alert: No Peaks
log_alert <- function(msg = "No protein peak detected") {
  message(sprintf("  ├─ ⚠️ %s.  ", msg))
  # Sys.sleep(0.05)
}

# Footer: Closing a successful sample
log_done <- function() {
  message(paste0("  │\n", "  └─ ☑ Sample completed.\n  "))
  # Sys.sleep(0.05)
}

# Alert: empty hits argument
log_err_no_df <- function() {
  msg <- sprintf(
    "  │  └─ %s ALERT: 'hits' argument must be a data frame with at least one row. Skipping.\n",
    warning_sym
  )
  message(msg)
}

# Alert: discrepancy in expected hits columns
log_err_cols <- function(current_cols) {
  msg <- sprintf(
    "  │  └─ %s ALERT: 'hits' data frame has %s columns, but 13 are required.\n",
    warning_sym,
    current_cols
  )
  message(msg)
}

# Alert: 100% Total %-Binding plausibility check
log_err_binding <- function() {
  msg <- sprintf(
    "  │  └─ %s ALERT: Total relative binding is not 100%%. Check data integrity.\n",
    warning_sym
  )
  message(msg)
}

# Log hits summary
log_hits_summary <- function(hits_summarized) {
  message(paste(
    "SUMMARIZING HITS\n  │\n",
    sprintf(
      " ├─ %s sample(s) screened\n",
      length(unique(hits_summarized$Sample))
    ),
    sprintf(" └─ %s hit(s) detected in total\n", nrow(hits_summarized))
  ))
}

# Log binding kinetics analysis initiation
#' @export
log_binding_kinetics <- function(concentrations, times, units) {
  message(paste(
    "COMPUTING BINDING KINETICS\n  │\n",
    sprintf(
      " ├─ %s concentrations from %s to %s [%s] \n",
      length(unique(concentrations)),
      min(concentrations),
      max(concentrations),
      units[1]
    ),
    sprintf(
      " ├─ %s time points from %s to %s [%s] \n",
      length(unique(times)),
      min(times),
      max(times),
      units[2]
    ),
    " ├─ Infer observed first-order rate constant k_obs\n  │  │"
  ))
}

# Log filtered samples
log_filtered_samples <- function(diff) {
  if (diff > 0) {
    message(paste(
      sprintf(
        "  │  ├─ %s sample(s) ignored due to missing hits",
        diff
      )
    ))
  }
}

# Log filtered concentrations
log_filtered_concentrations <- function(initial_tbl, filtered_tbl, conc_time) {
  conc_diff <- unique(initial_tbl[[conc_time[1]]]) %in%
    unique(filtered_tbl[[conc_time[1]]])

  not_present_conc <- unique(initial_tbl[[conc_time[1]]])[!conc_diff]

  if (length(not_present_conc)) {
    message(paste(
      sprintf(
        "  │  ├─ Concentrations %s are omitted after filtering",
        paste(not_present_conc, collapse = "; ")
      )
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
  message(paste(
    sprintf(
      ifelse(
        last,
        "  │     ├─ %s time points included (%s - %s %s)",
        "  │  │  ├─ %s time points included (%s - %s %s)"
      ),
      nrow(data),
      min(data$time),
      max(data$time),
      unit
    ),
    if (nrow(data) < 3) {
      sprintf(
        ifelse(
          last,
          "\n  │     └─ %s ≥ 3 time points required for nonlinear fit",
          "\n  │  │  └─ %s ≥ 3 time points required for nonlinear fit"
        ),
        warning_sym
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
  ns
) {
  results <<- results
  sample_table <<- sample_table
  protein_table <<- protein_table
  compound_table <<- compound_table
  peak_tolerance <<- peak_tolerance
  max_multiples <<- max_multiples

  samples <- names(results$deconvolution)
  protein_mw <- protein_table$`Mass 1`
  compound_mw <- as.matrix(compound_table[, -1])
  rownames(compound_mw) <- compound_table[, 1]

  for (i in seq_along(samples)) {
    shinyWidgets::updateProgressBar(
      session = session,
      id = ns("conversion_progress"),
      value = ifelse(i == 1, 0, (i - 1) / length(samples) * 100),
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

    present_protein <- sample_table$Protein[sample_table$Sample == samples[i]]
    present_cmp <- sample_table[sample_table$Sample == samples[i], -c(1, 2)]

    results$deconvolution[[samples[i]]][["hits"]] <- check_hits(
      sample_table = sample_table,
      protein_mw = protein_table[protein_table$Protein == present_protein, ],
      compound_mw = compound_table[compound_table$Compound == present_cmp, ],
      peaks = get_peaks(result_sample = samples[i], results = results),
      peak_tolerance = peak_tolerance,
      max_multiples = max_multiples,
      sample = samples[i]
    )

    # Add hits data frame to sample
    results$deconvolution[[samples[i]]][[
      "hits"
    ]] <- conversion(results$deconvolution[[samples[i]]][[
      "hits"
    ]])

    # Add plot to sample
    # TODO - necessary?
    # if (!is.null(results$deconvolution[[samples[i]]][["hits"]])) {
    #   results$deconvolution[[samples[i]]][["hits_spectrum"]] <- spectrum_plot(
    #     sample = results$deconvolution[[samples[i]]]
    #   )
    # }

    log_done()
  }

  shinyWidgets::updateProgressBar(
    session = session,
    id = ns("conversion_progress"),
    value = 100,
    title = paste0(
      "Search for hits in ",
      length(samples),
      " samples completed."
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
    hits_summarized <- hits_summarized |>
      dplyr::left_join(
        sample_table[, c("Sample", conc_time)],
        by = "Sample"
      ) |>
      dplyr::mutate(binding = `Total % Binding` * 100) |>
      dplyr::arrange(dplyr::across(all_of(conc_time)))
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

# Function to add binding/kobs results to result list
#' @export
add_kobs_binding_result <- function(
  result_list,
  concentrations_select = NULL,
  units,
  conc_time
) {
  # Filter NA
  hits_summary <- result_list$hits_summary |>
    dplyr::filter(!is.na(Compound))

  # Log filtered samples
  log_filtered_samples(
    diff = nrow(result_list$hits_summary) - nrow(hits_summary)
  )

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

  # Log filtered samples
  log_filtered_concentrations(
    initial_tbl = result_list$hits_summary,
    filtered_tbl = hits_summary,
    conc_time = conc_time
  )

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
  units = NULL
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
        "%-Binding: %{y:.2f}<br>",
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
      hovertemplate = ~ paste(
        "<b>Observed</b><br>",
        paste(
          "Time: %{x}",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
          "<br>"
        ),
        "%-Binding: %{y:.2f}<br>",
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
      font = list(size = 14, color = "white"),
      legend = list(
        title = list(
          text = paste0(
            "Concentration [",
            gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
            "]"
          ),
          font = list(color = "white")
        ),
        bgcolor = "rgba(0,0,0,0)",
        bordercolor = "rgba(0,0,0,0)",
        font = list(color = "white")
      ),
      xaxis = list(
        title = "Time [min]",
        color = "white",
        showgrid = TRUE,
        gridcolor = "rgba(255, 255, 255, 0.2)",
        zerolinecolor = "rgba(255, 255, 255, 0.5)"
      ),
      yaxis = list(
        title = "Binding [%]",
        color = "white",
        showgrid = TRUE,
        gridcolor = "rgba(255, 255, 255, 0.2)",
        zerolinecolor = "rgba(255, 255, 255, 0.5)"
      )
    )

  # Return plot
  return(binding_plot)
}

# Function to generate and display kobs plot
#' @export
make_kobs_plot <- function(ki_kinact_result, colors, units) {
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

  # Generate plot
  kobs_plot <- plotly::plot_ly() |>
    # Predicted / modeled kobs
    plotly::add_lines(
      data = df,
      x = ~conc,
      y = ~predicted_kobs,
      colors = colors,
      symbols = symbol_map,
      line = list(width = 1.5, color = "white"),
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
        line = list(width = 1, color = "white")
      ),
      name = ~conc,
      symbols = symbol_map,
      symbol = ~ as.character(conc),
      hovertemplate = paste(
        "<b>Calculated</b><br>",
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
    ) |>
    plotly::layout(
      hovermode = "closest",
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      # Global font settings (White)
      font = list(size = 14, color = "white"),
      legend = list(
        title = list(
          text = paste0(
            "Concentration [",
            gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
            "]"
          ),
          font = list(color = "white")
        ),
        bgcolor = "rgba(0,0,0,0)",
        bordercolor = "rgba(0,0,0,0)",
        font = list(color = "white")
      ),
      # X-Axis Styling (White)
      xaxis = list(
        title = paste0(
          "Compound [",
          gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
          "]"
        ),
        color = "white",
        showgrid = TRUE,
        gridcolor = "rgba(255, 255, 255, 0.2)",
        zerolinecolor = "rgba(255, 255, 255, 0.5)"
      ),
      # Y-Axis Styling (White)
      yaxis = list(
        title = "k<sub>obs</sub> [s⁻¹]",
        color = "white",
        showgrid = TRUE,
        gridcolor = "rgba(255, 255, 255, 0.2)",
        zerolinecolor = "rgba(255, 255, 255, 0.5)"
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

    # TODO
    # Currently no duplicates/triplicates considered
    data <- hits |>
      dplyr::filter(!!rlang::sym(conc) == i) |>
      dplyr::distinct(time, .keep_all = TRUE)

    # Log timepoints
    log_timepoints(data = data, unit = units["Time"], last = last)

    # If less than 3 entries abort and continue with next iteration
    if (nrow(data) < 3) {
      next
    }
    # if (nrow(data) < 3) {
    # Make dummy row to anchor fitting at 0
    dummy_row <- data[1, ]
    # }
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

    # Append predictions to predictions data frame
    binding_table <- rbind(
      binding_table,
      dplyr::left_join(
        predictions,
        dplyr::select(data, c("time", "binding")),
        by = "time"
      ) |>
        dplyr::mutate(concentration = i, kobs = result$kobs)
    )

    # Add predictions specific for concentration
    concentration_list[[i]][["predictions"]] <- predictions

    # Save hits for concentration
    concentration_list[[i]][["hits"]] <- data

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
  # Get kobs subset
  kobs <- kobs_result$binding_table |>
    dplyr::filter(!duplicated(kobs_result$binding_table$kobs)) |>
    dplyr::mutate(conc = as.numeric(as.character(concentration))) |>
    dplyr::select(conc, kobs)

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
  units = NULL
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

  # Prepare hit marker symbols
  if (!all(is.na(peaks_data$mass))) {
    prot_peaks <- hits_summary$`Meas. Prot.`[
      if (time) {
        hits_summary$`Sample ID` %in% samples
      } else if (!isFALSE(truncated)) {
        hits_summary$truncSample_ID %in% peaks_data$z
      } else {
        hits_summary$`Sample ID` %in% peaks_data$z
      }
    ]
    prot_peaks <- prot_peaks[prot_peaks != "N/A"]
    prot_peaks <- as.numeric(gsub(
      " Da",
      "",
      prot_peaks
    ))

    prot_names <- unique(peaks_data$name[peaks_data$mass %in% prot_peaks])

    peaks_data <- dplyr::mutate(
      peaks_data,
      symbol = ifelse(mass %in% prot_peaks, "diamond", "circle"),
      linecolor = ifelse(mass %in% prot_peaks, "#000000", "#ffffff")
    )
  }

  color_cmp <- color_cmp[!is.na(names(color_cmp))]

  # Prepare compound marker colors and symbols
  if (!is.null(color_cmp) && !is.null(color_variable)) {
    if (color_variable == "Compounds") {
      if (length(color_cmp)) {
        # Adding protein peak marker
        prot_colors <- rep("#ffffff", length(prot_names))
        names(prot_colors) <- prot_names
        color_cmp <- c(prot_colors, color_cmp)

        # Match colors to peaks data
        peaks_data$color <- color_cmp[match(
          as.character(peaks_data$name),
          names(color_cmp)
        )]

        marker_color <- ~ I(color)
      } else {
        marker_color <- "#ffffff"
      }

      # Declare coloring variables for graph elements
      color <- NULL
      line <- list(color = "white", width = 1)
      z_linecolor <- list(color = "#ffffff")
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
      factor = 1.33
    )
    names(color_cmp) <- levels(spectrum_data$z)

    # Adding protein peak marker
    peaks_data <- dplyr::mutate(
      peaks_data,
      color = ifelse(symbol == "diamond", "#ffffff", "#000000")
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
          line = list(color = ~ I(linecolor), width = 2)
        )
      } else {
        marker_list <- list(
          color = marker_color,
          symbol = ~ I(symbol),
          size = 5,
          zindex = 100,
          line = list(color = ~ I(linecolor), width = 2)
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
        paper_bgcolor = "rgba(255,255,255,0)",
        font = list(color = "white"),
        legend = list(
          bgcolor = "rgba(0,0,0,0)",
          bordercolor = "rgba(0,0,0,0)",
          font = list(color = "white"),
          title = list(
            text = paste(
              "<b>",
              ifelse(
                time,
                paste0(
                  "Time [",
                  gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
                  "]"
                ),
                "Sample ID"
              ),
              "</b>"
            ),
            color = "white"
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
            gridcolor = "#7f7f7fff",
            showgrid = TRUE,
            showline = FALSE,
            showzeroline = FALSE,
            showticklabels = TRUE,
            showspikes = FALSE,
            showbackground = FALSE
          ),
          yaxis = list(
            title = "Intensity [%]",
            gridcolor = "#7f7f7fff",
            showgrid = TRUE,
            showline = FALSE,
            showzeroline = FALSE,
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
            gridcolor = "#7f7f7fff",
            showgrid = ifelse(time, TRUE, FALSE),
            showline = FALSE,
            showzeroline = FALSE,
            showticklabels = labels_show,
            showspikes = FALSE,
            showbackground = FALSE,
            type = 'category',
            tickvals = levels(spectrum_data$z)
          ),
          camera = list(
            center = list(x = 0.33, y = -0.05, z = 0.05),
            eye = if (length(unique(peaks_data$z)) <= 8) {
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
    plotly::plot_ly(
      data = spectrum_data,
      x = ~mass,
      y = ~intensity,
      color = ~z,
      colors = brighten_hex(
        viridisLite::viridis(
          length(unique(peaks_data$z))
        ),
        factor = 1.33
      ),
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
          # color = paste0(color, "50"),
          symbol = paste0(symbol, "-open")
        ),
        x = ~mass,
        y = ~intensity,
        split = ~ interaction(z, color),
        # split = ~z, # Splitting by time
        legendgroup = ~z,
        mode = "markers",
        color = marker_color,
        symbol = ~ I(symbol),
        inherit = FALSE,
        marker = list(
          size = 10,
          zindex = 100,
          color = "white"
          # ,
          # line = list(color = "white", width = 2)
        ),
        # marker = list(
        #   # color = marker_color,
        #   color = "white",
        #   symbol = ~ I(symbol),
        #   size = 10,
        #   zindex = 100,
        #   # line = list(color = ~ I(linecolor), width = 1)
        #   line = list(color = "white", width = 1)
        # ),
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
        font = list(color = "white"),
        xaxis = list(
          title = "Mass [Da]",
          color = "white",
          gridcolor = "rgba(255, 255, 255, 0.2)",
          zerolinecolor = "rgba(255, 255, 255, 0.5)"
        ),
        yaxis = list(
          title = "Intensity [%]",
          color = "white",
          gridcolor = "rgba(255, 255, 255, 0.2)",
          zerolinecolor = "rgba(255, 255, 255, 0.5)"
        ),
        legend = list(
          bgcolor = "rgba(0,0,0,0)",
          bordercolor = "rgba(0,0,0,0)",
          font = list(color = "white"),
          title = list(
            text = paste0(
              "<b>Time</b> [",
              gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
              "]"
            ),
            color = "white"
          )
        )
      )
  }
}

# Rendering function for relative binding table view
#' @export
render_table_view <- function(table, colors, tab, inputs, units) {
  # If table empty
  if (!nrow(table)) {
    return(DT::datatable(
      data.frame(rep(list(as.character()), 5)) |>
        stats::setNames(c(
          "Sample ID",
          "Cmp Name",
          "Mass Shift",
          "%-Binding",
          "Total %"
        )),
      selection = "none",
      class = "order-column",
      options = list(
        dom = 't',
        paging = FALSE
      )
    ))
  }

  # Get optional concentration and time cols
  optional_cols <- if (length(units) == 2) {
    c(units[["Concentration"]], units[["Time"]])
  } else {
    NULL
  }

  # Replace NA in color names
  names(colors)[is.na(names(colors))] <- "N/A"

  # Prepate data frame for table
  tbl <- table |>
    dplyr::ungroup() |>
    dplyr::select(
      `Sample ID` = `Sample ID`,
      `Cmp Name` = `Cmp Name`,
      dplyr::any_of(optional_cols),
      `Mass Shift` = `Theor. Cmp`,
      `%-Binding` = `%-Binding`,
      `Total %` = `Total %-Binding`
    ) |>
    dplyr::mutate(
      `Sample ID` = if (inputs$truncate_names) {
        table$`truncSample_ID`
      } else {
        `Sample ID`
      },
      `Cmp Name` = ifelse(is.na(`Cmp Name`), "N/A", `Cmp Name`),
      `Mass Shift` = ifelse(
        `Mass Shift` == "N/A",
        "N/A",
        paste0(
          "[",
          `Mass Shift`,
          "]",
          "&thinsp;<sub>",
          table$`Bind. Stoich.`,
          "</sub>"
        )
      ),
      label_color = get_contrast_color(colors[match(
        if (
          length(units) == 2 && inputs$color_variable == units["Concentration"]
        ) {
          table[[units["Concentration"]]]
        } else if (inputs$color_variable == "Compounds") {
          `Cmp Name`
        } else if (inputs$color_variable == "Samples") {
          `Sample ID`
        },
        names(colors)
      )]),
      `%-Binding` = if (
        is.null(inputs$binding_bar) || isTRUE(inputs$binding_bar)
      ) {
        `%-Binding`
      } else {
        as.character(`%-Binding`)
      },
      `Total %` = if (
        is.null(inputs$tot_binding_bar) || isTRUE(inputs$tot_binding_bar)
      ) {
        `Total %`
      } else {
        as.character(`Total %`)
      },
      col_var = !!rlang::sym(
        if (
          length(units) == 2 &&
            inputs$color_variable == units[["Concentration"]]
        ) {
          units[["Concentration"]]
        } else if (inputs$color_variable == "Compounds") {
          "Cmp Name"
        } else if (inputs$color_variable == "Samples") {
          "Sample ID"
        }
      )
    )

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
    if (length(unique(tbl[[group_variable]])) != nrow(tbl)) {
      if (all(tbl$`Sample ID` %in% names(colors))) {
        names(colors) <- paste("Sample ID:", names(colors))
      }

      tbl$`Sample ID` <- paste("Sample ID:", tbl$`Sample ID`)
    }
  } else if (any(tab %in% c("Samples", "Proteins"))) {
    group_variable <- "Cmp Name"
    if (length(unique(tbl[[group_variable]])) != nrow(tbl)) {
      if (all(tbl$`Cmp Name` %in% names(colors))) {
        names(colors) <- paste("Compound:", names(colors))
      }

      tbl$`Cmp Name` <- paste("Compound:", tbl$`Cmp Name`)
    }
  } else {
    group_variable <- NULL
  }

  if (
    is.null(group_variable) ||
      length(unique(tbl[[group_variable]])) == nrow(tbl)
  ) {
    row_group <- NULL
  } else {
    row_group <- list(dataSrc = which(names(tbl) == group_variable) - 1)
  }

  # Add color adaptive font color to table
  tbl <- dplyr::mutate(
    tbl,
    dplyr::across(
      -dplyr::any_of(c(
        group_variable,
        "col_var",
        if (is.null(inputs$binding_bar) || isTRUE(inputs$binding_bar)) {
          "%-Binding"
        },
        if (is.null(inputs$tot_binding_bar) || isTRUE(inputs$tot_binding_bar)) {
          "Total %"
        }
      )),
      ~ paste0(
        "<span style='color:",
        label_color,
        "'>",
        .x,
        "</span>"
      )
    )
  )

  DT::datatable(
    data = tbl,
    escape = FALSE,
    extensions = "RowGroup",
    rownames = FALSE,
    class = "order-column",
    selection = "none",
    options = list(
      dom = 't',
      paging = FALSE,
      scrollY = TRUE,
      scrollCollapse = TRUE,
      rowGroup = row_group,
      columnDefs = list(
        list(
          visible = ifelse(
            is.null(group_variable) ||
              length(unique(tbl[[group_variable]])) == nrow(tbl),
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
            if (tab == "Concentration") "Cmp Name"
          )
        ),
        list(className = 'dt-center', targets = "_all"),
        list(
          targets = "%-Binding",
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

# Rendering function of hits table
#' @export
render_hits_table <- function(
  hits_table,
  concentration_colors,
  single_conc = NULL,
  selected_cols = NULL,
  bar_chart = character(),
  compounds = NULL,
  samples = NULL,
  colors = NULL,
  color_variable = NULL,
  expand = TRUE,
  na_include = TRUE,
  truncated = NULL,
  clickable = FALSE,
  units
) {
  # Modify if samples are summarized instead of expanded
  if (!expand) {
    hits_table <- hits_table |>
      dplyr::distinct(
        `Sample ID`,
        `Protein`,
        `Cmp Name`,
        `Theor. Prot.`,
        `Total %-Binding`,
        `truncSample_ID`
      )
  }

  if (length(bar_chart)) {
    if (
      "Total %-Binding" %in%
        names(hits_table) &
        any("Total %-Binding" %in% bar_chart)
    ) {
      hits_table$`Total %-Binding`[hits_table$`Total %-Binding` == "N/A"] <- NA
    }
    if (
      "%-Binding" %in%
        names(hits_table) &
        any("%-Binding" %in% bar_chart)
    ) {
      hits_table$`%-Binding`[hits_table$`%-Binding` == "N/A"] <- NA
    }
  }

  # Filter compounds
  if (!is.null(compounds) && na_include) {
    hits_table <- dplyr::filter(
      hits_table,
      `Cmp Name` %in% compounds | is.na(`Cmp Name`)
    )
  } else if (!is.null(compounds)) {
    hits_table <- dplyr::filter(
      hits_table,
      `Cmp Name` %in% compounds
    )
  }

  # Filter samples
  if (!is.null(samples)) {
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
      all_of(std_cols),
      all_of(selected_cols)
    )

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

      rowCallback <- c(
        "function(row, data){",
        "  var targets = ",
        jsonlite::toJSON(clickable_targets),
        ";",
        "  for(var i=0; i<data.length; i++){",
        "    if(data[i] === null){",
        "      $('td:eq('+i+')', row).html('N/A').css({'color': 'inherit'});",
        "    }",
        "    if(targets.includes(i) && data[i] !== null){",
        "      $('td:eq('+i+')', row).addClass('clickable-column');",
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
      fixedHeader = TRUE,
      stripe = FALSE,
      dom = dom_value,
      paging = ifelse(!is.null(single_conc), TRUE, FALSE),
      columnDefs = list(
        if (length(bar_chart) > 0 & any(bar_chart %in% names(hits_table))) {
          list(
            targets = bar_chart[bar_chart %in% names(hits_table)],
            render = htmlwidgets::JS(chart_js)
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

# Empty sample declaration table generator function
#' @export
new_sample_table <- function(
  result,
  protein_table,
  compound_table,
  ki_kinact = FALSE
) {
  sample_tab <- data.frame(
    Sample = names(result$deconvolution),
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
    if (!is.null(ki_kinact) && ki_kinact) c("Concentration", "Time")
  )

  return(sample_tab)
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

  # Disable header checkbox
  shinyjs::disable(paste0(tab_low, "_header_checkbox"))

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

  # Enable header checkbox
  shinyjs::enable(paste0(tab_low, "_header_checkbox"))

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
  # Show waiter with 0.25 seconds minimum runtime
  waiter::waiter_show(
    id = ns(paste0(tab, "_table_info")),
    html = waiter::spin_throbber()
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

      # Enable confirm button
      shinyjs::enable(paste0("confirm_", tab))

      # Set status variable to TRUE
      table_status <- TRUE
    } else {
      # Table info UI changes
      shinyjs::removeClass(
        paste0(tab, "_table_info"),
        "table-info-green"
      )
      shinyjs::addClass(
        paste0(tab, "_table_info"),
        "table-info-red"
      )
      output[[paste0(tab, "_table_info")]] <- shiny::renderText(
        table_check
      )

      # Disable confirm button
      shinyjs::disable(paste0("confirm_", tab))

      # Set status variable to FALSE
      table_status <- FALSE
    }
  }

  # Hide waiter
  waiter::waiter_hide(id = ns(paste0(tab, "_table_info")))

  return(table_status)
}

# Generalized function to handle file uploads for proteins or compounds
#' @export
handle_file_upload <- function(
  file_input,
  header_checkbox,
  type,
  output,
  declaration_vars
) {
  shiny::req(file_input)

  table_upload <- read_uploaded_file(
    file_input$datapath,
    tolower(tools::file_ext(file_input$name)),
    header_checkbox
  )

  table_upload_processed <- process_uploaded_table(table_upload, type)

  if (!is.null(table_upload_processed)) {
    declaration_vars[[paste0(type, "_table_status")]] <- TRUE

    shinyWidgets::show_toast(
      paste0(tools::toTitleCase(type), " table loaded!"),
      type = "success",
      timer = 3000
    )
  } else {
    shinyWidgets::show_toast(
      paste0("Loading ", tolower(tools::toTitleCase(type)), " table failed!"),
      type = "error",
      timer = 3000
    )
  }

  table_upload_processed
}

# Transform summarized hits into readable table
#' @export
transform_hits <- function(hits_summary) {
  # Shared transformations
  summary_table <- hits_summary |>
    dplyr::mutate(
      # Format all percentages
      dplyr::across(
        c(`% Binding`, `Total % Binding`),
        ~ scales::percent(.x, accuracy = 0.1)
      ),
      dplyr::across(
        c(Intensity, `Protein Intensity`),
        ~ scales::percent(.x / 100, accuracy = 0.1)
      ),
      # Format all [Da] columns
      dplyr::across(
        dplyr::ends_with("[Da]"),
        ~ dplyr::if_else(
          is.na(.x),
          "N/A",
          paste(format(.x, nsmall = 1, trim = TRUE), "Da")
        )
      ),
      dplyr::across(
        !Compound,
        ~ tidyr::replace_na(as.character(.x), "N/A")
      )
    ) |>
    dplyr::relocate(`Total % Binding`, .after = "% Binding") |>
    dplyr::mutate(
      `Total % Binding` = as.numeric(gsub(
        "%",
        "",
        gsub("N/A", "0", `Total % Binding`)
      )),
      `% Binding` = as.numeric(gsub("%", "", gsub("N/A", "0", `% Binding`)))
    )

  # Define column names
  col_names <- c(
    "Well",
    "Sample ID",
    "Protein",
    "Theor. Prot.",
    "Meas. Prot.",
    "Δ Prot.",
    "Ⅰ Prot.",
    "Peak Signal",
    "Ⅰ Cmp",
    "Cmp Name",
    "Theor. Cmp",
    "Δ Cmp",
    "Bind. Stoich.",
    "%-Binding",
    "Total %-Binding"
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
      colors <- vir_func(n)
    } else {
      stop(paste("Scale", scale, "not recognized in provided lists."))
    }
  }

  # Adjust brightness
  colors <- brighten_hex(colors, factor = 1.33)

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
