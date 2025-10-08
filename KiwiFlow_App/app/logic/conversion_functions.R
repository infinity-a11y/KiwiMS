# app/logic/conversion_functions.R

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

#' @export
prot_comp_handsontable <- function(tab, disabled = FALSE) {
  renderer_js <- "function(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    
    td.style.background = ''; // Clear existing background for new rendering
    
    // Function to get the value rounded to 3 digits (or empty string if non-numeric/NA)
    var getNormalizedValue = function(val) {
        if (val == null || val === '') {
            return '';
        }
        
        // 1. Try to parse as a float
        var floatVal = parseFloat(val);
        
        // 2. Check if it's a valid number (i.e., not NaN)
        if (isNaN(floatVal)) {
            // For non-numeric text, return the trimmed string itself (for column 0 check)
            return String(val).trim(); 
        } else {
            // 3. Round to 3 decimal places and return as a string
            // This handles floating-point precision issues
            return floatVal.toFixed(3); 
        }
    };

    // Get the normalized/rounded value for the current cell
    var normalizedValue = getNormalizedValue(value);
    
    if (normalizedValue === '') {
        // Skip all duplication checks and styling for empty cells
        return; 
    }

    var isDuplicated = false;
    
    // --- A. Column 0 Duplication Check ('Sample' column - likely non-numeric) ---
    // This check uses the string value, as it's the 'Sample' column
    if (col === 0) {
        var colData = instance.getDataAtCol(0); 
        var valueCounts = {};
        
        // Count frequencies of non-empty values in the entire column 0
        for (var i = 0; i < colData.length; i++) {
            var cellValue = colData[i];
            var trimmedValue = cellValue == null ? '' : String(cellValue).trim();
            
            if (trimmedValue !== '') {
                valueCounts[trimmedValue] = (valueCounts[trimmedValue] || 0) + 1;
            }
        }
        
        if (valueCounts[normalizedValue] > 1) {
            isDuplicated = true;
        }
    }
    
    // --- B. Row Duplication Check (Columns 1 and greater - numeric/mass columns) ---
    // This check uses the 3-digit rounded value
    if (col >= 1) {
      var rowData = instance.getDataAtRow(row);
      var valueCounts = {};
      var startCol = 1; // Start from the second column
      
      // Count frequencies of non-empty/rounded values in the current row (from col 1 onwards)
      for (var i = startCol; i < rowData.length; i++) {
          var cellValue = rowData[i];
          // Use the rounding function for mass columns
          var roundedValue = getNormalizedValue(cellValue);
          
          if (roundedValue !== '') {
              valueCounts[roundedValue] = (valueCounts[roundedValue] || 0) + 1;
          }
      }
      
      // The current cell's value (normalizedValue) is already rounded via getNormalizedValue(value)
      if (valueCounts[normalizedValue] > 1) {
          isDuplicated = true;
      }
    }
    
    // --- C. Apply Styles ---
    if (isDuplicated) {
      td.style.background = 'orange';
    }
    
    // --- D. Re-apply Dropdown Renderer ---
    if (cellProperties.type === 'dropdown') {
        Handsontable.renderers.DropdownRenderer.apply(this, arguments);
    }
  }"

  table <- rhandsontable::rhandsontable(
    tab,
    rowHeaders = NULL,
    stretchH = "all"
  ) |>
    rhandsontable::hot_cols(fixedColumnsLeft = 1, renderer = renderer_js) |>
    rhandsontable::hot_table(
      contextMenu = TRUE,
      highlightCol = TRUE,
      highlightRow = TRUE
    ) |>
    rhandsontable::hot_context_menu(
      allowRowEdit = TRUE,
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
    
    // --- 5. Re-apply Dropdown Renderer ---
    if (cellProperties.type === 'dropdown') {
        Handsontable.renderers.DropdownRenderer.apply(this, arguments);
    }
  }"
  } else {
    allowed_per_col <- list(NULL)
    renderer_js <- ""
  }

  handsontable <- rhandsontable::rhandsontable(
    tab,
    rowHeaders = NULL,
    allowed_per_col = allowed_per_col
  ) |>
    rhandsontable::hot_col("Sample", readOnly = TRUE) |>
    rhandsontable::hot_cols(fixedColumnsLeft = 1, renderer = renderer_js) |>
    rhandsontable::hot_table(
      contextMenu = FALSE
    )

  if (length(proteins) > 1) {
    handsontable <- handsontable |>
      rhandsontable::hot_col(
        col = "Protein",
        type = "dropdown",
        source = proteins
      )
  }

  if (length(compounds) > 1) {
    handsontable <- handsontable |>
      rhandsontable::hot_col(
        col = min(cmp_cols):max(cmp_cols),
        type = "dropdown",
        source = compounds
      )
  }

  if (disabled) {
    handsontable <- rhandsontable::hot_cols(handsontable, readOnly = TRUE)
  }

  return(handsontable)
}

#' @export
slice_tab <- function(tab) {
  row_contain <- which(rowSums(is.na(tab) | tab == "") != ncol(tab))
  return(tab[row_contain, ])
}

# Validate sample table
#' @export
check_sample_table <- function(sample_table, proteins, compounds) {
  if (any(!sample_table$Protein %in% proteins)) {
    return("Protein name not found")
  }

  if (any(!sample_table$Compound %in% compounds)) {
    return("Compound name not found")
  }

  if (
    any(apply(
      as.data.frame(sample_table[, 3:ncol(sample_table)]),
      1,
      duplicated
    ))
  ) {
    return("Duplicated compounds")
  }

  return(TRUE)
}

# Validate protein/compound table
#' @export
check_table <- function(tab, col_limit) {
  if (!nrow(tab)) {
    return("Fill the table with values.")
  }

  # Check variable types
  tab_variables <- sapply(tab, class)
  if (tab_variables[1] != "character") {
    return(paste("Only text characters are allowed as name IDs"))
  }
  if (
    !all(tab_variables[-1] == "numeric") |
      !all(rowSums(!is.na(tab[, -1])) > 0)
  ) {
    return(paste("Mass fields require numeric values"))
  }

  # Check missing names
  if (!all(!is.na(tab[, 1]))) {
    return(paste("Missing name ID values"))
  }

  # Check duplicated names
  if (any(duplicated(tab[, 1]))) {
    return(paste("Duplicated name ID values"))
  }

  # Check duplicated masses
  if (
    any(apply(tab[, -1], 1, function(x) {
      any(duplicated(round(stats::na.omit(x), digits = 3)))
    }))
  ) {
    return("Duplicated mass shift")
  }

  return(TRUE)
}
