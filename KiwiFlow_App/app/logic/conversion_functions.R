# app/logic/conversion_functions.R

box::use(
  app / logic / deconvolution_functions[spectrum_plot, ],
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

#' @export
slice_sample_tab <- function(sample_table) {
  non_empty <- which(
    colSums(is.na(sample_table) | sample_table == "") != nrow(sample_table)
  )
  return(sample_table[, non_empty])
}

# Validate sample table
#' @export
check_sample_table <- function(sample_table, proteins, compounds) {
  # Check if protein names valid
  if (
    !all(
      unlist(
        sample_table |>
          dplyr::select("Protein") |>
          dplyr::select(where(~ !all(is.na(.))))
      ) %in%
        proteins
    )
  ) {
    return("Protein name not found")
  }

  # Check if compound names valid
  if (
    !all(
      unlist(
        sample_table |>
          dplyr::select(-c("Sample", "Protein")) |>
          dplyr::select(where(~ !all(is.na(.))))
      ) %in%
        compounds
    )
  ) {
    return("Compound name not found")
  }

  # Check if compounds are defined
  if (all(is.na(sample_table[, -c(1, 2)]))) {
    # Reject only NA
    return("Enter compounds")
  } else {
    nonEmpty_cmps <- sample_table |>
      dplyr::select(where(~ !all(is.na(.)))) |>
      dplyr::select(-c(1, 2)) !=
      ""

    if (!all(nonEmpty_cmps)) {
      # Reject empty names
      return("Compound names missing")
    }
  }

  # Check duplicated compounds
  if (
    any(apply(as.data.frame(sample_table[, c(-1, -2)]), 1, function(x) {
      any(duplicated(stats::na.omit(x)))
    }))
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
    any(apply(as.data.frame(tab[, -1]), 1, function(x) {
      any(duplicated(round(stats::na.omit(x), digits = 3)))
    }))
  ) {
    return("Duplicated mass shift")
  }

  return(TRUE)
}


# Get unicode character for warning symbols
warning_sym <- "\u26A0"

# Parse filename according to nomenclature of test files
parse_filename <- function(s) {
  # Remove file extension if present
  s <- sub("\\.[^\\.]+$", "", s)

  # Split on + (corrected escaping for fixed=TRUE)
  parts <- strsplit(s, "+", fixed = TRUE)[[1]]

  if (length(parts) != 2) {
    stop("String does not contain exactly one +")
  }

  before <- parts[1]

  after <- parts[2]

  # Now split after on _
  after_parts <- strsplit(after, "_", fixed = TRUE)[[1]]

  # Combine into a vector
  result <- c(before, after_parts)

  return(result)
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

  # Message information
  message(
    "-> ",
    nrow(peaks),
    " mass peaks detected ranging from ",
    min(peaks$mass),
    " to ",
    max(peaks$mass),
    " Da and intensities ranging from ",
    min(peaks$intensity),
    " to ",
    max(peaks$intensity)
  )

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
  protein_peak <- peaks$mass >= protein_mw - peak_tolerance &
    peaks$mass <= protein_mw + peak_tolerance

  # Abort if peaks show invalid peaks
  if (!any(protein_peak)) {
    message(warning_sym, " No protein peak detected")

    hits_df <- data.frame(
      well = "A1",
      sample = sample,
      protein = parse_filename(sample)[1],
      theor_prot = as.numeric(protein_mw),
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
  peaks_valid <- peaks$mass >= protein_mw - peak_tolerance
  if (any(peaks_valid) && sum(peaks_valid) > 1) {
    peaks_filtered <- as.data.frame(peaks[peaks_valid, ])
  } else {
    message(warning_sym, " No peaks other than the protein were detected")

    hits_df <- data.frame(
      well = "A1",
      sample = sample,
      protein = parse_filename(sample)[1],
      theor_prot = as.numeric(protein_mw),
      measured_prot = peaks$mass[which(protein_peak)],
      delta_prot = abs(
        as.numeric(protein_mw) - peaks$mass[which(protein_peak)]
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

  # Only keep compounds that are in sample
  if (nrow(compound_mw) == 1) {
    cmp_mat <- t(compound_mw)
  } else {
    sample_compounds <- sample_table[which(sample == sample_table$Sample), ]
    sample_compound_vector <- unlist(sample_compounds[-c(1, 2)])
    cmp_mat <- t(as.matrix(compound_mw[sample_compound_vector, ]))
  }

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
  complex_mat <- mat + protein_mw

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
          protein = parse_filename(sample)[1],
          theor_prot = as.numeric(protein_mw),
          measured_prot = peaks$mass[which(protein_peak)],
          delta_prot = round(
            abs(
              as.numeric(protein_mw) - peaks$mass[which(protein_peak)]
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
              (peaks_filtered[j, "mass"] - as.numeric(protein_mw))
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
      protein = parse_filename(sample)[1],
      theor_prot = as.numeric(protein_mw),
      measured_prot = peaks$mass[which(protein_peak)],
      delta_prot = abs(
        as.numeric(protein_mw) - peaks$mass[which(protein_peak)]
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

  message("-> ", nrow(hits_df), " hits detected.")
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
    message(
      warning_sym,
      " 'hits' argument has to be a data frame with at least one row"
    )
    return(NULL)
  } else if (ncol(hits) != 13) {
    message(
      warning_sym,
      " 'hits' data frame has ",
      ncol(hits),
      " columns, but 13 are required."
    )
    return(NULL)
  } else if (anyNA(hits)) {
    message("No binding events in sample.")

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
    message("-> Total intensity = ", I_total)

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
      message(
        warning_sym,
        " total relative binding is not 100%."
      )
      return(NULL)
    }

    # Inform unbound protein intensity
    message(
      "-> Unbound protein intensity = ",
      hits$intensity[1],
      " (",
      scales::label_percent(accuracy = 0.1)(hits$`%binding`[1]),
      ")"
    )

    # Inform %binding except protein
    message(
      "-> Total binding compounds intensity = ",
      sum(unique(hits$intensity)),
      " (",
      scales::label_percent(accuracy = 0.1)(hits$`%binding_tot`[1]),
      ")"
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

#' @export
add_hits <- function(
  results,
  sample_table,
  protein_table,
  compound_table,
  peak_tolerance,
  max_multiples
) {
  samples <- names(results$deconvolution)
  # protein_mw <- get_protein_mw(protein_mw_file)
  protein_mw <- protein_table$`Mass 1`
  # compound_mw <- get_compound_matrix(compound_mw_file)
  compound_mw <- as.matrix(compound_table[, -1])
  rownames(compound_mw) <- compound_table[, 1]

  for (i in seq_along(samples)) {
    message("### Checking hits for ", samples[i])
    results$deconvolution[[samples[i]]][["hits"]] <- check_hits(
      sample_table = sample_table,
      protein_mw = protein_mw,
      compound_mw = compound_mw,
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
    if (!is.null(results$deconvolution[[samples[i]]][["hits"]])) {
      results$deconvolution[[samples[i]]][["hits_spectrum"]] <- spectrum_plot(
        sample = results$deconvolution[[samples[i]]]
      )
    }
  }

  message("Search for hits in ", length(samples), " samples completed.")
  return(results)
}

# Concatenate and extract all hits data frames from all samples
#' @export
summarize_hits <- function(result_list) {
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
add_kobs_binding_result <- function(result_list, concentrations_select = NULL) {
  # Replace NA's with 0
  # hits_summary <- result_list[["hits_summary"]]
  # hits_summary[is.na(hits_summary)] <- 0

  hits_summary <- result_list$hits_summary |>
    # Add concentration, time and binding columns to hits summary
    dplyr::mutate(
      time = extract_minutes(Sample),
      binding = `Total % Binding` * 100,
      concentration = gsub(
        "o",
        ".",
        sapply(strsplit(result_list$hits_summary$Sample, "_"), `[`, 3)
      )
    ) |>
    dplyr::filter(!is.na(Compound)) |>
    dplyr::group_by(concentration) |>
    dplyr::arrange(as.numeric(concentration), time)

  # Compute and model kobs values
  if (!is.null(concentrations_select)) {
    hits_summary <- dplyr::filter(
      hits_summary,
      concentration %in% concentrations_select
    )
  }

  # Compute kobs
  binding_kobs_result <- compute_kobs(hits_summary, units = "µM - minutes")

  # Add and display binding plot
  binding_kobs_result$binding_plot <- make_binding_plot(binding_kobs_result)

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
add_ki_kinact_result <- function(result_list) {
  # Calculcate Ki/kinact from binding/kobs result
  ki_kinact_result <- compute_ki_kinact(result_list[["binding_kobs_result"]])

  # Add and display kobs plot to Ki/kinact results
  ki_kinact_result$kobs_plot <- make_kobs_plot(ki_kinact_result)

  return(ki_kinact_result)
}

# Function to generate and display binding plot
make_binding_plot <- function(kobs_result) {
  # Keep only non-zero observed data points
  df_points <- kobs_result$binding_table[
    !(kobs_result$binding_table$time == 0 |
      kobs_result$binding_table$binding == 0),
  ]

  # Generate plot
  binding_plot <- plotly::plot_ly() |>
    # Predicted/modeled binding
    plotly::add_lines(
      data = kobs_result$binding_table,
      x = ~time,
      y = ~predicted_binding,
      color = ~concentration,
      colors = "Set1",
      line = list(width = 2, opacity = 0.6),
      legendgroup = ~concentration,
      hovertemplate = paste(
        "<b>Predicted</b><br>",
        "Time: %{x}<br>",
        "%-Binding: %{y:.2f}<br>",
        "K<sub>obs</sub>: %{customdata:.2f}<extra></extra>"
      ),
      customdata = ~kobs
    ) |>
    # Observed binding
    plotly::add_markers(
      data = df_points,
      x = ~time,
      y = ~binding,
      color = ~concentration,
      colors = "Set1",
      symbol = ~concentration,
      marker = list(
        size = 12,
        opacity = 0.9,
        line = list(width = 1.5, color = "black")
      ),
      legendgroup = ~concentration,
      hovertemplate = paste(
        "<b>Observed</b><br>",
        "Time: %{x}<br>",
        "%-Binding: %{y:.2f}<br>",
        "K<sub>obs</sub>: %{customdata:.2f}<extra></extra>"
      ),
      customdata = ~kobs
    ) |>
    # Layout changes
    plotly::layout(
      hovermode = "closest",
      legend = list(title = list(text = "<b>Concentration</b>")),
      xaxis = list(title = "Time [min]"),
      yaxis = list(title = "Binding [%]"),
      font = list(size = 14)
    )

  # Print plot
  print(binding_plot)

  # Return plot
  return(binding_plot)
}

# Function to generate and display kobs plot
make_kobs_plot <- function(ki_kinact_result) {
  # Get predicted/modeled kobs
  df <- ki_kinact_result$Kobs_Data[
    !is.na(ki_kinact_result$Kobs_Data$predicted_kobs),
  ]

  # Get observed kobs data points
  df_points <- ki_kinact_result$Kobs_Data[
    !is.na(ki_kinact_result$Kobs_Data$kobs) &
      ki_kinact_result$Kobs_Data$kobs != 0,
  ]
  df_points$kobs <- factor(
    df_points$kobs,
    levels = sort(df_points$kobs, decreasing = TRUE)
  )

  # Prepare color scale
  discrete_colors <- RColorBrewer::brewer.pal(n = 6, name = "Set1") |>
    plotly::toRGB()

  kobs_plot <- plotly::plot_ly() |>
    plotly::add_lines(
      data = df,
      x = ~conc,
      y = ~predicted_kobs,
      line = list(width = 2, opacity = 0.6, color = "black"),
      showlegend = FALSE
    ) |>
    plotly::add_trace(
      data = dplyr::group_by(df_points, kobs),
      x = ~conc,
      y = ~kobs,
      type = "scatter",
      mode = "markers",
      name = ~kobs,
      symbol = ~kobs,
      marker = list(
        size = 12,
        opacity = 0.9,
        line = list(width = 1.5, color = "black")
      )
    ) |>
    plotly::layout(
      hovermode = "closest",
      legend = list(title = list(text = "<b>k<sub>obs</sub></b>")),
      yaxis = list(title = "k<sub>obs</sub>"),
      xaxis = list(title = "Compound [µM]"),
      font = list(size = 14),
      colorway = discrete_colors
    )

  # Print plot
  print(kobs_plot)

  # Return plot
  return(kobs_plot)
}

# Function to predict binding/kobs values
predict_values <- function(
  data,
  predict,
  x,
  interval,
  fitted_model
) {
  # Prepare sequence of predictions
  prediction_df <- data.frame(seq(0, max(data[[x]]), by = interval))
  colnames(prediction_df) <- x

  # Predict using the fitted model
  predicted <- stats::predict(
    fitted_model,
    prediction_df
  )

  prediction_df[[paste0("predicted_", predict)]] <- predicted

  return(prediction_df)
}

compute_kobs <- function(hits, units = "µM - minutes") {
  # Prepare empty objects
  concentration_list <- list()
  binding_table <- data.frame()

  # Filter non-zero concentrations
  # TODO add dynamic outlier selection to filter
  # hits <- dplyr::filter(hits, concentration != "0", concentration != "2.1875")

  # Loop over each unique concentration
  for (i in unique(hits$concentration)) {
    # TODO
    # Currently no duplicates/triplicates considered
    data <- hits |>
      dplyr::filter(concentration == i, !duplicated(time))

    # Make dummy row to anchor fitting at 0
    dummy_row <- data[1, ]
    dummy_row$binding <- 0.0
    dummy_row$time <- 0
    if ("Well" %in% colnames(data)) {
      dummy_row$Well <- "XX"
    } # If Well column exists
    data <- rbind(data, dummy_row)

    # Starting values based on units
    # TODO
    if (units == "M - seconds") {
      start_vals <- c(v = 1, kobs = 0.0004)
    } else {
      start_vals <- c(v = 1, kobs = 0.001)
    }

    # Nonlinear regression with customized minpack.lm::nlsLM() function
    nonlin_mod <- nlsLM_fixed(
      formula = binding ~ 100 * (v / kobs * (1 - exp(-kobs * time))),
      start = start_vals,
      data = data
    )

    # Extract parameters
    params <- summary(nonlin_mod)$parameters
    result <- list(
      kobs = params[2, 1], # Kobs value
      v = params[1, 1], # v parameter
      plateau = 100 * (params[1, 1] / params[2, 1]), # Computed max binding %
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
      interval = 1
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

compute_ki_kinact <- function(kobs_result, units = "µM - minutes") {
  # Get kobs subset
  kobs <- kobs_result$binding_table |>
    dplyr::filter(!duplicated(kobs_result$binding_table$kobs)) |>
    dplyr::mutate(conc = as.numeric(as.character(concentration))) |>
    dplyr::select(conc, kobs)

  # Adjust start values to units
  if (units == "M - seconds") {
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
