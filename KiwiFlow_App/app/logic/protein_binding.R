### Script collecting all functions related to calculate protein binding per sample

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
    result_sample == names(results)
    peaks <- results[[which(result_sample == names(results))]]$peaks
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
    return(NULL)
  }

  # Keep only peaks above protein mw
  peaks_valid <- peaks$mass >= protein_mw - peak_tolerance
  if (any(peaks_valid) && sum(peaks_valid) > 1) {
    peaks_filtered <- as.data.frame(peaks[peaks_valid, ])
  } else {
    message(warning_sym, " No peaks other than the protein were detected")
    return(NULL)
  }

  # Only keep compounds that are in sample
  cmp_name <- parse_filename(sample)[2]
  cmp_mat <- t(as.matrix(compound_mw[cmp_name, ]))
  dimnames(cmp_mat) <- list(cmp_name, colnames(compound_mw))

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
          delta_prot = abs(
            as.numeric(protein_mw) - peaks$mass[which(protein_peak)]
          ),
          prot_intensity = peaks$intensity[which(protein_peak)],
          peak = peaks_filtered[j, "mass"],
          intensity = peaks_filtered[j, "intensity"],
          compound = rownames(indices)[k],
          cmp_mass = cmp_mass,
          multiple = multiple
        )

        hits_df <- rbind(hits_df, hits_add)
      }
    }
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
  } else if (ncol(hits) != 12) {
    message(
      warning_sym,
      " 'hits' data frame has ",
      ncol(hits),
      " columns, but five are required."
    )
    return(NULL)
  }

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
    "Binding Stoichiometry",
    "% Binding"
  )

  return(hits)
}

add_hits <- function(
  results,
  protein_mw_file,
  compound_mw_file,
  peak_tolerance,
  max_multiples
) {
  samples <- head(names(results), -2)
  # protein_mw <- get_protein_mw(protein_mw_file)
  protein_mw <- protein_table$`Mass 1`
  # compound_mw <- get_compound_matrix(compound_mw_file)
  compound_mw <- as.matrix(compound_table[, -1])
  rownames(compound_mw) <- compound_table[, 1]

  for (i in seq_along(samples)) {
    message("### Checking hits for ", samples[i])
    results[[samples[i]]][["hits"]] <- check_hits(
      protein_mw = protein_mw,
      compound_mw = compound_mw,
      peaks = get_peaks(result_sample = samples[i], results = results),
      peak_tolerance = peak_tolerance,
      max_multiples = max_multiples,
      sample = names(results)[i]
    )

    results[[samples[i]]][["hits"]] <- conversion(results[[samples[i]]][[
      "hits"
    ]])
  }

  message("Search for hits in ", length(samples), " samples completed.")
  return(results)
}

# Concatenate and extract all hits data frames from all samples
summarize_hits <- function(result_list) {
  # Get samples from result list without session and output elements
  samples <- head(names(result_list), -2)

  # Prepare empty hits data frame
  hits_summarized <- data.frame()

  for (i in samples) {
    hits_summarized <- rbind(hits_summarized, result_list[[i]]$hits)
  }

  return(hits_summarized)
}
