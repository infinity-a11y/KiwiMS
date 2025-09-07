### Script collecting all functions related to calculate protein binding per sample

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
      "Protein Mw file contains ",
      length(protein_mw),
      " molecular weight value(s)."
    )
  } else {
    warning("Protein Mw file is empty")
    return(NULL)
  }

  # Return protein mw
  return(as.numeric(protein_mw))
}

# Read file containing compound mass and mass shifts
get_compound_matrix <- function(compound_file) {
  # Check if path valid
  if (!file.exists(compound_file)) {
    warning("File does not exist.")
    return(NULL)
  }

  # Read peaks.dat file
  tryCatch(
    {
      compounds <- read.delim(compound_file, header = F)
    },
    error = function(e) {
      warning("Error reading compounds file: ", e$message)
      return(NULL)
    }
  )

  # Check if data frame valid
  if (ncol(compounds) < 2) {
    warning(
      "Compounds file contains just one fields. Expected at least two: Compound_Name, Compound_Mass."
    )
    return(NULL)
  } else if (ncol(compounds) > 10) {
    warning(
      "Peaks file contains ",
      ncol(compounds),
      " fields. Only the compound name and nine mass shifts are allowed."
    )
    return(NULL)
  }

  # Check if data types correct
  if (class(compounds[, 1]) != "character") {
    warning(
      "First field (compound name) has the data type: ",
      class(compounds[, 1], ". Allowed are only characters.")
    )
    return(NULL)
  }

  if (!all(sapply(compounds[, -1], class) == "numeric")) {
    warning(
      "Mass fields have the data type(s): ",
      paste(unique(sapply(compounds[, -1], class)), collapse = ", "),
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

  # Inform comopund list dimensions
  message(
    nrow(compounds),
    " compounds with up to ",
    ncol(compounds) - 1,
    " mass shifts imported."
  )
  return(compounds_matrix)
}

# Fill out all possible complex mw
add_compound_mw <- function(protein_mw, compound_mw, tolerance = 1) {
  # Check protein_mw
  if (!is.list(protein_mw) && length(protein_mw)) {
    warning("'protein_mw' needs to be a non-empty list object.")
    return(NULL)
  }

  # Check compound_mw
  if (!is.data.frame(compound_mw) && length(compound_mw)) {
    warning("'protein_mw' needs to be a non-empty data frame.")
    return(NULL)
  }

  # Check tolerance
  if (tolerance <= 0) {
    warning("Tolerance can not be zero or negative.")
    return(NULL)
  }

  # Extend protein list with complex mw data frame
  for (protein in seq_along(names(protein_mw))) {
    for (prot_variant in 1:length(protein_mw[[protein]]$MolWeightDa)) {
      variant_name <- paste0(names(protein_mw)[protein], "_", prot_variant)

      protein_mw[[protein]][[variant_name]] <- list()

      # Add protein Mw to compound Mw's
      complex_mw <- compound_mw[, -compound] +
        protein_mw[[protein]]$MolWeightDa[prot_variant]

      upper <- complex_mw + tolerance
      lower <- complex_mw - tolerance
      rownames(lower) <- rownames(upper) <- compound_mw$compound
      protein_mw[[protein]][[variant_name]] <- complex_mw
    }
  }

  message("Added compound masses to ", length(protein_mw), " protein(s).")
  return(protein_mw)
}

check_hits <- function(
  protein_mw,
  compound_mw,
  peaks,
  peak_tolerance = 2,
  max_multiples = 4
) {
  # Keep only peaks above protein mw
  peaks_valid <- peaks$mass >= protein_mw - peak_tolerance
  if (!any(peaks_valid)) {
    warning("No protein peak detected.")
    return(NULL)
  }
  peaks_filtered <- peaks[peaks_valid, ]

  # Fill multiples matrix
  for (i in 1:max_multiples) {
    if (i == 1) {
      mat <- compound_mw * i
      colnames(mat) <- paste0(colnames(compound_mw), "*", i)
    } else {
      multiple <- compound_mw * i
      colnames(multiple) <- paste0(colnames(multiple), "*", i)
      mat <- cbind(mat, multiple)
    }
  }
  # Addition of protein mw with multiples matrix
  complex_mat <- mat + protein_mw

  # Prepare empy hits_df
  hits_df <- data.frame(
    peak = numeric(),
    intensity = numeric(),
    compound = character(),
    cmp_mass = character(),
    multiple = integer()
  )

  # Fill hits_df
  for (j in 1:nrow(peaks_filtered)) {
    upper <- peaks_filtered$mass[j] + peak_tolerance
    lower <- peaks_filtered$mass[j] - peak_tolerance

    hits <- complex_mat >= lower & complex_mat <= upper

    if (any(hits, na.rm = TRUE)) {
      indices <- which(hits, arr.ind = TRUE)
      indices <- indices[order(rownames(indices)), ]

      for (k in 1:nrow(indices)) {
        multiple <- as.numeric(sub(".*\\*", "", colnames(hits)[k]))

        hits_add <- data.frame(
          peak = peaks_filtered[j, "mass"],
          intensity = peaks_filtered[j, "intensity"],
          compound = rownames(indices)[k],
          cmp_mass = mat[indices[k, 1], indices[k, 2]],
          multiple = multiple
        )

        hits_df <- rbind(hits_df, hits_add)
      }
    }
  }

  return(hits_df)
}

get_result_hits <- function(
  results,
  protein_mw_file,
  compound_mw_file,
  peak_tolerance,
  max_multiples
) {
  samples <- head(names(results), -2)
  protein_mw <- get_protein_mw(protein_mw_file)
  compound_mw <- get_compound_matrix(compound_mw_file)

  for (i in seq_along(samples)) {
    message("Checking hits for ", samples[i])
    results[[samples[i]]][["hits"]] <- check_hits(
      protein_mw = protein_mw,
      compound_mw = compound_mw,
      peaks = get_peaks(result_sample = samples[i], results = results),
      peak_tolerance = peak_tolerance,
      max_multiples = max_multiples
    )

    message(nrow(results[[samples[i]]][["hits"]]), " hit(s) found in peaks")
  }

  return(results)
}

###################################################
# intensitäten aufsummieren -> 100 %
# prot signal intenstität (einzeln) / gesamtintensität

# Compounds
# 1. Unterschiedliche massenshifts
# 2. multiple bindungen -> vielfache von compound MW (! jeweils pro massenshift)

# Protein MW = 1000
# Compound MW = 10|11

# Peaks: 1000(IA), 1010(IB), 1020(IC), 1011(ID)

# Gesamtintensität: IA + IB + IC + ID = Itotal

# nicht umgesetztes / ungebundenes Protein: IA / Itotal * 100
# Plausibilitätscheck check result < 100

# %BinIB = IB / Itotal * 100
# %BinIC = IC / Itotal * 100
# usw ....
#
# %Bintotal = %BinIB + %BinIC + %BinID  (alles was nicht freies Prot ist)
