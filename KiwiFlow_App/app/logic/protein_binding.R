# Define reuslt path
result <- "C:\\Users\\marian\\Desktop\\VALB0008_VALX+BAY-6666666_1_1_rawdata_unidecfiles"

# Read peaks.dat file
peaks <- read.delim(
  file.path(result, "VALB0008_VALX+BAY-6666666_1_1_rawdata_peaks.dat"),
  header = F,
  sep = " "
)

# Set names
names(peaks) <- c("mass", "intensity")

# Dummy protein list with masses
protein_mw <- list(
  cathepsin = c(37701, 35487),
  kras = c(11543),
  mcml = c(34618, 37287)
)

# Dummy compound list with masses
compound_mw <- list(alpha = c(65, 70), beta = c(2.5, 2.0))

# Fill out all possible complex mw
get_complex_mw <- function(protein_mw, compound_mw) {
  # Make complex list
  complex_mw <- list()

  for (protein in seq_along(names(protein_mw))) {
    for (compound in seq_along(names(compound_mw))) {
      for (prot_variant in seq_along(protein_mw[[protein]])) {
        protein_name <- paste0(
          names(protein_mw)[protein],
          "~",
          prot_variant
        )

        for (cmp_variant in seq_along(compound_mw[[compound]])) {
          compound_name <- paste0(
            names(compound_mw)[compound],
            "~",
            cmp_variant
          )

          # Get combined name
          complex_name <- paste0(protein_name, "//", compound_name)

          # Add complex mass
          protein_mass <- protein_mw[[protein]][prot_variant]
          compound_mass <- compound_mw[[compound]][cmp_variant]
          complex_mw[[complex_name]] <- protein_mass + compound_mass
        }
      }
    }
  }

  return(complex_mw)
}

complex_mw <- get_complex_mw(protein_mw = protein_mw, compound_mw = compound_mw)

# Add tolerance to peak mw
add_peak_tolerance <- function(peak_mass, tolerance = 1) {
  lower <- peak_mass - tolerance
  upper <- peak_mass + tolerance

  return(list(lower = lower, upper = upper))
}

peaks_tolerance <- add_peak_tolerance(peaks$mass)

check_hits <- function(masses, peaks) {
  # Check argument validity
  if (length(masses) == 0) {
    warning("Zero length masses mw.")
    return(NULL)
  }
  if (length(peaks) == 0) {
    warning("Zero length peak mw.")
    return(NULL)
  }
  if (!is.list(masses)) {
    warning("'masses' is not a list.")
    return(NULL)
  }
  if (!is.list(peaks)) {
    warning("'peaks' is not a list.")
    return(NULL)
  }
  if (any(lapply(masses, length) == 0)) {
    warning(
      names(masses)[which(lapply(masses, length) == 0)],
      " has zero length."
    )
    return(NULL)
  }

  # Declare lower and upper peak tolerance values
  if (length(peaks) == 2) {
    lower <- peaks[[1]]
    upper <- peaks[[2]]
  } else if (length(peaks) == 1) {
    upper <- lower <- peaks[[1]]
  } else {
    warning("Invalid peak list length.")
    return(NULL)
  }

  hit_list <- list()

  for (i in seq_along(names(masses))) {
    for (j in seq_along(masses[[i]])) {
      # Initially NA for peak mass values
      lower_range <- NA
      upper_range <- NA
      hits <- 0

      # Check if hits in tolerance range
      hits <- masses[[i]][j] >= lower & masses[[i]][j] <= upper

      if (any(hits)) {
        if (!sum(hits) > 1) {
          lower_range <- lower[which(hits)]
          upper_range <- upper[which(hits)]
        }
      }
    }

    hit_df <- data.frame(
      mass = masses[[i]],
      n_hits = sum(hits),
      lower = lower_range,
      upper = upper_range
    )

    hit_list[[names(masses)[i]]] <- hit_df
  }

  return(hit_list)
}

protein_hits <- check_hits(protein_mw, peaks_tolerance)
compound_hits <- check_hits(compound_mw, peaks_tolerance)
complex_hits <- check_hits(complex_mw, peaks_tolerance)
