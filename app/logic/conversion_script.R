# app/logic/conversion_script.R

box::use(
  shiny[need, validate],
  shinyalert[shinyalert],
  stringr[str_split, str_split_fixed],
  tidyr[spread],
)

#create list of spectra per sample

#' @export
get_Spectra <- function(input, sample_number, peak_number) {
  start_line <- which(input == paste("Sample", sample_number)) + 1
  peaks <- data.frame(Mass = numeric(), Intensity = numeric())

  while (
    grepl("Sample", input[start_line]) == FALSE &&
      start_line < length(input)
  ) {
    if (input[start_line] == paste("Peak", peak_number, "Spectrum")) {
      peak_line <- start_line + 2
      while (input[peak_line] != "") {
        peaks <- rbind(
          peaks,
          data.frame(
            Mass = as.numeric(unlist(str_split(input[peak_line], "\t", 2))[1]),
            Intensity = as.numeric(unlist(str_split(input[peak_line], "\t", 2))[
              2
            ])
          )
        )
        peak_line <- peak_line + 1
      }
    }

    start_line <- start_line + 1
  }

  topSpectra <- peaks[order(peaks$Intensity, decreasing = TRUE), ]
  return(topSpectra)
}

#helper functions
comp <- function(a, b, noise_cmpd) {
  # noise = int(sys.argv[5])
  # noise = 4
  return(abs(a - b) < noise_cmpd)
}

# allow +- 15Da when assigning protein peak
compProt <- function(a, b, noise_protein) {
  #noise = int(sys.argv[6]) #15 was to large?!
  #noise = 10
  return(abs(a - b) < noise_protein)
}

# allow +- 8Da when assigning (non-compound) protein adducts
#
# compAdd <- function(a,b)
# {
#   #noise = int(sys.argv[7])
#   noise = 8
#   return (abs(a-b)<noise)
# }

#' @export
assign_complex_peaks <- function(
  topSpectra,
  n_label,
  protein_mass,
  sample,
  noise_cmpd
) {
  multi_labelling <- data.frame()

  for (i in 1:n_label) {
    multi_labelling <- rbind(
      multi_labelling,
      data.frame(
        n_label = i,
        topSpectra,
        Mass_Protein_Compound_Compare = sapply(
          topSpectra$Mass,
          function(current_mass) {
            comp(
              current_mass,
              protein_mass + (i * sample$MW_Compound),
              noise_cmpd
            )
          }
        )
      )
    )
  }

  # spread
  new_labelling_df <- spread(
    data = multi_labelling,
    n_label,
    Mass_Protein_Compound_Compare
  )

  # double and more modifications - only MW of interest no adduct
  spectrum_bound <- new_labelling_df[
    which(
      new_labelling_df$ID %in%
        multi_labelling[
          which(
            multi_labelling$Mass_Protein_Compound_Compare == TRUE
          ),
          "ID"
        ]
    ),
  ]

  spectrum_bnd_sel <- (ncol(spectrum_bound) - n_label + 1):ncol(spectrum_bound)
  colnames(spectrum_bound)[spectrum_bnd_sel] <- paste0(
    "Mass_Protein_Compound_Compare.",
    tail(colnames(spectrum_bound), n_label)
  )

  # process multi_labelled
  if (nrow(spectrum_bound) > 0) {
    spectrum_bound$label <- sample$CompoundName
    topSpectra[
      which(
        topSpectra$ID %in% spectrum_bound$ID
      ),
    ]$label <- sample$CompoundName
    total <- sum(topSpectra[which(topSpectra$label != ""), ]$Intensity)
  }

  # restructure spectrum  bound - consider multilabelling
  if (nrow(spectrum_bound) > 1) {
    binding <- sum(spectrum_bound$Intensity) / total
  } else if (nrow(spectrum_bound) == 1) {
    binding <- spectrum_bound$Intensity / total
  } else {
    binding <- 0
  }

  if (nrow(spectrum_bound) > 0) {
    new_spectrum_bound <- data.frame(binding = binding)

    for (i in 1:n_label) {
      new_spectrum_bound$label <- sample$CompoundName

      if (
        any(
          spectrum_bound[, paste0("Mass_Protein_Compound_Compare.", i)] == TRUE
        )
      ) {
        validate(
          need(
            nrow(spectrum_bound[
              which(
                spectrum_bound[, paste0("Mass_Protein_Compound_Compare.", i)] ==
                  TRUE
              ),
            ]) ==
              1,
            "Multiple bound peaks found, adjust noise level or protein mass."
          )
        )

        new_spectrum_bound[, paste0("Intensity_n", i)] <- spectrum_bound[
          which(
            spectrum_bound[,
              paste0("Mass_Protein_Compound_Compare.", i)
            ] ==
              TRUE
          ),
          "Intensity"
        ]
        new_spectrum_bound[, paste0("Mass_n", i)] <- spectrum_bound[
          which(
            spectrum_bound[,
              paste0("Mass_Protein_Compound_Compare.", i)
            ] ==
              TRUE
          ),
          "Mass"
        ]
      } else {
        new_spectrum_bound[, paste0("Intensity_n", i)] <- 0
        new_spectrum_bound[, paste0("Mass_n", i)] <- 0
      }
    }
  } else {
    new_spectrum_bound <- data.frame()
  }

  return(new_spectrum_bound)
}

#' @export
assign_sample <- function(
  raw_data,
  i,
  sample,
  protein_mass,
  n_spectra,
  n_label,
  noise_protein,
  noise_cmpd
) {
  sample$well <- unlist(str_split(gsub(",", "", sample$Vial), ":"))[2]
  topSpectra <- head(
    get_Spectra(
      raw_data,
      as.numeric(sample$Sample),
      as.numeric(sample$`Peak Number`)
    ),
    n = n_spectra
  )
  baseChange <- 0
  badWell <- 0
  foundNotLabeled <- 0
  if (nrow(topSpectra) > 0) {
    max_intensity <- topSpectra[which.max(topSpectra$Intensity), "Intensity"]
    topSpectra$normalized_Intensity <- topSpectra$Intensity / max_intensity
    topSpectra$mass_without_protein <- topSpectra$Mass - protein_mass
    topSpectra$label <- ""

    if (i != 1) {
      topSpectra <- topSpectra[
        which(
          topSpectra$mass_without_protein > 100 |
            (topSpectra$mass_without_protein < 15 &
              topSpectra$mass_without_protein > -15)
        ),
      ]

      for (spectrum_number in c(seq_len(nrow(topSpectra)))) {
        spectrum <- topSpectra[spectrum_number, ]

        if (
          spectrum$normalized_Intensity > 0.8 &&
            spectrum$mass_without_protein < 100 &&
            spectrum$mass_without_protein > 15
        ) {
          baseChange <- as.numeric(spectrum[2]) - protein_mass
        }

        if (spectrum$normalized_Intensity > 0.45) badWell <- badWell + 1
      }
    }
  }

  topSpectra$Mass_Protein_Compare <- sapply(
    topSpectra$Mass,
    function(current_mass) compProt(current_mass, protein_mass, noise_protein)
  )
  topSpectra[
    which(topSpectra$Mass_Protein_Compare == TRUE),
    "label"
  ] <- "protein"
  topSpectra$ID <- seq_len(nrow(topSpectra))

  # assign protein peak
  if (any(topSpectra$Mass_Protein_Compare == TRUE)) {
    topSpectra_sub <- topSpectra[
      which(
        topSpectra$Mass_Protein_Compare == TRUE
      ),
    ]
    protein_mass <- topSpectra_sub[
      which.min(
        topSpectra_sub$mass_without_protein
      ),
    ]$Mass
    foundNotLabeled <- foundNotLabeled + 1
  }

  spectrum_unbound <- topSpectra[
    which(
      topSpectra$Mass_Protein_Compare == TRUE
    ),
  ]

  # validate(
  #   need(
  #     nrow(spectrum_unbound) == 1,
  #     'Multiple unbound peaks found, adjust noise level or protein mass.'))

  # assign protein-compound peak --> compound bound?
  spectrum_bound <- assign_complex_peaks(
    topSpectra,
    n_label,
    protein_mass,
    sample,
    noise_cmpd
  )

  #initialize df
  final_df <- data.frame(
    SampleNo = i,
    Well = sample$well,
    SampleName_long = sample$File,
    CompoundName = "",
    MW_Compound = sample$MW_Compound,
    Binding = 0,
    MW_Protein = protein_mass,
    Intensity_Protein = 0,
    Comment = ""
  )

  for (i in 1:n_label) {
    final_df[, paste0("Intensity_Complex_n", i)] <- 0
    final_df[, paste0("MW_Protein_Comp_n", i)] <- 0
  }

  if (nrow(spectrum_unbound) > 1) {
    spectrum_unbound <- spectrum_unbound[
      which.max(
        spectrum_unbound$Intensity
      ),
    ]
    final_df$Comment <- paste(
      final_df$Comment,
      "Similiar protein mass found twice"
    )
  }

  if (nrow(spectrum_bound) == 0 && nrow(spectrum_unbound) == 0) {
    final_df$Comment <- paste(final_df$Comment, "No match!")
  } else if (nrow(spectrum_bound) == 0) {
    final_df$CompoundName <- "protein"
    final_df$Intensity_Protein <- spectrum_unbound$Intensity
  } else if (nrow(spectrum_unbound) == 0) {
    final_df$CompoundName <- spectrum_bound$label
    final_df$Binding <- spectrum_bound$binding * 100

    for (i in 1:n_label) {
      final_df[, paste0(
        "Intensity_Complex_n",
        i
      )] <- spectrum_bound[, paste0("Intensity_n", i)]
      final_df[, paste0(
        "MW_Protein_Comp_n",
        i
      )] <- spectrum_bound[, paste0("Mass_n", i)]
    }

    final_df$Comment <- paste(final_df$Comment, "No unlabeled!")
  } else {
    final_df$CompoundName <- spectrum_bound$label
    final_df$Binding <- spectrum_bound$binding * 100
    final_df$Intensity_Protein <- spectrum_unbound$Intensity

    for (i in 1:n_label) {
      final_df[, paste0(
        "Intensity_Complex_n",
        i
      )] <- spectrum_bound[, paste0("Intensity_n", i)]
      final_df[, paste0(
        "MW_Protein_Comp_n",
        i
      )] <- spectrum_bound[, paste0("Mass_n", i)]
    }
  }

  if (badWell > 4) final_df$Comment <- paste(final_df$Comment, "Bad well!")

  if (baseChange > 1)
    final_df$Comment <- paste(
      final_df$Comment,
      "BaseChange",
      round(baseChange, 3),
      "!"
    )
  return(final_df)
}

#' @export
calculate_conversions <- function(
  raw_data,
  MW,
  protein_mass,
  tmp,
  noise_cmpd,
  noise_protein,
  n_label,
  n_spectra
) {
  # get first occurrence of term Sample to extract sample information
  start_sample_matrix <- grep("Sample", raw_data)[1]
  sample_line_counter <- start_sample_matrix + 1
  sample_information <- data.frame()

  while (raw_data[sample_line_counter] != "") {
    sample_information <- rbind(
      sample_information,
      unlist(str_split(raw_data[sample_line_counter], "\t", 9))
    )
    sample_line_counter <- sample_line_counter + 1
  }

  colnames(sample_information) <- unlist(
    str_split(raw_data[start_sample_matrix], "\t", 9)
  )

  start_peak_info <- grep("Peak", raw_data)[1]
  peak_line_counter <- start_peak_info
  peak_information <- data.frame()
  sample_counter <- 1

  while (sample_counter <= nrow(sample_information)) {
    if (grepl("Sample", raw_data[peak_line_counter]) == TRUE) {
      peak_line_counter <- peak_line_counter + 2
      sample_counter <- sample_counter + 1
    } else if (raw_data[peak_line_counter] == "") {
      peak_line_counter <- peak_line_counter + 1
    } else if (grepl("Peak", raw_data[peak_line_counter]) == TRUE) {
      peak_line_counter <- peak_line_counter + 1
    } else {
      peaks <- c(
        unlist(str_split(raw_data[peak_line_counter], "\t", 15)),
        sample_counter
      )
      peak_information <- rbind(peak_information, peaks)
      #peak_information$Sample = sample_counter
      peak_line_counter <- peak_line_counter + 1
    }
  }

  colnames(peak_information) <- c(
    unlist(str_split(raw_data[start_peak_info], "\t", 14)),
    "sampleNo"
  )

  # eventually no peaks reach 100% area %BP - in this case ignore sample?
  # correct column?
  peak_information.filt <- peak_information[
    which(
      as.numeric(peak_information$`Area %BP`) == 100
    ),
  ]

  write.table(
    sample_information,
    file.path(tmp, "sample_info.txt"),
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )
  write.table(
    peak_information.filt,
    file.path(tmp, "peak_info.txt"),
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )

  collection_infos <- cbind(
    sample_information[
      match(
        as.numeric(peak_information.filt$sampleNo),
        as.numeric(sample_information$Sample)
      ),
    ],
    peak_information.filt
  )

  if (nrow(sample_information) != nrow(peak_information.filt)) {
    #print("Number of peaks does not agree with number of samples")
    shinyalert("Number of peaks does not agree with number of samples")
    samples_without_peaks <- data.frame(
      sample_information[
        which(
          !(as.numeric(sample_information$Sample) %in%
            as.numeric(peak_information.filt$sampleNo))
        ),
      ]
    )
  } else {
    samples_without_peaks <- data.frame()
  }

  #Add sample name - Compound Concentration...
  collection_infos$CompoundName <- str_split_fixed(
    collection_infos$File,
    "\\+",
    2
  )[, 2]
  collection_infos$MW_Compound <- MW[
    match(
      str_split_fixed(collection_infos$CompoundName, "_", 2)[, 1],
      MW$V1
    ),
    "V2"
  ]

  combined_df <- data.frame()

  for (i in seq_len(nrow(collection_infos))) {
    combined_df <- rbind(
      combined_df,
      assign_sample(
        raw_data,
        i,
        collection_infos[i, ],
        protein_mass,
        n_spectra,
        n_label,
        noise_protein,
        noise_cmpd
      )
    )
  }

  #print(length(samples_without_peaks))
  if (nrow(samples_without_peaks) > 0) {
    samples_without_peaks.full <- data.frame(
      SampleNo = as.numeric(samples_without_peaks$Sample),
      Well = str_split_fixed(
        gsub(",", "", samples_without_peaks$Vial),
        ":",
        2
      )[, 2],
      SampleName_long = samples_without_peaks$File,
      CompoundName = str_split_fixed(
        str_split_fixed(samples_without_peaks$File, "\\+", 2)[, 2],
        "_",
        4
      )[, 1],
      MW_Compound = "",
      Binding = 0,
      MW_Protein = 0,
      Intensity_Protein = 0,
      Comment = "No peaks found!"
    )

    for (i in 1:n_label) {
      samples_without_peaks.full[, paste0("Intensity_Complex_n", i)] <- 0
      samples_without_peaks.full[, paste0("MW_Protein_Comp_n", i)] <- 0
    }

    combined_df <- rbind(combined_df, samples_without_peaks.full)
  }

  combined_df <- combined_df[order(combined_df$SampleNo), ]

  write.table(
    combined_df,
    file.path(tmp, "conversion_table.txt"),
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )

  return(combined_df)
}
