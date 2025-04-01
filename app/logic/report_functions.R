# app/logic/report_functions.R

box::use(
  data.table[fread, setnames, data.table, as.data.table],
)

box::use(
  app /
    logic /
    deconvolution_functions[
      spectrum_plot,
    ],
)

#' @export
generate_decon_rslt <- function(
  paths,
  log = NULL,
  output = NULL,
  heatmap = NULL
) {
  # Optimized file reader function
  read_file_safe <- function(filename, col_names = NULL) {
    if (!file.exists(filename)) return(data.frame())
    df <- fread(
      filename,
      header = FALSE,
      sep = " ",
      fill = TRUE,
      showProgress = FALSE
    )
    if (!is.null(col_names)) setnames(df, col_names)
    return(df)
  }

  process_path <- function(path) {
    rslt_folder <- gsub(".raw", "_rawdata_unidecfiles", path)
    raw_name <- gsub("_unidecfiles", "", basename(rslt_folder))

    if (!dir.exists(rslt_folder)) return(list())

    # Read config file
    conf_df <- read_file_safe(file.path(
      rslt_folder,
      paste0(raw_name, "_conf.dat")
    ))
    if (nrow(conf_df) > 0) {
      conf_df <- as.data.table(t(conf_df))
      setnames(conf_df, as.character(conf_df[1, ]))
      conf_df <- conf_df[-1, , drop = FALSE]
    }

    # Read other files
    peaks_df <- read_file_safe(
      file.path(rslt_folder, paste0(raw_name, "_peaks.dat")),
      c("mass", "intensity")
    )
    error_df <- read_file_safe(file.path(
      rslt_folder,
      paste0(raw_name, "_error.txt")
    ))

    if (nrow(error_df) > 0) {
      key_value_pairs <- strsplit(error_df$V1, " = ")
      error_df <- data.table(
        Key = vapply(key_value_pairs, `[`, 1, FUN.VALUE = character(1)),
        Value = vapply(key_value_pairs, `[`, 2, FUN.VALUE = character(1))
      )
      error_df[, Value := as.numeric(Value)]
    }

    # Read large files
    rawdata_df <- read_file_safe(file.path(
      rslt_folder,
      paste0(raw_name, "_rawdata.txt")
    ))
    mass_df <- read_file_safe(
      file.path(rslt_folder, paste0(raw_name, "_mass.txt")),
      c("mass", "intensity")
    )
    input_df <- read_file_safe(file.path(
      rslt_folder,
      paste0(raw_name, "_input.dat")
    ))

    decon_spec <- spectrum_plot(
      rslt_folder,
      raw = FALSE,
      interactive = FALSE
    )
    raw_spec <- spectrum_plot(
      rslt_folder,
      raw = TRUE,
      interactive = FALSE
    )

    return(list(
      config = conf_df,
      decon_spec = decon_spec,
      raw_spec = raw_spec,
      peaks = peaks_df,
      error = error_df,
      rawdata = rawdata_df,
      mass = mass_df,
      input = input_df
    ))
  }

  results <- lapply(paths, process_path)
  names(results) <- basename(paths)
  results[["session"]] <- log
  results[["output"]] <- output
  if (file.exists(file.path(getwd(), "results/heatmap.rds")))
    results[["heatmap"]] <- readRDS(file.path(getwd(), "results/heatmap.rds"))

  return(results)
}
