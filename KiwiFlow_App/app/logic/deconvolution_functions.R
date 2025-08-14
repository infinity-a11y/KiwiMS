# app/logic/deconvolution_functions.R

box::use(
  data.table[fread, setnames, data.table, as.data.table],
  dplyr[left_join, mutate, n_distinct],
  ggplot2,
  parallel[
    clusterExport,
    clusterEvalQ,
    detectCores,
    makeCluster,
    parLapply,
    stopCluster
  ],
  plotly[config, event_register, ggplotly, hide_colorbar, layout, style],
  reticulate[use_condaenv, use_python, py_config, py_run_string],
  scales[percent_format],
  utils[read.delim, read.table],
)

# Processing a single waters dir
#' @export
process_single_dir <- function(
  waters_dir,
  result_dir,
  startz,
  endz,
  minmz,
  maxmz,
  masslb,
  massub,
  massbins,
  peakthresh,
  peakwindow,
  peaknorm,
  time_start,
  time_end
) {
  input_path <- gsub("\\\\", "/", waters_dir)
  result_dir <- gsub("\\\\", "/", result_dir)

  # Function to properly format parameters for Python
  format_param <- function(x) {
    if (is.character(x) && x == "") {
      return("''")
    } else {
      return(as.character(x))
    }
  }

  # Create parameters string for Python
  params_string <- sprintf(
    paste0(
      '"startz": %s, "endz": %s, "minmz": %s, "maxmz": %s, "masslb": %s',
      ', "massub": %s, "massbins": %s, "peakthresh": %s, "peakwindow": ',
      '%s, "peaknorm": %s, "time_start": %s, "time_end": %s'
    ),
    format_param(startz),
    format_param(endz),
    format_param(minmz),
    format_param(maxmz),
    format_param(masslb),
    format_param(massub),
    format_param(massbins),
    format_param(peakthresh),
    format_param(peakwindow),
    format_param(peaknorm),
    format_param(time_start),
    format_param(time_end)
  )

  # Set up Conda environment
  tryCatch(
    {
      # Run Python code
      reticulate::py_run_string(sprintf(
        '
import sys
import unidec
import re
import os

# Parameters passed from R
params = {%s}
input_file = r"%s"
result_dir = r"%s"
      
# Initialize UniDec engine
engine = unidec.UniDec()

# Convert Waters .raw to txt
engine.raw_process(input_file)
    
# Move processed file to output directory
txt_file = input_file.removesuffix(".raw") + "_rawdata.txt"
output = os.path.join(result_dir, os.path.basename(txt_file))
os.rename(txt_file, output)

# Make result directory
engine.open_file(output)

# Set configuration parameters
engine.config.startz = params["startz"]
engine.config.endz = params["endz"]
engine.config.minmz = params["minmz"]
engine.config.maxmz = params["maxmz"]
engine.config.masslb = params["masslb"]
engine.config.massub = params["massub"]
engine.config.massbins = params["massbins"]
engine.config.peakthresh = params["peakthresh"]
engine.config.peakwindow = params["peakwindow"]
engine.config.peaknorm = params["peaknorm"]
engine.config.time_start = params["time_start"]
engine.config.time_end = params["time_end"]

# Process and deconvolve the data
engine.process_data()
engine.run_unidec()
engine.pick_peaks()
',
        params_string,
        input_path,
        result_dir
      ))

      # Save spectra
      result <- file.path(
        result_dir,
        gsub(".raw", "_rawdata_unidecfiles", basename(waters_dir))
      )

      if (dir.exists(result)) {
        plots <- list(
          decon_spec = spectrum_plot(result, FALSE),
          raw_spec = spectrum_plot(result, TRUE)
        )
        saveRDS(plots, file.path(result, "plots.rds"))
      }
    },
    error = function(e) {
      cat("Error in process_single_dir for", waters_dir, ":", e$message, "\n")
    }
  )
}

#' @export
deconvolute <- function(
  raw_dirs,
  result_dir,
  num_cores = detectCores() - 1,
  startz = 1,
  endz = 50,
  minmz = "",
  maxmz = "",
  masslb = 5000,
  massub = 500000,
  massbins = 10,
  peakthresh = 0.1,
  peakwindow = 500,
  peaknorm = 1,
  time_start = "",
  time_end = ""
) {
  # List of all parameters to pass to workers
  params_list <- list(
    result_dir = result_dir,
    startz = startz,
    endz = endz,
    minmz = minmz,
    maxmz = maxmz,
    masslb = masslb,
    massub = massub,
    massbins = massbins,
    peakthresh = peakthresh,
    peakwindow = peakwindow,
    peaknorm = peaknorm,
    time_start = time_start,
    time_end = time_end
  )

  # Evaluate processing mode: parallel or sequential
  if (length(raw_dirs) > 20 && num_cores > 1) {
    # Set up the cluster
    cl <- parallel::makeCluster(num_cores)
    on.exit(parallel::stopCluster(cl))

    # Initialize workers
    parallel::clusterEvalQ(cl, {
      tryCatch(
        {
          reticulate::use_condaenv("kiwiflow", required = TRUE)
          TRUE
        },
        error = function(e) FALSE
      )
    })

    # Export only what's needed
    parallel::clusterExport(cl, c("process_single_dir"), envir = environment())

    message(paste0("Using ", num_cores, " cores for parallel processing."))

    # Create wrapper function that includes all parameters
    process_wrapper <- function(dir, params) {
      do.call(process_single_dir, c(list(waters_dir = dir), params))
    }

    # Run parLapply with error handling
    parallel::parLapply(cl, raw_dirs, function(dir) {
      tryCatch(
        {
          process_wrapper(dir, params_list)
        },
        error = function(e) {
          message("Error processing ", dir, ": ", e$message)
          NULL
        }
      )
    })
  } else {
    use_condaenv("kiwiflow", required = TRUE)

    message("Sequential processing started.")
    tryCatch(
      {
        for (dir in seq_along(raw_dirs)) {
          process_single_dir(
            raw_dirs[dir],
            result_dir,
            startz,
            endz,
            minmz,
            maxmz,
            masslb,
            massub,
            massbins,
            peakthresh,
            peakwindow,
            peaknorm,
            time_start,
            time_end
          )
        }
      },
      error = function(e) {
        message("Error in sequential processing for dir ", dir, ": ", e$message)
        return(NULL)
      }
    )
  }
}

#' @export
create_384_plate_heatmap <- function(data) {
  # Create plate layout coordinates
  rows <- rev(LETTERS[1:16])
  cols <- 1:24
  plate_layout <- expand.grid(row = rows, col = cols) |>
    mutate(well_id = paste0(row, col))

  # Merge data with plate layout
  plate_data <- left_join(plate_layout, data, by = "well_id")

  # Tooltip text creation
  plate_data <- plate_data |>
    mutate(
      value_fmt = ifelse(is.na(value), "NA", sprintf("%.2f", value)),
      sample_fmt = ifelse(is.na(sample), "Empty", as.character(sample)),
      tooltip_text = sprintf(
        "Well: %s\nValue: %s\nSample: %s",
        well_id,
        value_fmt,
        sample_fmt
      )
    )

  num_unique_values <- n_distinct(plate_data$value, na.rm = TRUE)

  if (num_unique_values == 1) {
    left <- 80
  } else {
    left <- 0
  }

  # Create the heatmap
  plate_plot <- ggplot2$ggplot(
    plate_data,
    ggplot2$aes(x = col, y = factor(row, levels = rev(rows)), fill = value)
  ) +
    ggplot2$geom_rect(
      data = plate_layout,
      ggplot2$aes(
        xmin = col - 0.5,
        xmax = col + 0.5,
        ymin = match(row, rev(rows)) - 0.5,
        ymax = match(row, rev(rows)) + 0.5
      ),
      fill = NA,
      color = "black",
      linewidth = 0.5
    ) +
    suppressWarnings({
      ggplot2$geom_tile(
        ggplot2$aes(text = tooltip_text),
        width = 0.95,
        height = 0.95
      )
    }) +
    ggplot2$scale_y_discrete(limits = rows) +
    ggplot2$scale_x_continuous(
      breaks = 1:24,
      labels = 1:24,
      position = "top",
      expand = c(0, 0)
    ) +
    ggplot2$scale_fill_viridis_c(
      name = "Peak Mass [Da]",
      na.value = "white"
    ) +
    ggplot2$coord_fixed() +
    ggplot2$theme_minimal() +
    ggplot2$theme(
      axis.text.x = ggplot2$element_text(
        size = 8,
        angle = 0,
        vjust = 0,
        hjust = 0.5
      ),
      axis.text.y = ggplot2$element_text(size = 8, hjust = 1),
      axis.title = ggplot2$element_blank(),
      panel.grid = ggplot2$element_blank(),
      axis.ticks = ggplot2$element_blank()
    )

  # Convert to plotly
  interactive_plot <- ggplotly(plate_plot, tooltip = "text")

  interactive_plot <- interactive_plot |>
    layout(
      dragmode = FALSE,
      hoverlabel = list(
        bgcolor = "#38387Cdb",
        font = list(size = 14, color = "white"),
        bordercolor = "white"
      ),
      yaxis = list(
        scaleanchor = "x",
        scaleratio = 1,
        showgrid = FALSE,
        zeroline = FALSE,
        tickson = "boundaries",
        tickfont = list(size = 12),
        tickangle = 0,
        automargin = TRUE,
        title = "",
        ticklabelposition = "outside",
        ticklabeloverflow = "allow"
      ),
      xaxis = list(
        anchor = "y",
        automargin = TRUE,
        overlaying = "y",
        side = "top",
        showgrid = FALSE,
        griddash = "20px",
        zeroline = FALSE,
        tickmode = "array",
        tickvals = 1:24,
        ticktext = as.character(1:24),
        tickfont = list(size = 12),
        tickangle = 0
      ),
      margin = list(t = 40, r = 60, b = 0, l = left),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    ) |>
    config(
      displayModeBar = "hover",
      scrollZoom = FALSE,
      modeBarButtons = list(
        list(
          "zoom2d",
          "toImage",
          "autoScale2d",
          "resetScale2d",
          "zoomIn2d",
          "zoomOut2d"
        )
      ),
      toImageButtonOptions = list(
        filename = paste0(Sys.Date(), "_Plate_Heatmap")
      )
    )

  if (num_unique_values == 1) {
    interactive_plot |>
      style(
        0,
        colorscale = list(c(0, 1), c("#440154FF", "#440154FF")),
        showscale = FALSE,
        showlegend = FALSE,
        traces = 2
      ) |>
      hide_colorbar()
  } else {
    interactive_plot
  }
}

# Make spectrum plot interactively (plotly) or non-interactively (ggplot2)
#' @export
spectrum_plot <- function(
  result_path,
  raw,
  interactive = TRUE,
  bin_width = 0.01
) {
  base <- gsub("_unidecfiles", "", basename(result_path))
  raw_file <- file.path(result_path, paste0(base, "_rawdata.txt"))
  mass_file <- file.path(result_path, paste0(base, "_mass.txt"))
  peaks_file <- file.path(result_path, paste0(base, "_peaks.dat"))

  if (!file.exists(mass_file) || !file.exists(peaks_file)) {
    return()
  }

  if (raw) {
    mass <- fread(raw_file, sep = " ", col.names = c("V1", "V2"))
    mass[, bin := floor(V1 / bin_width) * bin_width + bin_width / 2]
    mass <- mass[, .(V2 = sum(V2)), by = bin]
    setnames(mass, "bin", "V1")
    mass$V2 <- (mass$V2 - min(mass$V2)) / (max(mass$V2) - min(mass$V2)) * 100
  } else {
    mass <- utils::read.delim(mass_file, sep = " ", header = FALSE)
    peaks <- utils::read.delim(peaks_file, sep = " ", header = FALSE)
    mass$V2 <- (mass$V2 - min(mass$V2)) / (max(mass$V2) - min(mass$V2)) * 100
    highlight_peaks <- mass[mass$V1 %in% peaks$V1, ]
  }

  if (!interactive) {
    plot <- ggplot2::ggplot(
      mass,
      ggplot2::aes(
        x = V1,
        y = V2,
        group = 1,
        text = paste0("Mass: ", V1, " Da\nIntensity: ", round(V2, 2))
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      ggplot2::theme_minimal()

    if (raw) {
      plot <- plot + ggplot2::labs(y = "Intensity [%]", x = "m/z [Th]")
    } else {
      plot <- plot +
        ggplot2::geom_point(
          data = highlight_peaks,
          ggplot2::aes(x = V1, y = V2),
          fill = "#e8cb97",
          colour = "#35357A",
          shape = 21,
          size = 2
        ) +
        ggplot2::labs(y = "Intensity [%]", x = "Mass [Da]")
    }
    return(plot)
  }

  if (raw) {
    plot <- plotly::plot_ly(
      mass,
      x = ~V1,
      y = ~V2,
      type = "scattergl",
      mode = "lines",
      color = I("black"),
      hoverinfo = "text",
      text = ~ paste0("Mass: ", V1, " Da\nIntensity: ", round(V2, 2), "%")
    ) |>
      plotly::layout(
        yaxis = list(
          title = "Intensity [%]",
          showgrid = TRUE,
          zeroline = FALSE,
          ticks = "outside",
          tickcolor = "transparent"
        ),
        xaxis = list(title = "m/z [Th]", showgrid = TRUE, zeroline = FALSE),
        margin = list(t = 0, r = 0, b = 0, l = 50),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
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
          filename = paste0(Sys.Date(), "_", gsub("_rawdata", "", base), "_raw")
        )
      )
  } else {
    plot <- plotly::plot_ly(
      mass,
      x = ~V1,
      y = ~V2,
      type = "scattergl",
      mode = "lines",
      color = I("black"),
      hoverinfo = "text",
      text = ~ paste0("Mass: ", V1, " Da\nIntensity: ", round(V2, 2), "%")
    ) |>
      plotly::add_markers(
        data = highlight_peaks,
        x = ~V1,
        y = ~V2,
        marker = list(
          color = "#e8cb97",
          line = list(
            color = "#35357A",
            width = 2
          ),
          symbol = "circle",
          size = 10,
          zindex = 100
        ),
        hoverinfo = "text",
        text = ~ paste0("Mass: ", V1, " Da\nIntensity: ", round(V2, 2), "%"),
        showlegend = FALSE
      ) |>
      plotly::layout(
        yaxis = list(
          title = "Intensity [%]",
          showgrid = TRUE,
          zeroline = FALSE,
          ticks = "outside",
          tickcolor = "transparent"
        ),
        xaxis = list(title = "Mass [Da]", showgrid = TRUE, zeroline = FALSE),
        margin = list(t = 0, r = 0, b = 0, l = 50),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
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
          filename = paste0(
            Sys.Date(),
            "_",
            gsub("_rawdata", "", base),
            "_deconvoluted"
          )
        )
      )
  }

  return(plot)
}

# Generate deconvolution report
#' @export
generate_decon_rslt <- function(
  paths,
  log = NULL,
  output = NULL,
  heatmap = NULL,
  result_dir
) {
  # Optimized file reader function
  read_file_safe <- function(filename, col_names = NULL) {
    if (!file.exists(filename)) {
      return(data.frame())
    }
    df <- fread(
      filename,
      header = FALSE,
      sep = " ",
      fill = TRUE,
      showProgress = FALSE
    )
    if (!is.null(col_names)) {
      setnames(df, col_names)
    }
    return(df)
  }

  process_path <- function(path) {
    rslt_folder <- gsub(".raw", "_rawdata_unidecfiles", path)
    raw_name <- gsub("_unidecfiles", "", basename(rslt_folder))

    if (!dir.exists(rslt_folder)) {
      return(list())
    }

    # Read config file
    conf_df <- read_file_safe(file.path(
      rslt_folder,
      paste0(raw_name, "_conf.dat")
    ))
    if (nrow(conf_df) > 0) {
      conf_df <- conf_df[, 1:2]
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

  paths <- file.path(result_dir, basename(paths))
  results <- lapply(paths, process_path)
  names(results) <- basename(paths)
  results[["session"]] <- log
  results[["output"]] <- output

  if (file.exists(file.path(result_dir, "heatmap.rds"))) {
    results[["heatmap"]] <- readRDS(file.path(result_dir, "heatmap.rds"))
  }

  return(results)
}
