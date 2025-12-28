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

box::use(
  app / logic / conversion_constants[warning_sym, ],
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
      # Run unidec with python
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
        gsub(".raw", "_rawdata_unidecfiles", basename(input_path))
      )

      if (dir.exists(result)) {
        plots <- list(
          decon_spec = spectrum_plot(
            result_path = result,
            raw = FALSE,
            interactive = FALSE,
            theme = "light"
          ),
          raw_spec = spectrum_plot(
            result_path = result,
            raw = TRUE,
            interactive = FALSE,
            theme = "light"
          )
        )

        saveRDS(plots, file.path(result, "plots.rds"))
      } else {
        stop()
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
  num_cores = detectCores() - 2,
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
  # Evaluate processing mode: parallel or sequential
  if (length(raw_dirs) > 20 && num_cores > 1) {
    message("Initiating ", num_cores, " cores for parallel processing ...")

    Sys.setenv(RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE")
    on.exit(Sys.unsetenv("RENV_CONFIG_SYNCHRONIZED_CHECK"), add = TRUE)

    # Validate Conda environment
    library(reticulate)
    if (!"kiwiflow" %in% conda_list()$name) {
      stop(
        "Conda environment 'kiwiflow' not found. Create it with conda_create('kiwiflow')."
      )
    } else {
      message("Conda environment 'kiwiflow' found.")
    }

    # Create log directory and define outfile
    outfile <- file.path(
      Sys.getenv("USERPROFILE"),
      "Documents",
      "KiwiFlow",
      "logs",
      "last_cluster_log.txt"
    )
    writeLines(paste("Deconvolution Cluster Output", Sys.time()), outfile)

    # Set up the cluster
    message("Inducing cluster ...")
    cl <- parallel::makeCluster(num_cores, outfile = outfile)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Initialize reticulate and Conda environment in each worker
    message("Setting python environment for each worker ...")
    invisible(capture.output(
      {
        clusterEvalQ(cl, {
          library(reticulate)

          # Create and set a unique temp dir for this worker to avoid Conda file conflicts
          unique_temp_dir <- file.path(
            tempdir(),
            paste0("conda_worker_", Sys.getpid())
          )
          dir.create(unique_temp_dir, showWarnings = FALSE, recursive = TRUE)
          Sys.setenv(TEMP = unique_temp_dir)
          Sys.setenv(TMP = unique_temp_dir)

          tryCatch(
            {
              use_condaenv("kiwiflow", required = TRUE)
              NULL
            },
            error = function(e) {
              message("Error in worker ", Sys.getpid(), ": ", e$message)
              return(NULL)
            }
          )
        })
      },
      type = "output"
    ))

    # Create wrapper function that includes all parameters
    process_wrapper <- function(dir, params) {
      do.call(process_single_dir, c(list(waters_dir = dir), params))
    }

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

    # Export environment
    message("Passing functions and parameter to the workers ...")
    parallel::clusterExport(
      cl,
      c(
        "process_single_dir",
        "spectrum_plot",
        "process_wrapper",
        "params_list"
      ),
      envir = environment()
    )

    # Run parLapply with error handling and collect results
    message("Running parallel deconvolution ...")
    results <- invisible(capture.output(
      {
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
      },
      type = "output"
    ))

    message("Parallel processing finalized.")

    # Check for errors in results
    errors <- vapply(results, is.character, logical(1))
    if (any(errors)) {
      warning(
        "Errors occurred in processing: ",
        paste(results[errors], collapse = "; ")
      )
    }
  } else {
    Sys.setenv(RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE")
    on.exit(Sys.unsetenv("RENV_CONFIG_SYNCHRONIZED_CHECK"), add = TRUE)

    # Validate Conda environment
    library(reticulate)
    if (!"kiwiflow" %in% conda_list()$name) {
      stop(
        "Conda environment 'kiwiflow' not found. Create it with conda_create('kiwiflow')."
      )
    } else {
      message("Conda environment 'kiwiflow' found.")
    }

    tryCatch(
      {
        use_condaenv("kiwiflow", required = TRUE)
      },
      error = function(e) {
        message("Error activating 'kiwiflow' environment: ", e$message)
        return(NULL)
      }
    )

    message("Sequential processing started ...")
    tryCatch(
      {
        for (dir in seq_along(raw_dirs)) {
          process_single_dir(
            waters_dir = raw_dirs[dir],
            result_dir = result_dir,
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

        message("Sequential processing finalized.")
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
      # margin = list(t = 40, r = 60, b = 0, l = left),
      margin = list(t = 0, r = 60, b = 0, l = left),
      plot_bgcolor = "#dfdfdf42",
      paper_bgcolor = "#dfdfdf42"
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

# Helper function to harmonize data for plotting
#' @export
process_plot_data <- function(
  sample = NULL,
  result_path = NULL,
  raw = FALSE,
  bin_width = 0.01
) {
  if (is.null(sample) & is.null(result_path)) {
    message(
      "Provide either the path to a '.rds' result file or a list object carrying sample results"
    )
    return(NULL)
  }

  if (!is.null(result_path)) {
    # Get file paths from deconvolution result
    base <- gsub("_unidecfiles", "", basename(result_path))
    raw_file <- file.path(result_path, paste0(base, "_rawdata.txt"))
    mass_file <- file.path(result_path, paste0(base, "_mass.txt"))
    peaks_file <- file.path(result_path, paste0(base, "_peaks.dat"))

    # Abort if files missing
    if (!file.exists(mass_file) || !file.exists(peaks_file)) {
      message("Mass or peak file missing in ", result_path)
      return()
    }

    if (raw) {
      mass <- data.table::fread(
        raw_file,
        sep = " ",
        col.names = c("mass", "intensity")
      )
      mass[, bin := floor(mass / bin_width) * bin_width + bin_width / 2]
      mass <- mass[, .(intensity = sum(intensity)), by = bin]
      data.table::setnames(mass, "bin", "mass")
      mass$intensity <- (mass$intensity - min(mass$intensity)) /
        (max(mass$intensity) - min(mass$intensity)) *
        100
      highlight_peaks <- NULL
    } else {
      # Read mass spectrum and filter zero intensity values
      mass <- utils::read.delim(
        mass_file,
        sep = " ",
        header = FALSE,
        col.names = c("mass", "intensity")
      ) |>
        dplyr::filter(intensity != 0)

      # Cut off outer limits
      mass <- mass[-c(1, nrow(mass)), ]

      # Read detected peaks
      peaks <- utils::read.delim(
        peaks_file,
        sep = " ",
        header = FALSE,
        col.names = c("mass", "intensity")
      )

      # Normalize intensities
      mass$intensity <- (mass$intensity - min(mass$intensity)) /
        (max(mass$intensity) - min(mass$intensity)) *
        100

      # Match peaks to spectrum
      highlight_peaks <- mass[mass$mass %in% peaks$mass, ]
    }
  } else if (!is.null(sample)) {
    if (is.null(sample$hits)) {
      message(
        "Sample has no annotated hits. See: 'add_hits()' applied to a result list."
      )
      return()
    }

    # Read mass spectrum and filter zero intensity values
    mass <- sample$mass |> dplyr::filter(intensity != 0) |> as.data.frame()

    # Cut off outer limits
    mass <- mass[-c(1, nrow(mass)), ]

    # Normalize intensities
    mass$intensity <- (mass$intensity - min(mass$intensity)) /
      (max(mass$intensity) - min(mass$intensity)) *
      100

    # Read peaks
    peaks <- c(
      unique(
        sample$hits$`Measured Mw Protein [Da]`
      ),
      sample$hits$`Peak [Da]`
    )

    # If duplicated peaks throw message
    if (any(duplicated(peaks))) {
      message(
        warning_sym,
        " ",
        sample$hits$Sample[1],
        " has multiple hits for ",
        sum(duplicated(peaks)),
        " compound(s)."
      )
    }

    # Match peaks to mass spectrum
    indices <- match(peaks, mass$mass)
    peak_df <- mass[indices, ]

    # Get protein and compound names
    name <- c(
      unique(
        sample$hits$Protein
      ),
      sample$hits$Compound
    )

    # Get molecular weights
    mw <- c(
      unique(
        sample$hits$`Mw Protein [Da]`
      ),
      sample$hits$`Compound Mw [Da]`
    )

    # Get stoichiometry values
    multiple <- c(1, sample$hits$`Binding Stoichiometry`)

    # Summarize in data frame
    highlight_peaks <- cbind(peak_df, name, mw, multiple) |>
      dplyr::filter(!is.na(name))
  }

  return(list(mass = mass, highlight_peaks = highlight_peaks))
}

# Make spectrum plot interactively (plotly) or non-interactively (ggplot2)
#' @export
spectrum_plot <- function(
  result_path = NULL,
  sample = NULL,
  raw = FALSE,
  interactive = TRUE,
  bin_width = 0.01,
  theme = "dark",
  color_cmp = NULL
) {
  plot_data <- process_plot_data(
    sample,
    result_path,
    raw = raw,
    bin_width = bin_width
  )

  # Theme Styling Logic
  if (tolower(theme) == "light") {
    bg_color <- "white"
    plot_bg_color <- "white"
    font_color <- "black"
    grid_color <- "rgba(0, 0, 0, 0.1)"
    zeroline_color <- "rgba(0, 0, 0, 0.5)"
    data_line_color <- "black"
  } else {
    bg_color <- "rgba(0,0,0,0)"
    plot_bg_color <- "rgba(0,0,0,0)"
    font_color <- "white"
    grid_color <- "rgba(255, 255, 255, 0.2)"
    zeroline_color <- "rgba(255, 255, 255, 0.5)"
    data_line_color <- "white"
  }

  marker_border_color <- ifelse(!is.null(color_cmp), "#000000", "#7777f9")

  # ggplot (non-interactive) section
  if (!interactive) {
    plot <- ggplot2::ggplot(
      plot_data$mass,
      ggplot2::aes(
        x = mass,
        y = intensity,
        group = 1,
        text = paste0(
          "Mass: ",
          mass,
          " Da\nIntensity: ",
          round(intensity, 2)
        )
      )
    ) +
      ggplot2::geom_line(color = data_line_color) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1))

    if (tolower(theme) == "dark") {
      plot <- plot + ggplot2::theme_dark()
    } else {
      plot <- plot + ggplot2::theme_minimal()
    }

    if (raw) {
      plot <- plot + ggplot2::labs(y = "Intensity [%]", x = "m/z [Th]")
    } else {
      plot <- plot +
        ggplot2::geom_point(
          data = plot_data$highlight_peaks,
          ggplot2::aes(x = mass, y = intensity),
          fill = "#e8cb97",
          colour = marker_border_color,
          shape = 21,
          size = 2
        ) +
        ggplot2::labs(y = "Intensity [%]", x = "Mass [Da]")
    }
    return(plot)
  }

  # PLOTLY (Interactive) Section
  if (raw) {
    plot <- plotly::plot_ly(
      plot_data$mass,
      x = ~mass,
      y = ~intensity,
      type = "scattergl",
      mode = "lines",
      color = I(data_line_color),
      hoverinfo = "text",
      text = ~ paste0(
        "Mass: ",
        mass,
        " Da\nIntensity: ",
        round(intensity, 2),
        "%"
      )
    ) |>
      plotly::layout(
        hovermode = "closest",
        paper_bgcolor = bg_color,
        plot_bgcolor = plot_bg_color,
        font = list(size = 14, color = font_color),
        yaxis = list(
          title = "Intensity [%]",
          color = font_color,
          showgrid = TRUE,
          gridcolor = grid_color,
          zeroline = FALSE,
          zerolinecolor = zeroline_color,
          ticks = "outside",
          tickcolor = "transparent"
        ),
        xaxis = list(
          title = "m/z [Th]",
          color = font_color,
          showgrid = TRUE,
          gridcolor = grid_color,
          zeroline = FALSE,
          zerolinecolor = zeroline_color
        ),
        margin = list(t = 0, r = 0, b = 0, l = 50)
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
        ))
      )
  } else {
    plot <- plotly::plot_ly(
      plot_data$mass,
      x = ~mass,
      y = ~intensity,
      type = "scattergl",
      mode = "lines",
      color = I(data_line_color),
      hoverinfo = "text",
      text = ~ paste0(
        "Mass: ",
        mass,
        " Da\nIntensity: ",
        round(intensity, 2),
        "%"
      )
    )

    if (!is.null(result_path)) {
      plot <- plotly::add_markers(
        plot,
        data = plot_data$highlight_peaks,
        x = ~mass,
        y = ~intensity,
        marker = list(
          color = "#e8cb97",
          line = list(
            color = marker_border_color,
            width = 1.5
          ),
          symbol = "circle",
          size = 12,
          zindex = 100
        ),
        hoverinfo = "text",
        text = ~ paste0(
          "Mass: ",
          mass,
          " Da\nIntensity: ",
          round(intensity, 2),
          "%"
        ),
        showlegend = FALSE
      )
    } else {
      dupl <- duplicated(plot_data$highlight_peaks$mass)
      if (any(dupl)) {
        plot_data$highlight_peaks$intensity[
          dupl
        ] <- plot_data$highlight_peaks$intensity[dupl] +
          1
      }

      color_cmp <<- color_cmp
      plot_data <<- plot_data

      if (!is.null(color_cmp)) {
        color_cmp <- c("#ffffff", color_cmp)
        names(color_cmp) <- c(
          as.character(max(unique(plot_data$highlight_peaks$mw))),
          names(color_cmp)[-1]
        )

        plot_data$highlight_peaks$mw_color <- color_cmp[match(
          as.character(plot_data$highlight_peaks$mw),
          gsub("\\.?0+$", "", gsub(" Da", "", names(color_cmp)))
        )]
      }

      plot <- plotly::add_markers(
        plot,
        data = plot_data$highlight_peaks,
        x = ~mass,
        y = ~intensity,
        marker = list(
          color = if (!is.null(color_cmp)) {
            ~ I(mw_color)
          } else {
            "#e8cb97"
          },
          line = list(
            color = marker_border_color,
            width = ifelse(!is.null(color_cmp), 1, 1.5)
          ),
          symbol = "circle",
          size = 12,
          zindex = 100
        ),
        hoverinfo = "text",
        text = ~ paste0(
          "Name: ",
          name,
          "\nMeasured: ",
          mass,
          " Da\nIntensity: ",
          round(intensity, 2),
          "%\n",
          "Theor. Mw: ",
          mw
        ),
        showlegend = FALSE
      )
    }

    plot <- plotly::layout(
      plot,
      hovermode = "closest",
      paper_bgcolor = bg_color,
      plot_bgcolor = plot_bg_color,
      font = list(size = 14, color = font_color),
      yaxis = list(
        title = "Intensity [%]",
        color = font_color,
        showgrid = TRUE,
        gridcolor = grid_color,
        zeroline = FALSE,
        zerolinecolor = zeroline_color,
        ticks = "outside",
        tickcolor = "transparent"
      ),
      xaxis = list(
        title = "Mass [Da]",
        color = font_color,
        showgrid = TRUE,
        gridcolor = grid_color,
        zeroline = FALSE,
        zerolinecolor = zeroline_color
      ),
      margin = list(t = 0, r = 0, b = 0, l = 50)
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
        ))
      )
  }

  return(plot)
}

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

# Generate deconvolution report
#' @export
generate_decon_rslt <- function(
  paths,
  log = NULL,
  output = NULL,
  heatmap = NULL,
  result_dir,
  temp_dir
) {
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

    plots <- readRDS(file.path(rslt_folder, "plots.rds"))
    # decon_spec <- spectrum_plot(
    #   result_path = rslt_folder,
    #   raw = FALSE,
    #   interactive = FALSE
    # )
    # raw_spec <- spectrum_plot(
    #   result_path = rslt_folder,
    #   raw = TRUE,
    #   interactive = FALSE
    # )

    return(list(
      config = conf_df,
      decon_spec = plots$decon_spec,
      raw_spec = plots$raw_spec,
      peaks = peaks_df,
      error = error_df,
      rawdata = rawdata_df,
      mass = mass_df,
      input = input_df
    ))
  }

  paths <- file.path(result_dir, basename(paths))
  results <- list()
  deconvolution <- lapply(paths, process_path)
  names(deconvolution) <- basename(paths)
  results[["deconvolution"]] <- deconvolution
  results[["session"]] <- log
  results[["output"]] <- output

  if (file.exists(file.path(temp_dir, "heatmap.rds"))) {
    results[["heatmap"]] <- readRDS(file.path(temp_dir, "heatmap.rds"))
  }

  return(results)
}
