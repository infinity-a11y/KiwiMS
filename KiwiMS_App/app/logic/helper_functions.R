# app/logic/helper_functions.R

box::use(
  fs[path_home, dir_ls],
  ggplot2,
  grid[gpar, grid.text, unit],
  httr[add_headers, content, GET, status_code],
  minpack.lm[nlsLM],
  plyr[ddply, rename],
  readxl[read_excel],
  shiny[div, HTML, icon, NS, span],
  stringr[str_split_fixed],
)

# Unexpected error aware observer
#' @export
safe_observe <- function(
  event_expr = NULL,
  observer_name = "Unknown Observer",
  handler_fn,
  ...
) {
  event_quoted <- rlang::enquo(event_expr)

  handle_error <- function(e) {
    if (inherits(e, "shiny.silent.error")) {
      return(NULL)
    }

    message("--- KiwiMS SYSTEM ERROR ---")
    message("Location: ", observer_name)
    err_msg <- conditionMessage(e)
    message(
      "Error: ",
      if (nchar(err_msg) > 0) err_msg else "No message provided by R"
    )

    shinyjs::runjs(
      'document.getElementById("blocking-overlay").style.display = "none";'
    )
    shiny::removeModal()
    shiny::showModal(shiny::modalDialog(
      title = shiny::span(
        "⚠️ System Error",
        style = "color: #d9534f; font-weight: bold;"
      ),
      shiny::tags$p(shiny::HTML(paste0(
        "An unexpected error occurred in <b>",
        observer_name,
        "</b>.<br>",
        "KiwiMS needs to be restarted to ensure data integrity."
      ))),
      shiny::tags$hr(),
      shiny::tags$b("Error Details:"),
      shiny::tags$pre(
        if (nchar(err_msg) > 0) err_msg else "Check R console for traceback.",
        style = "background-color: #f8f9fa; padding: 10px; border: 1px solid #ddd;"
      ),
      easyClose = FALSE,
      footer = shiny::tagList(
        shiny::tags$button(
          "Terminate Session",
          class = "btn btn-danger",
          onclick = "
            document.getElementById('blocking-overlay').style.display = 'block';
            Shiny.setInputValue('app-quit_kiwims', Math.random(), {priority: 'event'});
            setTimeout(function() { window.open('', '_self', ''); window.close(); }, 1000);"
        )
      )
    ))
  }

  # Logic to wrap the handler
  wrapped_handler <- function() {
    if (Sys.getenv("KIWIMS_DEV_MODE") == "TRUE") {
      handler_fn()
    } else {
      tryCatch(
        {
          handler_fn()
        },
        error = handle_error
      )
    }
  }

  # Execution
  if (rlang::quo_is_null(event_quoted)) {
    return(shiny::observe(
      {
        wrapped_handler()
      },
      ...
    ))
  } else {
    return(shiny::observeEvent(
      rlang::eval_tidy(event_quoted),
      {
        wrapped_handler()
      },
      ignoreInit = TRUE,
      ...
    ))
  }
}

#' @export
get_kiwims_version <- function() {
  # Get version file from static directory
  version_file <- readLines("app/static/version.txt")

  # Assign names
  names(version_file) <- c("version", "date", "url")

  # Clean values
  version_file[1] <- gsub("version=", "", version_file[1])
  version_file[2] <- gsub("release_date=", "", version_file[2])
  version_file[3] <- gsub("zip_url=", "", version_file[1])

  return(version_file)
}

#' @export
get_volumes <- function() {
  # Get the path to the user's home directory
  home_path <- path_home()

  # Initialize an empty named vector for the roots
  roots <- c(Home = home_path)

  # Detect the operating system
  os <- Sys.info()['sysname']

  if (os == "Windows") {
    # Use PowerShell with Get-CimInstance to get logical disk drives
    drives_raw <- system(
      "powershell -command \"Get-CimInstance Win32_LogicalDisk | Select-Object DeviceID | ForEach-Object { $_.DeviceID }\"",
      intern = TRUE
    )

    # Clean the output to get just the drive letters (e.g., "C:")
    drives_list <- drives_raw[drives_raw != ""]
    drives_list <- trimws(drives_list)
    drives_list <- drives_list[grepl("^[A-Z]:$", drives_list)]

    # Create a named vector from the list of drives
    drive_names <- substr(drives_list, 1, 1)
    drive_values <- paste0(drives_list, "/")

    # Set the names and append to the roots vector
    names(drive_values) <- drive_names
    roots <- c(roots, drive_values)
  } else {
    # For macOS/Linux, drives are at the root level
    drives_list <- dir_ls(path = "/", type = "directory")

    # Base name of the path as the name for the vector
    drives_names <- basename(drives_list)
    names(drives_list) <- drives_names

    # Append the detected drives to the roots vector
    roots <- c(roots, drives_list)
  }

  return(roots)
}

#' @export
fill_empty <- function(string) {
  if (nchar(string) == 0) {
    gsub("", "%~%", string)
  } else {
    string_pre <- gsub(" ", "%~%", string)
    gsub("\n", "%~%", string_pre)
  }
}

#' @export
check_github_version <- function(
  repo_url = "https://raw.githubusercontent.com/infinity-a11y/KiwiMS/master/KiwiMS_App/resources/version.txt"
) {
  tryCatch(
    {
      # Fetch the version.txt file from the GitHub repository
      response <- GET(repo_url)

      # Check if the request was successful
      if (status_code(response) != 200) {
        stop(
          "Failed to fetch version.txt. HTTP status code: ",
          status_code(response)
        )
      }

      # Read the content of the file
      content <- content(response, as = "text", encoding = "UTF-8")

      # Split content into lines and extract the first line
      lines <- strsplit(content, "\n")[[1]]
      if (length(lines) == 0 || nchar(trimws(lines[1])) == 0) {
        stop("version.txt is empty or has no valid version number.")
      }

      # Return the version number (first line, trimmed)
      return(trimws(lines[1]))
    },
    error = function(e) {
      message("Error fetching version: ", e$message)
      return(NULL)
    }
  )
}

#' @export
get_latest_release_url <- function(repo = "infinity-a11y/KiwiMS") {
  tryCatch(
    {
      # Construct the GitHub URL for the latest release
      api_url <- paste0(
        "https://github.com/",
        repo,
        "/releases/latest"
      )

      # Fetch the latest release data
      response <- httr::GET(
        api_url
      )

      # Check if the request was successful
      if (httr::status_code(response) != 200) {
        stop(
          "Failed to fetch latest release. HTTP status code: ",
          httr::status_code(response)
        )
      } else {
        return(api_url)
      }
    },
    error = function(e) {
      message("Error fetching latest release URL: ", e$message)
      return(NULL)
    }
  )
}

# New version of length which can handle NA's
length2 <- function(x, na_rm = FALSE) {
  if (na_rm) sum(!is.na(x)) else length(x)
}

#' @export
summarySE <- function(
  data = NULL,
  measurevar,
  groupvars = NULL,
  na.rm = FALSE,
  conf.interval = .95,
  .drop = TRUE
) {
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  fun <- function(xx, col) {
    c(
      N = length2(xx[[col]], na.rm = na.rm),
      mean = mean(xx[[col]], na.rm = na.rm),
      sd = sd(xx[[col]], na.rm = na.rm)
    )
  }

  datac <- ddply(data, groupvars, .drop = .drop, .fun = fun, measurevar)

  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N) # Calculate standard error of the mean
  ciMult <- qt(conf.interval / 2 + .5, datac$N - 1)
  datac$ci <- datac$se * ciMult

  return(datac)
}

#' @export
kobs_matrix <- function(kobs_input, units, tmp_dir) {
  unit_str <- gsub(" ", "", units)
  kobs_matrix <- NULL

  kobs_valid_conc <- kobs_input[which(kobs_input$concentration_plot != 0), ]

  for (sample in unique(kobs_valid_conc$sample_conc)) {
    subset <- kobs_input[which(kobs_input$sample_conc == sample), ]
    subset_dummy <- subset
    subset_dummy$Binding <- 0.0
    subset_dummy$time_plot <- 0
    subset_dummy$Well <- "XX"
    subset <- rbind(subset, subset_dummy)
    nonlin_mod <- nlsLM(
      formula = as.numeric(Binding) ~
        100 *
        (v /
          kobs *
          (1 -
            exp(
              -kobs * as.numeric(time_plot)
            ))),
      start = c(v = 1, kobs = 0.001),
      data = subset
    )

    kobs_matrix <- rbind(
      kobs_matrix,
      data.frame(
        sample_conc = sample,
        kobs = summary(nonlin_mod)$parameters[2, 1]
      )
    )
  }

  kobs_matrix$predict_kinact <- 0
  kobs_matrix$sample <- str_split_fixed(kobs_matrix$sample_conc, "_", 2)[, 1]
  kobs_matrix$conc <- as.numeric(
    str_split_fixed(kobs_matrix$sample_conc, "_", 2)[, 2]
  )
  colnames(kobs_matrix)[1] <- "sampleID"
  write.table(
    kobs_matrix,
    file = paste0(tmp_dir, "/kobs_matrix_", unit_str, ".txt"),
    sep = "\t",
    col.names = TRUE,
    row.names = FALSE,
    quote = FALSE
  )
  # maditr function maditr::dcast() not compatible with project due to license incompatibilities (GPL-2 only)
  # kobs_matrix_final <- dcast(
  #   kobs_matrix,
  #   formula = sample ~ conc,
  #   value.var = "kobs"
  # )
  replace_colnames <- paste0(
    "concentration ",
    colnames(kobs_matrix_final)[2:ncol(kobs_matrix_final)]
  )

  if (units == "M - seconds") {
    colnames(kobs_matrix_final)[2:ncol(kobs_matrix_final)] <- paste0(
      replace_colnames,
      "M"
    )
  } else {
    colnames(kobs_matrix_final)[2:ncol(kobs_matrix_final)] <- paste0(
      replace_colnames,
      "uM"
    )
  }

  write.table(
    kobs_matrix_final,
    file = paste0(tmp_dir, "/kobs_table_", unit_str, ".tab"),
    sep = "\t",
    quote = FALSE,
    col.names = TRUE,
    row.names = FALSE
  )

  return(kobs_matrix)
}

#' @export
kobs_modelled <- function(kobs_input) {
  modelled_values <- NULL
  fitted_values <- NULL

  kobs_valid_conc <- kobs_input[which(kobs_input$concentration_plot != 0), ]

  for (sample in unique(kobs_valid_conc$sample_conc)) {
    subset <- kobs_input[which(kobs_input$sample_conc == sample), ]
    subset_dummy <- subset
    subset_dummy$Binding <- 0.0
    subset_dummy$time_plot <- 0
    subset_dummy$Well <- "XX"
    subset <- rbind(subset, subset_dummy)

    nonlin_mod <- nlsLM(
      formula = as.numeric(Binding) ~
        100 *
        (v /
          kobs *
          (1 -
            exp(
              -kobs * as.numeric(time_plot)
            ))),
      start = c(v = 1, kobs = 0.001),
      data = subset
    )

    fitted_values <- rbind(
      fitted_values,
      data.frame(sample.name = sample, summary(nonlin_mod)$parameters)
    )
    modelled_values <- rbind(
      modelled_values,
      data.frame(
        sample_conc = sample,
        time = seq(
          0,
          max(kobs_input$time_plot),
          1
        ),
        value = predict(
          nonlin_mod,
          data.frame(
            time_plot = seq(
              0,
              max(kobs_input$time_plot),
              1
            )
          )
        )
      )
    )
  }

  modelled_values$conc <- str_split_fixed(modelled_values$sample_conc, "_", 2)[,
    2
  ]
  modelled_values$compound <- str_split_fixed(
    modelled_values$sample_conc,
    "_",
    2
  )[, 1]

  return(modelled_values)
}

#' @export
make_kinact_matrix <- function(kobs, units, tmp_dir) {
  kobs_matrix_predict <- NULL
  kinact_matrix <- NULL
  unit_str <- gsub(" ", "", units)

  if (units == "M - seconds") {
    start_values <- c(kinact = 0.001, KI = 0.000001)
  } else {
    start_values <- c(kinact = 1000, KI = 10)
  }

  for (sample in unique(kobs$sample)) {
    subset <- kobs[which(kobs$sample == sample), ]
    subset_dummy <- subset[1, ]
    subset_dummy$kobs <- 0
    subset_dummy$conc <- 0
    subset <- rbind(subset, subset_dummy)
    subset <- subset[order(subset$conc), ]

    # fix subotopimal workaround
    if (any(subset$kobs > 1)) {
      subset <- subset[-which(subset$kobs > 1), ]
    }

    nonlin_mod2 <- minpack.lm::nlsLM(
      formula = kobs ~ (kinact * conc) / (KI + conc),
      data = subset,
      start = start_values
    )

    kinact_matrix <- rbind(
      kinact_matrix,
      data.frame(
        sample = sample,
        KI = summary(nonlin_mod2)$parameters[2, 1],
        Kinact = summary(nonlin_mod2)$parameters[1, 1]
      )
    )
  }

  kinact_matrix$KI <- kinact_matrix$KI
  kinact_matrix$Kinact_KI <- kinact_matrix$Kinact / kinact_matrix$KI
  write.table(
    kinact_matrix,
    file = paste0(tmp_dir, "/KI_table_", unit_str, ".tab"),
    sep = "\t",
    quote = FALSE,
    col.names = TRUE,
    row.names = FALSE
  )

  return(kinact_matrix)
}

#' @export
modelled_kobs <- function(kobs, kobs_input, units, tmp_dir) {
  kobs_matrix_predict <- NULL
  kinact_matrix <- NULL

  if (units == "M - seconds") {
    start_values <- c(kinact = 0.001, KI = 0.000001)
    steps <- 0.000001
  } else {
    start_values <- c(kinact = 1000, KI = 10)
    steps <- 1
  }

  for (sample in unique(kobs$sample)) {
    subset <- kobs[which(kobs$sample == sample), ]
    subset_dummy <- subset[1, ]
    subset_dummy$kobs <- 0
    subset_dummy$conc <- 0
    subset <- rbind(subset, subset_dummy)
    subset <- subset[order(subset$conc), ]

    #dirty hack (needs to be fixed)
    if (any(subset$kobs > 1)) {
      subset <- subset[-which(subset$kobs > 1), ]
    }

    nonlin_mod2 <- nlsLM(
      formula = kobs ~ (kinact * conc) / (KI + conc),
      data = subset,
      start = start_values
    )

    kobs_matrix_predict <- rbind(
      kobs_matrix_predict,
      data.frame(
        sample = sample,
        conc = seq(0, max(kobs_input$concentration_plot), steps),
        value = predict(
          nonlin_mod2,
          data.frame(
            conc = seq(0, max(kobs_input$concentration_plot), steps)
          )
        )
      )
    )

    #print(kobs_matrix_predict)
  }

  return(kobs_matrix_predict)
}

#' @export
make_kobs_plots <- function(
  kobs_input,
  modelled_values_kobs,
  sele_sample,
  units
) {
  subset_kobs <- kobs_input[which(kobs_input$compound == sele_sample), ]
  subset_kobs_se <- summarySE(
    subset_kobs,
    measurevar = "Binding",
    groupvars = c("time_plot", "concentration"),
    na.rm = TRUE
  )
  subset_kobs_modelled <- modelled_values_kobs[
    which(
      modelled_values_kobs$compound == sele_sample
    ),
  ]

  if (units == "M - seconds") {
    concentration_numbers <- sort(
      unique(as.numeric(as.character(gsub(
        "M",
        "",
        subset_kobs_se$concentration
      ))))
    )
    subset_kobs_se$labels <- factor(
      subset_kobs_se$concentration,
      levels = paste0(concentration_numbers, "M")
    )
    xlab_new <- "time [s]"
    breaks_adjust <- seq(0, 21600, 3600)
  } else {
    concentration_numbers <- sort(
      unique(as.numeric(as.character(gsub(
        "uM",
        "",
        subset_kobs_se$concentration
      ))))
    )
    subset_kobs_se$labels <- factor(
      gsub("uM", "\U003BCM", subset_kobs_se$concentration),
      levels = paste0(concentration_numbers, "\U003BCM")
    )
    xlab_new <- "time [min]"
    breaks_adjust <- seq(0, 360, 60)
  }

  p <- ggplot2$ggplot(
    data = subset_kobs_se,
    ggplot2$aes(x = time_plot, y = Binding, group = labels)
  ) +
    ggplot2$geom_point(ggplot2$aes(shape = labels)) +
    ggplot2$geom_line(
      data = subset_kobs_modelled,
      ggplot2$aes(x = time, y = value, group = conc)
    ) +
    ggplot2$geom_errorbar(
      ggplot2$aes(ymin = Binding - se, ymax = Binding + se),
      colour = "black",
      width = 5
    ) +
    ggplot2$ylab("relative Binding [%]") +
    ggplot2$xlab(xlab_new) +
    ggplot2$ggtitle(sele_sample) +
    ggplot2$expand_limits(y = 0) +
    ggplot2$scale_x_continuous(breaks = breaks_adjust) +
    ggplot2$theme_classic(base_size = 13) +
    ggplot2$theme(
      legend.position = "bottom",
      legend.title = ggplot2$element_blank()
    )

  return(p)
}

#' @export
make_kobs_plots_png <- function(
  kobs_input,
  modelled_values_kobs,
  sele_sample,
  units,
  tmp_dir
) {
  unit_str <- gsub(" ", "", units)
  kobs_input <- kobs_input[which(kobs_input$compound != "protein"), ]

  for (sele_sample in unique(kobs_input$compound)) {
    subset_kobs <- kobs_input[which(kobs_input$compound == sele_sample), ]
    subset_kobs_se <- na.omit(
      summarySE(
        subset_kobs,
        measurevar = "Binding",
        groupvars = c("time_plot", "concentration")
      )
    )
    subset_kobs_modelled <- modelled_values_kobs[
      which(
        modelled_values_kobs$compound == sele_sample
      ),
    ]

    if (units == "M - seconds") {
      concentration_numbers <- sort(
        unique(as.numeric(as.character(gsub(
          "M",
          "",
          subset_kobs_se$concentration
        ))))
      )
      subset_kobs_se$labels <- factor(
        subset_kobs_se$concentration,
        levels = paste0(concentration_numbers, "M")
      )
      xlab_new <- "time [s]"
      breaks_adjust <- seq(0, 21600, 3600)
    } else {
      concentration_numbers <- sort(
        unique(as.numeric(as.character(gsub(
          "uM",
          "",
          subset_kobs_se$concentration
        ))))
      )
      subset_kobs_se$labels <- factor(
        gsub("uM", "\U003BCM", subset_kobs_se$concentration),
        levels = paste0(concentration_numbers, "\U003BCM")
      )
      xlab_new <- "time [min]"
      breaks_adjust <- seq(0, 360, 60)
    }

    p <- ggplot2$ggplot(
      data = subset_kobs_se,
      ggplot2$aes(x = time_plot, y = Binding, group = labels)
    ) +
      ggplot2$geom_point(ggplot2$aes(shape = labels)) +
      ggplot2$geom_line(
        data = subset_kobs_modelled,
        ggplot2$aes(x = time, y = value, group = conc)
      ) +
      ggplot2$geom_errorbar(
        ggplot2$aes(
          ymin = Binding - se,
          ymax = Binding + se
        ),
        colour = "black",
        width = 5
      ) +
      ggplot2$ylab("relative Binding [%]") +
      ggplot2$xlab(xlab_new) +
      ggplot2$ggtitle(sele_sample) +
      ggplot2$expand_limits(y = 0) +
      ggplot2$scale_x_continuous(breaks = breaks_adjust) +
      ggplot2$theme_classic(base_size = 13) +
      ggplot2$theme(
        legend.position = "bottom",
        legend.title = ggplot2$element_blank()
      )

    png(
      file = paste0(tmp_dir, "/plots/Kobs_", sele_sample, unit_str, ".png"),
      bg = "transparent",
      width = 12,
      height = 9,
      units = "cm",
      res = 600,
      pointsize = 12
    )
    print(p)
    dev.off()
  }
}

#' @export
make_KI_plots <- function(
  kobs,
  kobs_matrix_predict,
  kinact_matrix,
  sele_sample,
  units
) {
  subset_kinact <- kobs[which(kobs$sample == sele_sample), ]
  subset_kinact_modelled <- kobs_matrix_predict[
    which(
      kobs_matrix_predict$sample == sele_sample
    ),
  ]

  if (units == "M - seconds") {
    xlab_new <- ~ paste("conc. [M]")
    ylab_new <- ~ paste("k obs [s"^-1, "]")
  } else {
    xlab_new <- ~ paste("conc. [\U003BCM]")
    ylab_new <- ~ paste("k obs [min"^-1, "]")
  }

  if (nrow(subset_kinact_modelled) > 0) {
    q <- ggplot2$ggplot(data = subset_kinact, ggplot2$aes(x = conc, y = kobs)) +
      ggplot2$geom_point() +
      ggplot2$geom_line(
        data = subset_kinact_modelled,
        ggplot2$aes(x = conc, y = value, color = "black")
      ) +
      ggplot2$theme_classic(base_size = 13) +
      ggplot2$labs(x = xlab_new, y = ylab_new) +
      ggplot2$ggtitle(sele_sample) +
      ggplot2$scale_color_manual(values = "black") +
      ggplot2$theme(
        legend.position = "none",
        plot.margin = unit(c(1, 1, 2, 1), "cm")
      )
  } else {
    q <- ggplot2$ggplot(data = subset_kinact, ggplot2$aes(x = conc, y = kobs)) +
      ggplot2$geom_point() +
      ggplot2$theme_classic(base_size = 13) +
      ggplot2$labs(x = xlab_new, y = ylab_new) +
      ggplot2$ggtitle(sele_sample) +
      ggplot2$scale_color_manual(values = "black") +
      ggplot2$theme(
        legend.position = "none",
        plot.margin = unit(c(1, 1, 2, 1), "cm")
      )
  }

  return(q)
}

#' @export
make_KI_plots_png <- function(
  kobs,
  kobs_matrix_predict,
  kinact_matrix,
  sele_sample,
  units,
  tmp_dir
) {
  unit_str <- gsub(" ", "", units)
  kobs <- kobs[which(kobs$sample != "protein"), ]

  for (sele_sample in unique(kobs$sample)) {
    subset_kinact <- kobs[which(kobs$sample == sele_sample), ]
    subset_kinact_modelled <- kobs_matrix_predict[
      which(
        kobs_matrix_predict$sample == sele_sample
      ),
    ]

    if (units == "M - seconds") {
      xlab_new <- ~ paste("conc. [M]")
      ylab_new <- ~ paste("k obs [s"^-1, "]")
    } else {
      xlab_new <- ~ paste("conc. [\U003BCM]")
      ylab_new <- ~ paste("k obs [min"^-1, "]")
    }

    if (nrow(subset_kinact_modelled) > 0) {
      q <- ggplot2$ggplot(
        data = subset_kinact,
        ggplot2$aes(x = conc, y = kobs)
      ) +
        ggplot2$geom_point() +
        ggplot2$geom_line(
          data = subset_kinact_modelled,
          ggplot2$aes(x = conc, y = value, color = "black")
        ) +
        ggplot2$theme_classic(base_size = 13) +
        ggplot2$labs(x = xlab_new, y = ylab_new) +
        ggplot2$ggtitle(sele_sample) +
        ggplot2$scale_color_manual(values = "black") +
        ggplot2$theme(
          legend.position = "none",
          plot.margin = unit(c(1, 1, 2, 1), "cm")
        )
    } else {
      q <- ggplot2$ggplot(
        data = subset_kinact,
        ggplot2$aes(x = conc, y = kobs)
      ) +
        ggplot2$geom_point() +
        ggplot2$theme_classic(base_size = 13) +
        ggplot2$labs(x = xlab_new, y = ylab_new) +
        ggplot2$ggtitle(sele_sample) +
        ggplot2$scale_color_manual(values = "black") +
        ggplot2$theme(
          legend.position = "none",
          plot.margin = unit(c(1, 1, 2, 1), "cm")
        )
    }

    png(
      file = paste0(tmp_dir, "/plots/KI_", sele_sample, unit_str, ".png"),
      bg = "transparent",
      width = 13,
      height = 10,
      units = "cm",
      res = 600,
      pointsize = 12
    )
    print(q)
    if (units == "M - seconds") {
      Kinact <- round(
        kinact_matrix[which(kinact_matrix$sample == sele_sample), "Kinact"],
        4
      )
      KI <- round(
        kinact_matrix[which(kinact_matrix$sample == sele_sample), "KI"],
        8
      )

      grid.text(
        substitute(
          K[inact] == a * "s"^-1 * "       " ~ K[i] == b * "M",
          list(b = KI, a = Kinact)
        ),
        x = unit(.2, "npc"),
        y = unit(.1, "npc"),
        just = c("left", "bottom"),
        gp = gpar(fontface = "bold", fontsize = 13, col = "black")
      )
    } else {
      Kinact <- round(
        kinact_matrix[which(kinact_matrix$sample == sele_sample), "Kinact"],
        2
      )
      KI <- round(
        kinact_matrix[which(kinact_matrix$sample == sele_sample), "KI"],
        4
      )

      grid.text(
        substitute(
          K[inact] == a * "min"^-1 * "       " ~ K[i] == b * ~ mu * "M",
          list(b = KI, a = Kinact)
        ),
        x = unit(.2, "npc"),
        y = unit(.1, "npc"),
        just = c("left", "bottom"),
        gp = gpar(fontface = "bold", fontsize = 13, col = "black")
      )
    }

    dev.off()
  }
}

# ---- Experiment config helpers ----

#' @export
read_config_file <- function(path, ext) {
  if (ext == "xlsx") {
    as.data.frame(read_excel(path))
  } else {
    first_line <- readLines(path, n = 1, warn = FALSE)
    sep <- if (grepl(";", first_line)) ";" else ","
    utils::read.csv(path, sep = sep, stringsAsFactors = FALSE)
  }
}

#' @export
normalize_colnames <- function(df) {
  nms <- trimws(names(df))
  nms <- gsub("\\s+", "_", nms)
  nms <- gsub("_+", "_", nms)
  names(df) <- nms
  df
}

#' @export
validate_config <- function(df) {
  issues <- character()
  required_cols <- c("Sample", "Protein")
  numeric_cols <- c("Compound_Concentration", "Incubation_Time")
  compound_pattern <- "^Compound_\\d+$"

  missing_req <- setdiff(required_cols, names(df))
  if (length(missing_req) > 0) {
    issues <- c(issues, paste("Missing required columns:", paste(missing_req, collapse = ", ")))
  }

  compound_cols <- grep(compound_pattern, names(df), value = TRUE)
  if (length(compound_cols) == 0) {
    issues <- c(issues, "No compound columns found (Compound_1 \u2013 Compound_5); at least one required.")
  } else {
    dup_rows <- which(apply(df[compound_cols], 1, function(row) {
      vals <- row[!is.na(row) & trimws(as.character(row)) != ""]
      anyDuplicated(vals) > 0
    }))
    if (length(dup_rows) > 0) {
      issues <- c(issues, paste0("Duplicate compound names in row(s): ", paste(dup_rows, collapse = ", "), "."))
    }
  }

  for (col in required_cols) {
    if (col %in% names(df)) {
      bad <- is.na(df[[col]]) | trimws(as.character(df[[col]])) == ""
      if (any(bad)) {
        issues <- c(issues, paste0("'", col, "': ", sum(bad), " missing value(s)."))
      }
    }
  }

  if ("Sample" %in% names(df)) {
    dups <- df[["Sample"]][duplicated(df[["Sample"]])]
    if (length(dups) > 0) {
      issues <- c(issues, paste0("'Sample': duplicate value(s): ", paste(unique(dups), collapse = ", "), "."))
    }
  }

  if ("Well" %in% names(df)) {
    empty <- is.na(df[["Well"]]) | trimws(as.character(df[["Well"]])) == ""
    if (any(empty) && !all(empty)) {
      issues <- c(issues, paste0("'Well': must be all filled or all empty (", sum(empty), " missing)."))
    } else {
      non_empty <- trimws(as.character(df[["Well"]][!empty]))
      invalid <- !grepl("^[A-Pa-p](1[0-9]|2[0-4]|[1-9])$", non_empty)
      if (any(invalid)) {
        issues <- c(issues, paste0("'Well': invalid well ID (valid range A1\u2013P24): ", paste(non_empty[invalid], collapse = ", "), "."))
      }
    }
  }

  for (col in numeric_cols) {
    if (col %in% names(df)) {
      vals <- df[[col]]
      empty <- is.na(vals) | trimws(as.character(vals)) == ""
      if (any(empty) && !all(empty)) {
        issues <- c(issues, paste0("'", col, "': must be all filled or all empty (", sum(empty), " missing)."))
      } else {
        non_empty <- vals[!empty]
        if (length(non_empty) > 0) {
          converted <- suppressWarnings(as.numeric(as.character(non_empty)))
          if (any(is.na(converted))) {
            issues <- c(issues, paste0("'", col, "' contains non-numeric values."))
          }
        }
      }
    }
  }

  issues
}

#' @export
config_badge <- function(type, label, body = NULL) {
  bg <- if (type == "ok") "#7CB342" else "#D17050"
  badge <- paste0(
    '<span class="config-badge-pill" style="background:', bg, ';">', label, "</span>"
  )
  detail <- if (!is.null(body)) {
    paste0(
      '<span class="config-badge-detail">',
      paste0(body, collapse = " \u00b7 "), "</span>"
    )
  } else ""
  div(class = "config-badge-wrapper", HTML(paste0(badge, detail)))
}
