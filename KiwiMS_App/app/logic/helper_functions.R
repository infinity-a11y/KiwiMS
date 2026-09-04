# app/logic/helper_functions.R

box::use(
  fs[path_home, dir_ls],
  ggplot2,
  grid[gpar, grid.text, unit],
  httr[add_headers, content, GET, status_code, timeout],
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
get_kiwims_version <- function(
  path = "resources/version.txt"
) {
  # resources/version.txt is the single source of truth for the app version.
  # Parse it as key=value so the caller is not tied to the line order, and so
  # extra keys can be added later without breaking anything.
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]

  keys <- trimws(sub("=.*$", "", lines))
  values <- trimws(sub("^[^=]*=", "", lines))
  names(values) <- keys

  # Expose the keys under the short names the app has always used.
  c(
    version = unname(values["version"]),
    date = unname(values["release_date"]),
    url = unname(values["zip_url"])
  )
}

# Drive detection for the file pickers.
#
# The rule the whole section is built on: never touch a drive to find out that
# it exists. A disconnected mapped network drive answers a stat() only after
# Windows has given up on reconnecting the SMB session, which takes tens of
# seconds, single-threaded, with the Shiny session blocked behind it. Both
# earlier implementations broke that rule in different ways - one probed
# A:/ through Z:/ with dir.exists(), the other shelled out to a WMI query with
# no timeout and no fallback, so a machine where the query was slow or blocked
# by policy either started slowly or silently ended up with no drives at all.
#
# Three layers, cheapest first:
#   1. volumes.txt, written by the launcher before R starts (no cost here).
#   2. A live query, capped by a timeout, for dev sessions and stale caches.
#   3. Home alone, logged loudly, so the picker is never empty.

# How long the launcher's file stays trustworthy. It is written seconds before
# R starts, so this only has to cover launcher-write to first-session-init; a
# generous window absorbs a slow first launch while Defender scans the install.
volume_cache_max_age_mins <- 60

volume_cache <- new.env(parent = emptyenv())

# "<label>\t<path>" per line, as written by dev/launch.ps1.
read_volume_file <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  age <- difftime(Sys.time(), file.mtime(path), units = "mins")
  if (is.na(age) || age > volume_cache_max_age_mins) {
    return(NULL)
  }

  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) {
    return(NULL)
  }

  # The launcher writes ASCII precisely so there is no BOM, but strip one anyway
  # rather than let a future encoding change produce a root named "<BOM>C:".
  bom <- rawToChar(as.raw(c(0xEF, 0xBB, 0xBF)))
  if (startsWith(lines[1], bom)) {
    lines[1] <- substring(lines[1], nchar(bom) + 1L)
  }

  parts <- str_split_fixed(lines, "\t", 2)
  keep <- nzchar(parts[, 1]) & nzchar(parts[, 2])
  if (!any(keep)) {
    return(NULL)
  }

  stats::setNames(parts[keep, 2], parts[keep, 1])
}

# [System.IO.DriveInfo]::GetDrives() wraps the Win32 GetLogicalDrives() bitmask:
# it reads the mount table and touches no filesystem. IsReady does touch the
# device, which is how an empty card reader or optical drive is filtered out -
# cheap locally, but never called on a network drive, for the reason above.
#
# Run from a temp script rather than -Command so nothing has to survive two
# levels of quoting. -NoProfile matters: a corporate PowerShell profile that
# autoloads modules off a share can cost seconds on its own.
query_windows_volumes <- function() {
  script <- tempfile(fileext = ".ps1")
  on.exit(unlink(script), add = TRUE)

  writeLines(
    c(
      "$ErrorActionPreference = 'Stop'",
      "foreach ($d in [System.IO.DriveInfo]::GetDrives()) {",
      "  $t = $d.DriveType.ToString()",
      "  if ($t -eq 'NoRootDirectory') { continue }",
      "  if ($t -ne 'Network' -and -not $d.IsReady) { continue }",
      "  $d.Name.TrimEnd('\\') + \"`t\" + ($d.Name -replace '\\\\$', '/')",
      "}"
    ),
    script
  )

  out <- tryCatch(
    system2(
      "powershell",
      c(
        "-NoProfile",
        "-NonInteractive",
        "-ExecutionPolicy",
        "Bypass",
        "-File",
        shQuote(script)
      ),
      stdout = TRUE,
      stderr = FALSE,
      timeout = 10
    ),
    error = function(e) NULL,
    warning = function(w) NULL
  )

  out <- out[nzchar(trimws(out))]
  if (!length(out)) {
    return(NULL)
  }

  parts <- str_split_fixed(out, "\t", 2)
  keep <- nzchar(parts[, 1]) & nzchar(parts[, 2])
  if (!any(keep)) {
    return(NULL)
  }

  stats::setNames(parts[keep, 2], parts[keep, 1])
}

#' @export
get_volumes <- function(refresh = FALSE) {
  # Resolved once per R process. The result has to be a plain named vector, not
  # a function: shinyFiles 0.9.3 re-evaluates a function root in dirGetter() and
  # parseDirPath() but subsets list(...)$roots directly inside shinyDirChoose(),
  # which errors on a closure the first time the user navigates.
  if (!refresh && !is.null(volume_cache$roots)) {
    return(volume_cache$roots)
  }

  home_path <- path_home()
  roots <- c(Home = home_path)

  if (Sys.info()[["sysname"]] == "Windows") {
    drives <- read_volume_file(
      file.path(Sys.getenv("LOCALAPPDATA"), "KiwiMS", "volumes.txt")
    )

    if (is.null(drives)) {
      drives <- query_windows_volumes()
    }

    if (is.null(drives)) {
      # Home still works, so the app stays usable - but the user cannot reach
      # anything off their profile drive, which is worth a line in the log
      # rather than a picker that is quietly missing its drives.
      message(
        "[WARN] No drives detected; the file picker will only offer Home. ",
        "See %LOCALAPPDATA%\\KiwiMS\\launch.log."
      )
    } else {
      roots <- c(roots, drives)
    }
  } else {
    # macOS/Linux: drives are directories at the root level.
    drives_list <- dir_ls(path = "/", type = "directory")
    names(drives_list) <- basename(drives_list)
    roots <- c(roots, drives_list)
  }

  # A duplicate root name makes shinyFiles resolve the wrong one, and Home is
  # usually on a drive that is listed in its own right.
  roots <- roots[!duplicated(names(roots))]

  volume_cache$roots <- roots
  roots
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

# Both version checks below run synchronously in the server function, so the
# user stares at the loading screen until they return. curl has no connect
# timeout by default: on a machine that is offline behind a firewall that
# drops packets rather than refusing them, that is minutes of dead wait. Cap
# it - a failed check just means the update banner is skipped.

#' @export
check_github_version <- function(
  repo_url = "https://raw.githubusercontent.com/infinity-a11y/KiwiMS/master/KiwiMS_App/resources/version.txt"
) {
  tryCatch(
    {
      # Fetch the version.txt file from the GitHub repository
      response <- GET(repo_url, timeout(5))

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
        api_url,
        httr::timeout(5)
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
    return(as.data.frame(read_excel(path)))
  }

  # Excel on Windows commonly saves CSVs in the system codepage (e.g.
  # Windows-1252) rather than UTF-8, so bytes like the micro sign are not
  # valid UTF-8 continuation sequences on their own. read.csv() errors out
  # ("invalid multibyte string") on such files instead of just reading them,
  # so detect that up front and re-decode as Latin-1/CP1252 before parsing.
  raw <- readBin(path, "raw", file.info(path)$size)
  txt <- rawToChar(raw, multiple = FALSE)
  if (validUTF8(txt)) {
    Encoding(txt) <- "UTF-8"
  } else {
    Encoding(txt) <- "latin1"
    txt <- enc2utf8(txt)
  }

  first_line <- strsplit(txt, "\r?\n")[[1]][1]
  sep <- if (grepl(";", first_line)) ";" else ","
  utils::read.csv(text = txt, sep = sep, stringsAsFactors = FALSE)
}

# Excel and most keyboards produce the micro sign (U+00B5) for "µ", not
# the Greek mu (U+03BC) that config_unit_choices matches against — the two
# render identically, so uploads were silently rejected as "unknown unit".
# Normalize both unit columns right after read so the rest of the app only
# ever sees the Greek mu.
#' @export
normalize_config_units <- function(df) {
  for (col in c("Concentration_Unit", "Time_Unit")) {
    if (col %in% names(df)) {
      df[[col]] <- gsub("µ", "μ", as.character(df[[col]]))
    }
  }
  df
}

#' @export
normalize_colnames <- function(df) {
  nms <- trimws(names(df))
  nms <- gsub("\\s+", "_", nms)
  nms <- gsub("_+", "_", nms)
  names(df) <- nms
  df
}

# Excel and most keyboards produce the micro sign (U+00B5) for "µ", not
# the Greek mu (U+03BC) that config_unit_choices matches against — the two
# render identically, so real-world uploads were silently rejected as
# "unknown unit". Normalize both unit columns before validation so the rest
# of the app only ever sees the Greek mu.
#' @export
normalize_config_units <- function(df) {
  for (col in c("Concentration_Unit", "Time_Unit")) {
    if (col %in% names(df)) {
      df[[col]] <- gsub("µ", "μ", as.character(df[[col]]))
    }
  }
  df
}

# Optional config columns naming the unit of the numeric column they belong to.
#' @export
config_unit_choices <- list(
  Concentration_Unit = c("M", "mM", "μM", "nM", "pM"),
  Time_Unit = c("s", "min")
)

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

  # Concentration/time are plain numbers, so the config may name the unit they
  # were measured in. One value for the whole file — a config mixing units in
  # one column would silently misscale the kinetics.
  for (col in names(config_unit_choices)) {
    if (!col %in% names(df)) {
      next
    }
    vals <- trimws(as.character(df[[col]]))
    vals <- vals[!is.na(vals) & vals != ""]
    if (length(vals) == 0) {
      next
    }
    allowed <- config_unit_choices[[col]]
    if (length(unique(vals)) > 1) {
      issues <- c(issues, paste0(
        "'", col, "': must be the same for every row (found ",
        paste(unique(vals), collapse = ", "), ")."
      ))
    } else if (!unique(vals) %in% allowed) {
      issues <- c(issues, paste0(
        "'", col, "': unknown unit '", unique(vals), "' (allowed: ",
        paste(allowed, collapse = ", "), ")."
      ))
    }
  }

  # Replicate: optional free-text group label; partial fill is allowed.

  issues
}

# Pulls the declared units out of a confirmed config, as
# list(conc = <symbol or NULL>, time = <symbol or NULL>).
#' @export
config_units <- function(cfg) {
  pick <- function(col) {
    if (is.null(cfg) || !col %in% names(cfg)) {
      return(NULL)
    }
    vals <- trimws(as.character(cfg[[col]]))
    vals <- unique(vals[!is.na(vals) & vals != ""])
    if (length(vals) != 1 || !vals %in% config_unit_choices[[col]]) {
      return(NULL)
    }
    vals
  }
  list(conc = pick("Concentration_Unit"), time = pick("Time_Unit"))
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
