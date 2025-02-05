# app/logic/helper_functions.R

box::use(
  ggplot2,
  grid[grid.text, gpar, unit],
  maditr[dcast],
  minpack.lm[nlsLM],
  plyr[ddply, rename],
  shiny[div, icon, NS, span],
  stringr[str_split_fixed],
)

#' @export
collapsiblePanelUI <- function(id, title, content) {
  ns <- NS(id)
  
  div(
    style = "border: 1px solid #ddd; margin: 10px 0;",
    div(
      style = "background-color: #f8f9fa; padding: 10px; cursor: pointer;",
      onclick = sprintf("$('#%s').slideToggle()", ns("content")),
      span(
        icon("chevron-right", class = "toggle-icon"),
        style = "margin-right: 10px;"
      ),
      title
    ),
    div(
      id = ns("content"),
      style = "padding: 15px; display: none;",
      content
    )
  )
}

# New version of length which can handle NA's: if na.rm==T, don't count them
length2 <- function(x, na_rm = FALSE) {
  if (na_rm) sum(!is.na(x)) else length(x)
}

#' @export
summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
                      conf.interval = .95, .drop = TRUE) {
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  fun <- function(xx, col) {
    c(N = length2(xx[[col]], na.rm = na.rm),
      mean = mean(xx[[col]], na.rm = na.rm),
      sd = sd(xx[[col]], na.rm = na.rm)
    )
  }
  
  datac <- ddply(data, groupvars, .drop = .drop, .fun = fun, measurevar)
  
  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
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
      formula = as.numeric(Binding) ~ 100 * (v / kobs * (1 - exp(
        -kobs * as.numeric(time_plot)))),
      start = c(v = 1, kobs = 0.001), data = subset)
    
    kobs_matrix <- rbind(kobs_matrix, data.frame(
      sample_conc = sample,
      kobs = summary(nonlin_mod)$parameters[2, 1]))
  }
  
  kobs_matrix$predict_kinact <- 0
  kobs_matrix$sample <- str_split_fixed(kobs_matrix$sample_conc, "_", 2)[, 1]
  kobs_matrix$conc <- as.numeric(
    str_split_fixed(kobs_matrix$sample_conc, "_", 2)[, 2])
  colnames(kobs_matrix)[1] <- "sampleID"
  write.table(kobs_matrix,
              file = paste0(tmp_dir, "/kobs_matrix_", unit_str, ".txt"),
              sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
  kobs_matrix_final <- dcast(kobs_matrix, formula = sample ~ conc,
                             value.var = "kobs")
  replace_colnames <- paste0(
    "concentration ",
    colnames(kobs_matrix_final)[2:ncol(kobs_matrix_final)])
  
  if (units == "M - seconds") {
    colnames(kobs_matrix_final)[2:ncol(kobs_matrix_final)] <- paste0(
      replace_colnames, "M")
  } else {
    colnames(kobs_matrix_final)[2:ncol(kobs_matrix_final)] <- paste0(
      replace_colnames, "uM")
  }
  
  write.table(kobs_matrix_final,
              file = paste0(tmp_dir, "/kobs_table_", unit_str, ".tab"), 
              sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
  
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
      formula = as.numeric(Binding) ~ 100 * (v / kobs * (1 - exp(
        -kobs * as.numeric(time_plot)))),
      start = c(v = 1, kobs = 0.001), data = subset)
    
    fitted_values <- rbind(fitted_values, 
                           data.frame(sample.name = sample, 
                                      summary(nonlin_mod)$parameters))
    modelled_values <- rbind(modelled_values, 
                             data.frame(sample_conc = sample, 
                                        time = seq(
                                          0, max(kobs_input$time_plot), 1), 
                                        value = predict(
                                          nonlin_mod, 
                                          data.frame(time_plot = seq(
                                            0, max(kobs_input$time_plot), 1)
                                            ))))
  }
  
  modelled_values$conc <- str_split_fixed(modelled_values$sample_conc, 
                                          "_", 2)[, 2]
  modelled_values$compound <- str_split_fixed(modelled_values$sample_conc, 
                                              "_", 2)[, 1]
  
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
    
    # dirty hack (needs to be fixed)
    if (any(subset$kobs > 1)) {
      subset <- subset[-which(subset$kobs > 1), ]
    }
    
    nonlin_mod2 <- nlsLM(
      formula = kobs ~ (kinact * conc) / (KI + conc), 
      data = subset, start = start_values)
    
    kinact_matrix <- rbind(
      kinact_matrix, 
      data.frame(sample = sample, 
                 KI = summary(nonlin_mod2)$parameters[2, 1],
                 Kinact = summary(nonlin_mod2)$parameters[1, 1]))
  }
  
  kinact_matrix$KI <- kinact_matrix$KI 
    kinact_matrix$Kinact_KI <- kinact_matrix$Kinact / kinact_matrix$KI
    write.table(kinact_matrix, 
                file = paste0(tmp_dir, "/KI_table_", unit_str, ".tab"), 
                sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
  
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
      data = subset, start = start_values)
    
    kobs_matrix_predict <- rbind(
      kobs_matrix_predict, 
      data.frame(sample = sample, 
                 conc = seq(0, max(kobs_input$concentration_plot), steps), 
                 value = predict(nonlin_mod2, data.frame(
                   conc = seq(0, max(kobs_input$concentration_plot), steps)))))
      
    #print(kobs_matrix_predict)
  }
  
  return(kobs_matrix_predict)
}

#' @export
make_kobs_plots <- function(kobs_input, modelled_values_kobs, 
                            sele_sample, units) {
  
  subset_kobs <- kobs_input[which(kobs_input$compound == sele_sample), ]
  subset_kobs_se <- summarySE(subset_kobs, 
                              measurevar = "Binding", 
                              groupvars = c("time_plot", "concentration"), 
                              na.rm = TRUE)
  subset_kobs_modelled <- modelled_values_kobs[which(
    modelled_values_kobs$compound == sele_sample), ]
  
  if (units == "M - seconds") {
    concentration_numbers <- sort(
      unique(as.numeric(as.character(gsub("M", "", 
                                          subset_kobs_se$concentration)))))
    subset_kobs_se$labels <- factor(subset_kobs_se$concentration, 
                                    levels = paste0(concentration_numbers, "M"))
    xlab_new <- "time [s]"
    breaks_adjust <- seq(0, 21600, 3600)
  } else {
    concentration_numbers <- sort(
      unique(as.numeric(as.character(gsub("uM", "", 
                                          subset_kobs_se$concentration)))))
    subset_kobs_se$labels <- factor(
      gsub("uM", "\U003BCM", subset_kobs_se$concentration), 
      levels = paste0(concentration_numbers, "\U003BCM"))
    xlab_new <- "time [min]"
    breaks_adjust <- seq(0, 360, 60)
  }
  
  p <- ggplot2$ggplot(data = subset_kobs_se, 
                      ggplot2$aes(x = time_plot, y = Binding, group = labels)) +
    ggplot2$geom_point(ggplot2$aes(shape = labels)) +
    ggplot2$geom_line(data = subset_kobs_modelled, 
                      ggplot2$aes(x = time, y = value, group = conc)) +
    ggplot2$geom_errorbar(ggplot2$aes(ymin = Binding - se, ymax = Binding + se), 
                          colour = "black", width = 5) +
    ggplot2$ylab("relative Binding [%]") +
    ggplot2$xlab(xlab_new) +
    ggplot2$ggtitle(sele_sample) +
    ggplot2$expand_limits(y = 0) +
    ggplot2$scale_x_continuous(breaks = breaks_adjust) +
    ggplot2$theme_classic(base_size = 13) +
    ggplot2$theme(legend.position = "bottom", 
                  legend.title = ggplot2$element_blank())

  return(p)
}

#' @export
make_kobs_plots_png <- function(kobs_input, modelled_values_kobs, sele_sample, 
                                units, tmp_dir) {
  unit_str <- gsub(" ", "", units)
  kobs_input <- kobs_input[which(kobs_input$compound != "protein"), ]
  
  for (sele_sample in unique(kobs_input$compound)) {
    subset_kobs <- kobs_input[which(kobs_input$compound == sele_sample), ]
    subset_kobs_se <- na.omit(
      summarySE(subset_kobs, 
                measurevar = "Binding",
                groupvars = c("time_plot", "concentration")))
    subset_kobs_modelled <- modelled_values_kobs[which(
      modelled_values_kobs$compound == sele_sample), ]
    
    if (units == "M - seconds") {
      concentration_numbers <- sort(
        unique(as.numeric(as.character(gsub("M", "", 
                                            subset_kobs_se$concentration)))))
      subset_kobs_se$labels <- factor(subset_kobs_se$concentration, 
                                      levels = paste0(concentration_numbers, 
                                                      "M"))
      xlab_new <- "time [s]"
      breaks_adjust <- seq(0, 21600, 3600)
    } else {
      concentration_numbers <- sort(
        unique(as.numeric(as.character(gsub("uM", "", 
                                            subset_kobs_se$concentration)))))
      subset_kobs_se$labels <- factor(
        gsub("uM", "\U003BCM", subset_kobs_se$concentration), 
        levels = paste0(concentration_numbers, "\U003BCM"))
      xlab_new <- "time [min]"
      breaks_adjust <- seq(0, 360, 60)
    }
    
    p <- ggplot2$ggplot(data = subset_kobs_se, 
                        ggplot2$aes(x = time_plot, y = Binding, 
                                    group = labels)) +
      ggplot2$geom_point(ggplot2$aes(shape = labels)) +
      ggplot2$geom_line(data = subset_kobs_modelled,
                        ggplot2$aes(x = time, y = value, group = conc)) +
      ggplot2$geom_errorbar(ggplot2$aes(
        ymin = Binding - se, ymax = Binding + se), colour = "black", 
        width = 5) +
      ggplot2$ylab("relative Binding [%]") +
      ggplot2$xlab(xlab_new) +
      ggplot2$ggtitle(sele_sample) +
      ggplot2$expand_limits(y = 0) +
      ggplot2$scale_x_continuous(breaks = breaks_adjust) +
      ggplot2$theme_classic(base_size = 13) +
      ggplot2$theme(legend.position = "bottom", 
                    legend.title = ggplot2$element_blank())
    
    png(file = paste0(tmp_dir, "/plots/Kobs_", sele_sample, unit_str, ".png"), 
        bg = "transparent", width = 12, height = 9, units = "cm", res = 600,
        pointsize = 12)
    print(p)
    dev.off()
  }
}

#' @export
make_KI_plots <- function(kobs, kobs_matrix_predict, kinact_matrix, 
                          sele_sample, units) {
  subset_kinact <- kobs[which(kobs$sample == sele_sample), ]
  subset_kinact_modelled <- kobs_matrix_predict[which(
    kobs_matrix_predict$sample == sele_sample), ]
  
  if (units == "M - seconds") {
    xlab_new <- ~paste("conc. [M]")
    ylab_new <- ~paste("k obs [s"^-1, "]")
  } else {
    xlab_new <- ~paste("conc. [\U003BCM]")
    ylab_new <- ~paste("k obs [min"^-1, "]")
  }

  if (nrow(subset_kinact_modelled) > 0) {
    q <- ggplot2$ggplot(data = subset_kinact, ggplot2$aes(x = conc, y = kobs)) +
      ggplot2$geom_point() +
      ggplot2$geom_line(data = subset_kinact_modelled, 
                        ggplot2$aes(x = conc, y = value, color = "black")) +
      ggplot2$theme_classic(base_size = 13) +
      ggplot2$labs(x = xlab_new, y = ylab_new) +
      ggplot2$ggtitle(sele_sample) +
      ggplot2$scale_color_manual(values = "black") +
      ggplot2$theme(legend.position = "none", 
                    plot.margin = unit(c(1, 1, 2, 1), "cm"))
  } else {
    q <- ggplot2$ggplot(data = subset_kinact, ggplot2$aes(x = conc, y = kobs)) +
      ggplot2$geom_point() +
      ggplot2$theme_classic(base_size = 13) +
      ggplot2$labs(x = xlab_new, y = ylab_new) +
      ggplot2$ggtitle(sele_sample) +
      ggplot2$scale_color_manual(values = "black") + 
      ggplot2$theme(legend.position = "none", 
                    plot.margin = unit(c(1, 1, 2, 1), "cm"))
  }
  
  return(q)
}
  
#' @export
make_KI_plots_png <- function(kobs, kobs_matrix_predict, kinact_matrix, 
                              sele_sample, units, tmp_dir) {
  unit_str <- gsub(" ", "", units)
  kobs <- kobs[which(kobs$sample != "protein"), ]
  
  for (sele_sample in unique(kobs$sample)) {
    subset_kinact <- kobs[which(kobs$sample == sele_sample), ]
    subset_kinact_modelled <- kobs_matrix_predict[which(
      kobs_matrix_predict$sample == sele_sample), ]
    
    if (units == "M - seconds") {
      xlab_new <- ~paste("conc. [M]")
      ylab_new <- ~paste("k obs [s"^-1, "]")
    } else {
      xlab_new <- ~paste("conc. [\U003BCM]")
      ylab_new <- ~paste("k obs [min"^-1, "]")
    }

    if (nrow(subset_kinact_modelled) > 0) {
      q <- ggplot2$ggplot(data = subset_kinact, 
                          ggplot2$aes(x = conc, y = kobs)) +
        ggplot2$geom_point() +
        ggplot2$geom_line(data = subset_kinact_modelled, 
                          ggplot2$aes(x = conc, y = value, color = "black")) +
        ggplot2$theme_classic(base_size = 13) +
        ggplot2$labs(x = xlab_new, y = ylab_new) +
        ggplot2$ggtitle(sele_sample) +
        ggplot2$scale_color_manual(values = "black") + 
        ggplot2$theme(legend.position = "none", 
                      plot.margin = unit(c(1, 1, 2, 1), "cm"))
    } else {
      q <- ggplot2$ggplot(data = subset_kinact, 
                          ggplot2$aes(x = conc, y = kobs)) +
        ggplot2$geom_point() +
        ggplot2$theme_classic(base_size = 13) +
        ggplot2$labs(x = xlab_new, y = ylab_new) +
        ggplot2$ggtitle(sele_sample) +
        ggplot2$scale_color_manual(values = "black") +
        ggplot2$theme(legend.position = "none", 
                      plot.margin = unit(c(1, 1, 2, 1), "cm"))
    }

    png(file = paste0(tmp_dir, "/plots/KI_", sele_sample, unit_str, ".png"), 
        bg = "transparent", width = 13, height = 10, units = "cm", res = 600, 
        pointsize = 12)
    print(q)
    if (units == "M - seconds") {
      Kinact <- round(kinact_matrix[which(kinact_matrix$sample == sele_sample), 
                                    "Kinact"], 4)
      KI <- round(kinact_matrix[which(kinact_matrix$sample == sele_sample), 
                               "KI"], 8)

      grid.text(
        substitute(K[inact] == a *"s"^-1*"       "~ K[i] == b*"M", 
                   list(b = KI, a = Kinact)),
        x = unit(.2, "npc"), 
        y = unit(.1, "npc"), 
        just = c("left", "bottom"),
        gp = gpar(fontface = "bold", fontsize = 13, col = "black"))
    } else {
      Kinact <- round(kinact_matrix[which(kinact_matrix$sample == sele_sample), 
                                    "Kinact"], 2)
      KI <- round(kinact_matrix[which(kinact_matrix$sample == sele_sample), 
                                "KI"], 4)

      grid.text(
        substitute(K[inact] == a *"min"^-1*"       "~ K[i] == b *~mu *"M", 
                   list(b = KI, a = Kinact)),
        x = unit(.2, "npc"),
        y = unit(.1, "npc"),
        just = c("left", "bottom"),
        gp = gpar(fontface = "bold", fontsize = 13, col = "black"))
    }
    
    dev.off()
  }
}
