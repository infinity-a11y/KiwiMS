# app/logic/deconvolution_report.R

box::use(
  quarto[quarto_render],
)

message("Initiating report generation ...")

message("Setting render parameters ...")
tryCatch(
  {
    args <- commandArgs(trailingOnly = TRUE)
    decon_rep_title <- gsub("%~%", " ", args[1])
    decon_rep_author <- gsub("%~%", " ", args[2])
    decon_rep_desc <- gsub("%~%", " ", args[3])
    filename <- args[4]
    log_path <- args[5]
    results_dir <- file.path(
      Sys.getenv("USERPROFILE"),
      "Documents",
      "KiwiFlow",
      "results"
    )
  },
  error = function(e) {
    message("Error setting render parameters: ", e$message)
    stop("Report generation failed.")
  }
)

message("Started render engine ...")
Sys.sleep(2)
tryCatch(
  {
    quarto_render(
      input = "deconvolution_report.qmd",
      output_file = filename,
      execute_params = list(
        report_title = decon_rep_title,
        report_author = decon_rep_author,
        comment = decon_rep_desc,
        result_path = results_dir
      )
    )
  },
  error = function(e) {
    message("Error rendering report: ", e$message)
    stop("Report generation failed.")
  }
)

Sys.sleep(2)
message("Saving report ...")
Sys.sleep(2)

tryCatch(
  {
    filename_id <- gsub(
      ".log",
      "_deconvolution_report.html",
      basename(log_path)
    )

    file.rename(from = filename, to = file.path(dirname(log_path), filename_id))
  },
  error = function(e) {
    message("Error saving report: ", e$message)
    stop("Report generation failed.")
  }
)

message("Rendering finalized!")
