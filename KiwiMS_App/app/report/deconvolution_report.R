# app/logic/deconvolution_report.R

box::use(
  quarto[quarto_render],
)

message("Initiating report generation ...")
Sys.sleep(1)
message("Setting render parameters ...")

tryCatch(
  {
    args <- commandArgs(trailingOnly = TRUE)
    decon_rep_title <- gsub("%~%", " ", args[1])
    decon_rep_author <- gsub("%~%", " ", args[2])
    decon_rep_desc <- gsub("%~%", " ", args[3])
    filename <- args[4]
    log_path <- args[5]
    results_dir <- args[6]
    kiwims_version <- args[7]
    kiwims_date <- args[8]
    temp_dir <- args[9]
  },
  error = function(e) {
    message("Error setting render parameters: ", e$message)
    stop("Report generation failed.")
  }
)

result_file <-
  file.path(
    results_dir,
    gsub(
      ".log",
      "_RESULT.rds",
      basename(log_path)
    )
  )

Sys.sleep(1)
message("Started render engine ...")
Sys.sleep(1)

tryCatch(
  {
    quarto_render(
      input = "deconvolution_report.qmd",
      output_file = filename,
      execute_params = list(
        report_title = decon_rep_title,
        report_author = decon_rep_author,
        comment = decon_rep_desc,
        result_path = results_dir,
        result_file = result_file,
        version = kiwims_version,
        date = kiwims_date,
        temp_dir = temp_dir
      )
    )
  },
  error = function(e) {
    message("Error rendering report: ", e$message)
    stop("Report generation failed.")
  }
)

Sys.sleep(1)
message("Saving report ...")
Sys.sleep(1)

tryCatch(
  {
    filename_id <- gsub(
      ".log",
      "_deconvolution_report.html",
      basename(log_path)
    )

    file.rename(from = filename, to = file.path(results_dir, filename_id))
  },
  error = function(e) {
    message("Error saving report: ", e$message)
    stop("Report generation failed.")
  }
)

message("Rendering finalized!")
