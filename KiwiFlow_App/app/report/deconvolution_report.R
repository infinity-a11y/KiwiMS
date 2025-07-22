# app/logic/deconvolution_report.R

box::use(
  quarto[quarto_render],
)

args <- commandArgs(trailingOnly = TRUE)
decon_rep_title <- gsub("%~%", " ", args[1])
decon_rep_author <- gsub("%~%", " ", args[2])
decon_rep_desc <- gsub("%~%", " ", args[3])
filename <- args[4]
log_path <- args[5]
results_dir <- file.path(Sys.getenv("USERPROFILE"), 
                         "Documents", "KiwiFlow", "results")

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

filename_id <- gsub(".log", "_deconvolution_report.html", basename(log_path))

file.rename(from = filename,
            to = file.path(dirname(log_path), filename_id))
