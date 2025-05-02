# app/logic/deconvolution_report.R

box::use(
  quarto[quarto_render],
)

box::use(
  app / logic / logging[get_log],
)

decon_rep_title <- commandArgs(trailingOnly = TRUE)[1]
decon_rep_author <- commandArgs(trailingOnly = TRUE)[2]
decon_rep_desc <- commandArgs(trailingOnly = TRUE)[3]
filename <- commandArgs(trailingOnly = TRUE)[4]
log_path <- commandArgs(trailingOnly = TRUE)[5]

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
