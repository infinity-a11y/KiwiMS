# app/logic/deconvolution_report.R

box::use(
  quarto[quarto_render],
)

decon_rep_title <- commandArgs(trailingOnly = TRUE)[1]
decon_rep_author <- commandArgs(trailingOnly = TRUE)[2]
decon_rep_desc <- commandArgs(trailingOnly = TRUE)[3]
filename <- commandArgs(trailingOnly = TRUE)[4]

quarto_render(
  input = "deconvolution_report.qmd",
  output_file = filename,
  execute_params = list(
    report_title = decon_rep_title,
    report_author = decon_rep_author,
    comment = decon_rep_desc,
    result_path = file.path(getwd(), "results")
  )
)
