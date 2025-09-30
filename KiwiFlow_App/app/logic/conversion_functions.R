sample_handsontable <- function(tab, proteins, compounds) {
  cmp_cols <- grep("Compound", colnames(tab))

  tab <- rhandsontable::rhandsontable(tab, rowHeaders = NULL) |>
    rhandsontable::hot_col("Sample", readOnly = TRUE) |>
    rhandsontable::hot_cols(fixedColumnsLeft = 1) |>
    rhandsontable::hot_table(
      contextMenu = FALSE,
      highlightCol = TRUE,
      highlightRow = TRUE
    )

  if (length(proteins) > 1) {
    tab <- tab |>
      rhandsontable::hot_col(
        col = "Protein",
        type = "dropdown",
        source = proteins
      )
  }

  if (length(compounds) > 1) {
    tab <- tab |>
      rhandsontable::hot_col(
        col = min(cmp_cols):max(cmp_cols),
        type = "dropdown",
        source = compounds
      )
  }

  return(tab)
}
