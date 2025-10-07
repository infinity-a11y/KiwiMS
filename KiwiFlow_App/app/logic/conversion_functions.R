# app/logic/conversion_functions.R

#' @export
prot_comp_handsontable <- function(tab, disabled = FALSE) {
  table <- rhandsontable::rhandsontable(
    tab,
    rowHeaders = NULL,
    stretchH = "all"
  ) |>
    rhandsontable::hot_cols(fixedColumnsLeft = 1) |>
    rhandsontable::hot_table(
      contextMenu = TRUE,
      highlightCol = TRUE,
      highlightRow = TRUE
    ) |>
    rhandsontable::hot_context_menu(
      allowRowEdit = TRUE,
      allowColEdit = FALSE
    ) |>
    rhandsontable::hot_validate_numeric(
      cols = 2:ncol(tab),
      min = 1,
      allowInvalid = TRUE
    )

  if (disabled) {
    table <- rhandsontable::hot_cols(table, readOnly = TRUE)
  }

  return(table)
}

#' @export
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
        source = proteins,
        strict = TRUE
      )
  } else {
    tab <- tab |>
      rhandsontable::hot_col(
        col = "Protein",
        readOnly = TRUE
      )
  }

  if (length(compounds) > 1) {
    tab <- tab |>
      rhandsontable::hot_col(
        col = min(cmp_cols):max(cmp_cols),
        type = "dropdown",
        source = compounds,
        strict = TRUE
      )
  } else {
    tab <- tab |>
      rhandsontable::hot_col(
        col = "Compound",
        readOnly = TRUE
      )
  }

  return(tab)
}

#' @export
slice_tab <- function(tab) {
  row_contain <- which(rowSums(is.na(tab) | tab == "") != ncol(tab))
  return(tab[row_contain, ])
}

#' @export
check_table <- function(tab, col_limit) {
  if (!nrow(tab)) {
    return("Fill the table with values.")
  }

  # Check variable types
  tab_variables <- sapply(tab, class)
  if (tab_variables[1] != "character") {
    return(paste("Only text characters are allowed as name IDs"))
  }
  if (
    !all(tab_variables[-1] == "numeric") |
      !all(rowSums(!is.na(tab[, -1])) > 0)
  ) {
    return(paste("Mass fields require numeric values"))
  }

  # protein is not na
  if (!all(!is.na(tab[, 1]))) {
    return(paste("Missing name ID values"))
  }

  return(TRUE)
}
