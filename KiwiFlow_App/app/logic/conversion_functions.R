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

  # Allowed protein and compound values
  allowed_per_col <- list(
    NULL,
    proteins,
    compounds
  )

  # Custom renderer
  renderer_js <- "function(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    
    var allowedPerCol = instance.params ? instance.params.allowed_per_col : null;
    
    var allowedRaw;
    if (col === 1) {
      allowedRaw = allowedPerCol ? allowedPerCol[1] : null; 
    } else if (col >= 2) {
      allowedRaw = allowedPerCol ? allowedPerCol[2] : null; 
    } else {
      return;
    }
    
    var normalizedValue = value == null ? '' : String(value).trim();
    
    var allowedList = [];
    
    if (Array.isArray(allowedRaw)) {
      allowedList = allowedRaw;
    } else if (typeof allowedRaw === 'string' && allowedRaw.length > 0) {
      allowedList = [allowedRaw];
    } else if (allowedRaw && Array.isArray(allowedRaw) === false) {
      allowedList = [allowedRaw];
    }
    
    
    if (allowedList.length > 0) {
      var isValid = allowedList.includes(normalizedValue) || normalizedValue === '';
      
      if (!isValid) {
        td.style.background = 'red';
      }
    }
    
    // Re-apply DropdownRenderer if the cell properties indicate a dropdown type.
    // This is necessary to force the rendering of the dropdown arrow, which is 
    // often suppressed when a custom renderer is used via hot_cols().
    if (cellProperties.type === 'dropdown') {
        Handsontable.renderers.DropdownRenderer.apply(this, arguments);
    }
  }"
  handsontable <- rhandsontable::rhandsontable(
    tab,
    rowHeaders = NULL,
    allowed_per_col = allowed_per_col,
    overflow = "visible"
  ) |>
    rhandsontable::hot_col("Sample", readOnly = TRUE) |>
    rhandsontable::hot_cols(fixedColumnsLeft = 1, renderer = renderer_js) |>
    rhandsontable::hot_table(
      contextMenu = FALSE
    )

  if (length(proteins) > 1) {
    handsontable <- handsontable |>
      rhandsontable::hot_col(
        col = "Protein",
        type = "dropdown",
        source = proteins
      )
  }

  if (length(compounds) > 1) {
    handsontable <- handsontable |>
      rhandsontable::hot_col(
        col = min(cmp_cols):max(cmp_cols),
        type = "dropdown",
        source = compounds
      )
  }

  return(handsontable)
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
