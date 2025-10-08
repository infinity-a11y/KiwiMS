# app/logic/conversion_constants.R

# empty data frame for protein and compound input tables
#' @export
empty_tab <- data.frame(
  name = as.character(rep(NA, 9)),
  mass_shift1 = as.numeric(rep(NA, 9)),
  mass_shift3 = as.numeric(rep(NA, 9)),
  mass_shift3 = as.numeric(rep(NA, 9)),
  mass_shift4 = as.numeric(rep(NA, 9)),
  mass_shift5 = as.numeric(rep(NA, 9)),
  mass_shift6 = as.numeric(rep(NA, 9)),
  mass_shift7 = as.numeric(rep(NA, 9)),
  mass_shift8 = as.numeric(rep(NA, 9)),
  mass_shift9 = as.numeric(rep(NA, 9))
)
