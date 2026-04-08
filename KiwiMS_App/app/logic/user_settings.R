# app/logic/user_settings.R
# Persistent key-value store for user preferences (LOCALAPPDATA/KiwiMS/settings/user_settings.rds)

settings_path <- file.path(
  Sys.getenv("LOCALAPPDATA"), "KiwiMS", "settings", "user_settings.rds"
)

#' @export
read_user_settings <- function() {
  tryCatch(readRDS(settings_path), error = function(e) list())
}

#' @export
save_user_settings <- function(s) {
  d <- dirname(settings_path)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  saveRDS(s, settings_path)
}

#' @export
update_user_setting <- function(key, value) {
  s <- read_user_settings()
  s[[key]] <- value
  save_user_settings(s)
}

#' @export
clear_user_setting <- function(key) {
  s <- read_user_settings()
  s[[key]] <- NULL
  save_user_settings(s)
}
