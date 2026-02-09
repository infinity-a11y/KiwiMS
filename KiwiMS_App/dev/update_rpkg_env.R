find_binary_version_specs <- function(
  lockfile_path = "KiwiMS_App/renv.lock"
) {
  # 1. Load lockfile and CRAN binary metadata
  lock <- renv::lockfile_read(lockfile_path)
  bins <- as.data.frame(available.packages(type = "binary"))

  to_update <- c()

  # 2. Compare versions
  for (pkg in names(lock$Packages)) {
    if (pkg %in% rownames(bins)) {
      locked_ver <- lock$Packages[[pkg]]$Version
      binary_ver <- bins[pkg, "Version"]

      # If the binary version is different, we want that specific binary version
      if (locked_ver != binary_ver) {
        to_update <- c(to_update, paste0(pkg, "@", binary_ver))
      }
    }
  }

  if (length(to_update) == 0) {
    message("✅ All locked packages match currently available binaries.")
    return(character(0))
  }

  return(to_update)
}

# --- EXECUTION ---
pkgs_to_fix <- find_binary_version_specs()

if (length(pkgs_to_fix) > 0) {
  cat(
    "Attempting to install binaries for:\n",
    paste(pkgs_to_fix, collapse = "\n "),
    "\n"
  )

  # We use type = "binary" to ensure it doesn't try to compile the new version
  renv::install(pkgs_to_fix, type = "binary")

  # Snapshot to save these new working versions to your lockfile
  # renv::snapshot()
}
