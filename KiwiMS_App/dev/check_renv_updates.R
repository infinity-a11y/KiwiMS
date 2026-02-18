# check_renv_updates.R
find_binary_version_specs <- function(
  # Adjusted path: Assumes KiwiMS_App folder is in the root of your repository
  lockfile_path = "./KiwiMS_App/renv.lock"
) {
  if (!file.exists(lockfile_path)) {
    return(FALSE)
  }

  # 'renv' must be installed in the GitHub Action runner (see YAML below)
  lock <- renv::lockfile_read(lockfile_path)

  # Fetch available packages to compare versions
  bins <- as.data.frame(available.packages(
    type = "binary",
    repos = "https://cran.rstudio.com"
  ))

  to_update <- c()

  for (pkg in names(lock$Packages)) {
    if (pkg %in% rownames(bins)) {
      locked_ver <- lock$Packages[[pkg]]$Version
      binary_ver <- bins[pkg, "Version"]
      if (locked_ver != binary_ver) {
        to_update <- c(
          to_update,
          paste0(pkg, " (", locked_ver, " -> ", binary_ver, ")")
        )
      }
    }
  }

  return(to_update)
}

# --- Run the check ---
pkgs_to_fix <- find_binary_version_specs()

cat("\n=== Renv Binary Update Check ===\n")

if (isFALSE(pkgs_to_fix)) {
  cat(
    "⚠️ Lockfile not found at ./KiwiMS_App/renv.lock. Check your repo structure.\n"
  )
  quit(status = 1) # Exit with error
} else if (length(pkgs_to_fix) == 0) {
  cat(
    "✅ All locked packages are using the latest available binary versions.\n"
  )
  quit(status = 0) # Exit with success
} else {
  cat("⚠️ The following packages have newer binary versions available:\n")
  cat(paste("   -", pkgs_to_fix, collapse = "\n"), "\n")
  quit(status = 10)
}
