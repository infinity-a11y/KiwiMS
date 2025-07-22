# install_reticulate.R

# This script is executed by the PowerShell installer to
# install the 'reticulate' package.

message("Starting reticulate installation script.")

# --- Function to install and verify single package ---
install_and_verify_package <- function(pkg_name, repos_url) {
  message(paste0("Attempting to install '", pkg_name, "'..."))
  tryCatch({
    install.packages(pkg_name, repos = repos_url, quiet = FALSE, verbose = TRUE)
    message(paste0("'", pkg_name, "' installation command finished."))

    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      stop(paste0("Package '", pkg_name, "' was not successfully installed or found after installation."))
    }
    message(paste0("Successfully verified '", pkg_name, "' package."))
    TRUE # Indicate success for this package
  }, error = function(e) {
    message(paste0("ERROR: R package '", pkg_name, "' installation failed: ", e$message))
    FALSE # Indicate failure for this package
  })
}

# --- Installation logic ---

overall_success <- TRUE

# Install renv
if (!install_and_verify_package("reticulate", "https://cloud.r-project.org")) {
    overall_success <- FALSE
}

# --- Exit with status code ---
if (!overall_success) {
    message("Overall R package installation failed. Exiting R with error status (1).")
    q(save = "no", status = 1)
} else {
    message("All R packages successfully installed. Exiting R with success status (0).")
    q(save = "no", status = 0)
}