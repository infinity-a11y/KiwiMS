# setup_renv.R

# This script is executed by the PowerShell installer to
# setup the R package environment.

message("Restoring R package environment with renv::restore().")

# --- Installation logic ---

overall_success <- FALSE

tryCatch(
    {
        if (!requireNamespace("renv", quietly = TRUE)) {
            stop(
                "ERROR: 'renv' package not available for restore operation. Please ensure it was installed."
            )
        }

        renv::restore(clean = TRUE, rebuild = TRUE)
        message("renv::restore() completed.")
        overall_success <- TRUE
    },
    error = function(e) {
        message(paste0("ERROR: renv::restore() failed: ", e$message))
        overall_success <- FALSE
    }
)


# --- Exit with status code ---
if (!overall_success) {
    message("R script 'setup_renv.R' failed. Exiting R with error status (1).")
    q(save = "no", status = 1)
} else {
    message(
        "R script 'setup_renv.R' completed successfully. Exiting R with success status (0)."
    )
    q(save = "no", status = 0)
}
