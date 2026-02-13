# setup_renv.R

script_path <- getwd()
message(paste("Current Working Directory:", getwd()))

# Check for lockfile in the current directory
if (!file.exists("renv.lock")) {
    message(
        "renv.lock not found in getwd(). Attempting to set WD to script location..."
    )
}

tryCatch(
    {
        if (!requireNamespace("renv", quietly = TRUE)) {
            stop("ERROR: 'renv' package not available.")
        }

        renv::restore(
            project = getwd(),
            lockfile = "renv.lock",
            rebuild = TRUE,
            prompt = FALSE
        )
        message("renv::restore() completed.")
        overall_success <- TRUE
    },
    error = function(e) {
        message(paste0("ERROR: renv::restore() failed: ", e$message))
        overall_success <- FALSE
    }
)
