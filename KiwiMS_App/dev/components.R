# Get r package environment information
component_table <- function(
  export = FALSE,
  output = getwd(),
  lockfile = file.path(getwd(), "renv.lock")
) {
  # Get required pkgs
  if (!require("renv")) {
    install.packages("renv")
  } else {
    renv::deactivate()
  }

  # Read lockfile
  lock <- renv::lockfile_read(file = lockfile)

  # Initiate package metadata data frame
  pkg_df <- data.frame(
    Package = character(),
    Version = character(),
    Source = character(),
    Type = character(),
    Title = character(),
    License = character(),
    URL = character(),
    Author = character(),
    Maintainer = character(),
    Repository = character(),
    row.names = NULL
  )

  # Fill package metadata data frame
  for (pkg in seq_along(names(lock$Packages))) {
    # Function to retrieve meta info
    get_meta <- function(meta) {
      meta_val <- unlist(lock$Packages[[pkg]][meta])
      if (is.null(meta_val)) {
        meta_val <- ""
      }
      return(meta_val)
    }

    new_entry <- data.frame(
      Package = get_meta("Package"),
      Version = get_meta("Version"),
      Source = get_meta("Source"),
      Type = get_meta("Type"),
      Title = get_meta("Title"),
      License = get_meta("License"),
      URL = get_meta("URL"),
      Author = get_meta("Author"),
      Maintainer = get_meta("Maintainer"),
      Repository = get_meta("Repository")
    )

    pkg_df <- rbind(pkg_df, new_entry, make.row.names = F)
  }

  if (export) {
    write.csv(
      pkg_df,
      file.path(output, "packages_table.csv"),
      row.names = FALSE
    )
  }

  return(pkg_df)
}
