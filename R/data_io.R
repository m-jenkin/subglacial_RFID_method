# =====================================================================
# Title: Data loading helpers
# Author: Matt Jenkin
#
# Purpose:
#   Load the cleaned CSV and GeoPackage inputs used by the method
#   demonstration and check that required objects are available.
#
# Notes:
#   Input filenames are converted to object names. This keeps the compact
#   public workflow readable, but assumes stable filenames in data/.
# =====================================================================

load_data_files <- function(data_dir = "data", envir = .GlobalEnv) {
  csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  gpkg_files <- list.files(data_dir, pattern = "\\.gpkg$", full.names = TRUE)

  walk(csv_files, \(path) {
    assign(
      file_path_sans_ext(basename(path)),
      read_csv(path, show_col_types = FALSE),
      envir = envir
    )
  })

  walk(gpkg_files, \(path) {
    assign(file_path_sans_ext(basename(path)), read_sf(path), envir = envir)
  })

  invisible(c(csv_files, gpkg_files))
}

check_required_objects <- function(required_objects, envir = .GlobalEnv) {
  missing_objects <- setdiff(required_objects, ls(envir = envir))

  if (length(missing_objects) > 0) {
    stop(
      "Missing required data object(s): ",
      paste(missing_objects, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
