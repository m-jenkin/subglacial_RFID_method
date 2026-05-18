# =====================================================================
# Title: Package loading helper
# Author: Matt Jenkin
#
# Purpose:
#   Load the R packages required by the minimal RFID method demonstration.
#
# Output:
#   Packages attached for the sourced workflow scripts.
# =====================================================================

load_method_packages <- function() {
  pacman::p_load(
    tools, readr, tidyr, dplyr, lubridate, purrr, furrr, future, tibble,
    ggplot2, patchwork, scales, sf, ks, eks
  )

  options(readr.show_col_types = FALSE)
}
