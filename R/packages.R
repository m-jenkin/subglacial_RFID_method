load_method_packages <- function() {
  pacman::p_load(
    tools, readr, tidyr, dplyr, lubridate, purrr, furrr, future, tibble,
    ggplot2, patchwork, scales, sf, ks, eks
  )

  options(readr.show_col_types = FALSE)
}
