validate_method_outputs <- function(t_dist, stat, combi) {
  expected_systems <- c("rov", "stat")

  checks <- list(
    roving_records = nrow(t_dist) > 0,
    stationary_records = nrow(stat) > 0,
    combined_records = nrow(combi) > 0,
    combined_systems = all(expected_systems %in% unique(combi$system)),
    ordered_distances = all(combi$min_dist <= combi$max_dist, na.rm = TRUE)
  )

  failed_checks <- names(checks)[!unlist(checks)]

  if (length(failed_checks) > 0) {
    stop(
      "Validation failed: ",
      paste(failed_checks, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
