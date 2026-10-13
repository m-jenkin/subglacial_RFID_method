# =====================================================================
# Title: Validate method-example outputs
# Author: Matt Jenkin
#
# Purpose:
#   Check output structure, missingness, duplicate records and distance
#   bounds. Known seeding observations may omit uncertainty bounds.
#   These checks do not validate the full field interpretation.
#
# Inputs:
#   roving_transport_distances
#   stationary_transport_records
#   combined_transport_records
#
# Output:
#   Invisible TRUE if checks pass; otherwise stops with a validation error.
# =====================================================================

validate_method_outputs <- function(roving_transport_distances,
                                    stationary_transport_records,
                                    combined_transport_records,
                                    seeding_locations = NULL) {
  check_columns <- function(data, required, label) {
    missing <- setdiff(required, names(data))
    if (length(missing) > 0) {
      stop("Validation failed: ", label, " missing column(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
  }

  roving <- roving_transport_distances
  stationary <- stationary_transport_records
  combined <- combined_transport_records
  check_columns(roving, c("ID", "day", "time", "dist", "min_dist", "max_dist"), "roving")
  check_columns(stationary, c("ID", "antenna", "bin", "duration", "min_dist", "max_dist"), "stationary")
  check_columns(combined, c("ID", "time", "dist", "min_dist", "max_dist", "duration", "system"), "combined")

  # Missing bounds are allowed only for explicitly supplied seeding records,
  # rather than for every zero-distance observation.
  is_seed <- rep(FALSE, nrow(roving))
  if (!is.null(seeding_locations)) {
    check_columns(seeding_locations, c("ID", "day", "dist"), "seeding")
    is_seed <- paste(roving$ID, roving$day, roving$dist) %in%
      paste(seeding_locations$ID, seeding_locations$day, seeding_locations$dist)
  }
  combined_is_seed <- combined$system == "rov" &
    paste(combined$ID, combined$time, combined$dist) %in%
      paste(roving$ID[is_seed], roving$time[is_seed], roving$dist[is_seed])

  valid_bounds <- function(data, allow_missing = rep(FALSE, nrow(data))) {
    all(
      (is.finite(data$min_dist) & is.finite(data$max_dist) &
         data$min_dist <= data$max_dist) |
        (allow_missing & is.na(data$min_dist) & is.na(data$max_dist))
    )
  }

  expected_systems <- c("rov", "stat")
  checks <- list(
    roving_records = nrow(roving) > 0,
    stationary_records = nrow(stationary) > 0,
    combined_records = nrow(combined) > 0,
    particle_ids = !anyNA(roving$ID) && !anyNA(stationary$ID) && !anyNA(combined$ID),
    roving_values = all(is.finite(roving$day) & is.finite(roving$time) & is.finite(roving$dist)),
    stationary_values = !anyNA(stationary$antenna) &&
      all(is.finite(stationary$bin) & is.finite(stationary$duration) & stationary$duration >= 0),
    combined_systems = !anyNA(combined$system) &&
      all(expected_systems %in% combined$system) && all(combined$system %in% expected_systems),
    combined_times = all(is.finite(combined$time)),
    roving_bounds = valid_bounds(roving, is_seed),
    stationary_bounds = valid_bounds(stationary),
    combined_bounds = valid_bounds(combined, combined_is_seed),
    roving_distances = all(is_seed |
      (roving$dist >= roving$min_dist & roving$dist <= roving$max_dist)),
    roving_unique = !anyDuplicated(as.data.frame(roving)[c("ID", "day")]),
    stationary_unique = !anyDuplicated(as.data.frame(stationary)[c("ID", "antenna", "bin")]),
    combined_unique = !anyDuplicated(as.data.frame(combined)[c("ID", "time", "dist", "min_dist", "max_dist", "duration", "system")])
  )

  results <- unlist(checks)
  failed_checks <- names(checks)[is.na(results) | !results]

  if (length(failed_checks) > 0) {
    stop(
      "Validation failed: ",
      paste(failed_checks, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
