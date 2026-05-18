# =====================================================================
# Title: Validate method-example outputs
# Author: Matt Jenkin
#
# Purpose:
#   Run simple sanity checks on the roving, stationary, and combined
#   transport-distance tables produced by the method example. These checks
#   are intended to catch broken joins or empty outputs, not to validate the
#   full field interpretation.
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
                                    combined_transport_records) {
  expected_systems <- c("rov", "stat")

  checks <- list(
    roving_records = nrow(roving_transport_distances) > 0,
    stationary_records = nrow(stationary_transport_records) > 0,
    combined_records = nrow(combined_transport_records) > 0,
    combined_systems = all(expected_systems %in% unique(combined_transport_records$system)),
    ordered_distances = all(
      combined_transport_records$min_dist <= combined_transport_records$max_dist,
      na.rm = TRUE
    )
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
