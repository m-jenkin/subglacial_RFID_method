# =====================================================================
# Title: Write checked method output tables
# Author: Matt Jenkin
#
# Purpose:
#   Combine roving and stationary transport-distance records, run light
#   reproducibility checks, and write the CSV outputs for the method
#   demonstration.
#
# Core logic:
#   1. Bind roving point estimates and stationary reach records into a
#      single transport table.
#   2. Check that the expected record types and distance bounds are present.
#   3. Write roving, stationary and combined output CSV files.
#
# Inputs:
#   roving_transport_distances
#   stationary_transport_records
#
# Outputs:
#   outputs/roving_transport_distances.csv
#   outputs/stationary_transport_records.csv
#   outputs/combined_transport_records.csv
# =====================================================================

# The combined table preserves the two evidence systems rather than forcing
# stationary reach detections into point estimates.
combined_transport_records <- combine_transport_records(
  roving_transport_distances,
  stationary_transport_records
)

validate_method_outputs(
  t_dist = roving_transport_distances,
  stat = stationary_transport_records,
  combi = combined_transport_records
)

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

write_csv(
  st_drop_geometry(roving_transport_distances),
  "outputs/roving_transport_distances.csv"
)
write_csv(stationary_transport_records, "outputs/stationary_transport_records.csv")
write_csv(combined_transport_records, "outputs/combined_transport_records.csv")
