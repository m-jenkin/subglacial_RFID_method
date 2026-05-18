#!/usr/bin/env Rscript
# =====================================================================
# Title: Write method-example transport outputs
# Author: Matt Jenkin
#
# Purpose:
#   Combine roving and stationary transport-distance records, validate the
#   resulting tables, and write the compact method-example CSV outputs.
#
# Core logic:
#   - combine roving point estimates and stationary reach constraints
#   - run simple output checks
#   - create outputs/ if needed
#   - write roving, stationary, and combined records to CSV
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

combined_transport_records <- combine_transport_records(
  roving_transport_distances = roving_transport_distances,
  stationary_transport_records = stationary_transport_records
)

validate_method_outputs(
  roving_transport_distances = roving_transport_distances,
  stationary_transport_records = stationary_transport_records,
  combined_transport_records = combined_transport_records
)

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

write_csv(
  st_drop_geometry(roving_transport_distances),
  "outputs/roving_transport_distances.csv"
)
write_csv(
  stationary_transport_records,
  "outputs/stationary_transport_records.csv"
)
write_csv(
  combined_transport_records,
  "outputs/combined_transport_records.csv"
)
