combi <- combine_transport_records(t_dist, stat)

validate_method_outputs(
  t_dist = t_dist,
  stat = stat,
  combi = combi
)

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

write_csv(st_drop_geometry(t_dist), "outputs/roving_transport_distances.csv")
write_csv(stat, "outputs/stationary_transport_records.csv")
write_csv(combi, "outputs/combined_transport_records.csv")
