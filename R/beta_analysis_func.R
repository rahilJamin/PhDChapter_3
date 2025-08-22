## Deprecated aggregator file
## Functions have been split into one-function-per-file under R/ with roxygen2 docs.
## This file remains as a lightweight shim for backward compatibility.

shim_files <- c(
  "R/get_ALA_data.R",
  "R/process_presence_data.R",
  "R/generate_unique_hex_sizes.R",
  "R/generate_hex_grid.R",
  "R/create_variable_hex_grids.R",
  "R/plot_hex_grids.R",
  "R/species_presences.R",
  "R/potential_absences_from_sites.R",
  "R/process_species_presences.R",
  "R/process_hex_grid.R",
  "R/process_all_grids.R",
  "R/calculate_pairwise_metrics.R",
  "R/calculate_beta_diversity.R",
  "R/beta_across_scales.R",
  "R/plot_ridge.R"
)

for (f in shim_files) {
  if (file.exists(f)) try(source(f), silent = TRUE)
}


## End shim
