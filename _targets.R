library(targets)

# Packages used within targets
tar_option_set(
  packages = c(
    "sf", "dplyr", "readr", "purrr", "parallel",
    "tidyr", "tibble", "galah"
  )
)

# User-editable configuration for the pipeline
config <- list(
  shapefile_path    = system.file("extdata", "study_area.geojson", package = "PhDChapter3"),
  species_list_path = system.file("extdata", "species_list.csv", package = "PhDChapter3"),
  grid_csv_path     = system.file("extdata", "grid.csv", package = "PhDChapter3"),
  sample_occ_path   = system.file("extdata", "occ_sample.csv", package = "PhDChapter3"),
  use_sample_occ    = TRUE,    # set FALSE to retrieve from ALA
  first_year        = 2000,
  dTol              = 10000,
  max_hex_km        = 100,
  min_hex_km        = 10,
  reduction_factor  = 0.9,
  total_cores       = 6
)

# Optional: set a CRS reference globally if needed by downstream functions
# crs_ref <- 4326  # or sf::st_crs(read_sf(config$shapefile_path))

list(
  tar_target(
    t_shp,
    sf::read_sf(config$shapefile_path),
    cue = tar_cue(mode = "always")
  ),

  tar_target(
    t_species_list,
    readr::read_csv(config$species_list_path, show_col_types = FALSE)
  ),

  tar_target(
    occ_csv,
    {
      if (isTRUE(config$use_sample_occ)) {
        file.copy(config$sample_occ_path, "data/occurrences.csv", overwrite = TRUE)
      } else {
        df <- get_ALA_data(
          first_year   = config$first_year,
          dTol         = config$dTol,
          species_list = t_species_list,
          shp          = t_shp,
          output       = "data/occurrences.csv"
        )
      }
      "data/occurrences.csv"
    },
    format = "file"
  ),

  tar_target(
    snapped_csv,
    {
      df <- process_presence_data(
        grid    = config$grid_csv_path,
        pres    = occ_csv,
        crs_ref = sf::st_crs(t_shp)
      )
      utils::write.csv(df, "data/snapped.csv", row.names = FALSE)
      "data/snapped.csv"
    },
    format = "file"
  ),

  tar_target(
    hexes,
    create_variable_hex_grids(
      shapefile_path  = config$shapefile_path,
      max_hex_km      = config$max_hex_km,
      min_hex_km      = config$min_hex_km,
      reduction_factor = config$reduction_factor,
      num_cores       = max(1, parallel::detectCores() - 1)
    )
  ),

  tar_target(
    species_mats,
    process_all_grids(hexes, presence_all_path = snapped_csv, total_cores = config$total_cores, crs_ref = sf::st_crs(t_shp))
  ),

  tar_target(
    metrics,
    beta_across_scales(species_mats, index_family = "sorensen", framework = "BAS")
  )
)
