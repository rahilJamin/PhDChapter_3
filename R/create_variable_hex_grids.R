#' Create variable-resolution hexagonal grids over a study area
#'
#' Read a study-area geometry and generate multiple hex grids at different
#' cell sizes, optionally in parallel, returning a named list of `sf` objects.
#'
#' @param shapefile_path character. Path to an `sf`-readable file (e.g., gpkg, shp).
#' @param max_hex_km numeric. Maximum hex cell size (km).
#' @param min_hex_km numeric. Minimum hex cell size (km).
#' @param reduction_factor numeric. Multiplicative reduction per step (default 0.9).
#' @param num_cores integer or `NULL`. Number of cores for parallel generation.
#'
#' @return Named list of `sf` hex grids keyed by size labels (e.g., `"10 km"`).
#' @export
create_variable_hex_grids <- function(shapefile_path, max_hex_km, min_hex_km, reduction_factor = 0.90, num_cores = 5) {
  area <- sf::read_sf(shapefile_path)

  hex_sizes_km <- generate_unique_hex_sizes(max_hex_km, min_hex_km, reduction_factor)

  if (is.null(num_cores)) {
    num_cores <- parallel::detectCores() - 1
  }

  hex_grids_list <- parallel::mclapply(hex_sizes_km, function(size) generate_hex_grid(area, size), mc.cores = num_cores)

  names(hex_grids_list) <- paste0(round(hex_sizes_km, 2), " km")

  hex_grids_list
}
