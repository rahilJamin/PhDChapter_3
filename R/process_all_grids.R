#' Process a list of hex grids into species matrices
#'
#' For each hex grid in a list, compute the species-by-hex presence matrix.
#'
#' @param hex_grids_list named list of sf hex grids.
#' @param presence_all_path character. CSV path with snapped presences.
#' @param total_cores integer. Total cores to split across grids and species.
#' @param crs_ref `sf`/`sfc`/`crs` object, EPSG code, or WKT for input lon/lat CRS.
#'
#' @return Named list of matrices keyed by grid name.
#' @export
process_all_grids <- function(hex_grids_list, presence_all_path, total_cores = 6, crs_ref) {
  presences_all <- utils::read.csv(presence_all_path)
  species_pool <- unique(presences_all$species)

  # Allocate cores
  hex_cores <- max(1, round(total_cores / 3))
  species_cores <- max(1, total_cores - hex_cores)

  hex_species_matrices <- parallel::mclapply(names(hex_grids_list), function(grid_name) {
    grid_sf <- hex_grids_list[[grid_name]]
    species_mat <- process_hex_grid(grid_sf, presences_all, species_pool, species_cores, crs_ref)
    return(species_mat)
  }, mc.cores = hex_cores)

  names(hex_species_matrices) <- names(hex_grids_list)
  hex_species_matrices
}
