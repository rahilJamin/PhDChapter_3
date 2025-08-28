#' Build species-by-hex presence matrix for a single grid
#'
#' For a given hex grid and a list of species, compute a hex-by-species matrix
#' of presence (1) and absence (0) using nearest-hex assignment.
#'
#' @param grid_sf sf polygon grid with `hex_id` column.
#' @param presences_all data.frame of snapped presences across species.
#' @param species_pool character vector of species names.
#' @param species_cores integer. Parallel workers for species mapping.
#' @param crs_ref `sf`/`sfc`/`crs` object, EPSG code, or WKT for input lon/lat CRS.
#'
#' @return A matrix with hex IDs as row names and species as columns (0/1).
#' @export
process_hex_grid <- function(grid_sf, presences_all, species_pool, species_cores, crs_ref) {
  if (is.null(grid_sf$hex_id)) grid_sf$hex_id <- seq_len(nrow(grid_sf))

  hex_species_list <- parallel::mclapply(species_pool, function(species) {
    process_species_presences(presences_all, species, grid_sf, crs_ref)
  }, mc.cores = species_cores)

  hex_species_combined <- dplyr::bind_rows(hex_species_list)

  species_mat <- hex_species_combined %>%
    tidyr::pivot_wider(id_cols = c(hex_id),
                names_from = "species",
                values_from = "pa",
                values_fill = 0,
                values_fn = sum) %>%
    tibble::column_to_rownames("hex_id")

  species_mat
}
