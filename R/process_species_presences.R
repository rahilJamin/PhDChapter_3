#' Map species presences to nearest hex IDs
#'
#' Convert species-specific presence points to `sf`, align CRS to the grid,
#' and assign each point to its nearest hex cell, returning one record per hex.
#'
#' @param presences_all data.frame of snapped presences across species.
#' @param species character. Species name to extract.
#' @param grid_sf sf polygon grid with `hex_id` column.
#' @param crs_ref `sf`/`sfc`/`crs` object, EPSG code, or WKT string for input lon/lat CRS.
#'
#' @return data.frame with columns `hex_id`, `species`, `pa` (presence-absence indicator).
#' @export
process_species_presences <- function(presences_all, species, grid_sf, crs_ref) {
  presences <- species_presences(presences_all, species)

  pa_sf <- presences %>%
    dplyr::select(lon, lat, species, pa) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = crs_ref) %>%
    sf::st_transform(sf::st_crs(grid_sf))

  hex_species <- pa_sf %>%
    {
      nearest_indices <- sf::st_nearest_feature(., grid_sf)
      dplyr::mutate(., hex_id = grid_sf$hex_id[nearest_indices])
    } %>%
    sf::st_drop_geometry() %>%
    dplyr::select(hex_id, species, pa) %>%
    dplyr::distinct(hex_id, .keep_all = TRUE)

  hex_species
}
