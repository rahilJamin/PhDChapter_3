#' Generate a hexagonal grid intersected with a study area
#'
#' Build a hexagonal grid of a specified cell size (in km), convert to `sf`,
#' intersect with the study-area geometry, and add a `hex_id` column.
#'
#' @param area `sf`/`sfc` polygon or multipolygon for the study area.
#' @param current_cellsize_km numeric. Hex cell size in kilometers.
#'
#' @return `sf` with hex polygons intersected to `area` and `hex_id` sequence.
#' @export
generate_hex_grid <- function(area, current_cellsize_km) {
  cellsize_dd <- current_cellsize_km / 111  # km to degrees (approx)
  hex_grid_sf <- sf::st_make_grid(area, cellsize = cellsize_dd, square = FALSE)
  hex_grid_sf <- sf::st_as_sf(hex_grid_sf)
  hex_grid_sf <- sf::st_intersection(hex_grid_sf, area)
  hex_grid_sf$hex_id <- 1:nrow(hex_grid_sf)
  hex_grid_sf
}
