#' Plot a list of hexagonal grids
#'
#' Quick faceted plotting of multiple hex grids (as `sf` objects) to visually
#' inspect coverage and relative resolution.
#'
#' @param hex_grids_list list of `sf` hex grids.
#' @export
plot_hex_grids <- function(hex_grids_list) {
  n_grids <- length(hex_grids_list)
  n_cols <- ceiling(sqrt(n_grids))
  n_rows <- ceiling(n_grids / n_cols)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(n_rows, n_cols), mar = c(2, 2, 2, 2))

  for (i in seq_along(hex_grids_list)) {
    hex_grid_sf <- hex_grids_list[[i]]
    plot(sf::st_geometry(hex_grid_sf), main = names(hex_grids_list)[i])
  }
}
