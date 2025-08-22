#' Generate a sequence of unique hexagon sizes
#'
#' Create a monotonically decreasing sequence of hexagon cell sizes between
#' `max_hex_size_km` and `min_hex_size_km` by repeatedly multiplying by
#' `reduction_factor` and deduplicating rounded values.
#'
#' @param max_hex_size_km numeric. Maximum hex cell size in kilometers.
#' @param min_hex_size_km numeric. Minimum hex cell size in kilometers.
#' @param reduction_factor numeric in (0,1). Multiplicative reduction per step.
#'
#' @return Numeric vector of unique hex sizes (km), including `min_hex_size_km`.
#' @export
generate_unique_hex_sizes <- function(max_hex_size_km, min_hex_size_km, reduction_factor = 0.90) {
  max_cellsize <- max_hex_size_km
  min_cellsize <- min_hex_size_km

  hex_sizes <- numeric(0)
  i <- 0
  while (TRUE) {
    new_size <- max_cellsize * round((reduction_factor ^ i), 2)
    if (new_size %in% hex_sizes) {
      i <- i + 1
      next
    }
    if (new_size < min_cellsize) {
      if (min_cellsize %in% hex_sizes) {
        break
      } else {
        hex_sizes <- c(hex_sizes, min_cellsize)
        break
      }
    }
    hex_sizes <- c(hex_sizes, new_size)
    i <- i + 1
  }

  hex_sizes
}
