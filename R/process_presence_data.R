#' Snap presence records to nearest grid coordinates
#'
#' Read a grid CSV (with `lon`/`lat`) and a presence CSV (with
#' `longitude`/`latitude` plus attributes), convert to `sf`, snap each presence
#' to its nearest grid coordinate, and return a standardized data frame.
#'
#' @param grid character. Path to CSV with columns `lon` and `lat` (grid nodes).
#' @param pres character. Path to CSV with presence records containing columns
#'   `longitude`, `latitude`, `family`, `species`, `date`, `dataResourceName`.
#' @param crs_ref `sf`/`sfc`/`crs` object, EPSG code, or WKT string describing
#'   the CRS of the input coordinates in the files. Typically an EPSG code or
#'   the CRS of the study area.
#'
#' @return A data frame with columns `lon`, `lat`, `family`, `species`, `date`, `source`.
#' @export
#' @examples
#' \dontrun{
#' snapped <- process_presence_data("grid.csv", "occ.csv", crs_ref = 4326)
#' }
process_presence_data <- function(grid, pres, crs_ref) {
  # Helper to coerce to an sf::crs object
  to_crs <- function(x) {
    if (inherits(x, "crs")) return(x)
    if (inherits(x, c("sf", "sfc"))) return(sf::st_crs(x))
    # numeric EPSG or WKT string
    crs <- try(sf::st_crs(x), silent = TRUE)
    if (inherits(crs, "try-error") || is.na(crs$epsg) && is.null(crs$wkt)) {
      stop("crs_ref must be an sf/sfc/crs object, EPSG code, or WKT string")
    }
    crs
  }

  crs_obj <- to_crs(crs_ref)

  # Load and validate grid
  grid_coords <- utils::read.csv(grid, stringsAsFactors = FALSE)
  if (!all(c("lon", "lat") %in% names(grid_coords))) {
    stop("grid CSV must contain 'lon' and 'lat' columns")
  }
  grid_sf <- sf::st_as_sf(grid_coords, coords = c("lon", "lat"), crs = crs_obj)

  # Load and validate presences
  pres <- utils::read.csv(pres, stringsAsFactors = FALSE)
  required_pres <- c("longitude", "latitude", "family", "species", "date", "dataResourceName")
  if (!all(required_pres %in% names(pres))) {
    missing <- setdiff(required_pres, names(pres))
    stop("presence CSV missing required columns: ", paste(missing, collapse = ", "))
  }

  presences_all <- pres |>
    dplyr::select(tidyselect::all_of(required_pres)) |>
    dplyr::rename(lon = longitude, lat = latitude, source = dataResourceName) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = crs_obj) |>
    {
      nearest_indices <- sf::st_nearest_feature(., grid_sf)
      dplyr::mutate(., lon = grid_coords[nearest_indices, "lon"],
                    lat = grid_coords[nearest_indices, "lat"]) 
    } |>
    sf::st_drop_geometry() |>
    dplyr::select(lon, lat, family, species, date, source)

  presences_all
}
