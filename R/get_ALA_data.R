#' Get ALA occurrences for a species list
#'
#' Retrieve occurrence data from the Atlas of Living Australia (ALA) for a
#' provided species list, intersect the records with a study-area geometry,
#' tidy columns, and optionally write a CSV.
#'
#' Note: Requires packages `galah`, `sf`, `dplyr`, and `purrr` available in the
#' session. Coordinate reference systems (CRS) of the study area are used to
#' interpret occurrence coordinates.
#'
#' @param first_year numeric or NULL. Earliest year to keep (default `NULL` = no filter).
#' @param dTol numeric. Tolerance passed to `sf::st_simplify()` when simplifying the study area.
#' @param species_list character vector of species names, or a list/data.frame with a `Species.Name` element.
#' @param shp `sf`/`sfc` polygon or multipolygon used for spatial filtering.
#' @param output character or `NULL`. Optional file path to write CSV. If `NULL`, no CSV is written.
#'
#' @return A data frame of filtered, intersected occurrences with `longitude`, `latitude`, `date`, etc.
#' @export
#' @examples
#' \dontrun{
#' shp <- sf::read_sf("path/to/area.gpkg")
#' spp <- data.frame(Species.Name = c("Isoodon obesulus", "Petaurus breviceps"))
#' occ <- get_ALA_data(first_year = 2000, species_list = spp, shp = shp, output = "occ.csv")
#' }
get_ALA_data <- function(first_year = NULL, dTol = 10000, species_list, shp, output = NULL) {
  # Basic input validation
  stopifnot(inherits(shp, "sf") || inherits(shp, "sfc"))
  # Accept both character vector and data.frame/list with 'Species.Name'
  sp_select <- if (is.character(species_list)) {
    species_list
  } else if (is.list(species_list) && "Species.Name" %in% names(species_list)) {
    species_list$Species.Name
  } else {
    stop("species_list must be a character vector, or contain a 'Species.Name' element")
  }

  # Simplify shapefile and create convex hull
  shp <- st_simplify(st_union(shp), dTolerance = dTol)
  hull <- st_convex_hull(st_union(shp))

  # Conditional date filter helper
  check_date_filter <- function(data, year_filter) {
    if (!is.null(year_filter)) return(data |> galah_filter(year >= year_filter))
    data
  }

  sp_occ <- purrr::map(sp_select, function(sp) {
    cat("Downloading records for", sp, ":\n")
    galah_call() |>
      galah_identify(sp) |>
      check_date_filter(first_year) |>
      galah_polygon(hull) |>
      galah_select(decimalLongitude, decimalLatitude, class, family, genus, species, vernacularName, eventDate,
                   dataResourceName) |>
      atlas_occurrences()
  })

  sp_df <- dplyr::bind_rows(sp_occ)

  sp_df <- sp_df |>
    dplyr::rename(longitude = decimalLongitude,
                  latitude = decimalLatitude,
                  common_name = vernacularName,
                  date = eventDate) |>
    dplyr::filter(!is.na(longitude) & !is.na(latitude)) |>
    dplyr::filter(!(longitude %% 1 == 0 | latitude %% 1 == 0))

  sp_df <- sf::st_as_sf(sp_df, coords = c("longitude", "latitude"), crs = sf::st_crs(shp)) |>
    sf::st_intersection(shp) |>
    {
      coords <- sf::st_coordinates(sf::st_geometry(.))
      .$longitude <- coords[, 'X']
      .$latitude <- coords[, 'Y']
      sf::st_drop_geometry(.) |>
        as.data.frame()
    } |>
    {sp_df[rownames(sp_df) %in% rownames(.), ]}

  if (!is.null(output)) utils::write.csv(sp_df, output, row.names = FALSE)

  sp_df
}
