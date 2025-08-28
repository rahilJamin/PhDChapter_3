#' Select distinct presences for a target species
#'
#' @param pres_all data.frame of snapped presences with at least `lon`, `lat`, and `species`.
#' @param sp character. Target species name.
#' @return Tibble/data.frame of unique sites for the species with a `pa = 1` column.
#' @export
species_presences <- function(pres_all, sp) {
  presences <- pres_all %>%
    dplyr::filter(species == sp) %>%
    dplyr::distinct(lon, lat, .keep_all = TRUE) %>%
    dplyr::mutate(pa = 1) %>%
    tibble::as_tibble()
  return(presences)
}
