#' Construct potential absences from multi-species presence data
#'
#' Generate a set of candidate absence sites for a target species by selecting
#' grid cells where other species are observed but the target species is not,
#' requiring a minimum number of species per site.
#'
#' @param pres_all data.frame of presences for multiple species (snapped to grid).
#' @param pres data.frame of presences for the target species (from `species_presences`).
#' @param min_sp integer. Minimum number of species required per site.
#'
#' @return data.frame of candidate absences with `pa = 0` for the target species.
#' @note Expects columns `lon`, `lat`, and `species` in inputs; uses dplyr.
#' @export
potential_absences_from_sites <- function(pres_all, pres, min_sp) {
  absences <- pres_all %>%
    dplyr::filter(species != pres$species[1]) %>%
    dplyr::anti_join(pres, by = c("lon", "lat")) %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::filter(dplyr::n_distinct(species) >= min_sp) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(lon, lat, .keep_all = TRUE) %>%
    dplyr::mutate(species = pres$species[1], pa = 0)

  absences
}
