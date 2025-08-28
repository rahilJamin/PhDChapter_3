#' Compute beta-diversity across multiple spatial scales
#'
#' Apply `calculate_beta_diversity()` to each species-by-hex matrix in a list,
#' returning a parallel list of results.
#'
#' @param hex_species_matrices named list of matrices (hex x species).
#' @param index_family character. One of `"jaccard"`, `"sorensen"`, `"wb"`.
#' @param framework character. One of `"BAS"`, `"POD"`, `"SET"`.
#' @param num_cores integer. Number of cores for parallel evaluation.
#'
#' @return Named list of metric lists per scale.
#' @export
beta_across_scales <- function(hex_species_matrices, index_family = "sorensen", framework = "BAS", num_cores = (parallel::detectCores()-2)) {
  metrics_list <- parallel::mclapply(hex_species_matrices, function(species_mat) {
    calculate_beta_diversity(species_mat, index_family, framework)
  }, mc.cores = num_cores)

  names(metrics_list) <- names(hex_species_matrices)
  metrics_list
}
