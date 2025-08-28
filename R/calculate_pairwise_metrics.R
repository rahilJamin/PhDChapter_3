#' Internal: compute shared and unshared species matrices
#'
#' @param x matrix/data.frame of 0/1 (presence-absence) with sites x species.
#' @return An object of class `scalebeta` with intermediate matrices for beta computations.
#' @keywords internal
calculate_pairwise_metrics <- function(x) {
  x <- as.matrix(x)

  shared <- x %*% t(x)
  not.shared <- abs(sweep(shared, 2, diag(shared)))
  sumSi <- sum(diag(shared))
  St <- sum(colSums(x) > 0)
  a <- sumSi - St
  sum.not.shared <- not.shared + t(not.shared)
  max.not.shared <- pmax(not.shared, t(not.shared))
  min.not.shared <- pmin(not.shared, t(not.shared))
  computations <- list(data = x, sumSi = sumSi, St = St, a = a,
                       shared = shared, not.shared = not.shared, sum.not.shared = sum.not.shared,
                       max.not.shared = max.not.shared, min.not.shared = min.not.shared)
  class(computations) <- "scalebeta"
  computations
}
