#' Calculate pairwise beta-diversity metrics
#'
#' Compute beta-diversity matrices using Sorensen, Jaccard, or Whittaker family
#' and several decomposition frameworks.
#'
#' @param x matrix/data.frame or `scalebeta` object from `calculate_pairwise_metrics`.
#' @param index_family character. One of `"jaccard"`, `"sorensen"`, `"wb"`.
#' @param framework character. One of `"BAS"`, `"POD"`, `"SET"`.
#'
#' @return A list of `dist` objects with beta and components, depending on options.
#' @export
calculate_beta_diversity <- function(x, index_family = "sorensen", framework = "BAS") {
  index.family <- match.arg(index_family, c("jaccard", "sorensen", "wb"))
  framework <- match.arg(framework, c("BAS", "POD", "SET"))

  if (!inherits(x, "scalebeta")) {
    x <- calculate_pairwise_metrics(x)
  }

  result <- switch(index_family,
                   sorensen = {
                     beta_s = x$sum.not.shared / ((2 * x$shared) + x$sum.not.shared)
                     switch(framework,
                            BAS = {
                              beta_repl_bs = x$min.not.shared / (x$shared + x$min.not.shared)
                              nest_bs = (abs(x$not.shared - t(x$not.shared)) / ((2 * x$shared) + x$sum.not.shared)) * (x$shared / (x$shared + x$min.not.shared))
                              list(beta_s = as.dist(beta_s), beta_repl_bs = as.dist(beta_repl_bs), nest_bs = as.dist(nest_bs))
                            },
                            POD = {
                              beta_repl_s = (2 * x$min.not.shared) / ((2 * x$shared) + x$sum.not.shared)
                              rich_s = abs(x$not.shared - t(x$not.shared)) / ((2 * x$shared) + x$sum.not.shared)
                              list(beta_s = as.dist(beta_s), beta_repl_s = as.dist(beta_repl_s), rich_s = as.dist(rich_s))
                            },
                            SET = {
                              i_s = ifelse(x$shared > 0, abs(x$not.shared - t(x$not.shared)) / ((2 * x$shared) + x$sum.not.shared), 0)
                              rc_s = ifelse(x$shared > 0, (2 * x$min.not.shared) / ((2 * x$shared) + x$sum.not.shared), x$sum.not.shared / ((2 * x$shared) + x$sum.not.shared))
                              list(beta_s = as.dist(beta_s), i_s = as.dist(i_s), rc_s = as.dist(rc_s))
                            })
                   },
                   jaccard = {
                     beta_j = x$sum.not.shared / (x$shared + x$sum.not.shared)
                     switch(framework,
                            BAS = {
                              beta_repl_bj = (2 * x$min.not.shared) / (x$shared + (2 * x$min.not.shared))
                              nest_bj = (abs(x$not.shared - t(x$not.shared)) / (x$shared + x$sum.not.shared)) * (x$shared / (x$shared + (2 * x$min.not.shared)))
                              list(beta_j = as.dist(beta_j), beta_repl_bj = as.dist(beta_repl_bj), nest_bj = as.dist(nest_bj))
                            },
                            POD = {
                              beta_repl_j = (2 * x$min.not.shared) / (x$shared + x$sum.not.shared)
                              rich_j = abs(x$not.shared - t(x$not.shared)) / (x$shared + x$sum.not.shared)
                              list(beta_j = as.dist(beta_j), beta_repl_j = as.dist(beta_repl_j), rich_j = as.dist(rich_j))
                            },
                            SET = {
                              i_j = ifelse(x$shared > 0, abs(x$not.shared - t(x$not.shared)) / (x$shared + x$sum.not.shared), 0)
                              rc_j = ifelse(x$shared > 0, (2 * x$min.not.shared) / (x$shared + x$sum.not.shared), x$sum.not.shared / (x$shared + x$sum.not.shared))
                              list(beta_j = as.dist(beta_j), i_j = as.dist(i_j), rc_j = as.dist(rc_j))
                            })
                   },
                   wb = {
                     beta_wb = x$sum.not.shared
                     switch(framework,
                            POD = {
                              beta_repl_wb = 2 * x$min.not.shared
                              rich_wb = abs(x$not.shared - t(x$not.shared))
                              list(beta_wb = as.dist(beta_wb), beta_repl_wb = as.dist(beta_repl_wb), rich_wb = as.dist(rich_wb))
                            },
                            SET = {
                              i_wb = ifelse(x$shared > 0, abs(x$not.shared - t(x$not.shared)), 0)
                              rc_wb = ifelse(x$shared > 0, 2 * x$min.not.shared, x$sum.not.shared)
                              list(beta_wb = as.dist(beta_wb), i_wb = as.dist(i_wb), rc_wb = as.dist(rc_wb))
                            })
                   })

  result
}
