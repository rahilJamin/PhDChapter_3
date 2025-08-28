#' Ridge plot of beta-diversity distributions by scale
#'
#' Convenience plotting function for visualizing distributions of beta metrics
#' across spatial scales using `ggridges` and `viridis`.
#'
#' @param df data.frame with columns `value`, `scale`, and `measure`.
#' @export
plot_ridge <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = value, y = scale, fill = ..x..)) +
    ggridges::geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
    ggplot2::scale_fill_viridis_c(name = "Beta Diversity", option = "plasma", direction = -1) +
    ggplot2::scale_y_discrete(labels = function(x) paste0(format(as.numeric(gsub(" km", "", x)), big.mark = ","), " km²")) +
    ggplot2::labs(x = "Mean Beta Deviation", y = "Spatial Grain (km²)") +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.width = grid::unit(2, "cm"),
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 12),
      strip.text = ggplot2::element_text(size = 14, face = "bold"),
      axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 12)
    ) +
    ggplot2::facet_wrap(~measure, scales = "free_y", ncol = 1)
}
