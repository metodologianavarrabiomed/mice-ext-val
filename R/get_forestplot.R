#' Function that generates a C-Index forestplot for the given data
#'
#' @param data Dataset where all the data to plot is stored. It is recommended to be generated using [get_forestplot_data()]
#' @param center x intercept to display a stripped vertical line
#'
#' @returns a C-Index forestplot
#' @export
#'
#' @examples
#' \dontrun{
#' get_forestplot(data, 0.5)
#' }
get_forestplot <- function(data, center) {
  data |>
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = strat, color = strat)) +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = lower, xmax = upper),
      size = 0.5
    ) +
    ggplot2::facet_grid(model ~ 1, switch = "y") +
    ggplot2::geom_vline(xintercept = center, linetype = "dashed") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Strat")) +
    ggplot2::xlab("Harrell C-Index") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text.x = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(
        size = 12, face = "bold", angle = 0
      ),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "gray85"),
      panel.grid.minor.x = ggplot2::element_line(colour = "gray85"),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "gray97", color = NA),
      panel.background = ggplot2::element_rect(fill = "gray97", color = NA),
      panel.spacing = ggplot2::unit(0.25, "lines")
    )
}
