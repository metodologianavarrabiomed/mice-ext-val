#' @title
#' Generates the calibration plot
#'
#' @description
#' Generates the calibration plot from the data needed to print it. This data is generated with the [get_calibration_plot_data()] function.
#'
#' @param data Outcome of the [get_calibration_plot_data()] function.
#'
#' @return ggplot2 object ready to be plotted
#'
#' @importFrom ggplot2 theme_minimal theme element_line element_text margin element_rect ggplot scale_y_continuous scale_x_continuous expand_limits geom_abline geom_point aes xlab ylab geom_vline geom_hline
#'
#' @export
#'
#' @examples
#' \dontrun{
#' model |>
#'    get_calibration_plot_data(data, 10, type = "predictions_aggregated") |>
#'    get_calibration_plot()
#' }
get_calibration_plot <- function(data) {
  my_theme <- function() {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(colour = "gray90"),
        panel.grid.minor = ggplot2::element_line(colour = "gray90", linetype = "blank"),
        axis.title.x = ggplot2::element_text(
          size = 20,
          face = "italic",
          margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)
        ),
        axis.title.y = ggplot2::element_text(
          size = 20,
          face = "italic",
          margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)
        ),
        axis.text = ggplot2::element_text(face = "plain", size = 16, color = "gray25"),
        plot.title = ggplot2::element_text(size = 20, hjust = 0.25),
        plot.background = ggplot2::element_rect(fill = "white", color = "white")
      )
  }

  gg <- data %>%
    ggplot2::ggplot() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::geom_abline(linetype = "dashed") +
    ggplot2::geom_point(
      ggplot2::aes(x = predicted, y = observed),
      size = 2.5,
      color = "#00BFC4",
      stroke = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(x = predicted, y = observed, ymin = ll, ymax = ul),
      linewidth = 1,
      color = "#00BFC4",
      width = 0.01
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(x = predicted, y = observed),
      weight = 1,
      linewidth = 1,
      color = "#00BFC4",
      se = FALSE,
      method = "loess",
      fullrange = FALSE
    ) +
    ggplot2::xlab("Predicted Risk") +
    ggplot2::ylab("Observed Risk") +
    ggplot2::geom_vline(xintercept = 0, color = "black") +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    my_theme()

  return(gg)
}
