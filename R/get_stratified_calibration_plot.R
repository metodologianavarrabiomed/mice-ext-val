#' @title generates an stratified calibration plot
#'
#' @param data dataset where the dependent variable is
#' @param n_groups number of points that should be displayed
#' @param ... models that should be plotted in the stratified calibration plot. If they are a named paramter the name is used as strat
#'
#' @returns the stratified calibration plot for the given models
#' @export
#'
#' @examples
#'
#' \dontrun{
#' get_stratified_calibration_plot(data, 10, model1, model2)
#' get_stratified_calibration_plot(data, 10, strat1 = model1, strat2 = model2)
#' }
get_stratified_calibration_plot <- function(data, n_groups, ...) {
  models <- list(...)
  is_model_class <- purrr::map_lgl(models, \(x) methods::is(x, "MiceExtVal"))

  model_names_call <- as.character(as.list(match.call())[-c(1, 2, 3)])
  model_names_callname <- if (!is.null(names(models))) names(models) else rep(NA, length(model_names_call))

  model_names <- purrr::map2_chr(model_names_callname, model_names_call, ~ ifelse(is.na(.x) | .x == "", .y, .x))

  if (!all(is_model_class)) {
    cli::cli_abort("The model{?s} {.arg {model_names[!is_model_class]}} must be {.cls MiceExtVal}")
  }

  plot_data <- purrr::map_df(seq_along(model_names), ~ {
    MiceExtVal::get_calibration_plot_data(model = models[[.x]], data = data, n_groups = n_groups) |>
      tibble::add_column(strat = model_names[[.x]])
  }, data = data)

  gg <- plot_data |>
    ggplot2::ggplot() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::geom_abline(linetype = "dashed") +
    ggplot2::geom_point(
      ggplot2::aes(x = {
        .data[["predicted"]]
      }, y = {
        .data[["observed"]]
      }, colour = {
        .data[["strat"]]
      }),
      size = 2.5,
      stroke = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(x = {
        .data[["predicted"]]
      }, y = {
        .data[["observed"]]
      }, ymin = {
        .data[["ll"]]
      }, ymax = {
        .data[["ul"]]
      }, colour = {
        .data[["strat"]]
      }),
      linewidth = 1,
      width = 0.01
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(x = {
        .data[["predicted"]]
      }, y = {
        .data[["observed"]]
      }, colour = {
        .data[["strat"]]
      }),
      weight = 1,
      linewidth = 1,
      se = FALSE,
      method = "loess",
      fullrange = FALSE
    ) +
    ggplot2::xlab("Predicted Risk") +
    ggplot2::ylab("Observed Risk") +
    ggplot2::geom_vline(xintercept = 0, color = "black") +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
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
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Strats"))

  return(gg)
}
