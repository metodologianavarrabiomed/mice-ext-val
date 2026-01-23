#' @title generates an stratified calibration plot
#'
#' @param data dataset where the dependent variable for all the models is
#' @param n_groups number of points that should be displayed
#' @param type Type of the predictions that the calibration plot data should be generated from: `"predictions_aggregated"`, `"predictions_recal_type_1"` or `"predictions_recal_type_2"`. The variable with the predictions need to be generated if it is not consider using the [calculate_predictions_recalibrated_type_1()] and [calculate_predictions_recalibrated_type_2()] functions.
#' @param ... models that should be plotted in the stratified calibration plot. If they are a named paramter the name is used as strat
#'
#' @returns the stratified calibration plot for the given models
#' @export
#'
#' @examples
#' \dontrun{
#' get_stratified_calibration_plot_prop(data, 10, model1, model2)
#' get_stratified_calibration_plot_prop(data, 10, strat1 = model1, strat2 = model2)
#' }
get_stratified_calibration_plot_prop <- function(data, n_groups, type = c("predictions_aggregated", "predictions_recal_type_1", "predictions_recal_type_2"), ...) {
  # get model names ---------------------------------------------------------
  models <- rlang::dots_list(
    ...,
    .named = TRUE, .ignore_empty = "all", .homonyms = "error"
  )

  is_model_class <- purrr::map_lgl(models, ~ methods::is(.x, "MiceExtVal"))

  # assert preconditions ----------------------------------------------------
  error_message <- NULL
  if (!all(is_model_class)) {
    error_message <- c(error_message, "*" = cli::format_error("The model{?s} {.arg {names(models)[!is_model_class]}} must be {.cls MiceExtVal}"))
  }

  if (!any(type %in% c("prediction", "prediction_type_1", "prediction_type_2"))) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg type} must be one of the following types: {.arg {c('predictions_aggregated', 'predictions_recal_type_1', 'predictions_recal_type_2')}}"))
  }

  if (all(is_model_class) & type == "predictions_aggregated") {
    no_calc_pred <- purrr::map_lgl(models, \(x) is.null(x[["predictions_aggregated"]]))
    if (any(no_calc_pred)) {
      error_message <- c(error_message, "*" = cli::format_error("The model{?s} {.arg {names(models)[no_calc_pred]}} must contain {.var predictions_aggregated} consider using the function {.fn MiceExtVal::calculate_predictions}"))
    }
  }

  if (all(is_model_class) & type == "predictions_recal_type_1") {
    no_calc_pred <- purrr::map_lgl(models, \(x) is.null(x[["predictions_recal_type_1"]]))
    if (any(no_calc_pred)) {
      error_message <- c(error_message, "*" = cli::format_error("The model{?s} {.arg {names(models)[no_calc_pred]}} must contain {.var predictions_recal_type_1} consider using the function {.fn MiceExtVal::calculate_predictions_recalibrated_type_1}"))
    }
  }

  if (all(is_model_class) & type == "predictions_recal_type_2") {
    no_calc_pred <- purrr::map_lgl(models, \(x) is.null(x[["predictions_recal_type_2"]]))
    if (any(no_calc_pred)) {
      error_message <- c(error_message, "*" = cli::format_error("The model{?s} {.arg {names(models)[no_calc_pred]}} must contain {.var predictions_recal_type_2} consider using the function {.fn MiceExtVal::calculate_predictions_recalibrated_type_2}"))
    }
  }

  if (all(is_model_class)) {
    is_dichotomous <- \(x) length(unique(x)) == 2
    is_dep_var_num <- purrr::map_lgl(
      models,
      ~ methods::is(data[[all.vars(.x[["formula"]])[[1]]]], "Surv") ||
        (methods::is(data[[all.vars(.x[["formula"]])[[1]]]], "numeric") &&
          is_dichotomous(data[[all.vars(.x[["formula"]])[[1]]]]))
    )

    if (!all(is_dep_var_num)) {
      error_message <- c(error_message, "*" = cli::format_error(cli::format_error("The {.arg {names(models)[!is_dep_var_num]}} model{?s} must have a dependent variable of class {.cls {c('Surv', 'numeric')}} and be dichotomous")))
    }
  }


  if (!is.null(error_message)) {
    cli::cli_abort(error_message)
  }

  # generate the stratified calibration plot --------------------------------
  plot_data <- purrr::map_df(seq_along(names(models)), ~ {
    get_calibration_plot_data_prop(model = models[[.x]], data = data, n_groups = n_groups, type) |>
      tibble::add_column(strat = names(models)[[.x]])
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
