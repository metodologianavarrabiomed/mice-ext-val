#' @title
#' Generates the data needed for the calibration plot
#'
#' @description
#' Generates the data needed for the calibration plot. The calibration plot needs to separate the model predictions by risk groups. First the function separates the predictions in `n_groups` then computes the mean value of the model predictions and also the observed value. The observed value is the estimated value at the study time estimated using a Kaplan-Meier estimator.
#'
#' @param model Model generated with [mv_model_cox()] or [mv_model_logreg()]. Needs the `predictions` parameter of the model, to generate it the function [calculate_predictions()] must be executed over the model. If we want to obtain also the recalibrated data the model must be initalize the recalibrated predictions with [calculate_predictions_recalibrated_type_1()] and [calculate_predictions_recalibrated_type_2()].
#' @param data Data for what the observed predictions will be calculated.
#' @param n_groups Number of groups that must be calculated.
#' @param type Type of the predictions that the calibration plot data should be generated from: `"predictions_aggregated"`, `"predictions_recal_type_1"` or `"predictions_recal_type_2"`
#'
#' @return `tibble` with the data ready to generate a calibration plot.
#'
#' | group | prediction | observed  |
#' |-------------|:-------------:|:-----:|
#' | 1 | 0.03 | 0.05 |
#' | ... | ... | ... |
#' | n_group | 0.84 | 0.79 |
#'
#' @export
#'
#' @examples
#' \dontrun{
#' model |>
#'   get_calibration_plot_data_surv(data = test_data, n_groups = 10, type = "predictions_aggregated")
#' }
get_calibration_plot_data_surv <- function(model, data, n_groups, type = "predictions_aggregated") {
  error_message <- NULL
  if (!methods::is(model, "MiceExtVal")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg model} must be {.cls MiceExtVal}"))
  }
  if (!methods::is(data, "data.frame")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg data} must be {.cls data.frame}"))
  }

  if (!methods::is(n_groups, "numeric")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg n_groups} must be {.cls numeric}"))
  }

  if (!any(type %in% c("predictions_aggregated", "predictions_recal_type_1", "predictions_recal_type_2"))) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg type} must be one of the following types: {.arg {c('predictions_aggregated', 'predictions_recal_type_1', 'predictions_recal_type_2')}}"))
  }

  # Returns an error if `.imp` is not part of the `data` parameter
  if (!".imp" %in% colnames(data)) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg data} variable must contain {.arg .imp}"))
  }

  # Returns an error if `id` is not part of the `data` parameter
  if (!"id" %in% colnames(data)) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg data} variable must contain {.arg id}"))
  }

  # Returns an error if the dependent variable in the model formula does not exist
  # in `data` or is not a survival class
  dependent_variable <- all.vars(model$formula)[1]
  if (!dependent_variable %in% colnames(data)) {
    error_message <- c(error_message, "*" = cli::format_error("the dependent variable {.var {dependent_variable}} must be part of {.arg data}"))
  }
  if (!methods::is(data[[dependent_variable]], "Surv")) {
    error_message <- c(error_message, "*" = cli::format_error("the dependent variable {.var {dependent_variable}} must be {.cls Surv}"))
  }

  if (!is.null(error_message)) cli::cli_abort(error_message)

  # We assume that the observed variable is completed and therefore the same in
  # all the imputed datasets. If not we should generate the aggregated result
  # using Rubin Rules.
  original_data <- data |> dplyr::filter(.data[[".imp"]] == 1) |>
    # We also assume that data is a `mice` imputed dataset in long format.
    # Thus, `.imp` exists and have at least 1 imputed dataset.
    dplyr::select(dplyr::all_of(c("id", dependent_variable)))

  # Select the variable that is used from the model
  pred_var <- as.name(names(model[[type]])[2])
  model[[type]] |>
    # Generates the groups by the prediction variable and group by the generated group
    dplyr::mutate(group = dplyr::ntile(!!pred_var, n_groups)) |>
    dplyr::left_join(original_data, by = "id") |>
    dplyr::group_by_at(dplyr::vars("group")) |>
    # Calculates the predicted and observed value for each of the predicted risk groups
    dplyr::group_map(~ {
      .x$survobj <- survival::Surv(time = .x[[dependent_variable]][, "time"], event = .x[[dependent_variable]][, "status"])
      # Estimates the observed risk inside the group
      km <- survival::survfit(survobj ~ 1, data = .x)
      return(
        tibble::tibble(
          bin = .y$group,
          predicted = mean(.x[[pred_var]]),
          # Should be calculated for a certain time given by the user.
          observed = 1 - km$surv[length(km$surv)],
          se_observed = km$std.err[length(km$std.err)],
          # It could be that these values go outside the range [0, 1], so we should clip them to this range. It should be also an option the generation of the CI.
          ll = max(0, observed - 1.96 * se_observed),
          ul = min(1, observed + 1.96 * se_observed)
        )
      )
    }) |>
    dplyr::bind_rows()
}
