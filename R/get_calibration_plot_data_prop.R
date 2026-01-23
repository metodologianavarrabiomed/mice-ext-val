#' @title
#' Generates the data needed for the calibration plot
#'
#' @description
#' Generates the data needed for the calibration plot. The calibration plot needs to separate the model predictions by risk groups. First the function separates the predictions in `n_groups` then computes the mean value of the model predictions and also the observed value. The observed value is the estimated value at the study time estimated using the proportion of events in each group.
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
get_calibration_plot_data_prop <- function(model, data, n_groups, type = "predictions_aggregated") {
  is_dichotomous <- \(x) is.numeric(x) && length(unique(x)) == 2
  error_message <- NULL
  if (!methods::is(model, "MiceExtVal")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg model} must be {.cls MiceExtVal}"))
  }

  # assertions --------------------------------------------------------------

  if (!methods::is(data, "data.frame")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg data} must be {.cls data.frame}"))
  }

  if (!methods::is(n_groups, "numeric")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg n_groups} must be {.cls numeric}"))
  }

  if (!any(type %in% c("prediction", "prediction_type_1", "prediction_type_2"))) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg type} must be one of the following types: {.arg {c('prediction', 'prediction_type_1', 'prediction_type_2')}}"))
  }

  if (methods::is(model, "MiceExtVal") && is.null(model[["predictions_agg"]][[type]])) {
    error_message <- c(error_message, "*" = cli::format_error("It seems that {.arg {type}} is not yet calculated, calculate it using {.fn {c('MiceExtVal::calculate_predictions', 'MiceExtVal::calculate_predictions_recalibrated_type_1', 'MiceExtVal::calculate_predictions_recalibrated_type_2')}}"))
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
  dependent_variable <- all.vars(model[["formula"]])[1]
  if (!dependent_variable %in% colnames(data)) {
    error_message <- c(error_message, "*" = cli::format_error("the dependent variable {.var {dependent_variable}} must be part of {.arg data}"))
  }

  if (methods::is(model, "logreg")) {
    if (!methods::is(data[[dependent_variable]], "numeric") && !methods::is(data[[dependent_variable]], "Surv")) {
      error_message <- c(error_message, "*" = cli::format_error("the dependent variable {.var {dependent_variable}} must be {.cls {c('Surv', 'numeric')}} and dichotomous"))
    } else if (!methods::is(data[[dependent_variable]], "Surv") && !is_dichotomous(data[[dependent_variable]])) {
      error_message <- c(error_message, "*" = cli::format_error("the dependent variable {.var {dependent_variable}} must be {.cls {c('Surv', 'numeric')}} and dichotomous"))
    }
  } else if (methods::is(model, "cox")) {
    if (!methods::is(data[[dependent_variable]], "Surv")) {
      error_message <- c(error_message, "*" = cli::format_error("the dependent variable {.var {dependent_variable}} must be {.cls Surv}"))
    }
  }

  if (!is.null(error_message)) cli::cli_abort(error_message)

  # calculate calibration plot data -----------------------------------------
  # We assume that the observed variable is completed and therefore the same in
  # all the imputed datasets. If not we should generate the aggregated result
  # using Rubin Rules.
  original_data <- data |> dplyr::filter(.data[[".imp"]] == 1) |>
    # We also assume that data is a `mice` imputed dataset in long format.
    # Thus, `.imp` exists and have at least 1 imputed dataset.
    dplyr::select(dplyr::all_of(c("id", dependent_variable)))

  if (methods::is(original_data[[dependent_variable]], "Surv")) {
    original_data[[dependent_variable]] <- original_data[[dependent_variable]][, "status"]
  }

  # Select the variable that is used from the model
  dependent_variable <- as.name(dependent_variable)

  model[["predictions_agg"]] |>
    # Generates the groups by the prediction variable and group by the generated group
    dplyr::left_join(original_data, by = "id") |>
    dplyr::mutate(bin = dplyr::ntile(!!as.name(type), n_groups)) |>
    dplyr::group_by_at("bin") |>
    dplyr::summarise(
      predicted = mean(!!as.name(type)),
      observed = mean(!!dependent_variable),
      se_observed = stats::sd(!!dependent_variable),
      ll = stats::binom.test(sum(!!dependent_variable), dplyr::n())[["conf.int"]][1],
      ul = stats::binom.test(sum(!!dependent_variable), dplyr::n())[["conf.int"]][2]
    )
}
