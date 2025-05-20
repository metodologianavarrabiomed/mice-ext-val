#' calculate the brier score for the given model definition
#'
#' @description
#' The Brier Score is calculated using the formula \deqn{BS=\frac{1}{n}\sum_{t=1}^n{(f_t - o_t)^2}} where \eqn{n} is the population size, \eqn{f_t} is the predictions for the row \eqn{t} and \eqn{o_t} is the dichotomous observation for the row \eqn{t}.
#'
#' The function works accordingly to the `model` definition. If the model is defined as Cox, the survival variable is transformed to dichotomous as `1` if the event has appeared during the follow up and `0` othercase. In the case the model is a logistic regression the dependent variable can be already dichotomous or survival, in the case of the survival variable the dichotomous is defined as in the Cox model.
#'
#' @param model Model generated with [mv_model_cox()] or [mv_model_logreg()]. Needs the `predictions` parameter of the model, to generate it the function [calculate_predictions()] must be executed over the model. If we want to obtain also the recalibrated data the model must be initalize the recalibrated predictions with [calculate_predictions_recalibrated_type_1()] and [calculate_predictions_recalibrated_type_2()].
#' @param data Data for what the observed predictions will be calculated.
#' @param type Type of the predictions that the calibration plot data should be generated from: `"predictions_aggregated"`, `"predictions_recal_type_1"` or `"predictions_recal_type_2"`
#' @param boot_samples number of bootstrap resamples to calculate the Brier Score standar error
#' @param seed random seed generator
#'
#' @returns Aggregated Brier Score
#' @export
#'
#' @examples
#' \dontrun{
#' model |>
#'   calculate_brier_score(data, type = "predictions_aggregated")
#' }
calculate_brier_score <- function(model, data, type = c("predictions_aggregated", "predictions_recal_type_1", "predictions_recal_type_2"), n_boot = 1000, seed = NULL) {
  is_dichotomous <- \(x) is.numeric(x) & length(unique(x)) == 2
  if (!is.null(seed)) set.seed(seed)

  # assertions --------------------------------------------------------------
  error_message <- NULL
  if (!methods::is(model, "MiceExtVal")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg model} must be {.cls MiceExtVal}"))
  } else {
    if (!methods::is(data, "data.frame")) {
      error_message <- c(error_message, "*" = cli::format_error("{.arg data} must be {.cls data.frame}"))
    } else {
      if (!any(type %in% c("predictions_aggregated", "predictions_recal_type_1", "predictions_recal_type_2"))) {
        error_message <- c(error_message, "*" = cli::format_error("{.arg type} must be one of the following types: {.arg {c('predictions_aggregated', 'predictions_recal_type_1', 'predictions_recal_type_2')}}"))
      } else {
        # NOTE: if we want to change the aggregated predictions for all the predictions in `mice` methodology we probably need to update this assertion
        if (methods::is(model, "MiceExtVal") && is.null(model[[type]])) {
          error_message <- c(error_message, "*" = cli::format_error("It seems that {.arg type} is not yet calculated, calculate it using {.fn {c('MiceExtVal::calculate_predictions', 'MiceExtVal::calculate_predictions_recalibrated_type_1', 'MiceExtVal::calculate_predictions_recalibrated_type_2')}}"))
        }
      }

      if (!".imp" %in% colnames(data)) {
        error_message <- c(error_message, "*" = cli::format_error("{.arg data} variable must contain {.arg .imp}"))
      }

      dependent_variable <- all.vars(model$formula)[1]
      if (
        methods::is(model, "logreg") &
          (!methods::is(data[[dependent_variable]], "Surv") & !methods::is(data[[dependent_variable]], "numeric"))
      ) {
        error_message <- c(error_message, "*" = cli::format_error("The dependent variable {.var {dependent_variable}} must be {.cls {c('Surv', 'numeric')}}"))
      } else {
        if (methods::is(data[[dependent_variable]], "numeric") & !is_dichotomous(data[[dependent_variable]])) {
          error_message <- c(error_message, "*" = cli::format_error("The dependent variable {.var {dependent_variable}} must be {.arg dichotomous}"))
        }
      }

      if (methods::is(model, "cox")) {
        # Returns an error if `S0` does not exist or it is bad defined in the cox model
        if (is.null(model$S0) | !is.numeric(model$S0)) {
          error_message <- c(error_message, "*" = cli::format_error("{.arg S0} must be {.cls numeric}"))
        }
      }
    }
  }

  if (!is.null(error_message)) cli::cli_abort(error_message)

  # brier score calculation -------------------------------------------------
  get_brier_score <- \(x, y) mean(sum((x - y)**2))
  b_samp <- rsample::bootstraps(data = model[[type]], times = n_boot)
  boot_bs_res <- purrr::map_dbl(
    b_samp[["splits"]], ~ {
      data <- rsample::analysis(.x) |>
        dplyr::left_join(
          data |>
            dplyr::filter(data[[".imp"]] == 1) |>
            dplyr::select(dplyr::all_of(c("id", dependent_variable))),
          by = "id"
        )

      if (methods::is(data[[dependent_variable]], "Surv")) {
        data[["out"]] <- data[[dependent_variable]][, "status"]
      } else {
        data[["out"]] <- data[[dependent_variable]]
      }

      switch(type,
        "predictions_aggregated" = get_brier_score(data[["prediction"]], data[["out"]]),
        "predictions_recal_type_1" = get_brier_score(data[["prediction_type_1"]], data[["out"]]),
        "predictions_recal_type_2" = get_brier_score(data[["prediction_type_2"]], data[["out"]])
      )
    }
  )


  # assign variable in the model --------------------------------------------
  get_brier_score_attribute <- function(data) {
    c(
      "Estimate" = mean(data),
      "95% CI L" = quantile(data, 0.025),
      "95% CI U" = quantile(data, 0.975)
    )
  }

  switch(type,
    "predictions_aggregated" = model$brier_score <- get_brier_score_attribute(boot_bs_res),
    "predictions_recal_type_1" = model$brier_score_type_1 <- get_brier_score_attribute(boot_bs_res),
    "predictions_recal_type_2" = model$brier_score_type_2 <- get_brier_score_attribute(boot_bs_res)
  )

  return(model)
}
