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
#'   calculate_brier_score(data, type = "predictions_data")
#' }
calculate_brier_score <- function(model, data, type = c("predictions_data", "predictions_recal_type_1", "predictions_recal_type_2"), n_boot = 1000, seed = 123) {
  set.seed(seed)

  # assertions --------------------------------------------------------------
  error_message <- NULL
  if (!methods::is(model, "MiceExtVal")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg model} must be {.cls MiceExtVal}"))
  }

  if (!methods::is(data, "data.frame")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg data} must be {.cls data.frame}"))
  } else {
    if (!any(type %in% c("predictions_data", "predictions_recal_type_1", "predictions_recal_type_2"))) {
      error_message <- c(error_message, "*" = cli::format_error("{.arg type} must be one of the following types: {.arg {c('predictions_data', 'predictions_recal_type_1', 'predictions_recal_type_2')}}"))
    } else {
      if (methods::is(model, "MiceExtVal") && is.null(model[[type]])) {
        error_message <- c(error_message, "*" = cli::format_error("It seems that {.arg type} is not yet calculated, calculate it using {.fn {c('MiceExtVal::calculate_predictions', 'MiceExtVal::calculate_predictions_recalibrated_type_1', 'MiceExtVal::calculate_predictions_recalibrated_type_2')}}"))
      }
    }

    if (!".imp" %in% colnames(data)) {
      error_message <- c(error_message, "*" = cli::format_error("{.arg data} variable must contain {.arg .imp}"))
    }

    if (methods::is(model, "MiceExtVal")) {
      dependent_variable <- all.vars(model$formula)[1]
      if (!dependent_variable %in% colnames(data)) {
        error_message <- c(error_message, "*" = cli::format_error("the dependent variable {.var {dependent_variable}} must be part of {.arg data}"))
      }
      if (!methods::is(data[[dependent_variable]], "Surv")) {
        error_message <- c(error_message, "*" = cli::format_error("the dependent variable {.var {dependent_variable}} must be {.cls Surv}"))
      }
    }
  }

  if (!is.null(error_message)) cli::cli_abort(error_message)

  # brier score calculation -------------------------------------------------
  get_brier_score <- \(x, y) mean(sum((x - y)**2))
  brier_score_res <- data |>
    dplyr::group_by_at(".imp") |>
    dplyr::group_map(~ {
      predictions_df <- model$predictions_data |>
        dplyr::filter(.data[[".imp"]] == .y$.imp) |>
        dplyr::left_join(
          data |> dplyr::select(dplyr::all_of(c("id", dependent_variable))),
          by = "id"
        )

      if (methods::is(predictions_df[[dependent_variable]], "Surv")) {
        predictions_df[["out"]] <- predictions_df[[dependent_variable]][, "status"]
      } else {
        predictions_df[["out"]] <- predictions_df[[dependent_variable]]
      }

      b_samp <- rsample::bootstraps(data = predictions_df, times = n_boot)

      boot_bs_res <- purrr::map_dbl(
        b_samp[["splits"]], ~ {
          data <- rsample::analysis(.x)
          get_brier_score(data[["prediction"]], data[["out"]])
        }
      )

      tibble::tibble(
        .imp = .y$.imp,
        brier_score = mean(boot_bs_res),
        se = sd(boot_bs_res)
      )
    }) |>
    dplyr::bind_rows()

  n <- data |>
    dplyr::filter(.data[[".imp"]] == 1) |>
    dplyr::pull("id") |>
    length()

  model$brier_score <- psfmi::pool_RR(
    est = brier_score_res[["brier_score"]],
    se = brier_score_res[["se"]],
    conf.level = 0.95,
    n = n,
    k = 1
  )

  return(model)
}
