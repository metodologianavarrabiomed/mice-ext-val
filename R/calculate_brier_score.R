#' calculate the brier score for the given model
#'
#' @description
#' The Brier Score is calculated using the formula \deqn{BS=\frac{1}{n}\sum_{t=1}^n{(f_t - o_t)^2}} where \eqn{n} is the population size, \eqn{f_t} is the predictions for the row \eqn{t} and \eqn{o_t} is the dichotomous observation for the row \eqn{t}.
#'
#' The function operates according to the chosen `model`: if the model is Cox, the survival variable is converted into a binary value (1 if the event occurred during follow-up, 0 otherwise), whereas for a logistic regression model, the dependent variable may already be binary or represent survival time; in the latter case, it is transformed into a binary variable using the same rule as in the Cox model.
#'
#' The confidence interval is calculated by bootstrap resamples.
#'
#' @param model Model generated with [mv_model_cox()] or [mv_model_logreg()]. Needs the expected prediction parameter already calculated in the model. To generate the predictions you must use the function/s [calculate_predictions()], [calculate_predictions_recalibrated_type_1()] or [calculate_predictions_recalibrated_type_2()]
#' @param data Data for what the observed predictions will be calculated.
#' @param type Type of the predictions that the calibration plot data should be generated from: `"prediction"`, `"prediction_type_1"` or `"prediction_type_2"`
#' @param n_boot number of bootstrap resamples to calculate the Brier Score standar error.
#' @param seed random seed generator
#'
#' @export
#'
#' @examples
#' \dontrun{
#' model |>
#'   calculate_brier_score(data, type = "predictions_aggregated")
#' }
calculate_brier_score <- function(model, data, type = c("prediction", "prediction_type_1", "prediction_type_2"), n_boot = 1000, seed = NULL) {
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
      if (!any(type %in% c("prediction", "prediction_type_1", "prediction_type_2"))) {
        error_message <- c(error_message, "*" = cli::format_error("{.arg type} must be one of the following types: {.arg {c('prediction', 'prediction_type_1', 'prediction_type_2')}}"))
      } else {
        if (methods::is(model, "MiceExtVal") && is.null(model[["predictions_agg"]][[type]])) {
          error_message <- c(error_message, "*" = cli::format_error("It seems that {.arg type} is not yet calculated, calculate it using {.fn {c('MiceExtVal::calculate_predictions', 'MiceExtVal::calculate_predictions_recalibrated_type_1', 'MiceExtVal::calculate_predictions_recalibrated_type_2')}}"))
        }
      }

      if (!".imp" %in% colnames(data)) {
        error_message <- c(error_message, "*" = cli::format_error("{.arg data} variable must contain {.arg .imp}"))
      }

      dependent_variable <- all.vars(model[["formula"]])[1]
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
        if (is.null(model[["S0"]]) | !is.numeric(model[["S0"]])) {
          error_message <- c(error_message, "*" = cli::format_error("{.arg S0} must be {.cls numeric}"))
        }
      }
    }
  }

  if (!is.null(error_message)) cli::cli_abort(error_message)

  # brier score calculation -------------------------------------------------
  get_brier_score <- \(x, y) sum((x - y)**2) / length(y)
  b_samp <- rsample::bootstraps(data = model[["predictions_agg"]], times = n_boot)
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

      get_brier_score(data[["prediction"]], data[["out"]])
    }
  )

  # assign variable in the model --------------------------------------------
  get_brier_score_attribute <- function(data) {
    c(
      "Estimate" = mean(data),
      "95% CI L" = stats::quantile(data, 0.025) |> unname(),
      "95% CI U" = stats::quantile(data, 0.975) |> unname(),
      "P-val" = NA_real_
    )
  }

  res <- get_brier_score_attribute(boot_bs_res)

  results_agg <- if (!is.null(model[["results_agg"]])) {
    switch(type,
      "prediction" = model[["results_agg"]] |> dplyr::filter(name != "brier_score"),
      "prediction_type_1" = model[["results_agg"]] |> dplyr::filter(name != "brier_score_type_1"),
      "prediction_type_2" = model[["results_agg"]] |> dplyr::filter(name != "brier_score_type_2"),
    )
  } else {
    model[["results_agg"]]
  }

  model[["results_agg"]] <- dplyr::bind_rows(
    results_agg,
    tibble::tibble(
      name = switch(type,
        "prediction" = "brier_score",
        "prediction_type_1" = "brier_score_type_1",
        "prediction_type_2" = "brier_score_type_2"
      ),
      estimate = res[["Estimate"]],
      lower = res[["95% CI L"]],
      upper = res[["95% CI U"]],
      p_val = res[["P-val"]]
    )
  )

  return(model)
}
