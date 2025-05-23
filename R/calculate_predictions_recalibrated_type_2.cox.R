#' @title
#' Calculates the type 2 recalibrated predictions for a Cox model
#'
#' @description
#' This function calculates the type 2 recalibrated predictions for a Cox model. To help the recalibration of the model the function [get_recalibrate_params_type_2_cox()] is defined elsewhere in this package. Using this auxiliar function the recalibration parameters are calculated in each of the imputed datasets stored in `data` as a long dataset.
#'
#' After estimating the recalibration parameters in each of the imputed datasets they are aggregated by their mean to use them to recalibrate the predictions of the model. The type 2 recalibration needs from two parameters `S0_type_2` and `beta_overall`. These parameters are calculated with the [get_recalibrate_params_type_2_cox()] function. Once they are estimated, they are aggregated by the mean. Finally with the type 2 recalibration parameters and the aggregated predictions the type 2 recalibrated predictions are calculated.
#'
#' \deqn{S_{0, \text{type 2}}(t)^{exp(\beta_{overall}(\beta \cdot X))}}
#'
#' where \eqn{S_{0, \text{type 2}}(t)} is estimated using a Weibull distribution and \eqn{\beta_{overall}} is estimated deriving a Cox model with \eqn{\beta \cdot X} as an unique covariate. Both parameters are estimated using the [get_recalibrate_params_type_2_cox()] function.
#'
#' @param model Model generated with [mv_model_cox()]. Needs the `predictions` parameter of the model, to generate it the function [calculate_predictions()] must be executed over the model.
#' @param data External validation data. Multiple imputation dataset in long format.
#' @param .progress `TRUE` to render the progress bar `FALSE` otherwise.
#'
#' @return A model with the parameter `predictons_recalibrated_type_2`, `S0_type_2` and `beta_overall` populated.
#'
#'    * `predictions_recal_type_2`: stores the type 2 recalibrated predictions as follows.
#'        | id | prediction_type_2 |
#'        |-------------|:-------------:|
#'        | 1 | 0.03 |
#'        | ... | ...|
#'        | n | 0.16 |
#'    * `S0_type_2`: stores the \eqn{S_{0, \text{type 2}}(t)} type 2 recalibration parameter.
#'    * `beta_overall`: stores the \eqn{\beta_{overall}} type 2 recalibration parameter.
#'
#' @exportS3Method calculate_predictions_recalibrated_type_2 cox
#'
#' @examples
#' set.seed(123)
#'
#' model <- mv_model_cox(
#'   formula = event ~ 0.5 * (x - 1) + 0.3 * (z - 2),
#'   S0 = 0.98765
#' )
#'
#' data <- data.frame(
#'   .imp = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   x = rnorm(9, 1, 0.25),
#'   z = rnorm(9, 2, 0.75),
#'   event = survival::Surv(rpois(9, 5), rbinom(n = 9, size = 1, prob = 0.5))
#' )
#'
#' model |>
#'   calculate_predictions(data) |>
#'   calculate_predictions_recalibrated_type_1(data) |>
#'   calculate_predictions_recalibrated_type_2(data)
calculate_predictions_recalibrated_type_2.cox <- function(model, data, .progress = FALSE) {
  error_message <- get_error_message_calculate_recalibrated(model, data)
  if (!is.null(error_message)) cli::cli_abort(error_message)

  # Progress bar code
  if (.progress) {
    env <- rlang::env()
    cli::cli_progress_step("Calculating type 2 recalibration parameters", spinner = TRUE, .envir = env)
  }

  # Obtains the calibration parameters
  cal_param <- data |>
    dplyr::group_by_at(dplyr::vars(".imp")) |>
    dplyr::group_map(~ {
      # Progress bar code
      if (.progress) {
        cli::cli_progress_update(.envir = env)
      }

      # Obtains the data of the event variable
      survival_data <- .x[[all.vars(model$formula)[1]]]
      # Calculates the `betax` data
      betax <- model$betax_data |>
        dplyr::filter(.imp == .y$.imp) |>
        dplyr::pull(betax)
      # Calculates the type 2 recalibration params
      get_recalibrate_params_type_2_cox(
        time = survival_data[, "time"],
        event = survival_data[, "status"],
        betax = betax
      )
    }) |>
    dplyr::bind_rows()

  # Populates the aggregated variables in the model
  model$S0_type_2 <- cal_param |>
    dplyr::pull("S0") |>
    mean()
  model$beta_overall <- cal_param |>
    dplyr::pull("beta_overall") |>
    mean()

  # Progress bar code
  if (.progress) {
    cli::cli_progress_done(.envir = env)
    cli::cli_progress_step("recalibrating predictions with type 2 recalibration", .envir = env)
  }

  # Calculates the recalibrated type 2 predictions
  model$predictions_recal_type_2 <- model$betax |>
    dplyr::mutate(
      prediction_type_2 = 1 - model$S0_type_2^exp(model$beta_overall * .data[["betax"]])
    ) |>
    dplyr::select(dplyr::all_of(c("id", "prediction_type_2")))

  # Progress bar code
  if (.progress) {
    cli::cli_progress_done(.envir = env)
  }

  return(model)
}
