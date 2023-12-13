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
#'where \eqn{S_{0, \text{type 2}}(t)} is estimated using a Weibull distribution and \eqn{\beta_{overall}} is estimated deriving a Cox model with \eqn{\beta \cdot X} as an unique covariate. Both parameters are estimated using the [get_recalibrate_params_type_2_cox()] function.
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
#' @importFrom dplyr %>% group_by group_map filter select
#' @importFrom tibble tibble as_tibble
#' @importFrom progress progress_bar
#'
#' @export
#'
#' @examples
#'
#' model |>
#'   calculate_predictions(data) |>
#'   calculate_predictions_recalibrated_type_1(data) |>
#'   calculate_predictions_recalibrated_type_2(data)
calculate_predictions_recalibrated_type_2.cox <- function(model, data, .progress = TRUE) {
  # Checks pre-conditions
  stopifnot(is(model, "MiceExtVal"))
  stopifnot(is(data, "data.frame"))

  # Progress bar code
  if (.progress) {
    n_iter <- max(data$.imp) + 1
    pb <- progress::progress_bar$new(
      format = "Type 2 recalibration \t[:bar] :percent [E.T.: :elapsedfull || R.T.: :eta]",
      total = n_iter,
      complete = "=",
      incomplete = "-",
      current = ">",
      clear = FALSE,
      width = 100
    )
  }

  # Obtains the calibration parameters
  cal_param <- data %>%
    dplyr::group_by(.imp) %>%
    dplyr::group_map(~ {
      # Progress bar code
      if (.progress) {
        pb$tick()
      }

      # Obtains the data of the event variable
      survival_data <- .x[[all.vars(model$formula)[1]]]
      # Calculates the `betax` data
      betax <- model$betax_data %>%
        dplyr::filter(.imp == .y$.imp) %>%
        dplyr::select(betax)
      # Calculates the type 2 recalibration params
      get_recalibrate_params_type_2_cox(
        recalibrate_data = data.frame(
          time = survival_data[, "time"],
          event = survival_data[, "status"]
        ),
        betax = betax,
        t = max(survival_data[, "time"])
      )
    }) %>%
    do.call(rbind, args = .) %>%
    tibble::as_tibble() %>%
    # Transform to tibble and summarise the reuslts
    dplyr::summarise(
      S0 = mean(unlist(S0)),
      beta_overall = mean(unlist(beta_overall))
    )

  # Populates the aggregated variables in the model
  model$S0_type_2 <- cal_param$S0
  model$beta_overall <- cal_param$beta_overall

  # Calculates the recalibrated type 2 predictions
  model$predictions_recal_type_2 <- model$betax %>%
    dplyr::group_by(id) %>%
    dplyr::group_map(~ {
      tibble::tibble(
        id = .y$id,
        # Formula to calculate the new predictions
        prediction_type_2 = 1 - model$S0_type_2^exp(model$beta_overall * .x$betax)
      )
    }) %>%
    do.call(rbind, args = .)

  # Progress bar code
  if (.progress) {
    pb$tick()
  }

  return(model)
}
