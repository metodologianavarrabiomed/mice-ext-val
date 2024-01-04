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
#' @importFrom methods is
#'
#' @exportS3Method calculate_predictions_recalibrated_type_2 cox
#'
#' @examples
#' set.seed(123)
#'
#' model <- mv_model_cox(
#'    coefficients = list(x = 0.5, z = 0.3),
#'    means = list(x = 1, z = 2),
#'    formula = event ~ x + z,
#'    S0 = 0.98765
#' )
#'
#' data <- data.frame(
#'   .imp = c(1,1,1,2,2,2,3,3,3),
#'   id = c(1,2,3,1,2,3,1,2,3),
#'   x = rnorm(9, 1, 0.25),
#'   z = rnorm(9, 2, 0.75),
#'   status = c(1,0,0,1,0,0,1,0,0),
#'   time = c(2,3,5,2,3,5,2,3,5)
#' )
#' data$event <- survival::Surv(data$time, data$status)
#'
#' model |>
#'   calculate_predictions(data) |>
#'   calculate_predictions_recalibrated_type_1(data) |>
#'   calculate_predictions_recalibrated_type_2(data)
calculate_predictions_recalibrated_type_2.cox <- function(model, data, .progress = TRUE) {
  # Checks pre-conditions
  stopifnot(methods::is(model, "MiceExtVal"))
  stopifnot(methods::is(data, "data.frame"))

  # Returns an error if `.imp` is not part of the `data` parameter
  if (!".imp" %in% colnames(data)) {
    stop("`data` variable must contain `.imp`")
    return()
  }

  # Returns an error if `id` is not part of the `data` parameter
  if (!"id" %in% colnames(data)) {
    stop("`data` variable must contain `id`")
    return()
  }

  # Returns an error if `predictions_data` does not exist in `model`
  if (!"predictions_data" %in% names(model) | !methods::is(model$predictions_data, "data.frame")) {
    stop("`model` must have `predictions_data` calculated")
    return()
  }

  # Returns an error if the dependent variable in the model formula does not exist
  # in `data` or is not a survival class
  dependent_variable <- all.vars(model$formula)[1]
  if (!dependent_variable %in% colnames(data)) {
    stop("the dependent variable must be part of `data`")
    return()
  }
  if (!methods::is(data[[dependent_variable]], "Surv")) {
    stop("the dependent variable must be of class `Surv`")
    return()
  }

  # Returns an error if `S0` does not exist or it is bad defined in the cox model
  if (is.null(model$S0) | !is.numeric(model$S0)) {
    stop("`S0` must be a `numeric`")
    return()
  }


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
