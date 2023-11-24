#' @title
#' Calculates the type 2 recalibrated predictions
#'
#' @description
#' Using the function `get_recalibrate_params_type_2` calculates the recalibration parameters in each of the imputed datasets stored in `data`. With all the parameters estimated aggregates them and calculates the recalibrated predictions with these aggregated parameters and the aggregated predictions. Finally, populates the `predictions_recal_type_2` with a `tibble` that stores the id and the recalibrated prediction. It also populates the `S0_type_2` and `beta_overall` attributes of the model.
#'
#' @param model Model generated with `mv_model`. Needs the `predictions` parameter of the model, to generate it the function `calculate_predictions` must be executed over the model.
#' @param data Data for what the predictions must be recalibrated.
#' @param .progress `TRUE` to render the progress bar `FALSE` otherwise.
#'
#' @return A model with the parameter `predictons_recalibrated_type_2`, `S0_type_2` and `beta_overall` populated.
#'
#' @importFrom dplyr %>% group_by group_map filter select
#' @importFrom tibble tibble as_tibble
#' @importFrom progress progress_bar
#'
#' @export
#'
#' @examples
#'
#' model %>%
#'   calculate_predictions(data) |>
#'   calculate_predictions_recalibrated_type_1(data) |>
#'   calculate_predictions_recalibrated_type_2(data)
calculate_predictions_recalibrated_type_2 <- function(model, data, .progress = TRUE) {
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
      get_recalibrate_params_type_2(
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
