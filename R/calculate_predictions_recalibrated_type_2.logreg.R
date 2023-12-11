#' @title
#' Calculates the type 2 recalibrated predictions for a logistic regression model
#'
#' @description
#' Calculates the type 2 recalibrated predictions for a logistic regression model following the guidance in ...
#'
#' @param model Model generated with `mv_model`. Needs the `predictions` parameter of the model, to generate it the function `calculate_predictions` must be executed over the model.
#' @param data Data for what the predictions must be recalibrated.
#' @param .progress `TRUE` to render the progress bar `FALSE` otherwise.
#'
#' @return A model with the parameter `predictons_recalibrated_type_2`, `S0_type_2` and `beta_overall` populated.
#' @export
#'
#' @examples
#' model %>%
#'   calculate_predictions(data) |>
#'   calculate_predictions_recalibrated_type_1(data) |>
#'   calculate_predictions_recalibrated_type_2(data)
calculate_predictions_recalibrated_type_2.logreg <- function(model, data, .progress = TRUE) {
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

  # Calculates the recalibrate parameters for the model
  recal_parameters <- data %>%
    group_by(.imp) %>%
    group_map(~ {
      # Progress bar code
      if (.progress) {
        pb$tick()
      }
      survival_data <- .x[[all.vars(model$formula)[1]]]

      recal_data <- tibble::tibble(
        y = survival_data[, "status"],
        betax = model$betax_data %>%
          dplyr::filter(.imp == .y$.imp) %>%
          dplyr::select(betax) %>%
          unlist()
      )

      model_recal <- rms::lrm(y ~ betax, data = recal_data)

      tibble::tibble(
        alpha_type_2 = model_recal$coefficients[1],
        beta_overall = model_recal$coefficients[2]
      )
    }) %>%
    do.call(rbind, args = .) %>%
    tibble::as_tibble() %>%
    # Transform to tibble and summarise the reuslts
    dplyr::summarise(
      alpha_type_2 = mean(unlist(alpha_type_2)),
      beta_overall = mean(unlist(beta_overall))
    )

  model$alpha_type_2 <- recal_parameters$alpha_type_2
  model$beta_overall <- recal_parameters$beta_overall

  # Calculates the type 2 recalibration
  model$predictions_recal_type_2 <- tibble::tibble(
    id = model$betax$id,
    prediction_type_2 = 1 / (1 + exp(-(model$alpha_type_2 + (model$beta_overall * model$betax$betax))))
  )

  pb$tick()

  return(model)
}
