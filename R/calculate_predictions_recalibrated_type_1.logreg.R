#' @title
#' Calculates the type 1 recalibrated predictions for a logistic regression model.
#'
#' @description
#' Calculates the type 1 recalibrated predictions for a logistic regression model using ...
#'
#' @param model Model generated with `mv_model`. Needs the `predictions` parameter of the model, to generate it the function `calculate_predictions` must be executed over the model.
#' @param data Data for what the predictions must be recalibrated.
#' @param .progress `TRUE` to render the progress bar `FALSE` otherwise.
#'
#' @return A model with the parameter `predictions_recal_type_1` populated.
#' @export
#'
#' @examples
#' model %>%
#'   calculate_predictions(data) |>
#'   calculate_predictions_recalibrated_type_1(data)
calculate_predictions_recalibrated_type_1.logreg <- function(model, data, .progress = TRUE) {
  # Checks pre-conditions
  stopifnot(is(model, "MiceExtVal"))
  stopifnot(is(data, "data.frame"))

  # Progress bar code
  if (.progress) {
    n_iter <- max(data$.imp) + 1
    pb <- progress::progress_bar$new(
      format = "Type 1 recalibration \t[:bar] :percent [E.T.: :elapsedfull || R.T.: :eta]",
      total = n_iter,
      complete = "=",
      incomplete = "-",
      current = ">",
      clear = FALSE,
      width = 100
    )
  }

  model$alpha_type_1 <- data %>%
    dplyr::group_by(.imp) %>%
    dplyr::group_map(~ {
      # Progress bar code
      if (.progress) {
        pb$tick()
      }

      # Obtains the data of the event variable
      survival_data <- .x[[all.vars(model$formula)[1]]]
      betax <- model$betax_data %>%
        dplyr::filter(.imp == .y$.imp) %>%
        dplyr::select(betax) %>%
        unlist()

      # Calculates the `alpha` parameter value
      model_recal <- rms::lrm.fit(
        y = survival_data[, "status"],
        offset = betax
      )
      alpha <- model_recal$coefficients
    }) %>%
    do.call(rbind, args = .) %>%
    mean()

  # Calculates the type 1 recalibration
  model$predictions_recal_type_1 <- tibble::tibble(
    id = model$betax$id,
    prediction_type_1 = 1 / (1 + exp(-(model$betax$betax + model$alpha_type_1)))
  )

  pb$tick()

  return(model)
}
