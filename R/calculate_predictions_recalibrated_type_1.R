#' @title
#' Calculates the type 1 recalibrated predictions
#'
#' @description
#' Using the function `get_recalibrate_params_type_1` calculates the recalibration parameters in each of the imputed datasets stored in `data`. With all the parameters estimated aggregates them and calculates the recalibrated predictions with these aggregated parameters and the aggregated predictions. Finally, populates the `predictions_recal_type_1` with a `tibble` that stores the id and the recalibrated prediction. It also populates the `alpha` attribute of the model.
#'
#' @param model Model generated with `mv_model`. Needs the `predictions` parameter of the model, to generate it the function `calculate_predictions` must be executed over the model.
#' @param data Data for what the predictions must be recalibrated.
#' @param .progress `TRUE` to render the progress bar `FALSE` otherwise.
#'
#' @return A model with the parameter `predictions_recal_type_1` populated.
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
#'    calculate_predictions(data) |>
#'    calculate_predictions_recalibrated_type_1(data) |>
calculate_predictions_recalibrated_type_1 <- function(model, data, .progress = TRUE) {
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

  # Obtain the `alpha` value
  model$alpha <- data %>%
    dplyr::group_by(.imp) %>%
    dplyr::group_map(~ {
      # Progress bar code
      if (.progress) {
        pb$tick()
      }

      # Obtains the data of the event variable
      survival_data <- .x[[all.vars(model$formula)[1]]]
      # Calculates the `alpha` parameter value
      get_recalibrate_param_type_1(
        recalibrate_data = data.frame(
          time = survival_data[,"time"],
          event = survival_data[,"status"]
        ),
        s0 = model$S0
      )
    }) %>%
    do.call(rbind, args = .) %>%
    mean() # Aggregates the results, no rubin rules here

  # Calculates the recalibrated type 1 predictions
  model$predictions_recal_type_1 <- model$predictions_aggregated %>%
    dplyr::group_by(id) %>%
    dplyr::group_map(~ {
      tibble(
        id = .y$id,
        # Generation of the recalibrated predictions
        prediction_type_1 = 1 - exp(-exp(model$alpha + log(-log(1 - .x$prediction))))
      )
    }) %>% do.call(rbind, args = .)

  # Progress bar code
  if (.progress) {
    pb$tick()
  }

  return(model)
}
