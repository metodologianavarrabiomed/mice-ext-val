#' @title
#' Calculates the type 1 recalibrated predictions
#'
#' @description
#' Using the function `get_recalibrate_params_type_1` calculates the recalibration parameters in each of the imputed datasets stored in `data`. With all the parameters estimated aggregates them and calculates the recalibrated predictions with these aggregated parameters and the aggregated predictions.
#'
#' @param model Model generated with [mv_model_cox()]. Needs the `predictions` parameter of the model, to generate it the function [calculate_predictions()] must be executed over the model.
#' @param data External validation data. Multiple imputation dataset in long format.
#' @param .progress `TRUE` to render the progress bar `FALSE` otherwise.
#'
#' @return A model with the parameter `predictions_recal_type_1` and `alpha` populated.
#'
#'    * `predictions_recal_type_1`: stores the type 1 recalibrated predictions stored as follows
#'        | id        | prediction           |
#'        |-------------|:-------------:|
#'        | 1 | 0.03 |
#'        | ... | ...|
#'        | n | 0.16 |
#'    * `alpha`: stores the \eqn{\alpha} recalibration parameter.
#'
#' @importFrom dplyr %>% group_by group_map filter select
#' @importFrom tibble tibble as_tibble
#' @importFrom progress progress_bar
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#'
#' model |>
#'    calculate_predictions(data) |>
#'    calculate_predictions_recalibrated_type_1(data)
calculate_predictions_recalibrated_type_1.cox <- function(model, data, .progress = TRUE) {
  # Checks pre-conditions
  stopifnot(methods::is(model, "MiceExtVal"))
  stopifnot(methods::is(data, "data.frame"))

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
      get_recalibrate_param_type_1_cox(
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
