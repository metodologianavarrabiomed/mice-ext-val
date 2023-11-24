#' @title
#' Calculates the predictions for the given model.
#'
#' @description
#' Calculates the predictions for the given model and validation data. The predictions are calculated following the definition of a cox model, \eqn{S_0(t)^{exp(\beta \cdot X)}}, where \eqn{S_0(t)} stands for the survival function in a time \eqn{t}, \eqn{\beta} stands for the model coefficients and \eqn{X} stands for the centered values in the patient. After that populates the `predictions_aggregated`, `predictions_data`, `betax` and `betax_data` parameters in the model.
#'
#' @param model Model generated with `mv_model`.
#' @param data Data for what the predictions must be recalibrated.
#'
#' @return A model with the parameters `predictions_aggregated`, `predictions_data`, `betax` and `betax_data` populated.
#'
#' @import mathjaxr
#' @importFrom dplyr %>% group_by group_map rename summarise
#'
#' @export
#'
#' @examples
#' model |>
#'    calculate_predictions(data)
calculate_predictions <- function(model, data) {
  # Checks pre-conditions
  stopifnot(is(model, "MiceExtVal"))
  stopifnot(is(data, "data.frame"))

  # Obtain the expression that calculates the `betax` from the `coefficients` and `mean` paramters. Loop over all the variable names and generates the expression `(coef * (var - mean))`
  variables <- sapply(
    1:length(model$coefficient),
    \(index) sprintf(
      "(%f * (%s - %f))",
      model$coefficients[[index]],
      names(model$coefficients)[[index]],
      model$mean[[index]]
    )
  )

  expression <- parse(text = paste(variables, collapse = " + "))

  # Calculates the predictions evaluating the previous expression in each imputation
  model$predictions_data <- data %>%
    dplyr::group_by(.imp) %>%
    dplyr::group_map(~ {
      # Calculates the predictions and generates the results as a `tibble`
      with(.x, {1 - model$S0^exp(eval(expression))}) %>%
        tibble::as_tibble() %>%
        tibble::add_column(.imp = .y$.imp) %>%
        tibble::add_column(id = .x$id) %>%
        # Changes the name to prediction instead of value (name of the `with` function results)
        dplyr::rename(prediction = value)
    }) %>%
    do.call(rbind, args = .)

  # Generates the `betax` results for each of the imputations as done with the predictions.
  # TODO: Add this code logic into the predictions one to save all the iterations here.
  model$betax_data <- data %>%
    dplyr::group_by(.imp) %>%
    dplyr::group_map(~ {
      with(.x, {eval(expression)}) %>%
        tibble::as_tibble() %>%
        tibble::add_column(.imp = .y$.imp) %>%
        tibble::add_column(id = .x$id) %>%
        dplyr::rename(betax = value)
    }) %>%
    do.call(rbind, args = .)

  # Generates the aggregated `predictions` and stores them into the model
  model$predictions_aggregated <- model$predictions_data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(prediction = mean(prediction))

  # Generates the aggregated `betax` and stores them into the model
  model$betax <- model$betax_data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(betax = mean(betax))

  return(model)
}
