#' @title
#' Calculates the predictions of the model
#'
#' @description
#' Calculates the predictions of the `model` for the given `data`. The predictions in a logistic regression model are calculated as ...
#'
#' @param model Model for which the predictions are calculated.
#' @param data Data for the predictions.
#'
#' @return A model with the parameters `predictions_aggregated`, `predictions_data`, `betax` and `betax_data` populated.
#'
#' @export
#'
#' @examples
#' model |>
#'    calculate_predictions(data)
calculate_predictions.logreg <- function(model, data) {
  # Checks pre-conditions
  stopifnot(is(model, "MiceExtVal"))
  stopifnot(is(data, "data.frame"))

  # Calculates the model predictions as 1 / (1 + exp(beta * x))
  variables <- sapply(
    1:length(model$coefficient),
    \(index) sprintf(
      "(%f * %s)",
      model$coefficients[[index]],
      names(model$coefficients)[[index]]
    )
  )

  expression <- parse(text = paste(c(model$intercept, variables), collapse = " + "))

  # Calculates the predictions evaluating the previous expression in each imputation
  model$predictions_data <- data %>%
    dplyr::group_by(.imp) %>%
    dplyr::group_map(~ {
      # Calculates the predictions and generates the results as a `tibble`
      with(.x, {1 / (1 + exp(-eval(expression)))}) %>%
        tibble::as_tibble() %>%
        tibble::add_column(.imp = .y$.imp) %>%
        tibble::add_column(id = .x$id) %>%
        # Changes the name to prediction instead of value (name of the `with` function results)
        dplyr::rename(prediction = value)
    }) %>%
    do.call(rbind, args = .)

  # Calculates the betax values
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
