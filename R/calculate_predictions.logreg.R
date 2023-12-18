#' @title
#' Calculates the predictions for a logistic regression model
#'
#' @description
#' Calculates the predictions of the given logistic regression model and external validation data. The predictions in a logistic regression model are calculated following the next formula.
#'
#' \deqn{p(X) = \frac{1}{1 + e^{-(\beta \cdot X)}}}
#'
#' where \eqn{X} stands for the covariable values and \eqn{\beta} for the coefficient values.
#'
#' @param model Model generated with [mv_model_logreg()]
#' @param data External validation data. Multiple imputation dataset in long format.
#'
#' @return A model with the parameters `predictions_aggregated`, `predictions_data`, `betax` and `betax_data` populated.
#'
#'   * `predictions_aggregated`, stores the predictions aggregated by the mean.
#'   * `predictions_data`, stores all the predictions in each of the imputed datasets.
#'   * `betax`, stores the \eqn{\beta \cdot X} values aggregated by the mean.
#'   * `betax_data`, stores the \eqn{\beta \cdot X} values in each of the imputed datasets.
#'
#' @import mathjaxr
#' @importFrom dplyr %>% group_by group_map rename summarise
#' @importFrom tibble tibble as_tibble add_column
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' model |>
#'    calculate_predictions(data)
calculate_predictions.logreg <- function(model, data) {
  # Checks pre-conditions
  stopifnot(methods::is(model, "MiceExtVal"))
  stopifnot(methods::is(data, "data.frame"))

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
