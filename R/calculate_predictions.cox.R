#' @title
#' Calculates the predictions for a Cox model
#'
#' @description
#' Calculates the predictions for the given Cox model and external validation data. The predictions are calculated following the definition of a cox model
#'
#'  \deqn{S_0(t)^{e^{(\beta \cdot X)}}}
#'
#'  where \eqn{S_0(t)} stands for the survival function in a time \eqn{t}, \eqn{\beta} stands for the model coefficients and \eqn{X} stands for the centered values in each `id`.
#'
#' @param model Model generated with [mv_model_cox()]
#' @param data External validation data. Multiple imputation dataset in long format.
#'
#' @return The `model` with the parameters `predictions_aggregated`, `predictions_data`, `betax` and `betax_data` populated.
#'
#'   * `predictions_aggregated`, stores the predictions aggregated by the mean.
#'   * `predictions_data`, stores all the predictions in each of the imputed datasets.
#'   * `betax`, stores the \eqn{\beta \cdot X} values aggregated by the mean.
#'   * `betax_data`, stores the \eqn{\beta \cdot X} values in each of the imputed datasets.
#'
#'
#' @import mathjaxr
#' @importFrom dplyr %>% group_by_at group_map rename vars bind_rows
#' @importFrom tibble tibble as_tibble add_column
#' @importFrom methods is
#' @importFrom cli format_error cli_abort
#'
#' @exportS3Method calculate_predictions cox
#'
#' @examples
#' set.seed(123)
#'
#' model <- mv_model_cox(
#'   coefficients = list(x = 0.5, z = 0.3),
#'   means = list(x = 1, z = 2),
#'   formula = event ~ x + z,
#'   S0 = 0.98765
#' )
#'
#' data <- data.frame(
#'   .imp = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   x = rnorm(9, 1, 0.25),
#'   z = rnorm(9, 2, 0.75)
#' )
calculate_predictions.cox <- function(model, data) {
  error_message <- get_error_message_calculate(model, data)
  if (!is.null(error_message)) cli::cli_abort(error_message)

  # Calculates the predictions evaluating the previous expression in each imputation
  model$predictions_data <- data %>%
    dplyr::group_by_at(dplyr::vars(".imp")) %>%
    dplyr::group_map(~ {
      # Calculates the predictions and generates the results as a `tibble`
      with(.x, {
        1 - model$S0^exp(eval(model$formula[[3]]))
      }) %>%
        tibble::as_tibble() %>%
        tibble::add_column(.imp = .y$.imp) %>%
        tibble::add_column(id = .x$id) %>%
        # Changes the name to prediction instead of value (name of the `with` function results)
        dplyr::rename("prediction" = value)
    }) %>%
    dplyr::bind_rows()

  # Generates the `betax` results for each of the imputations as done with the predictions.
  # TODO: Add this code logic into the predictions one to save all the iterations here.
  model$betax_data <- data %>%
    dplyr::group_by_at(dplyr::vars(".imp")) %>%
    dplyr::group_map(~ {
      with(.x, {
        eval(eval(model$formula[[3]]))
      }) %>%
        tibble::as_tibble() %>%
        tibble::add_column(.imp = .y$.imp) %>%
        tibble::add_column(id = .x$id) %>%
        dplyr::rename("betax" = value)
    }) %>%
    dplyr::bind_rows()

  # Generates the aggregated `predictions` and stores them into the model
  model$predictions_aggregated <- model$predictions_data %>%
    dplyr::group_by_at(dplyr::vars("id")) %>%
    dplyr::group_map(~ tibble::tibble(id = .y$id, prediction = mean(.x$prediction))) %>%
    dplyr::bind_rows()

  # Generates the aggregated `betax` and stores them into the model
  model$betax <- model$betax_data %>%
    dplyr::group_by_at(dplyr::vars("id")) %>%
    dplyr::group_map(~ tibble::tibble(id = .y$id, betax = mean(.x$betax))) %>%
    dplyr::bind_rows()

  return(model)
}
