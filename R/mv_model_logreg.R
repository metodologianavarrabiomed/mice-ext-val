#' @title
#' Creates a logistic regression model
#'
#' @description
#' Creates a logistic regression model with all the requirements for the external validation. The logistic regression model calculates the predictions following the next equation.
#'
#' \deqn{p(X) = \frac{1}{1 + e^{-(\beta \cdot X)}}}
#'
#' where we can observe that the prediction only depends on the \eqn{\beta} coefficients and the covariable values, \eqn{X}. The logistic regression model have a parameter called `intercept` that usually is represented as \eqn{\beta_0}. The risk prediction is derived from the log-odds function.
#'
#' \deqn{log(\frac{p}{1 - p}) = \beta_0 + \beta_1 \cdot X_1 + \beta_2 \cdot X_2 + \dots + \beta_p \cdot X_p}
#'
#' where we can see that the `intercept` value is not associated with any covariable. Therefore this function parameters are the minimum needed to obtain the predictions.
#'
#' @param formula The model formula to calculate the linear predictor, including coefficients and intercept
#'
#' @return A model to be used along the package with the next characteristics that could be empty and will be generated with some other functions in the package.
#'   * `formula`: Formula of the model containing the coefficients and the intercept.
#'   * `predictions_imp`: `tibble` with the predictions for each of the imputed datasets.
#'   * `predictions_agg`: `tibble` with the aggregated predictions for each patient.
#'   * `recal_parameters`: `tibble` with the recalibration parameters needed.
#'   * `results_imp`: `tibble` with the results in each of the imputations.
#'   * `results_agg`: `tibble` with the aggregated results.
#'
#' @import mathjaxr
#'
#' @export
#'
#' @examples
#' model <- mv_model_logreg(formula = event ~ 0.5 * x + 0.3 * z - 1.2)
mv_model_logreg <- function(formula) {
  error_message <- NULL

  if (!methods::is(formula, "formula")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg formula} must be of type `formula`"))
  }

  if (!is.null(error_message)) cli::cli_abort(error_message)

  model <- list(
    formula = formula,
    predictions_imp = NULL,
    predictions_agg = NULL,
    recal_parameters = NULL,
    results_imp = NULL,
    results_agg = NULL
  )

  class(model) <- c("MiceExtVal", "logreg")

  return(model)
}
