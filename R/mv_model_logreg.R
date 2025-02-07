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
#' @param formula The model formula
#'
#' @return A model to be used along the package with the next characteristics that could be empty and will be generated with some other functions in the package.
#'   * `formula`: Formula of the model containing the coefficients and the intercept.
#'   * `alpha_type_1`: The \eqn{\alpha} value for the type 1 recalibration.
#'   * `alpha_type_2`: The \eqn{\alpha} value for the type 2 recalibration.
#'   * `beta_overall`: The \eqn{\beta_{overall}} value for the type 2 recalibration.
#'   * `predictions_aggregated`: Aggregated predictions for the validation data.
#'   * `predictions_data`: All of the predictions for the validation data in each imputation.
#'   * `betax`: Aggregated \eqn{\beta \cdot X} values for the validation data.
#'   * `betax_data`: All the \eqn{\beta \cdot X} values for the validation in each imputation.
#'   * `predictions_recal_type_1`: Aggregated predictions after recalibrating them with type 1 recalibration.
#'   * `predictions_recal_type_2`: Aggregated predictions after recalibrating them with type 2 recalibration.
#'   * `c_index`: Harrell C-Index of the predictions in the validation cohort.
#'
#' @import mathjaxr
#' @importFrom methods is
#' @importFrom cli format_error cli_abort
#'
#' @export
#'
#' @examples
#' model <- mv_model_logreg(
#'   formula = event ~ 0.5 * x + 0.3 * z - 1.2,
#' )
mv_model_logreg <- function(coefficients, formula, intercept) {
  error_message <- NULL

  if (!methods::is(formula, "formula")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg formula} must be of type `formula`"))
  }

  if (!is.null(error_message)) cli::cli_abort(error_message)

  model <- list(
    formula = formula,
    alpha_type_1 = NULL,
    alpha_type_2 = NULL,
    beta_overall = NULL,
    predictions_aggregated = NULL,
    predictions_data = NULL,
    betax = NULL,
    betax_data = NULL,
    predictions_recal_type_1 = NULL,
    predictions_recal_type_2 = NULL,
    c_index = NULL
  )

  class(model) <- c("MiceExtVal", "logreg")

  return(model)
}
