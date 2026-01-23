#' @title
#' Creates a cox model.
#'
#' @description
#' Creates a Cox model with all the variables needed to be used in the functions along this package. The Cox model follows
#'
#' \deqn{S_0(t)^{exp(\beta \cdot X)}}
#'
#' so we need to fullfill all the requirements of the model. When we are predicting the survival/risk for a \eqn{X} we have to center the values with the meas on the derivation data, normally reported inside the articles.
#'
#' @param formula Formula of the model to calculate the \eqn{\beta \cdot X} values, including coefficients and means if needed.
#' @param S0 Value of the \eqn{S_0(t)} function for the time of study.
#'
#' @return A model to be used along the package with the next characteristics that could be empty and will be generated with some other functions in the package.
#'   * `formula`: Formula of how the \eqn{\beta \cdot X} will be calculated.
#'   * `S0`: Value of the \eqn{S_0(t)} function for the time of study.
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
#' model <- mv_model_cox(
#'   formula = event ~ 0.5 * (x - 3) + 0.3 * (z - 0.2),
#'   S0 = 0.98765
#' )
#'
mv_model_cox <- function(formula, S0 = NULL) {
  # Checks preconditions
  error_message <- NULL
  if (!methods::is(formula, "formula")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg formula} must be of type `formula`"))
  }

  if (!is.null(S0) && !methods::is(S0, "numeric")) {
    error_message <- c(error_message, "*" = cli::format_error("{.arg S0} must be of type `numeric`"))
  }

  if (!is.null(error_message)) cli::cli_abort(error_message)

  # Creates an object
  object <- list(
    formula = formula,
    S0 = S0,
    predictions_imp = NULL,
    predictions_agg = NULL,
    recal_parameters = NULL,
    results_imp = NULL,
    results_agg = NULL
  )
  # Assigns the object class
  class(object) <- c("MiceExtVal", "cox")

  return(object)
}
