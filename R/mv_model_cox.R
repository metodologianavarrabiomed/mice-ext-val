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
#' @param coefficients \eqn{\beta} values of the model.
#' @param means Mean values of the variables in the derivation dataset.
#' @param formula Formula of the model.
#' @param S0 Value of the \eqn{S_0(t)} function for the time of study.
#'
#' @return A model to be used along the package with the next characteristics that could be empty and will be generated with some other functions in the package.
#'   * `coefficients`: \eqn{\beta} values of the model.
#'   * `means`: Mean values of the variables in the derivation dataset.
#'   * `formula`: Formula of the model.
#'   * `S0`: Value of the \eqn{S_0(t)} function for the time of study.
#'   * `alpha`: Recalibration parameter for the type 1 recalibration.
#'   * `S0_type_2`: Value of the \eqn{S_0(t)} function for the time of study for the type 2 recalibration.
#'   * `beta_overall`: Recalibration parameter for the type 2 recalibration.
#'   * `predictions_aggregated`: Aggregated predictions for the validation data.
#'   * `predictions_data`: All predictions for the validation data including all the imputations.
#'   * `betax`: Aggregated \eqn{\beta \cdot X} values for the validation data.
#'   * `betax_data`: All predictions for the validation data including all the imputations.
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
#' model <- mv_model_cox(
#'   coefficients = list(x = 0.5, z = 0.3),
#'   means = list(x = 3, z = 0.2),
#'   formula = event ~ x + z,
#'   S0 = 0.98765
#' )
#'
mv_model_cox <- function(coefficients, means, formula, S0 = NULL) {
  # Checks preconditions
  error_message <- NULL
  if (!methods::is(coefficients, "list")) {
    error_message <- c(error_message, cli::format_error("{.arg coefficients} must be of type `list`"))
  }

  if (!methods::is(means, "list")) {
    error_message <- c(error_message, cli::format_error("{.arg means} must be of type `list`"))
  }

  if (!methods::is(formula, "formula")) {
    error_message <- c(error_message, cli::format_error("{.arg formula} must be of type `formula`"))
  }

  if (!is.null(S0) && !methods::is(S0, "numeric")) {
    error_message <- c(error_message, cli::format_error("{.arg S0} must be of type `numeric`"))
  }

  if (!is.null(error_message)) {
    names(error_message) <- rep("*", length(error_message))
    cli::cli_abort(error_message)
  }

  # Creates an object
  object <- list(
    coefficients = coefficients,
    means = means,
    formula = formula,
    S0 = S0,
    # These parameters are defined to announce that they will be there at some point in time
    alpha = NULL,
    S0_type_2 = NULL,
    beta_overall = NULL,
    predictions_aggregated = NULL,
    predictions_data = NULL,
    betax = NULL,
    betax_data = NULL,
    predictions_recal_type_1 = NULL,
    predictions_recal_type_2 = NULL,
    c_index = NULL
  )
  # Assigns the object class
  class(object) <- c("MiceExtVal", "cox")

  return(object)
}
