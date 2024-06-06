#' @title
#' Calculates the type 2 recalibration predictions for a model.
#'
#' @description
#' A generic method for calculating the type 2 recalibration predictions for the given model.
#'
#' @param model Model for which the recalibrated predictions are calculated
#' @param data Data parameter for [calculate_predictions_recalibrated_type_2.cox()] function or [calculate_predictions_recalibrated_type_2.logreg()] function
#' @param .progress .progress parameter for [calculate_predictions_recalibrated_type_2.cox()] function or [calculate_predictions_recalibrated_type_2.logreg()] function
#'
#' @return A model with the parameter `predictions_recal_type_2` and also the recalibration parameters are populated.
#'
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' model <- mv_model_logreg(
#'   coefficients = list(x = 0.5, z = 0.3),
#'   formula = event ~ x + z,
#'   intercept = 1.2
#' )
#'
#' data <- data.frame(
#'   .imp = c(1,1,1,2,2,2,3,3,3),
#'   id = c(1,2,3,1,2,3,1,2,3),
#'   x = rnorm(9, 1, 0.25),
#'   z = rnorm(9, 2, 0.75),
#'   status = c(1,0,0,1,0,0,1,0,0),
#'   time = c(2,3,5,2,3,5,2,3,5)
#' )
#' data$event <- survival::Surv(data$time, data$status)
#'
#' model |>
#'   calculate_predictions(data) |>
#'   calculate_predictions_recalibrated_type_1(data) |>
#'   calculate_predictions_recalibrated_type_2(data)
calculate_predictions_recalibrated_type_2 <- function(model, data, .progress = FALSE) {
  error_message <- NULL

  if (!methods::is(model, "MiceExtVal")) {
    error_message <- c(error_message, cli::format_error("{.arg model} must be of class {.arg MiceExtVal}"))
  }

  if (!methods::is(data, "data.frame")) {
    error_message <- c(error_message, cli::format_error("{.arg data} must be of class {.arg data.frame}"))
  }

  if (!is.null(error_message)) {
    names(error_message) <- rep("*", length(error_message))
    cli::cli_abort(error_message)
  }

  UseMethod("calculate_predictions_recalibrated_type_2", model)
}
