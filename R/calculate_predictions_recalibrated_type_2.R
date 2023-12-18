#' @title
#' Calculates the type 2 recalibration predictions for a model.
#'
#' @description
#' A generic method for calculating the type 2 recalibration predictions for the given model.
#'
#' @param model Model for which the recalibrated predictions are calculated
#' @param ... Parameters for [calculate_predictions_recalibrated_type_2.cox()] function or [calculate_predictions_recalibrated_type_2.logreg()] function
#'
#' @return A model with the parameter `predictions_recal_type_2` and also the recalibration parameters are populated.
#'
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' #' model |>
#'    calculate_predictions_recalibrated_type_2(data)
calculate_predictions_recalibrated_type_2 <- function(model, ...) {
  stopifnot(methods::is(model, "MiceExtVal"))

  UseMethod("calculate_predictions_recalibrated_type_2", model)
}
