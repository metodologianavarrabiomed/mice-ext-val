#' @title
#' Calculates the type 1 recalibration predictions for a model.
#'
#' @description
#' A generic method for calculating the type 1 recalibration predictions for a model.
#'
#' @param model Model for which the recalibrated predictions are calculated
#' @param ... Parameters for [calculate_predictions_recalibrated_type_1.cox()] function or for [calculate_predictions_recalibrated_type_1.logreg()] function.
#'
#' @return A model with the parameter `predictions_recal_type_1` and also the recalibration parameters are populated.
#'
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' model |>
#'    calculate_predictions_recalibrated_type_1(data)
calculate_predictions_recalibrated_type_1 <- function(model, ...){
  stopifnot(methods::is(model, "MiceExtVal"))

  UseMethod("calculate_predictions_recalibrated_type_1", model)
}
