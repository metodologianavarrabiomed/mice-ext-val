#' @title
#' Calculates the type 2 recalibrated predictions for a logistic regression model
#'
#' @description
#' Calculates the type 2 recalibrated predictions for a logistic regression model following the guidance in ...
#'
#' @param model Model generated with `mv_model`. Needs the `predictions` parameter of the model, to generate it the function `calculate_predictions` must be executed over the model.
#' @param data Data for what the predictions must be recalibrated.
#' @param .progress `TRUE` to render the progress bar `FALSE` otherwise.
#'
#' @return A model with the parameter `predictons_recalibrated_type_2`, `S0_type_2` and `beta_overall` populated.
#' @export
#'
#' @examples
#' model %>%
#'   calculate_predictions(data) |>
#'   calculate_predictions_recalibrated_type_1(data) |>
#'   calculate_predictions_recalibrated_type_2(data)
calculate_predictions_recalibrated_type_2.logreg <- function(model, data, .progress = TRUE) {

}
