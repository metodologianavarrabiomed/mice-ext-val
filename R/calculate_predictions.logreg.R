#' @title
#' Calculates the predictions of the model
#'
#' @description
#' Calculates the predictions of the `model` for the given `data`. The predictions in a logistic regression model are calculated as ...
#'
#' @param model Model for which the predictions are calculated.
#' @param data Data for the predictions.
#'
#' @return A model with the parameters `predictions_aggregated`, `predictions_data`, `betax` and `betax_data` populated.
#'
#' @export
#'
#' @examples
#' model |>
#'    calculate_predictions(data)
calculate_predictions.logreg <- function(model, data) {
  print("Done")

  # Checks pre-conditions
  # stopifnot(is(model, "MiceExtVal"))
  # stopifnot(is(data, "data.frame"))

}
