calculate_predictions_recalibrated_type_2 <- function(model, ...) {
  stopifnot(is(model, "MiceExtVal"))

  UseMethod("calculate_predictions_recalibrated_type_2", model)
}
