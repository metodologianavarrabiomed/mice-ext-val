calculate_predictions_recalibrated_type_1 <- function(model, ...){
  stopifnot(is(model, "MiceExtVal"))

  UseMethod("calculate_predictions_recalibrated_type_1", model)
}
