#' Title
#'
#' @param model
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
calculate_predictions <- function(model, ...) {
  stopifnot(is(model, "MiceExtVal"))

  UseMethod("calculate_predictions", model)
}
