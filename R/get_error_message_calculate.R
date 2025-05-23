#' Logic for checkin all the needed parameters for `calculate_` functions are
#' correct
#'
#' @param model `MiceExtVal` model to check the errors
#' @param data External validation dataset where the model should be evaluated
#'
#' @return `NULL` if no errors were found or the actual error if some errors
#' were found.
#'
#' @noRd
get_error_message_calculate <- function(model, data) {
  error_message <- NULL

  if (!is.null(model$formula) & !all(all.vars(model$formula) %in% colnames(data))) {
    variables_not_present <- all.vars(model$formula) %in% colnames(data)
    variables <- all.vars(model$formula)[!variables_not_present]
    error_message <- c(error_message, "*" = cli::format_error("The {.strong model variable{?s}} {.var {variables}} must be present in {.arg data}"))
  }

  # check that the dataset is multiple imputed and contains an id
  if (!".imp" %in% colnames(data)) {
    error_message <- c(error_message, "*" = cli::format_error("The variable {.var .imp} (number of imputation each row is part of) must be present in {.arg data}"))
  }

  if (!"id" %in% colnames(data)) {
    error_message <- c(error_message, "*" = cli::format_error("The variable {.var id} (unique identifier for each row) must be present in {.arg data}"))
  }

  return(error_message)
}
