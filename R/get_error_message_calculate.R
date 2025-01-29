#' Logic for checkin all the needed parameters for `calculate_` functions are
#' correct
#'
#' @param data
#' @param model
#'
#' @return `NULL` if no errors were found or the actual error if some errors
#' were found.
get_error_message_calculate <- function(model, data) {
  error_message <- NULL

  # check that the coefficients variables are in the `data` argument
  if (is.null(model$coefficients)) {
    error_message <- c(error_message, cli::format_error(""))
  }

  if (!is.null(model$coefficients) & !all(names(model$coefficients) %in% colnames(data))) {
    coef_not_in_data <- names(model$coefficients) %in% colnames(data)
    coefs <- names(model$coefficients)[coef_not_in_data]
    error_message <- c(error_message, "*" = cli::format_error("The {.strong model coefficient{?s}} {.var {coefs}} must be present in {.arg data}"))
  }

  # check that the means variables are in the `data` argument
  if (methods::is(model, "cox") & is.null(model$means)) {
    error_message <- c(error_message, cli::format_error(""))
  }

  if (methods::is(model, "cox") & !is.null(model$means) & !all(names(model$means) %in% colnames(data))) {
    mean_not_in_data <- names(model$means) %in% colnames(data)
    means <- names(model$means)[mean_not_in_data]
    error_message <- c(error_message, "*" = cli::format_error("The {.strong model mean{?s}} {.var {means}} must be present in {.arg data}"))
  }


  # check that the dataset is multiple imputed and contains an id
  if (!".imp" %in% colnames(data)) {
    error_message <- c(error_message, "*" = cli::format_error("The variable {.var .imp} (number of imputation each row is part of) must be present in {.arg data}"))
  }

  if (!"id" %in% colnames(data)) {
    error_message <- c(error_message, "*" = cli::format_error("The variable {.var id} (unique identifier for each row) must be present in {.arg data}"))
  }

  # if `model` is a logreg, check for its intercept
  if (methods::is(model, "logreg") &
    (!"intercept" %in% names(model) | !methods::is(model$intercept, "numeric"))) {
    error_message <- c(error_message, "*" = "The {.strong model intercept} must be {.cls numeric} (check if exists)")
  }

  return(error_message)
}
