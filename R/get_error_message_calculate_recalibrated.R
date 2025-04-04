#' generates the error messages for the recalibrated predictions
#'
#' @param model `MiceExtVal` model to check the errors
#' @param data External validation dataset where the model should be evaluated
#'
#' @returns the error message for the given model and data
get_error_message_calculate_recalibrated <- function(model, data) {
  is_dichotomous <- \(x) is.numeric(x) & length(unique(x)) == 2

  error_message <- NULL
  # check that the dataset is multiple imputed and contains an id
  if (!".imp" %in% colnames(data)) {
    error_message <- c(error_message, "*" = cli::format_error("The variable {.var .imp} (number of imputation each row is part of) must be present in {.arg data}"))
  }

  if (!"id" %in% colnames(data)) {
    error_message <- c(error_message, "*" = cli::format_error("The variable {.var id} (unique identifier for each row) must be present in {.arg data}"))
  }

  # Returns an error if `predictions_data` does not exist in `model`
  if (!"predictions_data" %in% names(model) | !methods::is(model$predictions_data, "data.frame")) {
    error_message <- c(error_message, "*" = cli::format_error("In {.arg model} there should be the argument {.arg predictions_data} {.cls tibble} calculated (see {.fn MiceExtVal::calculate_predictions})"))
  }

  # Returns an error if the dependent variable in the model formula does not exist
  # in `data` or is not a survival class
  dependent_variable <- all.vars(model$formula)[1]
  if (!dependent_variable %in% colnames(data)) {
    error_message <- c(error_message, "*" = cli::format_error("The dependent variable {.var {dependent_variable}} must be part of {.arg data}"))
  }
  # the dependent variable must be checked differently in each model
  if (methods::is(model, "cox") & !methods::is(data[[dependent_variable]], "Surv")) {
    error_message <- c(error_message, "*" = cli::format_error("The dependent variable {.var {dependent_variable}} must be {.cls Surv}"))
  }

  if (
    methods::is(model, "logreg") &
      (!methods::is(data[[dependent_variable]], "Surv") & !methods::is(data[[dependent_variable]], "numeric"))
  ) {
    error_message <- c(error_message, "*" = cli::format_error("The dependent variable {.var {dependent_variable}} must be {.cls {c('Surv', 'numeric')}}"))
  } else {
    if (methods::is(data[[dependent_variable]], "numeric") & !is_dichotomous(data[[dependent_variable]])) {
      error_message <- c(error_message, "*" = cli::format_error("The dependent variable {.var {dependent_variable}} must be {.arg dichotomous}"))
    }
  }

  if (methods::is(model, "cox")) {
    # Returns an error if `S0` does not exist or it is bad defined in the cox model
    if (is.null(model$S0) | !is.numeric(model$S0)) {
      error_message <- c(error_message, "*" = cli::format_error("{.arg S0} must be {.cls numeric}"))
    }
  }

  return(error_message)
}
