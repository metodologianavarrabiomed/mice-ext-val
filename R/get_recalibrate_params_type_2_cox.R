#' @title
#' Obtains the \eqn{S_0(t)} and \eqn{\beta_{overall}} parameters for recalibration
#'
#' @description
#' Calculates the recalibration type 2 parameters for the `recalibrate_data` and returns a list with the two needed parameters \eqn{S_0(t)}, the survival estimation in the time \eqn{t} for the external validation data, and \eqn{\beta_{overall}}, the importance that the \eqn{\beta \cdot X} values should have over the external validation data.
#'
#' The survival function estimation is calculated as a Weibull distribution. A Weibull model is derived in the external validation cohort and the value estimated by the model for the time \eqn{t} is used as the recalibration of \eqn{S_0(t)}. The importance of the \eqn{\beta \cdot X} is calculated by deriving a Cox model using as only covariate the model \eqn{\beta \cdot X} values. The derived Cox model coefficient is used as \eqn{\beta_{overall}}.
#'
#' @param time time of follow up for each patient, must be `numeric`
#' @param event if the patient has suffered an event during follow up, must be `numeric` \eqn{1} event \eqn{0} othercase
#' @param betax the betax values, \eqn{\beta X}, of the original predictions, must be `numeric`
#'
#' @return a `list` with two elements:
#'    * S0: \eqn{S_0(t)}
#'    * beta_overall: \eqn{\beta_{overall}} value from the type 2 recalibration
#'
#' @import mathjaxr
#' @importFrom survival survreg Surv coxph survfit
#' @importFrom methods is
#'
#' @examples
#' \dontrun{
#' get_recalibrate_params_type_2_cox(data, betax, t)
#' }
get_recalibrate_params_type_2_cox <- function(time, event, betax) {
  error_message <- NULL
  if (!methods::is(time, "numeric")) {
    error_message <- c(error_message, cli::format_error("{.arg time} must be of class {.arg numeric}"))
  }
  if (!methods::is(event, "numeric")) {
    error_message <- c(error_message, cli::format_error("{.arg event} must be of class {.arg numeric}"))
  }
  if (!methods::is(betax, "numeric")) {
    error_message <- c(error_message, cli::format_error("{.arg betax} must be of class {.arg numeric}"))
  }

  if (!is.null(error_message)) {
    names(error_message) <- rep("*", length(error_message))
    cli::cli_abort(error_message)
  }

  # Generates a Weibull distribution over the data to obtain the survival function estimator
  recalibrate_data <- data.frame(time = time, event = event, betax = betax)
  recalibrate_data$surv_obj <- survival::Surv(time, event)

  # Calculates the `beta_overall` value from a Cox model derived with `betax` as the only independent variable.
  cox_model <- survival::coxph(surv_obj ~ betax, data = recalibrate_data)
  # `beta_overall` is the unique coefficient in the model.
  beta_overall <- as.numeric(cox_model$coefficients[1])
  s0 <- survival::survfit(cox_model)

  # Returns the two estimated parameters to be aggregated later on
  return(
    list(
      S0 = s0$surv[length(s0$surv)],
      beta_overall = beta_overall
    )
  )
}
