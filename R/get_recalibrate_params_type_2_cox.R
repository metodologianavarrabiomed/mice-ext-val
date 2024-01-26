#' @title
#' Obtains the \eqn{S_0(t)} and \eqn{\beta_{overall}} parameters for recalibration
#'
#' @description
#' Calculates the recalibration type 2 parameters for the `recalibrate_data` and returns a list with the two needed parameters \eqn{S_0(t)}, the survival estimation in the time \eqn{t} for the external validation data, and \eqn{\beta_{overall}}, the importance that the \eqn{\beta \cdot X} values should have over the external validation data.
#'
#' The survival function estimation is calculated as a Weibull distribution. A Weibull model is derived in the external validation cohort and the value estimated by the model for the time \eqn{t} is used as the recalibration of \eqn{S_0(t)}. The importance of the \eqn{\beta \cdot X} is calculated by deriving a Cox model using as only covariate the model \eqn{\beta \cdot X} values. The derived Cox model coefficient is used as \eqn{\beta_{overall}}.
#'
#' @param recalibrate_data dataset that contains the needed info to recalibrate the predictions. The `data.frame` must contain the following columns.
#'    * `time`: time to the end of follow up
#'    * `event`: if there is an event in this row
#' @param betax the betax values, \eqn{\beta X}, of the original predictions
#' @param t time of the study
#'
#' @return a `list` with two elements:
#'    * S0: \eqn{S_0(t)}
#'    * beta_overall: \eqn{\beta_{overall}} value from the type 2 recalibration
#'
#' @import mathjaxr
#' @importFrom survival survreg Surv coxph
#' @importFrom stats pweibull
#' @importFrom methods is
#'
#' @examples
#' \dontrun{
#' get_recalibrate_params_type_2_cox(data, betax, t)
#' }
get_recalibrate_params_type_2_cox <- function(recalibrate_data, betax, t) {
  # Checks preconditions
  stopifnot(methods::is(recalibrate_data, "data.frame"))
  stopifnot("recalibrate_data does not have the needed columns (time, event)" = all(c("time", "event") %in% colnames(recalibrate_data)))
  stopifnot(methods::is(betax, "data.frame"))
  stopifnot(methods::is(t, "numeric"))

  # Generates a Weibull distribution over the data to obtain the survival function estimator
  recalibrate_data$surv_obj <- survival::Surv(recalibrate_data$time + 0.001, recalibrate_data$event)
  weibull <- survival::survreg(surv_obj ~ 1, data = recalibrate_data, dist = "w")

  # Obtain the estimated parameters by the Weibull distribution to parse them into a value in a certain time
  shape <- 1 / exp(as.numeric(weibull$icoef[2]))
  scale <- exp(as.numeric(weibull$icoef[1]))

  # Gets the estimated survival function value in the time `t`
  S_0_5 <- 1 - stats::pweibull(t, shape, scale)

  # Calculates the `beta_overall` value from a Cox model derived with `betax` as the only independent variable.
  recalibrate_data$betax <- unlist(betax)
  cox.model <- survival::coxph(surv_obj ~ betax, data = recalibrate_data)
  # `beta_overall` is the unique coefficient in the model.
  beta_overall <- as.numeric(cox.model$coefficients[1])

  # Returns the two estimated parameters to be aggregated later on
  return(
    list(
      S0 = S_0_5,
      beta_overall = beta_overall
    )
  )
}
