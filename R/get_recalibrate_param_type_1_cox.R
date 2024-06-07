#' @title
#' Obtain the \eqn{\alpha} value for the recalibration.
#'
#' @description
#' Obtains the recalibration parameters following the type 1 recalibration definition. Recalibrates the predictions with the type 1 recalibration The type 1 recalibration follows the guidelines presented by Crowson et al. (2016). The recalibration follows the next proposition to obtain an \eqn{\alpha} parameter that allow to recalibrate the predictions for the new validation dataset. The recalibration adjusts the incidence of event known by the model to the new dataset.
#'
#' The \eqn{\alpha} value of the recalibration is calculated as the difference between the survival basal function reported by the model with the mean value of the model predictions in the external validation cohort.
#'
#' \deqn{\alpha = S_{model}(t) - \text{mean predictions}}.
#'
#' @references
#' C. S. Crowson, “Assessing calibration of prognostic risk scores - Cynthia S Crowson, Elizabeth J Atkinson, Terry M Therneau, 2016,” Statistical Methods in Medical Research, 2016. <https://journals.sagepub.com/doi/10.1177/0962280213497434> (accessed Nov. 17, 2023).
#'
#' @param time time of follow up for each patient, must be `time`
#' @param event if the patient suffers an event or not, must be `logical`
#' @param survival_predictions survival predictions for each of the patients, must be `numeric`
#'
#' @return The \eqn{\alpha} value.
#'
#' @import mathjaxr
#' @importFrom survival Surv survfit
#' @importFrom methods is
#'
#' @examples
#' \dontrun{
#' get_recalibrate_param_type_1_cox(data, s0)
#' }
get_recalibrate_param_type_1_cox <- function(time, event, survival_predictions) {
  error_message <- NULL
  if (methods::is(time, "numeric")) {
    error_message <- c(error_message, cli::format_error("{.arg model} must be of class {.arg numeric}"))
  }
  if (methods::is(event, "numeric")) {
    error_message <- c(error_message, cli::format_error("{.arg event} must be of class {.arg numeric}"))
  }
  if (methods::is(survival_predictions, "numeric")) {
    error_message <- c(error_message, cli::format_error("{.arg survival_predictions} must be of class {.arg numeric}"))
  }

  if (!is.null(error_message)) {
    names(error_message) <- rep("*", length(error_message))
    cli::cli_abort(error_message)
  }

  # Generates the KM estimator from which we will estimate the `alpha` parameter
  recalibrate_data <- data.frame(time = time, event = event)
  recalibrate_data$surv_obj <- survival::Surv(time, event)
  st <- survival::survfit(recalibrate_data$surv_obj ~ 1, data = recalibrate_data)

  # Returns the `alpha` parameter
  return(log(-log(st$surv[length(st$surv)])) - log(-log(mean(survival_predictions))))
}
