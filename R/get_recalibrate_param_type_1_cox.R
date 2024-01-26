#' @title
#' Obtain the \eqn{\alpha} value for the recalibration.
#'
#' @description
#' Obtains the recalibration parameters following the type 1 recalibration definition. Recalibrates the predictions with the type 1 recalibration The type 1 recalibration follows the guidelines presented by Crowson et al. (2016). The recalibration follows the next proposition to obtain an \eqn{\alpha} parameter that allow to recalibrate the predictions for the new validation dataset. The recalibration adjusts the incidence of event known by the model to the new dataset.
#'
#' The \eqn{\alpha} value of the recalibration is calculated as the difference between the reported survival equations \eqn{S_{model}(t) - S_0(t)}. Where \eqn{S_0(t)} is calculated for the new data and \eqn{S_{model}(t)} is the model-reported-value for the study time.
#'
#' @references
#' C. S. Crowson, “Assessing calibration of prognostic risk scores - Cynthia S Crowson, Elizabeth J Atkinson, Terry M Therneau, 2016,” Statistical Methods in Medical Research, 2016. <https://journals.sagepub.com/doi/10.1177/0962280213497434> (accessed Nov. 17, 2023).
#'
#' @param recalibrate_data dataset that contains the needed info to recalibrate the predictions. The `data.frame` must contain the following columns.
#'    * `time`: time to the end of follow up
#'    * `event`: if there is an event in this row
#'    * `pi`: the PI values of the original predictions PI stands for \eqn{\beta X}
#'
#' @param s0 value of the reported survival function for the study time (original model)
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
get_recalibrate_param_type_1_cox <- function(recalibrate_data, s0) {
  # Checks preconditions
  stopifnot(methods::is(recalibrate_data, "data.frame"))
  stopifnot(methods::is(s0, "numeric"))
  stopifnot("recalibrate_data does not have the needed columns (time, event)" = all(c("time", "event") %in% colnames(recalibrate_data)))

  # Generates the KM estimator from which we will estimate the `alpha` parameter
  recalibrate_data$surv_obj <- survival::Surv(recalibrate_data$time, recalibrate_data$event)
  st = survival::survfit(recalibrate_data$surv_obj ~ 1, data = recalibrate_data)

  # Returns the `alpha` parameter
  return(log(-log(st$surv[length(st$surv)])) - log(-log(s0)))
}

