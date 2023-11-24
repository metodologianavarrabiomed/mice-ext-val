#' @title
#' Generates the `data.frame` needed for the calibration plot
#'
#' @description
#' Generates the `data.frame` needed for the calibration plot. The calibration plot needs to separate the model predictions by risk groups. First the function separates the predictions in `n_groups` then computes the mean value of the model predictions and also the observed value. The observed value is the estimated value at the study time estimated using a Kaplan-Meier estimator. Finally it returns a `tibble` that has the following structure.
#'
#' | group        | prediction           | observed  |
#' |-------------|:-------------:|:-----:|
#' | 1 | 0.03 | 0.05 |
#' | ...      | ...      |   ... |
#' | n_group      | 0.84      |   0.79 |
#'
#' If the model has the variables of the recalibrations this function also generates the values and store them in the result `tibble`.
#'
#' @param model Model generated with `mv_model`. Needs the `predictions` parameter of the model, to generate it the function `calculate_predictions` must be executed over the model. If we want to obtain also the recalibrated data the model must be initalize the recalibrated predictions with `calculate_predictions_recalibrated_type_1` and `calculate_predictions_recalibrated_type_2`.
#' @param data Data for what the observed predictios will be calculated.
#' @param n_groups Number of groups that must be calculated.
#' @param type What predictions should the function generate the
#'
#' @return `tibble` with the data ready to generate a calibration plot.
#'
#' @export
#'
#' @examples
#' get_calibration_plot_data(model, data, 10)
get_calibration_plot_data <- function(model, data, n_groups, time, type = "predictions_aggregated") {
  stopifnot(is(model, "MiceExtVal"))
  stopifnot(is(data, "data.frame"))
  stopifnot(is(n_groups, "numeric"))
  stopifnot("Variable type is not a prediction attribute in model" = any(type %in% c("predictions_aggregated", "predictions_recal_type_1","predictions_recal_type_2")))

  # We assume that the observed variable is completed and therefore the same in all the imputed datasets. If not we should generate the aggregate result using Rubin Rules.
  original_data <- data %>%
    dplyr::filter(.imp == 1) %>%
    dplyr::select(id, event)

  # Select the variable that is used from the model
  pred_var <- as.name(names(model[[type]])[2])
  model[[type]] %>%
    # Generates the groups by the prediction variable and group by the generated group
    dplyr::mutate(group = dplyr::ntile(!!pred_var, n_groups)) %>%
    dplyr::left_join(original_data) %>%
    dplyr::group_by(group) %>%
    # Calculates the predicted and observed value for each of the predicted risk groups
    dplyr::group_map(~ {
      .x$survobj <- survival::Surv(time = .x$event[, "time"], event = .x$event[, "status"])
      # Estimates the observed risk inside the group
      km <- survival::survfit(survobj ~ 1, data = .x)
      return(
        tibble(
          bin = .y$group,
          predicted = mean(.x[[pred_var]]),
          # Should be calculated for a certain time given by the user.
          observed = 1 - km$surv[length(km$surv)], 
          se_observed = km$std.err[length(km$std.err)],
          # It could be that these values go outside the range [0, 1], so we should clip them to this range. It should be also an option the generation of the CI.
          ll = observed - 1.96 * se_observed,
          ul = observed + 1.96 * se_observed
        )
      )
    }) %>%
    do.call(rbind, args = .) %>%
    return()
}
