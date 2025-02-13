#' @title
#' Calculates the type 2 recalibrated predictions for a logistic regression model
#'
#' @description
#' Calculates the type 2 recalibrated predictions for a logistic regression model. The type 2 recalibration uses two parameters to update the model predictions, the \eqn{\alpha} parameter allow to update the model `intercept` and the \eqn{\beta_{overall}} parameter allow to update the importance of the log-odds (\eqn{\beta \cdot X}) values. The log-odds function can be rewritten as
#'
#' \deqn{log(\frac{p}{1 - p}) = \alpha + \beta_{overall} \cdot (\beta_0 + \beta_1 \cdot X_1 + \beta_2 \cdot X_2 + \dots + \beta_p \cdot X_p)}
#'
#' The parameters are estimated deriving a logistic regression model in each of the imputations using the model log-odds as only covariate. The coefficients of the model represent the parameter estimations and they are aggregated using the rubin rules. Then, the recalibrated predictions are calculated using these parameters and the aggregated log-odds.
#'
#' \deqn{\frac{1}{1 + e^{(-(\alpha + \beta_{overall}(\beta \cdot X)))}}}
#'
#' @param model Model generated with [mv_model_logreg()]. Needs the `predictions` parameter of the model, to generate it the function [calculate_predictions()] must be executed over the model.
#' @param data Data for what the predictions must be recalibrated.
#' @param .progress `TRUE` to render the progress bar `FALSE` otherwise.
#'
#' @return A model with the parameter `predictons_recalibrated_type_2`, `S0_type_2` and `beta_overall` populated.
#'
#'   * `predictions_recal_type_2`: stores the type 2 recalibrated predictions as follows.
#'        | id | prediction_type_2 |
#'        |-------------|:-------------:|
#'        | 1 | 0.03 |
#'        | ... | ...|
#'        | n | 0.16 |
#'    * `alpha_type_2`: stores the \eqn{\alpha} type 2 recalibration parameter.
#'    * `beta_overall`: stores the \eqn{\beta_{overall}} type 2 recalibration parameter.
#'
#' @import mathjaxr
#' @importFrom dplyr %>% group_by_at group_map filter pull vars bind_rows
#' @importFrom tibble tibble as_tibble
#' @importFrom rms lrm
#' @importFrom progress progress_bar
#' @importFrom methods is
#' @importFrom cli format_error cli_abort
#'
#' @exportS3Method calculate_predictions_recalibrated_type_2 logreg
#'
#' @examples
#' set.seed(123)
#'
#' model <- mv_model_logreg(
#'   coefficients = list(x = 0.5, z = 0.3),
#'   formula = event ~ x + z,
#'   intercept = 1.2
#' )
#'
#' data <- data.frame(
#'   .imp = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   x = rnorm(9, 1, 0.25),
#'   z = rnorm(9, 2, 0.75),
#'   status = c(1, 0, 0, 1, 0, 0, 1, 0, 0),
#'   time = c(2, 3, 5, 2, 3, 5, 2, 3, 5)
#' )
#' data$event <- survival::Surv(data$time, data$status)
#'
#' model |>
#'   calculate_predictions(data) |>
#'   calculate_predictions_recalibrated_type_1(data) |>
#'   calculate_predictions_recalibrated_type_2(data)
calculate_predictions_recalibrated_type_2.logreg <- function(model, data, .progress = FALSE) {
  error_message <- get_error_message_calculate_recalibrated(model, data)
  if (!is.null(error_message)) cli::cli_abort(error_message)

  # Progress bar code
  if (.progress) {
    n_iter <- max(data$.imp) + 1
    pb <- progress::progress_bar$new(
      format = "Type 2 recalibration \t[:bar] :percent [E.T.: :elapsedfull || R.T.: :eta]",
      total = n_iter,
      complete = "=",
      incomplete = "-",
      current = ">",
      clear = FALSE,
      width = 100
    )
  }

  # Calculates the recalibrate parameters for the model
  recal_parameters <- data %>%
    dplyr::group_by_at(dplyr::vars(".imp")) %>%
    dplyr::group_map(~ {
      # Progress bar code
      if (.progress) {
        pb$tick()
      }
      survival_data <- .x[[all.vars(model$formula)[1]]]

      recal_data <- tibble::tibble(
        y = survival_data[, "status"],
        betax = model$betax_data %>%
          dplyr::filter(.imp == .y$.imp) %>%
          dplyr::pull(betax)
      )

      model_recal <- rms::lrm(y ~ betax, data = recal_data)

      tibble::tibble(
        alpha_type_2 = model_recal$coefficients[1],
        beta_overall = model_recal$coefficients[2]
      )
    }) %>%
    dplyr::bind_rows()

  model$alpha_type_2 <- mean(recal_parameters$alpha_type_2)
  model$beta_overall <- mean(recal_parameters$beta_overall)

  # Calculates the type 2 recalibration
  model$predictions_recal_type_2 <- tibble::tibble(
    id = model$betax$id,
    prediction_type_2 = 1 / (1 + exp(-(model$alpha_type_2 + (model$beta_overall * model$betax$betax))))
  )

  if (.progress) {
    pb$tick()
  }

  return(model)
}
