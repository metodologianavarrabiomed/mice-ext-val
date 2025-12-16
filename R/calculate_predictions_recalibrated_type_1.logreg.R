#' @title
#' Calculates the type 1 recalibrated predictions for a logistic regression model.
#'
#' @description
#' Calculates the type 1 recalibrated predictions for a logistic regression model. The type 1 recalibration is defined by an \eqn{\alpha} parameter that updates the value of the `intercept` (\eqn{\beta_0}) of the model. The log-odds function is rewritten as follows.
#'
#' \deqn{log(\frac{p}{1 - p}) = \alpha + \beta_0 + \beta_1 \cdot X_1 + \beta_2 \cdot X_2 + \dots + \beta_p \cdot X_p}
#'
#' Thus, the predictions are updated by adjusting the `intercept` value in the model against the external validation data. The \eqn{\alpha} parameter is estimated in each of the imputed datasets by deriving a logistic regression model using the model log-odds as offset. The coefficients in all the models are aggregated using the mean. Using the aggregated parameter and the aggregated log-odds the new predictions are calculated as follows.
#'
#' \deqn{\frac{1}{1 + e^{(-(\alpha + (\beta \cdot X)))}}}
#'
#' @param model Model generated with [mv_model_logreg()]. Needs the `predictions` parameter of the model, to generate it the function `calculate_predictions` must be executed over the model. This attribute must be generated using [calculate_predictions()]
#' @param data Data for what the predictions must be recalibrated.
#' @param .progress `TRUE` to render the progress bar, `FALSE` otherwise.
#'
#' @return A model with the parameters `predictions_recal_type_1` and `alpha_type_1` populated.
#'
#'    * `predictions_recal_type_1`: stores the type 1 recalibrated predictions as follows
#'        | id        | prediction           |
#'        |-------------|:-------------:|
#'        | 1 | 0.03 |
#'        | ... | ...|
#'        | n | 0.16 |
#'    * `alpha_type_1`: stores the \eqn{\alpha} recalibration parameter.
#'
#' @import mathjaxr
#'
#' @exportS3Method calculate_predictions_recalibrated_type_1 logreg
#'
#' @examples
#' set.seed(123)
#'
#' model <- mv_model_logreg(formula = event ~ 0.5 * x + 0.3 * z - 1.2)
#'
#' data <- data.frame(
#'   .imp = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   event = survival::Surv(rpois(9, 5), rbinom(n = 9, size = 1, prob = 0.5)),
#'   x = rnorm(9, 1, 0.25),
#'   z = rnorm(9, 2, 0.75)
#' )
#'
#' model |>
#'   calculate_predictions(data) |>
#'   calculate_predictions_recalibrated_type_1(data)
calculate_predictions_recalibrated_type_1.logreg <- function(model, data, .progress = FALSE) {
  error_message <- get_error_message_calculate_recalibrated(model, data)
  if (!is.null(error_message)) cli::cli_abort(error_message)

  # Progress bar code
  if (.progress) {
    env <- rlang::env()
    cli::cli_progress_step("calculating type 1 recalibration parameters", spinner = TRUE, .envir = env)
  }

  alpha_type_1 <- data |>
    dplyr::group_by_at(dplyr::vars(".imp")) |>
    dplyr::group_map(~ {
      # Progress bar code
      if (.progress) {
        cli::cli_progress_update(.envir = env)
      }
      # Obtains the data of the event variable
      dependent_variable <- .x[[all.vars(model$formula)[1]]]
      betax <- model$predictions_imp |>
        dplyr::filter(.imp == .y$.imp) |>
        dplyr::pull(betax)

      if (methods::is(dependent_variable, "Surv")) {
        event <- dependent_variable[, "status"]
      } else {
        event <- dependent_variable
      }

      # Calculates the `alpha` parameter value
      model_recal <- rms::lrm.fit(
        y = event,
        offset = betax
      )
      tibble::tibble(alpha = model_recal$coefficients)
    }) |>
    dplyr::bind_rows() |>
    dplyr::pull("alpha") |>
    mean()

  model$recal_parameters <- tibble::tibble(param = "alpha_type_1", value = alpha_type_1)

  # Calculates the type 1 recalibration
  model$predictions_imp <- model$predictions_imp |>
    dplyr::mutate(
    prediction_type_1 = 1 / (1 + exp(-(.data[["betax"]] + alpha_type_1)))
      )

  if (.progress) {
    cli::cli_progress_done(.envir = env)
  }

  return(model)
}
