#' @title
#' Calculates the type 1 recalibrated predictions
#'
#' @description
#' Using the function `get_recalibrate_params_type_1` calculates the recalibration parameters in each of the imputed datasets stored in `data`. With all the parameters estimated aggregates them and calculates the recalibrated predictions with these aggregated parameters and the aggregated predictions.
#'
#' @param model Model generated with [mv_model_cox()]. Needs the `predictions` parameter of the model, to generate it the function [calculate_predictions()] must be executed over the model.
#' @param data External validation data. Multiple imputation dataset in long format.
#' @param .progress `TRUE` to render the progress bar `FALSE` otherwise.
#'
#' @return A model with the parameter `predictions_recal_type_1` and `alpha` populated.
#'
#'    * `predictions_recal_type_1`: stores the type 1 recalibrated predictions stored as follows
#'        | id        | prediction           |
#'        |-------------|:-------------:|
#'        | 1 | 0.03 |
#'        | ... | ...|
#'        | n | 0.16 |
#'    * `alpha`: stores the \eqn{\alpha} recalibration parameter.
#'
#' @importFrom dplyr %>% group_by_at group_map filter pull vars
#' @importFrom tibble tibble as_tibble
#' @importFrom methods is
#' @importFrom cli format_error cli_abort cli_progress_update cli_progress_done cli_progress_step
#' @importFrom rlang env
#'
#' @exportS3Method calculate_predictions_recalibrated_type_1 cox
#'
#' @examples
#' set.seed(123)
#'
#' model <- mv_model_cox(
#'   formula = event ~ 0.5 * (x - 1) + 0.3 * (z - 2),
#'   S0 = 0.98765
#' )
#'
#' data <- data.frame(
#'   .imp = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   x = rnorm(9, 1, 0.25),
#'   z = rnorm(9, 2, 0.75),
#'   event = survival::Surv(rpois(9, 5), rbinom(n = 9, size = 1, prob = 0.5))
#' )
#'
#' model |>
#'   calculate_predictions(data) |>
#'   calculate_predictions_recalibrated_type_1(data)
calculate_predictions_recalibrated_type_1.cox <- function(model, data, .progress = FALSE) {
  error_message <- get_error_message_calculate_recalibrated(model, data)
  if (!is.null(error_message)) cli::cli_abort(error_message)

  # Progress bar code
  if (.progress) {
    env <- rlang::env()
    cli::cli_progress_step("calculating type 1 recalibration parameters", spinner = TRUE, .envir = env)
  }

  # Obtain the `alpha` value
  model$alpha <- data %>%
    dplyr::group_by_at(dplyr::vars(".imp")) %>%
    dplyr::group_map(~ {
      # Progress bar code
      if (.progress) {
        cli::cli_progress_update(.envir = env)
      }

      # Obtains the data of the event variable
      survival_data <- .x[[all.vars(model$formula)[1]]]
      survival_predictions <- 1 - model$predictions_data %>%
        dplyr::filter(.imp == .y$.imp) %>%
        dplyr::pull(prediction)

      # Calculates the `alpha` parameter value
      get_recalibrate_param_type_1_cox(
        time = survival_data[, "time"],
        event = survival_data[, "status"],
        survival_predictions = survival_predictions
      )
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::pull("alpha") %>%
    mean() # Aggregates the results, no rubin rules here

  # Progress bar code
  if (.progress) {
    cli::cli_progress_done(.envir = env)
    cli::cli_progress_step("recalibrating predictions with type 1 recalibration", spinner = TRUE, .envir = env)
  }

  # Calculates the recalibrated type 1 predictions
  model$predictions_recal_type_1 <- model$predictions_aggregated %>%
    dplyr::group_by_at(dplyr::vars("id")) %>%
    dplyr::group_map(~ {
      if (.progress) {
        cli::cli_progress_update(.envir = env)
      }
      tibble::tibble(
        id = .y$id,
        # Generation of the recalibrated predictions
        prediction_type_1 = 1 - exp(-exp(model$alpha + log(-log(1 - .x$prediction))))
      )
    }) %>%
    dplyr::bind_rows()

  if (.progress) {
    cli::cli_progress_done(.envir = env)
  }


  return(model)
}
