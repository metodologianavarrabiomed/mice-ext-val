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
#' @importFrom progress progress_bar
#' @importFrom methods is
#'
#' @exportS3Method calculate_predictions_recalibrated_type_1 cox
#'
#' @examples
#' set.seed(123)
#'
#' model <- mv_model_cox(
#'   coefficients = list(x = 0.5, z = 0.3),
#'   means = list(x = 1, z = 2),
#'   formula = event ~ x + z,
#'   S0 = 0.98765
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
#'   calculate_predictions_recalibrated_type_1(data)
calculate_predictions_recalibrated_type_1.cox <- function(model, data, .progress = FALSE) {
  error_message <- NULL


  # Returns an error if `.imp` is not part of the `data` parameter
  if (!".imp" %in% colnames(data)) {
    error_message <- c(error_message, cli::format_error("{.arg data} variable must contain {.arg .imp}"))
  }

  # Returns an error if `id` is not part of the `data` parameter
  if (!"id" %in% colnames(data)) {
    error_message <- c(error_message, cli::format_error("{.arg data} variable must contain {.arg id}"))
  }

  # Returns an error if `predictions_data` does not exist in `model`
  if (!"predictions_data" %in% names(model) | !methods::is(model$predictions_data, "data.frame")) {
    error_message <- c(error_message, cli::format_error("{.arg model} must have {.arg predictions_data} calculated"))
  }

  # Returns an error if the dependent variable in the model formula does not exist
  # in `data` or is not a survival class
  dependent_variable <- all.vars(model$formula)[1]
  if (!dependent_variable %in% colnames(data)) {
    error_message <- c(error_message, cli::format_error("the dependent variable must be part of {.arg data}"))
  }
  if (!methods::is(data[[dependent_variable]], "Surv")) {
    error_message <- c(error_message, cli::format_error("the dependent variable must be of class {.arg Surv}"))
  }

  # Returns an error if `S0` does not exist or it is bad defined in the cox model
  if (is.null(model$S0) | !is.numeric(model$S0)) {
    error_message <- c(error_message, cli::format_error("{.arg S0} must be a {.arg numeric}"))
  }

  if (!is.null(error_message)) {
    names(error_message) <- rep("*", length(error_message))
    cli::cli_abort(error_message)
  }

  # Progress bar code
  if (.progress) {
    n_iter <- max(data$.imp) + 1
    pb <- progress::progress_bar$new(
      format = "Type 1 recalibration \t[:bar] :percent [E.T.: :elapsedfull || R.T.: :eta]",
      total = n_iter,
      complete = "=",
      incomplete = "-",
      current = ">",
      clear = FALSE,
      width = 100
    )
  }

  # Obtain the `alpha` value
  model$alpha <- data %>%
    dplyr::group_by_at(dplyr::vars(".imp")) %>%
    dplyr::group_map(~ {
      # Progress bar code
      if (.progress) {
        pb$tick()
      }

      # Obtains the data of the event variable
      survival_data <- .x[[all.vars(model$formula)[1]]]
      survival_predictions <- model$predictions_data %>%
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

  # Calculates the recalibrated type 1 predictions
  model$predictions_recal_type_1 <- model$predictions_aggregated %>%
    dplyr::group_by_at(dplyr::vars("id")) %>%
    dplyr::group_map(~ {
      tibble(
        id = .y$id,
        # Generation of the recalibrated predictions
        prediction_type_1 = 1 - exp(-exp(model$alpha + log(-log(1 - .x$prediction))))
      )
    }) %>%
    dplyr::bind_rows()

  # Progress bar code
  if (.progress) {
    pb$tick()
  }

  return(model)
}
