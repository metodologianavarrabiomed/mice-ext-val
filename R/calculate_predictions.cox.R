#' @title
#' Calculates the predictions for a Cox model
#'
#' @description
#' Calculates the predictions for the given Cox model and external validation data. The predictions are calculated following the definition of a cox model
#'
#'  \deqn{S_0(t)^{e^{(\beta \cdot X)}}}
#'
#'  where \eqn{S_0(t)} stands for the survival function in a time \eqn{t}, \eqn{\beta} stands for the model coefficients and \eqn{X} stands for the centered values in each `id`.
#'
#' @param model Model generated with [mv_model_cox()]
#' @param data External validation data. Multiple imputation dataset in long format.
#' @param .progress `TRUE` to render the progress bar `FALSE` otherwise.
#'
#' @return The `model` with the parameters `predictions_aggregated`, `predictions_data`, `betax` and `betax_data` populated.
#'
#'   * `predictions_aggregated`, stores the predictions aggregated by the mean.
#'   * `predictions_data`, stores all the predictions in each of the imputed datasets.
#'   * `betax`, stores the \eqn{\beta \cdot X} values aggregated by the mean.
#'   * `betax_data`, stores the \eqn{\beta \cdot X} values in each of the imputed datasets.
#'
#'
#' @import mathjaxr
#' @importFrom dplyr group_by_at group_map bind_rows rename vars mutate all_of summarise
#' @importFrom tibble tibble as_tibble add_column
#' @importFrom methods is
#' @importFrom cli format_error cli_abort cli_progress_update cli_progress_done cli_progress_step
#' @importFrom rlang env
#'
#' @exportS3Method calculate_predictions cox
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
#'   event = survival::Surv(rpois(9, 5), rbinom(n = 9, size = 1, prob = 0.2)),
#'   x = rnorm(9, 1, 0.25),
#'   z = rnorm(9, 2, 0.75)
#' )
#'
#' model |> calculate_predictions(data)
calculate_predictions.cox <- function(model, data, .progress = FALSE) {
  error_message <- get_error_message_calculate(model, data)
  if (!is.null(error_message)) {
    cli::cli_abort(error_message)
  }

  if (.progress) {
    env <- rlang::env()
    cli::cli_progress_step("calculating betax predictions", spinner = TRUE, .envir = env)
  }

  model$betax_data <- data |>
    dplyr::group_by_at(dplyr::vars(".imp")) |>
    dplyr::group_map(~ {
      if (.progress) {
        cli::cli_progress_update(.envir = env)
      }
      with(.x, {
        eval(eval(model$formula[[3]]))
      }) |>
        tibble::as_tibble() |>
        tibble::add_column(.imp = .y$.imp) |>
        tibble::add_column(id = .x$id) |>
        dplyr::rename("betax" = value)
    }) |>
    dplyr::bind_rows()

  if (.progress) {
    cli::cli_progress_done(.envir = env)
    cli::cli_progress_step("aggregating predictions", .envir = env)
  }

  model$predictions_data <- model$betax_data |>
    dplyr::mutate(prediction = 1 - model$S0^exp(eval(.data[["betax"]]))) |>
    dplyr::select(dplyr::all_of(c("prediction", ".imp", "id")))

  if (.progress) {
    cli::cli_progress_done(.envir = env)
    cli::cli_progress_step("calculating predictions", .envir = env)
  }

  # Generates the aggregated `predictions` and stores them into the model
  model$predictions_aggregated <- model$predictions_data |>
    dplyr::group_by_at(dplyr::vars("id")) |>
    dplyr::summarise(prediction = mean(.data[["prediction"]]))

  if (.progress) {
    cli::cli_progress_done(.envir = env)
    cli::cli_progress_step("aggregating betax", .envir = env)
  }
  # Generates the aggregated `betax` and stores them into the model
  model$betax <- model$betax_data |>
    dplyr::group_by_at(dplyr::vars("id")) |>
    dplyr::summarise(betax = mean(.data[["betax"]]))

  if (.progress) {
    cli::cli_progress_done(.envir = env)
  }

  return(model)
}
