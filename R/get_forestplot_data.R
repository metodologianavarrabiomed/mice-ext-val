#' Generates the forestplot data needed in [get_forestplot()]
#'
#' @param strat Name of the strat that the models are part of
#' @param type Choosen statistic to plot. It must be already calculated
#' @param ... A list of models that should be plotted
#'
#' @returns A `tibble` with the information needed to generate the forestplot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_forestplot_data(strat = "overall", cox_model, logreg_model)
#' data <- get_forestplot_data(strat = "overall", Cox = cox_model, LogReg = logreg_model)
#' }
get_forestplot_data <- function(strat, type = c("harrell_c_index", "auc", "brier_score", "brier_score_type_1", "brier_score_type_2"), ...) {
  # get model and model names -----------------------------------------------
  models <- rlang::dots_list(
    ...,
    .named = TRUE, .ignore_empty = "all", .homonyms = "error"
  )

  # assertions --------------------------------------------------------------
  error_message <- NULL

  is_model_class <- purrr::map_lgl(models, ~ methods::is(.x, "MiceExtVal"))
  if (!all(is_model_class)) {
    error_message <- c(error_message, "*" = cli::format_error("The model{?s} {.arg {names(models)[!is_model_class]}} must be {.cls MiceExtVal}"))
  }

  has_stats <- purrr::map_lgl(models, ~ methods::is(.x, "MiceExtVal") && !is.null(.x[["results_agg"]]) && (.x[["results_agg"]] |> dplyr::filter(name == type) |> dplyr::count() > 0))
  if (all(is_model_class) & !all(has_stats)) {
    error_message <- c(error_message, "*" = cli::format_error("The {.arg {names(models)[!has_stats]}} model{?s} must have their {.arg {type}} calculated, consider using {.fn MiceExtVal::calculate_harrell_c_index}, {.fn MiceExtVal::calculate_auc} or {.fn MiceExtVal::calculate_brier_score}"))
  }

  if (!is.null(error_message)) {
    cli::cli_abort(error_message)
  }

  # generate tibble ---------------------------------------------------------
  purrr::map_df(seq_along(models), ~ {
    res_agg <- models[[.x]][["results_agg"]] |> dplyr::filter(name == type)

    tibble::tibble(
      model = names(models)[[.x]],
      strat = strat,
      estimate = res_agg |> dplyr::pull("estimate"),
      lower = res_agg |> dplyr::pull("lower"),
      upper = res_agg |> dplyr::pull("upper"),
    )
  })
}
