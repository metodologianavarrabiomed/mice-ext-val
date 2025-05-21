#' Generates the forestplot data needed in [get_forestplot()]
#'
#' @param strat Name of the strat that the models are part of
#' @param type Choosen statistic to plot. It must be already calculated
#' @param ... A list of models that should be plotted
#'
#' @returns A `tibble` with the information needed to generate the forestplot
#'
#' @importFrom purrr map2_chr map_lgl map_df
#' @importFrom methods is
#' @importFrom tibble tibble
#' @importFrom cli format_error cli_abort
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_forestplot_data(strat = "overall", cox_model, logreg_model)
#' data <- get_forestplot_data(strat = "overall", Cox = cox_model, LogReg = logreg_model)
#' }
get_forestplot_data <- function(strat, type = c("c_index", "auc", "brier_score", "brier_score_type_1", "brier_score_type_2"), ...) {
  # get model and model names -----------------------------------------------
  models <- list(...)

  model_names_call <- as.character(as.list(match.call())[-c(1:3)])
  model_names_callname <- if (!is.null(names(models))) names(models) else rep(NA, length(model_names_call))

  model_names <- purrr::map2_chr(model_names_callname, model_names_call, ~ ifelse(is.na(.x) | .x == "", .y, .x))

  # assertions --------------------------------------------------------------
  error_message <- NULL

  is_model_class <- purrr::map_lgl(models, ~ methods::is(.x, "MiceExtVal"))
  if (!all(is_model_class)) {
    error_message <- c(error_message, "*" = cli::format_error("The model{?s} {.arg {model_names[!is_model_class]}} must be {.cls MiceExtVal}"))
  }

  has_stats <- purrr::map_lgl(models, ~ methods::is(.x, "MiceExtVal") && !is.null(.x[[type]]))
  if (all(is_model_class) & !all(has_stats)) {
    error_message <- c(error_message, "*" = cli::format_error("The {.arg {model_names[!has_stats]}} model{?s} must have their {.arg {type}} calculated, consider using {.fn MiceExtVal::calculate_harrell_c_index}, {.fn MiceExtVal::calculate_auc} or {.fn MiceExtVal::calculate_brier_score}"))
  }

  if (!is.null(error_message)) {
    cli::cli_abort(error_message)
  }

  # generate tibble ---------------------------------------------------------
  purrr::map_df(seq_along(models), ~ {
    tibble::tibble(
      model = model_names[[.x]],
      strat = strat,
      estimate = models[[.x]][[type]]["Estimate"],
      lower = models[[.x]][[type]]["95% CI L"],
      upper = models[[.x]][[type]]["95% CI U"]
    )
  })
}
