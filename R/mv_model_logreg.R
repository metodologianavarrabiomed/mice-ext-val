mv_model_logreg <- function(coefficients, formula, intercept) {
  model <- list(
    coefficients = coefficients,
    formula = formula,
    intercept = intercept,
    alpha_type_1 = NULL,
    alpha_type_2 = NULL,
    beta_overall = NULL,
    predictions_aggregated = NULL,
    predictions_data = NULL,
    log_odds = NULL,
    log_odds_data = NULL,
    predictions_recal_type_1 = NULL,
    predictions_recal_type_2 = NULL,
    c_index = NULL
  )

  class(model) <- c("MiceExtVal", "logreg")

  return(model)
}
