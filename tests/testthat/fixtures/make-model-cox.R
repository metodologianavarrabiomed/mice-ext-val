#' Generates a cox model for testing
#'
#' @description
#' Generates a cox model for testing with fixed parameters for all the tests. The model is supossed to have two covariates \eqn{x} and \eqn{z} and one dependent variable \eqn{y}
#'
#' * `formula`: `event ~ 0.1 * (x - 1) + 0.3 * (z - 2)`
#' * `S0`: `0.8`
#'
#' @param env Environment in which the model should be generated
#'
#' @return A cox model generated with the fixed parameters
make_cox_model <- function(env) {
  mv_model_cox(
    formula = as.formula(event ~ 0.1 * (x - 1) + 0.3 * (z - 2), env = env),
    S0 = 0.8
  )
}
