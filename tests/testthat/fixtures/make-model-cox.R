#' Generates a cox model for testing
#'
#' @description
#' Generates a cox model for testing with fixed parameters for all the tests. The model is supossed to have two covariates \eqn{x} and \eqn{z} and one dependent variable \eqn{y}
#'
#' * `coefficients`: `list(x = 0.1, z = 0.3)`
#' * `means`: `list(x = 1, z = 2)`
#' * `formula`: `event ~ x + z`
#' * `S0`: `0.8`
#'
#' @param env Environment in which the model should be generated
#'
#' @return A cox model generated with the fixed parameters
make_cox_model <- function(env) {
  mv_model_cox(
    coefficients = list(x = 0.1, z = 0.3),
    means = list(x = 1, z = 2),
    formula = as.formula(event ~ x + z, env = env),
    S0 = 0.8
  )
}
