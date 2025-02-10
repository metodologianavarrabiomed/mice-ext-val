#' Generates a logreg model for testing
#'
#' @description
#' Generates a logreg model for testing with fixed parameters for all the tests. The model is supossed to have two covariates \eqn{x} and \eqn{z} and one dependent variable \eqn{y}
#'
#' * `coefficients`: `list(x = 0.5, z = 0.7)`
#' * `formula`: `event ~ x + z`
#' * `intercep`: `0.9`
#'
#' @param env Environment in which the model should be generated
#'
#' @return
make_logreg_model <- function(env) {
  mv_model_logreg(
    formula = as.formula(event ~ 0.1 * x + 0.3 * z + 0.8, env = env)
    )
}
