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
#' @importFrom dplyr %>% group_by group_map filter select
#' @importFrom tibble tibble
#' @importFrom rms lrm.fit
#' @importFrom progress progress_bar
#' @importFrom methods is
#'
#' @exportS3Method calculate_predictions_recalibrated_type_1 logreg
#'
#' @examples
#' set.seed(123)
#'
#' model <- mv_model_logreg(
#'   coefficients = list(x = 0.5, z = 0.3),
#'   formula = event ~ x + z,
#'   intercept = 1.2
#' )
#'
#' data <- data.frame(
#'   .imp = c(1,1,1,2,2,2,3,3,3),
#'   id = c(1,2,3,1,2,3,1,2,3),
#'   x = rnorm(9, 1, 0.25),
#'   z = rnorm(9, 2, 0.75),
#'   status = c(1,0,0,1,0,0,1,0,0),
#'   time = c(2,3,5,2,3,5,2,3,5)
#' )
#' data$event <- survival::Surv(data$time, data$status)
#'
#' model |>
#'   calculate_predictions(data) |>
#'   calculate_predictions_recalibrated_type_1(data)
calculate_predictions_recalibrated_type_1.logreg <- function(model, data, .progress = FALSE) {
  # Checks pre-conditions
  stopifnot(methods::is(model, "MiceExtVal"))
  stopifnot(methods::is(data, "data.frame"))

  # Returns an error if `.imp` is not part of the `data` parameter
  if (!".imp" %in% colnames(data)) {
    stop("`data` variable must contain `.imp`")
    return()
  }

  # Returns an error if `id` is not part of the `data` parameter
  if (!"id" %in% colnames(data)) {
    stop("`data` variable must contain `id`")
    return()
  }

  # Returns an error if `predictions_data` does not exist in `model`
  if (!"predictions_data" %in% names(model) | !methods::is(model$predictions_data, "data.frame")) {
    stop("`model` must have `predictions_data` calculated")
    return()
  }

  # Returns an error if the dependent variable in the model formula does not exist
  # in `data` or is not a survival class
  dependent_variable <- all.vars(model$formula)[1]
  if (!dependent_variable %in% colnames(data)) {
    stop("the dependent variable must be part of `data`")
    return()
  }
  if (!methods::is(data[[dependent_variable]], "Surv")) {
    stop("the dependent variable must be of class `Surv`")
    return()
  }

  # Returns an error if `intercept` does not exist or it is bad defined
  if (is.null(model$intercept) | !is.numeric(model$intercept)) {
    stop("`intercept` must be a `numeric`")
    return()
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

  model$alpha_type_1 <- data %>%
    dplyr::group_by(.imp) %>%
    dplyr::group_map(~ {
      # Progress bar code
      if (.progress) {
        pb$tick()
      }

      # Obtains the data of the event variable
      survival_data <- .x[[all.vars(model$formula)[1]]]
      betax <- model$betax_data %>%
        dplyr::filter(.imp == .y$.imp) %>%
        dplyr::select(betax) %>%
        unlist()

      # Calculates the `alpha` parameter value
      model_recal <- rms::lrm.fit(
        y = survival_data[, "status"],
        offset = betax
      )
      alpha <- model_recal$coefficients
    }) %>%
    do.call(rbind, args = .) %>%
    mean()

  # Calculates the type 1 recalibration
  model$predictions_recal_type_1 <- tibble::tibble(
    id = model$betax$id,
    prediction_type_1 = 1 / (1 + exp(-(model$betax$betax + model$alpha_type_1)))
  )

  pb$tick()

  return(model)
}
