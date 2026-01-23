source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))


test_that("Checks formula properly", {
  expect_error(mv_model_cox(2, 0.8))
})

test_that("Checks S0 properly", {
  expect_error(mv_model_cox(y ~ x + z, "a"))
})

test_that("Model is generated properly MiceExtVal class", {
  model <- make_cox_model(environment())
  expect_s3_class(model, "MiceExtVal")
})

test_that("Model is generated properly cox class", {
  model <- make_cox_model(environment())
  expect_s3_class(model, "cox")
})

test_that("Formula are assigned properly", {
  model <- make_cox_model(environment())
  expect_identical(as.character(model$formula), as.character(event ~ 0.1 * (x - 1) + 0.3 * (z - 2)))
})

test_that("All the parameters are assigned", {
  model <- make_cox_model(environment())
  expect_named(model, c("formula", "S0", "predictions_imp", "predictions_agg", "recal_parameters", "results_imp", "results_agg"))
})
