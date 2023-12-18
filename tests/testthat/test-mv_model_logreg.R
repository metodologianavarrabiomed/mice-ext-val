source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

test_that("Checks it has coefficients", {
  expect_error(mv_model_logreg())
})

test_that("Checks the coefficients", {
  expect_error(mv_model_logreg(1, y ~ x + z, 0.91))
})

test_that("Checks the formula", {
  expect_error(mv_model_logreg(list(x = 0.56, z = 0.7), 2, 0.91))
})

test_that("Checks the intercept", {
  expect_error(mv_model_logreg(list(x = 0.56, z = 0.7), y ~ x + z, "a"))
})

test_that("Model is generated properly MiceExtVal class", {
  expect_s3_class(make_logreg_model(environment()), "MiceExtVal")
})

test_that("Model is generated properly logreg class", {
  expect_s3_class(make_logreg_model(environment()), "logreg")
})

test_that("Coefficients are assigned properly", {
  model <- make_logreg_model(environment())
  expect_identical(model$coefficients, list(x = 0.1, z = 0.3))
})

test_that("Formula is assigned properly", {
  model <- make_logreg_model(environment())
  expect_identical(as.character(model$formula), as.character(event ~ x + z))
})

test_that("Intercept is assigned properly", {
  model <- make_logreg_model(environment())
  expect_identical(model$intercept, 0.8)
})

test_that("All the parameters are assigned", {
  model <- make_logreg_model(environment())
  expect_named(model, c("coefficients", "formula", "intercept", "alpha_type_1", "alpha_type_2", "beta_overall", "predictions_aggregated", "predictions_data", "betax", "betax_data", "predictions_recal_type_1", "predictions_recal_type_2", "c_index"))
})
