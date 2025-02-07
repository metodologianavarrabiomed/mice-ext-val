source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

test_that("Checks the formula", {
  expect_error(mv_model_logreg(2))
})

test_that("Model is generated properly MiceExtVal class", {
  expect_s3_class(make_logreg_model(environment()), "MiceExtVal")
})

test_that("Model is generated properly logreg class", {
  expect_s3_class(make_logreg_model(environment()), "logreg")
})

test_that("Formula is assigned properly", {
  model <- make_logreg_model(environment())
  expect_identical(as.character(model$formula), as.character(event ~ 0.1 * x + 0.3 * z + 0.8))
})

test_that("All the parameters are assigned", {
  model <- make_logreg_model(environment())
  expect_named(model, c("formula", "alpha_type_1", "alpha_type_2", "beta_overall", "predictions_aggregated", "predictions_data", "betax", "betax_data", "predictions_recal_type_1", "predictions_recal_type_2", "c_index"))
})
