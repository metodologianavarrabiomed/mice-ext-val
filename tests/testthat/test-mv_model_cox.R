test_that("Checks it has coefficients", {
  expect_error(mv_model_cox())
})

test_that("Checks coefficients properly", {
  expect_error(mv_model_cox(4, list(x = 1, z = 2), y ~ x + z, 0.8))
})

test_that("Checks mean properly", {
  expect_error(mv_model_cox(list(x = 0.08, z = 0.15), 3, y ~ x + z, 0.8))
})

test_that("Checks formula properly", {
  expect_error(mv_model_cox(list(x = 0.08, z = 0.15), list(x = 1, z = 2), 2, 0.8))
})

test_that("Checks S0 properly", {
  expect_error(mv_model_cox(list(x = 0.08, z = 0.15), list(x = 1, z = 2), y ~ x + z, "a"))
})

test_that("Model is generated properly MiceExtVal class", {
  model <- make_cox_model(environment())
  expect_s3_class(model, "MiceExtVal")
})

test_that("Model is generated properly cox class", {
  model <- make_cox_model(environment())
  expect_s3_class(model, "cox")
})

test_that("Coefficients are assigned properly", {
  model <- make_cox_model(environment())
  expect_identical(model$coefficients, list(x = 0.1, z = 0.3))
})

test_that("Means are assigned properly", {
  model <- make_cox_model(environment())
  expect_identical(model$means, list(x = 1, z = 2))
})

test_that("Formula are assigned properly", {
  model <- make_cox_model(environment())
  expect_identical(as.character(model$formula), as.character(event ~ x + z))
})

test_that("All the parameters are assigned", {
  model <- make_cox_model(environment())
  expect_named(model, c("coefficients", "means", "formula",  "S0", "alpha", "S0_type_2", "beta_overall", "predictions_aggregated", "predictions_data", "betax", "betax_data", "predictions_recal_type_1", "predictions_recal_type_2", "c_index"))
})
