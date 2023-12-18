source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

test_that("Checks the model parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))

  expect_error(calculate_predictions_recalibrated_type_2())
  expect_error(calculate_predictions_recalibrated_type_2(data))
})

test_that("Checks the data argument in cox model", {
  model <- make_cox_model(environment())
  data <- readRDS(test_path("fixtures", "mice_data.rds"))

  expect_error(model |> calculate_predictions(data) |> calculate_predictions_recalibrated_type_2())
  expect_error(model |> calculate_predictions(data) |> calculate_predictions_recalibrated_type_2(3))
})

test_that("Checks the data argument in logreg model", {
  model <- make_logreg_model(environment())
  data <- readRDS(test_path("fixtures", "mice_data.rds"))

  expect_error(model |> calculate_predictions(data) |> calculate_predictions_recalibrated_type_2())
  expect_error(model |> calculate_predictions(data) |> calculate_predictions_recalibrated_type_2(3))
})

test_that("Calculates the type 2 recalibrated predictions properly for cox model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)

  expect_identical(model$predictions_recal_type_2, readRDS(test_path("fixtures", "cox", "predictions_recal_type_2_cox.rds")))
  expect_identical(model$S0_type_2, readRDS(test_path("fixtures", "cox", "S0_type_2_cox.rds")))
  expect_identical(model$beta_overall, readRDS(test_path("fixtures", "cox", "beta_overall_cox.rds")))
})

test_that("Calculates the type 2 recalibrated predictions properly for logreg model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)

  expect_identical(model$predictions_recal_type_2, readRDS(test_path("fixtures", "logreg", "predictions_recal_type_2_logreg.rds")))
  expect_identical(model$alpha_type_2, readRDS(test_path("fixtures", "logreg", "alpha_type_2_logreg.rds")))
  expect_identical(model$beta_overall, readRDS(test_path("fixtures", "logreg", "beta_overall_logreg.rds")))
})

