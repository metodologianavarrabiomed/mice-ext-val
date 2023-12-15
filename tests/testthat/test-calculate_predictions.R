test_that("Checks the model parameter", {
  expect_error(calculate_predictions())
  expect_error(calculate_predictions(5))
})

test_that("Checks the data argument in cox model", {
  model <- make_cox_model(environment())

  expect_error(model |> calculate_predictions())
  expect_error(model |> calculate_predictions(3))
})

test_that("Checks the data argument in logreg model", {
  model <- make_logreg_model(environment())

  expect_error(model |> calculate_predictions())
  expect_error(model |> calculate_predictions(3))
})

test_that("Calculates the predictions properly in Cox model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))

  model <- make_cox_model(environment()) |>
    calculate_predictions(data)

  # Predictions works
  expect_identical(model$predictions_aggregated, readRDS(test_path("fixtures", "cox", "predictions_aggregated_cox.rds")))
  expect_identical(model$predictions_data, readRDS(test_path("fixtures", "cox", "predictions_data_cox.rds")))
  expect_identical(model$betax, readRDS(test_path("fixtures", "cox", "betax_cox.rds")))
  expect_identical(model$betax_data, readRDS(test_path("fixtures", "cox", "betax_data_cox.rds")))
})

test_that("Calculates the predictions properly in logreg model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))

  model <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  # Predictions works
  expect_identical(model$predictions_aggregated, readRDS(test_path("fixtures", "logreg", "predictions_aggregated_logreg.rds")))
  expect_identical(model$predictions_data, readRDS(test_path("fixtures", "logreg", "predictions_data_logreg.rds")))
  expect_identical(model$betax, readRDS(test_path("fixtures", "logreg", "betax_logreg.rds")))
  expect_identical(model$betax_data, readRDS(test_path("fixtures", "logreg", "betax_data_logreg.rds")))
})
