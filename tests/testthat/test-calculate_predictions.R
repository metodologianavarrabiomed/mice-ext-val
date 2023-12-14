# What to test?
#   - [ ] Parameter checking
#   - [ ] Results generation
source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))


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

  expect_error(model |> calculate_predictions(3))
})

test_that("Calculates the predictions properly in Cox model", {
  model <- make_cox_model(environment())
  data <- readRDS(test_path("fixtures", "mice_data.rds"))

  model <- model |>
    calculate_predictions(data)

  # Predictions works
  predictions_aggregated <- readRDS(test_path("fixtures", "cox", "predictions_aggregated_cox.rds"))
  expect_identical(model$predictions_aggregated, predictions_aggregated)
  predictions_data <- readRDS(test_path("fixtures", "cox", "predictions_data_cox.rds"))
  expect_identical(model$predictions_data, predictions_data)
  betax <- readRDS(test_path("fixtures", "cox", "betax_cox.rds"))
  expect_identical(model$betax, betax)
  betax_data <- readRDS(test_path("fixtures", "cox", "betax_data_cox.rds"))
  expect_identical(model$betax_data, betax_data)
})
