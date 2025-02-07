source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))
source(test_path("fixtures", "concat-formulas.R"))

# common model errors -----------------------------------------------------
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

# cox model ---------------------------------------------------------------
test_that("Returns an error if `.imp` is not part of the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment())
  data_no_imp <- data |> select(-.imp)

  expect_error(model |> calculate_predictions(data_no_imp), "The variable `.imp`")
})

test_that("Returns an error if `id` is not part of the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment())
  data_no_id <- data |> select(-id)

  expect_error(model |> calculate_predictions(data_no_id), "The variable `id`")
})

test_that("Returns an error if not all the model variables are present in data", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment())

  model$formula <- concat_formulas(model$formula, "0.875 * m")
  expect_error(model |> calculate_predictions(data), "The model variable `m` must be present in `data`")
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

# logreg model ------------------------------------------------------------
test_that("Returns an error if `.imp` is not part of the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment())
  data_no_imp <- data %>% select(-.imp)

  expect_error(model |> calculate_predictions(data_no_imp), "The variable `.imp`")
})

test_that("Returns an error if `id` is not part of the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment())
  data_no_id <- data %>% select(-id)

  expect_error(model |> calculate_predictions(data_no_id), "The variable `id`")
})

test_that("Returns an error if model formula variables are not in the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment())

  model$formula <- concat_formulas(model$formula, "0.875 * m")

  expect_error(model |> calculate_predictions(data), "The model variable `m` must be present in `data`")
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
