source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))
source(test_path("fixtures", "round-to-precision.R"))

test_that("Checks the parameters properly", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  # Error with no parameters
  expect_error(calculate_harrell_c_index())

  # Error if model is not a MiceExtVal model
  expect_error(calculate_harrell_c_index("test", data))

  # Error if model is ok and no data
  expect_error(calculate_harrell_c_index(make_cox_model(environment())))
  expect_error(calculate_harrell_c_index(make_cox_model(environment()), "c"))

  # No error with cox model
  expect_no_error(calculate_harrell_c_index(model_cox, data))

  # No error with logreg model
  expect_no_error(calculate_harrell_c_index(model_logreg, data))
})

test_that("Returns an error if `.imp` does not exist in `data`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  data_no_imp <- data |> dplyr::select(-.imp)
  expect_error(calculate_harrell_c_index(model_cox, data_no_imp), "must contain `.imp`")
  expect_error(calculate_harrell_c_index(model_logreg, data_no_imp), "must contain `.imp`")
})

test_that("Returns an error if `formula` does not exist in `model`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_cox_no_formula <- model_cox
  model_cox_no_formula$formula <- NULL
  expect_error(calculate_harrell_c_index(model_cox_no_formula, data), "`model` must contain a valid `formula`")

  model_logreg_no_formula <- model_logreg
  model_logreg_no_formula$formula <- NULL
  expect_error(calculate_harrell_c_index(model_logreg_no_formula, data), "`model` must contain a valid `formula`")
})

test_that("Returns an error if `predictions_data` does not exist in `model`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_cox_no_predictions_data <- model_cox
  model_cox_no_predictions_data$predictions_data <- NULL
  expect_error(calculate_harrell_c_index(model_cox_no_predictions_data, data), "`model` must contain `predictions_data` calculate it")

  model_logreg_no_predictions_data <- model_logreg
  model_logreg_no_predictions_data$predictions_data <- NULL
  expect_error(calculate_harrell_c_index(model_logreg_no_predictions_data, data), "`model` must contain `predictions_data` calculate it")
})

test_that("Returns an error if the dependent variable in the model formula does not exist in `data` or is not a survival class", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_cox_bad_dependent_variable <- model_cox
  model_cox_bad_dependent_variable$formula <- y ~ x + z
  expect_error(calculate_harrell_c_index(model_cox_bad_dependent_variable, data), "the dependent variable `y` must be <Surv>")
  model_cox_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(calculate_harrell_c_index(model_cox_bad_dependent_variable, data), "the dependent variable `no_exists` must be part of `data`")

  model_logreg_bad_dependent_variable <- model_logreg
  model_logreg_bad_dependent_variable$formula <- y ~ x + z
  expect_error(calculate_harrell_c_index(model_logreg_bad_dependent_variable, data), "the dependent variable `y` must be <Surv>")
  model_logreg_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(calculate_harrell_c_index(model_logreg_bad_dependent_variable, data), "the dependent variable `no_exists` must be part of `data`")
})

test_that("Calculates the c-index properly for a cox model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  expect_identical(
    round_to_precision(model_cox$c_index),
    round_to_precision(readRDS(test_path("fixtures", "cox", "c_index_cox.rds")))
  )
})

test_that("Calculates the c-index properly for a logreg model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  expect_identical(
    round_to_precision(model_logreg$c_index),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "c_index_logreg.rds")))
  )
})
