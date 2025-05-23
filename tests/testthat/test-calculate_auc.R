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
  expect_error(calculate_auc())

  # Error if model is not a MiceExtVal model
  expect_error(calculate_auc("test", data))

  # Error if model is ok and no data
  expect_error(calculate_auc(make_cox_model(environment())))
  expect_error(calculate_auc(make_cox_model(environment()), "c"))

  # No error with cox model
  expect_no_error(calculate_auc(model_cox, data))

  # No error with logreg model
  expect_no_error(calculate_auc(model_logreg, data))
})

test_that("Returns an error if `.imp` does not exist in `data`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  data_no_imp <- data |> dplyr::select(-.imp)
  expect_error(calculate_auc(model_cox, data_no_imp), "The variable `.imp`")
  expect_error(calculate_auc(model_logreg, data_no_imp), "The variable `.imp`")
})

test_that("Returns an error if `formula` does not exist in `model`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_cox_no_formula <- model_cox
  model_cox_no_formula$formula <- NULL
  expect_error(calculate_auc(model_cox_no_formula, data), "`model` must contain a valid `formula`")

  model_logreg_no_formula <- model_logreg
  model_logreg_no_formula$formula <- NULL
  expect_error(calculate_auc(model_logreg_no_formula, data), "`model` must contain a valid `formula`")
})

test_that("Returns an error if `predictions_data` does not exist in `model`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_cox_no_predictions_data <- model_cox
  model_cox_no_predictions_data$predictions_data <- NULL
  expect_error(calculate_auc(model_cox_no_predictions_data, data), "`model` must contain `predictions_data` calculate it")

  model_logreg_no_predictions_data <- model_logreg
  model_logreg_no_predictions_data$predictions_data <- NULL
  expect_error(calculate_auc(model_logreg_no_predictions_data, data), "`model` must contain `predictions_data` calculate it")
})

test_that("Returns an error if `predictions_data` does not exist in `model`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_cox_no_predictions_data <- model_cox
  model_cox_no_predictions_data$predictions_data <- NULL
  expect_error(calculate_auc(model_cox_no_predictions_data, data), "`model` must contain `predictions_data` calculate it")

  model_logreg_no_predictions_data <- model_logreg
  model_logreg_no_predictions_data$predictions_data <- NULL
  expect_error(calculate_auc(model_logreg_no_predictions_data, data), "`model` must contain `predictions_data` calculate it")
})

test_that("Returns an error if the dependent variable in the model formula does not exist in `data` or is not a survival class", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_cox_bad_dependent_variable <- model_cox
  model_cox_bad_dependent_variable$formula <- y ~ x + z
  expect_error(calculate_auc(model_cox_bad_dependent_variable, data), "The dependent variable `y` must be <Surv>")
  model_cox_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(calculate_auc(model_cox_bad_dependent_variable, data), "the dependent variable `no_exists` must be part of `data`")

  model_logreg_bad_dependent_variable <- model_logreg
  data$a <- rep("a", length(data$y))
  model_logreg_bad_dependent_variable$formula <- a ~ x + z
  expect_error(calculate_auc(model_logreg_bad_dependent_variable, data), "The dependent variable `a` must be <Surv/numeric>")
  model_logreg_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(calculate_auc(model_logreg_bad_dependent_variable, data), "the dependent variable `no_exists` must be part of `data`")
})

test_that("Calculates the c-index properly for a cox model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_auc(data)

  expect_identical(
    round_to_precision(model_cox$auc),
    round_to_precision(readRDS(test_path("fixtures", "cox", "auc_cox.rds")))
  )
})

test_that("Calculates the c-index properly for a logreg model with `Surv` dependent variable", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_auc(data)

  expect_identical(
    round_to_precision(model_logreg$auc),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "auc_logreg.rds")))
  )
})

test_that("Calculates the c-index properly for a logreg model with `numeric` dependent variable", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment())

  model_logreg$formula <- y ~ 0.1 * x + 0.3 * z + 0.8

  model_logreg <- model_logreg |>
    calculate_predictions(data) |>
    calculate_auc(data)

  expect_identical(
    round_to_precision(model_logreg$auc),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "auc_logreg.rds")))
  )
})
