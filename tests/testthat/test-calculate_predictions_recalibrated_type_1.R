source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

test_that("Checks the model parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))

  expect_error(calculate_predictions_recalibrated_type_1())
  expect_error(calculate_predictions_recalibrated_type_1(data))
})

# Cox model
test_that("Checks the data argument in cox model", {
  model <- make_cox_model(environment())
  data <- readRDS(test_path("fixtures", "mice_data.rds"))

  expect_error(model |> calculate_predictions(data) |> calculate_predictions_recalibrated_type_1())
  expect_error(model |> calculate_predictions(data) |> calculate_predictions_recalibrated_type_1(3))
})

test_that("Returns an error if `.imp` is not part of the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)
  data_no_imp <- data |> select(-.imp)

  expect_error(model |> calculate_predictions_recalibrated_type_1(data_no_imp), "The variable `.imp`")
})

test_that("Returns an error if `id` is not part of the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)
  data_no_id <- data |> select(-id)

  expect_error(model |> calculate_predictions_recalibrated_type_1(data_no_id), "The variable `id`")
})


test_that("Returns an error if `predictions_data` does not exist in cox `model`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)

  model_cox_no_predictions_data <- model_cox
  model_cox_no_predictions_data$predictions_data <- NULL
  expect_error(calculate_predictions_recalibrated_type_1(model_cox_no_predictions_data, data), "In `model` there should be the argument `predictions_data` <tibble> calculated")
})

test_that("Returns an error if the dependent variable in the cox model formula does not exist in `data` or is not a survival class", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)

  model_cox_bad_dependent_variable <- model_cox
  model_cox_bad_dependent_variable$formula <- y ~ x + z
  expect_error(calculate_predictions_recalibrated_type_1(model_cox_bad_dependent_variable, data), "The dependent variable `y` must be <Surv>")
  model_cox_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(calculate_predictions_recalibrated_type_1(model_cox_bad_dependent_variable, data), "The dependent variable `no_exists` must be part of `data`")
})

test_that("Returns an error if `S0` does not exist or it is bad defined in the cox model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)

  model_cox_bad_s0 <- model_cox
  model_cox_bad_s0$S0 <- NULL
  expect_error(calculate_predictions_recalibrated_type_1(model_cox_bad_s0, data), "`S0` must be <numeric>")
  model_cox_bad_s0$S0 <- "a"
  expect_error(calculate_predictions_recalibrated_type_1(model_cox_bad_s0, data), "`S0` must be <numeric>")
})

test_that("Calculates the type 1 recalibrated predictions properly for cox model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data)

  expect_identical(model$predictions_recal_type_1, readRDS(test_path("fixtures", "cox", "predictions_recal_type_1_cox.rds")))
  expect_identical(model$alpha, readRDS(test_path("fixtures", "cox", "alpha_cox.rds")))
})

# Logreg model
test_that("Checks the data argument in logreg model", {
  model <- make_logreg_model(environment())
  data <- readRDS(test_path("fixtures", "mice_data.rds"))

  expect_error(model |> calculate_predictions(data) |> calculate_predictions_recalibrated_type_1())
  expect_error(model |> calculate_predictions(data) |> calculate_predictions_recalibrated_type_1(3))
})

test_that("Returns an error if `.imp` is not part of the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |> calculate_predictions(data)
  data_no_imp <- data |> select(-.imp)

  expect_error(model |> calculate_predictions_recalibrated_type_1(data_no_imp), "The variable `.imp`")
})

test_that("Returns an error if `id` is not part of the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |> calculate_predictions(data)
  data_no_id <- data |> select(-id)

  expect_error(model |> calculate_predictions_recalibrated_type_1(data_no_id), "The variable `id`")
})

test_that("Returns an error if `predictions_data` does not exist in logreg `model`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_logreg_no_predictions_data <- model_logreg
  model_logreg_no_predictions_data$predictions_data <- NULL
  expect_error(calculate_predictions_recalibrated_type_1(model_logreg_no_predictions_data, data), "In `model` there should be the argument `predictions_data` <tibble> calculated")
})

test_that("Returns an error if the dependent variable in the logreg model formula does not exist in `data` or is not a survival class", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_logreg_bad_dependent_variable <- model_logreg
  model_logreg_bad_dependent_variable$formula <- y ~ x + z
  expect_error(calculate_predictions_recalibrated_type_1(model_logreg_bad_dependent_variable, data), "The dependent variable `y` must be <Surv>")
  model_logreg_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(calculate_predictions_recalibrated_type_1(model_logreg_bad_dependent_variable, data), "The dependent variable `no_exists` must be part of `data`")
})

test_that("Calculates the type 1 recalibrated predictions properly for logreg model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data)

  expect_identical(
    sapply(model$predictions_recal_type_1, round, digits = 7),
    sapply(
      readRDS(test_path("fixtures", "logreg", "predictions_recal_type_1_logreg.rds")),
      round,
      digits = 7
    )
  )
  expect_identical(
    round(model$alpha_type_1, 7),
    round(readRDS(test_path("fixtures", "logreg", "alpha_type_1_logreg.rds")), 7)
  )
})
