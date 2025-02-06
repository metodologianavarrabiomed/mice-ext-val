source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

# General tests ----------------------------------------------------------------
test_that("Checks the model parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))

  expect_error(calculate_predictions_recalibrated_type_2())
  expect_error(calculate_predictions_recalibrated_type_2(data))
})

# Cox model --------------------------------------------------------------------
test_that("Checks the data argument in cox model", {
  model <- make_cox_model(environment())
  data <- readRDS(test_path("fixtures", "mice_data.rds"))

  expect_error(model |> calculate_predictions(data) |> calculate_predictions_recalibrated_type_2())
  expect_error(model |> calculate_predictions(data) |> calculate_predictions_recalibrated_type_2(3))
})

test_that("Returns an error if `.imp` is not part of the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)
  data_no_imp <- data |> select(-.imp)

  expect_error(model |> calculate_predictions_recalibrated_type_2(data_no_imp), "must contain `.imp`")
})

test_that("Returns an error if `id` is not part of the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)
  data_no_id <- data |> select(-id)

  expect_error(model |> calculate_predictions_recalibrated_type_2(data_no_id), "must contain `id`")
})

test_that("Returns an error if `predictions_data` does not exist in cox `model`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)

  model_cox_no_predictions_data <- model_cox
  model_cox_no_predictions_data$predictions_data <- NULL
  expect_error(calculate_predictions_recalibrated_type_2(model_cox_no_predictions_data, data), "`model` must have `predictions_data` calculated")
})

test_that("Returns an error if the dependent variable in the cox model formula does not exist in `data` or is not a survival class", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)

  model_cox_bad_dependent_variable <- model_cox
  model_cox_bad_dependent_variable$formula <- y ~ x + z
  expect_error(calculate_predictions_recalibrated_type_2(model_cox_bad_dependent_variable, data), "the dependent variable must be of class `Surv`")
  model_cox_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(calculate_predictions_recalibrated_type_2(model_cox_bad_dependent_variable, data), "the dependent variable must be part of `data`")
})

test_that("Returns an error if `S0t` does not exist or it is bad defined in the cox model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)

  model_cox_bad_s0 <- model_cox
  model_cox_bad_s0$S0 <- NULL
  expect_error(calculate_predictions_recalibrated_type_2(model_cox_bad_s0, data), "`S0` must be a `numeric`")
  model_cox_bad_s0$S0 <- "a"
  expect_error(calculate_predictions_recalibrated_type_2(model_cox_bad_s0, data), "`S0` must be a `numeric`")
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

# Logreg model -----------------------------------------------------------------
test_that("Checks the data argument in logreg model", {
  model <- make_logreg_model(environment())
  data <- readRDS(test_path("fixtures", "mice_data.rds"))

  expect_error(model |> calculate_predictions(data) |> calculate_predictions_recalibrated_type_2())
  expect_error(model |> calculate_predictions(data) |> calculate_predictions_recalibrated_type_2(3))
})

test_that("Returns an error if `.imp` is not part of the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |> calculate_predictions(data)
  data_no_imp <- data |> select(-.imp)

  expect_error(model |> calculate_predictions_recalibrated_type_2(data_no_imp), "must contain `.imp`")
})

test_that("Returns an error if `id` is not part of the `data` parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |> calculate_predictions(data)
  data_no_id <- data |> select(-id)

  expect_error(model |> calculate_predictions_recalibrated_type_2(data_no_id), "must contain `id`")
})

test_that("Returns an error if `predictions_data` does not exist in logreg `model`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_logreg_no_predictions_data <- model_logreg
  model_logreg_no_predictions_data$predictions_data <- NULL
  expect_error(calculate_predictions_recalibrated_type_2(model_logreg_no_predictions_data, data), "`model` must have `predictions_data` calculated")
})

test_that("Returns an error if the dependent variable in the logreg model formula does not exist in `data` or is not a survival class", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_logreg_bad_dependent_variable <- model_logreg
  model_logreg_bad_dependent_variable$formula <- y ~ x + z
  expect_error(calculate_predictions_recalibrated_type_2(model_logreg_bad_dependent_variable, data), "the dependent variable must be of class `Surv`")
  model_logreg_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(calculate_predictions_recalibrated_type_2(model_logreg_bad_dependent_variable, data), "the dependent variable must be part of `data`")
})

test_that("Returns an error if `intercept` does not exist or it is bad defined in the logreg model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_logreg_bad_intercept <- model_logreg
  model_logreg_bad_intercept$intercept <- NULL
  expect_error(calculate_predictions_recalibrated_type_2(model_logreg_bad_intercept, data), "`intercept` must be a `numeric`")
  model_logreg_bad_intercept$intercept <- "a"
  expect_error(calculate_predictions_recalibrated_type_2(model_logreg_bad_intercept, data), "`intercept` must be a `numeric`")
})

test_that("Calculates the type 2 recalibrated predictions properly for logreg model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)

  expect_identical(
    sapply(model$predictions_recal_type_2, round, digits = 7),
    sapply(
      readRDS(test_path("fixtures", "logreg", "predictions_recal_type_2_logreg.rds")),
      round,
      digits = 7
    )
  )
  expect_identical(
    round(model$alpha_type_2, 7),
    round(readRDS(test_path("fixtures", "logreg", "alpha_type_2_logreg.rds")), 7)
  )
  expect_identical(
    round(model$beta_overall, 7), round(readRDS(test_path("fixtures", "logreg", "beta_overall_logreg.rds")), 7)
  )
})
