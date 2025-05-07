source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

test_that("generates an stratified calibration plot", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)


  testthat::expect_s3_class(
    get_stratified_calibration_plot_prop(data, n_groups = 10, type = "predictions_aggregated", Cox = model_cox, LogReg = model_logreg),
    "ggplot"
  )
})

test_that("generates an stratified calibration plot with recalibration type 1", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data)


  testthat::expect_s3_class(
    get_stratified_calibration_plot_prop(data, n_groups = 10, type = "predictions_recal_type_1", Cox = model_cox, LogReg = model_logreg),
    "ggplot"
  )
})

test_that("generates an stratified calibration plot with recalibration type 2", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)


  testthat::expect_s3_class(
    get_stratified_calibration_plot_prop(data, n_groups = 10, type = "predictions_recal_type_2", Cox = model_cox, LogReg = model_logreg),
    "ggplot"
  )
})

test_that("returns an error if some model is not of class <MiceExtVal>", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  testthat::expect_error(
    get_stratified_calibration_plot_prop(data = data, n_groups = 10, type = "predictions_aggregated", model_cox, 5),
    "must be <MiceExtVal>"
  )
  testthat::expect_error(
    get_stratified_calibration_plot_prop(data = data, n_groups = 10, type = "predictions_aggregated", model_logreg, 5),
    "must be <MiceExtVal>"
  )
})

test_that("returns an error if not all the models have the predictions calculated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment())

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)
  testthat::expect_error(
    get_stratified_calibration_plot_prop(data = data, n_groups = 10, type = "predictions_aggregated", model_cox, model_logreg),
    "must contain `predictions_aggregated` consider using the function"
  )
})

test_that("returns an error if not all the models have the predictions recalibrated type 1 calculated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)
  testthat::expect_error(
    get_stratified_calibration_plot_prop(data = data, n_groups = 10, type = "predictions_recal_type_1", model_cox, model_logreg),
    "must contain `predictions_recal_type_1` consider using the function"
  )
})

test_that("returns an error if not all the models have the predictions recalibrated type 2 calculated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)
  testthat::expect_error(
    get_stratified_calibration_plot_prop(data = data, n_groups = 10, type = "predictions_recal_type_2", model_cox, model_logreg),
    "must contain `predictions_recal_type_2` consider using the function"
  )
})

test_that("returns an error if not all the models have a `Surv` dependent variable", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)

  model_logreg <- make_logreg_model(environment())

  data$a <- rep("a", length(data$y))
  model_logreg$formula <- a ~ 0.1 * x + 0.3 * z + 0.8
  model_logreg <- model_logreg |>
    calculate_predictions(data)
  testthat::expect_error(
    get_stratified_calibration_plot_prop(data = data, n_groups = 10, type = "predictions_aggregated", model_cox, model_logreg),
    "The `model_logreg` model must have a dependent variable of class <Surv/numeric> and be dichotomous"
  )
})
