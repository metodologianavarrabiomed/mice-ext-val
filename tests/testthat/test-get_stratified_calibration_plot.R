source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

test_that("generates an stratified calibration plot", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)


  testthat::expect_s3_class(
    get_stratified_calibration_plot(data, n_groups = 10, Cox = model_cox, LogReg = model_logreg),
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
    get_stratified_calibration_plot(data, n_groups = 10, type = "predictions_recal_type_1", Cox = model_cox, LogReg = model_logreg),
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
    get_stratified_calibration_plot(data, n_groups = 10, type = "predictions_recal_type_2", Cox = model_cox, LogReg = model_logreg),
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
    get_stratified_calibration_plot(data = data, n_groups = 10, model_cox, 5),
    "must be <MiceExtVal>"
  )
  testthat::expect_error(
    get_stratified_calibration_plot(data = data, n_groups = 10, model_logreg, 5),
    "must be <MiceExtVal>"
  )
})
