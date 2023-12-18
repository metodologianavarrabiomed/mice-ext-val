source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

test_that("The calibration plot data function checks all the parameters", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data)


  expect_error(get_calibration_plot_data())
  expect_error(get_calibration_plot_data(2, data, 10, 4, "predictions_aggregated"))
  expect_error(get_calibration_plot_data(model, 4, 10, 4, "predictions_aggregated"))
  expect_error(get_calibration_plot_data(model, 4, "a", 4, "predictions_aggregated"))
  expect_error(get_calibration_plot_data(model, data, 10, "a", "predictions_aggregated"))
  expect_error(get_calibration_plot_data(model, data, 10, 4, "test"))
})

test_that("The calibration plot data function works properly with type 'predictions_aggregated' cox", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment())

  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_aggregated"))

  model <- model |> calculate_predictions(data)

  expect_no_error(get_calibration_plot_data(model, data, 2, 5, "predictions_aggregated"))
  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_1"))
  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_2"))
  expect_identical(get_calibration_plot_data(model, data, 2, 5, "predictions_aggregated"), readRDS(test_path("fixtures", "cox", "calibration_plot_data_2_groups_cox.rds")))
})

test_that("The calibration plot data function works properly with type 'predictions_aggregated' logreg", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment())

  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_aggregated"))

  model <- model |> calculate_predictions(data)

  expect_no_error(get_calibration_plot_data(model, data, 2, 5, "predictions_aggregated"))
  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_1"))
  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_2"))
  expect_identical(get_calibration_plot_data(model, data, 2, 5, "predictions_aggregated"), readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_logreg.rds")))
})

test_that("The calibration plot data function works properly with type 'predictions_recal_type_1' cox", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)

  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_1"))

  model <- model |> calculate_predictions_recalibrated_type_1(data)

  expect_no_error(get_calibration_plot_data(model, data, 2, 5, "predictions_aggregated"))
  expect_no_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_1"))
  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_2"))
  expect_identical(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_1"), readRDS(test_path("fixtures", "cox", "calibration_plot_data_2_groups_recal_1_cox.rds")))
})

test_that("The calibration plot data function works properly with type 'predictions_recal_type_1' logreg", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |> calculate_predictions(data)

  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_1"))

  model <- model |> calculate_predictions_recalibrated_type_1(data)

  expect_no_error(get_calibration_plot_data(model, data, 2, 5, "predictions_aggregated"))
  expect_no_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_1"))
  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_2"))
  expect_identical(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_1"), readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_recal_1_logreg.rds")))
})

test_that("The calibration plot data function works properly with type 'predictions_recal_type_2' cox", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)

  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_2"))

  model <- model |> calculate_predictions_recalibrated_type_2(data)

  expect_no_error(get_calibration_plot_data(model, data, 2, 5, "predictions_aggregated"))
  expect_no_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_2"))
  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_1"))
  expect_identical(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_2"), readRDS(test_path("fixtures", "cox", "calibration_plot_data_2_groups_recal_2_cox.rds")))
})

test_that("The calibration plot data function works properly with type 'predictions_recal_type_2' logreg", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |> calculate_predictions(data)

  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_2"))

  model <- model |> calculate_predictions_recalibrated_type_2(data)

  expect_no_error(get_calibration_plot_data(model, data, 2, 5, "predictions_aggregated"))
  expect_no_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_2"))
  expect_error(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_1"))
  expect_identical(get_calibration_plot_data(model, data, 2, 5, "predictions_recal_type_2"), readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_recal_2_logreg.rds")))
})

test_that("The calibration plot data checks the 'data' parameter", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  plot_data <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    get_calibration_plot_data(data, 2, 5, "predictions_aggregated")

  expect_error(get_calibration_plot())
  expect_error(get_calibration_plot(4))
  expect_no_error(get_calibration_plot(plot_data))
})

test_that("The calibration plot function generates a plot for a Cox model and predictions aggregated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data(data, 2, 5, "predictions_aggregated") |>
      get_calibration_plot(),
    "ggplot"
  )
})

test_that("The calibration plot function generates a plot for a Cox model and predictions recalibrated type 1", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data(data, 2, 5, "predictions_recal_type_1") |>
      get_calibration_plot(),
    "ggplot"
  )
})

test_that("The calibration plot function generates a plot for a Cox model and predictions recalibrated type 2", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data(data, 2, 5, "predictions_recal_type_2") |>
      get_calibration_plot(),
    "ggplot"
  )
})

test_that("The calibration plot function generates a plot for a logreg model and predictions aggregated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data(data, 2, 5, "predictions_aggregated") |>
      get_calibration_plot(),
    "ggplot"
  )
})

test_that("The calibration plot function generates a plot for a logreg model and predictions recalibrated type 1", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data(data, 2, 5, "predictions_recal_type_1") |>
      get_calibration_plot(),
    "ggplot"
  )
})

test_that("The calibration plot function generates a plot for a logreg model and predictions recalibrated type 2", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data(data, 2, 5, "predictions_recal_type_2") |>
      get_calibration_plot(),
    "ggplot"
  )
})
