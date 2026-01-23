source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))
source(test_path("fixtures", "round-to-precision.R"))

# errors in function arguments --------------------------------------------
testthat::test_that("fails when the model is not of class `MiceExtVal`", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  testthat::expect_error(calculate_brier_score(model = "a", data = data, type = "predictions_data"), "`model` must be <MiceExtVal>")
})

testthat::test_that("fails when the data argument is not a data.frame", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |>
    calculate_predictions(data)

  testthat::expect_error(calculate_brier_score(model = cox_model, data = 1, type = "predictions_data"), "`data` must be <data.frame>")
})

testthat::test_that("fails when the type argument is wrong selected", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |>
    calculate_predictions(data)

  testthat::expect_error(calculate_brier_score(model = cox_model, data = data, type = "a"), "`type` must be one of the following types:")
})

testthat::test_that("fails when the type argument is not calculated in the model", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |>
    calculate_predictions(data)

  testthat::expect_error(calculate_brier_score(model = cox_model, data = data, type = "prediction_type_1"), "It seems that `type` is not yet calculated, calculate it using")
})

# cox model ---------------------------------------------------------------
testthat::test_that("the brier score is calculated properly for a cox model", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |>
    calculate_predictions(data)

  testthat::expect_no_error(calculate_brier_score(model = cox_model, data = data, type = "prediction", n_boot = 10))
  cox_model <- calculate_brier_score(model = cox_model, data = data, type = "prediction", n_boot = 10, seed = 123)
  testthat::expect_identical(
    round_to_precision(cox_model$results_agg |> dplyr::filter(name == "brier_score")),
    round_to_precision(readRDS(test_path("fixtures", "cox", "brier_score_cox.rds")))
  )
})

testthat::test_that("the brier score is calculated properly for a cox model and recalibrated predictions", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_predictions_recalibrated_type_2(data)

  testthat::expect_no_error(calculate_brier_score(model = cox_model, data = data, type = "prediction_type_1", n_boot = 10))
  cox_model <- calculate_brier_score(model = cox_model, data = data, type = "prediction_type_1", n_boot = 10, seed = 123)
  testthat::expect_identical(
    round_to_precision(cox_model$results_agg |> dplyr::filter(name == "brier_score_type_1")),
    round_to_precision(readRDS(test_path("fixtures", "cox", "brier_score_type_1_cox.rds")))
  )
  testthat::expect_no_error(calculate_brier_score(model = cox_model, data = data, type = "prediction_type_2", n_boot = 10))
  cox_model <- calculate_brier_score(model = cox_model, data = data, type = "prediction_type_2", n_boot = 10, seed = 123)
  testthat::expect_identical(
    round_to_precision(cox_model$results_agg |> dplyr::filter(name == "brier_score_type_2")),
    round_to_precision(readRDS(test_path("fixtures", "cox", "brier_score_type_2_cox.rds")))
  )
})

# logreg model survival outcome -------------------------------------------
testthat::test_that("the brier score is calculated properly for a logreg model with survival outcome", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  logreg_model <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  testthat::expect_no_error(calculate_brier_score(model = logreg_model, data = data, type = "prediction", n_boot = 10))
  logreg_model <- calculate_brier_score(model = logreg_model, data = data, type = "prediction", n_boot = 10, seed = 123)
  testthat::expect_identical(
    round_to_precision(logreg_model$results_agg |> dplyr::filter(name == "brier_score")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "brier_score_logreg.rds")))
  )
})

testthat::test_that("the brier score is calculated properly for a logreg model with survival outcome and recalibrated predictions", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  logreg_model <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_predictions_recalibrated_type_2(data)

  testthat::expect_no_error(calculate_brier_score(model = logreg_model, data = data, type = "prediction_type_1", n_boot = 10))
  logreg_model <- calculate_brier_score(model = logreg_model, data = data, type = "prediction_type_1", n_boot = 10, seed = 123)
  testthat::expect_identical(
    round_to_precision(logreg_model$results_agg |> dplyr::filter(name == "brier_score_type_1")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "brier_score_type_1_logreg.rds")))
  )
  testthat::expect_no_error(calculate_brier_score(model = logreg_model, data = data, type = "prediction_type_2", n_boot = 10))
  logreg_model <- calculate_brier_score(model = logreg_model, data = data, type = "prediction_type_2", n_boot = 10, seed = 123)
  testthat::expect_identical(
    round_to_precision(logreg_model$results_agg |> dplyr::filter(name == "brier_score_type_2")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "brier_score_type_2_logreg.rds")))
  )
})

# logreg model dichotomous outcome ----------------------------------------
testthat::test_that("the brier score is calculated properly for a logreg model with dichotomous outcome", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  logreg_model <- make_logreg_model(environment()) |>
    calculate_predictions(data)
  logreg_model$formula <- y ~ 0.1 * x + 0.3 * z + 0.8

  testthat::expect_no_error(calculate_brier_score(model = logreg_model, data = data, type = "prediction", n_boot = 10))
  logreg_model <- calculate_brier_score(model = logreg_model, data = data, type = "prediction", n_boot = 10, seed = 123)
  testthat::expect_identical(
    round_to_precision(logreg_model$results_agg |> dplyr::filter(name == "brier_score")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "brier_score_logreg.rds")))
  )
})

testthat::test_that("the brier score is calculated properly for a logreg model with dichotomous outcome and recalibrated predictions", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  logreg_model <- make_logreg_model(environment()) |>
    calculate_predictions(data)
  logreg_model$formula <- y ~ 0.1 * x + 0.3 * z + 0.8

  logreg_model <- logreg_model |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_predictions_recalibrated_type_2(data)

  testthat::expect_no_error(calculate_brier_score(model = logreg_model, data = data, type = "prediction_type_1", n_boot = 10))
  logreg_model <- calculate_brier_score(model = logreg_model, data = data, type = "prediction_type_1", n_boot = 10, seed = 123)
  testthat::expect_identical(
    round_to_precision(logreg_model$results_agg |> dplyr::filter(name == "brier_score_type_1")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "brier_score_type_1_logreg.rds")))
  )
  testthat::expect_no_error(calculate_brier_score(model = logreg_model, data = data, type = "prediction_type_2", n_boot = 10))
  logreg_model <- calculate_brier_score(model = logreg_model, data = data, type = "prediction_type_2", n_boot = 10, seed = 123)
  testthat::expect_identical(
    round_to_precision(logreg_model$results_agg |> dplyr::filter(name == "brier_score_type_2")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "brier_score_type_2_logreg.rds")))
  )
})

testthat::test_that("the brier score is only calculated once", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  logreg_model <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_predictions_recalibrated_type_2(data) |>
    calculate_brier_score(data, type = "prediction", n_boot = 5) |>
    calculate_brier_score(data, type = "prediction_type_1", n_boot = 5) |>
    calculate_brier_score(data, type = "prediction_type_2", n_boot = 5)
  cox_model <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_predictions_recalibrated_type_2(data) |>
    calculate_brier_score(data, type = "prediction", n_boot = 5) |>
    calculate_brier_score(data, type = "prediction_type_1", n_boot = 5) |>
    calculate_brier_score(data, type = "prediction_type_2", n_boot = 5)

  # original predictions
  logreg_model <- logreg_model |>
    calculate_brier_score(data, type = "prediction", n_boot = 5)
  testthat::expect_identical(
    dim(logreg_model[["results_agg"]] |> dplyr::filter(name == "brier_score"))[[1]],
    1L
    )

  cox_model <- cox_model |>
    calculate_brier_score(data, type = "prediction", n_boot = 5)
  testthat::expect_identical(
    dim(logreg_model[["results_agg"]] |> dplyr::filter(name == "brier_score"))[[1]],
    1L
    )

  # type 1 recalibrated predictions
  logreg_model <- logreg_model |>
    calculate_brier_score(data, type = "prediction_type_1", n_boot = 5)
  testthat::expect_identical(
    dim(logreg_model[["results_agg"]] |> dplyr::filter(name == "brier_score_type_1"))[[1]],
    1L
    )

  cox_model <- cox_model |>
    calculate_brier_score(data, type = "prediction_type_1", n_boot = 5)
  testthat::expect_identical(
    dim(logreg_model[["results_agg"]] |> dplyr::filter(name == "brier_score_type_1"))[[1]],
    1L
    )

  # type 2 recalibrated predictions
  logreg_model <- logreg_model |>
    calculate_brier_score(data, type = "prediction_type_2", n_boot = 5)
  testthat::expect_identical(
    dim(logreg_model[["results_agg"]] |> dplyr::filter(name == "brier_score_type_2"))[[1]],
    1L
    )

  cox_model <- cox_model |>
    calculate_brier_score(data, type = "prediction_type_2", n_boot = 5)
  testthat::expect_identical(
    dim(logreg_model[["results_agg"]] |> dplyr::filter(name == "brier_score_type_2"))[[1]],
    1L
    )
  })
