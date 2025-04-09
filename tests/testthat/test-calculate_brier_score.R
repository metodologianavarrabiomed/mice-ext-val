source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

# errors in function arguments --------------------------------------------
testthat::test_that("fails when the model is not of class `MiceExtVal`", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  testthat::expect_error(calculate_brier_score(model = 1, data = data, type = "predictions_data"), "`model` must be <MiceExtVal>")
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

  testthat::expect_error(calculate_brier_score(model = cox_model, data = data, type = "predictions_recal_type_1"), "It seems that `type` is not yet calculated, calculate it using")
})

# cox model ---------------------------------------------------------------
testthat::test_that("the brier score is calculated properly for a logreg model with survival outcome", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  logreg_model <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  testthat::expect_no_error(calculate_brier_score(model = logreg_model, data = data, type = "predictions_data"))
})


# logreg model survival outcome -------------------------------------------
testthat::test_that("the brier score is calculated properly for a logreg model with dichotomous outcome", {
  data <- readRDS(testthat::test_path("fixtures", "mice_data.rds"))
  logreg_model <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  testthat::expect_no_error(calculate_brier_score(model = logreg_model, data = data, type = "predictions_data"))
})


# logreg model dichotomous outcome ----------------------------------------
