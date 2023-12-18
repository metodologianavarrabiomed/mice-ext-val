source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

test_that("Parameter errors work", {
  expect_error(get_c_index_forestplot())
  expect_error(get_c_index_forestplot(model = 2))
})

test_that("Works for cox models", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data)

  expect_error(suppressWarnings(get_c_index_forestplot(model)))

  model <- model |>
    calculate_c_index(data)
  expect_no_error(get_c_index_forestplot(model))
  expect_no_error(get_c_index_forestplot(model, model, model))
})

test_that("Works for logreg models", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  expect_error(suppressWarnings(get_c_index_forestplot(model)))

  model <- model |>
    calculate_c_index(data)
  expect_no_error(get_c_index_forestplot(model))
  expect_no_error(get_c_index_forestplot(model, model, model))
})

test_that("Works for logreg and cox models at the same time", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_c_index(data)
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_c_index(data)

  expect_no_error(get_c_index_forestplot(model_cox, model_logreg))
  expect_s3_class(get_c_index_forestplot(model_cox, model_logreg), "gforge_forestplot")
})
