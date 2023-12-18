source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

test_that("Checks the parameters properly", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  # Error with no parameters
  expect_error(calculate_c_index())

  # Error if model is not a MiceExtVal model
  expect_error(calculate_c_index("test", data))

  # Error if model is ok and no data
  expect_error(calculate_c_index(make_cox_model(environment())))
  expect_error(calculate_c_index(make_cox_model(environment()), "c"))

  # No error with cox model
  expect_no_error(calculate_c_index(model_cox, data))

  # No error with logreg model
  expect_no_error(calculate_c_index(model_logreg, data))

})


test_that("Calculates the c-index properly for a cox model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_c_index(data)

  expect_identical(model_cox$c_index, readRDS(test_path("fixtures", "cox", "c_index_cox.rds")))
})

test_that("Calculates the c-index properly for a logreg model", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_c_index(data)

  expect_identical(model_logreg$c_index, readRDS(test_path("fixtures", "logreg", "c_index_logreg.rds")))
})
