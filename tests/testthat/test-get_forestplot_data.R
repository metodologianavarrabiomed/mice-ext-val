source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

test_that("returns an error if some model is not <MiceExtVal>", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  expect_error(get_forestplot_data(strat = "overall", model_cox, 5), "must be <MiceExtVal>")
})

test_that("returns an error if some model does not have the `c_index` calculated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  model_logreg <- make_cox_model(environment()) |>
    calculate_predictions(data)

  expect_error(get_forestplot_data(strat = "overall", type = "c_index", model_cox, model_logreg), "must contain the `c_index`, consider using")
})

test_that("generates properly the forestplot data", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  model_logreg <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  expect_s3_class(get_forestplot_data(strat = "overall", type = "c_index", model_cox, model_logreg), "tbl_df")
})
