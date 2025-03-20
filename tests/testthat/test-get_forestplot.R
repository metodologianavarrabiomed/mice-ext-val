source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

test_that("the forestplot is properly generated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_c_index(data)

  model_logreg <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_c_index(data)

  plot <- get_forestplot_data(strat = "overall", model_cox, model_logreg) |>
    get_forestplot(center = 0.3)

  expect_s3_class(plot, "ggplot")
})

test_that("checks variables in `data` argument", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_c_index(data)

  model_logreg <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_c_index(data)

  forest_data <- get_forestplot_data(strat = "overall", model_cox, model_logreg)

  expect_error(get_forestplot(forest_data |> dplyr::select(-model), center = 0.2), "The data variable `model` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-strat), center = 0.2), "The data variable `strat` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-estimate), center = 0.2), "The data variable `estimate` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-lower), center = 0.2), "The data variable `lower` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-upper), center = 0.2), "The data variable `upper` must be present in")
})

test_that("checks properly the class of the variables in `data`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_c_index(data)

  model_logreg <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_c_index(data)

  forest_data <- get_forestplot_data(strat = "overall", model_cox, model_logreg)

  expect_error(get_forestplot(forest_data |> dplyr::mutate(model = as.factor(model)), center = 0.2), "The data variable `model` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(strat = as.factor(strat)), center = 0.2), "The data variable `strat` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(estimate = as.factor(estimate)), center = 0.2), "The data variable `estimate` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(lower = as.factor(lower)), center = 0.2), "The data variable `lower` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(upper = as.factor(upper)), center = 0.2), "The data variable `upper` has wrong typing")
})
