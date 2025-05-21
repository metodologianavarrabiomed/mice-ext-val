source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))


# proper forestplot generation --------------------------------------------

test_that("the forestplot is properly generated for `c_index`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  plot <- get_forestplot_data(strat = "overall", type = "c_index", model_cox, model_logreg) |>
    get_forestplot(center = 0.3)

  expect_s3_class(plot, "ggplot")
})

test_that("the forestplot is properly generated for `auc`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_auc(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_auc(data)

  plot <- get_forestplot_data(strat = "overall", type = "auc", model_cox, model_logreg) |>
    get_forestplot(center = 0.3)

  expect_s3_class(plot, "ggplot")
})

test_that("the forestplot is properly generated for `brier_score`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_brier_score(data = data, type = "predictions_aggregated", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_brier_score(data = data, type = "predictions_aggregated", n_boot = 10, seed = 123)

  plot <- get_forestplot_data(strat = "overall", type = "brier_score", model_cox, model_logreg) |>
    get_forestplot(center = 0.3)

  expect_s3_class(plot, "ggplot")
})

test_that("the forestplot is properly generated for `brier_score_type_1`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_1", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_1", n_boot = 10, seed = 123)

  plot <- get_forestplot_data(strat = "overall", type = "brier_score_type_1", model_cox, model_logreg) |>
    get_forestplot(center = 0.3)

  expect_s3_class(plot, "ggplot")
})

test_that("the forestplot is properly generated for `brier_score_type_2`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_2", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_2", n_boot = 10, seed = 123)

  plot <- get_forestplot_data(strat = "overall", type = "brier_score_type_2", model_cox, model_logreg) |>
    get_forestplot(center = 0.3)

  expect_s3_class(plot, "ggplot")
})



# assert that model variables are present ---------------------------------

test_that("checks variables in `data` argument while generating `c_index` forestplot", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  forest_data <- get_forestplot_data(strat = "overall", type = "c_index", model_cox, model_logreg)

  expect_error(get_forestplot(forest_data |> dplyr::select(-model), center = 0.2), "The data variable `model` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-strat), center = 0.2), "The data variable `strat` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-estimate), center = 0.2), "The data variable `estimate` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-lower), center = 0.2), "The data variable `lower` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-upper), center = 0.2), "The data variable `upper` must be present in")
})

test_that("checks variables in `data` argument while generating `auc` forestplot", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_auc(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_auc(data)

  forest_data <- get_forestplot_data(strat = "overall", type = "auc", model_cox, model_logreg)

  expect_error(get_forestplot(forest_data |> dplyr::select(-model), center = 0.2), "The data variable `model` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-strat), center = 0.2), "The data variable `strat` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-estimate), center = 0.2), "The data variable `estimate` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-lower), center = 0.2), "The data variable `lower` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-upper), center = 0.2), "The data variable `upper` must be present in")
})

test_that("checks variables in `data` argument while generating `brier_score` forestplot", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_brier_score(data = data, type = "predictions_aggregated", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_brier_score(data = data, type = "predictions_aggregated", n_boot = 10, seed = 123)

  forest_data <- get_forestplot_data(strat = "overall", type = "brier_score", model_cox, model_logreg)

  expect_error(get_forestplot(forest_data |> dplyr::select(-model), center = 0.2), "The data variable `model` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-strat), center = 0.2), "The data variable `strat` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-estimate), center = 0.2), "The data variable `estimate` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-lower), center = 0.2), "The data variable `lower` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-upper), center = 0.2), "The data variable `upper` must be present in")
})

test_that("checks variables in `data` argument while generating `brier_score_type_1` forestplot", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_1", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_1", n_boot = 10, seed = 123)

  forest_data <- get_forestplot_data(strat = "overall", type = "brier_score_type_1", model_cox, model_logreg)

  expect_error(get_forestplot(forest_data |> dplyr::select(-model), center = 0.2), "The data variable `model` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-strat), center = 0.2), "The data variable `strat` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-estimate), center = 0.2), "The data variable `estimate` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-lower), center = 0.2), "The data variable `lower` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-upper), center = 0.2), "The data variable `upper` must be present in")
})

test_that("checks variables in `data` argument while generating `brier_score_type_2` forestplot", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_2", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_2", n_boot = 10, seed = 123)

  forest_data <- get_forestplot_data(strat = "overall", type = "brier_score_type_2", model_cox, model_logreg)

  expect_error(get_forestplot(forest_data |> dplyr::select(-model), center = 0.2), "The data variable `model` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-strat), center = 0.2), "The data variable `strat` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-estimate), center = 0.2), "The data variable `estimate` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-lower), center = 0.2), "The data variable `lower` must be present in")
  expect_error(get_forestplot(forest_data |> dplyr::select(-upper), center = 0.2), "The data variable `upper` must be present in")
})


# assert that data variables are proper type ------------------------------

test_that("checks properly the class of the variables in `data` while generating `c_index` foresplot", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  forest_data <- get_forestplot_data(strat = "overall", type = "c_index", model_cox, model_logreg)

  expect_error(get_forestplot(forest_data |> dplyr::mutate(model = as.factor(model)), center = 0.2), "The data variable `model` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(strat = as.factor(strat)), center = 0.2), "The data variable `strat` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(estimate = as.factor(estimate)), center = 0.2), "The data variable `estimate` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(lower = as.factor(lower)), center = 0.2), "The data variable `lower` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(upper = as.factor(upper)), center = 0.2), "The data variable `upper` has wrong typing")
})

test_that("checks properly the class of the variables in `data` while generating `auc` foresplot", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_auc(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_auc(data)

  forest_data <- get_forestplot_data(strat = "overall", type = "auc", model_cox, model_logreg)

  expect_error(get_forestplot(forest_data |> dplyr::mutate(model = as.factor(model)), center = 0.2), "The data variable `model` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(strat = as.factor(strat)), center = 0.2), "The data variable `strat` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(estimate = as.factor(estimate)), center = 0.2), "The data variable `estimate` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(lower = as.factor(lower)), center = 0.2), "The data variable `lower` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(upper = as.factor(upper)), center = 0.2), "The data variable `upper` has wrong typing")
})

test_that("checks properly the class of the variables in `data` while generating `brier_score` foresplot", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_brier_score(data = data, type = "predictions_aggregated", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_brier_score(data = data, type = "predictions_aggregated", n_boot = 10, seed = 123)

  forest_data <- get_forestplot_data(strat = "overall", type = "brier_score", model_cox, model_logreg)

  expect_error(get_forestplot(forest_data |> dplyr::mutate(model = as.factor(model)), center = 0.2), "The data variable `model` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(strat = as.factor(strat)), center = 0.2), "The data variable `strat` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(estimate = as.factor(estimate)), center = 0.2), "The data variable `estimate` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(lower = as.factor(lower)), center = 0.2), "The data variable `lower` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(upper = as.factor(upper)), center = 0.2), "The data variable `upper` has wrong typing")
})

test_that("checks properly the class of the variables in `data` while generating `brier_score_type_1` foresplot", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_1", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_1", n_boot = 10, seed = 123)

  forest_data <- get_forestplot_data(strat = "overall", type = "brier_score_type_1", model_cox, model_logreg)

  expect_error(get_forestplot(forest_data |> dplyr::mutate(model = as.factor(model)), center = 0.2), "The data variable `model` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(strat = as.factor(strat)), center = 0.2), "The data variable `strat` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(estimate = as.factor(estimate)), center = 0.2), "The data variable `estimate` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(lower = as.factor(lower)), center = 0.2), "The data variable `lower` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(upper = as.factor(upper)), center = 0.2), "The data variable `upper` has wrong typing")
})

test_that("checks properly the class of the variables in `data` while generating `brier_score_type_2` foresplot", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_2", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_2", n_boot = 10, seed = 123)

  forest_data <- get_forestplot_data(strat = "overall", type = "brier_score_type_2", model_cox, model_logreg)

  expect_error(get_forestplot(forest_data |> dplyr::mutate(model = as.factor(model)), center = 0.2), "The data variable `model` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(strat = as.factor(strat)), center = 0.2), "The data variable `strat` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(estimate = as.factor(estimate)), center = 0.2), "The data variable `estimate` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(lower = as.factor(lower)), center = 0.2), "The data variable `lower` has wrong typing")
  expect_error(get_forestplot(forest_data |> dplyr::mutate(upper = as.factor(upper)), center = 0.2), "The data variable `upper` has wrong typing")
})
