source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

test_that("returns an error if some model is not <MiceExtVal>", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  expect_error(get_forestplot_data(strat = "overall", type = "c_index", model_cox, 5), "must be <MiceExtVal>")
})

# error in calculated parameters of the model -----------------------------

test_that("returns an error if some model does not have the `c_index` calculated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_harrell_c_index(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  expect_error(get_forestplot_data(strat = "overall", type = "c_index", model_cox, model_logreg), "must have their `c_index` calculated, consider using")
})

test_that("returns an error if some model does not have the `auc` calculated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_auc(data)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  expect_error(get_forestplot_data(strat = "overall", type = "auc", model_cox, model_logreg), "must have their `auc` calculated, consider using")
})

test_that("returns an error if some model does not have the `brier_score` calculated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_brier_score(data = data, type = "predictions_aggregated", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  expect_error(get_forestplot_data(strat = "overall", type = "brier_score", model_cox, model_logreg), "must have their `brier_score` calculated, consider using")
})

test_that("returns an error if some model does not have the `brier_score_type_1` calculated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_1", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  expect_error(get_forestplot_data(strat = "overall", type = "brier_score_type_1", model_cox, model_logreg), "must have their `brier_score_type_1` calculated, consider using")
})

test_that("returns an error if some model does not have the `brier_score_type_2` calculated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_2", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  expect_error(get_forestplot_data(strat = "overall", type = "brier_score_type_2", model_cox, model_logreg), "must have their `brier_score_type_2` calculated, consider using")
})

test_that("works properly with `!!!` operator", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_brier_score(data = data, type = "predictions_aggregated", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_brier_score(data = data, type = "predictions_aggregated", n_boot = 10, seed = 123)

  params <- list("model_logreg" = model_logreg, "model_cox" = model_cox)
  expect_no_error(get_forestplot_data(strat = "overall", type = "brier_score", !!!params))
})

# proper result generation ------------------------------------------------

test_that("generates properly the forestplot data for `c_index`", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_predictions_recalibrated_type_2(data) |>
    calculate_harrell_c_index(data) |>
    calculate_auc(data) |>
    calculate_brier_score(data = data, type = "predictions_aggregated", n_boot = 10, seed = 123) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_1", n_boot = 10, seed = 123) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_2", n_boot = 10, seed = 123)

  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data) |>
    calculate_predictions_recalibrated_type_2(data) |>
    calculate_harrell_c_index(data) |>
    calculate_auc(data) |>
    calculate_brier_score(data = data, type = "predictions_aggregated", n_boot = 10, seed = 123) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_1", n_boot = 10, seed = 123) |>
    calculate_brier_score(data = data, type = "predictions_recal_type_2", n_boot = 10, seed = 123)

  expect_s3_class(get_forestplot_data(strat = "overall", type = "c_index", model_cox, model_logreg), "tbl_df")
  expect_s3_class(get_forestplot_data(strat = "overall", type = "auc", model_cox, model_logreg), "tbl_df")
  expect_s3_class(get_forestplot_data(strat = "overall", type = "brier_score", model_cox, model_logreg), "tbl_df")
  expect_s3_class(get_forestplot_data(strat = "overall", type = "brier_score_type_1", model_cox, model_logreg), "tbl_df")
  expect_s3_class(get_forestplot_data(strat = "overall", type = "brier_score_type_2", model_cox, model_logreg), "tbl_df")
})
