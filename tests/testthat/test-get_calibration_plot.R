source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

# survival outcome argument errors ----------------------------------------
test_that("The calibration plot data function checks all the parameters in surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data)


  expect_error(get_calibration_plot_data_surv())
  expect_error(get_calibration_plot_data_surv(2, data, 10, "predictions_aggregated"))
  expect_error(get_calibration_plot_data_surv(model, 4, 10, "predictions_aggregated"))
  expect_error(get_calibration_plot_data_surv(model, 4, "a", "predictions_aggregated"))
  expect_error(get_calibration_plot_data_surv(model, data, 10, "test"))
})

test_that("Returns an error if `.imp` is not part of the `data` parameter in surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |> calculate_predictions(data)
  logreg_model <- make_logreg_model(environment()) |> calculate_predictions(data)
  data_no_imp <- data |> select(-.imp)

  expect_error(
    get_calibration_plot_data_surv(
      model = cox_model,
      data = data_no_imp,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "must contain `.imp`"
  )
  expect_error(
    get_calibration_plot_data_surv(
      model = logreg_model,
      data = data_no_imp,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "must contain `.imp`"
  )
})

test_that("Returns an error if `id` is not part of the `data` parameter in surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |> calculate_predictions(data)
  logreg_model <- make_logreg_model(environment()) |> calculate_predictions(data)
  data_no_id <- data |> select(-id)

  expect_error(
    get_calibration_plot_data_surv(
      model = cox_model,
      data = data_no_id,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "must contain `id`"
  )
  expect_error(
    get_calibration_plot_data_surv(
      model = logreg_model,
      data = data_no_id,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "must contain `id`"
  )
})

test_that("Returns an error when the formula is not properly defined in cox in surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)

  model_cox_bad_dependent_variable <- model_cox
  model_cox_bad_dependent_variable$formula <- y ~ x + z
  expect_error(
    get_calibration_plot_data_surv(
      model = model_cox_bad_dependent_variable,
      data = data,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "the dependent variable `y` must be <Surv>"
  )
  model_cox_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(
    get_calibration_plot_data_surv(
      model = model_cox_bad_dependent_variable,
      data = data,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "the dependent variable `no_exists` must be part of `data`"
  )
})

test_that("Returns an error when the formula is not properly defined in logreg in surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_logreg_bad_dependent_variable <- model_logreg
  model_logreg_bad_dependent_variable$formula <- y ~ x + z
  expect_error(
    get_calibration_plot_data_surv(
      model = model_logreg_bad_dependent_variable,
      data = data,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "the dependent variable `y` must be <Surv>"
  )
  model_logreg_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(
    get_calibration_plot_data_surv(
      model = model_logreg_bad_dependent_variable,
      data = data,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "the dependent variable `no_exists` must be part of `data`"
  )
})

# dichotomous outcome argument errors -------------------------------------
test_that("The calibration plot data function checks all the parameters in prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data)

  expect_error(get_calibration_plot_data_prop())
  expect_error(get_calibration_plot_data_prop(2, data, 10, "predictions_aggregated"))
  expect_error(get_calibration_plot_data_prop(model, 4, 10, "predictions_aggregated"))
  expect_error(get_calibration_plot_data_prop(model, 4, "a", "predictions_aggregated"))
  expect_error(get_calibration_plot_data_prop(model, data, 10, "test"))
})

test_that("Returns an error if `.imp` is not part of the `data` parameter in prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |> calculate_predictions(data)
  logreg_model <- make_logreg_model(environment()) |> calculate_predictions(data)
  data_no_imp <- data |> select(-.imp)

  expect_error(
    get_calibration_plot_data_prop(
      model = cox_model,
      data = data_no_imp,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "must contain `.imp`"
  )
  expect_error(
    get_calibration_plot_data_prop(
      model = logreg_model,
      data = data_no_imp,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "must contain `.imp`"
  )
})

test_that("Returns an error if `id` is not part of the `data` parameter in prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |> calculate_predictions(data)
  logreg_model <- make_logreg_model(environment()) |> calculate_predictions(data)
  data_no_id <- data |> select(-id)

  expect_error(
    get_calibration_plot_data_prop(
      model = cox_model,
      data = data_no_id,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "must contain `id`"
  )
  expect_error(
    get_calibration_plot_data_prop(
      model = logreg_model,
      data = data_no_id,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "must contain `id`"
  )
})

test_that("Returns an error when the formula is not properly defined in cox in prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_cox <- make_cox_model(environment()) |>
    calculate_predictions(data)

  model_cox_bad_dependent_variable <- model_cox
  model_cox_bad_dependent_variable$formula <- y ~ x + z
  expect_error(
    get_calibration_plot_data_prop(
      model = model_cox_bad_dependent_variable,
      data = data,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "the dependent variable `y` must be <Surv>"
  )
  model_cox_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(
    get_calibration_plot_data_prop(
      model = model_cox_bad_dependent_variable,
      data = data,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "the dependent variable `no_exists` must be part of `data`"
  )
})

test_that("Returns an error when the formula is not properly defined in logreg in prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  model_logreg_bad_dependent_variable <- model_logreg
  data[["a"]] <- rep("a", length(data[["y"]]))
  model_logreg_bad_dependent_variable$formula <- a ~ x + z
  expect_error(
    get_calibration_plot_data_prop(
      model = model_logreg_bad_dependent_variable,
      data = data,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "the dependent variable `a` must be <Surv/numeric>"
  )
  model_logreg_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(
    get_calibration_plot_data_prop(
      model = model_logreg_bad_dependent_variable,
      data = data,
      n_groups = 2,
      type = "predictions_aggregated"
    ),
    "the dependent variable `no_exists` must be part of `data`"
  )
})

# survival outcome works --------------------------------------------------
test_that("The calibration plot data function works properly with type 'predictions_aggregated' cox with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment())

  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_aggregated"))

  model <- model |> calculate_predictions(data)

  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "predictions_aggregated"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_1"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_surv(model, data, 2, "predictions_aggregated")),
    round_to_precision(readRDS(test_path("fixtures", "cox", "calibration_plot_data_2_groups_cox.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'predictions_aggregated' logreg with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment())

  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_aggregated"))

  model <- model |> calculate_predictions(data)

  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "predictions_aggregated"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_1"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_surv(model, data, 2, "predictions_aggregated")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_logreg.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'predictions_recal_type_1' cox with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)

  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_1"))

  model <- model |> calculate_predictions_recalibrated_type_1(data)

  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "predictions_aggregated"))
  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_1"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_1")),
    round_to_precision(readRDS(test_path("fixtures", "cox", "calibration_plot_data_2_groups_recal_1_cox.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'predictions_recal_type_1' logreg with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |> calculate_predictions(data)

  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_1"))

  model <- model |> calculate_predictions_recalibrated_type_1(data)

  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "predictions_aggregated"))
  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_1"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_1")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_recal_1_logreg.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'predictions_recal_type_2' cox with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)

  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_2"))

  model <- model |> calculate_predictions_recalibrated_type_2(data)

  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "predictions_aggregated"))
  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_2"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_1"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_2")),
    round_to_precision(readRDS(test_path("fixtures", "cox", "calibration_plot_data_2_groups_recal_2_cox.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'predictions_recal_type_2' logreg with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |> calculate_predictions(data)

  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_2"))

  model <- model |> calculate_predictions_recalibrated_type_2(data)

  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "predictions_aggregated"))
  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_2"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_1"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_surv(model, data, 2, "predictions_recal_type_2")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_recal_2_logreg.rds")))
  )
})

test_that("The calibration plot data checks the 'data' parameter with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  plot_data <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    get_calibration_plot_data_surv(data, 2, "predictions_aggregated")

  expect_error(get_calibration_plot())
  expect_error(get_calibration_plot(4))
  expect_no_error(get_calibration_plot(plot_data))
})

test_that("The calibration plot function generates a plot for a Cox model and predictions aggregated with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data_surv(data, 2, "predictions_aggregated") |>
      get_calibration_plot(),
    "ggplot"
  )
})

test_that("The calibration plot function generates a plot for a Cox model and predictions recalibrated type 1 with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data_surv(data, 2, "predictions_recal_type_1") |>
      get_calibration_plot(),
    "ggplot"
  )
})

test_that("The calibration plot function generates a plot for a Cox model and predictions recalibrated type 2 with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data_surv(data, 2, "predictions_recal_type_2") |>
      get_calibration_plot(),
    "ggplot"
  )
})

test_that("The calibration plot function generates a plot for a logreg model and predictions aggregated with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |>
    calculate_predictions(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data_surv(data, 2, "predictions_aggregated") |>
      get_calibration_plot(),
    "ggplot"
  )
})

test_that("The calibration plot function generates a plot for a logreg model and predictions recalibrated type 1 with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data_surv(data, 2, "predictions_recal_type_1") |>
      get_calibration_plot(),
    "ggplot"
  )
})

test_that("The calibration plot function generates a plot for a logreg model and predictions recalibrated type 2 with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data_surv(data, 2, "predictions_recal_type_2") |>
      get_calibration_plot(),
    "ggplot"
  )
})


# dichotomous outcome works -----------------------------------------------
