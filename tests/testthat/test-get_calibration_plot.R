source(test_path("fixtures", "make-model-cox.R"))
source(test_path("fixtures", "make-model-logreg.R"))

# survival outcome argument errors ----------------------------------------
test_that("The calibration plot data function checks all the parameters in surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data)

  expect_error(get_calibration_plot_data_surv())
  expect_error(get_calibration_plot_data_surv(2, data, 10, "prediction"))
  expect_error(get_calibration_plot_data_surv(model, 4, 10, "prediction"))
  expect_error(get_calibration_plot_data_surv(model, 4, "a", "prediction"))
  expect_error(get_calibration_plot_data_surv(model, data, 10, "test"))
})

test_that("Returns an error if `.imp` is not part of the `data` parameter in surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |> calculate_predictions(data)
  logreg_model <- make_logreg_model(environment()) |> calculate_predictions(data)
  data_no_imp <- data |> dplyr::select(-.imp)

  expect_error(
    get_calibration_plot_data_surv(
      model = cox_model,
      data = data_no_imp,
      n_groups = 2,
      type = "prediction"
    ),
    "must contain `.imp`"
  )
  expect_error(
    get_calibration_plot_data_surv(
      model = logreg_model,
      data = data_no_imp,
      n_groups = 2,
      type = "prediction"
    ),
    "must contain `.imp`"
  )
})

test_that("Returns an error if `id` is not part of the `data` parameter in surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |> calculate_predictions(data)
  logreg_model <- make_logreg_model(environment()) |> calculate_predictions(data)
  data_no_id <- data |> dplyr::select(-id)

  expect_error(
    get_calibration_plot_data_surv(
      model = cox_model,
      data = data_no_id,
      n_groups = 2,
      type = "prediction"
    ),
    "must contain `id`"
  )
  expect_error(
    get_calibration_plot_data_surv(
      model = logreg_model,
      data = data_no_id,
      n_groups = 2,
      type = "prediction"
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
      type = "prediction"
    ),
    "the dependent variable `y` must be <Surv>"
  )
  model_cox_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(
    get_calibration_plot_data_surv(
      model = model_cox_bad_dependent_variable,
      data = data,
      n_groups = 2,
      type = "prediction"
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
      type = "prediction"
    ),
    "the dependent variable `y` must be <Surv>"
  )
  model_logreg_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(
    get_calibration_plot_data_surv(
      model = model_logreg_bad_dependent_variable,
      data = data,
      n_groups = 2,
      type = "prediction"
    ),
    "the dependent variable `no_exists` must be part of `data`"
  )
})

test_that("Returns an error when the 'prediction' is not calculated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment())
  model_cox <- make_cox_model(environment())

  expect_error(
    get_calibration_plot_data_surv(
      model = model_logreg,
      data = data,
      n_groups = 2,
      type = "prediction"
    ),
    "It seems that `prediction` is not yet calculated, calculate it using"
  )
  expect_error(
    get_calibration_plot_data_surv(
      model = model_cox,
      data = data,
      n_groups = 2,
      type = "prediction"
    ),
    "It seems that `prediction` is not yet calculated, calculate it using"
  )
})

test_that("Returns an error when the 'prediction_type_1' is not calculated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment())
  model_cox <- make_cox_model(environment())

  expect_error(
    get_calibration_plot_data_surv(
      model = model_logreg,
      data = data,
      n_groups = 2,
      type = "prediction_type_1"
    ),
    "It seems that `prediction_type_1` is not yet calculated, calculate it using"
  )
  expect_error(
    get_calibration_plot_data_surv(
      model = model_cox,
      data = data,
      n_groups = 2,
      type = "prediction_type_1"
    ),
    "It seems that `prediction_type_1` is not yet calculated, calculate it using"
  )

  })
test_that("Returns an error when the 'prediction_type_2' is not calculated", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model_logreg <- make_logreg_model(environment())
  model_cox <- make_cox_model(environment())

  expect_error(
    get_calibration_plot_data_surv(
      model = model_logreg,
      data = data,
      n_groups = 2,
      type = "prediction_type_2"
    ),
    "It seems that `prediction_type_2` is not yet calculated, calculate it using"
  )
  expect_error(
    get_calibration_plot_data_surv(
      model = model_cox,
      data = data,
      n_groups = 2,
      type = "prediction_type_2"
    ),
    "It seems that `prediction_type_2` is not yet calculated, calculate it using"
  )


  })

# dichotomous outcome argument errors -------------------------------------
test_that("The calibration plot data function checks all the parameters in prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data)

  expect_error(get_calibration_plot_data_prop())
  expect_error(get_calibration_plot_data_prop(2, data, 10, "prediction"))
  expect_error(get_calibration_plot_data_prop(model, 4, 10, "prediction"))
  expect_error(get_calibration_plot_data_prop(model, 4, "a", "prediction"))
  expect_error(get_calibration_plot_data_prop(model, data, 10, "test"))
})

test_that("Returns an error if `.imp` is not part of the `data` parameter in prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |> calculate_predictions(data)
  logreg_model <- make_logreg_model(environment()) |> calculate_predictions(data)
  data_no_imp <- data |> dplyr::select(-.imp)

  expect_error(
    get_calibration_plot_data_prop(
      model = cox_model,
      data = data_no_imp,
      n_groups = 2,
      type = "prediction"
    ),
    "must contain `.imp`"
  )
  expect_error(
    get_calibration_plot_data_prop(
      model = logreg_model,
      data = data_no_imp,
      n_groups = 2,
      type = "prediction"
    ),
    "must contain `.imp`"
  )
})

test_that("Returns an error if `id` is not part of the `data` parameter in prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  cox_model <- make_cox_model(environment()) |> calculate_predictions(data)
  logreg_model <- make_logreg_model(environment()) |> calculate_predictions(data)
  data_no_id <- data |> dplyr::select(-id)

  expect_error(
    get_calibration_plot_data_prop(
      model = cox_model,
      data = data_no_id,
      n_groups = 2,
      type = "prediction"
    ),
    "must contain `id`"
  )
  expect_error(
    get_calibration_plot_data_prop(
      model = logreg_model,
      data = data_no_id,
      n_groups = 2,
      type = "prediction"
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
      type = "prediction"
    ),
    "the dependent variable `y` must be <Surv>"
  )
  model_cox_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(
    get_calibration_plot_data_prop(
      model = model_cox_bad_dependent_variable,
      data = data,
      n_groups = 2,
      type = "prediction"
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
      type = "prediction"
    ),
    "the dependent variable `a` must be <Surv/numeric>"
  )
  model_logreg_bad_dependent_variable$formula <- no_exists ~ x + z
  expect_error(
    get_calibration_plot_data_prop(
      model = model_logreg_bad_dependent_variable,
      data = data,
      n_groups = 2,
      type = "prediction"
    ),
    "the dependent variable `no_exists` must be part of `data`"
  )
})

# survival outcome works --------------------------------------------------
test_that("The calibration plot data function works properly with type 'prediction' cox with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment())

  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction"))

  model <- model |> calculate_predictions(data)

  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "prediction"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_1"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_surv(model, data, 2, "prediction")),
    round_to_precision(readRDS(test_path("fixtures", "cox", "calibration_plot_data_2_groups_cox.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'prediction' logreg with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment())

  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction"))

  model <- model |> calculate_predictions(data)

  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "prediction"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_1"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_surv(model, data, 2, "prediction")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_logreg.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'prediction_type_1' cox with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)

  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_1"))

  model <- model |> calculate_predictions_recalibrated_type_1(data)

  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "prediction"))
  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_1"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_surv(model, data, 2, "prediction_type_1")),
    round_to_precision(readRDS(test_path("fixtures", "cox", "calibration_plot_data_2_groups_recal_1_cox.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'prediction_type_1' logreg with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |> calculate_predictions(data)

  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_1"))

  model <- model |> calculate_predictions_recalibrated_type_1(data)

  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "prediction"))
  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_1"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_surv(model, data, 2, "prediction_type_1")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_recal_1_logreg.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'prediction_type_2' cox with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)

  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_2"))

  model <- model |> calculate_predictions_recalibrated_type_2(data)

  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "prediction"))
  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_2"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_1"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_surv(model, data, 2, "prediction_type_2")),
    round_to_precision(readRDS(test_path("fixtures", "cox", "calibration_plot_data_2_groups_recal_2_cox.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'prediction_type_2' logreg with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |> calculate_predictions(data)

  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_2"))

  model <- model |> calculate_predictions_recalibrated_type_2(data)

  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "prediction"))
  expect_no_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_2"))
  expect_error(get_calibration_plot_data_surv(model, data, 2, "prediction_type_1"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_surv(model, data, 2, "prediction_type_2")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_recal_2_logreg.rds")))
  )
})

test_that("The calibration plot data checks the 'data' parameter with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  plot_data <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    get_calibration_plot_data_surv(data, 2, "prediction")

  expect_error(get_calibration_plot())
  expect_error(get_calibration_plot(4))
  expect_no_error(get_calibration_plot(plot_data))
})

test_that("The calibration plot function generates a plot for a Cox model and predictions aggregated with surv function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |> calculate_predictions(data)

  expect_s3_class(
    model |>
      get_calibration_plot_data_surv(data, 2, "prediction") |>
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
      get_calibration_plot_data_surv(data, 2, "prediction_type_1") |>
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
      get_calibration_plot_data_surv(data, 2, "prediction_type_2") |>
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
      get_calibration_plot_data_surv(data, 2, "prediction") |>
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
      get_calibration_plot_data_surv(data, 2, "prediction_type_1") |>
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
      get_calibration_plot_data_surv(data, 2, "prediction_type_2") |>
      get_calibration_plot(),
    "ggplot"
  )
})

# dichotomous outcome works -----------------------------------------------
test_that("The calibration plot data function works properly with type 'prediction' cox with prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment())

  expect_error(get_calibration_plot_data_prop(model, data, 2, "prediction"))

  model <- model |> calculate_predictions(data)

  expect_no_error(get_calibration_plot_data_prop(model, data, 2, "prediction"))
  expect_error(get_calibration_plot_data_prop(model, data, 2, "prediction_type_1"))
  expect_error(get_calibration_plot_data_prop(model, data, 2, "prediction_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_prop(model, data, 2, "prediction")),
    round_to_precision(readRDS(test_path("fixtures", "cox", "calibration_plot_data_2_groups_cox_prop.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'prediction' logreg with survival outcome with prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment())

  expect_error(get_calibration_plot_data_prop(model, data, 2, "prediction"))

  model <- model |> calculate_predictions(data)

  expect_no_error(get_calibration_plot_data_prop(model, data, 2, "prediction"))
  expect_error(get_calibration_plot_data_prop(model, data, 2, "prediction_type_1"))
  expect_error(get_calibration_plot_data_prop(model, data, 2, "prediction_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_prop(model, data, 2, "prediction")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_logreg_prop.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'prediction' logreg with numeric outcome with prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment())
  model$formula <- y ~ 0.1 * x + 0.3 * z + 0.8

  expect_error(get_calibration_plot_data_prop(model, data, 2, "prediction"))

  model <- model |> calculate_predictions(data)

  expect_no_error(get_calibration_plot_data_prop(model, data, 2, "prediction"))
  expect_error(get_calibration_plot_data_prop(model, data, 2, "prediction_type_1"))
  expect_error(get_calibration_plot_data_prop(model, data, 2, "prediction_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_prop(model, data, 2, "prediction")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_logreg_prop.rds")))
  )

  data$y[[1]] <- 3
  expect_error(get_calibration_plot_data_prop(model, data, 2, "prediction"))
})

test_that("The calibration plot data function works properly with type 'prediction_type_1' cox with prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data)

  expect_no_error(get_calibration_plot_data_prop(model, data, 2, "prediction_type_1"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_prop(model, data, 2, "prediction_type_1")),
    round_to_precision(readRDS(test_path("fixtures", "cox", "calibration_plot_data_2_groups_cox_recal_1_prop.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'prediction_type_1' logreg with survival outcome with prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data)

  expect_no_error(get_calibration_plot_data_prop(model, data, 2, "prediction_type_1"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_prop(model, data, 2, "prediction_type_1")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_logreg_recal_1_prop.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'prediction_type_1' logreg with numeric outcome with prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment())
  model$formula <- y ~ 0.1 * x + 0.3 * z + 0.8
  model <- model |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_1(data)

  expect_no_error(get_calibration_plot_data_prop(model, data, 2, "prediction_type_1"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_prop(model, data, 2, "prediction_type_1")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_logreg_recal_1_prop.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'prediction_type_2' cox with prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_cox_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)

  expect_no_error(get_calibration_plot_data_prop(model, data, 2, "prediction_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_prop(model, data, 2, "prediction_type_2")),
    round_to_precision(readRDS(test_path("fixtures", "cox", "calibration_plot_data_2_groups_cox_recal_2_prop.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'prediction_type_2' logreg with survival outcome with prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment()) |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)

  expect_no_error(get_calibration_plot_data_prop(model, data, 2, "prediction_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_prop(model, data, 2, "prediction_type_2")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_logreg_recal_2_prop.rds")))
  )
})

test_that("The calibration plot data function works properly with type 'prediction_type_2' logreg with numeric outcome with prop function", {
  data <- readRDS(test_path("fixtures", "mice_data.rds"))
  model <- make_logreg_model(environment())
  model$formula <- y ~ 0.1 * x + 0.3 * z + 0.8
  model <- model |>
    calculate_predictions(data) |>
    calculate_predictions_recalibrated_type_2(data)

  expect_no_error(get_calibration_plot_data_prop(model, data, 2, "prediction_type_2"))
  expect_identical(
    round_to_precision(get_calibration_plot_data_prop(model, data, 2, "prediction_type_2")),
    round_to_precision(readRDS(test_path("fixtures", "logreg", "calibration_plot_data_2_groups_logreg_recal_2_prop.rds")))
  )
})
