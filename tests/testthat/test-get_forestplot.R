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
