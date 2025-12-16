# Generates the data needed for the calibration plot

Generates the data needed for the calibration plot. The calibration plot
needs to separate the model predictions by risk groups. First the
function separates the predictions in `n_groups` then computes the mean
value of the model predictions and also the observed value. The observed
value is the estimated value at the study time estimated using a
Kaplan-Meier estimator.

## Usage

``` r
get_calibration_plot_data_surv(
  model,
  data,
  n_groups,
  type = "predictions_aggregated"
)
```

## Arguments

- model:

  Model generated with
  [`mv_model_cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_cox.md)
  or
  [`mv_model_logreg()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_logreg.md).
  Needs the `predictions` parameter of the model, to generate it the
  function
  [`calculate_predictions()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions.md)
  must be executed over the model. If we want to obtain also the
  recalibrated data the model must be initalize the recalibrated
  predictions with
  [`calculate_predictions_recalibrated_type_1()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions_recalibrated_type_1.md)
  and
  [`calculate_predictions_recalibrated_type_2()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions_recalibrated_type_2.md).

- data:

  Data for what the observed predictions will be calculated.

- n_groups:

  Number of groups that must be calculated.

- type:

  Type of the predictions that the calibration plot data should be
  generated from: `"predictions_aggregated"`,
  `"predictions_recal_type_1"` or `"predictions_recal_type_2"`

## Value

`tibble` with the data ready to generate a calibration plot.

|         |            |          |
|---------|------------|----------|
| group   | prediction | observed |
| 1       | 0.03       | 0.05     |
| ...     | ...        | ...      |
| n_group | 0.84       | 0.79     |

## Examples

``` r
if (FALSE) { # \dontrun{
model |>
  get_calibration_plot_data_surv(data = test_data, n_groups = 10, type = "predictions_aggregated")
} # }
```
