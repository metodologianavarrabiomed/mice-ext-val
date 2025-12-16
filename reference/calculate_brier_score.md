# calculate the brier score for the given model definition

The Brier Score is calculated using the formula
\$\$BS=\frac{1}{n}\sum\_{t=1}^n{(f_t - o_t)^2}\$\$ where \\n\\ is the
population size, \\f_t\\ is the predictions for the row \\t\\ and
\\o_t\\ is the dichotomous observation for the row \\t\\.

The function works accordingly to the `model` definition. If the model
is defined as Cox, the survival variable is transformed to dichotomous
as `1` if the event has appeared during the follow up and `0` othercase.
In the case the model is a logistic regression the dependent variable
can be already dichotomous or survival, in the case of the survival
variable the dichotomous is defined as in the Cox model.

The confidence interval is calculated by bootstrap resamples.

## Usage

``` r
calculate_brier_score(
  model,
  data,
  type = c("predictions_aggregated", "predictions_recal_type_1",
    "predictions_recal_type_2"),
  n_boot = 1000,
  seed = NULL
)
```

## Arguments

- model:

  Model generated with
  [`mv_model_cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_cox.md)
  or
  [`mv_model_logreg()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_logreg.md).
  Needs the expected prediction parameter already calculated in the
  model. To generate the predictions you must use the function/s
  [`calculate_predictions()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions.md),
  [`calculate_predictions_recalibrated_type_1()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions_recalibrated_type_1.md)
  or
  [`calculate_predictions_recalibrated_type_2()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions_recalibrated_type_2.md)

- data:

  Data for what the observed predictions will be calculated.

- type:

  Type of the predictions that the calibration plot data should be
  generated from: `"predictions_aggregated"`,
  `"predictions_recal_type_1"` or `"predictions_recal_type_2"`

- n_boot:

  number of bootstrap resamples to calculate the Brier Score standar
  error.

- seed:

  random seed generator

## Examples

``` r
if (FALSE) { # \dontrun{
model |>
  calculate_brier_score(data, type = "predictions_aggregated")
} # }
```
