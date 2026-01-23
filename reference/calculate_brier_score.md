# calculate the brier score for the given model

The Brier Score is calculated using the formula
\$\$BS=\frac{1}{n}\sum\_{t=1}^n{(f_t - o_t)^2}\$\$ where \\n\\ is the
population size, \\f_t\\ is the predictions for the row \\t\\ and
\\o_t\\ is the dichotomous observation for the row \\t\\.

The function operates according to the chosen `model`: if the model is
Cox, the survival variable is converted into a binary value (1 if the
event occurred during follow-up, 0 otherwise), whereas for a logistic
regression model, the dependent variable may already be binary or
represent survival time; in the latter case, it is transformed into a
binary variable using the same rule as in the Cox model.

The confidence interval is calculated by bootstrap resamples.

## Usage

``` r
calculate_brier_score(
  model,
  data,
  type = c("prediction", "prediction_type_1", "prediction_type_2"),
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
  generated from: `"prediction"`, `"prediction_type_1"` or
  `"prediction_type_2"`

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
