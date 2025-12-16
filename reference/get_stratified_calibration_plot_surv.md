# generates an stratified calibration plot

generates an stratified calibration plot

## Usage

``` r
get_stratified_calibration_plot_surv(
  data,
  n_groups,
  type = c("predictions_aggregated", "predictions_recal_type_1",
    "predictions_recal_type_2"),
  ...
)
```

## Arguments

- data:

  dataset where the dependent variable for all the models is

- n_groups:

  number of points that should be displayed

- type:

  Type of the predictions that the calibration plot data should be
  generated from: `"predictions_aggregated"`,
  `"predictions_recal_type_1"` or `"predictions_recal_type_2"`. The
  variable with the predictions need to be generated if it is not
  consider using the
  [`calculate_predictions_recalibrated_type_1()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions_recalibrated_type_1.md)
  and
  [`calculate_predictions_recalibrated_type_2()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions_recalibrated_type_2.md)
  functions.

- ...:

  models that should be plotted in the stratified calibration plot. If
  they are a named paramter the name is used as strat

## Value

the stratified calibration plot for the given models

## Examples

``` r
if (FALSE) { # \dontrun{
get_stratified_calibration_plot_surv(data, 10, model1, model2)
get_stratified_calibration_plot_surv(data, 10, strat1 = model1, strat2 = model2)
} # }
```
