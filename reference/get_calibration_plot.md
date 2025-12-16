# Generates the calibration plot

Generates the calibration plot from the data needed to print it. This
data is generated with the
[`get_calibration_plot_data_surv()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_calibration_plot_data_surv.md)
and
[`get_calibration_plot_data_prop()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_calibration_plot_data_prop.md)
functions.

## Usage

``` r
get_calibration_plot(data)
```

## Arguments

- data:

  Outcome of the
  [`get_calibration_plot_data_surv()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_calibration_plot_data_surv.md)
  or
  [`get_calibration_plot_data_prop()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_calibration_plot_data_prop.md)
  functions.

## Value

ggplot2 object ready to be plotted

## Examples

``` r
if (FALSE) { # \dontrun{
model |>
  get_calibration_plot_data_surv(data, 10, type = "predictions_aggregated") |>
  get_calibration_plot()

model |>
  get_calibration_plot_data_prop(data, 10, type = "predictions_aggregated") |>
  get_calibration_plot()
} # }
```
