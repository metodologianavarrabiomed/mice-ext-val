# Calculates the type 1 recalibration predictions for a model.

A generic method for calculating the type 1 recalibration predictions
for a model.

## Usage

``` r
calculate_predictions_recalibrated_type_1(model, data, .progress = FALSE)
```

## Arguments

- model:

  Model for which the recalibrated predictions are calculated

- data:

  Data parameter for
  [`calculate_predictions_recalibrated_type_1.cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions_recalibrated_type_1.cox.md)
  function or for
  [`calculate_predictions_recalibrated_type_1.logreg()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions_recalibrated_type_1.logreg.md)
  function.

- .progress:

  .progress parameter for
  [`calculate_predictions_recalibrated_type_1.cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions_recalibrated_type_1.cox.md)
  function or for
  [`calculate_predictions_recalibrated_type_1.logreg()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions_recalibrated_type_1.logreg.md)
  function.

## Value

A model with the variable `prediction_type_1` added to `predictions_agg`
and the recalibration parameter added to `recal_parameters`.

## Examples

``` r
set.seed(123)

model <- mv_model_logreg(formula = event ~ 0.5 * x + 0.3 * z - 1.2)

data <- data.frame(
  .imp = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  event = survival::Surv(rpois(9, 5), rbinom(n = 9, size = 1, prob = 0.5)),
  x = rnorm(9, 1, 0.25),
  z = rnorm(9, 2, 0.75)
)

model |>
  calculate_predictions(data) |>
  calculate_predictions_recalibrated_type_1(data)
#> 
#> ── <MiceExtVal/logreg> ─────────────────────────────────────────────────────────
#> 
#> ── formula ──
#> 
#> event ~ 0.5 * x + 0.3 * z - 1.2
#> 
#> ── predictions_imp ──
#> 
#> # A tibble: 5 × 4
#>    .imp    id    betax prediction
#>   <dbl> <dbl>    <dbl>      <dbl>
#> 1     1     1  0.00210      0.501
#> 2     1     2 -0.0534       0.487
#> 3     1     3 -0.295        0.427
#> 4     2     1 -0.0989       0.475
#> 5     2     2 -0.317        0.421
#> ── predictions_agg ──
#> 
#> # A tibble: 3 × 4
#>      id   betax prediction prediction_type_1
#>   <dbl>   <dbl>      <dbl>             <dbl>
#> 1     1 -0.0380      0.490             0.479
#> 2     2 -0.263       0.435             0.424
#> 3     3 -0.262       0.435             0.424
#> ── recal_parameters ──
#> 
#> # A tibble: 1 × 2
#>   param          value
#>   <chr>          <dbl>
#> 1 alpha_type_1 -0.0454
```
