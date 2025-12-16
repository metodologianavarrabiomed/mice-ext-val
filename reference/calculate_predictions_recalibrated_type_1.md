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

A model with the parameter `predictions_recal_type_1` and also the
recalibration parameters are populated.

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
#> ── alpha_type_1 ──
#> 
#> -0.0453625350741732
#> 
#> ── predictions_aggregated ──
#> 
#> # A tibble: 3 × 2
#>      id prediction
#>   <dbl>      <dbl>
#> 1     1      0.490
#> 2     2      0.435
#> 3     3      0.435
#> ── predictions_data ──
#> 
#> # A tibble: 5 × 3
#>   prediction  .imp    id
#>        <dbl> <dbl> <dbl>
#> 1      0.501     1     1
#> 2      0.487     1     2
#> 3      0.427     1     3
#> 4      0.475     2     1
#> 5      0.421     2     2
#> ── betax ──
#> 
#> # A tibble: 3 × 2
#>      id   betax
#>   <dbl>   <dbl>
#> 1     1 -0.0380
#> 2     2 -0.263 
#> 3     3 -0.262 
#> ── betax_data ──
#> 
#> # A tibble: 5 × 3
#>      betax  .imp    id
#>      <dbl> <dbl> <dbl>
#> 1  0.00210     1     1
#> 2 -0.0534      1     2
#> 3 -0.295       1     3
#> 4 -0.0989      2     1
#> 5 -0.317       2     2
#> ── predictions_recal_type_1 ──
#> 
#> # A tibble: 3 × 2
#>      id prediction_type_1
#>   <dbl>             <dbl>
#> 1     1             0.479
#> 2     2             0.424
#> 3     3             0.424
```
