# Calculates the type 1 recalibrated predictions

Using the function
[`get_recalibrate_param_type_1_cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_recalibrate_param_type_1_cox.md)
calculates the recalibration parameters in each of the imputed datasets
stored in `data`. With all the parameters estimated aggregates them and
calculates the recalibrated predictions with these aggregated parameters
and the aggregated predictions.

## Usage

``` r
# S3 method for class 'cox'
calculate_predictions_recalibrated_type_1(model, data, .progress = FALSE)
```

## Arguments

- model:

  Model generated with
  [`mv_model_cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_cox.md).
  Needs the `predictions` parameter of the model, to generate it the
  function
  [`calculate_predictions()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions.md)
  must be executed over the model.

- data:

  External validation data. Multiple imputation dataset in long format.

- .progress:

  `TRUE` to render the progress bar `FALSE` otherwise.

## Value

A model with the parameter `predictions_recal_type_1` and `alpha`
populated.

- `predictions_recal_type_1`: stores the type 1 recalibrated predictions
  stored as follows

  |     |            |
  |-----|------------|
  | id  | prediction |
  | 1   | 0.03       |
  | ... | ...        |
  | n   | 0.16       |

- `alpha`: stores the \\\alpha\\ recalibration parameter.

## Examples

``` r
set.seed(123)

model <- mv_model_cox(
  formula = event ~ 0.5 * (x - 1) + 0.3 * (z - 2),
  S0 = 0.98765
)

data <- data.frame(
  .imp = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  x = rnorm(9, 1, 0.25),
  z = rnorm(9, 2, 0.75),
  event = survival::Surv(rpois(9, 5), rbinom(n = 9, size = 1, prob = 0.5))
)

model |>
  calculate_predictions(data) |>
  calculate_predictions_recalibrated_type_1(data)
#> 
#> ── <MiceExtVal/cox> ────────────────────────────────────────────────────────────
#> 
#> ── formula ──
#> 
#> event ~ 0.5 * (x - 1) + 0.3 * (z - 2)
#> 
#> ── S0 ──
#> 
#> 0.98765
#> 
#> ── alpha ──
#> 
#> -Inf
#> 
#> ── predictions_aggregated ──
#> 
#> # A tibble: 3 × 2
#>      id prediction
#>   <dbl>      <dbl>
#> 1     1     0.0145
#> 2     2     0.0135
#> 3     3     0.0123
#> ── predictions_data ──
#> 
#> # A tibble: 5 × 3
#>   prediction  .imp    id
#>        <dbl> <dbl> <dbl>
#> 1     0.0104     1     1
#> 2     0.0158     1     2
#> 3     0.0162     1     3
#> 4     0.0136     2     1
#> 5     0.0129     2     2
#> ── betax ──
#> 
#> # A tibble: 3 × 2
#>      id   betax
#>   <dbl>   <dbl>
#> 1     1  0.129 
#> 2     2  0.0805
#> 3     3 -0.0544
#> ── betax_data ──
#> 
#> # A tibble: 5 × 3
#>     betax  .imp    id
#>     <dbl> <dbl> <dbl>
#> 1 -0.170      1     1
#> 2  0.247      1     2
#> 3  0.276      1     3
#> 4  0.0990     2     1
#> 5  0.0411     2     2
#> ── predictions_recal_type_1 ──
#> 
#> # A tibble: 3 × 2
#>      id prediction_type_1
#>   <dbl>             <dbl>
#> 1     1                 0
#> 2     2                 0
#> 3     3                 0
```
