# Calculates the predictions for a Cox model

Calculates the predictions for the given Cox model and external
validation data. The predictions are calculated following the definition
of a cox model

\$\$S_0(t)^{e^{(\beta \cdot X)}}\$\$

where \\S_0(t)\\ stands for the survival function in a time \\t\\,
\\\beta\\ stands for the model coefficients and \\X\\ stands for the
centered values in each `id`.

## Usage

``` r
# S3 method for class 'cox'
calculate_predictions(model, data, .progress = FALSE)
```

## Arguments

- model:

  Model generated with
  [`mv_model_cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_cox.md)

- data:

  External validation data. Multiple imputation dataset in long format.

- .progress:

  `TRUE` to render the progress bar `FALSE` otherwise.

## Value

The `model` with the parameters `predictions_imp`, `predictions_agg`.

- `predictions_imp`, stores the predictions for each of the imputations

- `predictions_agg`, stores the predictions aggregated by the mean.

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
  event = survival::Surv(rpois(9, 5), rbinom(n = 9, size = 1, prob = 0.2)),
  x = rnorm(9, 1, 0.25),
  z = rnorm(9, 2, 0.75)
)

model |> calculate_predictions(data)
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
#> ── predictions_imp ──
#> 
#> # A tibble: 5 × 4
#>    .imp    id    betax prediction
#>   <dbl> <dbl>    <dbl>      <dbl>
#> 1     1     1  0.102      0.0137 
#> 2     1     2  0.0466     0.0129 
#> 3     1     3 -0.195      0.0102 
#> 4     2     1  0.00105    0.0124 
#> 5     2     2 -0.217      0.00995
#> ── predictions_agg ──
#> 
#> # A tibble: 3 × 3
#>      id   betax prediction
#>   <dbl>   <dbl>      <dbl>
#> 1     1  0.0620     0.0131
#> 2     2 -0.163      0.0106
#> 3     3 -0.162      0.0105
```
