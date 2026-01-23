# Calculates the predictions for a logistic regression model

Calculates the predictions of the given logistic regression model and
external validation data. The predictions in a logistic regression model
are calculated following the next formula.

\$\$p(X) = \frac{1}{1 + e^{-(\beta \cdot X)}}\$\$

where \\X\\ stands for the covariable values and \\\beta\\ for the
coefficient values.

## Usage

``` r
# S3 method for class 'logreg'
calculate_predictions(model, data, .progress = FALSE)
```

## Arguments

- model:

  Model generated with
  [`mv_model_logreg()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_logreg.md)

- data:

  External validation data. Multiple imputation dataset in long format.

- .progress:

  `TRUE` to render the progress bar `FALSE` otherwise.

## Value

A model with the parameters `predictions_imp`, `predictions_agg`.

- `predictions_imp`, stores the predictions for each of the imputations

- `predictions_agg`, stores the predictions aggregated by the mean.

## Examples

``` r
set.seed(123)

model <- mv_model_logreg(
  formula = event ~ 0.5 * x + 0.3 * z + 1.2
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
#> ── <MiceExtVal/logreg> ─────────────────────────────────────────────────────────
#> 
#> ── formula ──
#> 
#> event ~ 0.5 * x + 0.3 * z + 1.2
#> 
#> ── predictions_imp ──
#> 
#> # A tibble: 5 × 4
#>    .imp    id betax prediction
#>   <dbl> <dbl> <dbl>      <dbl>
#> 1     1     1  2.40      0.917
#> 2     1     2  2.35      0.913
#> 3     1     3  2.10      0.891
#> 4     2     1  2.30      0.909
#> 5     2     2  2.08      0.889
#> ── predictions_agg ──
#> 
#> # A tibble: 3 × 3
#>      id betax prediction
#>   <dbl> <dbl>      <dbl>
#> 1     1  2.36      0.914
#> 2     2  2.14      0.894
#> 3     3  2.14      0.894
```
