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

A model with the parameters `predictions_aggregated`,
`predictions_data`, `betax` and `betax_data` populated.

- `predictions_aggregated`, stores the predictions aggregated by the
  mean.

- `predictions_data`, stores all the predictions in each of the imputed
  datasets.

- `betax`, stores the \\\beta \cdot X\\ values aggregated by the mean.

- `betax_data`, stores the \\\beta \cdot X\\ values in each of the
  imputed datasets.

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
#> ── predictions_aggregated ──
#> 
#> # A tibble: 3 × 2
#>      id prediction
#>   <dbl>      <dbl>
#> 1     1      0.914
#> 2     2      0.894
#> 3     3      0.894
#> ── predictions_data ──
#> 
#> # A tibble: 5 × 3
#>   prediction  .imp    id
#>        <dbl> <dbl> <dbl>
#> 1      0.917     1     1
#> 2      0.913     1     2
#> 3      0.891     1     3
#> 4      0.909     2     1
#> 5      0.889     2     2
#> ── betax ──
#> 
#> # A tibble: 3 × 2
#>      id betax
#>   <dbl> <dbl>
#> 1     1  2.36
#> 2     2  2.14
#> 3     3  2.14
#> ── betax_data ──
#> 
#> # A tibble: 5 × 3
#>   betax  .imp    id
#>   <dbl> <dbl> <dbl>
#> 1  2.40     1     1
#> 2  2.35     1     2
#> 3  2.10     1     3
#> 4  2.30     2     1
#> 5  2.08     2     2
```
