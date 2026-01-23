# Calculates the type 2 recalibrated predictions for a logistic regression model

Calculates the type 2 recalibrated predictions for a logistic regression
model. The type 2 recalibration uses two parameters to update the model
predictions, the \\\alpha\\ parameter allow to update the model
`intercept` and the \\\beta\_{overall}\\ parameter allow to update the
importance of the log-odds (\\\beta \cdot X\\) values. The log-odds
function can be rewritten as

\$\$log(\frac{p}{1 - p}) = \alpha + \beta\_{overall} \cdot (\beta_0 +
\beta_1 \cdot X_1 + \beta_2 \cdot X_2 + \dots + \beta_p \cdot X_p)\$\$

The parameters are estimated deriving a logistic regression model in
each of the imputations using the model log-odds as only covariate. The
coefficients of the model represent the parameter estimations and they
are aggregated using the rubin rules. Then, the recalibrated predictions
are calculated using these parameters and the aggregated log-odds.

\$\$\frac{1}{1 + e^{(-(\alpha + \beta\_{overall}(\beta \cdot X)))}}\$\$

## Usage

``` r
# S3 method for class 'logreg'
calculate_predictions_recalibrated_type_2(model, data, .progress = FALSE)
```

## Arguments

- model:

  Model generated with
  [`mv_model_logreg()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_logreg.md).
  Needs the `predictions` parameter of the model, to generate it the
  function
  [`calculate_predictions()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions.md)
  must be executed over the model.

- data:

  Data for what the predictions must be recalibrated.

- .progress:

  `TRUE` to render the progress bar `FALSE` otherwise.

## Value

A model with the parameter `prediction_type_2` added to
`predictions_agg` and the parameters `S0_type_2` and `beta_overall`
stored in `recal_parameters`

- `predictions_agg`: stores now a new variable `prediction_type_2`

- `alpha_type_2` and `beta_overall` are stored in the
  `recal_parameters`.

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
  calculate_predictions_recalibrated_type_1(data) |>
  calculate_predictions_recalibrated_type_2(data)
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
#> # A tibble: 3 × 5
#>      id   betax prediction prediction_type_1 prediction_type_2
#>   <dbl>   <dbl>      <dbl>             <dbl>             <dbl>
#> 1     1 -0.0380      0.490             0.479             1    
#> 2     2 -0.263       0.435             0.424             1.000
#> 3     3 -0.262       0.435             0.424             1.000
#> ── recal_parameters ──
#> 
#> # A tibble: 3 × 2
#>   param           value
#>   <chr>           <dbl>
#> 1 alpha_type_1  -0.0454
#> 2 alpha_type_2 156.    
#> 3 beta_overall 529.    
```
