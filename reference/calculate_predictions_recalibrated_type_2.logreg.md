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

A model with the parameter `predictons_recalibrated_type_2`, `S0_type_2`
and `beta_overall` populated.

- `predictions_recal_type_2`: stores the type 2 recalibrated predictions
  as follows.

  |     |                   |
  |-----|-------------------|
  | id  | prediction_type_2 |
  | 1   | 0.03              |
  | ... | ...               |
  | n   | 0.16              |

- `alpha_type_2`: stores the \\\alpha\\ type 2 recalibration parameter.

- `beta_overall`: stores the \\\beta\_{overall}\\ type 2 recalibration
  parameter.

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
#> ── alpha_type_1 ──
#> 
#> -0.0453625350741732
#> 
#> ── alpha_type_2 ──
#> 
#> 155.926045685198
#> 
#> ── beta_overall ──
#> 
#> 528.594440157522
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
#> ── predictions_recal_type_2 ──
#> 
#> # A tibble: 3 × 2
#>      id prediction_type_2
#>   <dbl>             <dbl>
#> 1     1             1    
#> 2     2             1.000
#> 3     3             1.000
```
