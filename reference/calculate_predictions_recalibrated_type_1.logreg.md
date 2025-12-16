# Calculates the type 1 recalibrated predictions for a logistic regression model.

Calculates the type 1 recalibrated predictions for a logistic regression
model. The type 1 recalibration is defined by an \\\alpha\\ parameter
that updates the value of the `intercept` (\\\beta_0\\) of the model.
The log-odds function is rewritten as follows.

\$\$log(\frac{p}{1 - p}) = \alpha + \beta_0 + \beta_1 \cdot X_1 +
\beta_2 \cdot X_2 + \dots + \beta_p \cdot X_p\$\$

Thus, the predictions are updated by adjusting the `intercept` value in
the model against the external validation data. The \\\alpha\\ parameter
is estimated in each of the imputed datasets by deriving a logistic
regression model using the model log-odds as offset. The coefficients in
all the models are aggregated using the mean. Using the aggregated
parameter and the aggregated log-odds the new predictions are calculated
as follows.

\$\$\frac{1}{1 + e^{(-(\alpha + (\beta \cdot X)))}}\$\$

## Usage

``` r
# S3 method for class 'logreg'
calculate_predictions_recalibrated_type_1(model, data, .progress = FALSE)
```

## Arguments

- model:

  Model generated with
  [`mv_model_logreg()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_logreg.md).
  Needs the `predictions` parameter of the model, to generate it the
  function `calculate_predictions` must be executed over the model. This
  attribute must be generated using
  [`calculate_predictions()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions.md)

- data:

  Data for what the predictions must be recalibrated.

- .progress:

  `TRUE` to render the progress bar, `FALSE` otherwise.

## Value

A model with the parameters `predictions_recal_type_1` and
`alpha_type_1` populated.

- `predictions_recal_type_1`: stores the type 1 recalibrated predictions
  as follows

  |     |            |
  |-----|------------|
  | id  | prediction |
  | 1   | 0.03       |
  | ... | ...        |
  | n   | 0.16       |

- `alpha_type_1`: stores the \\\alpha\\ recalibration parameter.

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
