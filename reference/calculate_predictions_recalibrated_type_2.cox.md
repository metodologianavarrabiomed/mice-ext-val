# Calculates the type 2 recalibrated predictions for a Cox model

This function calculates the type 2 recalibrated predictions for a Cox
model. To help the recalibration of the model the function
[`get_recalibrate_params_type_2_cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_recalibrate_params_type_2_cox.md)
is defined elsewhere in this package. Using this auxiliar function the
recalibration parameters are calculated in each of the imputed datasets
stored in `data` as a long dataset.

After estimating the recalibration parameters in each of the imputed
datasets they are aggregated by their mean to use them to recalibrate
the predictions of the model. The type 2 recalibration needs from two
parameters `S0_type_2` and `beta_overall`. These parameters are
calculated with the
[`get_recalibrate_params_type_2_cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_recalibrate_params_type_2_cox.md)
function. Once they are estimated, they are aggregated by the mean.
Finally with the type 2 recalibration parameters and the aggregated
predictions the type 2 recalibrated predictions are calculated.

\$\$S\_{0, \text{type 2}}(t)^{exp(\beta\_{overall}(\beta \cdot X))}\$\$

where \\S\_{0, \text{type 2}}(t)\\ is estimated using a Weibull
distribution and \\\beta\_{overall}\\ is estimated deriving a Cox model
with \\\beta \cdot X\\ as an unique covariate. Both parameters are
estimated using the
[`get_recalibrate_params_type_2_cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_recalibrate_params_type_2_cox.md)
function.

## Usage

``` r
# S3 method for class 'cox'
calculate_predictions_recalibrated_type_2(model, data, .progress = FALSE)
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

A model with the parameter `prediction_type_2` added to
`predictions_agg` and the parameters `S0_type_2` and `beta_overall`
stored in `recal_parameters`

- `predictions_agg`: stores now a new variable `prediction_type_2`

- `S0_type_2` and `beta_overall` are stored in the `recal_parameters`.

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
  calculate_predictions_recalibrated_type_1(data) |>
  calculate_predictions_recalibrated_type_2(data)
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
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
#>    .imp    id   betax prediction
#>   <dbl> <dbl>   <dbl>      <dbl>
#> 1     1     1 -0.170      0.0104
#> 2     1     2  0.247      0.0158
#> 3     1     3  0.276      0.0162
#> 4     2     1  0.0990     0.0136
#> 5     2     2  0.0411     0.0129
#> ── predictions_agg ──
#> 
#> # A tibble: 3 × 5
#>      id   betax prediction prediction_type_1 prediction_type_2
#>   <dbl>   <dbl>      <dbl>             <dbl>             <dbl>
#> 1     1  0.129      0.0145                 0                NA
#> 2     2  0.0805     0.0135                 0                NA
#> 3     3 -0.0544     0.0123                 0                NA
#> ── recal_parameters ──
#> 
#> # A tibble: 3 × 2
#>   param           value
#>   <chr>           <dbl>
#> 1 alpha        -Inf    
#> 2 S0_type_2       0.829
#> 3 beta_overall   NA    
```
