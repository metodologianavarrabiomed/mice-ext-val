# A generic function to print the `MiceExtVal` model

A generic function to print the `MiceExtVal` model

## Usage

``` r
# S3 method for class 'MiceExtVal'
print(x, ...)
```

## Arguments

- x:

  a `MiceExtVal` model

- ...:

  ignored and not passed to any function

## Value

the model printed

## Examples

``` r
model_cox <- mv_model_cox(
  formula = event ~ 0.5 * (x - 1) + 0.3 * (z - 2),
  S0 = 0.98765
)

print(model_cox)
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

model_logreg <- mv_model_logreg(formula = event ~ 0.5 * x + 0.3 * z - 1.2)

print(model_logreg)
#> 
#> ── <MiceExtVal/logreg> ─────────────────────────────────────────────────────────
#> 
#> ── formula ──
#> 
#> event ~ 0.5 * x + 0.3 * z - 1.2
```
