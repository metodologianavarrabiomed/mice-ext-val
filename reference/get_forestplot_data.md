# Generates the forestplot data needed in [`get_forestplot()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_forestplot.md)

Generates the forestplot data needed in
[`get_forestplot()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_forestplot.md)

## Usage

``` r
get_forestplot_data(
  strat,
  type = c("harrell_c_index", "auc", "brier_score", "brier_score_type_1",
    "brier_score_type_2"),
  ...
)
```

## Arguments

- strat:

  Name of the strat that the models are part of

- type:

  Choosen statistic to plot. It must be already calculated

- ...:

  A list of models that should be plotted

## Value

A `tibble` with the information needed to generate the forestplot

## Examples

``` r
if (FALSE) { # \dontrun{
data <- get_forestplot_data(strat = "overall", cox_model, logreg_model)
data <- get_forestplot_data(strat = "overall", Cox = cox_model, LogReg = logreg_model)
} # }
```
