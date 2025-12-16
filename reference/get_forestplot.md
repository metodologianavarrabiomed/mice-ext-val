# Function that generates a C-Index forestplot for the given data

Function that generates a C-Index forestplot for the given data

## Usage

``` r
get_forestplot(data, center = NULL, digits = 3, ...)
```

## Arguments

- data:

  Dataset where all the data to plot is stored. It is recommended to be
  generated using
  [`get_forestplot_data()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_forestplot_data.md)

- center:

  x intercept to display a stripped vertical line, default `NULL`, no
  center.

- digits:

  decimal digits for the table generation, default `3`

- ...:

  extra arguments passed to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html),
  [`ggplot2::geom_pointrange()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html),
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html),
  and
  [`gt::tab_options()`](https://gt.rstudio.com/reference/tab_options.html)

## Value

a C-Index forestplot stored in a `gt_tbl` table object

## Examples

``` r
if (FALSE) { # \dontrun{
get_forestplot(data, 0.5)
} # }
```
