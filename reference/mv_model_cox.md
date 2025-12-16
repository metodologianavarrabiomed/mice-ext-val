# Creates a cox model.

Creates a Cox model with all the variables needed to be used in the
functions along this package. The Cox model follows

\$\$S_0(t)^{exp(\beta \cdot X)}\$\$

so we need to fullfill all the requirements of the model. When we are
predicting the survival/risk for a \\X\\ we have to center the values
with the meas on the derivation data, normally reported inside the
articles.

## Usage

``` r
mv_model_cox(formula, S0 = NULL)
```

## Arguments

- formula:

  Formula of the model to calculate the \\\beta \cdot X\\ values,
  including coefficients and means if needed.

- S0:

  Value of the \\S_0(t)\\ function for the time of study.

## Value

A model to be used along the package with the next characteristics that
could be empty and will be generated with some other functions in the
package.

- `formula`: Formula of how the \\\beta \cdot X\\ will be calculated.

- `S0`: Value of the \\S_0(t)\\ function for the time of study.

- `alpha`: Recalibration parameter for the type 1 recalibration.

- `S0_type_2`: Value of the \\S_0(t)\\ function for the time of study
  for the type 2 recalibration.

- `beta_overall`: Recalibration parameter for the type 2 recalibration.

- `predictions_aggregated`: Aggregated predictions for the validation
  data.

- `predictions_data`: All predictions for the validation data including
  all the imputations.

- `betax`: Aggregated \\\beta \cdot X\\ values for the validation data.

- `betax_data`: All predictions for the validation data including all
  the imputations.

- `predictions_recal_type_1`: Aggregated predictions after recalibrating
  them with type 1 recalibration.

- `predictions_recal_type_2`: Aggregated predictions after recalibrating
  them with type 2 recalibration.

- `c_index`: Harrell C-Index of the predictions in the validation
  cohort.

## Examples

``` r
model <- mv_model_cox(
  formula = event ~ 0.5 * (x - 3) + 0.3 * (z - 0.2),
  S0 = 0.98765
)
```
