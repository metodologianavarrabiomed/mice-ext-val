# Creates a logistic regression model

Creates a logistic regression model with all the requirements for the
external validation. The logistic regression model calculates the
predictions following the next equation.

\$\$p(X) = \frac{1}{1 + e^{-(\beta \cdot X)}}\$\$

where we can observe that the prediction only depends on the \\\beta\\
coefficients and the covariable values, \\X\\. The logistic regression
model have a parameter called `intercept` that usually is represented as
\\\beta_0\\. The risk prediction is derived from the log-odds function.

\$\$log(\frac{p}{1 - p}) = \beta_0 + \beta_1 \cdot X_1 + \beta_2 \cdot
X_2 + \dots + \beta_p \cdot X_p\$\$

where we can see that the `intercept` value is not associated with any
covariable. Therefore this function parameters are the minimum needed to
obtain the predictions.

## Usage

``` r
mv_model_logreg(formula)
```

## Arguments

- formula:

  The model formula to calculate the linear predictor, including
  coefficients and intercept

## Value

A model to be used along the package with the next characteristics that
could be empty and will be generated with some other functions in the
package.

- `formula`: Formula of the model containing the coefficients and the
  intercept.

- `alpha_type_1`: The \\\alpha\\ value for the type 1 recalibration.

- `alpha_type_2`: The \\\alpha\\ value for the type 2 recalibration.

- `beta_overall`: The \\\beta\_{overall}\\ value for the type 2
  recalibration.

- `predictions_aggregated`: Aggregated predictions for the validation
  data.

- `predictions_data`: All of the predictions for the validation data in
  each imputation.

- `betax`: Aggregated \\\beta \cdot X\\ values for the validation data.

- `betax_data`: All the \\\beta \cdot X\\ values for the validation in
  each imputation.

- `predictions_recal_type_1`: Aggregated predictions after recalibrating
  them with type 1 recalibration.

- `predictions_recal_type_2`: Aggregated predictions after recalibrating
  them with type 2 recalibration.

- `c_index`: Harrell C-Index of the predictions in the validation
  cohort.

## Examples

``` r
model <- mv_model_logreg(formula = event ~ 0.5 * x + 0.3 * z - 1.2)
```
