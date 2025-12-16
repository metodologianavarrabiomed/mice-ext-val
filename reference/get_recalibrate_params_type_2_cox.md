# Obtains the \\S_0(t)\\ and \\\beta\_{overall}\\ parameters for recalibration

Calculates the recalibration type 2 parameters for the
`recalibrate_data` and returns a list with the two needed parameters
\\S_0(t)\\, the survival estimation in the time \\t\\ for the external
validation data, and \\\beta\_{overall}\\, the importance that the
\\\beta \cdot X\\ values should have over the external validation data.

The survival function estimation is calculated as a Weibull
distribution. A Weibull model is derived in the external validation
cohort and the value estimated by the model for the time \\t\\ is used
as the recalibration of \\S_0(t)\\. The importance of the \\\beta \cdot
X\\ is calculated by deriving a Cox model using as only covariate the
model \\\beta \cdot X\\ values. The derived Cox model coefficient is
used as \\\beta\_{overall}\\.

## Usage

``` r
get_recalibrate_params_type_2_cox(time, event, betax)
```

## Arguments

- time:

  time of follow up for each patient, must be `numeric`

- event:

  if the patient has suffered an event during follow up, must be
  `numeric` \\1\\ event \\0\\ othercase

- betax:

  the betax values, \\\beta X\\, of the original predictions, must be
  `numeric`

## Value

a `list` with two elements:

- S0: \\S_0(t)\\

- beta_overall: \\\beta\_{overall}\\ value from the type 2 recalibration

## Examples

``` r
if (FALSE) { # \dontrun{
get_recalibrate_params_type_2_cox(data, betax, t)
} # }
```
