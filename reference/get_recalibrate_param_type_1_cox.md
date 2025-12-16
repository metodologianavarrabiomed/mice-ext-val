# Obtain the \\\alpha\\ value for the recalibration.

Obtains the recalibration parameters following the type 1 recalibration
definition. Recalibrates the predictions with the type 1 recalibration
The type 1 recalibration follows the guidelines presented by Crowson et
al. (2016). The recalibration follows the next proposition to obtain an
\\\alpha\\ parameter that allow to recalibrate the predictions for the
new validation dataset. The recalibration adjusts the incidence of event
known by the model to the new dataset.

The \\\alpha\\ value of the recalibration is calculated as the
difference between the survival basal function reported by the model
with the mean value of the model predictions in the external validation
cohort.

\$\$\alpha = S\_{model}(t) - \text{mean predictions}\$\$.

## Usage

``` r
get_recalibrate_param_type_1_cox(time, event, survival_predictions)
```

## Arguments

- time:

  time of follow up for each patient, must be `time`

- event:

  if the patient suffers an event or not, must be `logical`

- survival_predictions:

  survival predictions for each of the patients, must be `numeric`

## Value

The \\\alpha\\ value.

## References

C. S. Crowson, “Assessing calibration of prognostic risk scores -
Cynthia S Crowson, Elizabeth J Atkinson, Terry M Therneau, 2016,”
Statistical Methods in Medical Research, 2016.
<https://journals.sagepub.com/doi/10.1177/0962280213497434> (accessed
Nov. 17, 2023).

## Examples

``` r
if (FALSE) { # \dontrun{
get_recalibrate_param_type_1_cox(data, s0)
} # }
```
