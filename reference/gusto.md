# An example of the GUSTO-I W region dataset extracted from the book 'Clinical Prediction Models', see references. We have inserted missing values to the dataset in order to be used as an example for this package.

An example of the GUSTO-I W region dataset extracted from the book
'Clinical Prediction Models', see references. We have inserted missing
values to the dataset in order to be used as an example for this
package.

## Usage

``` r
data(gusto)
```

## Format

A data frame with nine variables:

- `sho`:

  Shock: Killip class 3/4 vs. 1/2

- `a65`:

  If the patient is older than 65 years old

- `dia`:

  Diagnosed diabetes

- `hig`:

  Categorized for high risk of Anterior Infart Location or Prevous
  Myocardial Infarction

- `hyp`:

  Hypotension: Systolic Blood Pressure \< 100 mmHg

- `hrt`:

  Heart rate: Pulse\>80 (Tachycardia)

- `ttr`:

  Time to relief of chest pain \> 1h

- `day30`:

  30 day mortality

- `sex`:

  Gender

a dataset with 2188 rows and 9 variables

## References

Steyerberg, E.W. (2019). Updating for a New Setting. In: Clinical
Prediction Models. Statistics for Biology and Health. Springer, Cham.
https://doi.org/10.1007/978-3-030-16399-0_20
