# Example Of Use With Survival Outcome

##### Required Libraries

The following code snippet lists all the libraries needed to run this
report.

``` r
library(MiceExtVal)

# tidyverse packages
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

# table formatting packages
library(kableExtra)
library(gtsummary)

# analysis required packages
library(mice)
library(survival)
```

In this vignette, we demonstrate how to use the `MiceExtVal` package to
externally validate a prediction model with a survival outcome.
Specifically, we will validate the Framingham model for cardiovascular
risk prediction, see Wilson et al. ([1998](#ref-framingham)), using a
publicly available dataset obtained from
[Kaggle](https://www.kaggle.com). The Framingham model is a Cox model
that estimates 10-year risk of suffering a heart attack in patients
30-79 years old with no history of Coronary Heart Disease (CHD).

The example is organized into the following steps:

1.  Exploring the dataset
2.  Imputing missing data
3.  Defining the Framingham model
4.  Performing external validation

This structure is designed to guide users through a typical workflow,
highlighting how `MiceExtVal` can support robust validation of survival
models using multiply imputed data.

## Explore the Dataset

The first step of every external validation is to become familiar with
the dataset. To perform an external validation of the Framingham model,
we require a dataset that contains the relevant predictor variables used
by the model. For this purpose, the `MiceExtVal` package includes a
version of the Framingham dataset, originally shared on
[Kaggle](https://www.kaggle.com) by [Shrey
Jain](https://www.kaggle.com/datasets/shreyjain601/framingham-heart-study).
The dataset is accessible as
[`MiceExtVal::framingham`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/framingham.md)

This dataset includes key clinical and demographic variables needed to
replicate the Framingham risk prediction. A summary of the dataset’s
variables is provided in [Table 1](#tbl-summary-data), which serves as a
starting point for exploring the structure and content of the data.

[TABLE]

Table 1: Summary table of the Framingham dataset

The dataset contains some missing values, particularly in
cholesterol-related variables such as `Total Cholesterol`,
`HDL Cholesterol`, and `LDL Cholesterol`. These missing values must be
addressed before proceeding with model validation.
[Table 2](#tbl-perc-miss) shows the percentage of missing data for each
variable, highlighting the extent of missingness across the dataset.

| Variable | Percentage of Missings |
|:---------|:----------------------:|
| ldlc     |         73.97          |
| hdlc     |         73.97          |
| totchol  |          3.52          |

Table 2: Percentage of missings

## Impute Missing Data

To address the missing values, we will use the `mice` package to perform
multiple imputation, see van Buuren & Groothuis-Oudshoorn
([2011](#ref-mice-package)). In this vignette, we apply the default
imputation method provided by `mice`, as our primary goal is not to
optimize the imputation itself but to demonstrate a realistic use case
in which missing data must be handled prior to external validation. This
step allows us to proceed with validating one or more prediction models
using the imputed dataset.

``` r
pred_matrix <- mice::make.predictorMatrix(MiceExtVal::framingham)
pred_matrix[, "randid"] <- 0
pred_matrix[, "period"] <- 0

mice_data <- mice::mice(MiceExtVal::framingham)
fram_long <- mice::complete(mice_data, "long")
```

This report is aimed to show how the `MiceExtVal` package works it is
not an explanation on how to impute missing data. If you need a further
explanation of this methods please refer to `mice` explanations,
([Buuren, 2018](#ref-vanBuuren2018); [van Buuren & Groothuis-Oudshoorn,
2011](#ref-mice-package)). After the imputation the imputed datasets are
stored in `fram_long` in long format.

## The Framingham Model

With the data prepared, the next step is to examine the prediction model
we intend to validate: the Framingham risk model, see Wilson et al.
([1998](#ref-framingham)). This model is based on a Cox proportional
hazards regression and is designed to estimate the risk of developing
coronary heart disease in individuals aged 30 to 79 years. The model
includes several clinical and demographic predictors, and its estimated
coefficients are presented in [Table 3](#tbl-framingham-coefficients).

| Variable                                     |   Men   |  Women  |
|:---------------------------------------------|:-------:|:-------:|
| Age                                          | 0.0483  | 0.3377  |
| Age squared                                  |   \-    | -0.0027 |
| Diabetes                                     | 0.4284  | 0.5963  |
| Smoker                                       | 0.5234  | 0.2925  |
| TC, mg/dL                                    |         |         |
| \<160                                        | -0.6594 | -0.2614 |
| 160-199                                      |   \-    |   \-    |
| 200-239                                      | 0.1769  | 0.2077  |
| 240-279                                      | 0.5054  | 0.2439  |
| \>280                                        | 0.6571  | 0.5351  |
| HDL-C, mg/dL                                 |         |         |
| \<35                                         | 0.4974  | 0.8431  |
| 35-44                                        | 0.2431  | 0.3780  |
| 45-49                                        |   \-    | 0.1978  |
| 50-59                                        | -0.0511 |   \-    |
| \>60                                         | -0.4866 | -0.4295 |
| Blood Pressure                               |         |         |
| Optimal                                      | -0.0023 | -0.5336 |
| Normal                                       |   \-    |   \-    |
| High Normal                                  | 0.2832  | -0.0677 |
| Stage I hypertension                         | 0.5217  | 0.2629  |
| Stage II-IV hypertension                     | 0.6186  | 0.4657  |
| Baseline survival function at 10 years, S(t) | 0.9002  | 0.9625  |

Table 3: Framingham model coefficients

Most of the variables are categorical, this type of variables must be
represented as numeric. If any variable is multicategorical as
`Total Cholesterol` we need to generate as many dichotomous variables as
categories. This type of representation is usually used in the
mathematical formula definition of the models.

### Requirements to Define the Model

Before we can define the Framingham model in the `MiceExtVal` package
for validation we must ensure that the predictor variables in our
dataset are consistent with those used in the original derivation
cohort. Although the dataset includes the necessary information to
calculate each patient’s risk, several variables need to be transformed
to match the model or the package specifications.

As shown in [Table 3](#tbl-framingham-coefficients), some continuous
variables—specifically `Total Cholesterol`,`HDL Cholesterol`, and
`Blood Pressure` were categorized in the original model. Therefore, we
will recode these variables accordingly before applying the model to our
data.

#### Total Cholesterol

The total cholesterol variable is categorized into five categories, less
than \\160\\ mg/dL, between \\160\\ and \\199\\ mg/dL, between \\200\\
and \\239\\ mg/dL, between \\240\\ and \\279\\ mg/dl, and more than
\\280\\ mg/dL. The following code snippet generates the variable in the
dataset.

| Variable      |     Men |   Women |
|:--------------|--------:|--------:|
| **TC, mg/dL** |         |         |
| \<160         | -0.6594 | -0.2614 |
| 160-199       |      \- |      \- |
| 200-239       |  0.1769 |  0.2077 |
| 240-279       |  0.5054 |  0.2439 |
| \>280         |  0.6571 |  0.5351 |

``` r
fram_long <- fram_long |>
  dplyr::mutate(
    totchol_less_160 = as.numeric(totchol < 160),
    totchol_160_199 = as.numeric(totchol >= 160 & totchol < 200),
    totchol_200_239 = as.numeric(totchol >= 200 & totchol < 240),
    totchol_240_279 = as.numeric(totchol >= 240 & totchol < 280),
    totchol_greater_280 = as.numeric(totchol >= 280),
  )
```

#### HDL Cholesterol

The Framingham model characterizes the HDL cholesterol into five groups,
less than \\35\\ mg/dL, between \\35\\ and \\44\\ mg/dL, between \\45\\
and \\49\\ mg/dL, between \\50\\ and \\59\\ mg/dL, and more than \\60\\
mg/dL. As before the following code snippet generates the variable in
the dataset.

| Variable       |      Men |    Women |
|:---------------|---------:|---------:|
| **HDL, mg/dL** |          |          |
| \<35           |  0.49744 |  0.84312 |
| 35-44          |  0.24310 |  0.37796 |
| 45-49          |       \- |  0.19785 |
| 50-59          | -0.05107 |       \- |
| \>60           | -0.48660 | -0.42951 |

``` r
fram_long <- fram_long |>
  dplyr::mutate(
    hdl_less_35 = as.numeric(hdlc < 35),
    hdl_35_44 = as.numeric(hdlc >= 35 & hdlc < 44),
    hdl_45_49 = as.numeric(hdlc >= 45 & hdlc < 49),
    hdl_50_59 = as.numeric(hdlc >= 50 & hdlc < 59),
    hdl_greater_60 = as.numeric(hdlc >= 60),
  )
```

#### Blood Pressure

In the model it is defined a variable that aggregates the systolyc blood
pressure and diastolic blood pressure into five groups of hypertension.
As defined by Wilson et al. ([1998](#ref-framingham)), we categorize
each of the individual blood pressure variables into five groups,
optimal, normal, high-normal, hypertension I, and hypertension II-IV. If
a patient have different blood pressure categories we use the highest
one as a representation of they state. The following code adds the
variable to the dataset, we first generate the individual categorization
variables and generate the `pressure` variable as the aggregation of
both.

| Variable                 |      Men |    Women |
|:-------------------------|---------:|---------:|
| **Blood Pressure**       |          |          |
| Optimal                  | -0.00226 | -0.53363 |
| Normal                   |       \- |       \- |
| High Normal              |  0.28320 | -0.06773 |
| Stage I hypertension     |  0.52168 |  0.26288 |
| Stage II-IV hypertension |  0.61859 |  0.46573 |

We first generate a variable called `pressure` that categorize both
pressures into one variable.

``` r
pressure_labels <- c(
  "optimal", "normal", "high-normal", "hypertension I", "hypertension II-IV"
)
fram_long <- fram_long |>
  dplyr::mutate(
    sys_lvl = cut(
      sysbp,
      breaks = c(-Inf, 120, 130, 140, 160, Inf),
      labels = pressure_labels
    ),
    dys_lvl = cut(
      diabp,
      breaks = c(-Inf, 80, 85, 90, 100, Inf),
      labels = pressure_labels
    ),
    pressure = purrr::map2_int(
      as.numeric(sys_lvl), as.numeric(dys_lvl), ~ max(.x, .y)
    ),
    pressure = factor(
      pressure,
      labels = pressure_labels
    )
  )
```

We can generate the different variables used in the model from the
`pressure` variable.

``` r
fram_long <- fram_long |>
  dplyr::mutate(
    bp_optimal = as.numeric(pressure == "optimal"),
    bp_high_normal = as.numeric(pressure == "high-normal"),
    bp_hypertension_i = as.numeric(pressure == "hypertension I"),
    bp_hypertension_ii_iv = as.numeric(pressure == "hypertension II-IV")
  )
```

#### Generating Extra Needed Variables

Some variables like `cursmoke` or `diabetes` are factors and to be used
in the model we need to transform them to numeric variables where \\1\\
indicates `TRUE` and \\0\\ indicates `FALSE`. We will also generate the
variable `age2` \\age^2\\ needed for the women model. Additionally, we
also generate the survival outcome `anychd_surv` that is needed to
define the model.

``` r
fram_long <- fram_long |>
  dplyr::mutate(
    cursmoke = as.numeric(cursmoke == "1"),
    diabetes = as.numeric(diabetes == "1"),
    age2 = age**2,
    anychd_surv = survival::Surv(timechd, anychd == "Yes")
  )
```

## External Validation

Traditional external validation is typically performed on a single,
complete dataset and involves several sequential steps. First, the
original model is used to generate predictions for the new dataset.
Then, the model’s performance is evaluated by assessing its
discrimination (e.g. AUC) and calibration. If necessary, the model may
be recalibrated, and performance metrics re-evaluated after adjustment.

However, when working with datasets containing missing values—especially
in clinical research—multiple imputation is often used to preserve data
integrity and avoid bias. As a result, we no longer have a single
dataset, but multiple imputed versions of the original data, each
representing a plausible completion. This complicates traditional
validation workflows, since each imputed dataset yields different
results that must be pooled appropriately.

The `MiceExtVal` package is designed to bridge this gap by streamlining
the external validation process across multiple imputed datasets. It
allows users to perform all validation steps—including prediction,
performance evaluation, and optional recalibration while properly
handling the variability introduced by imputation.

After generating the required Framingham variables, we can proceed to
define the model using the `MiceExtVal` package. The original Framingham
model is stratified by sex, meaning separate sets of coefficients are
used for men and women. To reflect this structure, we will define two
separate Cox models, one for each sex.

The `MiceExtVal` package provides the
[`MiceExtVal::mv_model_cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_cox.md)
function to specify Cox proportional hazards models that can be used
with multiply imputed data. In the following steps, we will use this
function to define both sex-specific models for external validation.

### Generate the Models

In the `MiceExtVal` package, models are defined using specialized
constructors depending on the outcome type: use
[`MiceExtVal::mv_model_cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_cox.md)
for Cox proportional hazards models and
[`MiceExtVal::mv_model_logreg()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_logreg.md)
for logistic regression models. Since the Framingham model is a survival
model based on the Cox framework, we will use
[`MiceExtVal::mv_model_cox()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/mv_model_cox.md)
in this example.

To define a Cox model for external validation, the following key
components must be provided:

- `formula`: A formula specifying the structure of the model. This
  includes the predictor variables and their coefficients. If the
  original model was developed with centered variables, the formula
  should reflect that by subtracting the corresponding means to
  correctly compute the linear predictor (\\\beta \cdot X\\).
- `S0`: The baseline survival probability \\S_0(t)\\ at a specific time
  point \\t\\. For the Framingham model, predictions are made at 10
  years, so we will use \\S_0(10 \text{ years})\\ as provided in the
  original publication.

As the Framingham model is stratified by sex, we will define and
validate two separate models—one for men and one for women—each with its
corresponding formula and baseline survival value.

#### Men

The next code snippet generates the men model formula following the
original mean values and the coefficients described in the previous
tables.

``` r
men_formula <- anychd_surv ~ 0.0483 * (age - 48.3) +
  0.4284 * (diabetes - 0.05) + 0.5234 * (cursmoke - 0.403) -
  0.6595 * (totchol_less_160 - 0.075) + 0.1769 * (totchol_200_239 - 0.39) +
  0.5054 * (totchol_240_279 - 0.165) + 0.6571 * (totchol_greater_280 - 0.057) +
  0.4974 * (hdl_less_35 - 0.192) + 0.2431 * (hdl_35_44 - 0.357) -
  0.0511 * (hdl_50_59 - 0.19) - 0.4866 * (hdl_greater_60 - 0.106) -
  0.0023 * (bp_optimal - 0.202) + 0.2832 * (bp_high_normal - 0.202) +
  0.5217 * (bp_hypertension_i - 0.225) +
  0.6186 * (bp_hypertension_ii_iv - 0.128)
```

Once we have defined the needed parameters of the model we can create
the model as follows.

``` r
men_model <- MiceExtVal::mv_model_cox(
  formula = men_formula,
  S0 = 0.9002
)
```

#### Women

For women we can reproduce what we have already done for men.

``` r
women_formula <- anychd_surv ~ 0.3377 * (age - 49.6) -
  0.0027 * (age2 - 2604.5) + 0.5963 * (diabetes - 0.038) +
  0.2925 * (cursmoke - 0.378) + -0.2614 * (totchol_less_160 - 0.079) +
  0.2077 * (totchol_200_239 - 0.327) + 0.2439 * (totchol_240_279 - 0.2) +
  0.5351 * (totchol_greater_280 - 0.91) + 0.8431 * (hdl_less_35 - 0.043) +
  0.378 * (hdl_35_44 - 0.149) + 0.1978 * (hdl_45_49 - 0.124) -
  0.4295 * (hdl_greater_60 - 0.407) - 0.5336 * (bp_optimal - 0.348) -
  0.0677 * (bp_high_normal - 0.15) + 0.2629 * (bp_hypertension_i - 0.186) +
  0.4657 * (bp_hypertension_ii_iv - 0.1)
```

From the previous parameters we generate the `women_model` to externally
validate the Framingham women model.

``` r
women_model <- MiceExtVal::mv_model_cox(
  formula = women_formula,
  S0 = 0.9625
)
```

### Generate Stratified Datasets

As the Framingham model is defined as an stratified model we need to
generate the stratified datasets. Each model results must be calculated
in their respective subcohort of the dataset. To obtain the subcohorts
we simply filter the data for each of the genders and generate
`men_fram_long` and `women_fram_long` variables.

``` r
men_fram_long <- fram_long |> dplyr::filter(sex == 1)
women_fram_long <- fram_long |> dplyr::filter(sex == 2)
```

### External Validation Results

Once we’ve defined the Framingham model using the `MiceExtVal` package,
we can proceed to perform external validation. The functions in the
package are organized into two main categories:

- Functions that calculate predictions or other results based on the
  model definition begin with `calculate_`.
- Functions that obtain the external validation results begin with
  `get_`.

The external validation process can be divided into three main phases:

1.  Calculate Predictions: This step involves generating predicted risk
    scores for the individuals in the validation cohort.
2.  Calculate Recalibrations: If necessary, recalibration of the model
    is performed to adjust for any systematic differences between the
    derivation and validation cohorts.
3.  Obtain External Validation Results: This final step generates the
    validation metrics, such as discrimination and calibration, that
    assess the model’s performance in the external dataset.

### Calculate the predictions

``` r
men_model <- men_model |>
  MiceExtVal::calculate_predictions(data = men_fram_long)

women_model <- women_model |>
  MiceExtVal::calculate_predictions(data = women_fram_long)
```

After executing
[`MiceExtVal::calculate_predictions()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions.md),
the model object is updated with additional information derived from the
imputed dataset. Specifically, the predicted probabilities are computed
for each imputation and stored within the model. In addition, the
function calculates aggregated predictions for each patient by pooling
results across imputations. An example of these values are shown in
[Table 4](#tbl-model-predictions-example).

| id    | prediction |
|:------|-----------:|
| 2448  |      0.045 |
| 9428  |      0.139 |
| 14367 |      0.165 |
| 16365 |      0.168 |
| 20375 |      0.242 |
| 33077 |      0.114 |

\(a\) Aggregated predictions

| prediction | .imp | id    |
|-----------:|-----:|:------|
|      0.051 |    1 | 2448  |
|      0.202 |    1 | 9428  |
|      0.146 |    1 | 14367 |
|      0.168 |    1 | 16365 |
|      0.268 |    1 | 20375 |
|      0.123 |    1 | 33077 |

\(b\) Model predictions in each imputation

Table 4: Men Framingham model predictions

Beyond the predicted probabilities, the model object also includes the
prognostic indexes (\\\beta \cdot X\\), both at the individual
imputation level and as pooled estimates.
[Table 5](#tbl-model-betax-example) presents these values.

| id    |  betax |
|:------|-------:|
| 2448  | -0.851 |
| 9428  |  0.308 |
| 14367 |  0.499 |
| 16365 |  0.559 |
| 20375 |  0.941 |
| 33077 |  0.125 |

\(a\) Aggregated Beta X

|  betax | .imp | id    |
|-------:|-----:|:------|
| -0.701 |    1 | 2448  |
|  0.765 |    1 | 9428  |
|  0.408 |    1 | 14367 |
|  0.559 |    1 | 16365 |
|  1.087 |    1 | 20375 |
|  0.222 |    1 | 33077 |

\(b\) Model Beta X in each imputation

Table 5: Men Framingham model Beta X

### Model Performance

The model performance can be separated in two. The ability of the model
to discriminate high-risk patients from low-risk patients and its
calibration i.e. the ability to match the real risk and predicted risk.
The package provides different functions depending on the type of
outcome and model that it is externally validated. In this case as we
are externally validating a Cox model we will use the functions
associated to this model and a survival outcome.

#### Calculating C-Index

The model discrimination ability is calculated with the C-index or AUC.
The package provides users with two functions,
[`MiceExtVal::calculate_auc()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_auc.md)
for the AUC calculation and
[`MiceExtVal::calculate_harrell_c_index()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_harrell_c_index.md)
that calculates the Harrell C-Index. Both functions require that the
model predictions are already calculated. In our case we are externally
validating a Cox model, so we can calculate the C-Index in both models
by using the
[`MiceExtVal::calculate_harrell_c_index()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_harrell_c_index.md)
function.

``` r
men_model <- men_model |>
  MiceExtVal::calculate_harrell_c_index(data = men_fram_long)

women_model <- women_model |>
  MiceExtVal::calculate_harrell_c_index(data = women_fram_long)

dplyr::bind_rows(men_model[["c_index"]], women_model[["c_index"]]) |>
  tibble::add_column(Model = c("Men", "Women"), .before = "Estimate") |>
  kableExtra::kbl(digits = 3)
```

| Model | Estimate | 95% CI L | 95% CI U | P-val |
|:------|---------:|---------:|---------:|------:|
| Men   |    0.686 |    0.661 |    0.712 |     0 |
| Women |    0.707 |    0.664 |    0.749 |     0 |

Table 6: C-Index results

In this article we also show how to recalibrate the model with the two
techniques provided in the package. This recalibration of the
predictions does not modify the value of the C-Index because all the
predictions are modified using the same parameters of recalibration. As
all the variable are modified equally the order of the predictions are
not modified and therefore the C-Index remains unchanged.

#### Calculating Calibration Plot

The calibration plots help users to visualize how accurate are the model
predictions to the observed risks. To calculate the calibration plots it
is mandatory to have calculated before the model predictions or they
recalibration if needed. The package provides the user with some
functions that allow them to generate the calibration plots.

The calibration plot calculation is divided into two steps, first step
calculates the data where observed and predicted risks are represented.
The observed risk can be calculated in different ways, in the package we
have defined two function that allow the users to calculate them. The
[`MiceExtVal::get_calibration_plot_data_surv()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_calibration_plot_data_surv.md)
function calculates the observed risk by a Kaplan-Meier estimator and
the
[`MiceExtVal::get_calibration_plot_data_prop()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_calibration_plot_data_prop.md)
function that calculates the observed risk as the proportion of events.
From this data the calibration plot can be generated using the
[`MiceExtVal::get_calibration_plot()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/get_calibration_plot.md)
function. The following code snippet shows how to calculate the
calibration plots for the Framingham models.

``` r
men_model |>
  MiceExtVal::get_calibration_plot_data_surv(
    data = men_fram_long,
    n_groups = 10,
    type = "predictions_aggregated"
  ) |>
  MiceExtVal::get_calibration_plot()

women_model |>
  MiceExtVal::get_calibration_plot_data_surv(
    data = women_fram_long,
    n_groups = 10,
    type = "predictions_aggregated"
  ) |>
  MiceExtVal::get_calibration_plot()
```

![Calibration plot for original predictions in men and women
](get-started-surv_files/figure-html/fig-framingham-cal-plot-1.png)

\(a\) Men

![Calibration plot for original predictions in men and women
](get-started-surv_files/figure-html/fig-framingham-cal-plot-2.png)

\(b\) Women

Figure 1: Framingham original predictions calibration plots

The calibration plots are a `ggplot` object and they can be modified as
any other plot of the `ggplot2` package. Feel free to modify them to
match your requirements.

#### Calculation Brier Score

The Brier score is an statistic that measure the accuracy between the
predicted riska and the observed outcome. In survival analysis we do no
have an observed risk for each of the patients. Neverteless we can
assume that the outcome is dichotomous and calculate the Brier score
with this type of outcome. The statistic is calculated as:

\\ BS = \frac{1}{N}\sum^N\_{t = 1}(f_t - o_t)^2 \\

where \\N\\ is the size of the population, \\f_t\\ is the model
prediction for the patient \\t\\ and \\o_t\\ is the observed risk for
the patient \\t\\ in this case \\0\\ if they suffer no event and \\1\\
otherwise.

In the `MiceExtVal` package we have developed the function
[`MiceExtVal::calculate_brier_score`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_brier_score.md)
that help users to calculate the model brier score.

``` r
men_model <- men_model |>
  MiceExtVal::calculate_brier_score(
    data = men_fram_long, type = "predictions_aggregated", n_boot = 100
  )

women_model <- women_model |>
  MiceExtVal::calculate_brier_score(
    data = women_fram_long, type = "predictions_aggregated", n_boot = 100
  )
dplyr::bind_rows(men_model[["brier_score"]], women_model[["brier_score"]]) |>
  tibble::add_column(Model = c("Men", "Women"), .before = "Estimate") |>
  kableExtra::kbl(digits = 3)
```

| Model | Estimate | 95% CI L | 95% CI U |
|:------|---------:|---------:|---------:|
| Men   |    0.254 |    0.242 |    0.265 |
| Women |    0.184 |    0.171 |    0.197 |

Table 7: Brier score results

In [Table 7](#tbl-brier-score) there are represented the brier score
results for both models. It is important to remark that they are not
comparable between them as they are calculated over different subcohorts
of the framingham complete cohort.

### Recalibration

When we are validating a model in an external cohort is common that the
model predictions do not match the observed risks in the new cohort. To
minimize this problem we have developed two functions that allow the
users to recalibrate the model predictions (please refer to
[`MiceExtVal::calculate_predictions_recalibrated_type_1()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions_recalibrated_type_1.md)
and
[`MiceExtVal::calculate_predictions_recalibrated_type_2()`](https://metodologianavarrabiomed.github.io/mice-ext-val/reference/calculate_predictions_recalibrated_type_2.md)
documentation for more detailed information on how the recalibration
works).

``` r
men_model <- men_model |>
  MiceExtVal::calculate_predictions_recalibrated_type_1(data = men_fram_long) |>
  MiceExtVal::calculate_predictions_recalibrated_type_2(data = men_fram_long)

men_model |>
  MiceExtVal::get_calibration_plot_data_surv(
    data = men_fram_long,
    n_groups = 10,
    type = "predictions_recal_type_1"
  ) |>
  MiceExtVal::get_calibration_plot()

men_model |>
  MiceExtVal::get_calibration_plot_data_surv(
    data = men_fram_long,
    n_groups = 10,
    type = "predictions_recal_type_2"
  ) |>
  MiceExtVal::get_calibration_plot()
```

![Type 1 and type 2 calibration plots for men
](get-started-surv_files/figure-html/fig-framingham-cal-plot-recal-men-1.png)

\(a\) Type 1 recalibration

![Type 1 and type 2 calibration plots for men
](get-started-surv_files/figure-html/fig-framingham-cal-plot-recal-men-2.png)

\(b\) Type 2 recalibration

Figure 2: Framingham recalibrated calibration plots in men

``` r
women_model <- women_model |>
  MiceExtVal::calculate_predictions_recalibrated_type_1(data = women_fram_long) |>
  MiceExtVal::calculate_predictions_recalibrated_type_2(
    data = women_fram_long
  )

women_model |>
  MiceExtVal::get_calibration_plot_data_surv(
    data = women_fram_long,
    n_groups = 10,
    type = "predictions_recal_type_1"
  ) |>
  MiceExtVal::get_calibration_plot()

women_model |>
  MiceExtVal::get_calibration_plot_data_surv(
    data = women_fram_long,
    n_groups = 10,
    type = "predictions_recal_type_2"
  ) |>
  MiceExtVal::get_calibration_plot()
```

![Type 1 and type 2 calibration plots for women
](get-started-surv_files/figure-html/fig-framingham-cal-plot-recal-women-1.png)

\(a\) Type 1 recalibration

![Type 1 and type 2 calibration plots for women
](get-started-surv_files/figure-html/fig-framingham-cal-plot-recal-women-2.png)

\(b\) Type 2 recalibration

Figure 3: Framingham recalibrated calibration plots in women

Type 1 and type 2 recalibration are not consecutive, they are just two
different approaches for the same problem. As show in
[Figure 2](#fig-framingham-cal-plot-recal-men),
[Figure 3](#fig-framingham-cal-plot-recal-women) they obtain similar
results. With the recalibration it also changes the Brier score results
as shown in [Table 8](#tbl-brier-score-recal).

``` r
men_model <- men_model |>
  MiceExtVal::calculate_brier_score(
    data = men_fram_long, type = "predictions_recal_type_1", n_boot = 100
  ) |>
  MiceExtVal::calculate_brier_score(
    data = men_fram_long, type = "predictions_recal_type_2", n_boot = 100
  )

women_model <- women_model |>
  MiceExtVal::calculate_brier_score(
    data = women_fram_long, type = "predictions_recal_type_1", n_boot = 100
  ) |>
  MiceExtVal::calculate_brier_score(
    data = women_fram_long, type = "predictions_recal_type_2", n_boot = 100
  )

dplyr::bind_rows(men_model[["brier_score_type_1"]], women_model[["brier_score_type_1"]]) |>
  tibble::add_column(Model = c("Men", "Women"), .before = "Estimate") |>
  kableExtra::kbl(digits = 3)
dplyr::bind_rows(men_model[["brier_score_type_2"]], women_model[["brier_score_type_2"]]) |>
  tibble::add_column(Model = c("Men", "Women"), .before = "Estimate") |>
  kableExtra::kbl(digits = 3)
```

| Model | Estimate | 95% CI L | 95% CI U |
|:------|---------:|---------:|---------:|
| Men   |    0.211 |    0.205 |    0.218 |
| Women |    0.152 |    0.145 |    0.159 |

\(a\) Type 1 recalibration

| Model | Estimate | 95% CI L | 95% CI U |
|:------|---------:|---------:|---------:|
| Men   |    0.231 |    0.223 |    0.239 |
| Women |    0.152 |    0.144 |    0.160 |

\(b\) Type 2 recalibration

Table 8: Brier score results with recalibrated data

## Comparing results

External validations are normally performed to compare different model
performances among a certain cohort. The `MiceExtVal` package also
provides users with functions that allow to compare results of different
models.

#### Calibration Comparison: Stratified Calibration Plots

In order to compare two model calibration plots we have developed the
functions `MiceExtval::get_stratified_calibration_plot_surv()` and
`MiceExtval::get_stratified_calibration_plot_prop()` that allow the user
to plot stratified calibration plots. Accordingly with the individual
calibration plots we use the function
`MiceExtval::get_stratified_calibration_plot_surv()` to generate the
stratified calibration plot.

In our case it could be interesting to plot men and women calibrations
in the same plot, as shown in [Figure 4](#fig-stratified-cal-plot). The
function takes three arguments and a list of models as arguments.

- The external validation data (e.g., `fram_long`)
- The number of groups
- The type of prediction (either recalibrated or original predictions)
- A list of models to compare (in this case, `men_model` and
  `women_model`).

This will allow the function to overlay calibration plots for both
models on the same figure, facilitating direct comparison.

``` r
MiceExtVal::get_stratified_calibration_plot_surv(
  data = fram_long,
  n_groups = 10,
  type = "predictions_aggregated",
  Men = men_model,
  Women = women_model
)

MiceExtVal::get_stratified_calibration_plot_surv(
  data = fram_long,
  n_groups = 10,
  type = "predictions_recal_type_1",
  Men = men_model,
  Women = women_model
)

MiceExtVal::get_stratified_calibration_plot_surv(
  data = fram_long,
  n_groups = 10,
  type = "predictions_recal_type_2",
  Men = men_model,
  Women = women_model
)
```

![Stratified calibration plot
](get-started-surv_files/figure-html/fig-stratified-cal-plot-1.png)

\(a\) Original predictions

![Stratified calibration plot
](get-started-surv_files/figure-html/fig-stratified-cal-plot-2.png)

\(b\) Type 1 recalibration

![Stratified calibration plot
](get-started-surv_files/figure-html/fig-stratified-cal-plot-3.png)

\(c\) Type 2 recalibration

Figure 4: Framingham stratified by sex calibration plots

Note that we pass the entire imputed dataset (`fram_long`) as an
argument, even though each model only stores the results for its
respective subcohort (men or women). This is necessary because the
function needs access to the full dataset in order to correctly extract
and process the relevant subcohort for each model. The function then
uses the subcohort specified for each model (e.g., `men_model` for men)
to calculate the appropriate calibration data and generate the
corresponding calibration plots.

By passing the complete dataset, the function ensures that the
stratified calibration plots are generated correctly, comparing the
models’ calibration in the context of the full cohort’s distribution of
characteristics.

#### Discrimination Comparison: Forestplot

The `MiceExtVal` package also includes functionality for generating a
forest plot to compare the discrimination abilities of different models.
In this example, we will visualize the Harrell C-Index, but the package
also allows for the comparison of the AUC (Area Under the Curve) in
dichotomous outcomes.

The forest plot generation process is split into two steps:

1.  Data preparation: The first step generates the data that will be
    used in the forest plot.
2.  Forest plot creation: The second step generates the actual plot from
    the prepared data.

To compare different models, we can stratify the results by binding data
from different subgroups (e.g., men and women patients). Models that
share the same name across subgroups will be merged in the final forest
plot, making it easy to compare the performance of the same model in
different groups.

In the following example, we generate two separate strata for the same
model, `TIMI-II`, one for men and one for women, and then we pass these
strata to the forest plot function.

``` r
dplyr::bind_rows(
  MiceExtVal::get_forestplot_data(strat = "Men", type = "c_index", Framingham = men_model),
  MiceExtVal::get_forestplot_data(strat = "Women", type = "c_index", Framingham = women_model)
) |>
  MiceExtVal::get_forestplot(center = 0.7, digits = 2)
```

|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |                   |                   |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|-------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ![](data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnID8+CjxzdmcgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzcwOC42NnB0JyBoZWlnaHQ9JzIyLjY4cHQnIHZpZXdCb3g9JzAgMCA3MDguNjYgMjIuNjgnPgo8ZyBjbGFzcz0nc3ZnbGl0ZSc+CjxkZWZzPgogIDxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+PCFbQ0RBVEFbCiAgICAuc3ZnbGl0ZSBsaW5lLCAuc3ZnbGl0ZSBwb2x5bGluZSwgLnN2Z2xpdGUgcG9seWdvbiwgLnN2Z2xpdGUgcGF0aCwgLnN2Z2xpdGUgcmVjdCwgLnN2Z2xpdGUgY2lyY2xlIHsKICAgICAgZmlsbDogbm9uZTsKICAgICAgc3Ryb2tlOiAjMDAwMDAwOwogICAgICBzdHJva2UtbGluZWNhcDogcm91bmQ7CiAgICAgIHN0cm9rZS1saW5lam9pbjogcm91bmQ7CiAgICAgIHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsKICAgIH0KICAgIC5zdmdsaXRlIHRleHQgewogICAgICB3aGl0ZS1zcGFjZTogcHJlOwogICAgfQogICAgLnN2Z2xpdGUgZy5nbHlwaGdyb3VwIHBhdGggewogICAgICBmaWxsOiBpbmhlcml0OwogICAgICBzdHJva2U6IG5vbmU7CiAgICB9CiAgXV0+PC9zdHlsZT4KPC9kZWZzPgo8cmVjdCB3aWR0aD0nMTAwJScgaGVpZ2h0PScxMDAlJyBzdHlsZT0nc3Ryb2tlOiBub25lOyBmaWxsOiBub25lOycvPgo8ZGVmcz4KICA8Y2xpcFBhdGggaWQ9J2NwTUM0d01IdzNNRGd1TmpaOE1DNHdNSHd5TWk0Mk9BPT0nPgogICAgPHJlY3QgeD0nMC4wMCcgeT0nMC4wMCcgd2lkdGg9JzcwOC42NicgaGVpZ2h0PScyMi42OCcgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHczTURndU5qWjhNQzR3TUh3eU1pNDJPQT09KSc+CjxyZWN0IHg9JzAuMDAnIHk9Jy0wLjE0JyB3aWR0aD0nNzA4LjY2JyBoZWlnaHQ9JzIyLjk1JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjAwOyBzdHJva2U6IG5vbmU7JyAvPgo8L2c+CjxkZWZzPgogIDxjbGlwUGF0aCBpZD0nY3BNQzR3TUh3M01EZ3VOalo4TWpJdU5qaDhNakl1T0RFPSc+CiAgICA8cmVjdCB4PScwLjAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMCcgeT0nMjIuNjgnIHdpZHRoPSc3MDguNjYnIGhlaWdodD0nMC4xNCcgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHczTURndU5qWjhNakl1TmpoOE1qSXVPREU9KSc+CjxjaXJjbGUgY3g9JzM1NC4zMycgY3k9JzIyLjgxJyByPScwLjM1JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjcxOyBzdHJva2U6IG5vbmU7JyAvPgo8Y2lyY2xlIGN4PSczNTQuMzMnIGN5PScyMi44MScgcj0nMC4zNScgc3R5bGU9J3N0cm9rZS13aWR0aDogMC43MTsgc3Ryb2tlOiBub25lOycgLz4KPC9nPgo8ZyBjbGlwLXBhdGg9J3VybCgjY3BNQzR3TUh3M01EZ3VOalo4TUM0d01Id3lNaTQyT0E9PSknPgo8cmVjdCB4PSczMDIuNDcnIHk9Jy0wLjE0JyB3aWR0aD0nMTAzLjczJyBoZWlnaHQ9JzE3LjI4JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjAwOyBzdHJva2U6IG5vbmU7JyAvPgo8Y2lyY2xlIGN4PSczMTYuNTknIGN5PSc4LjUwJyByPSc0LjYyJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjcxOyBzdHJva2U6ICNGODc2NkQ7IGZpbGw6ICNGODc2NkQ7JyAvPgo8Y2lyY2xlIGN4PSczNjEuOTMnIGN5PSc4LjUwJyByPSc0LjYyJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjcxOyBzdHJva2U6ICMwMEJGQzQ7IGZpbGw6ICMwMEJGQzQ7JyAvPgo8dGV4dCB4PSczMzAuNzEnIHk9JzExLjUzJyBzdHlsZT0nZm9udC1zaXplOiA4LjgwcHg7IGZvbnQtZmFtaWx5OiAiTGliZXJhdGlvbiBTYW5zIjsnIHRleHRMZW5ndGg9JzE3LjExcHgnIGxlbmd0aEFkanVzdD0nc3BhY2luZ0FuZEdseXBocyc+TWVuPC90ZXh0Pgo8dGV4dCB4PSczNzYuMDUnIHk9JzExLjUzJyBzdHlsZT0nZm9udC1zaXplOiA4LjgwcHg7IGZvbnQtZmFtaWx5OiAiTGliZXJhdGlvbiBTYW5zIjsnIHRleHRMZW5ndGg9JzMwLjE0cHgnIGxlbmd0aEFkanVzdD0nc3BhY2luZ0FuZEdseXBocyc+V29tZW48L3RleHQ+CjwvZz4KPC9nPgo8L3N2Zz4K) |                   |                   |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| Model                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | Men (95% CI)      | Women (95% CI)    |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| Framingham                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | 0.69 (0.66, 0.71) | 0.71 (0.66, 0.75) | ![](data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnID8+CjxzdmcgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzQyNS4yMHB0JyBoZWlnaHQ9JzQ4LjE5cHQnIHZpZXdCb3g9JzAgMCA0MjUuMjAgNDguMTknPgo8ZyBjbGFzcz0nc3ZnbGl0ZSc+CjxkZWZzPgogIDxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+PCFbQ0RBVEFbCiAgICAuc3ZnbGl0ZSBsaW5lLCAuc3ZnbGl0ZSBwb2x5bGluZSwgLnN2Z2xpdGUgcG9seWdvbiwgLnN2Z2xpdGUgcGF0aCwgLnN2Z2xpdGUgcmVjdCwgLnN2Z2xpdGUgY2lyY2xlIHsKICAgICAgZmlsbDogbm9uZTsKICAgICAgc3Ryb2tlOiAjMDAwMDAwOwogICAgICBzdHJva2UtbGluZWNhcDogcm91bmQ7CiAgICAgIHN0cm9rZS1saW5lam9pbjogcm91bmQ7CiAgICAgIHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsKICAgIH0KICAgIC5zdmdsaXRlIHRleHQgewogICAgICB3aGl0ZS1zcGFjZTogcHJlOwogICAgfQogICAgLnN2Z2xpdGUgZy5nbHlwaGdyb3VwIHBhdGggewogICAgICBmaWxsOiBpbmhlcml0OwogICAgICBzdHJva2U6IG5vbmU7CiAgICB9CiAgXV0+PC9zdHlsZT4KPC9kZWZzPgo8cmVjdCB3aWR0aD0nMTAwJScgaGVpZ2h0PScxMDAlJyBzdHlsZT0nc3Ryb2tlOiBub25lOyBmaWxsOiBub25lOycvPgo8ZGVmcz4KICA8Y2xpcFBhdGggaWQ9J2NwTUM0d01IdzBNalV1TWpCOE1DNHdNSHcwT0M0eE9RPT0nPgogICAgPHJlY3QgeD0nMC4wMCcgeT0nMC4wMCcgd2lkdGg9JzQyNS4yMCcgaGVpZ2h0PSc0OC4xOScgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHcwTWpVdU1qQjhNQzR3TUh3ME9DNHhPUT09KSc+CjxyZWN0IHg9JzAuMDAnIHk9JzAuMDAnIHdpZHRoPSc0MjUuMjAnIGhlaWdodD0nNDguMTknIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogbm9uZTsnIC8+CjwvZz4KPGRlZnM+CiAgPGNsaXBQYXRoIGlkPSdjcE5TNDBPSHcwTVRrdU56SjhOUzQwT0h3ME1pNDNNUT09Jz4KICAgIDxyZWN0IHg9JzUuNDgnIHk9JzUuNDgnIHdpZHRoPSc0MTQuMjQnIGhlaWdodD0nMzcuMjMnIC8+CiAgPC9jbGlwUGF0aD4KPC9kZWZzPgo8ZyBjbGlwLXBhdGg9J3VybCgjY3BOUzQwT0h3ME1Ua3VOeko4TlM0ME9IdzBNaTQzTVE9PSknPgo8cmVjdCB4PSc1LjQ4JyB5PSc1LjQ4JyB3aWR0aD0nNDE0LjI0JyBoZWlnaHQ9JzM3LjIzJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6IG5vbmU7JyAvPgo8cG9seWxpbmUgcG9pbnRzPSc3Mi4xMiw0Mi43MSA3Mi4xMiw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC41Mzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzE1My4yNyw0Mi43MSAxNTMuMjcsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDAuNTM7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPScyMzQuNDIsNDIuNzEgMjM0LjQyLDUuNDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICM4QzhDOEM7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nMzE1LjU3LDQyLjcxIDMxNS41Nyw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC41Mzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzM5Ni43MSw0Mi43MSAzOTYuNzEsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDAuNTM7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPSczMS41NSw0Mi43MSAzMS41NSw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzExMi43MCw0Mi43MSAxMTIuNzAsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPScxOTMuODUsNDIuNzEgMTkzLjg1LDUuNDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6ICM4QzhDOEM7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nMjc0Ljk5LDQyLjcxIDI3NC45OSw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzM1Ni4xNCw0Mi43MSAzNTYuMTQsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8bGluZSB4MT0nNjcuMjInIHkxPSczMi41NicgeDI9JzIzMi42NCcgeTI9JzMyLjU2JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6ICNGODc2NkQ7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPGxpbmUgeDE9Jzc4LjI0JyB5MT0nMTUuNjMnIHgyPSczNTIuMjgnIHkyPScxNS42Mycgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjMDBCRkM0OyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+CjxjaXJjbGUgY3g9JzE0OS45MycgY3k9JzMyLjU2JyByPScyLjg0JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjQyOyBzdHJva2U6ICNGODc2NkQ7IGZpbGw6ICNGODc2NkQ7JyAvPgo8Y2lyY2xlIGN4PScyMTUuMjYnIGN5PScxNS42Mycgcj0nMi44NCcgc3R5bGU9J3N0cm9rZS13aWR0aDogMS40Mjsgc3Ryb2tlOiAjMDBCRkM0OyBmaWxsOiAjMDBCRkM0OycgLz4KPGxpbmUgeDE9JzE5My44NScgeTE9JzQyLjcxJyB4Mj0nMTkzLjg1JyB5Mj0nNS40OCcgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlLWRhc2hhcnJheTogNS42OSw1LjY5OyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+CjwvZz4KPGcgY2xpcC1wYXRoPSd1cmwoI2NwTUM0d01IdzBNalV1TWpCOE1DNHdNSHcwT0M0eE9RPT0pJz4KPC9nPgo8L2c+Cjwvc3ZnPgo=) |
|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |                   |                   | ![](data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnID8+CjxzdmcgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzYzNy44MHB0JyBoZWlnaHQ9JzE5Ljg0cHQnIHZpZXdCb3g9JzAgMCA2MzcuODAgMTkuODQnPgo8ZyBjbGFzcz0nc3ZnbGl0ZSc+CjxkZWZzPgogIDxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+PCFbQ0RBVEFbCiAgICAuc3ZnbGl0ZSBsaW5lLCAuc3ZnbGl0ZSBwb2x5bGluZSwgLnN2Z2xpdGUgcG9seWdvbiwgLnN2Z2xpdGUgcGF0aCwgLnN2Z2xpdGUgcmVjdCwgLnN2Z2xpdGUgY2lyY2xlIHsKICAgICAgZmlsbDogbm9uZTsKICAgICAgc3Ryb2tlOiAjMDAwMDAwOwogICAgICBzdHJva2UtbGluZWNhcDogcm91bmQ7CiAgICAgIHN0cm9rZS1saW5lam9pbjogcm91bmQ7CiAgICAgIHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsKICAgIH0KICAgIC5zdmdsaXRlIHRleHQgewogICAgICB3aGl0ZS1zcGFjZTogcHJlOwogICAgfQogICAgLnN2Z2xpdGUgZy5nbHlwaGdyb3VwIHBhdGggewogICAgICBmaWxsOiBpbmhlcml0OwogICAgICBzdHJva2U6IG5vbmU7CiAgICB9CiAgXV0+PC9zdHlsZT4KPC9kZWZzPgo8cmVjdCB3aWR0aD0nMTAwJScgaGVpZ2h0PScxMDAlJyBzdHlsZT0nc3Ryb2tlOiBub25lOyBmaWxsOiBub25lOycvPgo8ZGVmcz4KICA8Y2xpcFBhdGggaWQ9J2NwTUM0d01IdzJNemN1T0RCOE1DNHdNSHd4T1M0NE5BPT0nPgogICAgPHJlY3QgeD0nMC4wMCcgeT0nMC4wMCcgd2lkdGg9JzYzNy44MCcgaGVpZ2h0PScxOS44NCcgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHcyTXpjdU9EQjhNQzR3TUh3eE9TNDROQT09KSc+CjxyZWN0IHg9JzAuMDAnIHk9Jy0zLjQwJyB3aWR0aD0nNjM3LjgwJyBoZWlnaHQ9JzI2LjY0JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6IG5vbmU7JyAvPgo8L2c+CjxkZWZzPgogIDxjbGlwUGF0aCBpZD0nY3BOUzQwT0h3Mk16SXVNeko4TWk0d09Id3lMakE0Jz4KICAgIDxyZWN0IHg9JzUuNDgnIHk9JzIuMDgnIHdpZHRoPSc2MjYuODQnIGhlaWdodD0nMC4wMCcgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE5TNDBPSHcyTXpJdU16SjhNaTR3T0h3eUxqQTQpJz4KPHJlY3QgeD0nNS40OCcgeT0nMi4wOCcgd2lkdGg9JzYyNi44NCcgaGVpZ2h0PScwLjAwJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6IG5vbmU7JyAvPgo8cG9seWxpbmUgcG9pbnRzPSc1LjQ4LDIuMDggNjMyLjMyLDIuMDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICNFQkVCRUI7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nNS40OCwyLjA4IDYzMi4zMiwyLjA4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC41Mzsgc3Ryb2tlOiAjRUJFQkVCOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzUuNDgsMi4wOCA2MzIuMzIsMi4wOCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDAuNTM7IHN0cm9rZTogI0VCRUJFQjsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPSc1LjQ4LDIuMDggNjMyLjMyLDIuMDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICNFQkVCRUI7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPC9nPgo8ZyBjbGlwLXBhdGg9J3VybCgjY3BNQzR3TUh3Mk16Y3VPREI4TUM0d01Id3hPUzQ0TkE9PSknPgo8dGV4dCB4PSc0NC45MycgeT0nMTYuNTInIHRleHQtYW5jaG9yPSdtaWRkbGUnIHN0eWxlPSdmb250LXNpemU6IDEyLjAwcHg7ZmlsbDogIzRENEQ0RDsgZm9udC1mYW1pbHk6ICJMaWJlcmF0aW9uIFNhbnMiOycgdGV4dExlbmd0aD0nMzAuMDJweCcgbGVuZ3RoQWRqdXN0PSdzcGFjaW5nQW5kR2x5cGhzJz4wLjY1MDwvdGV4dD4KPHRleHQgeD0nMTY3LjcyJyB5PScxNi41MicgdGV4dC1hbmNob3I9J21pZGRsZScgc3R5bGU9J2ZvbnQtc2l6ZTogMTIuMDBweDtmaWxsOiAjNEQ0RDREOyBmb250LWZhbWlseTogIkxpYmVyYXRpb24gU2FucyI7JyB0ZXh0TGVuZ3RoPSczMC4wMnB4JyBsZW5ndGhBZGp1c3Q9J3NwYWNpbmdBbmRHbHlwaHMnPjAuNjc1PC90ZXh0Pgo8dGV4dCB4PScyOTAuNTInIHk9JzE2LjUyJyB0ZXh0LWFuY2hvcj0nbWlkZGxlJyBzdHlsZT0nZm9udC1zaXplOiAxMi4wMHB4O2ZpbGw6ICM0RDRENEQ7IGZvbnQtZmFtaWx5OiAiTGliZXJhdGlvbiBTYW5zIjsnIHRleHRMZW5ndGg9JzMwLjAycHgnIGxlbmd0aEFkanVzdD0nc3BhY2luZ0FuZEdseXBocyc+MC43MDA8L3RleHQ+Cjx0ZXh0IHg9JzQxMy4zMScgeT0nMTYuNTInIHRleHQtYW5jaG9yPSdtaWRkbGUnIHN0eWxlPSdmb250LXNpemU6IDEyLjAwcHg7ZmlsbDogIzRENEQ0RDsgZm9udC1mYW1pbHk6ICJMaWJlcmF0aW9uIFNhbnMiOycgdGV4dExlbmd0aD0nMzAuMDJweCcgbGVuZ3RoQWRqdXN0PSdzcGFjaW5nQW5kR2x5cGhzJz4wLjcyNTwvdGV4dD4KPHRleHQgeD0nNTM2LjExJyB5PScxNi41MicgdGV4dC1hbmNob3I9J21pZGRsZScgc3R5bGU9J2ZvbnQtc2l6ZTogMTIuMDBweDtmaWxsOiAjNEQ0RDREOyBmb250LWZhbWlseTogIkxpYmVyYXRpb24gU2FucyI7JyB0ZXh0TGVuZ3RoPSczMC4wMnB4JyBsZW5ndGhBZGp1c3Q9J3NwYWNpbmdBbmRHbHlwaHMnPjAuNzUwPC90ZXh0Pgo8L2c+CjwvZz4KPC9zdmc+Cg==)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |

Figure 5: Forestplot stratified

If we want to generate a forestplot with multiple models in one strat we
can generate it as the following example. In this example men and women
models are inserted in the `Overall` strat as individual models. We have
deleted the legend as it is not informative.

``` r
MiceExtVal::get_forestplot_data(
  strat = "Overall",
  type = "c_index",
  `Framingham Women` = women_model,
  `Framingham Men` = men_model
) |>
  MiceExtVal::get_forestplot(center = 0.7, digits = 2)
```

|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |                   |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ![](data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnID8+CjxzdmcgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzcwOC42NnB0JyBoZWlnaHQ9JzIyLjY4cHQnIHZpZXdCb3g9JzAgMCA3MDguNjYgMjIuNjgnPgo8ZyBjbGFzcz0nc3ZnbGl0ZSc+CjxkZWZzPgogIDxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+PCFbQ0RBVEFbCiAgICAuc3ZnbGl0ZSBsaW5lLCAuc3ZnbGl0ZSBwb2x5bGluZSwgLnN2Z2xpdGUgcG9seWdvbiwgLnN2Z2xpdGUgcGF0aCwgLnN2Z2xpdGUgcmVjdCwgLnN2Z2xpdGUgY2lyY2xlIHsKICAgICAgZmlsbDogbm9uZTsKICAgICAgc3Ryb2tlOiAjMDAwMDAwOwogICAgICBzdHJva2UtbGluZWNhcDogcm91bmQ7CiAgICAgIHN0cm9rZS1saW5lam9pbjogcm91bmQ7CiAgICAgIHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsKICAgIH0KICAgIC5zdmdsaXRlIHRleHQgewogICAgICB3aGl0ZS1zcGFjZTogcHJlOwogICAgfQogICAgLnN2Z2xpdGUgZy5nbHlwaGdyb3VwIHBhdGggewogICAgICBmaWxsOiBpbmhlcml0OwogICAgICBzdHJva2U6IG5vbmU7CiAgICB9CiAgXV0+PC9zdHlsZT4KPC9kZWZzPgo8cmVjdCB3aWR0aD0nMTAwJScgaGVpZ2h0PScxMDAlJyBzdHlsZT0nc3Ryb2tlOiBub25lOyBmaWxsOiBub25lOycvPgo8ZGVmcz4KICA8Y2xpcFBhdGggaWQ9J2NwTUM0d01IdzNNRGd1TmpaOE1DNHdNSHd5TWk0Mk9BPT0nPgogICAgPHJlY3QgeD0nMC4wMCcgeT0nMC4wMCcgd2lkdGg9JzcwOC42NicgaGVpZ2h0PScyMi42OCcgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHczTURndU5qWjhNQzR3TUh3eU1pNDJPQT09KSc+CjxyZWN0IHg9JzAuMDAnIHk9Jy0wLjE0JyB3aWR0aD0nNzA4LjY2JyBoZWlnaHQ9JzIyLjk1JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjAwOyBzdHJva2U6IG5vbmU7JyAvPgo8L2c+CjxkZWZzPgogIDxjbGlwUGF0aCBpZD0nY3BNQzR3TUh3M01EZ3VOalo4TWpJdU5qaDhNakl1T0RFPSc+CiAgICA8cmVjdCB4PScwLjAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMCcgeT0nMjIuNjgnIHdpZHRoPSc3MDguNjYnIGhlaWdodD0nMC4xNCcgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHczTURndU5qWjhNakl1TmpoOE1qSXVPREU9KSc+CjxjaXJjbGUgY3g9JzM1NC4zMycgY3k9JzIyLjgxJyByPScwLjM1JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjcxOyBzdHJva2U6IG5vbmU7JyAvPgo8L2c+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHczTURndU5qWjhNQzR3TUh3eU1pNDJPQT09KSc+CjxyZWN0IHg9JzMyNi4yOCcgeT0nLTAuMTQnIHdpZHRoPSc1Ni4xMCcgaGVpZ2h0PScxNy4yOCcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC4wMDsgc3Ryb2tlOiBub25lOycgLz4KPGNpcmNsZSBjeD0nMzQwLjQwJyBjeT0nOC41MCcgcj0nNC42Micgc3R5bGU9J3N0cm9rZS13aWR0aDogMC43MTsgc3Ryb2tlOiAjRjg3NjZEOyBmaWxsOiAjRjg3NjZEOycgLz4KPHRleHQgeD0nMzU0LjUyJyB5PScxMS41Mycgc3R5bGU9J2ZvbnQtc2l6ZTogOC44MHB4OyBmb250LWZhbWlseTogIkxpYmVyYXRpb24gU2FucyI7JyB0ZXh0TGVuZ3RoPScyNy44NnB4JyBsZW5ndGhBZGp1c3Q9J3NwYWNpbmdBbmRHbHlwaHMnPk92ZXJhbGw8L3RleHQ+CjwvZz4KPC9nPgo8L3N2Zz4K) |                   |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| Model                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | Overall (95% CI)  |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| Framingham Women                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | 0.71 (0.66, 0.75) | ![](data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnID8+CjxzdmcgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzQyNS4yMHB0JyBoZWlnaHQ9JzQ4LjE5cHQnIHZpZXdCb3g9JzAgMCA0MjUuMjAgNDguMTknPgo8ZyBjbGFzcz0nc3ZnbGl0ZSc+CjxkZWZzPgogIDxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+PCFbQ0RBVEFbCiAgICAuc3ZnbGl0ZSBsaW5lLCAuc3ZnbGl0ZSBwb2x5bGluZSwgLnN2Z2xpdGUgcG9seWdvbiwgLnN2Z2xpdGUgcGF0aCwgLnN2Z2xpdGUgcmVjdCwgLnN2Z2xpdGUgY2lyY2xlIHsKICAgICAgZmlsbDogbm9uZTsKICAgICAgc3Ryb2tlOiAjMDAwMDAwOwogICAgICBzdHJva2UtbGluZWNhcDogcm91bmQ7CiAgICAgIHN0cm9rZS1saW5lam9pbjogcm91bmQ7CiAgICAgIHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsKICAgIH0KICAgIC5zdmdsaXRlIHRleHQgewogICAgICB3aGl0ZS1zcGFjZTogcHJlOwogICAgfQogICAgLnN2Z2xpdGUgZy5nbHlwaGdyb3VwIHBhdGggewogICAgICBmaWxsOiBpbmhlcml0OwogICAgICBzdHJva2U6IG5vbmU7CiAgICB9CiAgXV0+PC9zdHlsZT4KPC9kZWZzPgo8cmVjdCB3aWR0aD0nMTAwJScgaGVpZ2h0PScxMDAlJyBzdHlsZT0nc3Ryb2tlOiBub25lOyBmaWxsOiBub25lOycvPgo8ZGVmcz4KICA8Y2xpcFBhdGggaWQ9J2NwTUM0d01IdzBNalV1TWpCOE1DNHdNSHcwT0M0eE9RPT0nPgogICAgPHJlY3QgeD0nMC4wMCcgeT0nMC4wMCcgd2lkdGg9JzQyNS4yMCcgaGVpZ2h0PSc0OC4xOScgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHcwTWpVdU1qQjhNQzR3TUh3ME9DNHhPUT09KSc+CjxyZWN0IHg9JzAuMDAnIHk9JzAuMDAnIHdpZHRoPSc0MjUuMjAnIGhlaWdodD0nNDguMTknIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogbm9uZTsnIC8+CjwvZz4KPGRlZnM+CiAgPGNsaXBQYXRoIGlkPSdjcE5TNDBPSHcwTVRrdU56SjhOUzQwT0h3ME1pNDNNUT09Jz4KICAgIDxyZWN0IHg9JzUuNDgnIHk9JzUuNDgnIHdpZHRoPSc0MTQuMjQnIGhlaWdodD0nMzcuMjMnIC8+CiAgPC9jbGlwUGF0aD4KPC9kZWZzPgo8ZyBjbGlwLXBhdGg9J3VybCgjY3BOUzQwT0h3ME1Ua3VOeko4TlM0ME9IdzBNaTQzTVE9PSknPgo8cmVjdCB4PSc1LjQ4JyB5PSc1LjQ4JyB3aWR0aD0nNDE0LjI0JyBoZWlnaHQ9JzM3LjIzJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6IG5vbmU7JyAvPgo8cG9seWxpbmUgcG9pbnRzPSc3Mi4xMiw0Mi43MSA3Mi4xMiw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC41Mzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzE1My4yNyw0Mi43MSAxNTMuMjcsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDAuNTM7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPScyMzQuNDIsNDIuNzEgMjM0LjQyLDUuNDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICM4QzhDOEM7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nMzE1LjU3LDQyLjcxIDMxNS41Nyw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC41Mzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzM5Ni43MSw0Mi43MSAzOTYuNzEsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDAuNTM7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPSczMS41NSw0Mi43MSAzMS41NSw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzExMi43MCw0Mi43MSAxMTIuNzAsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPScxOTMuODUsNDIuNzEgMTkzLjg1LDUuNDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6ICM4QzhDOEM7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nMjc0Ljk5LDQyLjcxIDI3NC45OSw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzM1Ni4xNCw0Mi43MSAzNTYuMTQsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8bGluZSB4MT0nNjcuMjInIHkxPScyNC4wOScgeDI9JzIzMi42NCcgeTI9JzI0LjA5JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6ICNGODc2NkQ7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPGNpcmNsZSBjeD0nMTQ5LjkzJyBjeT0nMjQuMDknIHI9JzIuODQnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuNDI7IHN0cm9rZTogI0Y4NzY2RDsgZmlsbDogI0Y4NzY2RDsnIC8+CjxsaW5lIHgxPScxOTMuODUnIHkxPSc0Mi43MScgeDI9JzE5My44NScgeTI9JzUuNDgnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZS1kYXNoYXJyYXk6IDUuNjksNS42OTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8L2c+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHcwTWpVdU1qQjhNQzR3TUh3ME9DNHhPUT09KSc+CjwvZz4KPC9nPgo8L3N2Zz4K) |
| Framingham Men                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | 0.69 (0.66, 0.71) | ![](data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnID8+CjxzdmcgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzQyNS4yMHB0JyBoZWlnaHQ9JzQ4LjE5cHQnIHZpZXdCb3g9JzAgMCA0MjUuMjAgNDguMTknPgo8ZyBjbGFzcz0nc3ZnbGl0ZSc+CjxkZWZzPgogIDxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+PCFbQ0RBVEFbCiAgICAuc3ZnbGl0ZSBsaW5lLCAuc3ZnbGl0ZSBwb2x5bGluZSwgLnN2Z2xpdGUgcG9seWdvbiwgLnN2Z2xpdGUgcGF0aCwgLnN2Z2xpdGUgcmVjdCwgLnN2Z2xpdGUgY2lyY2xlIHsKICAgICAgZmlsbDogbm9uZTsKICAgICAgc3Ryb2tlOiAjMDAwMDAwOwogICAgICBzdHJva2UtbGluZWNhcDogcm91bmQ7CiAgICAgIHN0cm9rZS1saW5lam9pbjogcm91bmQ7CiAgICAgIHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsKICAgIH0KICAgIC5zdmdsaXRlIHRleHQgewogICAgICB3aGl0ZS1zcGFjZTogcHJlOwogICAgfQogICAgLnN2Z2xpdGUgZy5nbHlwaGdyb3VwIHBhdGggewogICAgICBmaWxsOiBpbmhlcml0OwogICAgICBzdHJva2U6IG5vbmU7CiAgICB9CiAgXV0+PC9zdHlsZT4KPC9kZWZzPgo8cmVjdCB3aWR0aD0nMTAwJScgaGVpZ2h0PScxMDAlJyBzdHlsZT0nc3Ryb2tlOiBub25lOyBmaWxsOiBub25lOycvPgo8ZGVmcz4KICA8Y2xpcFBhdGggaWQ9J2NwTUM0d01IdzBNalV1TWpCOE1DNHdNSHcwT0M0eE9RPT0nPgogICAgPHJlY3QgeD0nMC4wMCcgeT0nMC4wMCcgd2lkdGg9JzQyNS4yMCcgaGVpZ2h0PSc0OC4xOScgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHcwTWpVdU1qQjhNQzR3TUh3ME9DNHhPUT09KSc+CjxyZWN0IHg9JzAuMDAnIHk9JzAuMDAnIHdpZHRoPSc0MjUuMjAnIGhlaWdodD0nNDguMTknIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogbm9uZTsnIC8+CjwvZz4KPGRlZnM+CiAgPGNsaXBQYXRoIGlkPSdjcE5TNDBPSHcwTVRrdU56SjhOUzQwT0h3ME1pNDNNUT09Jz4KICAgIDxyZWN0IHg9JzUuNDgnIHk9JzUuNDgnIHdpZHRoPSc0MTQuMjQnIGhlaWdodD0nMzcuMjMnIC8+CiAgPC9jbGlwUGF0aD4KPC9kZWZzPgo8ZyBjbGlwLXBhdGg9J3VybCgjY3BOUzQwT0h3ME1Ua3VOeko4TlM0ME9IdzBNaTQzTVE9PSknPgo8cmVjdCB4PSc1LjQ4JyB5PSc1LjQ4JyB3aWR0aD0nNDE0LjI0JyBoZWlnaHQ9JzM3LjIzJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6IG5vbmU7JyAvPgo8cG9seWxpbmUgcG9pbnRzPSc3Mi4xMiw0Mi43MSA3Mi4xMiw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC41Mzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzE1My4yNyw0Mi43MSAxNTMuMjcsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDAuNTM7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPScyMzQuNDIsNDIuNzEgMjM0LjQyLDUuNDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICM4QzhDOEM7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nMzE1LjU3LDQyLjcxIDMxNS41Nyw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC41Mzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzM5Ni43MSw0Mi43MSAzOTYuNzEsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDAuNTM7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPSczMS41NSw0Mi43MSAzMS41NSw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzExMi43MCw0Mi43MSAxMTIuNzAsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPScxOTMuODUsNDIuNzEgMTkzLjg1LDUuNDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6ICM4QzhDOEM7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nMjc0Ljk5LDQyLjcxIDI3NC45OSw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzM1Ni4xNCw0Mi43MSAzNTYuMTQsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8bGluZSB4MT0nNzguMjQnIHkxPScyNC4wOScgeDI9JzM1Mi4yOCcgeTI9JzI0LjA5JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6ICNGODc2NkQ7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPGNpcmNsZSBjeD0nMjE1LjI2JyBjeT0nMjQuMDknIHI9JzIuODQnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuNDI7IHN0cm9rZTogI0Y4NzY2RDsgZmlsbDogI0Y4NzY2RDsnIC8+CjxsaW5lIHgxPScxOTMuODUnIHkxPSc0Mi43MScgeDI9JzE5My44NScgeTI9JzUuNDgnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZS1kYXNoYXJyYXk6IDUuNjksNS42OTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8L2c+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHcwTWpVdU1qQjhNQzR3TUh3ME9DNHhPUT09KSc+CjwvZz4KPC9nPgo8L3N2Zz4K) |
|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |                   | ![](data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnID8+CjxzdmcgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzYzNy44MHB0JyBoZWlnaHQ9JzE5Ljg0cHQnIHZpZXdCb3g9JzAgMCA2MzcuODAgMTkuODQnPgo8ZyBjbGFzcz0nc3ZnbGl0ZSc+CjxkZWZzPgogIDxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+PCFbQ0RBVEFbCiAgICAuc3ZnbGl0ZSBsaW5lLCAuc3ZnbGl0ZSBwb2x5bGluZSwgLnN2Z2xpdGUgcG9seWdvbiwgLnN2Z2xpdGUgcGF0aCwgLnN2Z2xpdGUgcmVjdCwgLnN2Z2xpdGUgY2lyY2xlIHsKICAgICAgZmlsbDogbm9uZTsKICAgICAgc3Ryb2tlOiAjMDAwMDAwOwogICAgICBzdHJva2UtbGluZWNhcDogcm91bmQ7CiAgICAgIHN0cm9rZS1saW5lam9pbjogcm91bmQ7CiAgICAgIHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsKICAgIH0KICAgIC5zdmdsaXRlIHRleHQgewogICAgICB3aGl0ZS1zcGFjZTogcHJlOwogICAgfQogICAgLnN2Z2xpdGUgZy5nbHlwaGdyb3VwIHBhdGggewogICAgICBmaWxsOiBpbmhlcml0OwogICAgICBzdHJva2U6IG5vbmU7CiAgICB9CiAgXV0+PC9zdHlsZT4KPC9kZWZzPgo8cmVjdCB3aWR0aD0nMTAwJScgaGVpZ2h0PScxMDAlJyBzdHlsZT0nc3Ryb2tlOiBub25lOyBmaWxsOiBub25lOycvPgo8ZGVmcz4KICA8Y2xpcFBhdGggaWQ9J2NwTUM0d01IdzJNemN1T0RCOE1DNHdNSHd4T1M0NE5BPT0nPgogICAgPHJlY3QgeD0nMC4wMCcgeT0nMC4wMCcgd2lkdGg9JzYzNy44MCcgaGVpZ2h0PScxOS44NCcgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHcyTXpjdU9EQjhNQzR3TUh3eE9TNDROQT09KSc+CjxyZWN0IHg9JzAuMDAnIHk9Jy0zLjQwJyB3aWR0aD0nNjM3LjgwJyBoZWlnaHQ9JzI2LjY0JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6IG5vbmU7JyAvPgo8L2c+CjxkZWZzPgogIDxjbGlwUGF0aCBpZD0nY3BOUzQwT0h3Mk16SXVNeko4TWk0d09Id3lMakE0Jz4KICAgIDxyZWN0IHg9JzUuNDgnIHk9JzIuMDgnIHdpZHRoPSc2MjYuODQnIGhlaWdodD0nMC4wMCcgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE5TNDBPSHcyTXpJdU16SjhNaTR3T0h3eUxqQTQpJz4KPHJlY3QgeD0nNS40OCcgeT0nMi4wOCcgd2lkdGg9JzYyNi44NCcgaGVpZ2h0PScwLjAwJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6IG5vbmU7JyAvPgo8cG9seWxpbmUgcG9pbnRzPSc1LjQ4LDIuMDggNjMyLjMyLDIuMDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICNFQkVCRUI7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nNS40OCwyLjA4IDYzMi4zMiwyLjA4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC41Mzsgc3Ryb2tlOiAjRUJFQkVCOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzUuNDgsMi4wOCA2MzIuMzIsMi4wOCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDAuNTM7IHN0cm9rZTogI0VCRUJFQjsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPSc1LjQ4LDIuMDggNjMyLjMyLDIuMDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICNFQkVCRUI7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPC9nPgo8ZyBjbGlwLXBhdGg9J3VybCgjY3BNQzR3TUh3Mk16Y3VPREI4TUM0d01Id3hPUzQ0TkE9PSknPgo8dGV4dCB4PSc0NC45MycgeT0nMTYuNTInIHRleHQtYW5jaG9yPSdtaWRkbGUnIHN0eWxlPSdmb250LXNpemU6IDEyLjAwcHg7ZmlsbDogIzRENEQ0RDsgZm9udC1mYW1pbHk6ICJMaWJlcmF0aW9uIFNhbnMiOycgdGV4dExlbmd0aD0nMzAuMDJweCcgbGVuZ3RoQWRqdXN0PSdzcGFjaW5nQW5kR2x5cGhzJz4wLjY1MDwvdGV4dD4KPHRleHQgeD0nMTY3LjcyJyB5PScxNi41MicgdGV4dC1hbmNob3I9J21pZGRsZScgc3R5bGU9J2ZvbnQtc2l6ZTogMTIuMDBweDtmaWxsOiAjNEQ0RDREOyBmb250LWZhbWlseTogIkxpYmVyYXRpb24gU2FucyI7JyB0ZXh0TGVuZ3RoPSczMC4wMnB4JyBsZW5ndGhBZGp1c3Q9J3NwYWNpbmdBbmRHbHlwaHMnPjAuNjc1PC90ZXh0Pgo8dGV4dCB4PScyOTAuNTInIHk9JzE2LjUyJyB0ZXh0LWFuY2hvcj0nbWlkZGxlJyBzdHlsZT0nZm9udC1zaXplOiAxMi4wMHB4O2ZpbGw6ICM0RDRENEQ7IGZvbnQtZmFtaWx5OiAiTGliZXJhdGlvbiBTYW5zIjsnIHRleHRMZW5ndGg9JzMwLjAycHgnIGxlbmd0aEFkanVzdD0nc3BhY2luZ0FuZEdseXBocyc+MC43MDA8L3RleHQ+Cjx0ZXh0IHg9JzQxMy4zMScgeT0nMTYuNTInIHRleHQtYW5jaG9yPSdtaWRkbGUnIHN0eWxlPSdmb250LXNpemU6IDEyLjAwcHg7ZmlsbDogIzRENEQ0RDsgZm9udC1mYW1pbHk6ICJMaWJlcmF0aW9uIFNhbnMiOycgdGV4dExlbmd0aD0nMzAuMDJweCcgbGVuZ3RoQWRqdXN0PSdzcGFjaW5nQW5kR2x5cGhzJz4wLjcyNTwvdGV4dD4KPHRleHQgeD0nNTM2LjExJyB5PScxNi41MicgdGV4dC1hbmNob3I9J21pZGRsZScgc3R5bGU9J2ZvbnQtc2l6ZTogMTIuMDBweDtmaWxsOiAjNEQ0RDREOyBmb250LWZhbWlseTogIkxpYmVyYXRpb24gU2FucyI7JyB0ZXh0TGVuZ3RoPSczMC4wMnB4JyBsZW5ndGhBZGp1c3Q9J3NwYWNpbmdBbmRHbHlwaHMnPjAuNzUwPC90ZXh0Pgo8L2c+CjwvZz4KPC9zdmc+Cg==)                                                                                                                                                                     |

Figure 6: Forestplot non-stratified

#### Brier Score Comparison: Forestplot

The forestplot functions also allow us to generate the data from the
Brier score results. The next code snippet generates a forestplot that
compares the different Brier score in each model.

``` r
data <- dplyr::bind_rows(
  MiceExtVal::get_forestplot_data(
    strat = "Original", type = "brier_score",
    Men = men_model, Women = women_model
  ),
  MiceExtVal::get_forestplot_data(
    strat = "Type 1 recalibration", type = "brier_score_type_1",
    Men = men_model, Women = women_model
  ),
  MiceExtVal::get_forestplot_data(
    strat = "Type 2 recalibration", type = "brier_score_type_2",
    Men = men_model, Women = women_model
  ),
)

data |>
  MiceExtVal::get_forestplot(
    center = mean(data[["estimate"]]),
    digits = 2,
    axis.text.x = ggplot2::element_text(size = 25)
  )
```

|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |                   |                               |                               |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------|-------------------------------|-------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ![](data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnID8+CjxzdmcgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzcwOC42NnB0JyBoZWlnaHQ9JzIyLjY4cHQnIHZpZXdCb3g9JzAgMCA3MDguNjYgMjIuNjgnPgo8ZyBjbGFzcz0nc3ZnbGl0ZSc+CjxkZWZzPgogIDxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+PCFbQ0RBVEFbCiAgICAuc3ZnbGl0ZSBsaW5lLCAuc3ZnbGl0ZSBwb2x5bGluZSwgLnN2Z2xpdGUgcG9seWdvbiwgLnN2Z2xpdGUgcGF0aCwgLnN2Z2xpdGUgcmVjdCwgLnN2Z2xpdGUgY2lyY2xlIHsKICAgICAgZmlsbDogbm9uZTsKICAgICAgc3Ryb2tlOiAjMDAwMDAwOwogICAgICBzdHJva2UtbGluZWNhcDogcm91bmQ7CiAgICAgIHN0cm9rZS1saW5lam9pbjogcm91bmQ7CiAgICAgIHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsKICAgIH0KICAgIC5zdmdsaXRlIHRleHQgewogICAgICB3aGl0ZS1zcGFjZTogcHJlOwogICAgfQogICAgLnN2Z2xpdGUgZy5nbHlwaGdyb3VwIHBhdGggewogICAgICBmaWxsOiBpbmhlcml0OwogICAgICBzdHJva2U6IG5vbmU7CiAgICB9CiAgXV0+PC9zdHlsZT4KPC9kZWZzPgo8cmVjdCB3aWR0aD0nMTAwJScgaGVpZ2h0PScxMDAlJyBzdHlsZT0nc3Ryb2tlOiBub25lOyBmaWxsOiBub25lOycvPgo8ZGVmcz4KICA8Y2xpcFBhdGggaWQ9J2NwTUM0d01IdzNNRGd1TmpaOE1DNHdNSHd5TWk0Mk9BPT0nPgogICAgPHJlY3QgeD0nMC4wMCcgeT0nMC4wMCcgd2lkdGg9JzcwOC42NicgaGVpZ2h0PScyMi42OCcgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHczTURndU5qWjhNQzR3TUh3eU1pNDJPQT09KSc+CjxyZWN0IHg9JzAuMDAnIHk9Jy0wLjE0JyB3aWR0aD0nNzA4LjY2JyBoZWlnaHQ9JzIyLjk1JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjAwOyBzdHJva2U6IG5vbmU7JyAvPgo8L2c+CjxkZWZzPgogIDxjbGlwUGF0aCBpZD0nY3BNQzR3TUh3M01EZ3VOalo4TWpJdU5qaDhNakl1T0RFPSc+CiAgICA8cmVjdCB4PScwLjAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMCcgeT0nMjIuNjgnIHdpZHRoPSc3MDguNjYnIGhlaWdodD0nMC4xNCcgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHczTURndU5qWjhNakl1TmpoOE1qSXVPREU9KSc+CjxjaXJjbGUgY3g9JzM1NC4zMycgY3k9JzIyLjgxJyByPScwLjM1JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjcxOyBzdHJva2U6IG5vbmU7JyAvPgo8Y2lyY2xlIGN4PSczNTQuMzMnIGN5PScyMi44MScgcj0nMC4zNScgc3R5bGU9J3N0cm9rZS13aWR0aDogMC43MTsgc3Ryb2tlOiBub25lOycgLz4KPGNpcmNsZSBjeD0nMzU0LjMzJyBjeT0nMjIuODEnIHI9JzAuMzUnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDAuNzE7IHN0cm9rZTogbm9uZTsnIC8+CjwvZz4KPGcgY2xpcC1wYXRoPSd1cmwoI2NwTUM0d01IdzNNRGd1TmpaOE1DNHdNSHd5TWk0Mk9BPT0pJz4KPHJlY3QgeD0nMjIwLjA5JyB5PSctMC4xNCcgd2lkdGg9JzI2OC40OCcgaGVpZ2h0PScxNy4yOCcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC4wMDsgc3Ryb2tlOiBub25lOycgLz4KPGNpcmNsZSBjeD0nMjM0LjIxJyBjeT0nOC41MCcgcj0nNC42Micgc3R5bGU9J3N0cm9rZS13aWR0aDogMC43MTsgc3Ryb2tlOiAjRjg3NjZEOyBmaWxsOiAjRjg3NjZEOycgLz4KPGNpcmNsZSBjeD0nMjkyLjc0JyBjeT0nOC41MCcgcj0nNC42Micgc3R5bGU9J3N0cm9rZS13aWR0aDogMC43MTsgc3Ryb2tlOiAjMDBCQTM4OyBmaWxsOiAjMDBCQTM4OycgLz4KPGNpcmNsZSBjeD0nMzk3LjcyJyBjeT0nOC41MCcgcj0nNC42Micgc3R5bGU9J3N0cm9rZS13aWR0aDogMC43MTsgc3Ryb2tlOiAjNjE5Q0ZGOyBmaWxsOiAjNjE5Q0ZGOycgLz4KPHRleHQgeD0nMjQ4LjMzJyB5PScxMS41Mycgc3R5bGU9J2ZvbnQtc2l6ZTogOC44MHB4OyBmb250LWZhbWlseTogIkxpYmVyYXRpb24gU2FucyI7JyB0ZXh0TGVuZ3RoPSczMC4zMHB4JyBsZW5ndGhBZGp1c3Q9J3NwYWNpbmdBbmRHbHlwaHMnPk9yaWdpbmFsPC90ZXh0Pgo8dGV4dCB4PSczMDYuODYnIHk9JzExLjUzJyBzdHlsZT0nZm9udC1zaXplOiA4LjgwcHg7IGZvbnQtZmFtaWx5OiAiTGliZXJhdGlvbiBTYW5zIjsnIHRleHRMZW5ndGg9Jzc2LjczcHgnIGxlbmd0aEFkanVzdD0nc3BhY2luZ0FuZEdseXBocyc+VHlwZSAxIHJlY2FsaWJyYXRpb248L3RleHQ+Cjx0ZXh0IHg9JzQxMS44NCcgeT0nMTEuNTMnIHN0eWxlPSdmb250LXNpemU6IDguODBweDsgZm9udC1mYW1pbHk6ICJMaWJlcmF0aW9uIFNhbnMiOycgdGV4dExlbmd0aD0nNzYuNzNweCcgbGVuZ3RoQWRqdXN0PSdzcGFjaW5nQW5kR2x5cGhzJz5UeXBlIDIgcmVjYWxpYnJhdGlvbjwvdGV4dD4KPC9nPgo8L2c+Cjwvc3ZnPgo=) |                   |                               |                               |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| Model                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | Original (95% CI) | Type 1 recalibration (95% CI) | Type 2 recalibration (95% CI) |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| Men                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | 0.25 (0.24, 0.27) | 0.21 (0.20, 0.22)             | 0.23 (0.22, 0.24)             | ![](data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnID8+CjxzdmcgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzQyNS4yMHB0JyBoZWlnaHQ9JzQ4LjE5cHQnIHZpZXdCb3g9JzAgMCA0MjUuMjAgNDguMTknPgo8ZyBjbGFzcz0nc3ZnbGl0ZSc+CjxkZWZzPgogIDxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+PCFbQ0RBVEFbCiAgICAuc3ZnbGl0ZSBsaW5lLCAuc3ZnbGl0ZSBwb2x5bGluZSwgLnN2Z2xpdGUgcG9seWdvbiwgLnN2Z2xpdGUgcGF0aCwgLnN2Z2xpdGUgcmVjdCwgLnN2Z2xpdGUgY2lyY2xlIHsKICAgICAgZmlsbDogbm9uZTsKICAgICAgc3Ryb2tlOiAjMDAwMDAwOwogICAgICBzdHJva2UtbGluZWNhcDogcm91bmQ7CiAgICAgIHN0cm9rZS1saW5lam9pbjogcm91bmQ7CiAgICAgIHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsKICAgIH0KICAgIC5zdmdsaXRlIHRleHQgewogICAgICB3aGl0ZS1zcGFjZTogcHJlOwogICAgfQogICAgLnN2Z2xpdGUgZy5nbHlwaGdyb3VwIHBhdGggewogICAgICBmaWxsOiBpbmhlcml0OwogICAgICBzdHJva2U6IG5vbmU7CiAgICB9CiAgXV0+PC9zdHlsZT4KPC9kZWZzPgo8cmVjdCB3aWR0aD0nMTAwJScgaGVpZ2h0PScxMDAlJyBzdHlsZT0nc3Ryb2tlOiBub25lOyBmaWxsOiBub25lOycvPgo8ZGVmcz4KICA8Y2xpcFBhdGggaWQ9J2NwTUM0d01IdzBNalV1TWpCOE1DNHdNSHcwT0M0eE9RPT0nPgogICAgPHJlY3QgeD0nMC4wMCcgeT0nMC4wMCcgd2lkdGg9JzQyNS4yMCcgaGVpZ2h0PSc0OC4xOScgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHcwTWpVdU1qQjhNQzR3TUh3ME9DNHhPUT09KSc+CjxyZWN0IHg9JzAuMDAnIHk9JzAuMDAnIHdpZHRoPSc0MjUuMjAnIGhlaWdodD0nNDguMTknIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogbm9uZTsnIC8+CjwvZz4KPGRlZnM+CiAgPGNsaXBQYXRoIGlkPSdjcE5TNDBPSHcwTVRrdU56SjhOUzQwT0h3ME1pNDNNUT09Jz4KICAgIDxyZWN0IHg9JzUuNDgnIHk9JzUuNDgnIHdpZHRoPSc0MTQuMjQnIGhlaWdodD0nMzcuMjMnIC8+CiAgPC9jbGlwUGF0aD4KPC9kZWZzPgo8ZyBjbGlwLXBhdGg9J3VybCgjY3BOUzQwT0h3ME1Ua3VOeko4TlM0ME9IdzBNaTQzTVE9PSknPgo8cmVjdCB4PSc1LjQ4JyB5PSc1LjQ4JyB3aWR0aD0nNDE0LjI0JyBoZWlnaHQ9JzM3LjIzJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6IG5vbmU7JyAvPgo8cG9seWxpbmUgcG9pbnRzPScyMS45MSw0Mi43MSAyMS45MSw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC41Mzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzEzNy45MCw0Mi43MSAxMzcuOTAsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDAuNTM7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPScyNTMuODgsNDIuNzEgMjUzLjg4LDUuNDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICM4QzhDOEM7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nMzY5Ljg3LDQyLjcxIDM2OS44Nyw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC41Mzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9Jzc5LjkxLDQyLjcxIDc5LjkxLDUuNDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6ICM4QzhDOEM7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nMTk1Ljg5LDQyLjcxIDE5NS44OSw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzMxMS44OCw0Mi43MSAzMTEuODgsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8bGluZSB4MT0nMzE3LjE1JyB5MT0nMzUuNzMnIHgyPSczODUuNTAnIHkyPSczNS43Mycgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjRjg3NjZEOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+CjxsaW5lIHgxPScyMDkuODEnIHkxPScyNC4wOScgeDI9JzI0Ny4yNycgeTI9JzI0LjA5JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6ICMwMEJBMzg7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPGxpbmUgeDE9JzI2My4wOCcgeTE9JzEyLjQ2JyB4Mj0nMzA5Ljk0JyB5Mj0nMTIuNDYnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogIzYxOUNGRjsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8Y2lyY2xlIGN4PSczNTIuMjYnIGN5PSczNS43Mycgcj0nMi44NCcgc3R5bGU9J3N0cm9rZS13aWR0aDogMS40Mjsgc3Ryb2tlOiAjRjg3NjZEOyBmaWxsOiAjRjg3NjZEOycgLz4KPGNpcmNsZSBjeD0nMjI3LjIxJyBjeT0nMjQuMDknIHI9JzIuODQnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuNDI7IHN0cm9rZTogIzAwQkEzODsgZmlsbDogIzAwQkEzODsnIC8+CjxjaXJjbGUgY3g9JzI4Ni4yNycgY3k9JzEyLjQ2JyByPScyLjg0JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjQyOyBzdHJva2U6ICM2MTlDRkY7IGZpbGw6ICM2MTlDRkY7JyAvPgo8bGluZSB4MT0nMTg3LjkyJyB5MT0nNDIuNzEnIHgyPScxODcuOTInIHkyPSc1LjQ4JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2UtZGFzaGFycmF5OiA1LjY5LDUuNjk7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPC9nPgo8ZyBjbGlwLXBhdGg9J3VybCgjY3BNQzR3TUh3ME1qVXVNakI4TUM0d01IdzBPQzR4T1E9PSknPgo8L2c+CjwvZz4KPC9zdmc+Cg==) |
| Women                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | 0.18 (0.17, 0.20) | 0.15 (0.15, 0.16)             | 0.15 (0.14, 0.16)             | ![](data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnID8+CjxzdmcgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzQyNS4yMHB0JyBoZWlnaHQ9JzQ4LjE5cHQnIHZpZXdCb3g9JzAgMCA0MjUuMjAgNDguMTknPgo8ZyBjbGFzcz0nc3ZnbGl0ZSc+CjxkZWZzPgogIDxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+PCFbQ0RBVEFbCiAgICAuc3ZnbGl0ZSBsaW5lLCAuc3ZnbGl0ZSBwb2x5bGluZSwgLnN2Z2xpdGUgcG9seWdvbiwgLnN2Z2xpdGUgcGF0aCwgLnN2Z2xpdGUgcmVjdCwgLnN2Z2xpdGUgY2lyY2xlIHsKICAgICAgZmlsbDogbm9uZTsKICAgICAgc3Ryb2tlOiAjMDAwMDAwOwogICAgICBzdHJva2UtbGluZWNhcDogcm91bmQ7CiAgICAgIHN0cm9rZS1saW5lam9pbjogcm91bmQ7CiAgICAgIHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsKICAgIH0KICAgIC5zdmdsaXRlIHRleHQgewogICAgICB3aGl0ZS1zcGFjZTogcHJlOwogICAgfQogICAgLnN2Z2xpdGUgZy5nbHlwaGdyb3VwIHBhdGggewogICAgICBmaWxsOiBpbmhlcml0OwogICAgICBzdHJva2U6IG5vbmU7CiAgICB9CiAgXV0+PC9zdHlsZT4KPC9kZWZzPgo8cmVjdCB3aWR0aD0nMTAwJScgaGVpZ2h0PScxMDAlJyBzdHlsZT0nc3Ryb2tlOiBub25lOyBmaWxsOiBub25lOycvPgo8ZGVmcz4KICA8Y2xpcFBhdGggaWQ9J2NwTUM0d01IdzBNalV1TWpCOE1DNHdNSHcwT0M0eE9RPT0nPgogICAgPHJlY3QgeD0nMC4wMCcgeT0nMC4wMCcgd2lkdGg9JzQyNS4yMCcgaGVpZ2h0PSc0OC4xOScgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHcwTWpVdU1qQjhNQzR3TUh3ME9DNHhPUT09KSc+CjxyZWN0IHg9JzAuMDAnIHk9JzAuMDAnIHdpZHRoPSc0MjUuMjAnIGhlaWdodD0nNDguMTknIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogbm9uZTsnIC8+CjwvZz4KPGRlZnM+CiAgPGNsaXBQYXRoIGlkPSdjcE5TNDBPSHcwTVRrdU56SjhOUzQwT0h3ME1pNDNNUT09Jz4KICAgIDxyZWN0IHg9JzUuNDgnIHk9JzUuNDgnIHdpZHRoPSc0MTQuMjQnIGhlaWdodD0nMzcuMjMnIC8+CiAgPC9jbGlwUGF0aD4KPC9kZWZzPgo8ZyBjbGlwLXBhdGg9J3VybCgjY3BOUzQwT0h3ME1Ua3VOeko4TlM0ME9IdzBNaTQzTVE9PSknPgo8cmVjdCB4PSc1LjQ4JyB5PSc1LjQ4JyB3aWR0aD0nNDE0LjI0JyBoZWlnaHQ9JzM3LjIzJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6IG5vbmU7JyAvPgo8cG9seWxpbmUgcG9pbnRzPScyMS45MSw0Mi43MSAyMS45MSw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC41Mzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzEzNy45MCw0Mi43MSAxMzcuOTAsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDAuNTM7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8cG9seWxpbmUgcG9pbnRzPScyNTMuODgsNDIuNzEgMjUzLjg4LDUuNDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICM4QzhDOEM7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nMzY5Ljg3LDQyLjcxIDM2OS44Nyw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMC41Mzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9Jzc5LjkxLDQyLjcxIDc5LjkxLDUuNDggJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6ICM4QzhDOEM7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nMTk1Ljg5LDQyLjcxIDE5NS44OSw1LjQ4ICcgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjOEM4QzhDOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+Cjxwb2x5bGluZSBwb2ludHM9JzMxMS44OCw0Mi43MSAzMTEuODgsNS40OCAnIHN0eWxlPSdzdHJva2Utd2lkdGg6IDEuMDc7IHN0cm9rZTogIzhDOEM4Qzsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7JyAvPgo8bGluZSB4MT0nMTEyLjI5JyB5MT0nMzUuNzMnIHgyPScxODcuMTYnIHkyPSczNS43Mycgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjRjg3NjZEOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+CjxsaW5lIHgxPSczNi41MycgeTE9JzI0LjA5JyB4Mj0nNzUuNjcnIHkyPScyNC4wOScgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjMDBCQTM4OyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+CjxsaW5lIHgxPSczMi42NCcgeTE9JzEyLjQ2JyB4Mj0nODEuMTMnIHkyPScxMi40Nicgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiAjNjE5Q0ZGOyBzdHJva2UtbGluZWNhcDogYnV0dDsnIC8+CjxjaXJjbGUgY3g9JzE0OS41NicgY3k9JzM1LjczJyByPScyLjg0JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjQyOyBzdHJva2U6ICNGODc2NkQ7IGZpbGw6ICNGODc2NkQ7JyAvPgo8Y2lyY2xlIGN4PSc1Ni44NCcgY3k9JzI0LjA5JyByPScyLjg0JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjQyOyBzdHJva2U6ICMwMEJBMzg7IGZpbGw6ICMwMEJBMzg7JyAvPgo8Y2lyY2xlIGN4PSc1NS40MCcgY3k9JzEyLjQ2JyByPScyLjg0JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjQyOyBzdHJva2U6ICM2MTlDRkY7IGZpbGw6ICM2MTlDRkY7JyAvPgo8bGluZSB4MT0nMTg3LjkyJyB5MT0nNDIuNzEnIHgyPScxODcuOTInIHkyPSc1LjQ4JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2UtZGFzaGFycmF5OiA1LjY5LDUuNjk7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPC9nPgo8ZyBjbGlwLXBhdGg9J3VybCgjY3BNQzR3TUh3ME1qVXVNakI4TUM0d01IdzBPQzR4T1E9PSknPgo8L2c+CjwvZz4KPC9zdmc+Cg==)         |
|                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |                   |                               |                               | ![](data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnID8+CjxzdmcgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzYzNy44MHB0JyBoZWlnaHQ9JzE5Ljg0cHQnIHZpZXdCb3g9JzAgMCA2MzcuODAgMTkuODQnPgo8ZyBjbGFzcz0nc3ZnbGl0ZSc+CjxkZWZzPgogIDxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+PCFbQ0RBVEFbCiAgICAuc3ZnbGl0ZSBsaW5lLCAuc3ZnbGl0ZSBwb2x5bGluZSwgLnN2Z2xpdGUgcG9seWdvbiwgLnN2Z2xpdGUgcGF0aCwgLnN2Z2xpdGUgcmVjdCwgLnN2Z2xpdGUgY2lyY2xlIHsKICAgICAgZmlsbDogbm9uZTsKICAgICAgc3Ryb2tlOiAjMDAwMDAwOwogICAgICBzdHJva2UtbGluZWNhcDogcm91bmQ7CiAgICAgIHN0cm9rZS1saW5lam9pbjogcm91bmQ7CiAgICAgIHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsKICAgIH0KICAgIC5zdmdsaXRlIHRleHQgewogICAgICB3aGl0ZS1zcGFjZTogcHJlOwogICAgfQogICAgLnN2Z2xpdGUgZy5nbHlwaGdyb3VwIHBhdGggewogICAgICBmaWxsOiBpbmhlcml0OwogICAgICBzdHJva2U6IG5vbmU7CiAgICB9CiAgXV0+PC9zdHlsZT4KPC9kZWZzPgo8cmVjdCB3aWR0aD0nMTAwJScgaGVpZ2h0PScxMDAlJyBzdHlsZT0nc3Ryb2tlOiBub25lOyBmaWxsOiBub25lOycvPgo8ZGVmcz4KICA8Y2xpcFBhdGggaWQ9J2NwTUM0d01IdzJNemN1T0RCOE1DNHdNSHd4T1M0NE5BPT0nPgogICAgPHJlY3QgeD0nMC4wMCcgeT0nMC4wMCcgd2lkdGg9JzYzNy44MCcgaGVpZ2h0PScxOS44NCcgLz4KICA8L2NsaXBQYXRoPgo8L2RlZnM+CjxnIGNsaXAtcGF0aD0ndXJsKCNjcE1DNHdNSHcyTXpjdU9EQjhNQzR3TUh3eE9TNDROQT09KSc+CjxyZWN0IHg9JzAuMDAnIHk9Jy05LjIyJyB3aWR0aD0nNjM3LjgwJyBoZWlnaHQ9JzM4LjI4JyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2U6IG5vbmU7JyAvPgo8L2c+CjxkZWZzPgogIDxjbGlwUGF0aCBpZD0nY3BOUzQwT0h3Mk16SXVNeko4TFRNdU56UjhNQzR3TUE9PSc+CiAgICA8cmVjdCB4PSc1LjQ4JyB5PSctMy43NCcgd2lkdGg9JzYyNi44NCcgaGVpZ2h0PSczLjc0JyAvPgogIDwvY2xpcFBhdGg+CjwvZGVmcz4KPGcgY2xpcC1wYXRoPSd1cmwoI2NwTlM0ME9IdzJNekl1TXpKOExUTXVOelI4TUM0d01BPT0pJz4KPHJlY3QgeD0nNS40OCcgeT0nLTMuNzQnIHdpZHRoPSc2MjYuODQnIGhlaWdodD0nMC4wMCcgc3R5bGU9J3N0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiBub25lOycgLz4KPHBvbHlsaW5lIHBvaW50cz0nNS40OCwtMy43NCA2MzIuMzIsLTMuNzQgJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICNFQkVCRUI7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nNS40OCwtMy43NCA2MzIuMzIsLTMuNzQgJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICNFQkVCRUI7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nNS40OCwtMy43NCA2MzIuMzIsLTMuNzQgJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICNFQkVCRUI7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPHBvbHlsaW5lIHBvaW50cz0nNS40OCwtMy43NCA2MzIuMzIsLTMuNzQgJyBzdHlsZT0nc3Ryb2tlLXdpZHRoOiAwLjUzOyBzdHJva2U6ICNFQkVCRUI7IHN0cm9rZS1saW5lY2FwOiBidXR0OycgLz4KPC9nPgo8ZyBjbGlwLXBhdGg9J3VybCgjY3BNQzR3TUh3Mk16Y3VPREI4TUM0d01Id3hPUzQ0TkE9PSknPgo8dGV4dCB4PScxMTguMTAnIHk9JzIwLjk5JyB0ZXh0LWFuY2hvcj0nbWlkZGxlJyBzdHlsZT0nZm9udC1zaXplOiAyNS4wMHB4O2ZpbGw6ICM0RDRENEQ7IGZvbnQtZmFtaWx5OiAiTGliZXJhdGlvbiBTYW5zIjsnIHRleHRMZW5ndGg9JzQ4LjY3cHgnIGxlbmd0aEFkanVzdD0nc3BhY2luZ0FuZEdseXBocyc+MC4xNjwvdGV4dD4KPHRleHQgeD0nMjkzLjYyJyB5PScyMC45OScgdGV4dC1hbmNob3I9J21pZGRsZScgc3R5bGU9J2ZvbnQtc2l6ZTogMjUuMDBweDtmaWxsOiAjNEQ0RDREOyBmb250LWZhbWlseTogIkxpYmVyYXRpb24gU2FucyI7JyB0ZXh0TGVuZ3RoPSc0OC42N3B4JyBsZW5ndGhBZGp1c3Q9J3NwYWNpbmdBbmRHbHlwaHMnPjAuMjA8L3RleHQ+Cjx0ZXh0IHg9JzQ2OS4xMycgeT0nMjAuOTknIHRleHQtYW5jaG9yPSdtaWRkbGUnIHN0eWxlPSdmb250LXNpemU6IDI1LjAwcHg7ZmlsbDogIzRENEQ0RDsgZm9udC1mYW1pbHk6ICJMaWJlcmF0aW9uIFNhbnMiOycgdGV4dExlbmd0aD0nNDguNjdweCcgbGVuZ3RoQWRqdXN0PSdzcGFjaW5nQW5kR2x5cGhzJz4wLjI0PC90ZXh0Pgo8L2c+CjwvZz4KPC9zdmc+Cg==)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |

Figure 7: Forestplot Brier score comparison

### References

Buuren, S. van. (2018). *Flexible imputation of missing data, second
edition*. Chapman; Hall/CRC. <https://doi.org/10.1201/9780429492259>

van Buuren, S., & Groothuis-Oudshoorn, K. (2011). mice: Multivariate
imputation by chained equations in r. *Journal of Statistical Software*,
*45*(3), 1–67. <https://doi.org/10.18637/jss.v045.i03>

Wilson, P. W. F., D’Agostino, R. B., Levy, D., Belanger, A. M.,
Silbershatz, H., & Kannel, W. B. (1998). Prediction of coronary heart
disease using risk factor categories. *Circulation*, *97*(18),
1837–1847. <https://doi.org/10.1161/01.CIR.97.18.1837>
