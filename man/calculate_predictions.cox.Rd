% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_predictions.cox.R
\name{calculate_predictions.cox}
\alias{calculate_predictions.cox}
\title{Calculates the predictions for a Cox model}
\usage{
\method{calculate_predictions}{cox}(model, data, .progress = FALSE)
}
\arguments{
\item{model}{Model generated with \code{\link[=mv_model_cox]{mv_model_cox()}}}

\item{data}{External validation data. Multiple imputation dataset in long format.}

\item{.progress}{\code{TRUE} to render the progress bar \code{FALSE} otherwise.}
}
\value{
The \code{model} with the parameters \code{predictions_aggregated}, \code{predictions_data}, \code{betax} and \code{betax_data} populated.
\itemize{
\item \code{predictions_aggregated}, stores the predictions aggregated by the mean.
\item \code{predictions_data}, stores all the predictions in each of the imputed datasets.
\item \code{betax}, stores the \eqn{\beta \cdot X} values aggregated by the mean.
\item \code{betax_data}, stores the \eqn{\beta \cdot X} values in each of the imputed datasets.
}
}
\description{
Calculates the predictions for the given Cox model and external validation data. The predictions are calculated following the definition of a cox model

\deqn{S_0(t)^{e^{(\beta \cdot X)}}}

where \eqn{S_0(t)} stands for the survival function in a time \eqn{t}, \eqn{\beta} stands for the model coefficients and \eqn{X} stands for the centered values in each \code{id}.
}
\examples{
set.seed(123)

model <- mv_model_cox(
  formula = event ~ 0.5 * (x - 1) + 0.3 * (z - 2),
  S0 = 0.98765
)

data <- data.frame(
  .imp = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  event = survival::Surv(rpois(9, 5), rbinom(n = 9, size = 1, prob = 0.2)),
  x = rnorm(9, 1, 0.25),
  z = rnorm(9, 2, 0.75)
)

model |> calculate_predictions(data)
}
