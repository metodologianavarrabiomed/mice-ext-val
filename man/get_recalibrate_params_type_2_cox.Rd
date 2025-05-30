% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_recalibrate_params_type_2_cox.R
\name{get_recalibrate_params_type_2_cox}
\alias{get_recalibrate_params_type_2_cox}
\title{Obtains the \eqn{S_0(t)} and \eqn{\beta_{overall}} parameters for recalibration}
\usage{
get_recalibrate_params_type_2_cox(time, event, betax)
}
\arguments{
\item{time}{time of follow up for each patient, must be \code{numeric}}

\item{event}{if the patient has suffered an event during follow up, must be \code{numeric} \eqn{1} event \eqn{0} othercase}

\item{betax}{the betax values, \eqn{\beta X}, of the original predictions, must be \code{numeric}}
}
\value{
a \code{list} with two elements:
\itemize{
\item S0: \eqn{S_0(t)}
\item beta_overall: \eqn{\beta_{overall}} value from the type 2 recalibration
}
}
\description{
Calculates the recalibration type 2 parameters for the \code{recalibrate_data} and returns a list with the two needed parameters \eqn{S_0(t)}, the survival estimation in the time \eqn{t} for the external validation data, and \eqn{\beta_{overall}}, the importance that the \eqn{\beta \cdot X} values should have over the external validation data.

The survival function estimation is calculated as a Weibull distribution. A Weibull model is derived in the external validation cohort and the value estimated by the model for the time \eqn{t} is used as the recalibration of \eqn{S_0(t)}. The importance of the \eqn{\beta \cdot X} is calculated by deriving a Cox model using as only covariate the model \eqn{\beta \cdot X} values. The derived Cox model coefficient is used as \eqn{\beta_{overall}}.
}
\examples{
\dontrun{
get_recalibrate_params_type_2_cox(data, betax, t)
}
}
