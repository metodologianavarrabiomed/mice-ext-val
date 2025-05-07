#' An example of the GUSTO-I W region dataset extracted from the book 'Clinical Prediction Models', see references. We have inserted missing values to the dataset in order to be used as an example for this package.
#'
#' @format A data frame with nine variables:
#' \describe{
#' \item{\code{sho}}{Shock: Killip class 3/4 vs. 1/2}
#' \item{\code{a65}}{If the patient is older than 65 years old}
#' \item{\code{dia}}{Diagnosed diabetes}
#' \item{\code{hig}}{Categorized for high risk of Anterior Infart Location or Prevous Myocardial Infarction}
#' \item{\code{hyp}}{Hypotension: Systolic Blood Pressure < 100 mmHg}
#' \item{\code{hrt}}{Heart rate: Pulse>80 (Tachycardia)}
#' \item{\code{ttr}}{Time to relief of chest pain > 1h}
#' \item{\code{day30}}{30 day mortality}
#' \item{\code{sex}}{Gender}
#' }
#'
#' @docType data
#' @keywords dataset
#' @name gusto
#' @usage data(gusto)
#' @format a dataset with 2188 rows and 9 variables
#'
#' @references Steyerberg, E.W. (2019). Updating for a New Setting. In: Clinical Prediction Models. Statistics for Biology and Health. Springer, Cham. https://doi.org/10.1007/978-3-030-16399-0_20
"gusto"
