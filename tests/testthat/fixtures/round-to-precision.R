#' rounds the inserted value to a certain predefined precision
#'
#' @param x number or numbers to round
#'
#' @returns the parameter `x` rounded to the predefined precision
round_to_precision <- function(x) {
  PRECISION <- 7
  ret <- NULL
  if (is.numeric(x)) {
    ret <- round(x, PRECISION)
  } else {
    if (is.data.frame(x)) {
      ret <- x |> dplyr::mutate_if(is.numeric, round, digits = 7)
    } else {
      ret <- sapply(x, round, digits = PRECISION)
    }
  }
  return(ret)
}
