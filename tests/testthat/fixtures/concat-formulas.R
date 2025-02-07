#' Concats a model formula with an expression maintaining its definition
#' and environment
#'
#' @param model_formula formula
#' @param exp
#'
#' @returns the formula concatenated maintaining its environment
concat_formulas <- function(model_formula, exp) {
  f_str <- as.character(model_formula)
  paste(f_str[2],f_str[1],f_str[3]) |>
    paste(exp, sep = " + ") |>
    as.formula(attr(model_formula,".Environment"))
}
