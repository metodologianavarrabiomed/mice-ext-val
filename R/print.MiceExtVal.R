#' A generic function to print the `MiceExtVal` model
#'
#' @param x a `MiceExtVal` model
#' @param ... ignored and not passed to any function
#'
#' @returns the model printed
#' @exportS3Method print MiceExtVal
#'
#' @importFrom utils head
#'
#' @examples
#' model_cox <- mv_model_cox(
#'   formula = event ~ 0.5 * (x - 1) + 0.3 * (z - 2),
#'   S0 = 0.98765
#' )
#'
#' print(model_cox)
#'
#' model_logreg <- mv_model_logreg(formula = event ~ 0.5 * x + 0.3 * z - 1.2)
#'
#' print(model_logreg)
print.MiceExtVal <- function(x, ...) {
  cli::cli_h1("{.cls {class(x)}}")

  for (elem in names(x)) {
    if (!is.null(x[[elem]])) {
      cli::cli_h2(elem)

      # show tibbles
      if (methods::is(x[[elem]], "tbl_df")) {
        print(head(x[[elem]], n = 5))
      } else {
        # show lists
        if (methods::is(x[[elem]], "list")) {
          list_text <- purrr::map2_chr(
            names(x[[elem]]), x[[elem]], ~ cli::format_message("{.x} = {.val {.y}}")
          )
          cli::cli_text("{list_text}")
        } else {
          # c-index formatting
          if (elem %in% c("c_index", "auc", "brier_score")) {
            cli::cli_text(
              "{.val {x[[elem]]['Estimate']}} (95% CI {.val {x[[elem]]['95% CI L']}}, {.val {x[[elem]]['95% CI U']}})"
            )
          } else {
            # default
            cli::cli_text(x[[elem]])
          }
        }
      }
    }
  }
}
