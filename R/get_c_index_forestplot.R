#' @title
#' Generates a forestplot to visualize C-Index of different models
#'
#' @description
#' From a list of models passed as parameters of the function extracts their C-Index value and generates a forestplot to compare their results.
#'
#' @param ... The list of models that will conform the forestplot. If they are passed as variables, the variable name is assigned as model name in the forestplot. But, If they are passed as named arguments the argument name is used in the forestplot
#'
#' @return A forestplot with the C-Index data of the list of models
#'
#' @export
#'
#' @importFrom forestplot forestplot fpTxtGp fp_add_header fp_set_zebra_style fp_set_style
#' @importFrom purrr map_chr map_dbl
#' @importFrom grid unit gpar
#' @importFrom methods is
#'
#' @examples
#' \dontrun{
#' get_c_index_forestplot(A = model1, B = model2, C = model3, D = model4)
#' get_c_index_forestplot(model1, model2, model3, model4)
#' }
get_c_index_forestplot <- function(...) {
  # Check if all the parameters are MiceExtVal class
  is_model_class <- purrr::map_lgl(list(...), \(x) methods::is(x, "MiceExtVal"))
  models <- list(...)

  model_names_callname <- names(models)
  model_names_call <- as.character(as.list(match.call())[-1])

  if (is.null(model_names_callname)) {
    model_names_callname <- model_names_call
  } else if (is.null(model_names_call)) {
    model_names_call <- model_names_callname
  }
  model_names <- purrr::map2_chr(model_names_callname, model_names_call, ~ ifelse(is.na(.x) | .x == "", .y, .x))

  if (!all(is_model_class)) {
    cli::cli_abort("The model{?s} {.arg {model_names[!is_model_class]}} must be from the class {.arg MiceExtVal}")
  }

  has_c_index <- purrr::map_lgl(list(...), \(x) !is.null(x$c_index))
  if (!all(has_c_index)) {
    cli::cli_warn("The model{?s} {.arg {model_names[!has_c_index]}} {?has/have} no {.arg c_index} parameter so {?it/they} {?is/are} removed from the forestplot")

    model_names <- model_names[has_c_index]
    models <- models[has_c_index]
  }

  # Generates the parameter lines for the forestplot
  lines <- list(
    grid::gpar(lwd = 1.7, lty = 1),
    grid::gpar(lwd = 1.7, lty = 1)
  )
  names(lines) <- c("2", 2 + length(models))

  # Generation of the data for the forestplot
  tibble::tibble(
    model_name = as.character(model_names),
    c_index = purrr::map_chr(
      models,
      \(model) {
        sprintf(
          "%.3f (CI %.3f, %.3f)",
          model$c_index["Estimate"],
          model$c_index["95% CI L"],
          model$c_index["95% CI U"]
        )
      }
    ),
    c_index_mean = purrr::map_dbl(models, \(x) x$c_index["Estimate"]),
    c_index_lower = purrr::map_dbl(models, \(x) x$c_index["95% CI L"]),
    c_index_upper = purrr::map_dbl(models, \(x) x$c_index["95% CI U"])
  ) |>
    # Generation of the forestplot
    forestplot::forestplot(
      mean = "c_index_mean",
      lower = "c_index_lower",
      upper = "c_index_upper",
      labeltext = c("model_name", "c_index"),
      clip = c(0, 1),
      new_page = T,
      hrzl_lines = lines,
      lineheight = grid::unit(10.5, "mm"),
      txt_gp = forestplot::fpTxtGp(
        label = grid::gpar(fontfamily = "", col = "black", cex = 1.2),
        ticks = grid::gpar(fontfamily = "", cex = 1),
        xlab = grid::gpar(fontfamily = "", cex = 1),
        legend = grid::gpar(fontfamily = "", cex = 1),
        title = grid::gpar(fontfamily = "", cex = 1.2)
      ),
      line.margin = .17,
      boxsize = 0.2,
      ci.vertices = T,
      ci.vertices.height = 0.1,
      fn.ci_norm = forestplot::fpDrawCircleCI,
      zero = 0.5,
    ) |>
    forestplot::fp_add_header(
      model_name = c("Model"),
      c_index = c("C-index (95% CI)")
    ) |>
    forestplot::fp_set_zebra_style("#F2F2F2") |>
    forestplot::fp_set_style(lines = grid::gpar(fontfamily = "", col = "black", cex = 1.2))
}
