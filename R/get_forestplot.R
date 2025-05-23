#' Function that generates a C-Index forestplot for the given data
#'
#' @param data Dataset where all the data to plot is stored. It is recommended to be generated using [get_forestplot_data()]
#' @param center x intercept to display a stripped vertical line
#' @param digits decimal digits for the table generation, default `3`
#' @param table if the table is displayed while generating the forestplot, default `FALSE`
#'
#' @returns a C-Index forestplot
#' @export
#'
#' @examples
#' \dontrun{
#' get_forestplot(data, 0.5)
#' }
get_forestplot <- function(data, center, digits = 3, table = FALSE) {
  # assert dataset ----------------------------------------------------------
  error_message <- NULL
  if (!methods::is(data, "data.frame")) {
    error_message <- c(error_message, "*" = cli::format_error("The argument {.arg data} must be of class {.cls data.frame}"))
  }

  needed_variables <- c(
    "estimate" = "numeric", "strat" = "character", "model" = "character", "lower" = "numeric", "upper" = "numeric"
  )

  is_variable <- names(needed_variables) %in% colnames(data)
  if (any(is_variable)) {
    variables_in_data <- needed_variables[is_variable]
    correct_type <- purrr::map_lgl(names(variables_in_data), ~ methods::is(data[[.x]], needed_variables[.x]))
    if (!all(correct_type)) {
      error_message <- c(
        error_message,
        "*" = cli::format_error("The data variable{?s} {.var {names(variables_in_data)[!correct_type]}} ha{?s/ve} wrong typing"),
        "i" = cli::format_error("th{?is/ese} variable{?s} must be of class {.cls {variables_in_data[!correct_type]}} {?respectively}")
      )
    }
  }

  if (any(!is_variable)) {
    error_message <- c(
      error_message,
      "*" = cli::format_error("The data variable{?s} {.var {names(needed_variables)[!is_variable]}} must be present in {.arg data}"),
      "i" = cli::format_error("th{?is/ese} variable{?s} must be of class {.cls {needed_variables[!is_variable]}} {?respectively}")
    )
  }

  if (!is.null(error_message)) {
    cli::cli_abort(error_message)
  }

  if (table) {
    table_data <- data |>
      dplyr::mutate_if(is.numeric, round, digits = digits) |>
      dplyr::mutate(estimate_str = paste0(.data[["estimate"]], " (", .data[["lower"]], ", ", .data[["upper"]], ")")) |>
      tidyr::pivot_wider(id_cols = .data[["model"]], names_from = .data[["strat"]], values_from = .data[["estimate_str"]], names_glue = "{strat} (95% CI)") |>
      dplyr::rename(Model = .data[["model"]])
  }

  plot <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data[["estimate"]], y = .data[["strat"]], color = .data[["strat"]])) +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = .data[["lower"]], xmax = .data[["upper"]]),
      size = 0.5
    ) +
    ggplot2::facet_grid(model ~ 1, switch = "y") +
    ggplot2::geom_vline(xintercept = center, linetype = "dashed") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Strat")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text.x = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(
        size = 12, face = "bold", angle = 0
      ),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "gray85"),
      panel.grid.minor.x = ggplot2::element_line(colour = "gray85"),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "gray97", color = NA),
      panel.background = ggplot2::element_rect(fill = "gray97", color = NA),
      panel.spacing = ggplot2::unit(0.25, "lines")
    )

  if (table) {
    plot <- patchwork::wrap_table(table_data, panel = "full", space = "fixed") +
      plot
  }

  return(plot)
}
