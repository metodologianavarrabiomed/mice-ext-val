#' Function that generates a C-Index forestplot for the given data
#'
#' @param data Dataset where all the data to plot is stored. It is recommended to be generated using [get_forestplot_data()]
#' @param center x intercept to display a stripped vertical line, default `NULL`, no center.
#' @param digits decimal digits for the table generation, default `3`
#'
#' @returns a C-Index forestplot
#' @export
#'
#' @examples
#' \dontrun{
#' get_forestplot(data, 0.5)
#' }
get_forestplot <- function(data, center = NULL, digits = 3) {
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

  # generate the forestplot -------------------------------------------------
  get_model_plot <- function(data, min_x, max_x) {
    # create the plot
    p <- data |>
      ggplot2::ggplot(ggplot2::aes(x = .data[["estimate"]], y = .data[["strat"]], color = .data[["strat"]])) +
      ggplot2::geom_pointrange(
        ggplot2::aes(xmin = .data[["lower"]], xmax = .data[["upper"]]),
        size = 0.35
      ) +
      ggplot2::geom_vline(xintercept = center, linetype = "dashed") +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Strat")) +
      ggplot2::xlim(c(min_x, max_x)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.text.x = ggplot2::element_blank(),
        strip.text.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(colour = "gray55"),
        panel.grid.minor.x = ggplot2::element_line(colour = "gray55"),
        panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        panel.spacing = ggplot2::unit(0.25, "lines"),
        # TODO: decide wheter to add the axis text or not in each individual plot
        axis.text.x = ggplot2::element_text(size = 7),
        # axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "gray65", colour = NA),
        legend.position = "none",
        plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
      )

    # store it in a temporal file `svg`
    out_name <- file.path(
      tempfile(
        pattern = "file",
        tmpdir = tempdir(),
        fileext = ".svg"
      )
    )

    ggplot2::ggsave(
      out_name,
      plot = p,
      # TODO: take care of this magic numbers
      dpi = 25.4,
      height = 17,
      width = 150,
      units = "mm",
      device = "svg",
      bg = "transparent"
    )

    # return the file path
    return(out_name)
  }

  get_legend <- function(data) {
    legend_df <- data.frame(
      x = 0, y = 0,
      grp = factor(unique(data[["strat"]]))
    )

    p <- ggplot2::ggplot(legend_df, ggplot2::aes(x, y, colour = grp)) +
      ggplot2::geom_point(size = 0, alpha = 0) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 4, alpha = 1))) +
      ggplot2::labs(colour = "") +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "top",
        legend.background = ggplot2::element_rect(fill = NA, colour = NA),
        plot.margin = ggplot2::margin(0, 0, 0, 0)
      )

    # store it in a temporal file `svg`
    out_name <- file.path(
      tempfile(
        pattern = "file",
        tmpdir = tempdir(),
        fileext = ".svg"
      )
    )

    ggplot2::ggsave(
      out_name,
      plot = p,
      # TODO: take care of this magic numbers
      dpi = 25.4,
      height = 8,
      width = 250,
      units = "mm",
      device = "svg",
      bg = "transparent"
    )

    # return the file path
    return(out_name)
  }

  # generate the main table
  table <- data |>
    dplyr::mutate(estimate_str = sprintf(glue::glue("%.{digits}f (%.{digits}f, %.{digits}f)"), .data[["estimate"]], .data[["lower"]], .data[["upper"]])) |>
    tidyr::pivot_wider(id_cols = .data[["model"]], names_from = .data[["strat"]], values_from = .data[["estimate_str"]], names_glue = "{strat} (95% CI)") |>
    dplyr::rename(Model = .data[["model"]]) |>
    dplyr::mutate(plot = dplyr::row_number())

  # define the x-limits of the plot
  min_x <- min(data[["lower"]]) * 0.98
  max_x <- max(data[["upper"]]) * 1.02

  # generate the plots
  model_plots <- data |>
    dplyr::group_by(.data[["model"]]) |>
    dplyr::group_map(~ {
      get_model_plot(.x, min_x, max_x)
    })

  legend_plot <- get_legend(data)

  # add the plots to the `gt` table
  table <- table |>
    gt::gt() |>
    gt::cols_width(
      c("plot") ~ gt::px(150)
    ) |>
    gt::text_transform(
      locations = gt::cells_body(columns = plot),
      fn = function(x) {
        purrr::map(as.numeric(x), ~ {
          # read temporary file
          img_plot <- gt::html(gt::local_image(filename = model_plots[[as.numeric(.x)]], height = gt::px(55)))

          # delete temporary file
          file.remove(model_plots[[as.numeric(.x)]])

          return(img_plot)
        })
      }
    ) |>
    gt::text_transform(
      locations = gt::cells_column_labels(columns = "plot"),
      fn = \(x) ""
    ) |>
    gt::tab_style(
      style = list(gt::cell_text(weight = "bold")),
      locations = gt::cells_column_labels(columns = gt::everything())
    ) |>
    gt::tab_options(
      data_row.padding = gt::px(0),
      table_body.border.top.width = gt::px(0),
      table_body.border.bottom.width = gt::px(0),
      table.border.top.width = gt::px(0),
      table.border.bottom.width = gt::px(0),
    ) |>
    gt::tab_header(
      title = "",
      subtitle = gt::html(gt::local_image(filename = legend_plot))
    ) |>
    gt::cols_align(
      align = "auto",
      columns = gt::everything()
    )

  return(table)
}
