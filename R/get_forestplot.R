#' Function that generates a C-Index forestplot for the given data
#'
#' @param data Dataset where all the data to plot is stored. It is recommended to be generated using [get_forestplot_data()]
#' @param center x intercept to display a stripped vertical line, default `NULL`, no center.
#' @param digits decimal digits for the table generation, default `3`
#' @param ... extra arguments passed to [ggplot2::ggplot()], [ggplot2::geom_pointrange()], [ggplot2::theme()], and [gt::tab_options()]
#'
#' @returns a C-Index forestplot stored in a `gt_tbl` table object
#' @export
#'
#' @examples
#' \dontrun{
#' get_forestplot(data, 0.5)
#' }
get_forestplot <- function(data, center = NULL, digits = 3, ...) {
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
  table <- data |>
    dplyr::mutate(
      estimate_str = sprintf(
        glue::glue("%.{digits}f (%.{digits}f, %.{digits}f)"),
        .data[["estimate"]], .data[["lower"]], .data[["upper"]]
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of("model"),
      names_from = dplyr::all_of("strat"),
      values_from = dplyr::all_of("estimate_str"),
      names_glue = "{strat} (95% CI)"
    ) |>
    dplyr::rename(Model = dplyr::all_of("model")) |>
    dplyr::mutate(plot = dplyr::row_number())

  min_x <- min(data[["lower"]]) * 0.98
  max_x <- max(data[["upper"]]) * 1.02

  model_plots <- data |>
    dplyr::group_by_at("model") |>
    dplyr::group_map(~ {
      get_model_plot(.x, min_x, max_x, center)
    })
  legend_plot <- get_legend(data)
  axis_plot <- get_axis(min_x, max_x)

  model_plots <- append(model_plots, axis_plot)

  extra_row <- table[1, ]
  extra_row <- extra_row |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~""))
  max_index <- max(table[["plot"]]) + 1
  extra_row[["plot"]] <- max_index

  table <- table |>
    dplyr::bind_rows(extra_row)

  table <- table |>
    gt::gt() |>
    gt::cols_width(c("plot") ~ gt::px(150)) |>
    gt::text_transform(
      locations = gt::cells_body(columns = plot),
      fn = function(x) {
        purrr::map(as.numeric(x), ~ {
          row_index <- as.numeric(.x)
          height_row <- ifelse(row_index < max_index, 55, 15)
          img_plot <- gt::html(
            gt::local_image(
              filename = model_plots[[row_index]], height = gt::px(height_row)
            )
          )

          file.remove(model_plots[[row_index]])

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
    gt::tab_header(
      title = "",
      subtitle = gt::html(gt::local_image(filename = legend_plot))
    ) |>
    gt::cols_align(
      align = "auto",
      columns = gt::everything()
    ) |>
    gt::tab_options(...) |>
    gt::tab_options(
      data_row.padding = gt::px(0),
      table_body.border.top.width = gt::px(0),
      table_body.border.bottom.width = gt::px(0),
      table.border.top.width = gt::px(0),
      table.border.bottom.width = gt::px(0)
    )

  return(table)
}

#' clean theme to generate the partial plots
#'
#' @noRd
#' @keywords internal
clean_theme <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text.x = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "gray55"),
      panel.grid.minor.x = ggplot2::element_line(colour = "gray55"),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.spacing = ggplot2::unit(0.25, "lines"),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "gray65", colour = NA),
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )
}

#' save the given plot into a temporal file and returns the path to the file
#'
#' @param p `ggplot2` plot object to be saved
#' @param ... extra arguments for `ggplot2::ggsave` functions
#'
#' @noRd
#' @keywords internal
save_temp_file <- function(p, ...) {
  # store it in a temporal file `svg`
  out_name <- file.path(
    tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".svg")
  )

  ggplot2::ggsave(out_name, plot = p, ...)

  return(out_name)
}

#' generates a model summary plot
#'
#' @param data data for the plot
#' @param min_x min value in x axis
#' @param max_x max value in x axis
#' @param ... extra arguments given to the theme functions
#'
#' @noRd
#' @keywords internal
get_model_plot <- function(data, min_x, max_x, center = NULL, ...) {
  # create the plot
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data[["estimate"]], y = .data[["strat"]], color = .data[["strat"]]), ...) +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = .data[["lower"]], xmax = .data[["upper"]]), ...
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Strat")) +
    ggplot2::xlim(c(min_x, max_x)) +
    clean_theme() +
    # allow the user to redefine the theme
    ggplot2::theme(...)
  if (!is.null(center)) {
    p <- p +
      ggplot2::geom_vline(xintercept = center, linetype = "dashed")
  }

  save_temp_file(
    p,
    dpi = 25.4,
    height = 17,
    width = 150,
    units = "mm",
    device = "svg",
    bg = "transparent"
  )
}

#' Title
#'
#' @param data forestplot data where the `strat` variable is defined
#'
#' @noRd
#' @keywords internal
get_legend <- function(data) {
  legend_df <- data.frame(
    x = 0, y = 0,
    grp = factor(unique(data[["strat"]]))
  )

  p <- ggplot2::ggplot(legend_df, ggplot2::aes(.data[["x"]], .data[["y"]], colour = .data[["grp"]])) +
    ggplot2::geom_point(size = 0, alpha = 0) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 4, alpha = 1))) +
    ggplot2::labs(colour = "") +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "top",
      legend.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )

  save_temp_file(
    p,
    dpi = 25.4,
    height = 8,
    width = 250,
    units = "mm",
    device = "svg",
    bg = "transparent"
  )
}

#' function that generates the xticks to be rendered in the last row of the table
#'
#' @param x_min min value of x axis
#' @param x_max max value of x axis
#'
#' @noRd
#' @keywords internal
get_axis <- function(x_min, x_max) {
  df <- data.frame(x = c(x_min, x_max), y = 0)

  p <- ggplot2::ggplot(df, ggplot2::aes(.data[["x"]], .data[["y"]])) +
    ggplot2::geom_blank() +
    ggplot2::scale_x_continuous(limits = c(x_min, x_max)) +
    clean_theme() +
    ggplot2::theme(
      strip.text.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 12),
    )

  save_temp_file(
    p,
    dpi = 25.4,
    height = 7,
    width = 225,
    units = "mm",
    device = "svg",
    bg = "transparent"
  )
}
