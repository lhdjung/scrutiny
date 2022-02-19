

means_1 <- c(4.525, 4.53, 4.535)
means_2 <- c(3.735, 3.73, 3.725)

sds_1 <- c(1.065, 1.07, 1.075)
sds_2 <- c(0.925, 0.92, 0.915)

values_1 = means_1
values_2 = means_2
dir = "outward"
label_x_axis = NULL
label_y_axis = "Test statistic"
text_size = 12
color_1 = "royalblue1"
color_2 = "red"
tag = ggplot2::waiver()


#' Visualize RIVETS using columns
#'
#' `rivets_plot_cols()` displays the relationship between the rounding bounds of
#' a reported summary statistic (such as means or standard deviations) and a
#' corresponding test statistic (such as *t*).
#'
#' @param values_1,values_2 Numeric or string coercible to numeric (length 3).
#'   Each vector consists of one reported statistic plus the lower and upper
#'   bounds of the original number, as determined by `unround()`.
#' @param dir String (length 1). Direction on the x-axis in which the test
#'   statistic increases. Either `"forward"`, `"backward"`, `"inward"`, or
#'   `"outward"`.
#' @param label_x_axis String (length 2). X-axis labels. They should signify the
#'   summary statistic.
#' @param label_y_axis String (length 1). Y-axis label. It should signify the
#'   test statistic. Default is `"Test statistic"`.
#' @param text_size Numeric. Base text size. Default is `12`.
#' @param color_1,color_2 String. Line colors. Defaults are `"royalblue1"` and
#'   `"red"`.
#' @param tag Tag for the whole plot. Default is `waiver()`, i.e., no tag.
#'
#' @return
#' @export
#'
#' @references Brown, N. J. L., & Heathers, J. (2019). Rounded Input Variables,
#'   Exact Test Statistics (RIVETS). PsyArXiv.
#'   https://doi.org/10.31234/osf.io/ctu9z
#'
#' @examples
#' # In the t-test context, a pair of means
#' # along with their rounding bounds:
#' rivets_plot_cols(
#'   values_1 = c(4.525, 4.53, 4.535),
#'   values_2 = c(3.735, 3.73, 3.725),
#'   dir = "outward",
#'   label_x_axis = c("Mean 2", "Mean 1"),
#'   label_y_axis = "Test statistic (t)",
#'   tag = "a"
#' )
#'
#' # Also in the t-test context,
#' # the same for standard deviations:
#' rivets_plot_cols(
#'   values_1 = c(1.065, 1.07, 1.075),
#'   values_2 = c(0.925, 0.92, 0.915),
#'   dir = "backward",
#'   label_x_axis = c("SD 2", "SD 1"),
#'   label_y_axis = "Test statistic (t)",
#'   tag = "b"
#' )


rivets_plot_cols <- function(values_1,
                             values_2,
                             dir,
                             label_x_axis = NULL,
                             label_y_axis = "Test statistic",
                             text_size = 12,
                             color_1 = "royalblue1",
                             color_2 = "red",
                             alpha_1 = 0.5,
                             alpha_2 = 0.5,
                             tag = ggplot2::waiver()) {


  # Checks ---

  if (!is.null(label_x_axis)) {
    if (length(label_x_axis) != 2) {
      cli::cli_abort(c(
        "`label_x_axis` has length {length(label_x_axis)}",
        "x" = "It needs to have length 0 or 2."
      ))
    }
  }

  check_length(values_1, "values_1", 3)
  check_length(values_2, "values_2", 3)
  check_length(dir, "dir", 1)
  # check_length(label_x_axis, "label_x_axis", 2)
  check_length(label_y_axis, "label_y_axis", 1)
  check_length(text_size, "text_size", 1)


  # Main part ---

  if (is.numeric(values_1)) {
    labels_numbers_1 <- sort(values_1)
  } else {
    labels_numbers_1 <- values_1
  }

  if (is.numeric(values_2)) {
    labels_numbers_2 <- sort(values_2)
  } else {
    labels_numbers_2 <- values_2
  }

  labels_min_max_1 <- as.factor(c("Min", "Reported", "Max"))

  temp <- values_1
  values_1 <- values_2
  values_2 <- temp

  col_sizes_1 <- 3:1
  col_sizes_2 <- col_sizes_1

  if (dir == "forward") {
    col_sizes_1 <- rev(col_sizes_1)
    col_sizes_2 <- rev(col_sizes_2)
  } else if (dir == "inward") {
    col_sizes_2 <- rev(col_sizes_2)
  } else if (dir == "outward") {
    col_sizes_1 <- rev(col_sizes_1)
  } else if (dir != "backward") {
    cli::cli_abort(c(
      "`dir` given as `{dir}`",
      "x" = "It has to be either \"forward\", \"backward\",
      \"inward\", or \"outward\"."
    ))
  }

  df <- tibble::tibble(
    col_sizes_1, col_sizes_2,
    labels_min_max_1,
    labels_numbers_1 = factor(labels_numbers_1, levels = labels_numbers_1),
    labels_numbers_2 = factor(labels_numbers_2, levels = labels_numbers_2)
  )


  # Right-side plot:
  plot_1 <- ggplot2::ggplot(df, ggplot2::aes(
    x = labels_numbers_1,
    y = col_sizes_1
  )) +
    ggplot2::geom_col(fill = color_2, alpha = alpha_2) +    # col_1_args
    ggplot2::scale_y_continuous(
      labels = NULL,
      breaks = 1:3,
      expand = ggplot2::expansion(c(0, 0.05))
    ) +
    ggplot2::labs(y = NULL) +
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position = "none"
    )


  # Left-side plot:
  plot_2 <- ggplot2::ggplot(df, ggplot2::aes(
    x = labels_numbers_2,
    y = col_sizes_2
  )) +
    ggplot2::geom_col(fill = color_1, alpha = alpha_1) +    # col_2_args
    ggplot2::scale_y_continuous(
      labels = labels_min_max_1,
      breaks = 1:3,
      expand = ggplot2::expansion(c(0, 0.05))
    ) +
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::labs(
      y = label_y_axis,
      tag = tag
    )


  if (is.null(label_x_axis)) {
    plot_1 <- plot_1 + ggplot2::labs(x = NULL)
    plot_2 <- plot_2 + ggplot2::labs(x = NULL)
  } else {
    plot_1 <- plot_1 + ggplot2::labs(x = label_x_axis[2])
    plot_2 <- plot_2 + ggplot2::labs(x = label_x_axis[1])
  }


  # Create and return a composite plot:
  patchwork::wrap_plots(
    plot_2,
    patchwork::plot_spacer(),
    plot_1,
    widths = c(2.5, 1, 2.5)
  )

}



# Examples ----------------------------------------------------------------

# label_test_stat <- expression(paste(
#   "Test statistic (", italic("t"), ")"
# ))
#
# tag_a <- expression(bold("a"))
# tag_b <- expression(bold("b"))
#
# plot_means <- rivets_plot_cols(
#   values_1 = means_1,
#   values_2 = means_2,
#   dir = "outward",
#   label_x_axis = c("Mean 2", "Mean 1"),
#   label_y_axis = label_test_stat,
#   tag = tag_a
# )
#
# plot_sds <- rivets_plot_cols(
#   values_1 = sds_1,
#   values_2 = sds_2,
#   dir = "backward",
#   label_x_axis = c("Standard deviation 2", "Standard deviation 1"),
#   label_y_axis = label_test_stat,
#   tag = tag_b
# )
#
# plot_both <- patchwork::wrap_plots(plot_means / plot_sds)


# ggplot2::ggsave(
#   "rivets_plot_cols", device = "png",
#   width = 16, height = 14,
#   units = "cm"
# )



