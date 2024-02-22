#
#
# means_1 <- c(4.525, 4.53, 4.535)
# means_2 <- c(3.735, 3.73, 3.725)
#
# sds_1 <- c(1.065, 1.07, 1.075)
# sds_2 <- c(0.925, 0.92, 0.915)
#
#
# # Visualize RIVETS using lines
# #
# # @description `rivets_plot_lines()` displays the relationship between the
# #   rounding bounds of a reported summary statistic (such as means or standard
# #   deviations) and corresponding test statistics (such as *t*).
# #
# #   It is an alternative to [`rivets_plot_cols()`].
# #
# # @param values_1,values_2 Numeric or string coercible to numeric (length 3).
# #   Each vector consists of one reported statistic plus the lower and upper
# #   bounds of the original number, as determined by [`unround()`].
# # @param dir String (length 1). Direction on the x-axis in which the test
# #   statistic increases. Either `"forward"`, `"backward"`, `"inward"`, or
# #   `"outward"`.
# # @param label_stat String. Name of the test statistic that will be found under
# #   each line.
# # @param label_x_axis String (length 2). X-axis labels.
# # @param text_size Numeric. Base text size. Default is `12`.
# # @param color_1,color_2 String. Line colors. Defaults are `"royalblue1"` and
# #   `"red"`.
# # @param tag Tag for the whole plot. Default is [`waiver()`], i.e., no tag.
# #
# # @return
# # @export
# #
# # @examples
#
#
# rivets_plot_lines <- function(values_1,
#                               values_2,
#                               dir,
#                               label_stat,
#                               label_x_axis = NULL,
#                               text_size = 12,
#                               color_1 = "royalblue1",
#                               color_2 = "red",
#                               tag = waiver()) {
#
#   # Checks ---
#
#   check_length(values_1, 3)
#   check_length(values_2, 3)
#   check_length(dir, 1)
#   check_length(label_stat, 1)
#   check_length(text_size, 1)
#
#
#   # Main part ---
#
#   if (is.numeric(values_1)) {
#     labels_numbers_1 <- sort(values_1)
#   } else {
#     labels_numbers_1 <- values_1
#   }
#
#   if (is.numeric(values_2)) {
#     labels_numbers_2 <- sort(values_2)
#   } else {
#     labels_numbers_2 <- values_2
#   }
#
#   labels_min_max_1 <- c(
#     paste0("\n", "(Min ", label_stat, ")"),
#     paste0("\n", "(Reported ", label_stat, ")"),
#     paste0("\n", "(Max ", label_stat, ")")
#   )
#
#   labels_min_max_2 <- rev(labels_min_max_1)
#
#   temp <- color_1
#   color_1 <- color_2
#   color_2 <- temp
#
#   linetypes_1 <- 3:1
#   linetypes_2 <- linetypes_1
#
#   linesizes_1 <- c(0.75, 1, 1.25)
#   linesizes_2 <- linesizes_1
#
#   if (dir == "forward") {
#     labels_min_max_2 <- rev(labels_min_max_2)
#     linetypes_2      <- rev(linetypes_2)
#     linesizes_2      <- rev(linesizes_2)
#   } else if (dir == "backward") {
#     labels_min_max_1 <- rev(labels_min_max_1)
#     linetypes_1      <- rev(linetypes_1)
#     linesizes_1      <- rev(linesizes_1)
#   } else if (dir == "inward") {
#     labels_min_max_1 <- rev(labels_min_max_1)
#     linetypes_1      <- rev(linetypes_1)
#     linesizes_1      <- rev(linesizes_1)
#     labels_min_max_2 <- rev(labels_min_max_2)
#     linetypes_2      <- rev(linetypes_2)
#     linesizes_2      <- rev(linesizes_2)
#   } else if (dir != "outward") {
#     cli::cli_abort(c(
#       "`dir` given as `{dir}`",
#       "x" = "It has to be either \"forward\", \"backward\",
#       \"inward\", or \"outward\"."
#     ))
#   }
#
#
#   # # Create some center space around "0" or "...":
#   # plot_0 <- ggplot2::ggplot() +
#   #   ggplot2::scale_x_continuous(
#   #     breaks = 0, limits = 0, labels = "..."    # "<    ...    <"
#   #   ) +
#   #   ggplot2::theme_minimal() +
#   #   ggplot2::theme(
#   #     panel.grid = ggplot2::element_blank()
#   #   ) +
#   #   ggplot2::labs(x = "")
#
#
#   # Plot 1:
#   plot_1 <- ggplot2::ggplot() +
#     ggplot2::geom_vline(
#       xintercept = means_1, linetype = linetypes_1,
#       color = color_1, size = linesizes_1   # size = 0.75
#     ) +
#     ggplot2::scale_x_continuous(
#       n.breaks = 3,
#       labels = paste(labels_numbers_1, labels_min_max_1)
#     ) +
#     ggplot2::theme_minimal(base_size = text_size) +
#     ggplot2::theme(
#       panel.grid = ggplot2::element_blank()
#     )
#
#
#   # Plot 2:
#   plot_2 <- ggplot2::ggplot() +
#     ggplot2::geom_vline(
#       xintercept = means_2, linetype = linetypes_2,
#       color = color_2, size = linesizes_2
#     ) +
#     ggplot2::scale_x_continuous(
#       n.breaks = 3,
#       labels = paste(labels_numbers_2, labels_min_max_2)
#     ) +
#     ggplot2::labs(tag = tag) +
#     ggplot2::theme_minimal(base_size = text_size) +
#     ggplot2::theme(
#       panel.grid = ggplot2::element_blank()
#     )
#
#
#   if (!is.null(label_x_axis)) {
#     label_x_axis <- rev(label_x_axis)
#     plot_1 <- plot_1 + ggplot2::labs(x = label_x_axis[1L])
#     plot_2 <- plot_2 + ggplot2::labs(x = label_x_axis[2L])
#   } else {
#     plot_1 <- plot_1 + ggplot2::labs(x = NULL)
#     plot_2 <- plot_2 + ggplot2::labs(x = NULL)
#   }
#
#
#   # Create and return a composite plot:
#   patchwork::wrap_plots(
#     plot_2,
#     patchwork::plot_spacer(),
#     # plot_0,
#     # patchwork::plot_spacer(),
#     plot_1,
#     widths = c(2, 1.25, 2)  # c(8, 1, 2, 1, 8)
#   )
#
# }
#
#
#
# rivets_plot_lines(     # plot_means <-
#   values_1 = means_1, values_2 = means_2, dir = "outward",
#   label_stat = "t", label_x_axis = c("Mean 2", "Mean 1"),
#   tag = "a"
# )
# #
# # plot_sds <- rivets_plot_lines(
# #   values_1 = sds_1, values_2 = sds_2, dir = "backward",
# #   label_stat = "t",
# #   label_x_axis = c("SD 2", "SD 1"),
# #   tag = "b"
# # )
# #
# # plot_both <- patchwork::wrap_plots(plot_means / plot_sds)
#
#
#
# # Description -------------------------------------------------------------
#
# # Three distinct scenarios of independent-sample t-tests. These scenarios follow
# # from reconstructing the bounds of the numbers which were presumably rounded to
# # the reported summary statistics. These scenarios correspond to the reported t
# # value (Reported t; dashed lines) as well as the minimal (Min t; dotted lines)
# # and maximal (Max t; solid lines) t-values consistent with the rounding bounds
# # of the values presented. (a) Means which stand further apart from each other
# # correspond to greater t-values. (b) Lower standard deviations correspond to
# # greater t-values.
#
#
#
# # TO DO: ------------------------------------------------------------------
#
# # (1) Replace the `mirror = TRUE` algorithm by one that preserves a single
# # linear scale, i.e., one with "<    ...    <". Right now, it's just too much
# # mental gymnastics!
#
# # (2) Write an algorithm that determines the left-right sorting of plots by
# # their underlying numeric values, such that smaller values go on the left.
# # Above, I did this manually. It might look strange that Mean 2 is on the left
# # and Mean 1 is on the right, but this order is determined by the data.
#
# # (3) If labels are wanted, add a y-axis. It won't need to be displayed, but
# # it's necessary for labels. Then, add labels.
#
# # (4) As an alternative to a standalone function, write a generic function for
# # which the present visualization would then be a method for a class such as
# # `scr_rivets_t_test`. Afterwards, add patchwork to Suggests in DESCRIPTION.
#
#
#
