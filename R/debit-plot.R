
#' Visualize DEBIT results
#'
#' @description  Plot a distribution of binary data and their mutual DEBIT
#'   consistency. Call this function only on a data frame that resulted from a
#'   call to `debit_map()`.
#'
#'   Various parameters of the individual geoms can be controlled via arguments.
#'
#' @details The labels are created via `ggrepel::geom_text_repel()`, so the
#'   algorithm is designed to minimize overlap with the tiles and other labels.
#'   Yet, they don't take the DEBIT line into account, and their locations are
#'   ultimately random. You might therefore have to resize the plot or run the
#'   function a few times until the labels are localized in a satisfactory way.
#'
#'   An alternative to the present function would be an S3 method for
#'   `ggplot2::autoplot()`. However, a standalone function such as this allows
#'   for customizing geom parameters and might perhaps provide better
#'   accessibility overall.
#'
#' @param data Data frame. Result of a call to `debit_map()`.
#' @param show_outer_boxes Boolean. Should outer tiles surround the actual data
#'   points, making it easier to spot them and to assess their overlap? Default
#'   is `TRUE`.
#' @param show_labels Boolean. Should the data points have labels (of the form
#'   "mean; SD")? Default is `TRUE`.
#' @param show_full_scale Boolean. Should the plot be fixed to full scale,
#'   showing the entire consistency line independently of the data? Default is
#'   `TRUE`.
#' @param show_theme_other Boolean. Should the theme be modified in a way
#'   fitting the plot structure? Default is `TRUE`.
#' @param color_cons,color_incons Strings. Colors of the geoms representing
#'   consistent and inconsistent values, respectively.
#' @param rect_alpha Parameter of the DEBIT rectangles. (Due to the nature of
#'   the data mapping, there can be no leeway regarding the shape or size of
#'   this particular geom.)
#' @param line_alpha,line_color,line_linetype,line_width,line_size Parameters of
#'   the curved DEBIT line.
#' @param
#' tile_alpha,tile_height_offset,tile_width_offset,tile_height_min,tile_width_min
#' Parameters of the outer tiles surrounding the DEBIT rectangles. Offset refers
#' to the distance from the rectangles within.
#' @param
#' label_alpha,label_linetype,label_size,label_linesize,label_force,label_force_pull,label_padding
#' Parameters of the labels showing mean and SD values. Passed on to
#' `ggrepel::geom_text_repel()`; see there for more information.

#' @include debit-map.R restore-zeros.R utils.R
#'
#' @return A ggplot object.
#'
#' @references Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A
#'   Simple Consistency Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @export
#'
#' @examples
#' # Run `debit_plot()` on the output
#' # of `debit_map()`:
#' pigs3 %>%
#'   debit_map() %>%
#'   debit_plot()



debit_plot <- function(data,
                       show_outer_boxes = TRUE,
                       show_labels = TRUE,
                       show_full_scale = TRUE,
                       show_theme_other = TRUE,
                       color_cons = "royalblue1",
                       color_incons = "red",
                       line_alpha = 1,
                       line_color = "black",
                       line_linetype = 1,
                       line_width = 0.5,
                       line_size = 0.5,
                       rect_alpha = 1,
                       tile_alpha = 0.15,
                       tile_height_offset = 0.025,
                       tile_width_offset = 0.025,
                       tile_height_min = 0.0375,
                       tile_width_min = 0.0385,
                       label_alpha = 0.5,
                       label_linetype = 3,
                       label_size = 3.5,
                       label_linesize = 0.75,
                       label_force = 175,
                       label_force_pull = 0.75,
                       label_padding = 0.5) {


  # Checks ---

  if (!inherits(data, "scr_debit_map")) {
    cli::cli_abort(c(
      "!" = "`debit_plot()` only works with DEBIT results.",
      "x" = "`data` is not `debit_map()` output."
    ))
  }


  # Preparations ---

  sd <- data$sd
  x <- data$x
  n <- data$n
  consistency <- data$consistency
  sd_lower <- data$sd_lower
  sd_upper <- data$sd_upper
  x_lower <- data$x_lower
  x_upper <- data$x_upper

  sd_num <- as.numeric(sd)
  x_num <- as.numeric(x)

  value_labels <- paste0(x, "; ", sd)

  tile_height <- sd_upper - sd_lower + tile_width_offset
  tile_width <- x_upper - x_lower + tile_height_offset

  #  max_y <- max(sd_upper) + tile_width_offset
  #  min_y <- min(sd_lower) - tile_width_offset

  color_by_consistency <- dplyr::if_else(
    consistency, color_cons, color_incons
  )


  # The plot itself ---

  p <- ggplot2::ggplot(data = data, ggplot2::aes(
    x     = {{ x_num }},
    y     = {{ sd_num }},
    label = {{ value_labels }}
  ))

  # DEBIT line:
  draw_debit_line <- function(.x = x, .n = n, .label = p$label) {
    suppressWarnings(sqrt((.n / (.n - 1)) * (.x * (1 - .x))))
  }

  if (utils::packageVersion("ggplot2") >= 3.4) {
    check_ggplot2_size(line_size, 0.5)
    p <- p +
      ggplot2::geom_function(
        fun = draw_debit_line,
        alpha = line_alpha,
        color = line_color,
        linetype = line_linetype,
        linewidth = line_width,
        na.rm = TRUE
      )
  } else {
    check_ggplot2_linewidth(line_width, 0.5)
    p <- p +
      ggplot2::geom_function(
        fun = draw_debit_line,
        alpha = line_alpha,
        color = line_color,
        linetype = line_linetype,
        size = line_size,
        na.rm = TRUE
      )
  }

  # if (utils::packageVersion("ggplot2") >= 3.4) {
  #   if (line_size != 0.5) {
  #     msg1 <- paste0(
  #       "That's because your ggplot2 version is >= 3.4.0 (actually, ",
  #       utils::packageVersion("ggplot2"), ")."
  #     )
  #     msg2 <- paste(
  #       "In ggplot2, the `size` aesthetic has been deprecated since",
  #       "version 3.4.0."
  #     )
  #     msg3 <- "See https://www.tidyverse.org/blog/2022/11/ggplot2-3-4-0/#hello-linewidth"
  #     cli::cli_abort(c(
  #       "`line_size` is deprecated for you.",
  #       "x" = msg1,
  #       "i" = msg2,
  #       "i" = msg3
  #     ))
  #   }
  #   p <- p +
  #     ggplot2::geom_function(
  #       fun = draw_debit_line,
  #       alpha = line_alpha,
  #       color = line_color,
  #       linetype = line_linetype,
  #       linewidth = line_width,
  #       na.rm = TRUE
  #     )
  # } else {
  #   if (line_width != 0.5) {
  #     msg1 <- paste0(
  #       "That's because your ggplot2 version is < 3.4.0 (actually, ",
  #       utils::packageVersion("ggplot2"), ")."
  #     )
  #     msg2 <- paste(
  #       "In ggplot2, the `size` aesthetic has been deprecated since",
  #       "version 3.4.0. The `linewidth` aesthetic is used as a replacement,",
  #       "but it's not accessible for versions lower than 3.4.0."
  #     )
  #     msg3 <- "See https://www.tidyverse.org/blog/2022/11/ggplot2-3-4-0/#hello-linewidth"
  #     cli::cli_abort(c(
  #       "You can't use `line_width`.",
  #       "x" = msg1,
  #       "i" = msg2,
  #       "i" = msg3
  #     ))
  #   }
  #   p <- p +
  #     ggplot2::geom_function(
  #       fun = draw_debit_line,
  #       alpha = line_alpha,
  #       color = line_color,
  #       linetype = line_linetype,
  #       size = line_size,
  #       na.rm = TRUE
  #     )
  # }

  # Inner tiles that should cross the consistency line:
  p <- p +
    ggplot2::geom_rect(
      xmin = x_lower, xmax = x_upper,
      ymin = sd_lower, ymax = sd_upper,
      color = color_by_consistency,
      fill = color_by_consistency,
      alpha = rect_alpha
    )

  # Outer tiles that point out where the "rectangles" are (optional, default
  # is `TRUE`):
  if (show_outer_boxes) {
    p <- p +
      ggplot2::geom_tile(
        height = max(tile_height, tile_height_min),
        width = max(tile_width, tile_width_min),
        fill = color_by_consistency, alpha = tile_alpha
      )
  }

  # Text labels (optional, default is `TRUE`):
  if (show_labels) {
    p <- p +
      ggrepel::geom_text_repel(
        force = label_force,
        force_pull = label_force_pull,
        box.padding = label_padding,
        segment.alpha = label_alpha,
        color = color_by_consistency,
        segment.color = color_by_consistency,
        segment.linetype = label_linetype,
        segment.size = label_linesize,
        size = label_size
      )
  }

  # Scale specifications (optional, default is `TRUE`):
  if (show_full_scale) {
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 1, 0.1), limits = c(0, 1)
      ) +  # might or might not change: , limits = c(0, 1)
      ggplot2::scale_y_continuous(
        breaks = seq(0, (max(sd_upper) + tile_width_offset), 0.05),
        limits = c(min(sd_lower) - tile_width_offset,
                   max((max(sd_upper) + tile_width_offset), 0.5))
      )  # used to be 0.005
  }

  # Axis labels:
  p <- p +
    ggplot2::labs(x = "Ratio of n1/n2", y = "Standard deviation")

  # Other theme specifications (optional, default is `TRUE`):
  if (show_theme_other) {
    p <- p +
      ggplot2::theme_update() +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_line(seq(0, 0.5, 0.1))
      )
  }

  # Finally, return the plot while suppressing unnecessary ggplot2 warnings:
  suppressWarnings(print(p))
}


