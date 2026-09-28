#' Visualize DEBIT results
#'
#' @description  Plot a distribution of binary data and their mutual DEBIT
#'   consistency. Call this function only on a data frame that resulted from a
#'   call to [`debit_map()`].
#'
#'   Various parameters of the individual geoms can be controlled via arguments.
#'
#' @details The labels are created via [`ggrepel::geom_text_repel()`], so the
#'   algorithm is designed to minimize overlap with the data points and other labels.
#'   Yet, they don't take the DEBIT line into account, and their locations are
#'   ultimately random. You might therefore have to resize the plot or run the
#'   function a few times until the labels are localized in a satisfactory way.
#'
#'   The DEBIT line depends on the sample size. If `n` varies in `data`, the
#'   lines for the smallest and largest `n` are drawn, and the band between them
#'   is shaded: it contains the lines for all the other `n` values, which would
#'   be too close together to tell apart.
#'
#'   With `formula = "exact"`, the default of [`debit_map()`], a rectangle
#'   crossing the line is not enough. A mean of `n` binary values is `k / n` for
#'   a whole number `k`, and these means are marked as points on the line. A
#'   value set is consistent if one of them lies inside its rectangle. With a
#'   large `n`, these points are too dense to tell apart, so only those that
#'   would not be drawn over by others are drawn, with a message.
#'
#'   Value sets that lack a verdict or bounds (i.e., have
#'   `NA` in any of the columns needed for drawing them) are left out with a
#'   warning.
#'
#'   An alternative to the present function would be an S3 method for
#'   [`ggplot2::autoplot()`]. However, a standalone function such as this allows
#'   for customizing geom parameters and might perhaps provide better
#'   accessibility overall.
#'
#' @param data Data frame. Result of a call to [`debit_map()`].
#' @param show_labels Logical. Should the data points have labels (of the form
#'   "mean; SD")? Default is `TRUE`.
#' @param show_full_scale Logical. Should the plot be fixed to full scale,
#'   showing the entire consistency line independently of the data? Default is
#'   `TRUE`.
#' @param show_theme_other Logical. Should the theme be modified in a way
#'   fitting the plot structure? Default is `TRUE`.
#' @param color_cons,color_incons Strings. Colors of the geoms representing
#'   consistent and inconsistent values, respectively.
#' @param rect_alpha Parameter of the DEBIT rectangles. (Due to the nature of
#'   the data mapping, there can be no leeway regarding the shape or size of
#'   this particular geom.) Each rectangle is drawn over a point at the reported
#'   mean and SD, so that rectangles too small to see still show up.
#' @param line_alpha,line_color,line_linetype,line_width Parameters of
#'   the curved DEBIT line.
#' @param
#' label_alpha,label_linetype,label_size,label_linesize,label_force,label_force_pull,label_padding
#' Parameters of the labels showing mean and SD values. Passed on to
#' [`ggrepel::geom_text_repel()`]; see there for more information.

#' @include debit-map.R restore-zeros.R utils.R
#'
#' @return A ggplot object. It is returned, not printed, so it can be assigned
#'   or added to without drawing a plot. At the console, auto-printing draws it.
#'
#' @references Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A
#'   Simple Consistency Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @export
#'
#' @examplesIf rlang::is_installed("ggrepel")
#' # Run `debit_plot()` on the output
#' # of `debit_map()`:
#' pigs3 |>
#'   debit_map(digits_x = 2, digits_sd = 2) |>
#'   debit_plot()

debit_plot <- function(
  data,
  show_labels = TRUE,
  show_full_scale = TRUE,
  show_theme_other = TRUE,
  color_cons = "royalblue1",
  color_incons = "red",
  line_alpha = 1,
  line_color = "black",
  line_linetype = 1,
  line_width = 0.5,
  rect_alpha = 1,
  label_alpha = 0.5,
  label_linetype = 3,
  label_size = 3.5,
  label_linesize = 0.75,
  label_force = 175,
  label_force_pull = 0.75,
  label_padding = 0.5
) {
  # Checks ---

  if (!inherits(data, "scrutiny_debit_map")) {
    cli::cli_abort(c(
      "!" = "`debit_plot()` only works with DEBIT results.",
      "x" = "`data` is not `debit_map()` output."
    ))
  }

  # The formula decides what the plot shows. `debit_map()` and the mappers built
  # on it always record it, and the record survives the same dplyr verbs as the
  # class, so data with the class but no formula were given the class by hand:
  formula <- scrutiny_meta(data)$args$formula
  if (!is.character(formula) || !formula %in% c("exact", "mean_n")) {
    cli::cli_abort(c(
      "!" = "`data` doesn't record the DEBIT `formula` it was tested with.",
      "x" = "It has the class of `debit_map()` output, but not the \\
      \"scrutiny\" attribute that `debit_map()` gives its output.",
      "i" = "Plot the output of `debit_map()` itself."
    ))
  }

  # Preparations ---

  # A value set without a verdict has no color, and one without bounds has no
  # rectangle to draw, so ggplot2 would drop it -- or fail on the scale breaks,
  # which are computed from the bounds. Drop such rows here, but say so:
  cols_needed <- c(
    "x",
    "sd",
    "n",
    "consistency",
    "sd_lower",
    "sd_upper",
    "x_lower",
    "x_upper",
    "sum_lower",
    "sum_upper"
  )

  # The bounds only exist in `debit_map()` output with `show_rec = TRUE`, the
  # default. Without them, subsetting used to fail with a vctrs error:
  cols_missing <- setdiff(cols_needed, colnames(data))
  if (length(cols_missing) > 0L) {
    cli::cli_abort(c(
      "!" = "`data` lacks columns that `debit_plot()` needs.",
      "x" = "Missing: {.code {cols_missing}}.",
      "i" = "Call `debit_map()` with `show_rec = TRUE`, the default."
    ))
  }

  is_complete <- stats::complete.cases(data[cols_needed])

  if (!all(is_complete)) {
    if (!any(is_complete)) {
      cli::cli_abort(c(
        "!" = "No value set in `data` can be drawn.",
        "x" = "Each of the {nrow(data)} row{?s} has a missing value in at \
      least one of these columns: {.code {cols_needed}}."
      ))
    }

    n_dropped <- sum(!is_complete)
    cli::cli_warn(c(
      "!" = "Dropping {n_dropped} value set{?s} from the plot.",
      "i" = "{cli::qty(n_dropped)}{?It has/They have} a missing value in \
      at least one of these columns: {.code {cols_needed}}."
    ))
    data <- data[is_complete, ]
  }

  sd_num <- data$sd
  x_num <- data$x
  n <- data$n
  consistency <- data$consistency
  sd_lower <- data$sd_lower
  sd_upper <- data$sd_upper
  x_lower <- data$x_lower
  x_upper <- data$x_upper

  # `x` and `sd` are numeric, so trailing zeros are lost: an SD of `0.50` would
  # be labeled `0.5`. Restore them from the decimal counts the mapper stored:
  x <- x_num
  sd <- sd_num
  if (all(c("digits_x", "digits_sd") %in% colnames(data))) {
    x <- restore_zeros(x_num, width = data$digits_x)
    sd <- restore_zeros(sd_num, width = data$digits_sd)
  }
  value_labels <- paste0(x, "; ", sd)

  color_by_consistency <- dplyr::if_else(
    consistency,
    color_cons,
    color_incons
  )

  # The plot itself ---

  p <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = {{ x_num }},
      y = {{ sd_num }},
      label = {{ value_labels }}
    )
  )

  # DEBIT line: the SD of binary data as a function of their mean. It depends on
  # `n`, and it falls as `n` rises, so the lines for all the distinct `n` values
  # lie between those for the smallest and the largest one. For realistic sample
  # sizes, these lines are so close that drawing each of them would blur them
  # into one thick, uneven stroke. Instead, only the two outer lines are drawn,
  # and the band between them is shaded. With a single `n`, both outer lines are
  # the same, so the band is not needed. The lines don't inherit the `label`
  # aesthetic, which they have no use for.

  debit_line <- function(n_line) {
    function(x) sqrt((n_line / (n_line - 1)) * (x * (1 - x)))
  }
  n_range <- range(n)

  if (n_range[1] != n_range[2]) {
    x_grid <- seq(0, 1, length.out = 201)
    p <- p +
      ggplot2::geom_ribbon(
        data = tibble::tibble(
          x = x_grid,
          ymin = debit_line(n_range[2])(x_grid),
          ymax = debit_line(n_range[1])(x_grid)
        ),
        ggplot2::aes(
          x = .data$x,
          ymin = .data$ymin,
          ymax = .data$ymax
        ),
        fill = line_color,
        alpha = line_alpha * 0.25,
        na.rm = TRUE,
        inherit.aes = FALSE
      )
  }

  debit_lines <- n_range |>
    unique() |>
    lapply(function(n_line) {
      ggplot2::geom_function(
        fun = debit_line(n_line),
        alpha = line_alpha,
        color = line_color,
        linetype = line_linetype,
        linewidth = line_width,
        na.rm = TRUE,
        inherit.aes = FALSE
      )
    })

  p <- p + debit_lines

  # A point at the reported mean and SD. Unlike the rectangle drawn over it, it
  # has a fixed size, so that a value set whose rectangle is too small to see --
  # as with three or more decimal places -- still shows up:
  p <- p +
    ggplot2::geom_point(color = color_by_consistency, size = 1.5)

  # Rectangles that should cross the consistency line:
  p <- p +
    ggplot2::geom_rect(
      xmin = x_lower,
      xmax = x_upper,
      ymin = sd_lower,
      ymax = sd_upper,
      color = color_by_consistency,
      fill = color_by_consistency,
      alpha = rect_alpha
    )

  # Scale specifications (optional, default is `TRUE`): The y-axis has some room
  # beyond the outermost rectangles. This used to be the outer tiles' offset
  # from the rectangles:
  sd_margin <- 0.025

  if (show_full_scale) {
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 1, 0.1),
        limits = c(0, 1)
      ) + # might or might not change: , limits = c(0, 1)
      ggplot2::scale_y_continuous(
        breaks = seq(0, (max(sd_upper) + sd_margin), 0.05)
      ) +
      # Limit the y-axis on the coordinates, not the scale: a scale limit drops
      # every point of the band and lines that leaves it, rather than clipping.
      ggplot2::coord_cartesian(
        ylim = c(
          min(sd_lower) - sd_margin,
          max((max(sd_upper) + sd_margin), 0.5)
        )
      )
  }

  # Under `formula = "exact"`, the default, a value set is consistent if the SD
  # of some mean that `n` binary values can have, `k / n`, lies in its rectangle
  # -- not if the line merely crosses it. Mark those means on the line, from
  # `sum_lower` to `sum_upper` ones for each value set. This comes after the
  # scales because thinning the marks needs the final panel.
  if (formula == "exact") {
    k_lower <- data$sum_lower
    k_upper <- data$sum_upper
    k_count <- pmax(k_upper - k_lower + 1, 0)

    if (sum(k_count) <= 10000) {
      k_by_row <- purrr::map2(k_lower, k_upper, function(k_lower, k_upper) {
        if (k_lower > k_upper) {
          numeric(0)
        } else {
          k_lower:k_upper
        }
      })
    } else {
      panel <- ggplot2::ggplot_build(p)$layout$panel_params[[1L]]
      k_by_row <- purrr::pmap(
        list(k_lower = k_lower, k_upper = k_upper, n = n),
        thin_binary_means,
        x_range = panel$x.range,
        y_range = panel$y.range
      )
      n_thinned <- sum(lengths(k_by_row) < k_count)
      if (n_thinned > 0L) {
        cli::cli_inform(c(
          "i" = "Marking only some of the attainable means of \\
          {n_thinned} value set{?s}.",
          " " = "The others would be drawn over them at this plot's \\
          resolution, so the plot looks the same."
        ))
      }
    }

    k <- unlist(k_by_row)
    n_by_k <- rep(n, lengths(k_by_row))

    if (length(k) > 0L) {
      p <- p +
        ggplot2::geom_point(
          data = tibble::tibble(x = k / n_by_k, y = sd_binary_1_n(k, n_by_k)),
          ggplot2::aes(x = .data$x, y = .data$y),
          color = line_color,
          size = 1,
          inherit.aes = FALSE
        )
    }
  }

  # Text labels (optional, default is `TRUE`):
  if (show_labels) {
    rlang::check_installed("ggrepel", "for the labels in `debit_plot()`.")
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

  # Axis labels:
  p <- p +
    ggplot2::labs(x = "Ratio of n1/n2", y = "Standard deviation")

  # Other theme specifications (optional, default is `TRUE`):
  if (show_theme_other) {
    p <- p +
      ggplot2::theme_update() +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_line()
      )
  }

  # Return the plot -- returned, not printed, like `grim_plot()`'s.
  # Auto-printing draws it at the console anyway, whereas an explicit `print()`
  # here drew a canvas whenever the result was assigned or added to. Nor are
  # warnings suppressed: rows that can't be drawn are reported above.
  p
}


# The number of ones `k`, from `k_lower` to `k_upper`, whose means
# `debit_plot()` marks on the DEBIT line, thinned to those that a plot can tell
# apart. Divide the panel, which spans `x_range` and `y_range`, into a grid of
# `resolution` cells along each axis. As `k` rises, the line enters a cell by
# crossing a grid line, so the first `k` after some crossing is the first mark
# in that cell, if the cell has any. Keeping only those, plus `k_lower`, keeps
# every cell that has a mark: the plot looks the same, with a few thousand marks
# per value set at most, rather than one for every `k`.
thin_binary_means <- function(
  k_lower,
  k_upper,
  n,
  x_range,
  y_range,
  resolution = 2000
) {
  x_lines <- seq(x_range[1L], x_range[2L], length.out = resolution + 1)
  y_lines <- seq(y_range[1L], y_range[2L], length.out = resolution + 1)

  # The mean `k / n` crosses a vertical line once. The SD, the square root of `k
  # * (n - k) / (n * (n - 1))`, crosses a horizontal one twice, on its way up
  # and on its way down:
  discriminant <- n^2 / 4 - y_lines^2 * n * (n - 1)
  root <- sqrt(discriminant[discriminant >= 0])
  crossings <- c(x_lines * n, n / 2 - root, n / 2 + root)

  # The first mark after a crossing is `floor() + 1`. Its neighbors are added
  # because a mark right on a grid line can fall on either side of it, as can a
  # crossing that floating-point error moves by a hair:
  k <- c(
    k_lower,
    crossings |>
      floor() |>
      outer(0:2, `+`)
  )

  k[k >= k_lower & k <= k_upper] |>
    unique() |>
    sort()
}
