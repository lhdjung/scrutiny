#' Visualize DEBIT results
#'
#' @description `debit_plot()` plots the output of [`debit_map()`]. Each value
#'   set is a rectangle spanning the rounding bounds of its mean and SD, in
#'   blue if consistent and red if not, against the DEBIT line: the SD of
#'   binary data as a function of their mean, for the given sample size.
#'
#' @details The line is that of the sample SD unless [`debit_map()`] was called
#'   with `sd_meaning = "sd_pop"`, which shows the population SD. Under
#'   `sd_meaning = "sd_sample_or_pop"`, the lines for both are drawn. Under
#'   `"se"`, the line is that of the SE.
#'
#'   The means that `n` binary values can have, `k / n`, are marked as
#'   points on the line. Under `formula = "exact"`, the default of
#'   [`debit_map()`], a value set is consistent if one of these points lies
#'   inside its rectangle. If `n` is large, only the points that would not be
#'   drawn over by others are drawn, with a message.
#'
#'   If `n` varies in `data`, the lines for the smallest and largest `n` are
#'   drawn and the band between them is shaded; it contains the lines for all
#'   other `n`.
#'
#'   A rectangle can be too small to see, so each is drawn over a point at the
#'   reported mean and SD. Value sets with `NA` in a column needed for drawing
#'   are left out with a warning.
#'
#'   The labels come from [`ggrepel::geom_text_repel()`], which places them at
#'   random to minimize overlap. Resizing the plot or running the function
#'   again may give better placements.
#'
#'   Colors come from one scale for `colour` and `fill`, mapped to
#'   `consistency`. Add another to change them, as in the examples.
#'
#' @param data Data frame returned by [`debit_map()`].
#' @param show_labels Logical. Should the value sets be labeled with
#'   "mean; SD"? Default is `TRUE`.
#' @param show_full_scale Logical. If `TRUE`, the x-axis spans the whole DEBIT
#'   line, from 0 to 1, regardless of the data. Default is `TRUE`.
#' @param show_theme_other Logical. If `TRUE`, minor grid lines are removed and
#'   y-axis ticks are added. Default is `TRUE`.
#'
#' @include debit-map.R restore-zeros.R utils.R
#'
#' @return A ggplot object.
#'
#' @references Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A
#'   Simple Consistency Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @export
#'
#' @examplesIf rlang::is_installed("ggrepel")
#' pigs3 |>
#'   debit_map(digits_x = 2, digits_sd = 2) |>
#'   debit_plot()
#'
#' # Other colors:
#' pigs3 |>
#'   debit_map(digits_x = 2, digits_sd = 2) |>
#'   debit_plot() +
#'   ggplot2::scale_colour_manual(
#'     values = c("TRUE" = "darkgreen", "FALSE" = "orange"),
#'     aesthetics = c("colour", "fill"),
#'     guide = "none"
#'   )

debit_plot <- function(
  data,
  show_labels = TRUE,
  show_full_scale = TRUE,
  show_theme_other = TRUE
) {
  # Checks ---

  if (!inherits(data, "scrutiny_debit_map")) {
    abort_in_export(
      "!" = "`debit_plot()` only works with DEBIT results.",
      "x" = "`data` is not `debit_map()` output."
    )
  }

  # The formula decides what the plot shows. The record survives the same dplyr
  # verbs as the class, so data with the class but no formula got it by hand:
  formula <- scrutiny_args(data)$formula
  if (!is.character(formula) || !formula %in% c("exact", "mean_n")) {
    abort_in_export(
      "!" = "`data` doesn't record the DEBIT `formula` it was tested with.",
      "x" = "It has the class of `debit_map()` output, but not the \\
      \"scrutiny\" attribute that `debit_map()` gives its output.",
      "i" = "Plot the output of `debit_map()` itself."
    )
  }

  # Both lines are drawn under `"sd_sample_or_pop"`:
  sd_meaning <- scrutiny_args(data)$sd_meaning
  sd_meanings <- if (sd_meaning == "sd_sample_or_pop") {
    c("sd_sample", "sd_pop")
  } else {
    sd_meaning
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

  # Zero-row mapper output has no bound columns, so check rows first, and the
  # message blames the rows, not the columns:
  if (nrow(data) == 0L) {
    abort_in_export(
      "`data` has no rows, so there is nothing to plot.",
      "i" = "A DEBIT plot needs at least one value set."
    )
  }

  # `debit_map()` returns the bounds only under `show_rec = TRUE`, so they are
  # reconstructed here under the settings it recorded:
  cols_rec <- setdiff(cols_needed, c("x", "sd", "n", "consistency"))
  if (!all(cols_rec %in% colnames(data))) {
    check_cols_needed(data, c("x", "sd", "n", "digits_x", "digits_sd"))
    args <- scrutiny_args(data)
    args$show_rec <- TRUE
    rec <- do.call(
      debit_map,
      c(
        list(
          data[c("x", "sd", "n")],
          digits_x = data$digits_x,
          digits_sd = data$digits_sd
        ),
        args
      )
    )
    data[cols_rec] <- rec[cols_rec]
  }

  # Name the missing columns rather than let subsetting fail with a vctrs error:
  check_cols_needed(data, cols_needed, "i" = "`debit_map()` output has them.")

  is_complete <- stats::complete.cases(data[cols_needed])

  if (!all(is_complete)) {
    if (!any(is_complete)) {
      abort_in_export(
        "!" = "No value set in `data` can be drawn.",
        "x" = "Each of the {nrow(data)} row{?s} has a missing value in at \
      least one of these columns: {.code {cols_needed}}."
      )
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

  # The plot itself ---

  # Colors are mapped rather than set, so that adding a scale changes them:
  p <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = {{ x_num }},
      y = {{ sd_num }},
      label = {{ value_labels }},
      colour = {{ consistency }}
    )
  ) +
    ggplot2::scale_colour_manual(
      values = c("TRUE" = "royalblue1", "FALSE" = "red"),
      aesthetics = c("colour", "fill"),
      guide = "none"
    )

  # DEBIT line: the SD of binary data as a function of their mean. It depends on
  # `n`, and it falls as `n` rises, so the lines for all the distinct `n` values
  # lie between those for the smallest and the largest one. For realistic sample
  # sizes, these lines are so close that drawing each of them would blur them
  # into one thick, uneven stroke. Instead, only the two outer lines are drawn,
  # and the band between them is shaded. With a single `n`, both outer lines are
  # the same, so the band is not needed. The lines don't inherit the `label`
  # aesthetic, which they have no use for.

  # The line is drawn when the plot is, after the loop below has moved on, so
  # its `n` and `sd_meaning` are forced now:
  debit_line <- function(n_line, type) {
    force(n_line)
    force(type)
    function(x) sqrt(n_line / sd_denominator(n_line, type) * x * (1 - x))
  }

  # A sample SD needs `n >= 2`, so a value set with `n = 1`, which only
  # `sd_meaning = "sd_sample_or_pop"` can test, has no sample line.
  for (type in sd_meanings) {
    n_type <- n[n >= sd_min_n(type)]
    if (length(n_type) == 0L) {
      next
    }
    n_range <- range(n_type)

    if (n_range[1] != n_range[2]) {
      x_grid <- seq(0, 1, length.out = 201)
      p <- p +
        ggplot2::geom_ribbon(
          data = tibble::tibble(
            x = x_grid,
            ymin = debit_line(n_range[2], type)(x_grid),
            ymax = debit_line(n_range[1], type)(x_grid)
          ),
          ggplot2::aes(
            x = .data$x,
            ymin = .data$ymin,
            ymax = .data$ymax
          ),
          fill = "black",
          alpha = 0.25,
          na.rm = TRUE,
          inherit.aes = FALSE
        )
    }

    debit_lines <- n_range |>
      unique() |>
      lapply(function(n_line) {
        ggplot2::geom_function(
          fun = debit_line(n_line, type),
          linewidth = 0.5,
          na.rm = TRUE,
          inherit.aes = FALSE
        )
      })

    p <- p + debit_lines
  }

  # A point at the reported mean and SD. Unlike the rectangle drawn over it, it
  # has a fixed size, so that a value set whose rectangle is too small to see --
  # as with three or more decimal places -- still shows up:
  p <- p +
    ggplot2::geom_point(size = 1.5)

  p <- p +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = {{ x_lower }},
        xmax = {{ x_upper }},
        ymin = {{ sd_lower }},
        ymax = {{ sd_upper }},
        colour = {{ consistency }},
        fill = {{ consistency }}
      ),
      inherit.aes = FALSE
    )

  # Room on the y-axis beyond the outermost rectangles:
  sd_margin <- 0.025

  if (show_full_scale) {
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 1, 0.1),
        limits = c(0, 1)
      ) +
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
    is_thinned <- rep(FALSE, length(k_count))

    is_thinning <- sum(k_count) > 10000
    if (is_thinning) {
      panel <- ggplot2::ggplot_build(p)$layout$panel_params[[1L]]
    }

    # Each line has its own marks, at the same means but different SDs:
    for (type in sd_meanings) {
      if (is_thinning) {
        k_by_row <- list(k_lower = k_lower, k_upper = k_upper, n = n) |>
          purrr::pmap(function(k_lower, k_upper, n) {
            thin_binary_means(
              k_lower,
              k_upper,
              n,
              x_range = panel$x.range,
              y_range = panel$y.range,
              sd_meaning = type
            )
          })
        is_thinned <- is_thinned | lengths(k_by_row) < k_count
      } else {
        k_by_row <- purrr::map2(k_lower, k_upper, function(k_lower, k_upper) {
          if (k_lower > k_upper) {
            numeric(0)
          } else {
            k_lower:k_upper
          }
        })
      }

      # As for the lines above:
      k_by_row[n < sd_min_n(type)] <- list(numeric(0))

      k <- unlist(k_by_row)
      n_by_k <- rep(n, lengths(k_by_row))

      if (length(k) > 0L) {
        p <- p +
          ggplot2::geom_point(
            data = tibble::tibble(
              x = k / n_by_k,
              y = sqrt(
                k * (n_by_k - k) / (n_by_k * sd_denominator(n_by_k, type))
              )
            ),
            ggplot2::aes(x = .data$x, y = .data$y),
            size = 1,
            inherit.aes = FALSE
          )
      }
    }

    n_thinned <- sum(is_thinned)
    if (n_thinned > 0L) {
      cli::cli_inform(c(
        "i" = "Marking only some of the attainable means of \\
        {n_thinned} value set{?s}.",
        " " = "The others would be drawn over them at this plot's \\
        resolution, so the plot looks the same."
      ))
    }
  }

  if (show_labels) {
    rlang::check_installed("ggrepel", "for the labels in `debit_plot()`.")
    p <- p +
      ggrepel::geom_text_repel(
        force = 175,
        force_pull = 0.75,
        box.padding = 0.5,
        segment.alpha = 0.5,
        segment.linetype = 3,
        segment.size = 0.75,
        size = 3.5
      )
  }

  p <- p +
    ggplot2::labs(
      x = "Mean",
      y = if (sd_meaning == "se") "Standard error" else "Standard deviation"
    )

  if (show_theme_other) {
    p <- p +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_line()
      )
  }

  # Returned, not printed: `print()` would draw whenever the result is
  # assigned or added to.
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
  sd_meaning = "sd_sample",
  resolution = 2000
) {
  x_lines <- seq(x_range[1L], x_range[2L], length.out = resolution + 1)
  y_lines <- seq(y_range[1L], y_range[2L], length.out = resolution + 1)

  # The mean `k / n` crosses a vertical line once. The SD, the square root of `k
  # * (n - k) / (n * (n - 1))` with `n` in place of `n - 1` for a population SD,
  # crosses a horizontal one twice, on its way up and on its way down:
  discriminant <- n^2 / 4 - y_lines^2 * n * sd_denominator(n, sd_meaning)
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
