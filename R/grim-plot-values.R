# The comb behind `grim_plot_values()` and `grimmer_plot_values()`: every value
# a report could take within a window, drawn as a square where some sample can
# produce it and left out where none can. The panel is a strip, so that the
# squares are square in any device; the gaps between them make the point.
#
# The callers resolve `color_reported` by the verdict, so a consistent value is
# marked in its square's own color. An inconsistent value gets the nearest
# square on either side labeled, since "not this one" begs the question of
# which ones instead. `label_value` formats a value the way the caller reports
# it.

plot_comb <- function(
  values,
  possible,
  reported,
  consistent,
  label_value,
  claim,
  x_label,
  color_con,
  color_reported
) {
  span <- diff(range(values))

  # The panel's ranges are fixed rather than left to the scales, so that a value
  # can be placed in the panel's npc units below. The panel ends just above the
  # strip; everything over it is drawn into the top margin.
  xlim <- range(values) + c(-1, 1) * span * 0.067
  ylim <- c(0, 1)

  # A square's side as a share of the panel's width: 80% of the step between
  # values, but at most 8%, as with no decimal places. The panel's height is a
  # share of its width as well, so the squares stay square on any device; it
  # holds them with some room to spare, but not less than 3% of the width, to
  # remain a strip for the marker to point at.
  step <- min(diff(values))
  side <- min(0.8 * step / diff(xlim), 0.08)
  aspect <- max(2.5 * side, 0.03)
  squares <- tibble::tibble(x = values[possible])

  # The marker and, if the value is inconsistent, labels for the nearest
  # attainable values, each connected to its square by an elbow.
  #
  # Text and the arrowhead have a physical size, so no fixed data positions keep
  # them apart at every plot size. The marker is therefore one grob in grid
  # units: npc for where a value is, `strwidth`, `char`, and `lines` for spacing.
  # Heights are counted in `lines` up from the top of the panel, and the top
  # margin is sized from the same count.
  attainable <- values[possible]
  neighbors <- if (consistent) {
    numeric(0)
  } else {
    c(
      max(attainable[attainable < reported], -Inf),
      min(attainable[attainable > reported], Inf)
    )
  }
  neighbors <- neighbors[is.finite(neighbors)]
  npc_x <- function(v) grid::unit((v - xlim[1L]) / diff(xlim), "npc")
  above <- function(lines) grid::unit(1, "npc") + grid::unit(lines, "lines")

  # Heights above the panel, in lines. `elbow` runs across above the
  # arrowhead, whose base is at `arrow_tip` plus its length.
  arrow_tip <- 0.4
  arrow_head <- 0.6
  elbow <- 1.25
  label_base <- 2
  arrow_top <- 2.1
  verdict_base <- 2.3
  # The verdict's two lines at `lineheight` 0.9, plus a little air:
  stack_lines <- verdict_base + 2 * 0.9 + 0.3

  text_size <- 4.2
  fontsize <- text_size * ggplot2::.pt
  lineheight <- 0.9

  verdict <- paste0(
    label_value(reported),
    "\n",
    if (consistent) "consistent" else "inconsistent"
  )

  reported_x <- npc_x(reported)

  marker_grobs <- grid::gList(
    grid::segmentsGrob(
      x0 = reported_x,
      y0 = above(arrow_top),
      x1 = reported_x,
      y1 = above(arrow_tip),
      arrow = grid::arrow(
        angle = 25,
        length = grid::unit(arrow_head, "lines"),
        type = "closed"
      ),
      gp = grid::gpar(
        col = color_reported,
        fill = color_reported,
        lwd = 0.7 * ggplot2::.pt,
        linejoin = "mitre"
      ),
      name = "arrow"
    ),
    grid::textGrob(
      verdict,
      x = reported_x,
      y = above(verdict_base),
      vjust = 0,
      gp = grid::gpar(col = color_reported),
      name = "verdict"
    )
  )

  # `strwidth` of a multi-line string is the width of its longest line.
  verdict_half <- 0.5 * grid::unit(1, "strwidth", verdict)

  neighbor_grobs <- neighbors |>
    seq_along() |>
    lapply(function(i) {
      v <- neighbors[i]
      label <- label_value(v)
      offset <- grid::unit.pmax(
        grid::unit(abs(v - reported) / diff(xlim), "npc") +
          grid::unit(1.5, "char"),
        verdict_half +
          grid::unit(1, "char") +
          0.5 * grid::unit(1, "strwidth", label)
      )
      label_x <- reported_x + sign(v - reported) * offset
      grid::gList(
        grid::textGrob(
          label,
          x = label_x,
          y = above(label_base),
          vjust = 0,
          name = paste0("neighbor-label-", i)
        ),
        grid::polylineGrob(
          x = grid::unit.c(label_x, label_x, npc_x(v), npc_x(v)),
          y = grid::unit.c(
            above(label_base - 0.25),
            above(elbow),
            above(elbow),
            above(0.2)
          ),
          gp = grid::gpar(lwd = 0.4 * ggplot2::.pt),
          name = paste0("neighbor-elbow-", i)
        )
      )
    })

  marker_children <- do.call(
    grid::gList,
    c(marker_grobs, unlist(neighbor_grobs, recursive = FALSE))
  )

  marker_grob <- grid::gTree(
    children = marker_children,
    gp = grid::gpar(
      col = color_con,
      fontsize = fontsize,
      lineheight = lineheight
    )
  )

  ggplot2::ggplot() +
    # The interval the reported value stands for, behind the squares: the
    # values that would have been rounded to it. It is a fact about the
    # reported value, not about the scale, so it takes the marker's color at
    # low opacity.
    ggplot2::annotate(
      "rect",
      # Within the panel, which is not clipped:
      xmin = max(claim[1L], xlim[1L]),
      xmax = min(claim[2L], xlim[2L]),
      ymin = 0,
      ymax = 1,
      fill = color_reported,
      alpha = 0.18
    ) +
    ggplot2::annotate(
      "segment",
      x = min(values) - span * 0.03,
      xend = max(values) + span * 0.03,
      y = 0,
      yend = 0,
      colour = "grey60",
      linewidth = 0.5
    ) +
    ggplot2::geom_tile(
      data = squares,
      mapping = ggplot2::aes(x = .data$x, y = 0.5),
      width = side * diff(xlim),
      height = side * diff(ylim) / aspect,
      fill = color_con
    ) +
    # The reported value points down at the strip from above rather than being
    # drawn across it. A marker in the strip would stand exactly where the
    # square it is asking about stands, and hide the one thing the figure is
    # for: whether there is a square there at all.
    ggplot2::annotation_custom(marker_grob) +
    # `clip = "off"` so the marker can sit above the strip rather than being cut
    # by the top of the panel; the top margin is the marker's stack of lines,
    # and the side margins leave room for a value at either end of the window.
    ggplot2::coord_cartesian(
      xlim = xlim,
      ylim = ylim,
      expand = FALSE,
      clip = "off"
    ) +
    ggplot2::labs(x = x_label, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      aspect.ratio = aspect,
      # The strip has no height to read, so an axis for it would only invite
      # someone to look for one.
      axis.text.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      plot.margin = ggplot2::margin(
        ceiling(stack_lines * fontsize * lineheight),
        40,
        6,
        40
      )
    )
}


# Every value with `digits` decimal places from `from` to `to`. Rounded after
# the sequence is built rather than trusting `seq()`: a step of 0.01 added a
# hundred times over lands a little beside the two-decimal number it should be,
# and these values go into a consistency test that checks them against `digits`.
# The values from `from` to `to` at `digits` decimal places, one for each
# possible square. Each is tested, so their number is checked first; it grows tenfold
# with each decimal place. `name` is `"x"` or `"sd"`, for the message:
comb_values <- function(from, to, digits, name) {
  size <- (to - from) * 10^digits + 1
  if (size > COMB_TEETH_MAX) {
    limit <- format(COMB_TEETH_MAX, big.mark = ",", scientific = FALSE)
    abort_in_export(
      "Too many values to draw.",
      "x" = "With `digits_{name} = {digits}`, the plot would test \\
      {format(size, big.mark = ',', scientific = FALSE)} values, more than \\
      {limit}.",
      "i" = "Each decimal place multiplies them by 10."
    )
  }
  from |>
    seq(to, by = 10^-digits) |>
    round(digits)
}


# The values to draw, and which of them `test` admits: every value from `lower`
# to `upper` or, with `zoom`, the ones around `reported`, so that it and the
# strip behind it stand out. The zoomed window reaches `COMB_ZOOM_STEPS` values
# to either side. If `reported` is inconsistent, it doubles until the nearest
# attainable value on each side is in view, and then keeps three values beyond
# the farther one. It reaches no farther to either side than the full window is
# wide, so that a value near a limit can still reach a neighbor on the other
# side, and holds no values outside of `limits`, the ones that can be reported
# at all, nor more than `COMB_TEETH_MAX`.
comb_window <- function(
  reported,
  consistent,
  digits,
  lower,
  upper,
  limits,
  zoom,
  test,
  name
) {
  if (!zoom) {
    values <- comb_values(lower, upper, digits, name)
    return(list(values = values, possible = test(values)))
  }

  step <- 10^-digits
  steps_max <- min(
    max(ceiling((upper - lower) / step), 1),
    (COMB_TEETH_MAX - 1) %/% 2
  )
  steps <- min(COMB_ZOOM_STEPS, steps_max)

  repeat {
    values <- comb_values(
      max(reported - steps * step, limits[1L]),
      min(reported + steps * step, limits[2L]),
      digits,
      name
    )
    possible <- test(values)
    index_reported <- which.min(abs(values - reported))
    index_possible <- which(possible)
    left <- index_possible[index_possible < index_reported]
    right <- index_possible[index_possible > index_reported]
    # A side that ends at a limit has nothing more to show:
    seen_left <- length(left) > 0L || values[1L] - step < limits[1L]
    seen_right <- length(right) > 0L ||
      values[length(values)] + step > limits[2L]
    if (consistent || (seen_left && seen_right) || steps >= steps_max) {
      break
    }
    steps <- min(2 * steps, steps_max)
  }

  if (!consistent) {
    reach <- max(
      index_reported - left[length(left)],
      right[1L] - index_reported,
      0L,
      na.rm = TRUE
    )
    steps <- min(steps, max(min(COMB_ZOOM_STEPS, steps_max), reach + 3L))
  }
  keep <- abs(seq_along(values) - index_reported) <= steps
  list(values = values[keep], possible = possible[keep])
}


# `n` and `items_averaged` are needed for the window before the test itself gets
# to reject them, and an undecidable case has nothing to draw.
check_decidable_n_items <- function(n, items_averaged, min_n) {
  if (!is_decidable_n_items(n, items_averaged, min_n = min_n)) {
    abort_in_export(
      "`n` and `items_averaged` must be positive whole numbers, and `n` \\
      must be at least {min_n}.",
      "x" = "`n` is {n} and `items_averaged` is {items_averaged}.",
      "i" = "The test cannot be decided otherwise, so there is nothing to \\
      draw."
    )
  }
}


# The window is built from the value and its decimal count; bad ones would fail
# in `seq()` with an error that names neither argument. `name` is `"x"` or
# `"sd"`.
check_comb_value <- function(value, digits, name) {
  name_digits <- paste0("digits_", name)
  if (!is.finite(value)) {
    abort_in_export(
      "!" = "`{name}` must be a finite number.",
      "x" = "It is `{value}`."
    )
  }
  if (name == "sd" && value < 0) {
    abort_in_export(
      "!" = "`sd` can't be negative.",
      "x" = "It is {value}."
    )
  }
  if (
    length(digits) != 1L ||
      !is.numeric(digits) ||
      is.na(digits) ||
      !is_whole_number(digits) ||
      digits < 0
  ) {
    abort_in_export(
      "!" = "`{name_digits}` must be a single non-negative whole number.",
      "x" = "It is `{deparse(digits)}`."
    )
  }
}


#' Visualize the means that GRIM admits
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `grim_plot_values()` draws the GRIM test for a single value set as a
#'   row of squares. Of all means (or percentages) that could have been
#'   reported with `digits_x` decimal places, each square is one that a sample
#'   of size `n` can have; the others are left out. The reported value is
#'   marked above the row and labeled as consistent or inconsistent. If it is inconsistent, the nearest attainable
#'   values on either side are labeled.
#'
#'   [`grim_plot_grid()`] shows many results at once; this function explains
#'   one. [`grim_values()`] returns the attainable values as numbers.
#'
#' @inheritParams grim
#' @param x,n,digits_x,items_averaged,x_meaning,rounding,threshold,symmetric The
#'   same as in [`grim()`], but `x`, `n`, and `items_averaged` must have
#'   length 1.
#' @param zoom Logical. If `TRUE` (the default), the row shows the values
#'   around `x`, so that the marker and the strip behind it stand out; see
#'   *Details*. If `FALSE`, it shows a whole unit.
#' @param color_con String. Color of the squares for attainable values.
#' @param color_reported String. Color of the marker for the reported value
#'   and of the strip behind the squares, if the value is inconsistent. A
#'   consistent value is marked in `color_con`.
#'
#' @details By default, the row shows 10 values on either side of `x`. If `x`
#'   is inconsistent, it widens until the nearest attainable value on each side
#'   is in view, but no farther than the row with `zoom = FALSE` is wide.
#'
#'   With `zoom = FALSE`, the row runs from the whole number at or below `x` to
#'   the next one up. That unit contains `10 ^ digits_x + 1` reportable values, both ends
#'   included. With `items_averaged = 1`, about `n` of them are attainable, or
#'   all if `n` exceeds `10 ^ digits_x`. With
#'   `x_meaning = "percentage"`, a unit is one percentage point, and the row widens by
#'   `100 / (n * items_averaged)` points, rounded up, on either side so that
#'   the nearest attainable values remain visible, but not beyond 0 or 100.
#'   With more than two decimal places, the squares crowd together.
#'
#'   The strip behind the squares is the range of unrounded means that would
#'   have been reported as `x`, from [`roundwork::unround()`]. The value is
#'   consistent exactly if a square stands within the strip. Under methods
#'   such as `"trunc"` or `"ceiling"`, one edge of the strip is not part of it,
#'   so a square on that edge does not count.
#'
#' @return A ggplot object.
#'
#' @seealso [`grimmer_plot_values()`] for the same figure over standard
#'   deviations.
#'
#' @export
#'
#' @examples
#' # A mean of 5.19 is not consistent with a sample size of 28:
#' # no square stands where the marker points.
#' grim_plot_values(x = 5.19, n = 28, digits_x = 2, items_averaged = 1)
#'
#' # With `n = 26`, it is:
#' grim_plot_values(x = 5.19, n = 26, digits_x = 2, items_averaged = 1)
#'
#' # The same for a percentage:
#' grim_plot_values(
#'   x = 67.4,
#'   n = 40,
#'   digits_x = 1,
#'   items_averaged = 1,
#'   x_meaning = "percentage"
#' )

grim_plot_values <- function(
  x,
  n,
  digits_x,
  items_averaged,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  zoom = TRUE,
  color_con = "#2f7d74",
  color_reported = "#c1522e",
  percent = deprecated()
) {
  if (lifecycle::is_present(percent)) {
    stop_percent("grim_plot_values")
  }
  if (missing(digits_x)) {
    error_digits_missing(x)
  }
  check_length(x, 1L)
  check_length(n, 1L)
  if (missing(items_averaged)) {
    error_items_missing()
  }
  check_length(items_averaged, 1L)
  # As in the test itself: a string is a mistake, not an undecidable value:
  if (!is.numeric(n) && !is.na(n)) {
    check_type(n, c("double", "integer"))
  }
  if (!is.numeric(items_averaged) && !is.na(items_averaged)) {
    check_type(items_averaged, c("double", "integer"))
  }
  x_meaning <- arg_match_in_export(x_meaning, X_MEANINGS)
  is_percentage <- x_meaning == "percentage"
  check_flag(zoom)
  n <- round_whole(n)
  items_averaged <- round_whole(items_averaged)
  digits_x <- round_whole(digits_x)
  check_newly_numeric(
    x,
    digits_x,
    scale = rounding_scale(rounding, threshold, symmetric)
  )
  check_comb_value(x, digits_x, "x")
  check_decidable_n_items(n, items_averaged, min_n = 1)

  # One unit around `x`, which holds about `n` attainable means. Attainable
  # percentages are `100 / n` points apart, so the window widens by that much
  # on either side (within 0 to 100), to keep the nearest ones in view:
  reach <- if (is_percentage) {
    ceiling(100 / (as.numeric(n) * items_averaged))
  } else {
    0
  }
  lower <- floor(x) - reach
  upper <- floor(x) + 1 + reach
  if (is_percentage) {
    lower <- max(lower, min(0, floor(x)))
    upper <- min(upper, max(100, ceiling(x)))
  }
  test <- function(x) {
    grim(
      x = x,
      n = n,
      digits_x = digits_x,
      items_averaged = items_averaged,
      data_type = "integer",
      x_meaning = x_meaning,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )
  }

  consistent <- test(x)
  window <- comb_window(
    reported = x,
    consistent = consistent,
    digits = digits_x,
    lower = lower,
    upper = upper,
    limits = if (is_percentage) {
      c(min(0, floor(x)), max(100, ceiling(x)))
    } else {
      c(-Inf, Inf)
    },
    zoom = zoom,
    test = test,
    name = "x"
  )

  claim <- roundwork::unround(
    x = x,
    rounding = rounding,
    threshold = threshold,
    digits = digits_x,
    symmetric = symmetric
  )

  plot_comb(
    values = window$values,
    possible = window$possible,
    reported = x,
    consistent = consistent,
    label_value = function(value) {
      value |>
        formatC(format = "f", digits = digits_x) |>
        paste0(if (is_percentage) "%")
    },
    claim = c(claim$lower[1L], claim$upper[1L]),
    # fmt: skip
    x_label = paste0(
      if (is_percentage) "Reported percentage" else "Reported mean",
      " (n = ", format(n, scientific = FALSE),
      if (items_averaged != 1) {
        paste(", items_averaged =", format(items_averaged, scientific = FALSE))
      },
      ")"
    ),
    color_con = color_con,
    color_reported = if (consistent) color_con else color_reported
  )
}
