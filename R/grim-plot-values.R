# The comb behind `grim_plot_values()` and `grimmer_plot_values()`: every value
# a report could take within a window, drawn as a tooth -- short and pale where
# no sample can produce it, tall and colored where one can. The point is made by
# the ratio of ink to gaps, so the values that are ruled out have to be drawn
# too; a figure showing only the attainable values would look like an ordinary,
# unremarkable scale.
#
# The marker for the reported value -- arrow, label, and the strip behind the
# teeth -- takes `color_reported`, and the label says the verdict outright. The
# callers pass the marker's color already resolved by the verdict: a consistent
# value points at a tall tooth in the tooth's own color, so the figure and the
# verdict cannot be read apart. An inconsistent value gets the nearest tall
# tooth on either side labeled with its value, since "not this one" begs the
# question of which ones instead. `label_value` formats a value the way the
# caller reports it, with its decimal places and any `%`.

plot_comb <- function(
  values,
  possible,
  reported,
  consistent,
  label_value,
  claim,
  x_label,
  color_cons,
  color_incons,
  color_reported
) {
  bare_tooth <- function(keep, height) {
    nrow_out <- sum(keep)
    tibble::new_tibble(
      list(x = values[keep], height = rep(height, nrow_out)),
      nrow = nrow_out
    )
  }

  geom_teeth <- function(data, ...) {
    ggplot2::geom_segment(
      data = data,
      mapping = ggplot2::aes(
        x = .data$x,
        xend = .data$x,
        y = 0,
        yend = .data$height
      ),
      ...
    )
  }

  span <- diff(range(values))

  # The panel's ranges are fixed rather than left to the scales, so that a value
  # can be placed in the panel's npc units below. The panel ends just above the
  # strip; everything over it is drawn into the top margin.
  xlim <- range(values) + c(-1, 1) * span * 0.067
  ylim <- c(0, 1.04)

  # The marker for the reported value and, if it is inconsistent, labels for the
  # nearest attainable value below and above it, set outward far enough to clear
  # the marker's own label. An elbow connects each to the top of its tooth:
  # down, across, and down again.
  #
  # Text and the arrowhead have a physical size, so in data units their extent
  # depends on the size the plot is drawn at, and no fixed data positions keep
  # them apart. The marker is therefore one grob whose positions are grid units:
  # npc for where a value is, `strwidth`, `char`, and `lines` for how far apart
  # things must be. Grid resolves them when the plot is drawn, at whatever size
  # that is. Heights are counted in `lines` up from the top of the teeth, and
  # the top margin is sized from the same count.
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
  teeth_top <- grid::unit((1 - ylim[1L]) / diff(ylim), "npc")
  above <- function(lines) teeth_top + grid::unit(lines, "lines")

  # Heights above the teeth, in lines. `elbow` runs across above the
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
      col = color_cons,
      fontsize = fontsize,
      lineheight = lineheight
    )
  )

  ggplot2::ggplot() +
    # The interval the reported value stands for, behind the teeth: the values
    # that would have been rounded to it. It is a fact about the reported value,
    # not about the scale, so it takes the marker's color at low opacity.
    ggplot2::annotate(
      "rect",
      xmin = claim[1L],
      xmax = claim[2L],
      ymin = 0,
      ymax = 1.04,
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
    geom_teeth(bare_tooth(!possible, 0.3), colour = color_incons) +
    geom_teeth(bare_tooth(possible, 1), colour = color_cons, linewidth = 0.9) +
    # The reported value points down at the comb from above rather than being
    # drawn across it. A full-height marker would stand exactly where the tooth
    # it is asking about stands, and hide the one thing the figure is for:
    # whether there is a tooth there at all.
    ggplot2::annotation_custom(marker_grob) +
    # `clip = "off"` so the marker can sit above the comb rather than being cut
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
      # A comb has no height to read, so an axis for it would only invite
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
comb_values <- function(from, to, digits) {
  from |>
    seq(to, by = 10^-digits) |>
    round(digits)
}


# `n` and `items_averaged` are needed for the window before the test itself gets
# to reject them, and an undecidable case has nothing to draw. The length checks
# stay in the plot functions themselves, so that the error names the function
# the user called.
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


# The window around the reported value is built from it and from its decimal
# count, so a value that is missing or infinite, or a decimal count that is not
# a single non-negative whole number, has nothing to draw -- and would fail in
# `seq()` with an error that names neither argument. An SD can't be negative
# either. `name` is the argument's name: `"x"` or `"sd"`.
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
#' @description `grim_plot_values()` draws the GRIM test for a single value set.
#'   The plot is a comb. Each tooth is a mean (or percentage) that could have
#'   been reported with `digits_x` decimal places, between the whole number at
#'   or below `x` and the next one up. Teeth for means that a sample of size
#'   `n` can have are tall. The reported value is
#'   marked above the comb and labeled with its verdict. If it is inconsistent,
#'   the nearest attainable values on either side are labeled.
#'
#'   [`grim_plot()`] shows many results at once; this function shows one test
#'   in detail, for teaching or for explaining a verdict. [`grim_values()`]
#'   returns the attainable values as numbers.
#'
#' @param x,n,digits_x,items_averaged,percent,rounding,threshold,symmetric The
#'   same as in [`grim()`], but `x`, `n`, and `items_averaged` must have
#'   length 1.
#' @param color_cons,color_incons Strings. Colors of the teeth for attainable
#'   and unattainable values.
#' @param color_reported String. Color of the marker for the reported value
#'   and of the strip behind the teeth, if the value is inconsistent. A
#'   consistent value is marked in `color_cons`.
#'
#' @details The comb runs from the whole number at or below `x` to the next one
#'   up. That unit contains `10 ^ digits_x + 1` reportable values, both ends
#'   included, and, with `items_averaged = 1`, about `n` attainable ones. With
#'   `percent = TRUE`, a unit is one percentage point, and the comb widens by
#'   `100 / (n * items_averaged)` points, rounded up, on either side so that
#'   the nearest attainable values remain visible, but not beyond 0 or 100.
#'   With more than two decimal places, the teeth crowd together.
#'
#'   The strip behind the teeth is the range of unrounded means that would
#'   have been reported as `x`, from [`unround()`]. The value is consistent
#'   exactly if a tall tooth stands within the strip. Under methods such as
#'   `"trunc"` or `"ceiling"`, one edge of the strip is not part of it, so a
#'   tall tooth on that edge does not count.
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
#' # no tall tooth stands where the marker points.
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
#'   percent = TRUE
#' )

grim_plot_values <- function(
  x,
  n,
  digits_x,
  items_averaged,
  percent = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  color_cons = "#2f7d74",
  color_incons = "#d9d4ca",
  color_reported = "#c1522e"
) {
  if (missing(digits_x)) {
    error_digits_missing(x)
  }
  check_length(x, 1L)
  check_length(n, 1L)
  if (missing(items_averaged)) {
    error_items_missing()
  }
  check_length(items_averaged, 1L)
  check_newly_numeric(x, digits_x)
  check_comb_value(x, digits_x, "x")
  check_decidable_n_items(n, items_averaged, min_n = 1)

  # One unit around `x`, which holds about `n` attainable means. Attainable
  # percentages are `100 / n` points apart, so the window widens by that much
  # on either side (within 0 to 100), to keep the nearest ones in view:
  reach <- if (percent) ceiling(100 / (n * items_averaged)) else 0
  lower <- floor(x) - reach
  upper <- floor(x) + 1 + reach
  if (percent) {
    lower <- max(lower, min(0, floor(x)))
    upper <- min(upper, max(100, ceiling(x)))
  }
  values <- comb_values(lower, upper, digits_x)

  test <- function(x) {
    grim(
      x = x,
      n = n,
      digits_x = digits_x,
      items_averaged = items_averaged,
      percent = percent,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )
  }

  possible <- test(values)
  consistent <- test(x)

  claim <- unround(
    x = x,
    rounding = rounding,
    threshold = threshold,
    digits = digits_x,
    symmetric = symmetric
  )

  plot_comb(
    values = values,
    possible = possible,
    reported = x,
    consistent = consistent,
    label_value = function(value) {
      value |>
        formatC(format = "f", digits = digits_x) |>
        paste0(if (percent) "%")
    },
    claim = c(claim$lower[1L], claim$upper[1L]),
    # fmt: skip
    x_label = paste0(
      if (percent) "Reported percentage" else "Reported mean",
      " (n = ", format(n, scientific = FALSE),
      if (items_averaged != 1) {
        paste(", items_averaged =", format(items_averaged, scientific = FALSE))
      },
      ")"
    ),
    color_cons = color_cons,
    color_incons = color_incons,
    color_reported = if (consistent) color_cons else color_reported
  )
}
