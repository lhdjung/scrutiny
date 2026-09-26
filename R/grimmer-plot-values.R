#' Visualize the standard deviations that GRIMMER admits
#'
#' @description `grimmer_plot_values()` draws every standard deviation that
#'   could have been reported with a given number of decimal places within a
#'   window, and marks those that a sample of the given size with the reported
#'   mean could actually have produced. The reported SD points at the comb from
#'   above, so the question GRIMMER asks -- is there an attainable SD where the
#'   report says there is one? -- can be answered by eye. The verdict is also
#'   spelled out next to the value, and the marker takes the color of the
#'   attainable teeth if the value is consistent.
#'
#'   This is the GRIMMER counterpart of [`grim_plot_values()`]: one value set's
#'   test as a picture, meant for teaching and for explaining a single verdict.
#'   There is no GRIMMER-specific plot for many value sets at once;
#'   [`grim_plot()`] accepts [`grimmer_map()`] output and shows the GRIM part of
#'   the test.
#'
#' @param x,sd,n,digits_x,digits_sd,items,min_val,max_val,rounding,threshold,symmetric,tolerance
#'   As in [`grimmer()`], but `x`, `sd`, and `n` must have length 1.
#' @param color_scale String. Color of the teeth for SDs that GRIMMER admits on
#'   an unbounded scale but that the scale bounds rule out. Only used if
#'   `min_val` and `max_val` are specified.
#' @inheritParams grim_plot_values
#'
#' @details Without scale bounds, the comb runs from the whole number at or
#'   below `sd` to the next whole number up (but not below zero), like the comb
#'   in [`grim_plot_values()`]. With `min_val` and `max_val`, it runs from zero
#'   to a little past the largest SD any sample on that scale can have, so that
#'   the values the scale rules out are in view: these are drawn at a middle
#'   height in `color_scale`. GRIMMER knows nothing about the scale on its own,
#'   so it goes on admitting SDs long after the scale has run out, and the two
#'   ways an SD can be impossible stay distinct in the figure.
#'
#'   GRIMMER is nested on GRIM, so if `x` itself is GRIM-inconsistent with `n`,
#'   no SD is attainable and the comb has no tall teeth at all.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @examples
#' # A mean of 5.23 with an SD of 2.55 is not consistent with a sample size of
#' # 35, but it is with 31:
#' grimmer_plot_values(x = 5.23, sd = 2.55, n = 35, digits_x = 2, digits_sd = 2)
#' grimmer_plot_values(x = 5.23, sd = 2.55, n = 31, digits_x = 2, digits_sd = 2)
#'
#' # On a scale from 1 to 5, an SD of 2.08 is out of reach for a mean of 3.00
#' # and 20 values, even though GRIMMER admits it on an unbounded scale:
#' grimmer_plot_values(
#'   x = 3.00, sd = 2.08, n = 20, digits_x = 2, digits_sd = 2,
#'   min_val = 1, max_val = 5
#' )

grimmer_plot_values <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items = 1,
  min_val = NULL,
  max_val = NULL,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  tolerance = .Machine$double.eps^0.5,
  color_cons = "#2f7d74",
  color_incons = "#d9d4ca",
  color_scale = "#868b93",
  color_reported = "#c1522e"
) {
  if (missing(digits_x)) {
    error_digits_missing(x)
  }
  if (missing(digits_sd)) {
    error_digits_missing(sd)
  }
  check_length(x, 1L)
  check_length(sd, 1L)
  check_length(n, 1L)
  check_length(items, 1L)
  check_newly_numeric(x, digits_x)
  check_newly_numeric(sd, digits_sd)
  check_comb_value(x, digits_x, "x")
  check_comb_value(sd, digits_sd, "sd")
  check_decidable_n_items(n, items, min_n = 2)
  has_scale <- check_scale_bounds(min_val, max_val)

  x <- as.numeric(x)
  sd <- as.numeric(sd)

  # With known bounds, the comb reaches a little past the largest SD any sample
  # on the scale can have (half of it at each end), so that the teeth the scale
  # rules out are in view. Without them, one unit around `sd`, as for a mean:
  window <- if (has_scale) {
    sd_max <- (max_val - min_val) / 2 * sqrt(n / (n - 1))
    c(0, max(ceiling(sd_max * 1.12 * 10^digits_sd) / 10^digits_sd, sd))
  } else {
    c(max(0, floor(sd)), floor(sd) + 1)
  }
  values <- comb_values(window[1L], window[2L], digits_sd)

  test <- function(sd, min_val, max_val) {
    grimmer(
      x = x,
      sd = sd,
      n = n,
      digits_x = digits_x,
      digits_sd = digits_sd,
      items = items,
      min_val = min_val,
      max_val = max_val,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric,
      tolerance = tolerance
    )
  }

  possible <- test(values, min_val, max_val)
  consistent <- test(sd, min_val, max_val)

  # The bounds can only turn `TRUE` into `FALSE`, so the values they ruled out
  # are those that pass without them and fail with them:
  stopped_short <- if (has_scale) {
    test(values, NULL, NULL) & !possible
  } else {
    rep(FALSE, length(values))
  }

  claim <- unround(
    x = sd,
    rounding = rounding,
    threshold = threshold,
    digits = digits_sd,
    symmetric = symmetric
  )

  plot_comb(
    values = values,
    possible = possible,
    stopped_short = stopped_short,
    reported = sd,
    consistent = consistent,
    label_value = function(v) formatC(v, format = "f", digits = digits_sd),
    claim = c(claim$lower[1L], claim$upper[1L]),
    x_label = paste0(
      "Reported SD (mean = ",
      formatC(x, format = "f", digits = digits_x),
      ", n = ",
      format(n, scientific = FALSE),
      if (items != 1) paste0(", items = ", format(items, scientific = FALSE)),
      if (has_scale) paste0(", scale ", min_val, "-", max_val),
      ")"
    ),
    color_cons = color_cons,
    color_incons = color_incons,
    color_scale = color_scale,
    color_reported = if (consistent) color_cons else color_reported
  )
}
