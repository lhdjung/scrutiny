#' Visualize the standard deviations that GRIMMER admits
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `grimmer_plot_line()` draws the GRIMMER test for a single value set, as
#'   [`grim_plot_line()`] does for GRIM. Of all SDs that could have been
#'   reported with `digits_sd` decimal places, each square is one that a
#'   sample of size `n` with mean `x` could have; the others are left out. The
#'   reported SD is marked above the row and labeled as consistent or
#'   inconsistent.
#'
#'   For many value sets, [`grim_plot_grid()`] accepts [`grimmer_map()`] output
#'   and shows the GRIM part of the test.
#'
#' @param x,sd,n,digits_x,digits_sd,items_averaged,rounding,threshold,symmetric,sd_meaning
#'   The same as in [`grimmer()`], but `x`, `sd`, `n`, and `items_averaged`
#'   must have length 1.
#' @inheritParams grim_plot_line
#'
#' @details By default, the row shows the SDs around `sd`, as in
#'   [`grim_plot_line()`], where squares are labeled the same way. With
#'   `zoom = "wide"`, it runs from the whole number at or below `sd` to the
#'   next one up. It never goes below zero.
#'
#'   If `x` is GRIM-inconsistent with `n`, no SD is attainable and the row has
#'   no squares.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @examples
#' # A mean of 5.23 with an SD of 2.55 is not consistent with a sample size of
#' # 35, but it is with 31:
#' grimmer_plot_line(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 35,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1
#' )
#' grimmer_plot_line(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 31,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1
#' )

grimmer_plot_line <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  zoom = c("close", "medium", "wide"),
  color_con = "#0047ff",
  color_incon = "#ff2a00",
  color_attainable = "grey75"
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
  n <- round_whole(n)
  items_averaged <- round_whole(items_averaged)
  digits_x <- round_whole(digits_x)
  digits_sd <- round_whole(digits_sd)
  check_newly_numeric(
    x,
    digits_x,
    scale = rounding_scale(rounding, threshold, symmetric)
  )
  check_newly_numeric(
    sd,
    digits_sd,
    scale = rounding_scale(rounding, threshold, symmetric)
  )
  check_comb_value(x, digits_x, "x")
  check_comb_value(sd, digits_sd, "sd")
  sd_meaning <- arg_match_in_export(sd_meaning, SD_MEANINGS)
  zoom <- arg_match_in_export(zoom, COMB_ZOOMS)
  check_decidable_n_items(n, items_averaged, min_n = sd_min_n(sd_meaning))

  test <- function(sd) {
    grimmer(
      x = x,
      sd = sd,
      n = n,
      digits_x = digits_x,
      digits_sd = digits_sd,
      items_averaged = items_averaged,
      data_type = "integer",
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric,
      sd_meaning = sd_meaning
    )
  }

  consistent <- test(sd)
  window <- comb_window(
    reported = sd,
    consistent = consistent,
    digits = digits_sd,
    lower = max(0, floor(sd)),
    upper = floor(sd) + 1,
    limits = c(0, Inf),
    zoom = zoom,
    test = test,
    name = "sd"
  )

  claim <- roundwork::unround(
    x = sd,
    rounding = rounding,
    threshold = threshold,
    digits = digits_sd,
    symmetric = symmetric
  )

  # An SD cannot be negative, so the test's bounds start at zero; the strip
  # shows the same range:
  claim$lower <- max(claim$lower, 0)

  plot_comb(
    values = window$values,
    possible = window$possible,
    reported = sd,
    consistent = consistent,
    label_value = function(v) formatC(v, format = "f", digits = digits_sd),
    claim = c(claim$lower[1L], claim$upper[1L]),
    x_label = paste0(
      "SD (mean = ",
      formatC(x, format = "f", digits = digits_x),
      ", n = ",
      format(n, scientific = FALSE),
      if (items_averaged != 1) {
        paste0(
          ", items_averaged = ",
          format(items_averaged, scientific = FALSE)
        )
      },
      if (sd_meaning != "sd_sample") {
        paste0(", sd_meaning = \"", sd_meaning, "\"")
      },
      ")"
    ),
    color_verdict = if (consistent) color_con else color_incon,
    color_attainable = color_attainable
  )
}
