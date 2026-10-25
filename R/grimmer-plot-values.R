#' Visualize the standard deviations that GRIMMER admits
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `grimmer_plot_values()` draws the GRIMMER test for a single
#'   value set, as [`grim_plot_values()`] does for GRIM. Every SD that could
#'   have been reported with `digits_sd` decimal places within a window is a
#'   tooth of a comb; the ones that a sample of size `n` with mean `x` could
#'   have produced are tall. The reported SD is marked above the comb and
#'   labeled with its verdict.
#'
#'   There is no GRIMMER-specific plot for many value sets; [`grim_plot_grid()`]
#'   accepts [`grimmer_map()`] output and shows the GRIM part of the test.
#'
#' @param x,sd,n,digits_x,digits_sd,items_averaged,rounding,threshold,symmetric,sd_meaning
#'   The same as in [`grimmer()`], but `x`, `sd`, and `n` must have length 1.
#' @inheritParams grim_plot_values
#'
#' @details The comb runs from the whole number at or below `sd` to the next
#'   one up, but not below zero.
#'
#'   If `x` is GRIM-inconsistent with `n`, no SD is attainable and the comb has
#'   no tall teeth.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @examples
#' # A mean of 5.23 with an SD of 2.55 is not consistent with a sample size of
#' # 35, but it is with 31:
#' grimmer_plot_values(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 35,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1
#' )
#' grimmer_plot_values(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 31,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1
#' )

grimmer_plot_values <- function(
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
  color_cons = "#2f7d74",
  color_incons = "#d9d4ca",
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
  if (missing(items_averaged)) {
    error_items_missing()
  }
  check_length(items_averaged, 1L)
  check_newly_numeric(x, digits_x)
  check_newly_numeric(sd, digits_sd)
  check_comb_value(x, digits_x, "x")
  check_comb_value(sd, digits_sd, "sd")
  sd_meaning <- arg_match_in_export(sd_meaning, SD_MEANINGS)
  check_decidable_n_items(n, items_averaged, min_n = sd_min_n(sd_meaning))

  # One unit around `sd`, as for a mean:
  values <- comb_values(max(0, floor(sd)), floor(sd) + 1, digits_sd)

  test <- function(sd) {
    grimmer(
      x = x,
      sd = sd,
      n = n,
      digits_x = digits_x,
      digits_sd = digits_sd,
      items_averaged = items_averaged,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric,
      sd_meaning = sd_meaning
    )
  }

  possible <- test(values)
  consistent <- test(sd)

  claim <- unround(
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
    values = values,
    possible = possible,
    reported = sd,
    consistent = consistent,
    label_value = function(v) formatC(v, format = "f", digits = digits_sd),
    claim = c(claim$lower[1L], claim$upper[1L]),
    x_label = paste0(
      "Reported SD (mean = ",
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
    color_cons = color_cons,
    color_incons = color_incons,
    color_reported = if (consistent) color_cons else color_reported
  )
}
