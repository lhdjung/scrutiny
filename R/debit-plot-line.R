#' Visualize the standard deviations that DEBIT admits
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `debit_plot_line()` draws the DEBIT test for a single value set, as
#'   [`grimmer_plot_line()`] does for GRIMMER. Of all SDs that could have been
#'   reported with `digits_sd` decimal places, each square is one that `n`
#'   binary values with mean `x` could have; the others are left out. The
#'   reported SD is marked above the row and labeled as consistent or
#'   inconsistent.
#'
#'   For many value sets, [`debit_plot()`] shows the results of
#'   [`debit_map()`]. [`debit_values()`] returns the attainable values as
#'   numbers.
#'
#' @param x,sd,n,digits_x,digits_sd,sd_meaning,formula,rounding,threshold,symmetric
#'   The same as in [`debit()`], but `x`, `sd`, and `n` must have length 1.
#' @param data_type String. `"integer"` if the values are 0 and 1, as in
#'   [`debit()`]. There is no default, so that every call states this
#'   assumption. `"continuous"` and `NA` are errors: if the data may be
#'   continuous, there are no values to draw.
#' @inheritParams grim_plot_line
#'
#' @details By default, the row shows the SDs around `sd`, as in
#'   [`grim_plot_line()`], where squares are labeled the same way. With
#'   `zoom = "wide"`, it runs from 0 to 1, the range of `debit()`'s inputs.
#'
#'   Under `formula = "mean_n"`, the squares are the SDs of every mean that
#'   would be reported as `x`, whether or not `n` binary values can have it.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @examples
#' # A mean of 0.35 with an SD of 0.49 is consistent with 20 binary values,
#' # but 0.41 is not:
#' debit_plot_line(
#'   x = 0.35,
#'   sd = 0.49,
#'   n = 20,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   data_type = "integer"
#' )
#' debit_plot_line(
#'   x = 0.35,
#'   sd = 0.41,
#'   n = 20,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   data_type = "integer"
#' )

debit_plot_line <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  zoom = c("close", "medium", "wide"),
  color_con = "#0047ff",
  color_incon = "#ff2a00",
  color_attainable = "grey75"
) {
  check_args_given(c("x", "sd", "n"))
  if (missing(digits_x)) {
    error_digits_missing(x)
  }
  if (missing(digits_sd)) {
    error_digits_missing(sd)
  }
  check_length(x, 1L)
  check_length(sd, 1L)
  check_length(n, 1L)
  if (missing(data_type)) {
    error_data_type_missing()
  }
  check_data_type_whole(data_type, allow_na = FALSE)
  # As in the test itself: a string is a mistake, not an undecidable value:
  if (!is.numeric(n) && !is.na(n)) {
    check_type(n, c("double", "integer"))
  }
  n <- round_whole(n)
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
  check_debit_inputs_all(x, sd)
  sd_meaning <- arg_match_in_export(sd_meaning, SD_MEANINGS)
  formula <- arg_match_in_export(formula, DEBIT_FORMULAS)
  zoom <- arg_match_in_export(zoom, COMB_ZOOMS)

  # `n` is needed for the window before the test itself gets to reject it,
  # and an undecidable case has nothing to draw:
  min_n <- sd_min_n(sd_meaning)
  if (!is_decidable_n_items(n, 1, min_n = min_n)) {
    abort_in_export(
      "`n` must be a whole number of at least {min_n}.",
      "x" = "It is {n}.",
      "i" = "Otherwise, the test can't be decided, so there is nothing to \\
      draw."
    )
  }

  test <- function(sd) {
    debit(
      x = x,
      sd = sd,
      n = n,
      digits_x = digits_x,
      digits_sd = digits_sd,
      data_type = "integer",
      sd_meaning = sd_meaning,
      formula = formula,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )
  }

  consistent <- test(sd)
  window <- comb_window(
    reported = sd,
    consistent = consistent,
    digits = digits_sd,
    lower = 0,
    upper = 1,
    limits = c(0, 1),
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
      if (sd_meaning != "sd_sample") {
        paste0(", sd_meaning = \"", sd_meaning, "\"")
      },
      if (formula != "exact") {
        paste0(", formula = \"", formula, "\"")
      },
      ")"
    ),
    color_verdict = if (consistent) color_con else color_incon,
    color_attainable = color_attainable
  )
}
