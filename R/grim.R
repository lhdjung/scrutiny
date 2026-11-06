# Single-case function behind `grim()` and `grim_map()`.
grim_scalar <- function(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  show_rec = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  is_percentage <- arg_match_in_export(x_meaning, X_MEANINGS) == "percentage"
  check_flag(show_rec)
  # A missing `n` is undecidable, but a string is a mistake: comparing it with
  # numbers would make the verdict an unexplained `NA`.
  if (!is.numeric(n) && !is.na(n)) {
    check_type(n, c("double", "integer"))
  }

  if (missing(digits_x)) {
    error_digits_missing(x)
  }

  if (missing(items_averaged)) {
    error_items_missing()
  }
  # A plain `NA` is logical, but missing all the same:
  if (!is.numeric(items_averaged) && !is.na(items_averaged)) {
    check_type(items_averaged, c("double", "integer"))
  }

  if (missing(data_type)) {
    error_data_type_missing()
  }

  scale <- rounding_scale(rounding, threshold, symmetric)
  check_newly_numeric(x, digits_x, scale = scale)

  if (!identical(data_type, "integer")) {
    verdict <- unname(verdict_continuous(n, items_averaged, x = x))
    return(test_data_type(
      data_type,
      test_integer = function() {
        grim_scalar(
          x = x,
          n = n,
          digits_x = digits_x,
          items_averaged = items_averaged,
          data_type = "integer",
          x_meaning = x_meaning,
          show_rec = show_rec,
          rounding = rounding,
          threshold = threshold,
          symmetric = symmetric
        )
      },
      continuous = if (show_rec) {
        c(list(verdict), rep(list(NA_real_), 5L))
      } else {
        verdict
      },
      missing = if (show_rec) c(list(NA), rep(list(NA_real_), 5L)) else NA
    ))
  }

  # GRIM asks which integer sums of `n * items_averaged` integer values have a
  # mean that would be reported as `x`. Undecidable rather than inconsistent:
  # a fractional or non-positive `n` or `items_averaged`, which describes no
  # such data set; a missing `x`, which may be a character `NA` that the
  # arithmetic below would fail on; an infinite `x`, which would make
  # `sum_range()` infinitely wide, i.e., consistent with everything; and a
  # missing `digits_x`, which leaves no rounding bounds to derive.
  if (
    !is_decidable_n_items(n, items_averaged) ||
      !is.finite(x) ||
      is.na(digits_x)
  ) {
    if (!show_rec) {
      return(NA)
    }
    return(list(NA, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
  }

  # `x` itself stays on the reported scale for the `show_rec` values:
  x_num <- x
  if (is_percentage) {
    x_num <- x_num / 100
    digits_x <- digits_x + 2L
  }

  n_items <- as.numeric(n) * items_averaged
  rec_sum <- x_num * n_items

  # Inlined, as it runs once per row; the call only raises the error:
  if ((abs(x_num) * 10^(digits_x + 1L) + 10) * n_items * scale >= 2^53) {
    check_sum_range_exact(x_num, n_items, digits_x, scale = scale)
  }

  # The integer sums whose mean would have been reported as `x_num` at
  # `digits_x` decimal places, in exact integer arithmetic: a sum sitting right
  # on a rounding boundary is included or excluded as the rounding method
  # demands, not as representation error happens to dictate.
  sums_consistent <- roundwork::sum_range(
    x_num = x_num,
    n_items = n_items,
    digits = digits_x,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # `x` is GRIM-consistent if at least one integer sum falls into that range.
  # Equivalent to the classic formulation via the granules below: the range
  # contains `rec_sum`, so if it contains any integer, it contains one of the
  # two closest to `rec_sum`.
  consistency <- sums_consistent[1L] <= sums_consistent[2L]

  if (!show_rec) {
    return(consistency)
  }

  # The possible mean or percentage values ("granules"): `floor(rec_sum)` and
  # `ceiling(rec_sum)` over `n_items`, but via exact division so that a
  # mathematically integral `rec_sum` is not moved to its neighbor by
  # floating-point error. They depend on `rec_sum` and `n_items` alone, not on
  # the rounding method:
  denom <- 10^(digits_x + 1L)
  rec_sum_num <- round(x_num * denom) * n_items

  # On the scale of `x`, like `grim_values()` and `grim_closest()`. The sums are
  # not converted: `x_meaning` does not change the underlying data. The factor
  # multiplies the integer sum, not the quotient, so the granule is the result
  # of a single division; converting afterwards would round twice.
  scale_x <- if (is_percentage) 100 else 1
  rec_x_upper <- ceiling(rec_sum_num / denom) * scale_x / n_items
  rec_x_lower <- floor(rec_sum_num / denom) * scale_x / n_items

  # The same six values for every rounding method. `sum_lower` and `sum_upper`
  # are the numbers that decided `consistency` above, so the display cannot
  # contradict the verdict. An empty range is empty by exactly one, the bounds
  # straddling `rec_sum`, so the gap does not measure how far off a value set
  # is; `x` against `rec_x_lower` and `rec_x_upper` does.
  list(
    consistency,
    rec_sum,
    sums_consistent[1L],
    sums_consistent[2L],
    rec_x_upper,
    rec_x_lower
  )
}


#' The GRIM test (granularity-related inconsistency of means)
#'
#' @description `grim()` checks whether a reported mean of integer data is
#'   consistent with the reported sample size and the number of items that
#'   compose the mean. The mean of `n * items_averaged` integers is a whole
#'   number divided by `n * items_averaged`, so only certain values are
#'   possible; the test asks whether any of them would have been reported as
#'   `x`.
#'
#'   Set `x_meaning` to `"percentage"` if `x` is a percentage. It is then
#'   divided by 100 and tested with two more decimal places.
#'
#'   The function is vectorized, but [`grim_map()`] is more convenient for
#'   testing many cases.
#'
#' @details `items_averaged` must be specified because a mean over several
#'   items is a mean of more values, which changes the test. A default of 1
#'   would let multi-item scales pass unnoticed. `data_type` must be specified
#'   because GRIM only applies to whole numbers. A default of `"integer"`
#'   would let the test flag means of continuous data, which can take any
#'   value.
#'
#'   `x`, `n`, `digits_x`, `items_averaged`, and `data_type` are vectorized
#'   and recycled to a common length by the tidyverse rules: an argument has
#'   length 1 or the length of the longest. All other arguments must have length 1.
#'
#'   Numbers beyond the precision of a double are an error rather than a
#'   guess. `x` with its `digits_x` may have about 15 significant digits:
#'   `x * 10^(digits_x + 1)` must be below 2^52, about 4.5e15, and
#'   `(x * 10^(digits_x + 1) + 10) * n * items_averaged` below 2^53, about
#'   9e15. A `threshold` with decimal places raises the power of ten.
#'
#' @param x Numeric. The reported mean or percentage.
#' @param n Integer. The reported sample size.
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default, since a numeric value does not
#'   keep them (`1.40` is `1.4`).
#' @param items_averaged Integer. The number of items averaged into each
#'   respondent's score: 1 for a single item or the percentage of participants
#'   with some answer, 4 for the mean of a four-item scale, 10 for the average
#'   percent correct on a ten-question test. Not the number of scale points.
#'   No default.
#' @param data_type String. The kind of data that `x` summarizes. No default.
#'   - `"integer"`: whole numbers, such as answers on a rating scale, counts,
#'   or yes/no answers for a percentage.
#'   - `"quasi-integer"`: whole numbers that stand for something continuous,
#'   such as age in years or reaction times in whole milliseconds. They are
#'   tested like `"integer"` data, with a warning, once per session, that
#'   the result only holds if the analysis used the recorded whole numbers.
#'   - `"continuous"`: anything else, such as body mass index or a visual
#'   analogue scale. The test does not apply, and the result is `TRUE`
#'   unless the value set describes no data, as with `n = 0`, which is `NA`
#'   as it is for whole numbers.
#'
#'   If it is `NA`, the result is the one for whole numbers if it is the same
#'   for continuous data, and `NA` otherwise.
#' @param x_meaning String. What `x` is: `"mean"` (the default) or
#'   `"percentage"`. A percentage is divided by 100 and tested with two more
#'   decimal places, as the proportion it is. See *Defaults* in [`grim()`].
#' @param percent `r lifecycle::badge("deprecated")` Replaced by `x_meaning`:
#'   use `x_meaning = "percentage"` instead of `percent = TRUE`.
#' @param rounding String. The rounding method assumed to have produced `x`.
#'   Default is `"up_or_down"`, which accepts either direction; see
#'   *Defaults* in [`grim()`]. The others are `"up_from_or_down_from"`,
#'   `"ceiling_or_floor"`, `"up"`, `"down"`, `"even"`,
#'   `"up_from"`, `"down_from"`, `"ceiling"`, `"floor"`, `"trunc"`,
#'   `"anti_trunc"`, `"ties_up"`, `"ties_down"`, `"ties_away"`, `"ties_zero"`,
#'   and `"ties_even"`. See
#'   `vignette("rounding-options", package = "roundwork")`.
#' @param threshold Numeric. For `rounding = "up_from"`, `"down_from"`, or
#'   `"up_from_or_down_from"`, the point within a step at which rounding
#'   switches direction, in tenths of a step. Default is `5`. Ignored
#'   otherwise.
#' @param symmetric Logical. If `TRUE`, negative numbers are rounded like
#'   their absolute values (`-2.5` like `2.5`) under `"up"`, `"down"`, and the
#'   `"*_from"` methods. Default is `FALSE`. Must not be given with a
#'   `"ties_*"` rounding method, which already fixes the direction; see
#'   [`roundwork::reround()`].
#'
#' @section Defaults: The defaults are meant to flag a value set only if it is
#'   inconsistent under every usual convention:
#'
#'   - `rounding = "up_or_down"`: Reports rarely say how they rounded, and the
#'   common methods only disagree on ties. SPSS and Excel round a 5 away from
#'   zero, while R's `round()` and Python's `round()` round it to an even
#'   digit. The default accepts a tie rounded either way, so a value set that
#'   fails is inconsistent under all of them. Choose another method only if
#'   you know which one was used. `threshold` and `symmetric` do not affect
#'   it.
#'   - `x_meaning = "mean"`: GRIM was devised for means, and most values that
#'   it is applied to are means. A percentage needs to be marked as one, since
#'   the same number, such as 71, can be either.
#'
#' @return Logical. `TRUE` if `x`, `n`, and `items_averaged` are consistent,
#'   `FALSE` if not, and `NA` if the case cannot be decided: if a value is
#'   missing or infinite, or if `n` or `items_averaged` is not a positive whole
#'   number.
#'
#' @seealso [`grim_explain()`] to explain a result in plain words,
#'   [`grim_map()`] for data frames, [`grim_values()`] for the means that `n`
#'   integers can produce, and [`grim_exp_incon()`] for how informative the
#'   test is in a given case.
#'
#' @include utils.R decimal-places.R
#'
#' @rdname grim
#'
#' @export
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876
#'
#' @examples
#' # A mean of 5.19 is not consistent with a sample size of 28:
#' grim(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")
#'
#' # However, it is consistent with a sample size of 32:
#' grim(x = 5.19, n = 32, digits_x = 2, items_averaged = 1, data_type = "integer")
#'
#' # For a scale composed of two items:
#' grim(x = 2.84, n = 16, digits_x = 2, items_averaged = 2, data_type = "integer")
#'
#' # With percentages instead of means -- here, 71%:
#' grim(
#'   x = 71,
#'   n = 43,
#'   digits_x = 0,
#'   items_averaged = 1,
#'   data_type = "integer",
#'   x_meaning = "percentage"
#' )
#'
#' # Means of continuous data, such as reaction times, can take any value:
#' grim(
#'   x = 5.19,
#'   n = 28,
#'   digits_x = 2,
#'   items_averaged = 1,
#'   data_type = "continuous"
#' )

# The signature mirrors `grim_scalar()`'s minus `show_rec`; see
# `map_value_sets()`.
grim <- function(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  percent = deprecated()
) {
  if (lifecycle::is_present(percent)) {
    stop_percent("grim")
  }
  map_value_sets(
    fn = grim_scalar,
    along = c("x", "n", "digits_x", "items_averaged", "data_type")
  )
}
