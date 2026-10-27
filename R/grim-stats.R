#' Possible GRIM inconsistencies
#'
#' @description These functions quantify how informative a GRIM test is for a
#'   given sample size and number of decimal places:
#'
#'   - `grim_exp_incon()` returns the probability that a mean or percentage
#'   with `digits_x` decimal places that is otherwise random is
#'   GRIM-inconsistent with `n`. It is the `exp_incon` column of
#'   [`grim_map()`].
#'   - `grim_total()` returns the number of such inconsistent values between
#'   two whole numbers, out of `10^digits_x` (`10^(digits_x + 2)` with
#'   `percent = TRUE`).
#'
#'   See `vignette("grim")`, section *GRIM statistics*.
#'
#' @param x Numeric. The mean or percentage. Its value only matters if it is
#'   missing or infinite, which makes the result `NA`; the argument mirrors
#'   [`grim()`].
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default, since a numeric value does not
#'   keep them (`1.40` is `1.4`).
#' @param n Integer. The sample size.
#' @param items_averaged Integer. As in [`grim()`]. Like `x`, `n`, and
#'   `digits_x`, it is recycled under the tidyverse rules: all four must have
#'   the same length, or length 1.
#' @param percent Logical. If `TRUE`, `x` is a percentage, which adds 2 to the
#'   decimal count. Default is `FALSE`.
#' @param rounding,threshold The rounding method and, for the `"*_from"`
#'   methods, its threshold, as in [`grim()`]. Defaults are `"up_or_down"` and
#'   `5`. The count depends on the method: `"up_or_down"` accepts a mean on a
#'   tie in both directions, and `"ceiling_or_floor"` admits a range two steps
#'   wide. `"even"` counts a tie on either bound as consistent, like
#'   `"up_or_down"`, although [`base::round()`] may round it the other way;
#'   see [`roundwork::unround()`].
#'   [`grim_map()`] passes its own `rounding` and `threshold` on.
#'
#' @seealso [`grim()`] and [`grim_map()`].
#'
#' @return Double. `grim_exp_incon()` and `grim_total()` return `NA` where
#'   [`grim()`] cannot decide a case: if `x` is missing or infinite, or if `n`
#'   or `items_averaged` is not a positive whole number. They are never
#'   negative.
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876
#'
#' @export
#'
#' @name grim-stats
#'
#' @examples
#' # Many value sets are inconsistent here:
#' grim_exp_incon(x = 83.29, n = 21, digits_x = 2, items_averaged = 1)
#' grim_total(x = 83.29, n = 21, digits_x = 2, items_averaged = 1)
#'
#' # None are in this case...
#' grim_exp_incon(x = 5.14, n = 120, digits_x = 2, items_averaged = 1)
#' grim_total(x = 5.14, n = 120, digits_x = 2, items_averaged = 1)
#'
#' # ...but most would be if `x` were a percentage:
#' grim_exp_incon(
#'   x = 5.14,
#'   n = 120,
#'   digits_x = 2,
#'   items_averaged = 1,
#'   percent = TRUE
#' )
#' grim_total(
#'   x = 5.14,
#'   n = 120,
#'   digits_x = 2,
#'   items_averaged = 1,
#'   percent = TRUE
#' )

# Relative ----------------------------------------------------------------

grim_exp_incon <- function(
  x,
  n,
  digits_x,
  items_averaged,
  percent = FALSE,
  rounding = "up_or_down",
  threshold = 5
) {
  if (missing(digits_x)) {
    error_digits_missing(x)
  }

  out <- grim_total(
    x,
    n,
    digits_x,
    items_averaged,
    percent,
    rounding,
    threshold
  )

  if (percent) {
    digits_x <- digits_x + 2L
  }

  out / 10^digits_x
}


# Absolute ----------------------------------------------------------------

#' @rdname grim-stats
#' @export
grim_total <- function(
  x,
  n,
  digits_x,
  items_averaged,
  percent = FALSE,
  rounding = "up_or_down",
  threshold = 5
) {
  if (missing(digits_x)) {
    error_digits_missing(x)
  }

  if (missing(items_averaged)) {
    error_items_missing()
  }

  # As in `grim()`, a string is a mistake, but missing values are undecidable:
  if (!is.numeric(x) && !all(is.na(x))) {
    check_type(x, c("double", "integer"))
  }

  # As in `grim()`, which would reject a fractional `digits_x` or one outside
  # of 0 to 15:
  check_digits_whole(digits_x, "digits_x")
  if (any(digits_x < 0 | digits_x > 15, na.rm = TRUE)) {
    abort_in_export(
      "`digits_x` must be whole numbers from 0 to 15.",
      "x" = "It is {wrap_in_quotes_or_backticks(digits_x)}."
    )
  }

  if (percent) {
    digits_x <- digits_x + 2L
  }

  # Recycle as `grim()` does, so that a length mismatch is an error:
  vals <- list(x = x, n = n, digits_x = digits_x, items_averaged = items_averaged)
  length_out <- recycle_length_common(lengths(vals), names(vals))
  x <- rep_len(x, length_out)
  n <- rep_len(n, length_out)
  digits_x <- rep_len(digits_x, length_out)
  items_averaged <- rep_len(items_averaged, length_out)

  # Between two whole numbers, there are `10^digits_x` values that could be
  # reported, such as 0.00, 0.01, ..., 0.99 with two decimal places. The means
  # that the data can actually have are the multiples of
  # `1 / (n * items_averaged)`.
  n_values <- 10^digits_x
  n_means <- as.numeric(n) * items_averaged

  # `NA` exactly where `grim()` returns `NA`, so that the `exp_incon` column
  # of `grim_map()` never shows a number next to an `NA` verdict.
  decidable <- is_decidable_n_items(n, items_averaged) &
    is.finite(x) &
    is.finite(n_values)

  # A reported value is consistent if at least one possible mean lies inside its
  # rounding interval. With two decimal places and the default rounding, the
  # interval around 0.53 runs from 0.525 to 0.535. Testing each value in turn
  # would take too long with many decimal places, so the number of consistent
  # values is worked out directly.
  #
  # To do so, measure all distances in steps of `1 / (n_means * n_values)`.
  # Every reported value and every possible mean is then a whole number of steps
  # away from zero, so the distance between the two is a whole number of steps
  # as well. Those distances are all multiples of the greatest common divisor of
  # `n_means` and `n_values`, called `div_common` below. The reported values
  # fall into groups of `div_common` values each, and within a group they all
  # have the same distances to the possible means, up to whole units. For each
  # multiple of `div_common` that fits inside the rounding interval, one group
  # is consistent. The number of consistent values is therefore `div_common`
  # times the number of these multiples.
  #
  # The common divisor matters because of ties. With two decimal places and `n =
  # 40`, the possible means are 0.025, 0.050, 0.075, and so on, so every other
  # mean falls on a tie between two reported values. With `n = 30`, no mean
  # does. Under `"up_or_down"`, a mean on a tie is consistent with the values on
  # both sides of it, so `n = 40` has 60 consistent values, not the 40 that `n`
  # alone would suggest. The common divisor is what tells the two cases apart:
  # 20 for 40 and 100, but only 10 for 30 and 100.
  #
  # Base R has no greatest common divisor, so this is Euclid's algorithm,
  # vectorized. Undecidable cases start from 1 so that the loop still ends;
  # they are set to `NA` at the end.
  div_common <- dplyr::if_else(decidable, n_means, 1)
  div_main <- dplyr::if_else(decidable, n_values, 1)

  while (any(div_main > 0)) {
    unfinished <- div_main > 0
    remainder <- div_common[unfinished] %% div_main[unfinished]
    div_common[unfinished] <- div_main[unfinished]
    div_main[unfinished] <- remainder
  }

  # The rounding interval comes from `bound_numerators()`, which is also where
  # `grim()` gets it from. Only the number of decimal places should matter here,
  # not the reported value itself, so the value 1 with no decimal places stands
  # in for any value. Its bounds are 1 plus or minus some offset, stored as
  # numerators over `bounds$denom`. Subtracting `bounds$denom` leaves the
  # offsets. Multiplying them by `n_means` turns them into steps, and dividing
  # by `div_common` counts them in multiples of the divisor.
  bounds <- rethrow_in_export(
    roundwork::bound_numerators(1, 0L, rounding, threshold, symmetric = FALSE)
  )

  interval_lower <-
    (bounds$lower - bounds$denom) * n_means / (bounds$denom * div_common)

  interval_upper <-
    (bounds$upper - bounds$denom) * n_means / (bounds$denom * div_common)

  # Some rounding methods include an end of their interval and others don't, so
  # the first and last multiples inside it depend on the method.
  multiple_first <- if (bounds$incl_lower) {
    ceiling(interval_lower)
  } else {
    floor(interval_lower) + 1
  }

  multiple_last <- if (bounds$incl_upper) {
    floor(interval_upper)
  } else {
    ceiling(interval_upper) - 1
  }

  n_multiples <- pmax(multiple_last - multiple_first + 1, 0)

  # A double, not an integer: from `digits_x = 10` on (8 with `percent =
  # TRUE`), the count can exceed `.Machine$integer.max`, and the type should not
  # depend on the values. Doubles are exact for whole numbers up to 2^53.
  #
  # If the possible means lie closer together than the rounding interval is
  # wide, every reported value is consistent. The subtraction then gives a
  # negative number, which `pmax()` turns into 0.
  n_incons <- pmax(
    n_values - div_common * n_multiples,
    0
  )

  n_incons[!decidable] <- NA_real_
  n_incons
}


#' Deprecated name of `grim_exp_incon()`
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#'   `grim_probability()` was renamed to [`grim_exp_incon()`] in scrutiny
#'   1.0.0, which also takes new `digits_x` and `rounding` arguments.
#'
#' @param ... Passed on to [`grim_exp_incon()`].
#'
#' @keywords internal
#' @export

grim_probability <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "grim_probability()", "grim_exp_incon()")
  grim_exp_incon(...)
}
