#' Possible GRIM inconsistencies
#'
#' @description These functions compute statistics related to GRIM-testing. In
#'   general, `grim_probability()` is the most useful of them, and it is
#'   responsible for the `probability` column in a data frame returned by
#'   [`grim_map()`].
#'
#'   - `grim_probability()` returns the probability that a reported mean or
#'   percentage of integer data that is random except for the number of its
#'   decimal places is inconsistent with the reported sample size. For example,
#'   the mean 1.23 is treated like any other mean with two decimal places.
#'   - `grim_ratio()` is the raw formula `(10^digits_x - n * items) /
#'   10^digits_x`. It equals `grim_probability()` for rounding methods that
#'   admit exactly one step, such as `"up"`, unless `grim_ratio()` is negative,
#'   which can occur if the sample size is very large. Strictly speaking, this
#'   is more informative than `grim_probability()`, but it is harder to
#'   interpret. It takes no `rounding` argument and does not check its input.
#'   - `grim_total()` returns the absolute number of GRIM-inconsistencies that
#'   are possible given the mean or percentage's number of decimal places and
#'   the corresponding sample size.
#'
#'   For discussion, see `vignette("grim")`, section *GRIM statistics*.

#' @param x Numeric. Mean or percentage value computed from data with integer
#'   units, e.g., mean scores on a Likert scale or percentage of study
#'   participants in some condition. Only the number of decimal places matters;
#'   see `digits_x`. The argument exists for compatibility with [`grim()`].
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default because it cannot be inferred from a
#'   numeric `x`, which has no trailing zeros: both `1.4` and `1.40` are the
#'   number `1.4`, but only the latter has `digits_x = 2`.
#' @param n Integer. Sample size corresponding to `x`.
#' @param items Integer. Number of items composing the mean or percentage value
#'   in question. Default is `1`.
#' @param percent Logical. Set `percent` to `TRUE` if `x` is expressed as a
#'   proportion of 100 rather than 1. The functions will then account for this
#'   fact through increasing the decimal count by 2. Default is `FALSE`.
#' @param rounding,threshold Rounding method and, for `"up_from"` and similar
#'   methods, its threshold, as in [`grim()`]. Only in `grim_probability()` and
#'   `grim_total()`. Defaults are `"up_or_down"` and `5`. The count depends on
#'   the method: `"ceiling_or_floor"` admits a range two decimal units wide, so
#'   it rules out fewer values; `"up_or_down"` and other methods that admit both
#'   ends of their range count a mean that lands exactly on a tie toward both
#'   neighbors. [`grim_map()`] passes its own `rounding` and `threshold` on.

#' @seealso [`grim()`] for the GRIM test itself; as well as [`grim_map()`] for
#'   applying it to many cases at once.
#'
#' @return Double. The number of possible GRIM inconsistencies, or their
#'   probability for a random mean or percentage with a given number of decimal
#'   places. `grim_probability()` and `grim_total()` return `NA` where [`grim()`]
#'   does for lack of a decidable case: if `x` is missing or infinite, or if `n`
#'   or `items` is not a positive whole number. They are never negative.
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
#' grim_probability(x = 83.29, n = 21, digits_x = 2)
#' grim_total(x = 83.29, n = 21, digits_x = 2)
#'
#' # No sets are inconsistent in this case...
#' grim_probability(x = 5.14, n = 83, digits_x = 2)
#' grim_total(x = 5.14, n = 83, digits_x = 2)
#'
#' # ... but most would be if `x` was a percentage:
#' grim_probability(x = 5.14, n = 83, digits_x = 2, percent = TRUE)
#' grim_total(x = 5.14, n = 83, digits_x = 2, percent = TRUE)

# Relative ----------------------------------------------------------------

grim_probability <- function(
  x,
  n,
  digits_x,
  items = 1,
  percent = FALSE,
  rounding = "up_or_down",
  threshold = 5
) {
  out <- grim_total(x, n, digits_x, items, percent, rounding, threshold)

  if (percent) {
    digits_x <- digits_x + 2L
  }

  out / 10^digits_x
}


#' @rdname grim-stats
#' @export
grim_ratio <- function(x, n, digits_x, items = 1, percent = FALSE) {
  if (percent) {
    digits_x <- digits_x + 2L
  }

  n_values <- 10^digits_x
  (n_values - n * items) / n_values
}


# Absolute ----------------------------------------------------------------

#' @rdname grim-stats
#' @export
grim_total <- function(
  x,
  n,
  digits_x,
  items = 1,
  percent = FALSE,
  rounding = "up_or_down",
  threshold = 5
) {
  # A string `x` used to be coerced here, so that `"5.19"` got a count while
  # `grim()` rejected it. Missing values are undecidable, not mistakes:
  if (!is.numeric(x) && !all(is.na(x))) {
    check_type(x, c("double", "integer"))
  }

  # As in `grim()`, which would reject a fractional `digits_x`:
  check_digits_whole(digits_x, "digits_x")

  if (percent) {
    digits_x <- digits_x + 2L
  }

  # Between two whole numbers, there are `10^digits_x` values that could be
  # reported, such as 0.00, 0.01, ..., 0.99 with two decimal places. The means
  # that the data can actually have are the multiples of `1 / (n * items)`.
  n_values <- 10^digits_x
  n_means <- n * items

  # Where `grim()` has nothing to test, it returns `NA`, and so does this
  # function. That is the case for a missing or infinite `x`, and for an `n` or
  # `items` that is not a positive whole number. The `probability` column of
  # `grim_map()` used to show values such as `1.03` next to an `NA` verdict.
  decidable <- is_decidable_n_items(n, items) &
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
  # Base R has no function for the greatest common divisor, so the code below
  # uses Euclid's algorithm, for all elements at once. In each round,
  # `div_common` takes the value of `div_main`, and `div_main` takes the
  # remainder of dividing the old `div_common` by it. Once `div_main` is zero,
  # `div_common` holds the result. Cases that can't be decided start from 1 so
  # that the loop still ends; their results are replaced by `NA` at the end.
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
  bounds <- bound_numerators(1, 0L, rounding, threshold, symmetric = FALSE)

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

  # The count is returned as a double, even though it is a whole number. From
  # `digits_x = 10` on (or from 8 with `percent = TRUE`), it can be larger than
  # `.Machine$integer.max`, the largest integer that R can store. Returning an
  # integer only when the count fits would make the type depend on the largest
  # value in the call. A double stores whole numbers without error up to 2^53,
  # which is far beyond any count that GRIM can produce.
  #
  # If the possible means lie closer together than the rounding interval is
  # wide, every reported value is consistent. The subtraction then gives a
  # negative number, which `pmax()` turns into 0. Unlike this function,
  # `grim_ratio()` returns its raw formula, which can be negative.
  n_incons <- pmax(
    n_values - div_common * n_multiples,
    0
  )

  n_incons[!decidable] <- NA_real_
  n_incons
}
