# The two exported functions below share all of their work with each other and
# with `grim_scalar()`: `sum_range()` returns the least and the greatest whole
# number sum total that would have been reported as `x`. Everything else is
# division and comparison.

grim_sums_scalar <- function(
  x,
  n,
  digits_x,
  items_averaged,
  percent,
  rounding,
  threshold,
  symmetric
) {
  if (missing(digits_x)) {
    error_digits_missing(x)
  }
  if (missing(items_averaged)) {
    error_items_missing()
  }

  check_newly_numeric(x, digits_x)

  x_num <- x

  if (percent) {
    x_num <- x_num / 100
    digits_x <- digits_x + 2L
  }

  n_items <- n * items_averaged

  # An infinite `x` is undecidable, exactly as in `grim_scalar()`: no data set
  # has an infinite mean, and `sum_range()` would answer with an infinitely wide
  # range that `seq()` then refuses to walk. Both callers read the sums with
  # `anyNA()`, which is how a missing `x` arrives here as well:
  # So is a sample size or item count that is no positive whole number, as in
  # every other test (see `is_decidable_n_items()`):
  sums <- if (
    is.infinite(x_num) ||
      is.na(digits_x) ||
      !is_decidable_n_items(n, items_averaged)
  ) {
    c(NA_real_, NA_real_)
  } else {
    sum_range(
      x_num = x_num,
      n_items = n_items,
      digits = digits_x,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )
  }

  list(
    sums = sums,
    n_items = n_items,
    # Values are returned on the scale of `x` itself, so that they can be read
    # against it. Internally, a percentage is a decimal number:
    scale = if (percent) 100 else 1
  )
}


grim_values_scalar <- function(
  x,
  n,
  digits_x,
  items_averaged,
  percent = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  parts <- grim_sums_scalar(
    x,
    n,
    digits_x,
    items_averaged,
    percent,
    rounding,
    threshold,
    symmetric
  )
  sums <- parts$sums

  # The bounds are undefined -- as with a missing `x` -- so consistency is
  # undecidable, and so is the set of achievable means:
  if (anyNA(sums)) {
    return(NA_real_)
  }

  sum1 <- sums[1L]
  sum2 <- sums[2L]

  # An empty range means no whole-number sum total would have been reported as
  # `x`; i.e., GRIM-inconsistency:
  if (sum1 > sum2) {
    return(numeric(0L))
  }

  seq(sum1, sum2) * parts$scale / parts$n_items
}


grim_closest_scalar <- function(
  x,
  n,
  digits_x,
  items_averaged,
  percent = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  parts <- grim_sums_scalar(
    x,
    n,
    digits_x,
    items_averaged,
    percent,
    rounding,
    threshold,
    symmetric
  )
  sums <- parts$sums

  if (anyNA(sums)) {
    return(NA_real_)
  }

  sum1 <- sums[1L]
  sum2 <- sums[2L]

  sums_candidate <- if (sum1 > sum2) {
    # An empty range is empty by exactly one integer: its endpoints are the two
    # whole-number sum totals that straddle `x`, the lower one first.
    c(sum2, sum1)
  } else {
    # The whole number closest to the unrounded sum total, pulled into the
    # admissible range. The endpoints come along in case `round()` lands one
    # integer off through floating-point error in the product:
    sum_nearest <- round(x * parts$n_items / parts$scale)
    sum_nearest <- min(max(sum_nearest, sum1), sum2)
    unique(c(sum_nearest, sum1, sum2))
  }

  values <- sums_candidate * parts$scale / parts$n_items
  values[which.min(abs(values - x))]
}


#' Achievable means behind a reported one
#'
#' @description These functions return the means (or percentages) that a sample
#'   of `n * items_averaged` integers can have:
#'
#'   - `grim_values()` returns every achievable value that would be reported as
#'   `x`. The result is empty if `x` is GRIM-inconsistent with `n`.
#'   - `grim_closest()` returns the single achievable value closest to `x`,
#'   whether or not `x` is consistent.
#'
#'   Both are vectorized like [`grim()`]. A reported mean may correspond to one
#'   achievable mean, to several, or to none, so `grim_values()` returns a list
#'   with one element per value set.
#'
#' @details The achievable means are the whole numbers in the range that
#'   [`grim_map()`] shows as `sum_lower` and `sum_upper` under `show_rec`, each
#'   divided by `n * items_averaged`. Once `n * items_averaged` exceeds
#'   `10^digits_x`, several of them can be reported as the same `x`, and their
#'   number grows with `n * items_averaged`. GRIM is then no longer informative.
#'   `grim_closest()` stays cheap in that case; `grim_values()` does not.
#'
#'   With `percent = TRUE`, the values are percentages, like `x`.
#'
#' @param x Numeric. The reported mean or percentage.
#' @param n Integer. The reported sample size.
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default, since a numeric value does not
#'   keep them (`1.40` is `1.4`).
#' @param items_averaged Integer. As in [`grim()`].
#' @param percent Logical. If `TRUE`, `x` is a percentage. Default is `FALSE`.
#' @param rounding,threshold,symmetric The same as in [`grim()`].
#'
#' @return
#' - For `grim_values()`, a list of numeric vectors, one per value set. A
#'   vector is empty if the value set is inconsistent, and `NA` if it cannot
#'   be decided.
#' - For `grim_closest()`, a numeric vector as long as the inputs.
#'
#' @seealso [`grim()`], [`grim_map()`], and [`grim_probability()`].
#'
#' @include grim.R unround.R
#'
#' @rdname grim_values
#'
#' @export
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876
#'
#' @examples
#' # A mean of 5.19 is consistent with a sample size of 32,
#' # and there is exactly one way to get there:
#' grim_values(x = 5.19, n = 32, digits_x = 2, items_averaged = 1)
#'
#' # With a larger sample, several means round to 5.19:
#' grim_values(x = 5.19, n = 300, digits_x = 2, items_averaged = 1)
#'
#' # It is not consistent with a sample size of 28...
#' grim_values(x = 5.19, n = 28, digits_x = 2, items_averaged = 1)
#'
#' # ...but 5.19 is close to a mean that is achievable:
#' grim_closest(x = 5.19, n = 28, digits_x = 2, items_averaged = 1)
#'
#' # Both functions are vectorized:
#' grim_closest(
#'   x = c(5.19, 4.2),
#'   n = c(28, 30),
#'   digits_x = 2,
#'   items_averaged = 1
#' )

grim_values <- Vectorize(grim_values_scalar, SIMPLIFY = FALSE)


#' @rdname grim_values
#' @export
grim_closest <- Vectorize(grim_closest_scalar)
