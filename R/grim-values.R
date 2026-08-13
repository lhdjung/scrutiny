# The two exported functions below share all of their work with each other and
# with `grim_scalar()`: `sum_range()` returns the least and the greatest whole
# number sum total that would have been reported as `x`. Everything else is
# division and comparison.

grim_sums_scalar <- function(
  x,
  n,
  digits_x,
  items,
  percent,
  rounding,
  threshold,
  symmetric
) {
  if (missing(digits_x)) {
    error_digits_missing(x)
  }

  check_newly_numeric(x, digits_x)

  x_num <- as.numeric(x)

  if (percent) {
    x_num <- x_num / 100
    digits_x <- digits_x + 2L
  }

  n_items <- n * items

  list(
    sums = sum_range(
      x_num = x_num,
      n_items = n_items,
      digits = digits_x,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    ),
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
  items = 1,
  percent = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  parts <- grim_sums_scalar(
    x,
    n,
    digits_x,
    items,
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
  items = 1,
  percent = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  parts <- grim_sums_scalar(
    x,
    n,
    digits_x,
    items,
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
    sum_nearest <- round(as.numeric(x) * parts$n_items / parts$scale)
    unique(c(
      min(max(sum_nearest, sum1), sum2),
      sum1,
      sum2
    ))
  }

  values <- sums_candidate * parts$scale / parts$n_items
  values[which.min(abs(values - as.numeric(x)))]
}


#' Achievable means behind a reported one
#'
#' @description These functions reconstruct the mean or percentage values that
#'   integer data of the reported sample size could actually have produced:
#'
#'   - `grim_values()` returns every achievable value that would have been
#'   reported as `x` -- i.e., every value that GRIM accepts. It is empty if `x`
#'   is GRIM-inconsistent with `n`.
#'   - `grim_closest()` returns the single achievable value closest to `x`. For
#'   a GRIM-consistent value set that is one of the `grim_values()`; for an
#'   inconsistent one it is the nearest value that `x` could have been.
#'
#'   Both functions are vectorized, like [`grim()`]. Since a reported mean can
#'   stand for any number of achievable ones, `grim_values()` returns a list
#'   with one element per value set.
#'
#' @details A mean of integer data is a whole-number sum total divided by the
#'   sample size (times the number of `items`). The achievable means are
#'   therefore the whole numbers in the range that [`grim_map()`] shows as
#'   `sum_lower` and `sum_upper` under `show_rec`, each divided by `n * items`.
#'
#'   There are `sum_upper - sum_lower + 1` of them, or none if `x` is
#'   inconsistent. The count grows without bound once `n * items` reaches
#'   `10^digits_x`, the point where GRIM stops being informative and
#'   [`grim_probability()`] returns 0. `grim_closest()` is cheap in that
#'   regime, `grim_values()` is not.
#'
#'   With `percent` set to `TRUE`, the values are returned on the scale of `x`,
#'   as percentages rather than as decimal numbers, so that they can be read
#'   against it directly.
#'
#' @param x Numeric. The reported mean or percentage value.
#' @param n Integer. The reported sample size.
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default because it cannot be inferred from a
#'   numeric `x`, which has no trailing zeros: both `1.4` and `1.40` are the
#'   number `1.4`, but only the latter has `digits_x = 2`.
#' @param items Numeric. The number of items composing `x`. Default is 1, the
#'   most common case.
#' @param percent Logical. Set `percent` to `TRUE` if `x` is a percentage. This
#'   will convert it to a decimal number and adjust the decimal count (i.e.,
#'   increase it by 2). Default is `FALSE`.
#' @param rounding,threshold,symmetric Further parameters of GRIM-testing; see
#'   documentation for [`grim()`].
#'
#' @return
#' - For `grim_values()`, a list of numeric vectors, one per value set. A
#'   vector is empty if the value set is GRIM-inconsistent, and `NA` if
#'   consistency is undecidable.
#' - For `grim_closest()`, a numeric vector as long as the inputs.
#'
#' @seealso [`grim()`] for the test itself, [`grim_map()`] for applying it to
#'   many cases at once, and [`grim_probability()`] for how informative it is
#'   in a given case.
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
#' grim_values(x = 5.19, n = 32, digits_x = 2)
#'
#' # With a larger sample, several means round to 5.19:
#' grim_values(x = 5.19, n = 300, digits_x = 2)
#'
#' # It is not consistent with a sample size of 28, so
#' # there are no achievable means at all...
#' grim_values(x = 5.19, n = 28, digits_x = 2)
#'
#' # ...but 5.19 is close to one that is achievable:
#' grim_closest(x = 5.19, n = 28, digits_x = 2)
#'
#' # Both functions are vectorized:
#' grim_closest(x = c(5.19, 4.2), n = c(28, 30), digits_x = 2)

grim_values <- Vectorize(grim_values_scalar, SIMPLIFY = FALSE)


#' @rdname grim_values
#' @export
grim_closest <- Vectorize(grim_closest_scalar)
