# # Full example inputs:
# x         <- "5.19"
# n         <- 40
# items     <- 1
# percent   <- FALSE
# show_rec  <- FALSE
# rounding  <- "up_or_down"
# threshold <- 5
# symmetric <- FALSE
# tolerance <- .Machine$double.eps^0.5

# Single-case function; not exported but used as a basis for the vectorized
# `grim()` as well as within `grim_map()`:
grim_scalar <- function(
  x,
  n,
  digits_x,
  items = 1,
  percent = FALSE,
  show_rec = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  tolerance = .Machine$double.eps^0.5
) {
  check_type(items, c("double", "integer"))
  check_type(percent, "logical")

  if (missing(digits_x)) {
    error_digits_missing(x)
  }

  check_newly_numeric(x, digits_x)

  x_num <- as.numeric(x)

  # The `percent` argument allows for easy conversion of percentages to decimal
  # numbers:
  if (percent) {
    x_num <- x_num / 100
    digits_x <- digits_x + 2L
  }

  # Prepare further objects for reconstructing the original values:
  n_items <- n * items
  rec_sum <- x_num * n_items

  # Determine the range of integer sums whose mean would have been reported as
  # `x_num` at `digits_x` decimal places. `sum_range()` derives it in exact
  # integer arithmetic, so that a sum which sits mathematically right on a
  # rounding boundary is included or excluded as the rounding method demands,
  # rather than as floating-point representation error happens to dictate:
  sums_consistent <- sum_range(
    x_num = x_num,
    n_items = n_items,
    digits = digits_x,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # `x` is GRIM-consistent if at least one integer sum falls into that range.
  # (For every rounding method that maps `x_num` to itself -- i.e. all of them
  # except `"anti_trunc"` -- this is equivalent to the classic formulation in
  # terms of the two granules below: the range then contains `rec_sum`, so
  # whenever it is wide enough to contain an integer at all, it also contains
  # one of the two integers closest to `rec_sum`.)
  consistency <- sums_consistent[1L] <= sums_consistent[2L]

  if (!show_rec) {
    return(consistency)
  }

  # Reconstruct the possible mean or percentage values ("granules"). These are
  # `floor(rec_sum) / n_items` and `ceiling(rec_sum) / n_items`, but computed
  # via exact division so that an `rec_sum` which is mathematically an integer
  # is not floored or ceilinged to its neighbor by floating-point error. They
  # are the values GRIM is classically taught in terms of, and they depend on
  # `rec_sum` and `n_items` alone -- not on the rounding method:
  denom <- 10^(digits_x + 1L)
  rec_sum_num <- round(x_num * denom) * n_items
  rec_x_upper <- ceiling_div(rec_sum_num, denom) / n_items
  rec_x_lower <- floor_div(rec_sum_num, denom) / n_items

  # Return the same six values for every rounding method. `sum_lower` and
  # `sum_upper` are the numbers that actually decided `consistency` above: the
  # least and the greatest integer sum that would have been reported as `x`.
  # They also say how far off an inconsistent value set is, because the range is
  # empty exactly if the set is inconsistent.
  #
  # Up to scrutiny 1.0.0, the display was granule-based instead: the two
  # granules, re-rounded, in four columns for the "_or_" rounding methods and
  # two for the others. That was a second, parallel derivation of the verdict,
  # left behind when the verdict itself moved to exact integer arithmetic, and
  # for `rounding = "anti_trunc"` it could contradict the `consistency` column
  # it was meant to explain. The deciding numbers cannot contradict it.
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
#' @description `grim()` checks if a reported mean value of integer data is
#'   mathematically consistent with the reported sample size and the number of
#'   items that compose the mean value.
#'
#'   Set `percent` to `TRUE` if `x` is a percentage. This will convert `x` to a
#'   decimal number and adjust the decimal count accordingly.
#'
#'   The function is vectorized, but it is recommended to use [`grim_map()`] for
#'   testing multiple cases.
#'
#' @details `digits_x` needs to be specified because trailing zeros are as
#'   important for the GRIM test as any other decimal digits, and numeric values
#'   don't retain them: `1.40` is stored as the number `1.4`. State the number
#'   of decimal places that `x` was reported with, not the number that happen to
#'   survive in the numeric value.
#'
#'   Browse the source code in the grim.R file. `grim()` is a vectorized version
#'   of the internal `grim_scalar()` function found there.
#'
#'   `grim()` decides which reconstructed means are consistent with `x` using
#'   exact integer arithmetic, so `tolerance` has no effect on its results. The
#'   argument is retained because [`grimmer()`] and [`debit()`] inherit it and
#'   do use it.
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
#' @param show_rec Logical. For internal use only. If set to `TRUE`, the output
#'   is a list that also contains the reconstructed values from GRIM-testing.
#'   Don't specify this manually; instead, use `show_rec` in [`grim_map()`].
#'   Default is `FALSE`.
#' @param rounding String. Rounding method or methods to be used for
#'   reconstructing the values to which `x` will be compared. Default is
#'   `"up_or_down"` (from 5).
#' @param threshold Numeric. If `rounding` is set to `"up_from"`, `"down_from"`,
#'   or `"up_from_or_down_from"`, set `threshold` to the number from which the
#'   reconstructed values should then be rounded up or down. Otherwise, this
#'   argument plays no role. Default is `5`.
#' @param symmetric Logical. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers with `"up"`, `"down"`, `"up_from"`, or `"down_from"`
#'   should mirror that of positive numbers so that their absolute values are
#'   always equal. Default is `FALSE`.
#' @param tolerance Numeric. Tolerance of comparison between `x` and the
#'   possible mean or percentage values. Default is circa 0.000000015
#'   (1.490116e-08), as in [`dplyr::near()`].
#'
#' @return Logical. `TRUE` if `x`, `n`, and `items` are mutually consistent,
#'   `FALSE` if not.
#'
#' @seealso [`grim_map()`] applies `grim()` to any number of cases at once.
#'
#' @include utils.R decimal-places.R reround.R
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
#' grim(x = 5.19, n = 28, digits_x = 2)
#'
#' # However, it is consistent with a sample size of 32:
#' grim(x = 5.19, n = 32, digits_x = 2)
#'
#' # For a scale composed of two items:
#' grim(x = 2.84, n = 16, digits_x = 2, items = 2)
#'
#' # With percentages instead of means -- here, 71%:
#' grim(x = 71, n = 43, digits_x = 0, percent = TRUE)

# Vectorized version:
grim <- Vectorize(grim_scalar)
