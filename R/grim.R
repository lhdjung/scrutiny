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

  # Reconstruct the possible mean or percentage values ("granules"):
  rec_x_upper <- ceiling(rec_sum) / n_items
  rec_x_lower <- floor(rec_sum) / n_items

  # Determine the range of values that would round to x_num at digits_x decimal
  # places. unround() handles most rounding methods; the two compound methods
  # are not supported by it, so their bounds are computed as the union of their
  # two constituent methods:
  if (rounding == "ceiling_or_floor") {
    b_ceil  <- unround(x_num, "ceiling", threshold = threshold, digits = digits_x)
    b_floor <- unround(x_num, "floor",   threshold = threshold, digits = digits_x)
    lower <- min(b_ceil$lower,  b_floor$lower)
    upper <- max(b_ceil$upper, b_floor$upper)
  } else if (rounding %in% c("up_from", "down_from", "up_from_or_down_from")) {
    p10_plus1 <- 10^(digits_x + 1L)
    up_lower   <- x_num - (10 - threshold) / p10_plus1
    up_upper   <- x_num + threshold / p10_plus1
    down_lower <- x_num - threshold / p10_plus1
    down_upper <- x_num + (10 - threshold) / p10_plus1
    if (rounding == "up_from") {
      lower <- up_lower; upper <- up_upper
    } else if (rounding == "down_from") {
      lower <- down_lower; upper <- down_upper
    } else {
      lower <- min(up_lower, down_lower)
      upper <- max(up_upper, down_upper)
    }
  } else {
    bounds <- unround(x_num, rounding = rounding, threshold = threshold, digits = digits_x)
    lower <- bounds$lower
    upper <- bounds$upper
  }

  # A granule is consistent if it lies within the bounds -- i.e., if it is a
  # value that, when rounded to digits_x decimal places, gives x_num. Tolerance
  # handles floating-point imprecision near the boundary:
  granule_in_bounds <- function(g) {
    g >= lower - tolerance && g <= upper + tolerance
  }

  consistency <- granule_in_bounds(rec_x_upper) || granule_in_bounds(rec_x_lower)

  if (!show_rec) {
    return(consistency)
  }

  length_2ers <- c("up_or_down", "up_from_or_down_from", "ceiling_or_floor")

  # Round the granules for display in the reconstructed-values columns:
  granules_rounded <- reround(
    x = c(rec_x_upper, rec_x_lower),
    digits = digits_x,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  granules_rounded_subset <- if (any(length_2ers %in% rounding)) {
    # Two rounding variants per granule (e.g., "up" and "down"):
    list(
      granules_rounded[1L],
      granules_rounded[2L],
      granules_rounded[3L],
      granules_rounded[4L]
    )
  } else {
    # One rounding variant per granule:
    list(
      granules_rounded[1L],
      granules_rounded[2L]
    )
  }

  # Return a final combined list
  c(
    list(
      consistency,
      rec_sum,
      rec_x_upper,
      rec_x_lower
    ),
    granules_rounded_subset
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
#' @details The `x` values need to be strings because only strings retain
#'   trailing zeros, which are as important for the GRIM test as any other
#'   decimal digits.
#'
#'   Use [`restore_zeros()`] on numeric values (or values that were numeric
#'   values at some point) to easily supply the trailing zeros they might once
#'   have had. See documentation there.
#'
#'   Browse the source code in the grim.R file. `grim()` is a vectorized version
#'   of the internal `grim_scalar()` function found there.
#'
#' @param x String. The reported mean or percentage value.
#' @param n Integer. The reported sample size.
#' @param items Numeric. The number of items composing `x`. Default is 1, the
#'   most common case.
#' @param percent Logical. Set `percent` to `TRUE` if `x` is a percentage. This
#'   will convert it to a decimal number and adjust the decimal count (i.e.,
#'   increase it by 2). Default is `FALSE`.
#' @param show_rec Logical. For internal use only. If set to `TRUE`, the output
#'   is a matrix that also contains intermediary values from GRIM-testing. Don't
#'   specify this manually; instead, use `show_rec` in [`grim_map()`]. Default
#'   is `FALSE`.
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
#' grim(x = "5.19", n = 28)    # `x` in quotes!
#'
#' # However, it is consistent with a sample size of 32:
#' grim(x = "5.19", n = 32)
#'
#' # For a scale composed of two items:
#' grim(x = "2.84", n = 16, items = 2)
#'
#' # With percentages instead of means -- here, 71%:
#' grim(x = "71", n = 43, percent = TRUE)

# Vectorized version:
grim <- Vectorize(grim_scalar)
