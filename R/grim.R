
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
grim_scalar <- function(x, n, items = 1, percent = FALSE, show_rec = FALSE,
                        rounding = "up_or_down", threshold = 5,
                        symmetric = FALSE,
                        tolerance = .Machine$double.eps^0.5) {

  check_type(items, c("double", "integer"))
  check_type(percent, "logical")

  # As trailing zeros matter for the GRIM test, `x` must be a string:
  if (!is.character(x)) {
    cli::cli_abort(c(
      "!" = "`x` must be of type character.",
      "x" = "It is {an_a_type(x)}."
    ))
  }

  # Define key values from arguments:
  x_num <- as.numeric(x)
  digits <- decimal_places_scalar(x)

  # The `percent` argument allows for easy conversion of percentages to decimal
  # numbers:
  if (percent) {
    x_num <- x_num / 100
    digits <- digits + 2L
  }

  # Prepare further objects for reconstructing the original values:
  n_items <- n * items
  rec_sum <- x_num * n_items

  # Now, reconstruct the possible mean or percentage values (or "grains"),
  # controlling for small differences introduced by spurious precision:
  rec_x_upper <- dustify(ceiling(rec_sum) / n_items)
  rec_x_lower <- dustify(floor(rec_sum) / n_items)

  # Round these "grains" using an internal helper function that also gets the
  # number of decimal places as well as the `rounding`, `threshold`, and
  # `symmetric` arguments passed down to:
  grains_rounded <- reround(
    x         = c(rec_x_upper, rec_x_lower),
    digits    = digits,
    rounding  = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # Test if the reported mean or percentage is near-identical to either of the
  # two possible reconstructed values (the "grains"). The default for the
  # `tolerance` argument -- tolerance of comparison between the reported and
  # reconstructed values -- that comes into play here is the same as in
  # `dplyr::near()` itself, i.e., circa 0.000000015:
  grain_is_x <- any(dplyr::near(grains_rounded, x_num, tol = tolerance))

  if (!show_rec) {
    # Check if any of these two comparisons returned `TRUE`:
    return(grain_is_x)
  } else {
    consistency <- grain_is_x
    length_2ers <- c("up_or_down", "up_from_or_down_from", "ceiling_or_floor")
    if (any(length_2ers == rounding)) {
      # Skipping those values that are identical to the selected ones apart from
      # `dust` addition or subtraction via `dustify()`:
      return(list(
        consistency, rec_sum, rec_x_upper, rec_x_lower,
        grains_rounded[1L], grains_rounded[2L],
        grains_rounded[5L], grains_rounded[6L]
      ))
    } else {
      # Skipping as above:
      return(list(
        consistency, rec_sum, rec_x_upper, rec_x_lower,
        grains_rounded[1L], grains_rounded[3L]
      ))
    }
  }

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

