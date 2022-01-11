
# Single-case function; not exported but used as a basis for the vectorized
# `grim()` as well as within `grim_map()`:
grim_scalar <- function(x, n, items = 1, percent = FALSE, show_rec = FALSE,
                        rounding = "up_or_down", threshold = 5,
                        symmetric = FALSE,
                        tolerance = .Machine$double.eps^0.5) {

  check_rounding_singular(rounding)

  # Provide some guidance in case users confuse `grim()` with `grim_map()`:
  if (is.data.frame(x)) {
    cli::cli_abort(c(
      "`x` is a data frame",
      "x" = "For `grim()`, please provide a single set of values, not a \\
      data frame (see documentation). If you want to GRIM-test multiple \\
      value sets at once, call `grim_map()` instead."
    ))
  }

  # As trailing zeros matter for the GRIM test, `x` needs to be a string:
  if (!is.character(x)) {
    cli::cli_abort(c(
      "`x` is {an_a_type(x)}.",
      "i" = "It needs to be a string."
    ))
  }

  # Define key values from arguments:
  x_num <- as.numeric(x)
  digits <- decimal_places_scalar(x)

  # The `percent` argument allows for easy conversion of percentages to decimal
  # numbers:
  if (percent) {
    x_num <- x_num / 100
    digits <- digits + 2
  }

  # Prepare further objects for reconstructing the original values:
  n_items <- n * items
  rec_sum <- x_num * n_items

  # Now, reconstruct the two possible mean or percentage values (or "grains"):
  rec_x_upper <- ceiling(rec_sum) / n_items
  rec_x_lower <- floor(rec_sum) / n_items

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
  grain_is_x <- dplyr::near(grains_rounded, x_num, tol = tolerance)

  if (!show_rec) {
    # Check if any of these two comparisons returned `TRUE`:
    any(grain_is_x)
  } else {
    consistency <- any(grain_is_x)
    length_2ers <- c("up_or_down", "up_from_or_down_from", "ceiling_or_floor")

    if (any(rounding == length_2ers)) {

      list(consistency, rec_sum, rec_x_upper, rec_x_lower,
           grains_rounded[1], grains_rounded[2],
           grains_rounded[3], grains_rounded[4])

    } else {

      list(consistency, rec_sum, rec_x_upper, rec_x_lower,
           grains_rounded[1], grains_rounded[2])
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
#'   The function is vectorized, but it is recommended to use `grim_map()` for
#'   testing multiple cases.
#'
#' @details The `x` values need to be strings because only strings retain
#'   trailing zeros, which are as important for the GRIM test as any other
#'   decimal digits.
#'
#'   Use `restore_zeros()` on numeric values (or values that were numeric values
#'   at some point) to easily supply the trailing zeros they might once have
#'   had. See documentation there.
#'
#'   Browse the source code in the grim.R file. `grim()` is a vectorized version
#'   of the internal `grim_scalar()` function found there.
#'
#'   The GRIM test (granularity-related inconsistency of means) was devised by
#'   ---REFERENCE TO BROWN AND HEATHERS 2017---.
#'
#' @section Rounding: Here are the options for the `rounding` argument.
#'   Reconstructed mean or percentage values can be rounded in either of these
#'   ways:

#'   - Rounded to `"even"` using base R's own `round()`.
#'   - Rounded `"up"` or `"down"` from 5. (Note that SAS, SPSS, Stata, Matlab,
#'   and Excel round `"up"` from 5, whereas Python rounds `"down"` from 5.)
#'   - Rounded `"up_from"` or `"down_from"` some number, which then needs to be
#'   specified via the `threshold` argument.
#'   - Given a `"ceiling"` or `"floor"` at the respective decimal place.
#'   - Rounded towards zero with `"trunc"` or away from zero with
#'   `"anti_trunc"`.

#' The default, `"up_or_down"`, allows for numbers rounded either `"up"` or
#' `"down"` from 5 when GRIM-testing; and likewise for `"up_from_or_down_from"`
#' and `"ceiling_or_floor"`.
#'
#' With `rounding = "up_or_down"`, if `n` is 40 or 80 and `x` has two decimal
#' places, very few values will test as inconsistent; but note that many will be
#' with either of `rounding = "up"` and `rounding = "down"`, or indeed with any
#' other rounding method. This is part of a more general pattern: `n` is 400 or
#' 800 and `x` has three decimal places, etc.
#'
#' For more information about these methods, see documentation for `round()`,
#' `round_up()`, and `round_ceiling()`. These include all of the above ways of
#' rounding. How the reconstructed values are rounded can also be calibrated by
#' the `threshold` and `symmetric` arguments.
#'
#' @param x String. The reported mean or percentage value.
#' @param n Integer. The reported sample size.
#' @param items Numeric. The number of items composing `x`. Default is 1, the
#'   most common case.
#' @param percent Boolean. Set `percent` to `TRUE` if `x` is a percentage. This
#'   will convert it to a decimal number and adjust the decimal count (i.e.,
#'   increase it by 2). Default is `FALSE`.
#' @param show_rec Boolean. If set to `TRUE`, the output is a matrix that also
#'   contains intermediary values from GRIM-testing. This is not recommended;
#'   instead, use `show_rec` in `grim_map()`. Default is `FALSE`.
#' @param rounding String. Rounding method or methods to be used for
#'   reconstructing the values to which `x` will be compared. Default is
#'   `"up_or_down"` (from 5). For more options, see Details.
#' @param threshold Numeric. If `rounding` is set to `"up_from"`, `"down_from"`,
#'   or `"up_from_or_down_from"`, set `threshold` to the number from which the
#'   reconstructed values should then be rounded up or down. Otherwise, this
#'   argument plays no role. Default is `5`.
#' @param symmetric Boolean. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers with `"up"`, `"down"`, `"up_from"`, or `"down_from"`
#'   should mirror that of positive numbers so that their absolute values are
#'   always equal. Default is `FALSE`.
#' @param tolerance Numeric. Tolerance of comparison between `x` and the
#'   possible mean or percentage values. Default is circa 0.000000015
#'   (1.490116e-08), as in `dplyr::near()`.
#'
#' @return Boolean. `TRUE` if `x`, `n`, and `items` are mutually consistent,
#'   `FALSE` if not.
#'
#' @seealso `grim_map()` applies `grim()` to any number of cases at once.
#'
#' @include utils.R decimal-places.R reround.R
#'
#' @importFrom magrittr %>%
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
#'
#' @rdname grim
#' @export grim



# Vectorized version:
grim <- Vectorize(grim_scalar)


