

#' Compute rounding bias
#'
#' @description Rounding often leads to bias, such that the mean of a rounded
#'   distribution is different from the mean of the original distribution. Call
#'   `rounding_bias()` to compute the amount of this bias.
#'
#' @details Bias is calculated by subtracting the original vector, `x`, from a
#'   vector rounded in the specified way.
#'
#'   The function passes all arguments except for `mean` down to `reround()`.
#'   Other than there, however, `rounding` is `"up"` by default, and it can't be
#'   set to `"up_or_down"`, `"up_from_or_down_from"`, or`"ceiling_or_floor"`.
#'
#' @param x Numeric or string coercible to numeric.
#' @param digits Integer. Number of decimal digits to which `x` will be rounded.
#' @param rounding String. Rounding procedure that will be applied to `x`. See
#'   documentation for `grim()`, section `Rounding`. Default is `"up"`.
#' @param threshold,symmetric Further arguments passed down to `reround()`.
#' @param mean Boolean. If `TRUE` (the default), the mean total of bias will be
#'   returned. Set `mean` to `FALSE` to get a vector of individual biases the
#'   length of `x`.
#'
#' @include reround.R seq-decimal.R
#'
#' @return Numeric. By default of `mean`, the length is 1; otherwise, it is the
#'   same length as `x`.
#'
#' @export
#'
#' @examples
#' # Define example vector:
#' vec <- seq_distance(0.01, string_output = FALSE)
#' vec
#'
#' # The default rounds `x` up from 5:
#' rounding_bias(x = vec, digits = 1)
#'
#' # Other rounding procedures are supported,
#' # such as rounding down from 5...
#' rounding_bias(x = vec, digits = 1, rounding = "down")
#'
#' # ...or rounding to even with `base::round()`:
#' rounding_bias(x = vec, digits = 1, rounding = "even")


rounding_bias <- function(x, digits, rounding = "up", threshold = 5,
                          symmetric = FALSE, mean = TRUE) {

  # Checks ---

  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(
    x, digits, rounding, threshold, symmetric
))


  # Main part ---

  bias <- reround(x, digits, rounding, threshold, symmetric) - x

  if (mean) {
    mean(bias)
  } else {
    bias
  }

}



# # Proof that it works (this is for a previous version that didn't compute the
# # mean but only subtracted `x` from `x_rounded`) --
#
# # Define example vector:
# x <- seq_distance(0.01, string_output = FALSE)
# decimals <- 1
# rounding <- "up"
# threshold <- 5
# # These are all `TRUE`, so `x` can be reconstructed from first rounding it in
# # the specified way, then subtracting the bias:
# dplyr::near(
#   (reround(x, digits, rounding, threshold) - rounding_bias(x, 1)), x
# )


