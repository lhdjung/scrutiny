#' Compute rounding bias
#'
#' @description Rounding often leads to bias, such that the mean of a rounded
#'   distribution is different from the mean of the original distribution. Call
#'   `rounding_bias()` to compute the amount of this bias.
#'
#' @details Bias is calculated by subtracting the original vector, `x`, from a
#'   vector rounded in the specified way.
#'
#'   The function passes all arguments except for `mean` down to [`reround()`].
#'   Other than there, however, `rounding` is `"up"` by default, and it can't be
#'   set to `"up_or_down"`, `"up_from_or_down_from"`, or `"ceiling_or_floor"`:
#'   these compound methods return two rounded values per input value, so no
#'   single bias is defined for them.
#'
#' @param x Numeric or string coercible to numeric.
#' @param digits Integer. Number of decimal digits to which `x` will be rounded.
#' @param rounding String. Rounding procedure that will be applied to `x`. See
#'   `vignette("rounding-options")`. Default is `"up"`.
#' @param threshold,symmetric Further arguments passed down to [`reround()`].
#' @param mean Logical. If `TRUE` (the default), the mean total of bias will be
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

rounding_bias <- function(
  x,
  digits,
  rounding = "up",
  threshold = 5,
  symmetric = FALSE,
  mean = TRUE
) {
  # Checks ---

  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(x, digits))

  # `rounding`, `threshold`, and `symmetric` are deliberately left out. They
  # describe one rounding procedure, and `reround()` requires each of them to
  # have length 1, so including them here produced a warning about values
  # "getting paired" for arguments that cannot be paired at all -- immediately
  # followed by an error from `reround()` saying so.
  #
  # The pairing warning for `x` and `digits` is kept here, and deliberately not
  # in `reround()` itself: this is a user-facing function whose whole purpose is
  # to summarize bias over a vector, so a per-value `digits` is worth a second
  # look, whereas `reround()` is the interface every helper in the package calls
  # with exactly that shape.

  # A compound method makes `reround()` return two values per input value, so
  # the subtraction below would recycle `x` across them and return twice as many
  # "biases" as there are inputs, all of them meaningless.
  # `%in%` rather than `==`: the latter compares element by element, so a
  # `rounding` of length 2 was recycled against this length-3 vector and drew
  # R's "longer object length is not a multiple" warning before the check below
  # could say anything about it.
  if (
    any(
      rounding %in% c("up_or_down", "up_from_or_down_from", "ceiling_or_floor")
    )
  ) {
    cli::cli_abort(c(
      "`rounding` must be a single rounding procedure.",
      "x" = "It is {wrong_spec_string(rounding)}.",
      "i" = "The compound methods \"up_or_down\", \"up_from_or_down_from\", \\
      and \"ceiling_or_floor\" each return two rounded values per input \\
      value, so no single bias is defined for them."
    ))
  }

  # Main part ---

  bias <- reround(x, digits, rounding, threshold, symmetric) - x

  if (mean) {
    mean(bias)
  } else {
    bias
  }
}

