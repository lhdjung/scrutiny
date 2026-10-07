#' Compute rounding bias
#'
#' @description Rounding changes the mean of a distribution. `rounding_bias()`
#'   computes this bias as the mean of the rounded values minus the mean of the
#'   original ones, or, with `mean = FALSE`, the individual differences.
#'
#' @details All arguments except `mean` are passed on to [`reround()`]. Unlike
#'   there, `rounding` defaults to `"up"` and cannot be a compound method
#'   (`"up_or_down"`, `"up_from_or_down_from"`, `"ceiling_or_floor"`), since
#'   those return two rounded values per input and have no single bias.
#'
#' @param x Numeric, or a string coercible to numeric.
#' @param digits Integer. Number of decimal places to round to.
#' @param rounding String. The rounding method. Default is `"up"`. See
#'   `vignette("rounding-options")`.
#' @param threshold,symmetric Passed on to [`reround()`].
#' @param mean Logical. If `TRUE` (the default), return the mean bias;
#'   otherwise, the individual biases.
#'
#' @include reround.R seq-decimal.R
#'
#' @return Numeric. Length 1 by default, or the length of `x` with `mean =
#'   FALSE`.
#'
#' @export
#'
#' @examples
#' vec <- seq_distance(0.01, string_output = FALSE)
#' vec
#'
#' rounding_bias(x = vec, digits = 1)
#' rounding_bias(x = vec, digits = 1, rounding = "down")
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

  # A string only has to stand for its number: `digits` says where to round, so
  # nothing is read off the string itself. Unconverted, it failed in the
  # subtraction below. A factor is read by its labels, not its level codes.
  x <- as.numeric(as.character(x))

  bias <- reround(x, digits, rounding, threshold, symmetric) - x

  if (mean) {
    mean(bias)
  } else {
    bias
  }
}
