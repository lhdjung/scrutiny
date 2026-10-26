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
#' @include reround.R
#'
#' @return Numeric. Length 1 by default, or the length of `x` with `mean =
#'   FALSE`.
#'
#' @export
#'
#' @examples
#' vec <- 0.01 |> seq(0.1, by = 0.01) |> round(2)
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

  # Not `rounding`, `threshold`, and `symmetric`: `reround()` requires them to
  # have length 1. The pairing warning is kept for `x` and `digits` here, though
  # not in `reround()`: a per-value `digits` is unusual when summarizing bias.
  check_lengths_congruent(list(x, digits))

  # A compound method returns two values per input, so the subtraction below
  # would recycle `x` across them. `%in%`, not `==`, so that a longer `rounding`
  # isn't recycled against this vector before the check can report it:
  if (
    any(
      rounding %in% c("up_or_down", "up_from_or_down_from", "ceiling_or_floor")
    )
  ) {
    abort_in_export(
      "`rounding` must be a single rounding procedure.",
      "x" = "It is {wrong_spec_string(rounding)}.",
      "i" = "The compound methods \"up_or_down\", \"up_from_or_down_from\", \\
      and \"ceiling_or_floor\" each return two rounded values per input \\
      value, so no single bias is defined for them."
    )
  }

  # Main part ---

  # Strings are fine because `digits` says where to round. A factor is read by
  # its labels, not its level codes.
  x <- as.numeric(as.character(x))

  bias <- reround(x, digits, rounding, threshold, symmetric) - x

  if (mean) {
    mean(bias)
  } else {
    bias
  }
}
