#' Rounding in one direction
#'
#' @description These functions ignore which neighbor is nearer:
#'   - `round_ceiling()` always rounds up.
#'   - `round_floor()` always rounds down.
#'   - `round_trunc()` always rounds toward zero.
#'   - `round_anti_trunc()` always rounds away from zero. A value that already
#'   sits on the rounding grid stays where it is, so `0` stays `0`. This is
#'   what Excel's `ROUNDUP()`, Java's `RoundingMode.UP`, and Python's
#'   `decimal.ROUND_UP` do.
#'   - `anti_trunc()` is the counterpart of [`trunc()`]: it rounds a number to
#'   the nearest whole number away from zero. Whole numbers stay unchanged.
#'
#' @details `round_ceiling()`, `round_floor()`, and `round_trunc()` generalize
#'   [`ceiling()`], [`floor()`], and [`trunc()`] to any number of decimal
#'   places, and `round_anti_trunc()` generalizes `anti_trunc()`. With the
#'   default `digits = 0`, each equals its base counterpart.
#'
#'   `round_trunc()` is `round_floor()` for positive and `round_ceiling()` for
#'   negative numbers; `round_anti_trunc()` is the reverse.
#'
#'   All of these apply the floating-point tolerance described in
#'   [`round_up()`], so that `round_ceiling(0.28, 2)` is `0.28` although
#'   `0.28 * 100` is stored as `28.000000000000004`.
#'
#' @param x Numeric. The number to round.
#' @param digits Integer. Number of decimal places to round to. Default is
#'   `0`. Negative values round to powers of ten.
#'
#' @return Numeric. `x` rounded to `digits` (`anti_trunc()` has no `digits`
#'   argument).
#'
#' @export
#'
#' @name rounding-uncommon
#'
#' @seealso [`round_up()`] and [`round_down()`] round to the nearest number
#'   and decide ties by a fixed direction.
#'
#' @examples
#' round_ceiling(x = 4.52, digits = 1)
#' round_floor(x = 4.67, digits = 1)
#'
#' round_trunc(8.439, digits = 2)
#' round_trunc(-8.439, digits = 2)
#'
#' round_anti_trunc(x = 8.421, digits = 2)
#' round_anti_trunc(x = -8.421, digits = 2)

# The functions below nudge the shifted value by `ROUNDING_TOLERANCE` (see
# utils.R) before rounding it, so that floating-point representation error
# cannot move a number a whole step.

# Always round up ------------------------------------------------------------

#' @rdname rounding-uncommon
#' @export

round_ceiling <- function(x, digits = 0L) {
  p10 <- 10^digits
  ceiling(x * p10 - ROUNDING_TOLERANCE) / p10
}


# Always round down ----------------------------------------------------------

#' @rdname rounding-uncommon
#' @export

round_floor <- function(x, digits = 0L) {
  p10 <- 10^digits
  floor(x * p10 + ROUNDING_TOLERANCE) / p10
}


# Always round toward zero ---------------------------------------------------

#' @rdname rounding-uncommon
#' @export

round_trunc <- function(x, digits = 0L) {
  p10 <- 10^digits

  # For symmetry between positive and negative numbers, use the absolute value.
  # Truncation rounds toward zero, so the tolerance is added, just as in
  # `round_floor()`:
  core <- trunc(abs(x) * p10 + ROUNDING_TOLERANCE) / p10

  # If `x` is negative, its truncated version should be negative or zero:
  restore_sign(core, x)
}


# Interlude: "anti-truncate" a number ----------------------------------------

#' @rdname rounding-uncommon
#' @export

anti_trunc <- function(x) {
  # For symmetry between positive and negative numbers, use the absolute value.
  # The tolerance is subtracted, just as in `round_ceiling()`: an `x` which is
  # only just above a whole number by representation error should still count as
  # that number rather than be taken a whole step further out.
  core <- ceiling(abs(x) - ROUNDING_TOLERANCE)

  # Up to scrutiny 1.0.0 this was `trunc(abs(x)) + 1`, which moves a value one
  # step away from zero even when it already sits on a whole number, so
  # `anti_trunc(3)` was 4 and `anti_trunc(0)` was 1. (A comment here claimed the
  # two formulas were equivalent; they agree everywhere except on whole numbers,
  # which is precisely where the choice lies.) No software rounds that way.
  # Excel's and Google Sheets' `ROUNDUP()`, Java's `RoundingMode.UP`, and
  # Python's `decimal.ROUND_UP` all round away from zero in the weaker sense
  # implemented here, where a value on the rounding grid stays put and zero
  # stays zero. `rounding_offsets()` encodes the same reading.

  # If `x` is negative, its "anti-truncated" version should also be negative:
  restore_sign(core, x)
}


# Always round away from zero ------------------------------------------------

#' @rdname rounding-uncommon
#' @export

round_anti_trunc <- function(x, digits = 0L) {
  p10 <- 10^digits
  anti_trunc(x * p10) / p10
}
