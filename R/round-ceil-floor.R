#' Uncommon rounding procedures
#'
#' @description Always round up, down, toward zero, or away from it:
#'   - `round_ceiling()` always rounds up.
#'   - `round_floor()` always rounds down.
#'   - `round_trunc()` always rounds toward zero.
#'   - `round_anti_trunc()` always rounds away from zero. A value that already
#'   sits on the rounding grid stays where it is, and `0` stays `0`. This is
#'   what Excel's and Google Sheets' `ROUNDUP()`, Java's `RoundingMode.UP`, and
#'   Python's `decimal.ROUND_UP` do.
#'   - `anti_trunc()` does not round but otherwise works like
#'   `round_anti_trunc()`.
#'
#'   Despite not being widely used, they are featured here in case they are
#'   needed for reconstruction.

#' @details `round_ceiling()`, `round_floor()`, and `round_trunc()` generalize
#'   the base R functions [`ceiling()`], [`floor()`], and [`trunc()`], and
#'   include them as special cases: With the default value for `digits`, 0,
#'   these `round_*` functions are equivalent to their respective base
#'   counterparts.
#'
#'   The last `round_*` function, `round_anti_trunc()`, generalizes another
#'   function presented here: `anti_trunc()` works like `trunc()` except it
#'   moves away from 0, rather than towards it. That is, whereas `trunc()`
#'   minimizes the absolute value of `x` (as compared to the other rounding
#'   functions), `anti_trunc()` maximizes it. `anti_trunc(x)` is therefore equal
#'   to `ceiling(x)` if `x` is positive, and to `floor(x)` if `x` is negative.
#'
#'   `round_anti_trunc()`, then, generalizes `anti_trunc()` just as
#'   `round_ceiling()` generalizes [`ceiling()`], etc.
#'
#'   Moreover, `round_trunc()` is equivalent to `round_floor()` for positive
#'   numbers and to `round_ceiling()` for negative numbers. The reverse is again
#'   true for `round_anti_trunc()`: It is equivalent to `round_ceiling()` for
#'   positive numbers and to `round_floor()` for negative numbers. The two of
#'   them partition every value between them, with `0` going to both.
#'
#'   Like [`round_up()`] and the other functions on that page, all of these
#'   nudge the value by about `1.5e-9` before rounding it, so that
#'   floating-point representation error cannot move a number a whole step:
#'   `0.28 * 100` is stored as `28.000000000000004`, and `round_ceiling(0.28,
#'   2)` is `0.28` rather than `0.29` because of the nudge. See the
#'   `Floating-point tolerance` section of [`round_up()`] for the details and
#'   for the range of magnitudes in which it holds.
#'
#' @param x Numeric. The decimal number to round.
#' @param digits Integer. Number of digits to round `x` to. Default is `0`.
#'   Negative values round to powers of ten: `round_ceiling(1250, digits = -2)`
#'   is `1300`.
#'
#' @return Numeric. `x` rounded to `digits` (except for `anti_trunc()`, which
#'   has no `digits` argument).
#'
#' @export
#'
#' @name rounding-uncommon

# @aliases round_ceiling
# @aliases round_floor
# @aliases round_trunc

#' @seealso [`round_up()`] and [`round_down()`] round up or down from 5,
#'   respectively. [`round_up_from()`] and [`round_down_from()`] allow users to
#'   specify custom thresholds for rounding up or down.
#'
#' @examples
#' # Always round up:
#' round_ceiling(x = 4.52, digits = 1)        # 2 cut off
#'
#' # Always round down:
#' round_floor(x = 4.67, digits = 1)          # 7 cut off
#'
#' # Always round toward 0:
#' round_trunc(8.439, digits = 2)             # 9 cut off
#' round_trunc(-8.439, digits = 2)            # 9 cut off
#'
#' # Always round away from 0:
#' round_anti_trunc(x = 8.421, digits = 2)    # 1 cut off
#' round_anti_trunc(x = -8.421, digits = 2)   # 1 cut off

# The functions below nudge the shifted value by `rounding_tolerance` (see
# utils.R) before rounding it, so that floating-point representation error
# cannot move a number a whole step.

# Always round up ------------------------------------------------------------

#' @rdname rounding-uncommon
#' @export

round_ceiling <- function(x, digits = 0L) {
  p10 <- 10^digits
  ceiling(x * p10 - rounding_tolerance) / p10
}


# Always round down ----------------------------------------------------------

#' @rdname rounding-uncommon
#' @export

round_floor <- function(x, digits = 0L) {
  p10 <- 10^digits
  floor(x * p10 + rounding_tolerance) / p10
}


# Always round toward zero ---------------------------------------------------

#' @rdname rounding-uncommon
#' @export

round_trunc <- function(x, digits = 0L) {
  p10 <- 10^digits

  # For symmetry between positive and negative numbers, use the absolute value.
  # Truncation rounds toward zero, so the tolerance is added, just as in
  # `round_floor()`:
  core <- trunc(abs(x) * p10 + rounding_tolerance) / p10

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
  core <- ceiling(abs(x) - rounding_tolerance)

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
