#' Rounding procedures named by their tie-breaking rule
#'
#' @description Each of these functions rounds to the nearest number with
#'   `digits` decimal places, and they differ only in what they do with a tie --
#'   a value exactly halfway between two such numbers:
#'   - `round_ties_up()` sends a tie to the higher number.
#'   - `round_ties_down()` sends a tie to the lower number.
#'   - `round_ties_away()` sends a tie away from zero.
#'   - `round_ties_zero()` sends a tie toward zero.
#'   - `round_ties_even()` sends a tie to whichever of the two is even.
#'
#'   The first four are the combinations of [`round_up()`] and [`round_down()`]
#'   with `symmetric`, under names that say which procedure they are. Use
#'   whichever spelling you prefer; the results are identical.
#'   `round_ties_even()` is [`base::round()`], which has no such second
#'   spelling; it is here so that the family is complete.
#'
#'   These functions take no `symmetric` argument, because each name already
#'   fixes the tie direction on both sides of zero. The matching `rounding`
#'   strings -- `"ties_up"` and the rest, which [`reround()`], [`unround()`],
#'   [`grim()`], [`grimmer()`], and [`debit()`] all accept -- work the same way,
#'   and giving `symmetric` alongside one of them is an error rather than a
#'   silent no-op.
#'
#' @details For non-negative numbers, `round_ties_up()` and `round_ties_away()`
#'   are the same function, as are `round_ties_down()` and `round_ties_zero()`.
#'   The pairs only differ for numbers below zero. See the
#'   `Negative numbers` section of [`round_up()`].
#'
#'   `round_ties_away()` is IEEE 754's *roundTiesToAway*, and the procedure that
#'   Excel's `ROUND()`, SPSS's `RND()`, SAS's `ROUND()`, Matlab's `round()`, and
#'   `janitor::round_half_up()` all implement. `round_ties_up()` is what Stata's
#'   `round()` does. `round_ties_even()` is *roundTiesToEven*, the standard's
#'   default direction and the one R, Python, and NumPy take; it is
#'   [`base::round()`], reached in scrutiny through `rounding = "even"` or
#'   `"ties_even"`. `vignette("rounding-options")` maps each program to a
#'   setting.
#'
#'   Unlike the other four, `round_ties_even()` cannot be predicted from the
#'   decimal you see: `base::round()` breaks a tie by the parity of the binary
#'   double, and most decimal fractions are not exactly representable, so a
#'   number that looks like a tie usually is not one. Consistency tests handle
#'   that by treating both bounds of `rounding = "even"` as inclusive rather
#'   than guessing; see the `Rounding` section of [`unround()`].
#'
#'   Note that "up" and "down" here mean up and down *on the number line*, not
#'   the away-from-zero sense that "round up" carries in Excel and in Java's and
#'   Python's decimal vocabularies. For rounding that is always away from zero
#'   or always toward it, regardless of ties, see [`round_anti_trunc()`] and
#'   [`round_trunc()`].
#'
#' @param x Numeric. The decimal number to round.
#' @param digits Integer. Number of digits to round `x` to. Default is `0`.
#'
#' @return Numeric. `x` rounded to `digits`.
#'
#' @include round.R
#'
#' @export
#'
#' @name rounding-ties
#'
#' @seealso [`round_up()`] for the first four procedures spelled as two
#'   functions plus a `symmetric` argument, and for the shared details on
#'   thresholds and floating-point tolerance. [`round_ceiling()`] for procedures
#'   that ignore which number is nearer.
#'
#' @examples
#' # All five agree on everything except ties:
#' round_ties_up(x = 1.24, digits = 1)
#' round_ties_down(x = 1.24, digits = 1)
#'
#' # At a tie, they part company -- but only by sign below zero:
#' round_ties_up(x = c(-2.5, 2.5))
#' round_ties_down(x = c(-2.5, 2.5))
#' round_ties_away(x = c(-2.5, 2.5))    # Excel, SPSS, SAS, Matlab
#' round_ties_zero(x = c(-2.5, 2.5))
#'
#' # Each of the four is a spelling of a `round_up()`
#' # or `round_down()` call:
#' round_ties_away(x = -2.5)
#' round_up(x = -2.5, symmetric = TRUE)
#'
#' # `round_ties_even()` is `base::round()`, so it goes
#' # by the parity of the binary double, not of the
#' # decimal shown here:
#' round_ties_even(x = c(0.5, 1.5, 2.5))

# Ties to the higher number --------------------------------------------------

#' @rdname rounding-ties
#' @export

round_ties_up <- function(x, digits = 0L) {
  round_up_from(x = x, digits = digits, threshold = 5, symmetric = FALSE)
}


# Ties to the lower number ---------------------------------------------------

#' @rdname rounding-ties
#' @export

round_ties_down <- function(x, digits = 0L) {
  round_down_from(x = x, digits = digits, threshold = 5, symmetric = FALSE)
}


# Ties away from zero --------------------------------------------------------

#' @rdname rounding-ties
#' @export

round_ties_away <- function(x, digits = 0L) {
  round_up_from(x = x, digits = digits, threshold = 5, symmetric = TRUE)
}


# Ties toward zero -----------------------------------------------------------

#' @rdname rounding-ties
#' @export

round_ties_zero <- function(x, digits = 0L) {
  round_down_from(x = x, digits = digits, threshold = 5, symmetric = TRUE)
}


# Ties to the even number ----------------------------------------------------

# This one has no `round_up()`/`round_down()` spelling: parity is not a
# direction, so no combination of a threshold and `symmetric` produces it. It is
# `base::round()` itself, wrapped only so that the family can be called by one
# set of names.

#' @rdname rounding-ties
#' @export

round_ties_even <- function(x, digits = 0L) {
  round(x = x, digits = digits)
}
