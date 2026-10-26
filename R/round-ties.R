#' Rounding procedures named by their tie-breaking rule
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Each of these functions rounds to the nearest number with
#'   `digits` decimal places. They differ only at a tie, a value exactly
#'   halfway between two such numbers:
#'   - `round_ties_up()` sends a tie to the higher number.
#'   - `round_ties_down()` sends a tie to the lower number.
#'   - `round_ties_away()` sends a tie away from zero.
#'   - `round_ties_zero()` sends a tie toward zero.
#'   - `round_ties_even()` sends a tie to the even neighbor.
#'
#'   The first four are [`round_up()`] and [`round_down()`] with and without
#'   `symmetric = TRUE`, under names that state the procedure. They take no
#'   `symmetric` argument, since the name already fixes the direction on both
#'   sides of zero. `round_ties_even()` is [`base::round()`].
#'
#'   The matching `rounding` strings, `"ties_up"` and so on, are accepted
#'   wherever `rounding` is, and giving `symmetric` alongside one of them is
#'   an error.
#'
#' @details `round_ties_away()` is IEEE 754's *roundTiesToAway*, which Excel's
#'   `ROUND()`, SPSS's `RND()`, SAS's `ROUND()`, and Matlab's `round()`
#'   implement. `round_ties_up()` is what Stata's `round()` does.
#'   `round_ties_even()` is *roundTiesToEven*, the standard's default and what
#'   R, Python, and NumPy do. See `vignette("rounding-options")`.
#'
#'   "Up" and "down" mean up and down on the number line, not the
#'   away-from-zero sense that "round up" carries in Excel, Java, and Python.
#'   For rounding that is always away from zero or toward it, see
#'   [`round_anti_trunc()`] and [`round_trunc()`].
#'
#'   Unlike the other four, `round_ties_even()` cannot be predicted from the
#'   printed decimal: `base::round()` decides on the binary double, and most
#'   decimal fractions are not exact ties in binary. Consistency tests treat
#'   both bounds under `rounding = "even"` as inclusive for that reason; see
#'   [`unround()`].
#'
#' @param x Numeric. The number to round.
#' @param digits Integer. Number of decimal places to round to. Default is
#'   `0`.
#'
#' @return Numeric. `x` rounded to `digits`.
#'
#' @include round.R
#'
#' @export
#'
#' @name rounding-ties
#'
#' @seealso [`round_up()`] for the same procedures spelled with `symmetric`,
#'   and for the shared details on thresholds and floating-point tolerance.
#'
#' @examples
#' # All five agree except at ties:
#' round_ties_up(x = 1.24, digits = 1)
#' round_ties_down(x = 1.24, digits = 1)
#'
#' # At a tie, they differ, but only by sign below zero:
#' round_ties_up(x = c(-2.5, 2.5))
#' round_ties_down(x = c(-2.5, 2.5))
#' round_ties_away(x = c(-2.5, 2.5))    # Excel, SPSS, SAS, Matlab
#' round_ties_zero(x = c(-2.5, 2.5))
#'
#' # Each of the four is a spelling of `round_up()` or `round_down()`:
#' round_ties_away(x = -2.5)
#' round_up(x = -2.5, symmetric = TRUE)
#'
#' # `round_ties_even()` is `base::round()`:
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

# Parity is not a direction, so no threshold and `symmetric` produce this. It
# wraps `base::round()` only to complete the family.

#' @rdname rounding-ties
#' @export

round_ties_even <- function(x, digits = 0L) {
  round(x = x, digits = digits)
}
