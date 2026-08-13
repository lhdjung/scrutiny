#' Rounding procedures named by their tie-breaking rule
#'
#' @description Each of these functions rounds to the nearest number with
#'   `digits` decimal places, and they differ only in what they do with a tie --
#'   a value exactly halfway between two such numbers:
#'   - `round_ties_up()` sends a tie to the higher number.
#'   - `round_ties_down()` sends a tie to the lower number.
#'   - `round_ties_away()` sends a tie away from zero.
#'   - `round_ties_zero()` sends a tie toward zero.
#'
#'   They are the four combinations of [`round_up()`] and [`round_down()`] with
#'   `symmetric`, under names that say which procedure they are. Use whichever
#'   spelling you prefer; the results are identical.
#'
#' @details For non-negative numbers, `round_ties_up()` and `round_ties_away()`
#'   are the same function, as are `round_ties_down()` and `round_ties_zero()`.
#'   The names only come apart below zero, which is exactly where the choice of
#'   procedure is easy to get wrong -- see the `Negative numbers` section of
#'   [`round_up()`].
#'
#'   `round_ties_away()` is IEEE 754's *roundTiesToAway*, and the procedure that
#'   Excel's `ROUND()`, SPSS's `RND()`, SAS's `ROUND()`, Matlab's `round()`, and
#'   `janitor::round_half_up()` all implement. `round_ties_up()` is what Stata's
#'   `round()` does. [`base::round()`] is *roundTiesToEven*, reached in scrutiny
#'   through `rounding = "even"`. `vignette("rounding-options")` maps each
#'   program to a setting.
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
#' @seealso [`round_up()`] for the same four procedures spelled as two functions
#'   plus a `symmetric` argument, and for the shared details on thresholds and
#'   floating-point tolerance. [`round_ceiling()`] for procedures that ignore
#'   which number is nearer.
#'
#' @examples
#' # The four procedures agree on everything except ties:
#' round_ties_up(x = 1.24, digits = 1)
#' round_ties_down(x = 1.24, digits = 1)
#'
#' # At a tie, they part company -- but only by sign below zero:
#' round_ties_up(x = c(-2.5, 2.5))
#' round_ties_down(x = c(-2.5, 2.5))
#' round_ties_away(x = c(-2.5, 2.5))    # Excel, SPSS, SAS, Matlab
#' round_ties_zero(x = c(-2.5, 2.5))
#'
#' # Each is a spelling of a `round_up()` or `round_down()` call:
#' round_ties_away(x = -2.5)
#' round_up(x = -2.5, symmetric = TRUE)

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
