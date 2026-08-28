#' Common rounding procedures
#'
#' @description `round_up()` rounds up from 5, `round_down()` rounds down from
#'   5. Otherwise, both functions work like [`base::round()`].
#'
#'   `round_up()` and `round_down()` are special cases of `round_up_from()` and
#'   `round_down_from()`, which allow users to choose custom thresholds for
#'   rounding up or down, respectively.
#'
#' @details These functions differ from [`base::round()`] mainly insofar as the
#'   decision about rounding 5 up or down is not based on the integer portion of
#'   `x` (i.e., no "rounding to even"). Instead, in `round_up_from()`, that
#'   decision is determined by the `threshold` argument for rounding up, and
#'   likewise with `round_down_from()`. The threshold is constant at `5` for
#'   `round_up()` and `round_down()`.
#'
#'   As a result, these functions are more predictable and less prone to
#'   floating-point number quirks than [`base::round()`]. Compare `round_down()`
#'   and [`base::round()`] in the data frame for rounding 5 created in the
#'   Examples section below: `round_down()` yields a continuous sequence of
#'   final digits from 0 to 9, whereas [`base::round()`] behaves in a way that
#'   can only be explained by floating point issues.
#'
#'   However, this surprising behavior on the part of [`base::round()`] is not
#'   necessarily a flaw (see its documentation, or this vignette:
#'   https://rpubs.com/maechler/Rounding). In the present version of R (4.0.0 or
#'   later), [`base::round()`] works fine, and the functions presented here are
#'   not meant to replace it. Their main purpose as helpers within scrutiny is
#'   to reconstruct the computations of researchers who might have used
#'   different software. See `vignette("rounding-options")`.
#'
#' @section Negative numbers: `symmetric` decides how ties in negative numbers
#'   are broken, and it decides nothing else: every other value has a single
#'   nearest neighbor, which all of these functions round to.
#'
#'   By default (`symmetric = FALSE`), `round_up()` moves a tie to the higher
#'   number on the number line, so `round_up(-2.5)` is `-2`, and `round_down()`
#'   moves it to the lower one, so `round_down(-2.5)` is `-3`. This is what
#'   Java's `Math.round()` does.
#'
#'   With `symmetric = TRUE`, a negative number is rounded like its absolute
#'   value, so `round_up(-2.5, symmetric = TRUE)` is `-3`. This is the setting
#'   that reconstructs Excel, SAS, SPSS, and Matlab, all of which move a tie
#'   away from zero, as does `janitor::round_half_up()`. In IEEE 754 terms, it
#'   is *roundTiesToAway*.
#'
#'   So `symmetric` matters whenever the data contain negative values, as with
#'   difference scores, z-scores, or effect sizes. See
#'   `vignette("rounding-options")` for which setting matches which program.
#'   (The package-wide default, `rounding = "up_or_down"`, spans the results of
#'   both settings, so `symmetric` cannot change a consistency verdict unless
#'   you commit to a single direction.)
#'
#' @section Floating-point tolerance: Shifting a number by `digits` decimal
#'   places is not exact: `0.145 * 100` is stored as `14.499999999999998`, just
#'   below the tie it is meant to be. Rounding that shifted value directly would
#'   move `0.145` a whole step in the wrong direction.
#'
#'   All of these functions therefore nudge the shifted value by about `1.5e-9`
#'   before rounding it, so that `round_up(0.145, 2)` is `0.15`, as it would be
#'   in the software whose output is being reconstructed. Values within roughly
#'   `1e-9` below a rounding boundary are thereby treated as sitting *on* it.
#'   [`unround()`] reports bounds that assume exactly this, which is why a
#'   number and its reconstructed range always agree.
#'
#'   The nudge is a fixed amount, whereas representation error grows with the
#'   magnitude of `x * 10^digits`. Up to about `1e7` for that product -- far
#'   beyond any reported mean, SD, or percentage -- the nudge dominates by
#'   orders of magnitude. Past it, a value sitting exactly on a boundary may go
#'   either way.
#'
#'   This differs from `janitor::round_half_up()`, which adds its tolerance to
#'   the value rather than to the boundary, and from [`base::round()`], which
#'   measures which neighbor is closer instead of nudging at all.
#'
#' @param x Numeric. The decimal number to round.
#' @param digits Integer. Number of digits to round `x` to. Default is `0`.
#'   Negative values round to powers of ten: `round_up(1250, digits = -2)` is
#'   `1300`, which reconstructs values reported as "rounded to the nearest
#'   hundred".
#' @param threshold Numeric. Only in `round_up_from()` and `round_down_from()`.
#'   The point within a step at which rounding switches direction, in tenths of
#'   a step, so it must be greater than `0` and less than `10`.
#'
#'   It means the same thing in both functions. `round_up_from()` rounds up when
#'   the part cut off by rounding is at least `threshold` tenths of a step, and
#'   `round_down_from()` rounds down when it is at most that many, so the two
#'   differ only in where they send a value sitting exactly on the threshold.
#'   `round_up()` and `round_down()` are the pair at the `threshold` of `5`.
#'
#'   Up to scrutiny 1.0.0, `round_down_from()` was the point reflection of
#'   `round_up_from()` instead, switching direction at `10 - threshold`. The two
#'   agreed at `5` and nowhere else, which made
#'   `rounding = "up_from_or_down_from"` span up to 1.8 steps -- more than any
#'   one rounding procedure can -- and so made consistency tests under it far
#'   too permissive.
#' @param symmetric Logical. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers should mirror that of positive numbers so that their
#'   absolute values are equal. Only affects ties, and only in negative numbers.
#'   Default is `FALSE`. See the `Negative numbers` section.
#'
#' @return Numeric. `x` rounded to `digits`.
#'
#' @export
#'
#' @name rounding-common
#'
#' @seealso [`round_ties_up()`] and friends are these same four procedures
#'   under names that say which is which, so that `symmetric` need not be
#'   spelled out separately.
#'
#'   [`round_ceiling()`] always rounds up, [`round_floor()`] always rounds down,
#'   [`round_trunc()`] always rounds toward 0, and [`round_anti_trunc()`] always
#'   rounds away from 0.
#'
#' @examples
#' # Both `round_up()` and `round_down()` work like
#' # `base::round()` unless the closest digit to be
#' # cut off by rounding is 5:
#'
#'    round_up(x = 9.273, digits = 1)     # 7 cut off
#'  round_down(x = 9.273, digits = 1)     # 7 cut off
#' base::round(x = 9.273, digits = 1)     # 7 cut off
#'
#'    round_up(x = 7.584, digits = 2)     # 4 cut off
#'  round_down(x = 7.584, digits = 2)     # 4 cut off
#' base::round(x = 7.584, digits = 2)     # 4 cut off
#'
#'
#' # Here is the borderline case of 5 rounded by
#' # `round_up()`, `round_down()`, and `base::round()`:
#'
#' original <- c(    # Define example values
#'   0.05, 0.15, 0.25, 0.35, 0.45,
#'   0.55, 0.65, 0.75, 0.85, 0.95
#' )
#' tibble::tibble(   # Output table
#'   original,
#'   round_up = round_up(x = original, digits = 1),
#'   round_down = round_down(x = original, digits = 1),
#'   base_round = base::round(x = original, digits = 1)
#' )
#'
#'
#' # Ties in negative numbers go up on the number line
#' # by default, and away from zero with `symmetric`:
#'
#' round_up(x = -2.5)                     # Java's `Math.round()`
#' round_up(x = -2.5, symmetric = TRUE)   # Excel, SAS, SPSS, Matlab
#'
#'
#' # A custom threshold moves the point at which
#' # rounding switches direction:
#'
#' round_up_from(x = 4.28, digits = 1, threshold = 9)   # 8 < 9, so down
#' round_up_from(x = 4.28, digits = 1, threshold = 1)   # 8 >= 1, so up

# Round up from some threshold -----------------------------------------------

#' @rdname rounding-common
#' @export

round_up_from <- function(x, digits = 0L, threshold, symmetric = FALSE) {
  # The same check that `reround()` and `rounding_offsets()` run. It belongs
  # here most of all: this is the function that acts on `threshold`, and a value
  # outside of `(0, 10)` silently turns it into `round_ceiling()` or
  # `round_floor()` rather than rounding from a threshold at all:
  check_threshold_valid(threshold)

  p10 <- 10^digits
  offset <- tie_offset_up(threshold)

  if (symmetric) {
    # For a non-negative `x`, `abs(x)` is `x`, so this is the same rounding;
    # for a negative one, it is the mirror image of the rounding of `-x`:
    restore_sign(floor(abs(x) * p10 + offset) / p10, x)
  } else {
    floor(x * p10 + offset) / p10
  }
}


# Round down from some threshold ---------------------------------------------

#' @rdname rounding-common
#' @export

# Note that this implementation is slightly different from the formula for
# rounding down in the "Rounding in detail" article. However, this is only for
# performance reasons, and the results are equivalent.

round_down_from <- function(x, digits = 0L, threshold, symmetric = FALSE) {
  # See the comment in `round_up_from()`:
  check_threshold_valid(threshold)

  p10 <- 10^digits
  offset <- tie_offset_down(threshold)

  if (symmetric) {
    # See the comment in `round_up_from()`:
    restore_sign(ceiling(abs(x) * p10 - offset) / p10, x)
  } else {
    ceiling(x * p10 - offset) / p10
  }
}


# Round up from 5 ------------------------------------------------------------

# We want `x`, the decimal number, to be rounded up if the part of the decimal
# portion to be cut off by rounding is 5 or greater. However, if that part is
# less than 5, `x` should instead be rounded down. The `threshold` for rounding
# up is therefore set to 5.

#' @rdname rounding-common
#' @export

round_up <- function(x, digits = 0L, symmetric = FALSE) {
  round_up_from(x = x, digits = digits, threshold = 5, symmetric = symmetric)
}


# Round down from 5 ----------------------------------------------------------

# Here, we want `x` to be rounded down if the part of the decimal portion to be
# cut off by rounding is 5 or less. However, if that part is greater than 5, `x`
# should instead be rounded up. The `threshold` for rounding down is therefore
# set to 5.
#
# `round_down_from()` reads `threshold` the same way `round_up_from()` does, so
# the two differ only in where they send a value sitting exactly on it. At the 5
# used here, that is the tie.

#' @rdname rounding-common
#' @export

round_down <- function(x, digits = 0L, symmetric = FALSE) {
  round_down_from(x = x, digits = digits, threshold = 5, symmetric = symmetric)
}
