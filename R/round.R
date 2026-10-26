#' Common rounding procedures
#'
#' @description `round_up()` rounds ties upward, and `round_down()` rounds
#'   them downward. Otherwise, both agree with [`base::round()`].
#'
#'   They are special cases of `round_up_from()` and `round_down_from()`, which
#'   switch direction at a `threshold` other than 5.
#'
#' @details These functions decide a tie by a fixed rule rather than by the
#'   parity of the preceding digit, as [`base::round()`] does. They exist to
#'   reconstruct the output of software that rounds this way; see
#'   `vignette("rounding-options")` for which program uses which procedure.
#'   They are not meant to replace `base::round()` in original computations.
#'
#' @section Negative numbers: By default (`symmetric = FALSE`), `round_up()`
#'   moves a tie to the higher number on the number line, so `round_up(-2.5)`
#'   is `-2`, and `round_down(-2.5)` is `-3`. This is what Stata does.
#'
#'   With `symmetric = TRUE`, a negative number is rounded like its absolute
#'   value, so a tie moves away from zero: `round_up(-2.5, symmetric = TRUE)`
#'   is `-3`. This is what Excel, SAS, SPSS, and Matlab do; IEEE 754 calls it
#'   *roundTiesToAway*.
#'
#'   For `round_up()` and `round_down()`, `symmetric` only affects ties among
#'   negative numbers. For `round_up_from()` and `round_down_from()` with a
#'   `threshold` other than 5, it affects every negative number whose cut-off
#'   part lies between `threshold` and `10 - threshold`:
#'   `round_up_from(-4.28, 1, threshold = 9)` is `-4.3`, and `-4.2` with
#'   `symmetric = TRUE`.
#'
#'   Under the package-wide default `rounding = "up_or_down"`, both results
#'   are accepted, so `symmetric` cannot change a consistency verdict there.
#'
#' @section Floating-point tolerance: Shifting a number by `digits` decimal
#'   places is not exact: `0.145 * 100` is stored as `14.499999999999998`,
#'   just below the tie it is meant to be. All of these functions therefore
#'   nudge the shifted value by about `1.5e-9` before rounding, so that
#'   `round_up(0.145, 2)` is `0.15`, as in the software being reconstructed.
#'   [`unround()`] reports bounds that assume the same tolerance.
#'
#'   The nudge is fixed, whereas representation error grows with `x *
#'   10^digits`. Up to about `1e7` for that product, far beyond any reported
#'   summary statistic, the nudge dominates. Past it, a value exactly on a
#'   boundary may be rounded in either direction.
#'
#' @param x Numeric. The number to round.
#' @param digits Integer. Number of decimal places to round to. Default is
#'   `0`. Negative values round to powers of ten: `round_up(1250, digits =
#'   -2)` is `1300`.
#' @param threshold Numeric. Only in `round_up_from()` and `round_down_from()`.
#'   The point within a step at which rounding switches direction, in tenths of
#'   a step; greater than `0` and less than `10`. `round_up_from()` rounds up
#'   when the part cut off is at least `threshold` tenths of a step, and
#'   `round_down_from()` rounds down when it is at most that many, so the two
#'   only differ for a value exactly on the threshold. `round_up()` and
#'   `round_down()` are the pair at `5`.
#' @param symmetric Logical. If `TRUE`, negative numbers are rounded like their
#'   absolute values. Default is `FALSE`. See *Negative numbers*.
#'
#' @return Numeric. `x` rounded to `digits`.
#'
#' @export
#'
#' @name rounding-common
#'
#' @seealso [`round_ties_up()`] and friends are the same procedures under
#'   names that state the tie rule, so that `symmetric` need not be given.
#'   [`round_ceiling()`], [`round_floor()`], [`round_trunc()`], and
#'   [`round_anti_trunc()`] always round in one direction.
#'
#' @examples
#' # Except at a tie, all of these agree:
#'    round_up(x = 9.273, digits = 1)
#'  round_down(x = 9.273, digits = 1)
#' base::round(x = 9.273, digits = 1)
#'
#' # At ties, they differ:
#' original <- c(
#'   0.05, 0.15, 0.25, 0.35, 0.45,
#'   0.55, 0.65, 0.75, 0.85, 0.95
#' )
#' tibble::tibble(
#'   original,
#'   round_up = round_up(x = original, digits = 1),
#'   round_down = round_down(x = original, digits = 1),
#'   base_round = base::round(x = original, digits = 1)
#' )
#'
#' # Ties in negative numbers go up on the number line
#' # by default, and away from zero with `symmetric`:
#' round_up(x = -2.5)                     # Stata
#' round_up(x = -2.5, symmetric = TRUE)   # Excel, SAS, SPSS, Matlab
#'
#' # A custom threshold moves the point at which
#' # rounding switches direction:
#' round_up_from(x = 4.28, digits = 1, threshold = 9)   # 8 < 9, so down
#' round_up_from(x = 4.28, digits = 1, threshold = 1)   # 8 >= 1, so up

# Round up from some threshold -----------------------------------------------

#' @rdname rounding-common
#' @export

round_up_from <- function(x, digits = 0L, threshold, symmetric = FALSE) {
  # A `threshold` outside of `(0, 10)` would silently turn this into
  # `round_ceiling()` or `round_floor()`:
  check_threshold_valid(threshold)

  # Primitives only, not `isTRUE()` and `isFALSE()`: this runs once per
  # candidate value, and those closures cost half again as much as the rounding.
  if (!is.logical(symmetric) || length(symmetric) != 1L || is.na(symmetric)) {
    abort_symmetric_invalid(symmetric)
  }

  p10 <- 10^digits
  offset <- tie_offset_up(threshold)

  if (symmetric) {
    restore_sign(floor(abs(x) * p10 + offset) / p10, x)
  } else {
    floor(x * p10 + offset) / p10
  }
}


# Round down from some threshold ---------------------------------------------

#' @rdname rounding-common
#' @export

round_down_from <- function(x, digits = 0L, threshold, symmetric = FALSE) {
  check_threshold_valid(threshold)

  if (!is.logical(symmetric) || length(symmetric) != 1L || is.na(symmetric)) {
    abort_symmetric_invalid(symmetric)
  }

  p10 <- 10^digits
  offset <- tie_offset_down(threshold)

  if (symmetric) {
    restore_sign(ceiling(abs(x) * p10 - offset) / p10, x)
  } else {
    ceiling(x * p10 - offset) / p10
  }
}


# Round up from 5 ------------------------------------------------------------

#' @rdname rounding-common
#' @export

round_up <- function(x, digits = 0L, symmetric = FALSE) {
  round_up_from(x = x, digits = digits, threshold = 5, symmetric = symmetric)
}


# Round down from 5 ----------------------------------------------------------

#' @rdname rounding-common
#' @export

round_down <- function(x, digits = 0L, symmetric = FALSE) {
  round_down_from(x = x, digits = digits, threshold = 5, symmetric = symmetric)
}


# Shared by `check_rounding_spec_singular()` and the exported rounding
# functions. Each caller tests `symmetric` inline because it runs once per
# candidate value; only the error is shared.
abort_symmetric_invalid <- function(symmetric) {
  abort_in_export(
    "`symmetric` must be `TRUE` or `FALSE`.",
    "x" = "It is {.obj_type_friendly {symmetric}}."
  )
}
