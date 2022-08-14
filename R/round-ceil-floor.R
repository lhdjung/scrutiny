

#' Always round up, down, toward zero, or away from it
#'
#' @description Uncommon rounding procedures:
#'   - `round_ceiling()` always rounds up.
#'   - `round_floor()` always rounds down.
#'   - `round_trunc()` always rounds toward zero.
#'   - `round_anti_trunc()` always rounds away from zero. (`0` itself is
#'   rounded to `1`.)
#'
#' Despite not being widely used, they are featured here in case they are needed
#' for reconstruction.

#' @details `round_ceiling()`, `round_floor()`, and `round_trunc()` generalize
#'   the base functions `ceiling()`, `floor()`, and `trunc()`, and include them
#'   as special cases: With the default value for `digits`, 0, these `round_*`
#'   functions are equivalent to their respective base counterparts.
#'
#'   The last `round_*` function, `round_anti_trunc()`, generalizes another
#'   function presented here: `anti_trunc()` works like `trunc()` except it
#'   moves away from 0, rather than towards it. That is, whereas `trunc()`
#'   minimizes the absolute value of `x` (as compared to the other rounding
#'   functions), `anti_trunc()` maximizes it. `anti_trunc(x)` is therefore equal
#'   to `trunc(x)` ` + 1` if `x` is positive, and to `trunc(x) - 1` if `x` is
#'   negative.
#'
#'   `round_anti_trunc()`, then, generalizes `anti_trunc()` just as
#'   `round_ceiling()` generalizes `ceiling()`, etc.
#'
#'   Moreover, `round_trunc()` is equivalent to `round_floor()` for positive
#'   numbers and to `round_ceiling()` for negative numbers. The reverse is again
#'   true for `round_anti_trunc()`: It is equivalent to `round_ceiling()` for
#'   positive numbers and to `round_floor()` for negative numbers.
#'
#' @param x Numeric. The decimal number to round.
#' @param digits Integer. Number of digits to round `x` to. Default is `0`.
#'
#' @return Numeric. `x` rounded to `digits` (except for `anti_trunc()`, which
#'   has no `digits` argument).
#'
#' @export
#'
#' @aliases round_ceiling
#' @aliases round_floor
#' @aliases round_trunc
#'
#' @seealso `round_up()` and `round_down()` round up or down from 5,
#'   respectively. `round_up_from()` and `round_down_from()` allow users to
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



# Always round up ------------------------------------------------------------

round_ceiling <- function(x, digits = 0) {
  p10 <- 10 ^ digits

  ceiling(x * p10) / p10
}



# Always round down ----------------------------------------------------------

#' @rdname round_ceiling
#' @export

round_floor <- function(x, digits = 0) {
  p10 <- 10 ^ digits

  floor(x * p10) / p10
}



# Always round toward zero ---------------------------------------------------

#' @rdname round_ceiling
#' @export

round_trunc <- function(x, digits = 0) {

  p10 <- 10 ^ digits

  # For symmetry between positive and negative numbers, use the absolute value:
  core <- trunc(abs(x) * p10) / p10

  # If `x` is negative, its truncated version should be negative or zero.
  # Therefore, in this case, the function returns the negative of `core`, the
  # absolute value; otherwise it simply returns `core` itself:
  dplyr::if_else(x < 0, -core, core)

}



# Interlude: "anti-truncate" a number ----------------------------------------

#' @rdname round_ceiling
#' @export

anti_trunc <- function(x) {

  # For symmetry between positive and negative numbers, use the absolute value:
  core <- trunc(abs(x)) + 1
  # (Note that an equivalent formula would be `ceiling(abs(x))`.

  # If `x` is negative, its "anti-truncated" version should also be negative.
  # Therefore, in this case, the function returns the negative of the
  # anti-truncated absolute value of `x`, called `core`; otherwise it simply
  # returns `core` itself:
  dplyr::if_else(x < 0, -core, core)

}



# Always round away from zero ------------------------------------------------

#' @rdname round_ceiling
#' @export

round_anti_trunc <- function(x, digits = 0) {
  p10 <- 10 ^ digits

  anti_trunc(x * p10) / p10
}


