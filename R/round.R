

#' Likely rounding procedures
#'
#' @description `round_up()` rounds up from 5, `round_down()` rounds down from
#'   5. Otherwise, both functions work like `base::round()`.
#'
#'   `round_up()` and `round_down()` are special cases of `round_up_from()` and
#'   `round_down_from()`, which allow users to choose custom thresholds for
#'   rounding up or down, respectively.
#'
#' @details These functions differ from `base::round()` mainly insofar as the
#'   decision about rounding 5 up or down is not based on the integer portion of
#'   `x` (i.e., no "rounding to even"). Instead, in `round_up_from()`, that
#'   decision is determined by the `threshold` argument for rounding up, and
#'   likewise with `round_down_from()`. The threshold is constant at `5` for
#'   `round_up()` and `round_down()`.
#'
#'   As a result, these functions are more predictable and less prone to
#'   floating-point number quirks than `base::round()`. Compare `round_down()`
#'   and `base::round()` in the data frame for rounding 5 created in the
#'   Examples section below: `round_down()` yields a continuous sequence of
#'   final digits from 0 to 9, whereas `base::round()` behaves in a way that can
#'   only be explained by floating point issues.
#'
#'   However, this surprising behavior on the part of `base::round()` is not
#'   necessarily a flaw (see its documentation, or this vignette:
#'   https://rpubs.com/maechler/Rounding). In the present version of R (4.0.0 or
#'   later), `base::round()` works fine, and the functions presented here are
#'   not meant to replace it. Their main purpose as helpers within scrutiny is
#'   to reconstruct the computations of researchers who might have used
#'   different software. For example, SAS, SPSS, Stata, Matlab, and Excel round
#'   up from 5, whereas Python rounds down from 5. Other use cases might
#'   possibly include journal requirements.
#'
#' @param x Numeric. The decimal number to round.
#' @param digits Integer. Number of digits to round `x` to. Default is `0`.
#' @param threshold Integer. Only in `round_up_from()` and `round_down_from()`.
#'   Threshold for rounding up or down, respectively. Value is `5` in
#'   `round_up()`'s internal call to `round_up_from()` and in `round_down()`'s
#'   internal call to `round_down_from()`.
#' @param symmetric Boolean. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers should mirror that of positive numbers so that their
#'   absolute values are equal. Default is `FALSE`.
#'
#' @return Numeric. `x` rounded to `digits`.
#' @export
#'
#' @seealso `round_ceiling()` always rounds up, `round_floor()` always rounds
#'   down, `round_trunc()` always rounds toward 0, and `round_anti_trunc()`
#'   always round away from 0.
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
#' original <- c(     # Define example values
#'     0.05, 0.15, 0.25, 0.35, 0.45,
#'     0.55, 0.65, 0.75, 0.85, 0.95
#'     )
#' tibble::tibble(        # Output table
#'     original,
#'     round_up = round_up(x = original, digits = 1),
#'     round_down = round_down(x = original, digits = 1),
#'     base_round = base::round(x = original, digits = 1)
#'     )
#'
#' # (Note: Defining `original` as `seq(0.05:0.95, by = 0.1)`
#' # would lead to wrong results unless `original` is rounded
#' # to 2 or so digits before it's rounded to 1.)


# Round up from some threshold -----------------------------------------------

round_up_from <- function(x, digits = 0, threshold, symmetric = FALSE) {

  p10 <- 10 ^ digits

  if (symmetric) {
    dplyr::if_else(
      x < 0,
      - (floor(abs(x) * p10 + (1 - (threshold / 10))) / p10),
         floor(    x  * p10 + (1 - (threshold / 10))) / p10
    )
  } else {
    floor(x * p10 + (1 - (threshold / 10))) / p10
  }

}



# Round down from some threshold ---------------------------------------------

#' @rdname round_up_from
#' @export

round_down_from <- function(x, digits = 0, threshold, symmetric = FALSE) {

  p10 <- 10 ^ digits

  if (symmetric) {
    dplyr::if_else(
      x < 0,
      - (ceiling(abs(x) * p10 - (1 - (threshold / 10))) / p10),
         ceiling(    x  * p10 - (1 - (threshold / 10))) / p10
    )
  } else {
    ceiling(x * p10 - (1 - (threshold / 10))) / p10
  }

}


# Round up from 5 ------------------------------------------------------------

# We want `x`, the decimal number, to be rounded up if the part of the decimal
# portion to be cut off by rounding is 5 or greater. However, if that part is
# less than 5, `x` should instead be rounded down. The `threshold` for rounding
# up is therefore set to 5.

#' @rdname round_up_from
#' @export

round_up <- function(x, digits = 0, symmetric = FALSE) {

  round_up_from(x = x, digits = digits, threshold = 5, symmetric = symmetric)
}



# Round down from 5 ----------------------------------------------------------

# Here, we want `x` to be rounded down if the part of the decimal portion to be
# cut off by rounding is 5 or less. However, if that part is greater than 5, `x`
# should instead be rounded up. The `threshold` for rounding down is therefore
# set to 5.

#' @rdname round_up_from
#' @export

round_down <- function(x, digits = 0, symmetric = FALSE) {

  round_down_from(x = x, digits = digits, threshold = 5, symmetric = symmetric)
}

