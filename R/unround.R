# Helper function used in the main function `unround()` via a vectorized version
# right below:
rounding_bounds_scalar <- function(rounding, x_num, d_var, d) {
  # Manage the two rounding procedures that depend on the sign of the input
  # number, rounding with truncation and "anti-truncation":
  if (any(rounding %in% c("trunc", "anti_trunc"))) {
    rounding_orig <- rounding
    if (x_num > 0) {
      rounding <- "trunc_x_greater"
    } else if (x_num < 0) {
      rounding <- "trunc_x_less"
    } else {
      rounding <- "trunc_x_is_0"
    }

    if (any(rounding_orig == "anti_trunc")) {
      rounding <- paste0("anti_", rounding)
    }

    # fmt: skip
    return(switch(
      rounding,              #     (1)              (2)               (3)    (4)
      "trunc_x_greater"      = list(x_num,           x_num + (2 * d), "<=", "<"),
      "trunc_x_less"         = list(x_num - (2 * d), x_num,           "<", "<="),
      "trunc_x_is_0"         = list(x_num - (2 * d), x_num + (2 * d), "<",  "<"),
      "anti_trunc_x_greater" = list(x_num - (2 * d), x_num,           "<=", "<"),
      "anti_trunc_x_less"    = list(x_num,           x_num + (2 * d), "<=", "<"),
      "anti_trunc_x_is_0"    = list(NA,              NA,               NA,   NA)
    ))
  }

  # This switch-statement is evaluated for all other rounding procedures:
  # fmt: skip
  switch(
    rounding,    #     (1)              (2)               (3)   (4)
    "up_or_down" = list(x_num - d_var,   x_num + d_var,   "<=", "<="),
    "up"         = list(x_num - d_var,   x_num + d_var,   "<=",  "<"),
    "down"       = list(x_num - d_var,   x_num + d_var,   "<",  "<="),
    "even"       = list(x_num - d,       x_num + d,       "<",   "<"),
    "ceiling"    = list(x_num - (2 * d), x_num,           "<",  "<="),
    "floor"      = list(x_num,           x_num + (2 * d), "<=",  "<"),
    "error_trigger"
  )
}


# The above function is "scalar" (i.e., single-case only), but `unround()` is
# vectorized: It takes arguments of length > 1. Therefore, `rounding_bounds()`
# is created as a vectorized version of `rounding_bounds_scalar()`:
rounding_bounds <- Vectorize(rounding_bounds_scalar)


# Exact candidate-sum arithmetic ------------------------------------------

# GRIM and GRIMMER both need the set of integer sums `s` for which `s / n_items`
# would have been rounded to the reported mean. Deriving that set from
# floating-point products such as `floor(upper * n_items)` is unsafe: if such a
# product is mathematically an exact integer, its `double` representation can
# fall on either side of it, so a legitimate sum may be silently dropped or a
# phantom sum admitted. Either way, the verdict flips. See:
# https://github.com/lhdjung/scrutiny/issues/86
#
# The functions below therefore derive the range in integer arithmetic. Every
# bound that `unround()` can return is `x` plus a whole number of units of `1 /
# 10^(digits + 1)`, and `x` itself is a whole number of such units because it
# has `digits` decimal places. Both bounds hence have exact integer numerators
# over `10^(digits + 1)`, and comparing `s / n_items` to `numerator / 10^(digits
# + 1)` becomes a comparison between the integers `s * 10^(digits + 1)` and
# `n_items * numerator`.

# `floor_div()` and `ceiling_div()` divide `a` by `b` (with `b > 0`) and round
# the result towards `-Inf` and `+Inf`, respectively. Unlike `floor(a / b)` and
# `ceiling(a / b)`, they are exact for integer-valued `a` and `b`: the quotient
# `a / b` may land on the wrong side of an integer, so the candidate result is
# checked by multiplying it back out, which is exact in double precision. The
# error in `a / b` is far below 1, so a single correction step suffices.

floor_div <- function(a, b) {
  q <- floor(a / b)
  if (q * b > a) {
    q - 1
  } else if ((q + 1) * b <= a) {
    q + 1
  } else {
    q
  }
}

ceiling_div <- function(a, b) {
  -floor_div(-a, b)
}


# Integer offsets of the lower and upper rounding bounds from `x_num`, measured
# in units of `1 / 10^(digits + 1)`, plus the inclusivity of each bound. This is
# the exact-arithmetic counterpart of `rounding_bounds_scalar()` at the top of
# this file. The offsets follow the same table (see the `Rounding` section of
# `unround()`'s documentation), extended by the three compound rounding methods
# that `unround()` doesn't support: their bounds are the union of the bounds of
# the two constituent methods, and since both constituents include `x_num`
# itself, that union is again a single interval.
#
# Inclusivity, on the other hand, follows the rule that GRIM has always used:
# `"up"` excludes its upper bound (a value exactly at the midpoint rounds up,
# i.e. away from `x_num`), `"down"` excludes its lower bound, and every other
# method treats both bounds as inclusive. Several of those others do have an
# exclusive bound of their own -- `"ceiling"`, `"floor"`, `"trunc"`, and
# `"anti_trunc"` on one side, `"up_from"` and `"down_from"` like `"up"` and
# `"down"` -- and `"even"` has one that is unpredictable because `base::round()`
# breaks midpoint ties by the parity of the preceding digit. Tightening these is
# a separate question from the exact arithmetic below, which is why the existing
# lenient behavior is kept for now.
#
# Returns a list of four elements -- lower offset, upper offset, `incl_lower`,
# `incl_upper` -- or `NULL` if `rounding` is not a known method.

rounding_offsets <- function(rounding, threshold, x_num) {
  # Rounding with truncation and "anti-truncation" depends on the sign of the
  # input number:
  if (rounding == "trunc") {
    offsets <- if (x_num > 0) {
      list(0, 10)
    } else if (x_num < 0) {
      list(-10, 0)
    } else {
      list(-10, 10)
    }
  } else if (rounding == "anti_trunc") {
    offsets <- if (x_num > 0) {
      list(-10, 0)
    } else if (x_num < 0) {
      list(0, 10)
    } else {
      # `anti_trunc` is undefined for zero, just as in `unround()`:
      list(NA, NA)
    }
  } else {
    # fmt: skip
    offsets <- switch(
      rounding,              #     lower                             upper
      "up_or_down"           = list(-threshold,                      threshold),
      "up"                   = list(-threshold,                      threshold),
      "down"                 = list(-threshold,                      threshold),
      "even"                 = list(-5,                              5),
      "ceiling"              = list(-10,                             0),
      "floor"                = list(0,                               10),
      "ceiling_or_floor"     = list(-10,                             10),
      "up_from"              = list(threshold - 10,                  threshold),
      "down_from"            = list(-threshold,                      10 - threshold),
      "up_from_or_down_from" = list(min(threshold - 10, -threshold), max(threshold, 10 - threshold)),
      return(NULL)
    )
  }

  c(offsets, list(rounding != "down", rounding != "up"))
}


# Range of integer sums `s` for which `s / n_items` lies within the rounding
# bounds of `x_num`, which has `digits` decimal places. Returns a length-2
# numeric vector, `c(lower, upper)`; if the first element is greater than the
# second, no consistent sum exists. Both elements are `NA` if the rounding
# bounds are undefined (as with `"anti_trunc"` and a zero `x_num`).

sum_range <- function(x_num, n_items, digits, rounding, threshold) {
  offsets <- rounding_offsets(rounding, threshold, x_num)

  if (is.null(offsets)) {
    cli::cli_abort(c(
      "`rounding` must be one of the designated string values.",
      "x" = "It is {wrong_spec_string(rounding)}.",
      "i" = "See `vignette(\"rounding-options\")`."
    ))
  }

  if (anyNA(offsets) || !is.finite(n_items) || n_items <= 0) {
    return(c(NA_real_, NA_real_))
  }

  # `threshold` is documented as an integer but not enforced to be one. If it is
  # fractional, the offsets are scaled up by a power of ten (along with the
  # denominator) until they are whole numbers again. If no such power is found
  # within a sensible range, the arithmetic below silently degrades to the
  # floating-point behavior of earlier scrutiny versions, which is no worse than
  # the status quo:
  bounds <- c(offsets[[1L]], offsets[[2L]])
  scale <- 1
  while (scale < 1e6 && any(bounds * scale != round(bounds * scale))) {
    scale <- scale * 10
  }
  bounds <- bounds * scale

  # Common denominator of both bounds, and the numerators over it:
  denom <- 10^(digits + 1L) * scale
  x_shifted <- round(x_num * denom)
  num_lower <- x_shifted + bounds[1L]
  num_upper <- x_shifted + bounds[2L]

  # `s / n_items >= num_lower / denom`  <==>  `s * denom >= n_items * num_lower`
  lower <- ceiling_div(n_items * num_lower, denom)
  if (!offsets[[3L]] && lower * denom == n_items * num_lower) {
    lower <- lower + 1
  }

  # `s / n_items <= num_upper / denom`  <==>  `s * denom <= n_items * num_upper`
  upper <- floor_div(n_items * num_upper, denom)
  if (!offsets[[4L]] && upper * denom == n_items * num_upper) {
    upper <- upper - 1
  }

  c(lower, upper)
}


#' Reconstruct rounding bounds
#'
#' @description `unround()` takes a rounded number and returns the range of the
#'   original value: lower and upper bounds for the hypothetical earlier number
#'   that was later rounded to the input number. It also displays a range with
#'   inequation signs, showing whether the bounds are inclusive or not.
#'
#'   By default, the presumed rounding method is rounding up (or down) from 5.
#'   See the `Rounding` section for other methods.

#' @details The function is vectorized over `x` and `rounding`. This can be
#'   useful to unround multiple numbers at once, or to check how a single number
#'   is unrounded with different assumed rounding methods.
#'
#'   If both vectors have a length greater than 1, it must be the same
#'   length. However, this will pair numbers with rounding methods, which can be
#'   confusing. It is recommended that at least one of these input vectors has
#'   length 1.
#'
#'   Why does `x` need to be a string if `digits` is not specified? In that
#'   case, `unround()` must count decimal places by itself. If `x` then was
#'   numeric, it wouldn't have any trailing zeros because these get dropped from
#'   numerics.
#'
#'   Trailing zeros are as important for reconstructing boundary values as any
#'   other trailing digits would be. Strings don't drop trailing zeros, so they
#'   are used instead.

#' @section Rounding: Depending on how `x` was rounded, the boundary values can
#'   be inclusive or exclusive. The `incl_lower` and `incl_upper` columns in the
#'   resulting tibble are `TRUE` in the first case and `FALSE` in the second.
#'   The `range` column reflects this with equation and inequation signs.
#'
#'   However, these ranges are based on assumptions about the way `x` was
#'   rounded. Set `rounding` to the rounding method that hypothetically lead to
#'   `x`:
#'
#'   | \strong{Value of `rounding`}           | \strong{Corresponding range} |
#'   | ---                                    | ---                          |
#'   | `"up_or_down"` (default)               | `lower <= x <= upper`        |
#'   | `"up"`                                 | `lower <= x < upper`         |
#'   | `"down"`                               | `lower < x <= upper`         |
#'   | `"even"`                               | (no fix range)               |
#'   | `"ceiling"`                            | `lower < x = upper`          |
#'   | `"floor"`                              | `lower = x < upper`          |
#'   | `"trunc"` (positive `x`)               | `lower = x < upper`          |
#'   | `"trunc"` (negative `x`)               | `lower < x = upper`          |
#'   | `"trunc"` (zero `x`)                   | `lower < x < upper`          |
#'   | `"anti_trunc"` (positive `x`)          | `lower < x = upper`          |
#'   | `"anti_trunc"` (negative `x`)          | `lower = x < upper`          |
#'   | `"anti_trunc"` (zero `x`)              | (undefined; `NA`)            |
#'
#' Base R's own `round()` (R version >= 4.0.0), referenced by `rounding =
#' "even"`, is reconstructed in the same way as `"up_or_down"`, but whether the
#' boundary values are inclusive or not is hard to predict. Therefore,
#' `unround()` checks if they are, and informs you about it.

#' @param x String or numeric. Rounded number. `x` must be a string unless
#'   `digits` is specified (most likely by a function that uses `unround()` as a
#'   helper).
#' @param rounding String. Rounding method presumably used to create `x`.
#'   Default is `"up_or_down"`. For more, see section `Rounding`.
#' @param threshold Integer. Number from which to round up or down. Other
#'   rounding methods are not affected. Default is `5`.
#' @param digits Integer. This argument is meant to make `unround()` more
#'   efficient to use as a helper function so that it doesn't need to
#'   redundantly count decimal places. Don't specify it otherwise. Default is
#'   `NULL`, in which case decimal places really are counted internally and `x`
#'   must be a string.
#'
#' @return A tibble with seven columns: `range`, `rounding`, `lower`,
#'   `incl_lower`, `x`, `incl_upper`, and `upper`. The `range` column is a handy
#'   representation of the information stored in the columns from `lower` to
#'   `upper`, in the same order.
#'
#' @seealso For more about rounding `"up"`, `"down"`, or to `"even"`, see
#'   [`round_up()`].
#'
#'   For more about the less likely `rounding` methods, `"ceiling"`, `"floor"`,
#'   `"trunc"`, and `"anti_trunc"`, see [`round_ceiling()`].
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # By default, the function assumes that `x`
#' # was either rounded up or down:
#' unround(x = "2.7")
#'
#' # If `x` was rounded up, run this:
#' unround(x = "2.7", rounding = "up")
#'
#' # Likewise with rounding down...
#' unround(x = "2.7", rounding = "down")
#'
#' # ...and with `base::round()` which, broadly
#' # speaking, rounds to the nearest even number:
#' unround(x = "2.7", rounding = "even")
#'
#' # Multiple input number-strings return
#' # multiple rows in the output data frame:
#' unround(x = c(3.6, "5.20", 5.174))

# # Full example inputs:
# x <- "2.37"
# rounding <- "up_or_down"
# threshold <- 5
# digits <- NULL

unround <- function(x, rounding = "up_or_down", threshold = 5, digits = NULL) {
  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(x, rounding))

  # The number of decimal places might be given from within another function via
  # the `digits` argument. Otherwise -- if `digits` is not specified, and
  # therefore `NULL` -- the `x` argument must be a string so that decimal
  # places can be counted accurately (cf. trailing zeros), which is then done:
  if (is.null(digits)) {
    if (!is.character(x)) {
      cli::cli_abort(c(
        "`x` is {an_a_type(x)}.",
        "x" = "If `digits` is not specified, `x` must be a string."
      ))
    }
    digits <- decimal_places(x)
  }

  # Determine the difference between the rounded number and the boundary values.
  # That difference is variable when rounding up or down, because in that case,
  # it depends on the value of `threshold`:
  p10 <- 10^(digits + 1L)
  d <- 5 / p10
  d_var <- threshold / p10

  # The bound helper function operates on the numeric value of `x`:
  x_num <- as.numeric(x)

  # Calculate the boundary values and determine out whether they are inclusive
  # or not, going by the `rounding` argument. In order to vectorize `rounding`,
  # the helper function at the top of the present file is called:
  bounds <- rounding_bounds(
    rounding = rounding,
    x_num = x_num,
    d_var = d_var,
    d = d
  )

  # Throw error if `rounding` was not specified in a valid way:
  if (any("error_trigger" == bounds)) {
    cli::cli_abort(c(
      "`rounding` must be one or more of the designated \\
      string values. See documentation for `unround()`, \\
      section `Rounding`.",
      "x" = "It is {wrong_spec_string(rounding)}."
    ))
  }

  # Split the `bounds` list up into its four component vectors:
  lower <- as.numeric(bounds[1L, ]) # lower bound
  upper <- as.numeric(bounds[2L, ]) # upper bound
  sign_lower <- as.character(bounds[3L, ]) # lower bound inclusive (`"<="`)?
  sign_upper <- as.character(bounds[4L, ]) # upper bound inclusive (`"<="`)?

  # Return a tibble that displays the range with its appropriate signs and
  # includes all the results that constitute the range
  tibble::new_tibble(
    list(
      # fmt: skip
      range = paste0(
        lower, " ", sign_lower, " x(", x, ") ", sign_upper, " ", upper
      ),
      rounding = rounding,
      lower = lower,
      incl_lower = sign_lower == "<=",
      x = x,
      incl_upper = sign_upper == "<=",
      upper = upper
    ),
    nrow = length(x),
    class = NULL
  )
}
