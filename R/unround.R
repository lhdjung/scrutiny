# Exact candidate-sum arithmetic ------------------------------------------

# GRIM and GRIMMER both need the set of integer sums `s` for which `s / n_items`
# would have been rounded to the reported mean. Deriving it from floating-point
# products such as `floor(upper * n_items)` is unsafe: where such a product is
# mathematically an exact integer, its `double` can fall on either side of it,
# dropping a legitimate sum or admitting a phantom one, and the verdict flips.
# GRIMMER derived its sums this way until 1.0.0 (#86). GRIM did not: it rounded
# the two nearest granules forward and compared them to `x` with a tolerance.
#
# The functions below therefore work in integer arithmetic. Every bound that
# `unround()` can return is `x` plus a whole number of units of
# `1 / 10^(digits + 1)`, and `x` itself is such a whole number because it has
# `digits` decimal places -- so both bounds have exact integer numerators over
# `10^(digits + 1)`, and `s / n_items` vs. `numerator / 10^(digits + 1)` becomes
# a comparison of the integers `s * 10^(digits + 1)` and `n_items * numerator`.
#
# This is exact while every integer involved stays below `2^53`. The products
# are on the order of `n * 10^(2 * digits)`, far below it for any realistic
# input; beyond it the arithmetic degrades to plain floating point.
#
# Dividing two such integers is exact as well, so `floor(a / b)` and
# `ceiling(a / b)` need no correction. IEEE 754 division returns the double
# nearest the true quotient, and a quotient that is not an integer lies at least
# `1 / b` from every integer -- more than half the spacing of doubles there, as
# long as `|a| < 2^53`. A helper that multiplied the result back out to check it
# never corrected anything below that limit, and above it, where the product
# rounds, it turned correct results into wrong ones more often than the reverse.

# The floor of a sum of two fractions, `a1 / b1 + a2 / b2` (`b1`, `b2` positive
# integers), plus a flag for whether the sum is an integer, which callers need
# to honor an exclusive bound. Splitting each fraction into integer part and
# remainder first keeps every product below `2 * b1 * b2`; putting both over
# `b1 * b2` directly would leave the exact range far sooner.

floor_frac_sum <- function(a1, b1, a2, b2) {
  q1 <- floor(a1 / b1)
  q2 <- floor(a2 / b2)
  # Both remainders are non-negative and smaller than their own denominator, so
  # the combined numerator below stays under `2 * b1 * b2`:
  num <- (a1 - q1 * b1) * b2 + (a2 - q2 * b2) * b1
  den <- b1 * b2
  # The fractional parts sum to less than 2, so this carry is 0 or 1:
  carry <- floor(num / den)
  list(floor = q1 + q2 + carry, exact = num == carry * den)
}


# Integer offsets of the lower and upper rounding bounds from `x_num`, in units
# of `1 / 10^(digits + 1)`, plus each bound's inclusivity. This is the single
# source of truth for rounding bounds: `unround()`, the consistency tests, and
# every other function that needs such bounds derive them from it. The offsets
# follow the table in the `Rounding` section of `unround()`'s documentation,
# plus the compound methods, whose bounds are the union of their constituents'
# (a single interval, since both constituents include `x_num`).
#
# Each bound is inclusive exactly as the corresponding function in reround.R
# behaves there -- e.g. `"up"` excludes its upper bound, where a value rounds
# away from `x_num`. `"even"` is the exception: `base::round()` breaks ties by
# the parity of the preceding digit, and whether a tie occurs at all depends on
# the binary representation, so both its bounds are treated as inclusive. That
# makes a test too permissive rather than too strict -- the safe direction.
#
# `threshold` deliberately plays no role for `"up_or_down"`, `"up"`, and
# `"down"`, matching `round_up()` and `round_down()`, which round from a fixed
# 5. The `"*_from"` methods are the parameterized ones.
#
# Returns a list of lower offset, upper offset, `incl_lower`, `incl_upper` --
# all `NA` if `x_num` is missing or infinite, or `NULL` if `rounding` is
# unknown.

rounding_offsets <- function(rounding, threshold, x_num, symmetric = FALSE) {
  check_rounding_spec_singular(rounding, threshold, symmetric)

  # The branches below need a sign, and a missing value has none. Standing in a
  # positive number keeps `rounding` validated the way it is for any other
  # value; the offsets it yields are discarded at the end. An infinity counts as
  # missing, as it does in the tests themselves: its "bounds" were `Inf` on both
  # sides, which is no range at all.
  x_missing <- !is.finite(x_num)
  if (x_missing) {
    x_num <- 1
  }

  # A `"ties_*"` string stands in for a `rounding` and a `symmetric` together.
  # `reround()` resolves it through the same helper, so the bounds stay in step
  # with the rounding functions they invert:
  spec <- resolve_ties_rounding(rounding, symmetric)
  rounding <- spec$rounding
  symmetric <- spec$symmetric

  # Only the parameterized methods validate `threshold`, as in `reround()`: a
  # threshold outside `(0, 10)` makes one direction unreachable, and the offsets
  # below would encode that silently:
  if (rounding %in% c("up_from", "down_from", "up_from_or_down_from")) {
    check_threshold_valid(threshold)
  }

  # With `symmetric`, a negative number rounds like its absolute value -- which
  # is what the opposite method does to it anyway, so swapping the method
  # suffices. A threshold is measured from the lower end of the step, so
  # mirroring the step mirrors it too; at the fixed `5` of `"up"`/`"down"`,
  # `10 - threshold` is `threshold` again:
  if (symmetric && x_num < 0) {
    if (rounding %in% c("up_from", "down_from", "up_from_or_down_from")) {
      threshold <- 10 - threshold
    }
    # fmt: skip
    rounding <- switch(
      rounding,
      "up"        = "down",
      "down"      = "up",
      "up_from"   = "down_from",
      "down_from" = "up_from",
      rounding
    )
  }

  # Truncation and "anti-truncation" depend on the sign of `x_num`:

  # fmt: skip
  if (rounding == "trunc") {
    offsets <- if (x_num > 0) {
      list(0,   10,  TRUE,  FALSE)
    } else if (x_num < 0) {
      list(-10, 0,   FALSE, TRUE)
    } else {
      list(-10, 10,  FALSE, FALSE)
    }
  } else if (rounding == "anti_trunc") {
    # `anti_trunc()` is `round_ceiling()` above zero and `round_floor()` below
    # it. At zero it is neither: every non-zero value is taken away from zero,
    # so only zero itself is reported as zero. That single-point range is a real
    # answer, not a missing one.
    offsets <- if (x_num > 0) {
      list(-10, 0,   FALSE, TRUE)
    } else if (x_num < 0) {
      list(0,   10,  TRUE,  FALSE)
    } else {
      list(0,   0,   TRUE,  TRUE)
    }
  } else {
    # fmt: skip
    offsets <- switch(
      rounding,              #     lower            upper           incl_lower  incl_upper
      "up_or_down"           = list(-5,             5,              TRUE,       TRUE),
      "up"                   = list(-5,             5,              TRUE,       FALSE),
      "down"                 = list(-5,             5,              FALSE,      TRUE),
      "even"                 = list(-5,             5,              TRUE,       TRUE),
      "ceiling"              = list(-10,            0,              FALSE,      TRUE),
      "floor"                = list(0,              10,             TRUE,       FALSE),
      "ceiling_or_floor"     = list(-10,            10,             FALSE,      FALSE),
      "up_from"              = list(threshold - 10, threshold,      TRUE,       FALSE),
      "down_from"            = list(threshold - 10, threshold,      FALSE,      TRUE),
      "up_from_or_down_from" = list(threshold - 10, threshold,      TRUE,       TRUE),
      return(NULL)
    )
  }

  # At zero the mirroring happens inside the interval: its negative half is the
  # reflection of its positive half, so both ends behave like the upper end
  # does for a positive number.

  # fmt: skip
  if (
    symmetric &&
      x_num == 0 &&
      rounding %in% c(
        "up_or_down", "up", "down",
        "up_from_or_down_from", "up_from", "down_from"
      )
  ) {
    offsets[[1L]] <- -offsets[[2L]]
    offsets[[3L]] <- offsets[[4L]]
  }

  if (x_missing) {
    # `bound_numerators()` turns these into `NULL`, which every caller already
    # reports as an undecidable case:
    return(list(NA, NA, NA, NA))
  }

  offsets
}


# Integer numerators of `x_num`'s two rounding bounds over a common denominator,
# plus each bound's inclusivity. Returns `NULL` if the bounds are undefined,
# which happens only for a missing or infinite `x_num`, and errors on an unknown
# `rounding`.

bound_numerators <- function(x_num, digits, rounding, threshold, symmetric) {
  offsets <- rounding_offsets(rounding, threshold, x_num, symmetric)

  if (is.null(offsets)) {
    abort_in_export(
      "`rounding` must be one of the designated string values.",
      "x" = "It is {wrong_spec_string(rounding)}.",
      "i" = "See `vignette(\"rounding-options\")`."
    )
  }

  if (anyNA(offsets)) {
    return(NULL)
  }

  # `threshold` is documented as an integer but not enforced to be one. Scale a
  # fractional one up by a power of ten, along with the denominator, until the
  # offsets are whole again; failing that, the arithmetic degrades to floating
  # point:
  bounds <- c(offsets[[1L]], offsets[[2L]])
  scale <- 1
  while (scale < 1e6 && any(bounds * scale != round(bounds * scale))) {
    scale <- scale * 10
  }
  bounds <- bounds * scale

  denom <- 10^(digits + 1L) * scale
  x_shifted <- round(x_num * denom)

  lower <- x_shifted + bounds[1L]
  upper <- x_shifted + bounds[2L]
  incl_lower <- offsets[[3L]]
  incl_upper <- offsets[[4L]]

  list(
    lower = lower,
    upper = upper,
    denom = denom,
    incl_lower = incl_lower,
    incl_upper = incl_upper
  )
}


# Range of integer sums `s` for which `s / n_items` lies within the rounding
# bounds of `x_num`, which has `digits` decimal places. Returns a length-2
# numeric vector, `c(lower, upper)`; if the first element is greater than the
# second, no consistent sum exists. Both elements are `NA` if the rounding
# bounds are undefined, i.e. if `x_num` is missing.

sum_range <- function(
  x_num,
  n_items,
  digits,
  rounding,
  threshold,
  symmetric = FALSE
) {
  bounds <- bound_numerators(x_num, digits, rounding, threshold, symmetric)

  if (is.null(bounds) || !is.finite(n_items) || n_items <= 0) {
    return(c(NA_real_, NA_real_))
  }

  denom <- bounds$denom

  # `s / n_items >= lower / denom`  <==>  `s * denom >= n_items * lower`
  lower <- ceiling(n_items * bounds$lower / denom)
  if (!bounds$incl_lower && lower * denom == n_items * bounds$lower) {
    lower <- lower + 1
  }

  # `s / n_items <= upper / denom`  <==>  `s * denom <= n_items * upper`
  upper <- floor(n_items * bounds$upper / denom)
  if (!bounds$incl_upper && upper * denom == n_items * bounds$upper) {
    upper <- upper - 1
  }

  c(lower, upper)
}


# The sum of squares of the item-level values is
#
#   ((n - 1) * sd^2 + n * (s / (n * items))^2) * items^2 ==
#     (n - 1) * sd^2 * items^2 + s^2 / n # nolint
#
# for a sample SD, and the same with `n` in place of `n - 1` for a population
# SD; `n_dev` is that denominator. With `sd` given as `num / denom`, both terms
# are exact rationals. The first does not depend on the candidate sum `s`, so it
# is pre-computed once per SD bound, outside GRIMMER's loop, split into an
# integer part and a proper fraction to keep the products small. Returns that
# integer part plus the numerator and denominator of the fraction.

sd_square_term <- function(num, n_dev, items, denom) {
  # `sd^2 * items^2` as a fraction over `denom^2`:
  numerator <- num^2 * items^2
  denom_sq <- denom^2
  whole <- floor(numerator / denom_sq)
  list(
    int = n_dev * whole,
    num = n_dev * (numerator - whole * denom_sq),
    den = denom_sq
  )
}


# The smallest sum of squares that `n` whole numbers with the sum `s` can have.
# With `s == n * q + r`, these are the numbers closest to each other: `r` of
# them are `q + 1` and the others are `q`. Any other numbers with the same sum
# are more spread out, so the sum of their squares is greater. Unlike `s^2 / n`,
# this bound is reached by whole numbers.

sum_squares_min <- function(s, n) {
  q <- s %/% n
  r <- s - n * q
  n * q^2 + r * (2 * q + 1)
}


# The smallest sum of squares from `lower` to `upper` that `n < 5` whole numbers
# with the sum `s` can have, or `NA` if there is none; `lower` must be at least
# `sum_squares_min(s, n)`. For `n >= 5`, every sum of squares from there on with
# the parity of `s` is possible, but for fewer numbers, some are not. Which ones
# depends on `r`, the remainder of `s` divided by `n`, and on the excess over
# the minimum.

sum_squares_attainable <- function(s, n, lower, upper) {
  minimum <- sum_squares_min(s, n)

  # Replacing each number `k` by `1 - k` turns `r` into `n - r` and keeps the
  # excess, so `r` and `n - r` share a rule:
  r <- s %% n
  if (r > 0) {
    r <- min(r, n - r)
  }

  # Start at the parity of `s`, the only one possible:
  first <- lower + (lower - s) %% 2
  if (first > upper) {
    return(NA_real_)
  }

  # One number has only the minimum. Two numbers, `q + e` and `q - e` (or
  # `q + 1 + e` and `q - e`), have the excess `2 * e^2` (or `2 * e * (e + 1)`,
  # so that `2 * excess + 1` is an odd square). These are too sparse to scan
  # for: with many items averaged, the range can run into the hundreds of
  # millions, and the next square is computed instead.
  if (n == 1) {
    return(if (first == minimum) minimum else NA_real_)
  }
  if (n == 2) {
    excess_min <- first - minimum
    q <- if (r == 0) {
      minimum + 2 * next_square_root(excess_min / 2)^2
    } else {
      k <- next_square_root(2 * excess_min + 1)
      if (k %% 2 == 0) {
        k <- k + 1
      }
      minimum + (k^2 - 1) / 2
    }
    return(if (q <= upper) q else NA_real_)
  }

  # Three or four numbers can have most sums of squares, so a scan from `first`
  # ends soon. It runs step by step: `seq(first, upper, by = 2)` allocated the
  # whole range, which made GRIMMER slow for these `n` with many items averaged.
  q <- first
  while (q <= upper) {
    excess <- q - minimum
    half <- excess / 2
    attainable <- if (n == 3) {
      if (r == 0) is_loeschian(half) else is_loeschian(3 * half + 1)
    } else {
      if (r == 0) is_sum_three_squares(excess) else r == 1 || excess %% 8 != 6
    }
    if (attainable) {
      return(q)
    }
    q <- q + 2
  }

  NA_real_
}


# The smallest whole `k` with `k^2 >= m`, for `m >= 0`, corrected for
# floating-point error in `sqrt()`:
next_square_root <- function(m) {
  k <- ceiling(sqrt(m))
  if (k > 0 && (k - 1)^2 >= m) {
    k - 1
  } else if (k^2 < m) {
    k + 1
  } else {
    k
  }
}


is_square <- function(m) {
  m >= 0 && round(sqrt(m))^2 == m
}

# Is `m == x^2 + x * y + y^2` for some whole `x` and `y`? Equivalently, is
# `4 * m - 3 * y^2` a square (that of `2 * x + y`) for some `y`?
is_loeschian <- function(m) {
  if (m < 0) {
    return(FALSE)
  }
  y <- 0:floor(sqrt(4 * m / 3))
  any(round(sqrt(4 * m - 3 * y^2))^2 == 4 * m - 3 * y^2)
}

# Legendre's three-square theorem: `m >= 0` is a sum of three squares unless it
# is `4^a * (8 * b + 7)`.
is_sum_three_squares <- function(m) {
  while (m > 0 && m %% 4 == 0) {
    m <- m / 4
  }
  m %% 8 != 7
}


# Range of integer sums of squares consistent with candidate sum `s`, given the
# pre-computed SD terms from `sd_square_term()`. Returns a length-2 numeric
# vector; if the first element is greater than the second, no sum of squares
# fits, and the candidate sum fails GRIMMER's first test.

sum_squares_range <- function(
  s,
  n,
  term_lower,
  term_upper,
  incl_lower,
  incl_upper
) {
  s_squared <- s^2

  # An integer lower bound is its own ceiling; anything else, or an excluded
  # bound, takes the next integer up:
  low <- floor_frac_sum(term_lower$num, term_lower$den, s_squared, n)
  lower <- term_lower$int + low$floor
  if (!low$exact || !incl_lower) {
    lower <- lower + 1
  }

  high <- floor_frac_sum(term_upper$num, term_upper$den, s_squared, n)
  upper <- term_upper$int + high$floor
  if (high$exact && !incl_upper) {
    upper <- upper - 1
  }

  c(lower, upper)
}


#' Reconstruct rounding bounds
#'
#' @description `unround()` takes a rounded number and returns the range of
#'   values that would have been rounded to it: a lower and an upper bound,
#'   and whether each is inclusive. The default rounding method is
#'   `"up_or_down"`; see *Rounding* for the others.
#'
#' @details `x` is a string by default so that its trailing zeros can be
#'   counted, since a numeric value loses them. If the number of decimal places
#'   is known, pass it as `digits`, and `x` may be numeric. [`grim()`],
#'   [`grimmer()`], and [`debit()`] derive their bounds from the same internal
#'   functions as `unround()`, with `digits` taken from `digits_x` and
#'   `digits_sd`, so the tests and `unround()` always agree.
#'
#'   All arguments are vectorized. Those of length greater than 1 must have the
#'   same length and are paired element by element. Pairing `x` with
#'   `rounding` in this way gives a warning.
#'
#' @section Rounding: Whether a bound is inclusive depends on the rounding
#'   method. Under `"up"`, a value on the lower bound rounds up to `x`, but a
#'   value on the upper bound rounds up past it. The `range` column shows the
#'   bounds with the appropriate signs:
#'
#'   | \strong{Value of `rounding`}           | \strong{Corresponding range} |
#'   | ---                                    | ---                          |
#'   | `"up_or_down"` (default)               | `lower <= x <= upper`        |
#'   | `"up"`, `"ties_up"`                    | `lower <= x < upper`         |
#'   | `"down"`, `"ties_down"`                | `lower < x <= upper`         |
#'   | `"ties_away"` (positive `x`)           | `lower <= x < upper`         |
#'   | `"ties_away"` (negative `x`)           | `lower < x <= upper`         |
#'   | `"ties_zero"` (positive `x`)           | `lower < x <= upper`         |
#'   | `"ties_zero"` (negative `x`)           | `lower <= x < upper`         |
#'   | `"even"`, `"ties_even"`                | `lower <= x <= upper`        |
#'   | `"ceiling"`                            | `lower < x = upper`          |
#'   | `"floor"`                              | `lower = x < upper`          |
#'   | `"ceiling_or_floor"`                   | `lower < x < upper`          |
#'   | `"trunc"` (positive `x`)               | `lower = x < upper`          |
#'   | `"trunc"` (negative `x`)               | `lower < x = upper`          |
#'   | `"trunc"` (zero `x`)                   | `lower < x < upper`          |
#'   | `"anti_trunc"` (positive `x`)          | `lower < x = upper`          |
#'   | `"anti_trunc"` (negative `x`)          | `lower = x < upper`          |
#'   | `"anti_trunc"` (zero `x`)              | `lower = x = upper` (all `0`)|
#'   | `"up_from"`                            | `lower <= x < upper`         |
#'   | `"down_from"`                          | `lower < x <= upper`         |
#'   | `"up_from_or_down_from"`               | `lower <= x <= upper`        |
#'
#'   `threshold` only applies to the three `"*_from"` methods. It moves their
#'   range without widening it; each spans exactly one step at every
#'   threshold.
#'
#'   The `"ties_*"` methods each name a complete tie-breaking procedure, so
#'   giving `symmetric` with them is an error; see [`round_ties_up()`].
#'
#'   Under `"even"`, both bounds are reported as inclusive. [`base::round()`]
#'   decides a tie on the binary double, so whether a value on a bound rounds
#'   to `x` cannot be predicted from its decimal digits. Treating both bounds
#'   as inclusive can only widen the range, which is the safe direction for
#'   error detection.
#'
#' @param x String or numeric. The rounded number. Must be a string unless
#'   `digits` is specified. A missing or infinite `x` has `NA` bounds.
#' @param rounding String. The rounding method assumed to have produced `x`.
#'   Default is `"up_or_down"`. See *Rounding*.
#' @param threshold Numeric. For the `"*_from"` methods, the point within a
#'   step at which rounding switches direction, in tenths of a step; greater
#'   than `0` and less than `10`. Default is `5`. See [`round_up_from()`].
#' @param digits Integer. The number of decimal places in `x`. Default is
#'   `NULL`, in which case it is counted from the string `x`. Specify it if
#'   the number is already known, as a function that uses `unround()` as a
#'   helper would.
#' @param symmetric Logical. If `TRUE`, negative numbers are assumed to have
#'   been rounded like their absolute values under `"up"`, `"down"`, and the
#'   `"*_from"` methods. Default is `FALSE`. Must not be given with a `"ties_*"`
#'   method; see [`reround()`].
#'
#' @return A tibble with the columns `range`, `rounding`, `lower`,
#'   `incl_lower`, `x`, `incl_upper`, and `upper`. `range` summarizes the
#'   columns from `lower` to `upper`.
#'
#' @seealso [`round_up()`] for rounding up or down from 5, and
#'   [`round_ceiling()`] for the one-directional methods.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # By default, `x` may have been rounded up or down:
#' unround(x = "2.7")
#'
#' # If it was rounded up, the upper bound is exclusive:
#' unround(x = "2.7", rounding = "up")
#'
#' # Likewise with rounding down...
#' unround(x = "2.7", rounding = "down")
#'
#' # ...and with `base::round()`:
#' unround(x = "2.7", rounding = "even")
#'
#' # One row per input value:
#' unround(x = c(3.6, "5.20", 5.174))

unround <- function(
  x,
  rounding = "up_or_down",
  threshold = 5,
  digits = NULL,
  symmetric = FALSE
) {
  # Two arguments of length > 1 must have the same length. Pairing values of `x`
  # with values of `rounding` is confusing enough to warn about:
  check_lengths_congruent(list(x, rounding))

  # The other arguments need the same length check but no warning -- one
  # `digits` per `x` is the ordinary way to call this from a helper. Unchecked,
  # a short `digits` was recycled silently, giving the extra `x` values the
  # wrong number of decimal places and hence the wrong bounds.
  check_lengths_congruent(
    list(x, rounding, digits, threshold, symmetric),
    warn = FALSE
  )

  digits_given <- !is.null(digits)
  if (digits_given) {
    # The same check as in `reround()`: a fractional `digits` was accepted and
    # gave bounds on no decimal grid, and a string failed deep inside with
    # "non-numeric argument to mathematical function".
    check_digits_whole(digits)
    # `as.numeric()` below takes a factor's level codes and `TRUE` for 1, which
    # gave the bounds of those numbers next to `x`'s own spelling:
    if (
      !is.numeric(x) &&
        !is.character(x) &&
        !(is.logical(x) && all(is.na(x)))
    ) {
      cli::cli_abort(c(
        "`x` must be a string or a number.",
        "x" = "It is {.obj_type_friendly {x}}."
      ))
    }
  } else {
    # The decimal places are counted from `x`, which must then be a string so
    # that trailing zeros survive:
    if (!is.character(x)) {
      cli::cli_abort(c(
        "`x` is {an_a_type(x)}.",
        "x" = "If `digits` is not specified, `x` must be a string."
      ))
    }
    digits <- decimal_places(x)
  }

  # The bound helpers operate on the numeric value of `x`:
  x_num <- as.numeric(x)

  # Recycle all arguments to a common length, so that each output row describes
  # one complete combination. Leaving it to `paste0()` could make the columns
  # longer than the `nrow` taken from `x`, i.e. a malformed tibble:
  lengths_in <- c(
    length(x_num),
    length(rounding),
    length(digits),
    length(threshold),
    length(symmetric)
  )

  # Recycling stops at zero: with an empty argument there is no complete
  # combination to describe. The maximum alone let the length-1 defaults set the
  # row count, so `unround(character(0))` returned a phantom row:
  n_out <- if (any(lengths_in == 0L)) 0L else max(lengths_in)

  recycle <- function(value) rep_len(value, n_out)
  x_out <- recycle(x)
  x_num <- recycle(x_num)
  rounding <- recycle(rounding)
  digits <- recycle(digits)
  threshold <- recycle(threshold)
  symmetric <- recycle(symmetric)

  # With `digits` given, the value of `x` must fit into it. The bounds below are
  # `x` plus a whole number of units of the *next* decimal place, which takes
  # `x` to be a whole number of units of `digits` itself; a value with more
  # decimal places than that was silently rounded to `digits` and reported with
  # its own, longer spelling: `unround(1.234, digits = 1)` gave `1.18 <=
  # x(1.234) <= 1.28`, the bounds of nothing at all. Trailing zeros in a string
  # are not the problem -- `"4.50"` at one decimal place is 4.5 -- so the test
  # is the same two-step one as in `check_newly_numeric()`: `round()` decides,
  # and the decimal count is only consulted for what it leaves open. Counting is
  # the expensive step, so it runs on those values alone. A `digits` counted
  # from `x` itself cannot disagree with it. (`round()` rejects a zero-length
  # `digits`, hence the guard, and most of the cost of `decimal_places()` is
  # fixed, hence the second one.)
  too_many <- rep(FALSE, n_out)
  if (digits_given && n_out > 0L) {
    unsettled <-
      !is.na(x_num) &
      !is.na(digits) &
      x_num != round(x_num, digits)
    if (any(unsettled)) {
      too_many[unsettled] <-
        decimal_places(x_out[unsettled]) > digits[unsettled]
    }
  }

  if (any(too_many)) {
    decimals_x <- decimal_places(x_out[too_many])
    cli::cli_abort(c(
      "`x` has more decimal places than `digits` states.",
      "x" = "`digits` is {utils::head(digits[too_many], 3L)} for \\
      {utils::head(x_out[too_many], 3L)}, which {?has/have} \\
      {utils::head(decimals_x, 3L)}.",
      "i" = "`digits` is meant for helper use, where the decimal count is \\
      known. Leave it `NULL` to count the decimal places from a string `x`."
    ))
  }

  # The same helper GRIM and GRIMMER derive their candidate ranges from, so all
  # three agree on the bounds, on which rounding methods exist, and on what
  # `threshold` and `symmetric` mean. It gives each bound as an integer
  # numerator over a common denominator; dividing recovers the value:
  bounds <- lapply(seq_len(n_out), function(i) {
    bound_numerators(
      x_num = x_num[i],
      digits = digits[i],
      rounding = rounding[i],
      threshold = threshold[i],
      symmetric = symmetric[i]
    )
  })

  # `NULL` where the bounds are undefined, i.e. for a missing `x`:
  extract <- function(name, na_value) {
    vapply(
      bounds,
      function(b) if (is.null(b)) na_value else b[[name]],
      vector(mode = typeof(na_value), length = 1L),
      USE.NAMES = FALSE
    )
  }

  denom <- extract("denom", NA_real_)
  lower <- extract("lower", NA_real_) / denom
  upper <- extract("upper", NA_real_) / denom
  incl_lower <- extract("incl_lower", NA)
  incl_upper <- extract("incl_upper", NA)

  sign_lower <- dplyr::if_else(incl_lower, "<=", "<")
  sign_upper <- dplyr::if_else(incl_upper, "<=", "<")

  tibble::new_tibble(
    list(
      # fmt: skip
      range = paste0(
        lower, " ", sign_lower, " x(", x_out, ") ", sign_upper, " ", upper
      ),
      rounding = rounding,
      lower = lower,
      incl_lower = incl_lower,
      x = x_out,
      incl_upper = incl_upper,
      upper = upper
    ),
    nrow = n_out,
    class = NULL
  )
}
