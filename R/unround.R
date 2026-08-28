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

# All of this is exact only as long as every integer involved stays below
# `2^53`, the point up to which doubles represent integers without loss. The
# products formed below are on the order of `n * 10^(2 * digits)`, so the limit
# is far out of reach for the sample sizes and decimal counts that consistency
# testing deals with. Beyond it, the arithmetic silently degrades to the
# floating-point behavior of earlier scrutiny versions, which is no worse than
# the status quo.

# `floor_div()` and `ceiling_div()` divide `a` by `b` (with `b > 0`) and round
# the result towards `-Inf` and `+Inf`, respectively. Unlike `floor(a / b)` and
# `ceiling(a / b)`, they are exact for integer-valued `a` and `b`: the quotient
# `a / b` may land on the wrong side of an integer, so the candidate result is
# checked by multiplying it back out, which is exact in double precision. The
# error in `a / b` is far below 1, so a single correction step suffices.

floor_div <- function(a, b) {
  q <- floor(a / b)
  # A missing value has no floor to correct, and the comparisons below would
  # fail on it rather than pass it on. Every caller propagates `NA` from here up
  # to the verdict, which is what an undecidable value set should get:
  if (is.na(q)) {
    return(q)
  }
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


# `floor_frac_sum()` is the same idea for a sum of two fractions, `a1 / b1 + a2
# / b2` (with `b1` and `b2` positive integers). It returns the floor of that sum
# along with a flag for whether the sum is an integer, which is what the callers
# need in order to honor an exclusive bound.
#
# The obvious route -- putting both fractions over `b1 * b2` -- would multiply
# each numerator by the other denominator and overflow the exact range far
# sooner than necessary. Splitting each fraction into its integer part and its
# remainder first keeps every product below `2 * b1 * b2` instead.

floor_frac_sum <- function(a1, b1, a2, b2) {
  q1 <- floor_div(a1, b1)
  q2 <- floor_div(a2, b2)
  # Both remainders are non-negative and smaller than their own denominator, so
  # the combined numerator below stays under `2 * b1 * b2`:
  num <- (a1 - q1 * b1) * b2 + (a2 - q2 * b2) * b1
  den <- b1 * b2
  # The fractional parts sum to less than 2, so this carry is 0 or 1:
  carry <- floor_div(num, den)
  list(floor = q1 + q2 + carry, exact = num == carry * den)
}


# Integer offsets of the lower and upper rounding bounds from `x_num`, measured
# in units of `1 / 10^(digits + 1)`, plus the inclusivity of each bound. This is
# the single source of truth for rounding bounds in the package: `unround()`,
# `grim()`, and `grimmer()` all derive their ranges from it. The offsets follow
# the table in the `Rounding` section of `unround()`'s documentation, extended
# by the three compound rounding methods: their bounds are the union of the
# bounds of the two constituent methods, and since both constituents include
# `x_num` itself, that union is again a single interval. For `"up_or_down"` and
# `"up_from_or_down_from"` the two constituents span the very same interval and
# differ only in which endpoint each of them includes, so the union is that
# interval with both endpoints included. Only `"ceiling_or_floor"` is a union of
# two intervals that do not coincide.
#
# Each bound is inclusive or exclusive exactly as the corresponding rounding
# function in reround.R behaves at that bound -- e.g. `"up"` excludes its upper
# bound because a value at the midpoint rounds up, i.e. away from `x_num`, and
# `"ceiling"` excludes its lower bound because a value there ceilings to `x_num
# - 1` unit.
#
# `"even"` is the one method whose bounds cannot be pinned down: `base::round()`
# breaks midpoint ties by the parity of the preceding digit, and whether a tie
# occurs at all depends on the binary representation of the value. Both of its
# bounds are therefore treated as inclusive, which can only make a consistency
# test too permissive, never too strict -- the safe direction for an
# error-detection tool.
#
# `threshold` deliberately plays no role for `"up_or_down"`, `"up"`, and
# `"down"`, matching `round_up()` and `round_down()`, which round from a fixed
# 5. The `"*_from"` methods are the parameterized ones.
#
# Returns a list of four elements -- lower offset, upper offset, `incl_lower`,
# `incl_upper` -- all four of them `NA` if `x_num` is missing, or `NULL` if
# `rounding` is not a known method.

rounding_offsets <- function(rounding, threshold, x_num, symmetric = FALSE) {
  check_rounding_spec_singular(rounding, threshold, symmetric)

  # A missing value has no sign, and the branches below need one: `"trunc"` and
  # `"anti_trunc"` have different bounds on either side of zero, and `symmetric`
  # mirrors the methods it applies to. Standing in a positive number keeps
  # `rounding` validated the way it is for any other value -- an unknown method
  # is an input error whatever `x_num` is -- and the offsets it yields are
  # discarded at the end. A missing value is undecidable, not a value whose
  # bounds are known:
  x_missing <- is.na(x_num)
  if (x_missing) {
    x_num <- 1
  }

  # A `"ties_*"` string names a complete tie-breaking procedure, so it stands in
  # for a `rounding` and a `symmetric` together. `reround()` resolves it through
  # the same table, so the bounds below stay in step with the rounding functions
  # they invert:
  spec <- resolve_ties_rounding(rounding, symmetric)
  rounding <- spec$rounding
  symmetric <- spec$symmetric

  # The parameterized methods are the ones that `threshold` applies to, so they
  # are the ones that validate it -- as in `reround()`, and for the same reason:
  # a threshold outside `(0, 10)` makes one of the two directions unreachable,
  # and the offsets below would encode that silently:
  if (rounding %in% c("up_from", "down_from", "up_from_or_down_from")) {
    check_threshold_valid(threshold)
  }

  # With `symmetric`, the rounding of a negative number mirrors that of its
  # absolute value, which is precisely what the opposite method does to a
  # negative number anyway. Swapping the method here is therefore enough --
  # except that a threshold is measured from the lower end of the step, so
  # mirroring the step mirrors the threshold within it as well. At the `5` that
  # `"up"` and `"down"` round from, `10 - threshold` is `threshold` again, which
  # is why only the parameterized methods need the second line:
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

  # Rounding with truncation and "anti-truncation" depends on the sign of the
  # input number:

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
    # it, so it takes those bounds. At zero it is neither: every non-zero value,
    # however small, is taken away from zero to the next step out, so the only
    # value reported as zero is zero itself. That degenerate range is a real
    # answer rather than a missing one -- a mean reported as 0.00 under this
    # method really does pin the sum to exactly 0.
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

  # At zero, the mirroring happens inside the interval rather than beside it:
  # the negative half of the interval is the reflection of the positive half, so
  # both ends behave like the upper end does for a positive number.

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


# Integer numerators of the two rounding bounds of `x_num` over a common
# denominator, plus the inclusivity of each bound. Every bound that
# `rounding_offsets()` can produce is `x_num` plus a whole number of units of `1
# / 10^(digits + 1)`, and `x_num` itself is a whole number of such units because
# it has `digits` decimal places -- so both bounds have exact integer numerators
# over `10^(digits + 1)`.
#
# Returns `NULL` if the bounds are undefined, which now happens only for a
# missing `x_num`, and throws an error if `rounding` is not a known method.

bound_numerators <- function(x_num, digits, rounding, threshold, symmetric) {
  offsets <- rounding_offsets(rounding, threshold, x_num, symmetric)

  if (is.null(offsets)) {
    cli::cli_abort(c(
      "`rounding` must be one of the designated string values.",
      "x" = "It is {wrong_spec_string(rounding)}.",
      "i" = "See `vignette(\"rounding-options\")`."
    ))
  }

  if (anyNA(offsets)) {
    return(NULL)
  }

  # `threshold` is documented as an integer but not enforced to be one. If it is
  # fractional, the offsets are scaled up by a power of ten (along with the
  # denominator) until they are whole numbers again. If no such power is found
  # within a sensible range, the arithmetic downstream silently degrades to
  # floating point:
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
  lower <- ceiling_div(n_items * bounds$lower, denom)
  if (!bounds$incl_lower && lower * denom == n_items * bounds$lower) {
    lower <- lower + 1
  }

  # `s / n_items <= upper / denom`  <==>  `s * denom <= n_items * upper`
  upper <- floor_div(n_items * bounds$upper, denom)
  if (!bounds$incl_upper && upper * denom == n_items * bounds$upper) {
    upper <- upper - 1
  }

  c(lower, upper)
}


# GRIMMER's counterpart to `sum_range()`. For a given candidate sum `s`, this is
# the range of integer sums of squares that the reported SD admits. The sum of
# squares of the item-level values is
#
#   ((n - 1) * sd^2 + n * (s / (n * items))^2) * items^2 == (n - 1) * sd^2 *
#     items^2 + s^2 / n
#
# and with `sd` given as `num / denom`, both terms are exact rationals. The
# first one does not depend on `s`, so `sd_square_term()` pre-computes it once
# per SD bound, outside the loop over candidate sums, splitting it into an
# integer part and a proper fraction to keep the products small.
#
# Returns a list of the integer part and the numerator and denominator of the
# remaining fraction.

sd_square_term <- function(num, n, items, denom) {
  # `sd^2 * items^2` as a fraction over `denom^2`:
  numerator <- num^2 * items^2
  denom_sq <- denom^2
  whole <- floor_div(numerator, denom_sq)
  list(
    int = (n - 1) * whole,
    num = (n - 1) * (numerator - whole * denom_sq),
    den = denom_sq
  )
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

  # The lower bound is its own ceiling if it is an integer, and the next integer
  # up otherwise -- or in either case the next integer up if it is excluded:
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


# What a scale with known bounds adds to GRIMMER, where `sum_squares_range()`
# gives what the reported SD admits. The values summed and squared are the `n`
# whole-number totals of the individual respondents, each of them between
# `val_lower` and `val_upper` (i.e., between the scale's minimum and maximum,
# multiplied by the number of items), adding up to the candidate sum `s`. This
# is the greatest sum of squares they can have: the values are as far apart as
# the scale allows, so `k` of them sit at its maximum and the rest at its
# minimum, with at most one value in between to absorb what `s` leaves over.
#
# In SD space this is the sharp mean-conditional ceiling that Mestdagh et al.
# (2018) call "Structure S"; `strait::sd_bounds()` has it as
# `sd_max_structure_s()`, along with several bounds that scrutiny does not
# derive. Here it stays in sum-of-squares space, where GRIMMER already works and
# the arithmetic is exact.
#
# Returns `NULL` if no set of `n` values within the range adds up to `s` at all,
# in which case the candidate sum is out of reach whatever the SD is.
#
# The bound is exact, but the condition it yields is necessary rather than
# sufficient: not every sum of squares below it is attainable. With `n = 3`
# values from 0 to 10 that add up to 10, the ceiling is 100, yet 40 is out of
# reach. Like GRIMMER's other tests, it can therefore only move a verdict from
# `TRUE` to `FALSE` -- the safe direction for error detection. For a decision
# procedure that is also sufficient, see `strait::brimmest()`.
#
# There is a matching floor -- the least sum of squares that `n` whole numbers
# adding up to `s` can have, i.e. the values as equal as possible. It is
# deliberately not applied here, because it does not depend on the scale at all:
# the near-equal values always lie inside the range, since their mean does.
# Applying it would tighten GRIMMER for every caller, including those who say
# nothing about a scale, which is a separate decision from this one.

sum_squares_scale_max <- function(s, n, val_lower, val_upper) {
  if (s < n * val_lower || s > n * val_upper) {
    return(NULL)
  }

  span <- val_upper - val_lower

  # A scale with a single possible value leaves nothing to spread out:
  if (span == 0) {
    return(n * val_lower^2)
  }

  k <- floor_div(s - n * val_lower, span)
  rest <- (s - n * val_lower) - k * span

  if (rest == 0) {
    k * val_upper^2 + (n - k) * val_lower^2
  } else {
    k * val_upper^2 + (val_lower + rest)^2 + (n - k - 1) * val_lower^2
  }
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
#'   The bounds come from the same internal machinery that [`grim()`] and
#'   [`grimmer()`] use to derive their candidate ranges, so `unround()` accepts
#'   exactly the rounding methods those tests do, and `threshold` and
#'   `symmetric` mean the same thing everywhere.
#'
#'   The five `"ties_*"` methods each name a complete tie-breaking procedure,
#'   so they say by themselves what `rounding` and `symmetric` say together:
#'   `"ties_up"` is `"up"` with `symmetric = FALSE`, `"ties_away"` is `"up"`
#'   with `symmetric = TRUE`, and likewise for `"ties_down"` and `"ties_zero"`.
#'   `"ties_even"` is the odd one out: it is another name for `"even"`, which no
#'   combination of a threshold and `symmetric` produces, since parity is not a
#'   direction. `symmetric` is not consulted for any of them. See
#'   [`round_ties_up()`].
#'
#'   Note that `threshold` applies only to `"up_from"`, `"down_from"`, and
#'   `"up_from_or_down_from"`. The plain `"up"`, `"down"`, and `"up_or_down"`
#'   methods round from a fixed 5 -- see [`round_up()`] -- so their bounds do
#'   not depend on it.
#'
#'   `threshold` moves a range but never widens it: the three `"*_from"` methods
#'   span exactly one step at every threshold, just as `"up"`, `"down"`, and
#'   `"up_or_down"` do at the 5 they are fixed to. Up to scrutiny 1.0.0 they did
#'   not, because `round_down_from()` switched direction at `10 - threshold`
#'   rather than at `threshold`; see that function's `threshold` parameter.
#'
#' Base R's own `round()` (R version >= 4.0.0), referenced by `rounding =
#' "even"`, is reconstructed in the same way as `"up_or_down"`. Whether its
#' boundary values are really inclusive is hard to predict: `round()` breaks
#' midpoint ties by the parity of the preceding digit, and whether a tie occurs
#' at all depends on the binary representation of the value. Both bounds are
#' therefore reported as inclusive, which can only make a reconstructed range
#' too wide, never too narrow. That is the safe direction for error detection.

#' @param x String or numeric. Rounded number. `x` must be a string unless
#'   `digits` is specified (most likely by a function that uses `unround()` as a
#'   helper).
#' @param rounding String. Rounding method presumably used to create `x`.
#'   Default is `"up_or_down"`. For more, see section `Rounding`.
#' @param threshold Numeric. The point within a step at which rounding switches
#'   direction, in tenths of a step, for the `"up_from"`, `"down_from"`, and
#'   `"up_from_or_down_from"` methods; it must be greater than `0` and less than
#'   `10`. Other rounding methods are not affected. Default is `5`, which makes
#'   those three methods the same as `"up"`, `"down"`, and `"up_or_down"`. See
#'   [`round_up_from()`], which spells out what it means in each direction.
#' @param digits Integer. This argument is meant to make `unround()` more
#'   efficient to use as a helper function so that it doesn't need to
#'   redundantly count decimal places. Don't specify it otherwise. Default is
#'   `NULL`, in which case decimal places really are counted internally and `x`
#'   must be a string.
#' @param symmetric Logical. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers with `"up"`, `"down"`, `"up_from"`, or `"down_from"`
#'   mirrored that of positive numbers, so that their absolute values were
#'   always equal. Default is `FALSE`. It only ever affects negative numbers,
#'   and with `"up"` and `"down"` only ties among those; `TRUE` is what
#'   reconstructs Excel, SAS, SPSS, and Matlab. See
#'   `vignette("rounding-options")`.
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

unround <- function(
  x,
  rounding = "up_or_down",
  threshold = 5,
  digits = NULL,
  symmetric = FALSE
) {
  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(x, rounding))

  # The other arguments are vectorized as well, and they need the same length
  # check: with only `x` and `rounding` checked, a `digits` that was shorter
  # than `x` was recycled without a word, and the extra `x` values silently got
  # the wrong number of decimal places -- and hence the wrong bounds. They get
  # no pairing warning, though. One `digits` value per `x` value is the ordinary
  # way to call the function from a helper, not the confusing pairing of numbers
  # with rounding methods that the warning above is about.
  check_lengths_congruent(
    list(x, rounding, digits, threshold, symmetric),
    warn = FALSE
  )

  # The number of decimal places might be given from within another function via
  # the `digits` argument. Otherwise -- if `digits` is not specified, and
  # therefore `NULL` -- the `x` argument must be a string so that decimal places
  # can be counted accurately (cf. trailing zeros), which is then done:
  if (is.null(digits)) {
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

  # Every argument is vectorized, and they may have different lengths -- a
  # single `x` with five `digits` values is as meaningful as the reverse.
  # Recycle them all to a common length so that each row of the output describes
  # one complete combination. (Before this was done explicitly, the output
  # tibble kept the length of `x` as its row count while its columns took
  # whatever length `paste0()` recycling produced, which could yield a malformed
  # tibble.)
  lengths_in <- c(
    length(x_num),
    length(rounding),
    length(digits),
    length(threshold),
    length(symmetric)
  )

  # Recycling stops at zero: if any argument is empty, there is no complete
  # combination to describe, so the output has no rows. Taking the maximum alone
  # ignored the empty argument and let the length-1 defaults set the row count,
  # so `unround(character(0))` returned one row of missing values -- a phantom
  # result where an empty input should pass through as an empty output:
  n_out <- if (any(lengths_in == 0L)) 0L else max(lengths_in)

  recycle <- function(value) rep_len(value, n_out)
  x_out <- recycle(x)
  x_num <- recycle(x_num)
  rounding <- recycle(rounding)
  digits <- recycle(digits)
  threshold <- recycle(threshold)
  symmetric <- recycle(symmetric)

  # Determine the boundary values and whether they are inclusive, going by the
  # `rounding` argument. `bound_numerators()` is the same helper that GRIM and
  # GRIMMER derive their candidate ranges from, so all three tests now agree on
  # what the bounds of a rounded number are, on which rounding methods exist,
  # and on what `threshold` and `symmetric` mean. It expresses each bound as an
  # exact integer numerator over a common denominator; dividing recovers the
  # boundary value itself:
  bounds <- lapply(seq_len(n_out), function(i) {
    bound_numerators(
      x_num = x_num[i],
      digits = digits[i],
      rounding = rounding[i],
      threshold = threshold[i],
      symmetric = symmetric[i]
    )
  })

  # `bound_numerators()` returns `NULL` where the bounds are undefined, which is
  # the case for a missing `x`:
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

  sign_lower <- ifelse(incl_lower, "<=", "<")
  sign_upper <- ifelse(incl_upper, "<=", "<")

  # Return a tibble that displays the range with its appropriate signs and
  # includes all the results that constitute the range
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
