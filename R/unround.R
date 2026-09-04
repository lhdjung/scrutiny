# Exact candidate-sum arithmetic ------------------------------------------

# GRIM and GRIMMER both need the set of integer sums `s` for which `s / n_items`
# would have been rounded to the reported mean. Deriving it from floating-point
# products such as `floor(upper * n_items)` is unsafe: where such a product is
# mathematically an exact integer, its `double` can fall on either side of it,
# dropping a legitimate sum or admitting a phantom one, and the verdict flips
# (#86).
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

# `floor_div()` and `ceiling_div()` divide `a` by `b` (with `b > 0`), rounding
# towards `-Inf` and `+Inf`. Unlike `floor(a / b)`, they are exact for
# integer-valued `a` and `b`: the candidate result is multiplied back out, which
# is exact in double precision. The error in `a / b` is far below 1, so one
# correction step suffices.

floor_div <- function(a, b) {
  q <- floor(a / b)
  # The comparisons below would fail on a missing value rather than pass it on.
  # Every caller propagates the `NA` up to the verdict:
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


# Same idea for a sum of two fractions, `a1 / b1 + a2 / b2` (`b1`, `b2` positive
# integers): the floor of the sum, plus a flag for whether the sum is an integer,
# which callers need to honor an exclusive bound. Splitting each fraction into
# integer part and remainder first keeps every product below `2 * b1 * b2`;
# putting both over `b1 * b2` directly would leave the exact range far sooner.

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


# Integer offsets of the lower and upper rounding bounds from `x_num`, in units
# of `1 / 10^(digits + 1)`, plus each bound's inclusivity. This is the single
# source of truth for rounding bounds: `unround()`, `grim()`, and `grimmer()`
# all derive their ranges from it. The offsets follow the table in the
# `Rounding` section of `unround()`'s documentation, plus the compound methods,
# whose bounds are the union of their constituents' (a single interval, since
# both constituents include `x_num`).
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
# all `NA` if `x_num` is missing, or `NULL` if `rounding` is unknown.

rounding_offsets <- function(rounding, threshold, x_num, symmetric = FALSE) {
  check_rounding_spec_singular(rounding, threshold, symmetric)

  # The branches below need a sign, and a missing value has none. Standing in a
  # positive number keeps `rounding` validated the way it is for any other
  # value; the offsets it yields are discarded at the end:
  x_missing <- is.na(x_num)
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
# which happens only for a missing `x_num`, and errors on an unknown `rounding`.

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


# The sum of squares of the item-level values is
#
#   ((n - 1) * sd^2 + n * (s / (n * items))^2) * items^2 ==
#     (n - 1) * sd^2 * items^2 + s^2 / n
#
# and with `sd` given as `num / denom`, both terms are exact rationals. The first
# does not depend on the candidate sum `s`, so it is pre-computed once per SD
# bound, outside GRIMMER's loop, split into an integer part and a proper
# fraction to keep the products small. Returns that integer part plus the
# numerator and denominator of the fraction.

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


# What known scale bounds add to GRIMMER, where `sum_squares_range()` gives what
# the reported SD admits: the greatest sum of squares that `n` whole-number
# respondent totals between `val_lower` and `val_upper` adding up to `s` can
# have. They are as far apart as the scale allows, so `k` sit at the maximum and
# the rest at the minimum, with at most one in between to absorb the remainder.
# In SD space this is the mean-conditional ceiling that Mestdagh et al. (2018)
# call "Structure S" (`strait::sd_bounds()`); here it stays in sum-of-squares
# space, where GRIMMER already works and the arithmetic is exact.
#
# Returns `NULL` if no set of `n` values within the range adds up to `s`, in
# which case the candidate sum is out of reach whatever the SD is.
#
# The bound is exact, but necessary rather than sufficient: not every sum of
# squares below it is attainable (with `n = 3` values from 0 to 10 adding up to
# 10, the ceiling is 100, yet 40 is out of reach). So it can only move a verdict
# from `TRUE` to `FALSE`. For a sufficient decision procedure, see
# `strait::brimmest()`.
#
# The matching floor is deliberately not applied: it does not depend on the
# scale at all, so it would tighten GRIMMER for every caller, including those
# who say nothing about a scale. That is a separate decision.

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
#'   reconstructs Excel, SAS, SPSS, and Matlab.
#'
#'   It must not be given with any of the `"ties_*"` methods, which already
#'   name a complete tie-breaking procedure; see [`reround()`]. Because
#'   `rounding` is vectorized here, a single `"ties_*"` element is enough to
#'   make a `symmetric` of `TRUE` an error for the whole call. See
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

  # Without `digits`, the decimal places are counted from `x`, which must then
  # be a string so that trailing zeros survive:
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
