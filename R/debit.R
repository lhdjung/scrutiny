# Helpers to check input ranges (not exported) ----------------------------

check_debit_inputs <- function(input, type, symbol) {
  # For all input values, check if they are between 0 and 1. Plain comparisons
  # rather than `dplyr::between()`: this runs twice per row on a single value,
  # and the vctrs machinery behind `between()` was two thirds of a `debit_map()`
  # call.
  input_in_range <- input >= 0 & input <= 1

  # Anything outside that range is an error. Missing values are not offenders:
  # they are undecidable, not out of range, and the tests return `NA` for them.
  offenders <- input[!is.na(input_in_range) & !input_in_range]

  if (length(offenders) > 0L) {
    # Running inside `debit_scalar()`, this usually sees a single value, where
    # counting offenders out of a total says nothing:
    if (length(input) == 1L) {
      cli::cli_abort(c(
        "!" = "DEBIT only works with binary summary data.",
        "!" = "Binary {type} (`{symbol}`) values must range from 0 to 1.",
        "x" = "`{symbol}` is {offenders}."
      ))
    }

    if (length(offenders) == 1L) {
      msg_is_are <- "is"
    } else {
      msg_is_are <- "are"
    }

    offenders_all <- offenders

    if (length(offenders) > 3L) {
      offenders <- offenders[1:3]
      msg_offenders <- ", starting with"
    } else {
      msg_offenders <- ":"
    }

    # ...and second, the actual error is thrown:
    cli::cli_abort(c(
      "!" = "DEBIT only works with binary summary data.",
      "!" = "Binary {type} (`{symbol}`) values must range from 0 to 1.",
      "x" = "{length(offenders_all)} out of {length(input)} \\
      `{symbol}` values {msg_is_are} not in that \\
      range{msg_offenders} {offenders}."
    ))
  }
}


# Building up on the above, the following function is used within
# `debit_scalar()` to check the numeric range of its `x` and `sd` inputs:
check_debit_inputs_all <- function(x, sd) {
  check_debit_inputs(input = x, type = "mean", symbol = "x")
  check_debit_inputs(input = sd, type = "standard deviation", symbol = "sd")
}


# Single-case implementation ----------------------------------------------

# Not exported, but used as a basis for the vectorized `debit()` as well as
# within `debit_map()`.
#
# DEBIT asks whether the SD that follows from the reported mean of binary data
# can be rounded to the reported SD. Both reported values stand for a range of
# original values. Under `formula = "exact"`, the test asks whether some number
# of ones `k` has a mean `k / n` in the mean's range and an SD in the SD's range;
# see `binary_sd_attainable()`. Under `"mean_n"`, it treats the mean as
# continuous, reconstructs the SD at the bounds of its range, rounds the results
# the same way the reported SD was presumably rounded, and checks whether the
# reported SD's own range is met.
#
# `bound_numerators()` supplies both ranges. It is the same helper that
# `grim_scalar()` and `grimmer_scalar()` derive their candidate ranges from, so
# all three tests agree on the bounds of a rounded number, on which rounding
# methods exist, and on what `threshold` and `symmetric` mean. It expresses each
# bound as an exact integer numerator over a common denominator, which is what
# allows the comparison below to be exact rather than tolerance-based.

#' @include utils.R sd-binary.R round.R unround.R reround.R

# Every undecidable DEBIT case returns in the same shape: a bare `NA`, or --
# under `show_rec` -- a full row with `NA` in every reconstructed slot. The
# three call sites are a missing value, an `n` that cannot describe a sample,
# and undefined rounding bounds.
debit_undecidable <- function(show_rec, rounding) {
  if (show_rec) {
    list(
      NA,
      rounding,
      NA_real_,
      NA,
      NA_real_,
      NA,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_
    )
  } else {
    NA
  }
}


# Does a binary sample of size `n` with between `sum_lower` and `sum_upper` ones
# have an SD within `bounds_sd`? This is the `"exact"` DEBIT test.
#
# The sample SD of `k` ones among `n` values is the square root of `k * (n - k)`
# over `n * (n - 1)`, so it is enough to compare `k * (n - k)` -- the "spread"
# below -- with the squared SD bounds. For a bound `num / denom`, the condition
# `num / denom <= SD` is equivalent to this one:
#
#   num^2 * n * (n - 1) <= spread * denom^2
#
# It compares whole numbers, as in `sum_range()`, and it is exact while those
# stay below `2^53`, i.e., for `n` up to about 90,000 with an SD reported to two
# decimal places, or 9,000 with three. Beyond that, it degrades to plain
# floating point.
#
# The spread is the same for `k` and `n - k`, and it rises with `k` up to half
# of `n`, so the test needs no loop over `k`, and its cost does not grow with
# `n`.
binary_sd_attainable <- function(sum_lower, sum_upper, n, bounds_sd) {
  if (sum_lower > sum_upper) {
    return(FALSE)
  }

  # The least and the greatest spread that the reported SD admits:
  factor_sd <- n * (n - 1)
  denom_squared <- bounds_sd$denom^2
  lower_scaled <- bounds_sd$lower^2 * factor_sd
  upper_scaled <- bounds_sd$upper^2 * factor_sd

  spread_min <- ceiling_div(lower_scaled, denom_squared)
  lower_bound_hit <- spread_min * denom_squared == lower_scaled
  if (!bounds_sd$incl_lower && lower_bound_hit) {
    spread_min <- spread_min + 1
  }

  spread_max <- floor_div(upper_scaled, denom_squared)
  upper_bound_hit <- spread_max * denom_squared == upper_scaled
  if (!bounds_sd$incl_upper && upper_bound_hit) {
    spread_max <- spread_max - 1
  }

  # Fold the range of `k` onto the lower half, `j = min(k, n - k)`, where the
  # spread rises with `j`. Folding a range that straddles the middle gives a
  # range that ends there:
  half <- floor(n / 2)

  if (sum_upper <= half) {
    j_lower <- sum_lower
    j_upper <- sum_upper
  } else if (sum_lower >= n - half) {
    j_lower <- n - sum_upper
    j_upper <- n - sum_lower
  } else {
    j_lower <- min(sum_lower, n - sum_upper)
    j_upper <- half
  }

  # The least `j` in that range whose spread reaches `spread_min`. It is the
  # smaller root of the quadratic `j * (n - j) = spread_min`, rounded up. If no
  # spread is large enough, the root is half of `n`, and `j` ends up above
  # `j_upper`.
  root <- (n - sqrt(max(n^2 - 4 * spread_min, 0))) / 2
  j <- max(ceiling(root), j_lower)

  # The loops below only correct floating-point error in `root`, and where the
  # arithmetic is exact, they never run. `root` is a whole number `j` only if
  # `spread_min` is `j * (n - j)`, and the square root is then that of a perfect
  # square, which is exact. Any other root lies at least about `1 / (4 * n)` from
  # the nearest whole number, much further than floating-point error moves it.
  # Past `2^53`, the loops correct an overshoot in either direction.
  while (j > j_lower && (j - 1) * (n - j + 1) >= spread_min) {
    j <- j - 1
  }
  while (j <= j_upper && j * (n - j) < spread_min) {
    j <- j + 1
  }

  # Every greater `j` has a greater spread, so if this one is too large, all of
  # them are:
  j <= j_upper && j * (n - j) <= spread_max
}


# The `"mean_n"` DEBIT test. It treats the mean as continuous, between `x_lower`
# and `x_upper`, and asks whether the SD that follows from some mean in that
# range could have been rounded to the reported SD.
binary_sd_attainable_mean_n <- function(
  x_lower,
  x_upper,
  n,
  bounds_sd,
  digits_sd,
  rounding,
  threshold,
  symmetric
) {
  # The SD is reconstructed at the bounds of the mean's range. The two bounds
  # alone are not enough: the verdict below reasons from them to every mean in
  # between, which needs the reconstruction to be monotonic in the mean -- and
  # it isn't. `sd_binary_mean_n()` is `sqrt((n / (n - 1)) * mean * (1 - mean))`,
  # a downward parabola peaking at a mean of 0.5, so an interval containing 0.5
  # reaches SDs *above* both endpoints. At a mean of exactly 0.50 both endpoints
  # even give the same SD, collapsing the attainable band to a point. The peak
  # is the only interior extremum, so adding it where it falls inside the
  # interval restores the span and makes the step below sound:
  x_eval <- c(x_lower, x_upper)

  if (x_lower <= 0.5 && 0.5 <= x_upper) {
    x_eval <- c(x_eval, 0.5)
  }

  # Reconstruct the SD from each of those means...
  sd_rec <- sd_binary_mean_n(x_eval, n)

  # ...and round it the same way the reported SD was presumably rounded, to the
  # same number of decimal places:
  sd_rec <- reround(
    x = sd_rec,
    digits = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # Do the reconstructed SDs meet the range of the reported SD? `reround()`
  # returned values on the `digits_sd` decimal grid, so multiplying by the
  # bounds' denominator and rounding recovers their exact numerators over that
  # denominator -- the comparison below is between integers, not a tolerance
  # fudge (#86).
  num_rec <- round(sd_rec * bounds_sd$denom)

  above_lower <- if (bounds_sd$incl_lower) {
    num_rec >= bounds_sd$lower
  } else {
    num_rec > bounds_sd$lower
  }

  below_upper <- if (bounds_sd$incl_upper) {
    num_rec <= bounds_sd$upper
  } else {
    num_rec < bounds_sd$upper
  }

  # The two conditions need not be met by the same reconstructed value: with one
  # below the range and another above it, some mean in between reconstructs into
  # it. This intermediate-value argument holds because `x_eval` spans the
  # attainable range -- hence the peak added to it.
  any(above_lower) && any(below_upper)
}


debit_scalar <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  formula = "exact",
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  show_rec = FALSE
) {
  # A plain comparison rather than `rlang::arg_match()`, which would cost more
  # than the rest of the input checks on every row:
  if (!identical(formula, "exact") && !identical(formula, "mean_n")) {
    cli::cli_abort(c(
      "`formula` must be \"exact\" or \"mean_n\".",
      "x" = "It is {wrong_spec_string(formula)}."
    ))
  }

  # See `grim_scalar()`:
  if (!is.numeric(n) && !is.na(n)) {
    check_type(n, c("double", "integer"))
  }

  if (missing(digits_x)) {
    error_digits_missing(x)
  }

  if (missing(digits_sd)) {
    error_digits_missing(sd)
  }

  # The same input validation that `grim_scalar()` and `grimmer_scalar()` run.
  # DEBIT used to accept strings here because it counted decimal places itself;
  # it now takes them from `digits_x` and `digits_sd`, as the other tests do:
  check_newly_numeric(x, digits_x)
  check_newly_numeric(sd, digits_sd)

  # Check whether `x` and `sd` range from 0 to 1:
  check_debit_inputs_all(x, sd)

  # A missing value makes the test undecidable, and it is returned in the same
  # shape as the undefined-bounds case below. A missing decimal count counts: no
  # rounding bounds follow from it.
  if (anyNA(c(x, sd, n, digits_x, digits_sd))) {
    return(debit_undecidable(show_rec, rounding))
  }

  # DEBIT reconstructs the *sample* SD of `n` binary values, so it divides by
  # `n - 1`, and `n` has to be a whole number greater than 1 for that to
  # describe anything. Anything else is undecidable, as in `grim_scalar()` and
  # `grimmer_scalar()` -- not a `FALSE` reached through an `Inf` or a zero:
  if (!is_decidable_n_items(n, min_n = 2)) {
    return(debit_undecidable(show_rec, rounding))
  }

  bounds_x <- bound_numerators(
    x_num = x,
    digits = digits_x,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  bounds_sd <- bound_numerators(
    x_num = sd,
    digits = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # The bounds are undefined for a missing value, and consistency is then
  # undecidable, just as it is for `grim_scalar()` and `grimmer_scalar()` in the
  # same situation:
  if (is.null(bounds_x) || is.null(bounds_sd)) {
    return(debit_undecidable(show_rec, rounding))
  }

  # A mean of binary data cannot lie outside 0 and 1, so neither can the
  # original value behind a reported one, however the rounding bounds fall.
  # Clamping only ever narrows the range. Without it, a mean reported as 0.00
  # had a bound just outside, `sd_binary_mean_n()` gave `NaN`, and
  # `debit(x = 0, sd = 0, n = 50)` came out `NA` -- though every value is 0.
  x_lower <- max(bounds_x$lower / bounds_x$denom, 0)
  x_upper <- min(bounds_x$upper / bounds_x$denom, 1)

  # An SD cannot be negative, so a negative lower bound is really a bound of
  # zero -- attainable, hence inclusive. Same as in `grimmer_scalar()`:
  if (bounds_sd$lower < 0) {
    bounds_sd$lower <- 0
    bounds_sd$incl_lower <- TRUE
  }

  sd_lower <- bounds_sd$lower / bounds_sd$denom
  sd_upper <- bounds_sd$upper / bounds_sd$denom

  # The least and the greatest number of ones whose mean `k / n` would be
  # reported as `x`: GRIM's range of sums, confined to the 0 to `n` ones that `n`
  # binary values can have. If there is no such number, `sum_lower` is greater
  # than `sum_upper`.
  sums <- sum_range(x, n, digits_x, rounding, threshold, symmetric)
  sum_lower <- max(sums[1L], 0)
  sum_upper <- min(sums[2L], n)

  consistency <- switch(
    formula,
    "exact" = binary_sd_attainable(
      sum_lower = sum_lower,
      sum_upper = sum_upper,
      n = n,
      bounds_sd = bounds_sd
    ),
    "mean_n" = binary_sd_attainable_mean_n(
      x_lower = x_lower,
      x_upper = x_upper,
      n = n,
      bounds_sd = bounds_sd,
      digits_sd = digits_sd,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    ),
    cli::cli_abort("Internal error: incorrect `formula` value \"{formula}\".")
  )

  if (!show_rec) {
    return(consistency)
  }

  # The reconstructed numbers, in the order of the output columns of
  # `debit_map()`:
  list(
    consistency,
    rounding,
    sd_lower,
    bounds_sd$incl_lower,
    sd_upper,
    bounds_sd$incl_upper,
    x_lower,
    x_upper,
    sum_lower,
    sum_upper
  )
}


#' The DEBIT (descriptive binary) test
#'
#' @description `debit()` tests summaries of binary data for consistency: If the
#'   mean and the sample standard deviation of binary data are given, are they
#'   consistent with the reported sample size?
#'
#'   The function is vectorized, but it is recommended to use [`debit_map()`]
#'   for testing multiple cases.
#'
#'   `x`, `sd`, `n`, `digits_x`, and `digits_sd` are vectorized: they may have
#'   any length, and shorter ones are recycled to the length of the longest, as
#'   long as they have length 1. All other arguments describe the test as a
#'   whole and must have length 1.
#'
#' @param x Numeric. Mean of a binary distribution.
#' @param sd Numeric. Sample standard deviation of a binary distribution.
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default because it cannot be inferred from a
#'   numeric `x`, which has no trailing zeros: both `1.4` and `1.40` are the
#'   number `1.4`, but only the latter has `digits_x = 2`.
#' @param digits_sd Integer. The number of decimal places in `sd`, including
#'   trailing zeros. As with `digits_x`, there is no default, because trailing
#'   zeros don't survive in a numeric value.
#' @param n Integer. Total sample size.
#' @param formula String. `"exact"` (the default) only considers means that `n`
#'   binary values can have, i.e., `k / n` for a whole number `k`. `"mean_n"`
#'   treats the mean as continuous, as in Heathers and Brown (2019), and
#'   accepts some SDs that no binary sample of size `n` can have. For example,
#'   `debit(0.05, 0.21, 20, 2, 2)` is `TRUE` under `"mean_n"`, but a mean of
#'   0.05 with `n = 20` can only be one 1 in 20, whose SD rounds to 0.22. The
#'   difference only ever turns `TRUE` into `FALSE`, with one exception: under
#'   `rounding = "even"`, an SD exactly on a tie, such as 0.25, is taken to be
#'   reportable both ways by `"exact"`, but only the way [`round()`] goes by
#'   `"mean_n"`.
#' @param rounding String. Rounding method or methods to be used for
#'   reconstructing the SD values to which `sd` will be compared. Default is
#'   `"up_or_down"` (from 5). See `vignette("rounding-options")`.
#' @param threshold Integer. If `rounding` is set to `"up_from"`, `"down_from"`,
#'   or `"up_from_or_down_from"`, set `threshold` to the number from which the
#'   reconstructed values should then be rounded up or down. Otherwise
#'   irrelevant. Default is `5`.
#' @param symmetric Logical. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers with `"up"`, `"down"`, `"up_from"`, or `"down_from"`
#'   should mirror that of positive numbers so that their absolute values are
#'   always equal. Default is `FALSE`. It must not be given with any of the
#'   `"ties_*"` methods, which already name a complete tie-breaking procedure;
#'   see [`reround()`].

#' @export
#'
#' @return Logical. `TRUE` if `x`, `sd`, and `n` are mutually consistent,
#'   `FALSE` if not, and `NA` if the case cannot be decided: if any of the
#'   values is missing, if the rounding bounds are undefined, or if `n` is not
#'   a whole number greater than `1`. DEBIT reconstructs the *sample* SD of `n`
#'   binary values, so it divides by `n - 1`.
#'
#' @seealso [`debit_map()`] applies `debit()` to any number of cases at once.
#'
#' @references Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A
#'   Simple Consistency Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @examples
#' # Check single cases of binary
#' # summary data:
#' debit(x = 0.36, sd = 0.11, n = 20, digits_x = 2, digits_sd = 2)

# Vectorized version. The signature mirrors `debit_scalar()`'s minus `show_rec`;
# see `vectorize_test()`:
debit <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  formula = "exact",
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  vectorize_test(
    .fun = debit_scalar,
    .frame = environment(),
    .along = c("x", "sd", "n", "digits_x", "digits_sd")
  )
}
