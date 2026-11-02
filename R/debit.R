# Helpers to check input ranges (not exported) ----------------------------

check_debit_inputs <- function(input, type, symbol) {
  # Plain comparisons rather than `dplyr::between()`: this runs twice per row,
  # and the vctrs overhead of `between()` would dominate a `debit_map()` call.
  input_in_range <- input >= 0 & input <= 1

  # Anything outside that range is an error. Missing values are not offenders:
  # they are undecidable, not out of range, and the tests return `NA` for them.
  offenders <- input[!is.na(input_in_range) & !input_in_range]

  if (length(offenders) > 0L) {
    # Running inside `debit_scalar()`, this usually sees a single value, where
    # counting offenders out of a total says nothing:
    if (length(input) == 1L) {
      abort_in_export(
        "!" = "DEBIT only works with binary summary data.",
        "!" = "Binary {type} (`{symbol}`) values must range from 0 to 1.",
        "x" = "`{symbol}` is {offenders}."
      )
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

    abort_in_export(
      "!" = "DEBIT only works with binary summary data.",
      "!" = "Binary {type} (`{symbol}`) values must range from 0 to 1.",
      "x" = "{length(offenders_all)} out of {length(input)} \\
      `{symbol}` values {msg_is_are} not in that \\
      range{msg_offenders} {offenders}."
    )
  }
}


check_debit_inputs_all <- function(x, sd) {
  check_debit_inputs(input = x, type = "mean", symbol = "x")
  check_debit_inputs(input = sd, type = "standard deviation", symbol = "sd")
}


# Single-case implementation ----------------------------------------------

# Single-case function behind `debit()` and `debit_map()`.
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
# `bound_numerators()` supplies both ranges, as it does for `grim_scalar()` and
# `grimmer_scalar()`, so all three tests agree on the bounds of a rounded
# number. Each bound is an exact integer numerator over a common denominator,
# which makes the comparisons exact rather than tolerance-based.

#' @include utils.R sd-binary.R

# Every undecidable DEBIT case returns in the same shape: a bare `NA`, or --
# under `show_rec` -- a full row with `NA` in every reconstructed slot. The
# three call sites are a missing value, an `n` that cannot describe a sample,
# and undefined rounding bounds.
debit_undecidable <- function(show_rec) {
  if (show_rec) {
    list(
      NA,
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
#   num^2 * n * (n - 1) <= spread * denom^2 # nolint
#
# The population SD has `n * n` in place of `n * (n - 1)`, and an SE has
# `n * n * (n - 1)`; see `sd_denominator()`.
#
# It compares whole numbers, as in `sum_range()`, and it is exact while those
# stay below `2^53`, i.e., for `n` up to about 90,000 with an SD reported to two
# decimal places, or 9,000 with three. The products for an SE are those for the
# SD it implies, so the same holds for SEs that binary data can have. Beyond
# that, it degrades to plain floating point.
#
# The spread is the same for `k` and `n - k`, and it rises with `k` up to half
# of `n`, so the test needs no loop over `k`, and its cost does not grow with
# `n`.
binary_sd_attainable <- function(
  sum_lower,
  sum_upper,
  n,
  bounds_sd,
  sd_meaning
) {
  if (sum_lower > sum_upper) {
    return(FALSE)
  }

  spread <- binary_spread_range(n, bounds_sd, sd_meaning)
  spread_min <- spread[1L]
  spread_max <- spread[2L]

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
  root <- binary_spread_root(n, spread_min)
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


# The smaller root of `k * (n - k) = spread`, or half of `n` if no `k` has a
# spread this large. Shared with `debit_explain()`.
binary_spread_root <- function(n, spread) {
  (n - sqrt(max(n^2 - 4 * spread, 0))) / 2
}


# The least and the greatest spread `k * (n - k)` that the reported SD admits,
# as whole numbers; see `binary_sd_attainable()`. Shared with `debit_explain()`.
binary_spread_range <- function(n, bounds_sd, sd_meaning) {
  factor_sd <- n * sd_denominator(n, sd_meaning)
  denom_squared <- bounds_sd$denom^2
  lower_scaled <- bounds_sd$lower^2 * factor_sd
  upper_scaled <- bounds_sd$upper^2 * factor_sd

  spread_min <- ceiling(lower_scaled / denom_squared)
  lower_bound_hit <- spread_min * denom_squared == lower_scaled
  if (!bounds_sd$incl_lower && lower_bound_hit) {
    spread_min <- spread_min + 1
  }

  spread_max <- floor(upper_scaled / denom_squared)
  upper_bound_hit <- spread_max * denom_squared == upper_scaled
  if (!bounds_sd$incl_upper && upper_bound_hit) {
    spread_max <- spread_max - 1
  }

  c(spread_min, spread_max)
}


# The `"mean_n"` DEBIT test. It treats the mean as continuous, anywhere in its
# rounding range, and asks whether the SD that follows from some such mean
# lies in the SD's rounding range.
#
# The SD of binary data with mean `m` is the square root of `n / n_dev` times
# the "spread" `m * (1 - m)` (`sd_denominator()`), which rises up to a mean of
# 1/2 and falls after it. As `m` runs through its range, the spread runs
# through an interval: between the spreads at the two bounds of the mean, up
# to 1/4 if 1/2 lies strictly inside the range. A bound that the mean's range
# excludes is not attained, so the spread at it is not either -- every value
# arbitrarily close to it is, which is what makes the end of the interval
# open. The test is then whether this interval meets the range of the squared
# SD. In whole numbers: the spread at a mean `num / denom` is `num * (denom -
# num)` over `denom^2`, and the comparison with a squared SD bound `(num_sd /
# denom_sd)^2 * n_dev / n` is one of cross-multiplied whole numbers, exact
# while they stay below `2^53`, as in `binary_sd_attainable()`.
#
# Evaluating the SD at the bounds and rounding it, as before, treated an
# excluded bound as attained: `x = 0.03` under `"ceiling"` means a mean above
# 0.02, whose population SD at `n = 10` is above 0.14 and rounds to 0.15, but
# the SD at 0.02 itself is 0.14 and was taken for a match.
binary_sd_attainable_mean_n <- function(bounds_x, n, bounds_sd, sd_meaning) {
  denom_x <- bounds_x$denom

  # The spreads at the bounds of the mean, over `denom_x^2`, with the
  # inclusivity of the bound each comes from:
  spreads <- c(
    bounds_x$lower * (denom_x - bounds_x$lower),
    bounds_x$upper * (denom_x - bounds_x$upper)
  )
  incl <- c(bounds_x$incl_lower, bounds_x$incl_upper)

  if (spreads[[1L]] == spreads[[2L]]) {
    # The two bounds have the same spread, so the interval starts and ends
    # there, attained if either bound is:
    spread_min <- spreads[[1L]]
    incl_min <- any(incl)
    spread_max <- spread_min
    incl_max <- incl_min
  } else {
    index_min <- which.min(spreads)
    index_max <- which.max(spreads)
    spread_min <- spreads[[index_min]]
    incl_min <- incl[[index_min]]
    spread_max <- spreads[[index_max]]
    incl_max <- incl[[index_max]]
  }

  # A mean of 1/2 strictly inside the range has the greatest spread of all,
  # 1/4, and it is attained. `denom_x` is a multiple of 10, so `denom_x / 2`
  # is whole:
  half <- denom_x / 2
  if (bounds_x$lower < half && half < bounds_x$upper) {
    spread_max <- half^2
    incl_max <- TRUE
  }

  # `spread / denom_x^2 * n / n_dev` against `(num_sd / denom_sd)^2`, both
  # sides multiplied by `denom_x^2 * n_dev * denom_sd^2`:
  n_dev <- sd_denominator(n, sd_meaning)
  factor_spread <- n * bounds_sd$denom^2
  factor_sd <- n_dev * denom_x^2
  attained_min <- spread_min * factor_spread
  attained_max <- spread_max * factor_spread
  sd_min <- bounds_sd$lower^2 * factor_sd
  sd_max <- bounds_sd$upper^2 * factor_sd

  # Two intervals meet unless one ends before the other begins. A shared end
  # counts only if both include it:
  (attained_min < sd_max ||
    (attained_min == sd_max && incl_min && bounds_sd$incl_upper)) &&
    (sd_min < attained_max ||
      (sd_min == attained_max && bounds_sd$incl_lower && incl_max))
}


# The rounding bounds of the reported mean and SD, confined to what binary data
# can have. Clamping only ever narrows a range, and a bound moved this way is
# attainable, hence inclusive. Shared with `debit_explain()`, so that its text
# shows the bounds that decided the test.
debit_bounds <- function(
  x,
  sd,
  digits_x,
  digits_sd,
  rounding,
  threshold,
  symmetric
) {
  bounds_x <- roundwork::bound_numerators(
    x,
    digits_x,
    rounding,
    threshold,
    symmetric
  )
  bounds_sd <- roundwork::bound_numerators(
    sd,
    digits_sd,
    rounding,
    threshold,
    symmetric
  )

  if (is.null(bounds_x) || is.null(bounds_sd)) {
    return(NULL)
  }

  # A mean of binary data cannot lie outside 0 and 1, however the rounding
  # bounds fall. Otherwise, a mean reported as 0.00 would have a bound below 0,
  # `sd_binary_mean_n()` would give `NaN`, and `debit(x = 0, sd = 0, n = 50)`
  # would be `NA`, though every value is 0.
  if (bounds_x$lower < 0) {
    bounds_x$lower <- 0
    bounds_x$incl_lower <- TRUE
  }
  if (bounds_x$upper > bounds_x$denom) {
    bounds_x$upper <- bounds_x$denom
    bounds_x$incl_upper <- TRUE
  }

  # An SD cannot be negative, so a negative lower bound is really a bound of
  # zero. Same as in `grimmer_scalar()`:
  if (bounds_sd$lower < 0) {
    bounds_sd$lower <- 0
    bounds_sd$incl_lower <- TRUE
  }

  list(x = bounds_x, sd = bounds_sd)
}


debit_scalar <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  show_rec = FALSE
) {
  # `arg_match_in_export()` returns at once for a valid string or the default,
  # which matters here: this runs once per row. The constants are the defaults:
  formula <- arg_match_in_export(formula, DEBIT_FORMULAS)
  sd_meaning <- arg_match_in_export(sd_meaning, SD_MEANINGS)
  check_flag(show_rec)

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

  check_newly_numeric(x, digits_x)
  check_newly_numeric(sd, digits_sd)

  check_debit_inputs_all(x, sd)

  # As in `grimmer_sd_sample_or_pop()`, the verdict is the population SD's
  # unless the sample SD passes. The bounds and sums of `show_rec` don't depend
  # on `sd_meaning`, except that the sample SD leaves them `NA` at `n = 1`:
  if (sd_meaning == "sd_sample_or_pop") {
    test_type <- function(sd_meaning) {
      debit_scalar(
        x = x,
        sd = sd,
        n = n,
        digits_x = digits_x,
        digits_sd = digits_sd,
        formula = formula,
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric,
        sd_meaning = sd_meaning,
        show_rec = show_rec
      )
    }
    sample <- test_type("sd_sample")
    if (isTRUE(sample[[1L]])) {
      return(sample)
    }
    return(test_type("sd_pop"))
  }

  # A missing value makes the test undecidable, and it is returned in the same
  # shape as the undefined-bounds case below. A missing decimal count counts: no
  # rounding bounds follow from it.
  if (anyNA(c(x, sd, n, digits_x, digits_sd))) {
    return(debit_undecidable(show_rec))
  }

  # A sample SD of `n` binary values divides by `n - 1`, so `n` has to be a
  # whole number greater than 1 for it to describe anything; a population SD
  # needs a positive one. Anything else is undecidable, as in `grim_scalar()`
  # and `grimmer_scalar()` -- not a `FALSE` reached through an `Inf` or a zero:
  if (!is_decidable_n_items(n, min_n = sd_min_n(sd_meaning))) {
    return(debit_undecidable(show_rec))
  }

  bounds <- debit_bounds(
    x,
    sd,
    digits_x,
    digits_sd,
    rounding,
    threshold,
    symmetric
  )

  # Undefined bounds leave the case undecidable, as in `grim_scalar()`:
  if (is.null(bounds)) {
    return(debit_undecidable(show_rec))
  }

  bounds_x <- bounds$x
  bounds_sd <- bounds$sd

  x_lower <- bounds_x$lower / bounds_x$denom
  x_upper <- bounds_x$upper / bounds_x$denom

  sd_lower <- bounds_sd$lower / bounds_sd$denom
  sd_upper <- bounds_sd$upper / bounds_sd$denom

  # The least and the greatest number of ones whose mean `k / n` would be
  # reported as `x`: GRIM's range of sums, confined to the 0 to `n` ones that `n`
  # binary values can have. If there is no such number, `sum_lower` is greater
  # than `sum_upper`.
  check_sum_range_exact(x, n, digits_x)
  sums <- roundwork::sum_range(x, n, digits_x, rounding, threshold, symmetric)
  sum_lower <- max(sums[1L], 0)
  sum_upper <- min(sums[2L], n)

  consistency <- switch(
    formula,
    "exact" = binary_sd_attainable(
      sum_lower = sum_lower,
      sum_upper = sum_upper,
      n = n,
      bounds_sd = bounds_sd,
      sd_meaning = sd_meaning
    ),
    "mean_n" = binary_sd_attainable_mean_n(
      bounds_x = bounds_x,
      n = n,
      bounds_sd = bounds_sd,
      sd_meaning = sd_meaning
    ),
    cli::cli_abort(
      "Internal error: incorrect `formula` value \"{formula}\".",
      .internal = TRUE
    )
  )

  if (!show_rec) {
    return(consistency)
  }

  # The reconstructed numbers, in the order of the output columns of
  # `debit_map()`:
  list(
    consistency,
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
#' @description `debit()` checks whether the reported mean and standard
#'   deviation of binary data are consistent with the reported sample size.
#'   With `k` ones among `n` values, the mean is `k / n` and the SD follows
#'   from `k` and `n`; the test asks whether any `k` produces both reported
#'   numbers at their decimal places.
#'
#'   The function is vectorized, but [`debit_map()`] is more convenient for
#'   testing many cases.
#'
#' @details `x`, `sd`, `n`, `digits_x`, and `digits_sd` are vectorized and
#'   recycled to a common length by the tidyverse rules. All other arguments
#'   must have length 1.
#'
#' @param x Numeric. The reported mean of the binary data, between 0 and 1.
#' @param sd Numeric. The reported standard deviation, between 0 and 1.
#' @param n Integer. The reported sample size.
#' @param digits_x,digits_sd Integer. The number of decimal places in `x` and
#'   `sd`, including trailing zeros. There are no defaults, since a numeric
#'   value does not keep them (`1.40` is `1.4`).
#' @param formula String. `"exact"` (the default) only considers means that
#'   `n` binary values can have, i.e., `k / n` for a whole number `k`.
#'   `"mean_n"` treats the mean as continuous, as in Heathers and Brown
#'   (2019), and accepts some SDs that no binary sample of size `n` can have:
#'   `debit(0.05, 0.21, 20, 2, 2)` is `TRUE` under `"mean_n"`, although a mean
#'   of 0.05 with `n = 20` means one 1 in 20, whose SD rounds to 0.22.
#' @param rounding String. The rounding method assumed to have produced `x`
#'   and `sd`. Default is `"up_or_down"`. See
#'   `vignette("rounding-options", package = "roundwork")`.
#' @param threshold,symmetric The same as in [`grim()`].
#' @param sd_meaning String. What `sd` really is: the sample SD,
#'   `"sd_sample"` (the default); the population SD, `"sd_pop"`; either one,
#'   `"sd_sample_or_pop"`; or the standard error of the mean, `"se"`. As in
#'   [`grimmer()`], where the section *SD meaning* has details.
#'
#' @export
#'
#' @return Logical. `TRUE` if `x`, `sd`, and `n` are consistent, `FALSE` if
#'   not, and `NA` if the case cannot be decided: if a value is missing or if
#'   `n` is not a positive whole number. A sample SD divides by `n - 1`, so
#'   under `sd_meaning = "sd_sample"` (the default) and `"se"`, `n` must also
#'   be at least 2.
#'
#' @seealso [`debit_map()`] for data frames.
#'
#' @references Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A
#'   Simple Consistency Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @examples
#' debit(x = 0.36, sd = 0.11, n = 20, digits_x = 2, digits_sd = 2)

# The signature mirrors `debit_scalar()`'s minus `show_rec`; see
# `map_value_sets()`.
debit <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
) {
  map_value_sets(
    fn = debit_scalar,
    along = c("x", "sd", "n", "digits_x", "digits_sd")
  )
}
