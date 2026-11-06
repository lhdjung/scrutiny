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

# Every DEBIT result returns in the same shape: a bare verdict, or -- under
# `show_rec` -- a full row with the reconstructed numbers, `NA` where a missing
# value left them unknown. The verdict is `NA` for a missing value that could
# decide the case, an `n` that cannot describe a sample, and undefined rounding
# bounds.
debit_result <- function(
  consistency,
  show_rec,
  bounds_sd = NULL,
  bounds_x = NULL,
  sums = NULL
) {
  if (!show_rec) {
    return(consistency)
  }
  sd_part <- if (is.null(bounds_sd)) {
    list(NA_real_, NA, NA_real_, NA)
  } else {
    list(
      bounds_sd$lower / bounds_sd$denom,
      bounds_sd$incl_lower,
      bounds_sd$upper / bounds_sd$denom,
      bounds_sd$incl_upper
    )
  }
  x_part <- if (is.null(bounds_x)) {
    list(NA_real_, NA_real_, NA_real_, NA_real_)
  } else {
    list(
      bounds_x$lower / bounds_x$denom,
      bounds_x$upper / bounds_x$denom,
      sums[[1L]],
      sums[[2L]]
    )
  }
  c(list(consistency), sd_part, x_part)
}

debit_undecidable <- function(show_rec) {
  debit_result(NA, show_rec)
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
# It compares whole numbers, as in `sum_range()`. In doubles, that is exact
# below `2^53`; the products here can pass it with a large `n`, especially for
# an SE, so they switch to whole numbers of any size where they would (see
# `R/exact-arithmetic.R`).
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

  # Every greater `j` has a greater spread, so if this one is too large, all of
  # them are:
  j <- binary_least_ones(n, spread$min, j_lower, j_upper)
  j <= j_upper && binary_spread_cmp(j, n, spread$max) <= 0
}


# The least `j` from `j_lower` to `j_upper`, both at most half of `n`, whose
# spread `j * (n - j)` reaches `spread`, or `j_upper + 1` if none does.
binary_least_ones <- function(n, spread, j_lower, j_upper) {
  # Up to an `n` of about 1e8, the spreads are below `2^53`, and the root of
  # the quadratic `j * (n - j) = spread`, rounded up, is the answer. The loops
  # below only correct floating-point error in it, and where the arithmetic is
  # exact, they never run: the root is a whole number `j` only if `spread` is
  # `j * (n - j)`, and the square root is then that of a perfect square, which
  # is exact. Any other root lies at least about `1 / (4 * n)` from the nearest
  # whole number, much further than floating-point error moves it.
  if (!is_big(spread) && n^2 < 2^53) {
    j <- max(ceiling(binary_spread_root(n, spread)), j_lower)
    while (j > j_lower && (j - 1) * (n - j + 1) >= spread) {
      j <- j - 1
    }
    while (j <= j_upper && j * (n - j) < spread) {
      j <- j + 1
    }
    return(j)
  }

  # Beyond, the root is too coarse to correct step by step, so the spreads are
  # compared exactly while halving the range:
  low <- j_lower
  high <- j_upper + 1
  while (low < high) {
    middle <- floor((low + high) / 2)
    if (binary_spread_cmp(middle, n, spread) >= 0) {
      high <- middle
    } else {
      low <- middle + 1
    }
  }
  low
}


# The sign of `k * (n - k) - spread`, exactly, for each `k`:
binary_spread_cmp <- function(k, n, spread) {
  if (!is_big(spread) && n^2 < 2^53) {
    return(sign(k * (n - k) - spread))
  }
  vapply(
    k,
    function(k) whole_cmp(whole_mul(k, n - k), spread),
    numeric(1L)
  )
}


# The smaller root of `k * (n - k) = spread`, or half of `n` if no `k` has a
# spread this large. Shared with `debit_explain()`.
binary_spread_root <- function(n, spread) {
  if (is_big(spread)) {
    spread <- big_double(unclass(spread))
  }
  (n - sqrt(max(n^2 - 4 * spread, 0))) / 2
}


# The least and the greatest spread `k * (n - k)` that the reported SD admits,
# as whole numbers; see `binary_sd_attainable()`. A list of `min` and `max`,
# each a double or, from `2^53` on, a `big()` number. A spread beyond the
# greatest that `n` binary values have, about `n^2 / 4`, stands for no `k`, so
# it is lowered to just above that, which keeps the numbers in doubles for an
# `n` up to about 1e8. Shared with `debit_explain()`.
binary_spread_range <- function(n, bounds_sd, sd_meaning) {
  lower <- lowest_terms(bounds_sd$lower, bounds_sd$denom)
  upper <- lowest_terms(bounds_sd$upper, bounds_sd$denom)

  # The common case, all in doubles; the same steps as below:
  factor_sd <- n * sd_denominator(n, sd_meaning)
  root_max <- sqrt(2^53)
  if (
    factor_sd < 2^52 &&
      max(lower$num, lower$den, upper$num, upper$den) < root_max
  ) {
    low <- floor_product_ratio(lower$num^2, factor_sd, lower$den^2)
    high <- floor_product_ratio(upper$num^2, factor_sd, upper$den^2)
    spread_min <- low$floor + (!low$exact || !bounds_sd$incl_lower)
    spread_max <- high$floor - (high$exact && !bounds_sd$incl_upper)
    half <- floor(n / 2)
    spread_greatest <- half * (n - half)
    return(list(
      min = min(spread_min, spread_greatest + 1),
      max = min(spread_max, spread_greatest)
    ))
  }

  # The spread at each bound is `num^2 * n * n_dev / den^2`:
  low <- binary_spread_floor(lower, n, sd_meaning)
  spread_min <- low$floor
  if (!low$exact || !bounds_sd$incl_lower) {
    spread_min <- whole_add(spread_min, 1)
  }

  high <- binary_spread_floor(upper, n, sd_meaning)
  spread_max <- high$floor
  if (high$exact && !bounds_sd$incl_upper) {
    spread_max <- whole_add(spread_max, -1)
  }

  half <- floor(n / 2)
  spread_greatest <- whole_mul(half, n - half)
  if (whole_cmp(spread_min, spread_greatest) > 0) {
    spread_min <- whole_add(spread_greatest, 1)
  }
  if (whole_cmp(spread_max, spread_greatest) > 0) {
    spread_max <- spread_greatest
  }

  list(min = spread_min, max = spread_max)
}


# The floor of `num^2 * n * n_dev / den^2` for an SD bound `num / den`, and
# whether the division is exact. In doubles while that is exact; it is a
# product of four factors, which may pass `2^53` with an `n` of some 1e8, or
# much earlier for an SE, whose `n_dev` is `n * (n - 1)`.
binary_spread_floor <- function(bound, n, sd_meaning) {
  factor_sd <- n * sd_denominator(n, sd_meaning)
  if (factor_sd < 2^52 && bound$num < sqrt(2^53) && bound$den < sqrt(2^53)) {
    return(floor_product_ratio(bound$num^2, factor_sd, bound$den^2))
  }
  n_dev <- switch(
    sd_meaning,
    sd_pop = big(n),
    se = big_mul(big(n), big(n - 1)),
    big(n - 1)
  )
  num_big <- big(bound$num)
  den_big <- big(bound$den)
  division <- big_divmod(
    Reduce(big_mul, list(num_big, num_big, big(n), n_dev)),
    big_mul(den_big, den_big)
  )
  list(
    floor = big_or_double(division$quotient),
    exact = big_cmp(division$remainder, 0) == 0
  )
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
# denom_sd)^2 * n_dev / n` is one of cross-multiplied whole numbers, exact as
# in `binary_sd_attainable()`.
#
# Evaluating the SD at the bounds and rounding it, as before, treated an
# excluded bound as attained: `x = 0.03` under `"ceiling"` means a mean above
# 0.02, whose population SD at `n = 10` is above 0.14 and rounds to 0.15, but
# the SD at 0.02 itself is 0.14 and was taken for a match.
binary_sd_attainable_mean_n <- function(bounds_x, n, bounds_sd, sd_meaning) {
  denom_x <- bounds_x$denom

  # The spreads at the bounds of the mean, over `denom_x^2`, with the
  # inclusivity of the bound each comes from:
  spreads <- list(
    whole_mul(bounds_x$lower, denom_minus(denom_x, bounds_x$lower)),
    whole_mul(bounds_x$upper, denom_minus(denom_x, bounds_x$upper))
  )
  incl <- c(bounds_x$incl_lower, bounds_x$incl_upper)
  comparison <- whole_cmp(spreads[[1L]], spreads[[2L]])

  if (comparison == 0) {
    # The two bounds have the same spread, so the interval starts and ends
    # there, attained if either bound is:
    spread_min <- spreads[[1L]]
    incl_min <- any(incl)
    spread_max <- spread_min
    incl_max <- incl_min
  } else {
    index_min <- if (comparison < 0) 1L else 2L
    index_max <- 3L - index_min
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
    spread_max <- whole_mul(half, half)
    incl_max <- TRUE
  }

  # `spread / denom_x^2 * n / n_dev` against `(num_sd / denom_sd)^2`, both
  # sides multiplied by the denominators. `n / n_dev` is `n / (n - 1)` for a
  # sample SD, 1 for a population SD, and `1 / (n - 1)` for an SE. Each SD
  # bound is in lowest terms, so the two comparisons have their own
  # denominators. `compare_factors()` compares the products exactly:
  ratio_n <- switch(
    sd_meaning,
    sd_pop = c(1, 1),
    se = c(1, n - 1),
    c(n, n - 1)
  )
  lower_sd <- lowest_terms(bounds_sd$lower, bounds_sd$denom)
  upper_sd <- lowest_terms(bounds_sd$upper, bounds_sd$denom)

  # Two intervals meet unless one ends before the other begins. A shared end
  # counts only if both include it:
  min_vs_sd_max <- compare_factors(
    list(spread_min, ratio_n[[1L]], upper_sd$den, upper_sd$den),
    list(upper_sd$num, upper_sd$num, ratio_n[[2L]], denom_x, denom_x)
  )
  sd_min_vs_max <- compare_factors(
    list(lower_sd$num, lower_sd$num, ratio_n[[2L]], denom_x, denom_x),
    list(spread_max, ratio_n[[1L]], lower_sd$den, lower_sd$den)
  )
  (min_vs_sd_max < 0 ||
    (min_vs_sd_max == 0 && incl_min && bounds_sd$incl_upper)) &&
    (sd_min_vs_max < 0 ||
      (sd_min_vs_max == 0 && bounds_sd$incl_lower && incl_max))
}


# `denom - num` for `0 <= num <= denom`. With `digits_x = 15`, `denom` is 1e16,
# a power of ten that a double holds exactly, but the difference may not be:
denom_minus <- function(denom, num) {
  if (denom < 2^53) {
    return(denom - num)
  }
  big_or_double(big_sub(big(denom), big(num)))
}


# The rounding bounds of the reported mean and SD, confined to what binary data
# can have. Clamping only ever narrows a range, and a bound moved this way is
# attainable, hence inclusive. Shared with `debit_explain()`, so that its text
# shows the bounds that decided the test. A missing value, or one with a
# missing decimal count, has `NULL` bounds; so does one whose bounds are
# undefined.
debit_bounds <- function(
  x,
  sd,
  digits_x,
  digits_sd,
  rounding,
  threshold,
  symmetric
) {
  bounds_x <- if (!is.na(x) && !is.na(digits_x)) {
    roundwork::bound_numerators(x, digits_x, rounding, threshold, symmetric)
  }
  bounds_sd <- if (!is.na(sd) && !is.na(digits_sd)) {
    roundwork::bound_numerators(sd, digits_sd, rounding, threshold, symmetric)
  }

  # A mean of binary data cannot lie outside 0 and 1, however the rounding
  # bounds fall. Otherwise, a mean reported as 0.00 would have a bound below 0,
  # `sd_binary_mean_n()` would give `NaN`, and `debit(x = 0, sd = 0, n = 50)`
  # would be `NA`, though every value is 0.
  if (!is.null(bounds_x)) {
    if (bounds_x$lower < 0) {
      bounds_x$lower <- 0
      bounds_x$incl_lower <- TRUE
    }
    if (bounds_x$upper > bounds_x$denom) {
      bounds_x$upper <- bounds_x$denom
      bounds_x$incl_upper <- TRUE
    }
  }

  # An SD cannot be negative, so a negative lower bound is really a bound of
  # zero. Same as in `grimmer_scalar()`:
  if (!is.null(bounds_sd) && bounds_sd$lower < 0) {
    bounds_sd$lower <- 0
    bounds_sd$incl_lower <- TRUE
  }

  list(x = bounds_x, sd = bounds_sd)
}


# With the mean unknown, it may lie anywhere from 0 to 1, and the number of
# ones anywhere from 0 to `n`:
debit_bounds_x_any <- function() {
  list(lower = 0, upper = 10, denom = 10, incl_lower = TRUE, incl_upper = TRUE)
}


debit_scalar <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  data_type,
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

  if (missing(data_type)) {
    error_data_type_missing()
  }

  scale <- rounding_scale(rounding, threshold, symmetric)
  check_newly_numeric(x, digits_x, scale = scale)
  check_newly_numeric(sd, digits_sd, scale = scale)

  check_debit_inputs_all(x, sd)

  if (!identical(data_type, "integer")) {
    verdict <- verdict_continuous(
      n,
      min_n = sd_min_n(sd_meaning),
      sd = sd,
      digits_sd = digits_sd,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )
    return(test_data_type(
      data_type,
      test_integer = function() {
        debit_scalar(
          x = x,
          sd = sd,
          n = n,
          digits_x = digits_x,
          digits_sd = digits_sd,
          data_type = "integer",
          formula = formula,
          rounding = rounding,
          threshold = threshold,
          symmetric = symmetric,
          sd_meaning = sd_meaning,
          show_rec = show_rec
        )
      },
      continuous = debit_result(unname(verdict), show_rec),
      missing = debit_undecidable(show_rec)
    ))
  }

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
        data_type = "integer",
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
    population <- test_type("sd_pop")
    verdict <- combine_sd_sample_or_pop(sample[[1L]], population[[1L]], n)
    if (show_rec) {
      population[[1L]] <- verdict
      return(population)
    }
    return(verdict)
  }

  # A value with its decimal places missing has at least those it shows, and a
  # value set that is inconsistent with these is inconsistent with more; see
  # `digits_least()`. Otherwise, the value is as good as missing, as below,
  # unless the value set passes even with the most. An error is about a size
  # that the test refuses, which leaves the case open:
  digits_x_least <- digits_least(x, digits_x)
  digits_sd_least <- digits_least(sd, digits_sd)
  if (
    (is.na(digits_x) && !is.na(digits_x_least)) ||
      (is.na(digits_sd) && !is.na(digits_sd_least))
  ) {
    at_least <- tryCatch(
      debit_scalar(
        x = x,
        sd = sd,
        n = n,
        digits_x = digits_x_least,
        digits_sd = digits_sd_least,
        data_type = "integer",
        formula = formula,
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric,
        sd_meaning = sd_meaning
      ),
      error = function(cnd) NA
    )
    if (isFALSE(at_least)) {
      return(debit_result(FALSE, show_rec))
    }
    # ...and a value set that is consistent with so many that only the values
    # shown pass is consistent with any; see `digits_exact_mean()`. Not under
    # `"mean_n"`, which has no data behind the SD to be exact about:
    if (
      isTRUE(at_least) &&
        formula == "exact" &&
        is_decidable_n_items(n, min_n = sd_min_n(sd_meaning))
    ) {
      at_most <- tryCatch(
        debit_scalar(
          x = x,
          sd = sd,
          n = n,
          digits_x = if (is.na(digits_x) && !is.na(digits_x_least)) {
            digits_exact_mean(digits_x_least, as.numeric(n))
          } else {
            digits_x
          },
          digits_sd = if (is.na(digits_sd) && !is.na(digits_sd_least)) {
            digits_exact_sd(sd, digits_sd_least, as.numeric(n), 1, sd_meaning)
          } else {
            digits_sd
          },
          data_type = "integer",
          formula = formula,
          rounding = rounding,
          threshold = threshold,
          symmetric = symmetric,
          sd_meaning = sd_meaning
        ),
        error = function(cnd) NA
      )
      if (isTRUE(at_most)) {
        return(debit_result(TRUE, show_rec))
      }
    }
  }

  # A verdict with a missing value is `NA` only if the value could decide the
  # question either way. A sample SD of `n` binary values divides by `n - 1`,
  # so `n` has to be a whole number greater than 1 for it to describe anything;
  # a population SD needs a positive one. Anything else, a missing `n`
  # included, is undecidable, as in `grim_scalar()` and `grimmer_scalar()` --
  # not a `FALSE` reached through an `Inf` or a zero:
  if (!is_decidable_n_items(n, min_n = sd_min_n(sd_meaning))) {
    return(debit_undecidable(show_rec))
  }

  # As a double: an integer `n` squared below overflows from 46341, as it can
  # under `"sd_pop"`, where the denominator is `n` itself:
  n <- as.numeric(n)
  check_n_exact(n)

  # A missing decimal count is a missing value like any other: no rounding
  # bounds follow from it. With both values missing, nothing is left to decide
  # on; with one of them, the other may rule the case out on its own.
  x_known <- !is.na(x) && !is.na(digits_x)
  sd_known <- !is.na(sd) && !is.na(digits_sd)

  if (!x_known && !sd_known) {
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
  if ((x_known && is.null(bounds$x)) || (sd_known && is.null(bounds$sd))) {
    return(debit_undecidable(show_rec))
  }

  bounds_x <- bounds$x
  bounds_sd <- bounds$sd

  if (x_known) {
    # The least and the greatest number of ones whose mean `k / n` would be
    # reported as `x`: GRIM's range of sums, confined to the 0 to `n` ones that
    # `n` binary values can have. If there is no such number, `sum_lower` is
    # greater than `sum_upper`.
    check_sum_range_exact(x, n, digits_x, hint_items = FALSE, scale = scale)
    sums <- roundwork::sum_range(x, n, digits_x, rounding, threshold, symmetric)
    sums <- c(max(sums[1L], 0), min(sums[2L], n))
  } else {
    bounds_x <- debit_bounds_x_any()
    sums <- c(0, n)
  }

  # With the SD unknown, the mean alone decides the case only if no number of
  # ones has it, as in GRIM; `"mean_n"` takes the mean as continuous, so its
  # range is never empty:
  if (!sd_known) {
    consistency <- if (formula == "exact" && sums[[1L]] > sums[[2L]]) {
      FALSE
    } else {
      NA
    }
    return(debit_result(
      consistency,
      show_rec,
      bounds_x = bounds_x,
      sums = sums
    ))
  }

  consistency <- switch(
    formula,
    "exact" = binary_sd_attainable(
      sum_lower = sums[[1L]],
      sum_upper = sums[[2L]],
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

  # With the mean unknown, an attainable SD only says that some mean would be
  # consistent:
  if (!x_known) {
    if (isTRUE(consistency)) {
      consistency <- NA
    }
    return(debit_result(consistency, show_rec, bounds_sd = bounds_sd))
  }

  debit_result(consistency, show_rec, bounds_sd, bounds_x, sums)
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
#' @details `x`, `sd`, `n`, `digits_x`, `digits_sd`, and `data_type` are
#'   vectorized and recycled to a common length by the tidyverse rules. All
#'   other arguments must have length 1.
#'
#'   `x` and `sd` are limited to about 15 significant digits, and `x` with `n`
#'   as in [`grim()`]. Within these limits, `debit()` decides value sets of
#'   any size exactly. With `x` missing, `n` must be below 2^52, about 4.5e15.
#'
#' @param x Numeric. The reported mean of the binary data, between 0 and 1.
#' @param sd Numeric. The reported standard deviation, between 0 and 1.
#' @param n Integer. The reported sample size.
#' @param digits_x,digits_sd Integer. The number of decimal places in `x` and
#'   `sd`, including trailing zeros. There are no defaults, since a numeric
#'   value does not keep them (`1.40` is `1.4`).
#' @param data_type String. As in [`grim()`]: `"integer"` or `"continuous"`.
#'   No default. For DEBIT, `"integer"` means the binary
#'   values 0 and 1 that the test assumes. Continuous data can have any mean
#'   and SD from 0 to 1, so the result is `TRUE`, unless the SD is the
#'   population SD of a single value, which is 0. Values outside of 0 to 1 are
#'   an error for either data type.
#' @param formula String. `"exact"` (the default) only considers means that
#'   `n` binary values can have, i.e., `k / n` for a whole number `k`. It is
#'   the default because no other mean is possible.
#'   `"mean_n"` treats the mean as continuous, as in Heathers and Brown
#'   (2019), and accepts some SDs that no binary sample of size `n` can have:
#'   `debit(0.05, 0.21, 20, 2, 2, "integer")` is `TRUE` under `"mean_n"`,
#'   although a mean of 0.05 with `n = 20` means one 1 in 20, whose SD rounds
#'   to 0.22.
#' @param rounding String. The rounding method assumed to have produced `x`
#'   and `sd`. Default is `"up_or_down"`. See [`grim()`] for the others.
#' @param threshold,symmetric The same as in [`grim()`].
#' @param sd_meaning String. What `sd` really is: the sample SD,
#'   `"sd_sample"` (the default); the population SD, `"sd_pop"`; either one,
#'   `"sd_sample_or_pop"`; or the standard error of the mean, `"se"`. As in
#'   [`grimmer()`], whose sections *Defaults* and *SD meaning* say why the
#'   sample SD is the default and when to choose another.
#'
#' @export
#'
#' @return Logical. `TRUE` if `x`, `sd`, and `n` are consistent, `FALSE` if
#'   not, and `NA` if the case cannot be decided: if `n` is missing or not a
#'   positive whole number, or if a missing `x` or `sd` (or its decimal count)
#'   could decide the case either way. It is `FALSE` with a missing `sd` if no
#'   number of ones has a mean that would be reported as `x`, and with a
#'   missing `x` if no number of ones has an SD that would be reported as
#'   `sd`. A sample SD divides by `n - 1`, so under `sd_meaning =
#'   "sd_sample"` (the default) and `"se"`, `n` must also be at least 2.
#'
#' @seealso [`debit_explain()`] to explain a result in plain words, and
#'   [`debit_map()`] for data frames.
#'
#' @references Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A
#'   Simple Consistency Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @examples
#' debit(
#'   x = 0.36,
#'   sd = 0.11,
#'   n = 20,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   data_type = "integer"
#' )

# The signature mirrors `debit_scalar()`'s minus `show_rec`; see
# `map_value_sets()`.
debit <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  data_type,
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
) {
  map_value_sets(
    fn = debit_scalar,
    along = c("x", "sd", "n", "digits_x", "digits_sd", "data_type")
  )
}
