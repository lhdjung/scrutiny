# Analytic-GRIMMER (A-GRIMMER) was developed by Aurélien Allard
# (https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/).
# Changes here: reuse scrutiny's infrastructure (`reround()`, `sum_range()`,
# `bound_numerators()`), return a logical value like every other basic
# consistency test, follow scrutiny's naming conventions, and support
# multi-item scales via `rsprite2::GRIMMER_test()`.

# Translation of variable names -------------------------------------------

# original               --> scrutiny
# ********                   ********
#
# aGrimmer               --> grimmer_scalar
# mean                   --> x
# SD                     --> sd
# decimals_mean          --> digits_x
# decimals_SD            --> digits_sd
# realsum                --> s (candidate sum, loop variable)
# effective_n            --> n_items
# Lsigma, Usigma         --> sd_bounds$lower, sd_bounds$upper
# Lowerbound, Upperbound --> range (from `sum_squares_range_shifted()`)
# FirstTest              --> pass_test1
# Matches_Oddness        --> pass_test3
# Matches_SD             --> (test 2, not run; see `grimmer_candidate()`)

# Implementation ----------------------------------------------------------

# The SD's rounding bounds as exact integer numerators over a common
# denominator, as `sum_range()` derives the mean's, plus the
# `(n - 1) * sd^2 * items_averaged^2` part of the sum of squares at each bound
# (`n` for a population SD; `n * (n - 1)` for an SE, whose bounds these then
# are, since the SE was rounded). That part does not depend on the candidate
# sum, so it is computed once, here. `NULL` for a missing SD. Shared with
# `grimmer_explain()`.

grimmer_sd_bounds <- function(
  sd,
  n,
  items_averaged,
  digits_sd,
  rounding,
  threshold,
  symmetric,
  sd_meaning
) {
  sd_bounds <- roundwork::bound_numerators(
    x_num = sd,
    digits = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  if (is.null(sd_bounds)) {
    return(NULL)
  }

  # An SD cannot be negative, so a negative lower bound is really a bound of
  # zero -- and that one is attainable, hence inclusive:
  if (sd_bounds$lower < 0) {
    sd_bounds$lower <- 0
    sd_bounds$incl_lower <- TRUE
  }

  # As a double: `n * (n - 1)` overflows an integer from 46341:
  n <- as.numeric(n)
  sd_bounds$term_lower <- sd_square_term(
    sd_bounds$lower,
    n,
    items_averaged,
    sd_bounds$denom,
    sd_meaning
  )
  sd_bounds$term_upper <- sd_square_term(
    sd_bounds$upper,
    n,
    items_averaged,
    sd_bounds$denom,
    sd_meaning
  )

  sd_bounds
}


# GRIMMER's tests for a single candidate sum `s`. Returns the range of whole
# sums of squares that the SD admits and whether `s` passes each test.
# `grimmer_explain()` reports these very values, so it cannot contradict the
# verdict.

grimmer_candidate <- function(
  s,
  n,
  items_averaged,
  sd_bounds
) {
  # Shifted to the sum `r`; see `R/grimmer-sum-squares.R`. Every sum of squares
  # below is lower by `shift` than the one with the sum `s`. As doubles, since
  # `q * (s + r)` overflows an integer, as from an integer `n` column:
  n <- as.numeric(n)
  q <- s %/% n
  r <- s - n * q
  range <- sum_squares_range_shifted(r, n, sd_bounds)

  # TEST 1: Is there at least one integer between the bounds of the
  # reconstructed sum of squares? Derived in exact integer arithmetic, like the
  # candidate sums: a floating-point tolerance cannot repair a bound once the
  # sum of squares exceeds about 1000 (#86).
  #
  # Test 1 goes beyond Allard (2018) in also requiring a sum of squares that
  # whole numbers with the sum `s` can reach. Otherwise, it would accept value
  # sets like `x = 3.6`, `sd = 0.2`, `n = 11`: the sum must be 40, the SD admits
  # a sum of squares of 146, but no 11 whole numbers summing to 40 go below 148.
  # Shifted, that least sum of squares is `r`: `r` ones, the others zeros.
  upper <- range$upper
  in_doubles <- !is_big(range$lower) && !is_big(upper)
  lower <- if (in_doubles) {
    max(range$lower, r)
  } else if (whole_cmp(range$lower, r) < 0) {
    r
  } else {
    range$lower
  }

  out <- list(
    r = r,
    shift = q * (s + r),
    range = range,
    lower = lower,
    pass_test1 = FALSE,
    pass_test3 = FALSE
  )

  if (if (in_doubles) lower > upper else whole_cmp(lower, upper) > 0) {
    return(out)
  }

  out$pass_test1 <- TRUE

  # TEST 2 of Allard (2018) rounds each admissible sum of squares back to an SD
  # and compares it to the reported one. Test 1 implies it: the bounds are the
  # exact inverse of that rounding, so every integer between them passes.
  # Running it with a floating-point tolerance could only lose matches.

  # TEST 3: The sum of squares of whole numbers has the parity of their sum
  # (`k^2` is odd exactly if `k` is), which the shift keeps. Two or more
  # admissible integers always include one of each parity; a single one must
  # have that of `r`. If that is the minimum, it does, being the sum of squares
  # of whole numbers:
  out$pass_test3 <- if (in_doubles) {
    upper > lower || lower %% 2 == r %% 2
  } else {
    whole_cmp(upper, lower) > 0 || whole_is_odd(lower) == (r %% 2 == 1)
  }

  # For five or more numbers, the minimum and the parity suffice: they can have
  # every sum of squares that passes both. For fewer, some of these are still
  # impossible. A range with none that is possible fails test 1 after all, like
  # one that ends below the minimum:
  if (n < 5 && out$pass_test3) {
    if (n == 1) {
      # One number has only the minimum, which is 0 after the shift. Exact at
      # any size:
      out$attained <- if (whole_cmp(lower, 0) == 0) 0 else NA_real_
    } else {
      check_grimmer_attainable_exact(upper, n)
      out$attained <- sum_squares_attainable(r, n, lower, upper)
    }
    if (is.na(out$attained)) {
      out$pass_test1 <- FALSE
      out$pass_test3 <- FALSE
    }
  }

  out
}


# The sums of squares of a candidate before the shift, for `grimmer_explain()`
# and `grimmer_values()`, which show them: the range that the SD admits, the
# least one that whole numbers with the sum can have, and, for `n < 5`, the
# least one they attain in the range. `NULL` if any of them reaches `2^53`,
# where a double can't show it exactly.
grimmer_candidate_absolute <- function(candidate) {
  range <- candidate$range
  if (is_big(range$lower) || is_big(range$upper)) {
    return(NULL)
  }
  shift <- candidate$shift
  out <- list(
    sum_squares = c(range$lower, range$upper) + shift,
    sum_squares_min = candidate$r + shift,
    attained = candidate$attained + shift
  )
  # `shift` is exact if it is below `2^53`, and so are the sums with it:
  if (abs(shift) >= 2^53 || any(abs(unlist(out)) >= 2^53, na.rm = TRUE)) {
    return(NULL)
  }
  out
}


# Which sums of squares two to four whole numbers can have is worked out in
# doubles (`sum_squares_attainable()`), and for three, by generating the
# numbers `x^2 + xy + y^2`, which takes seconds and gigabytes from about 2^40
# on. That takes an SD of about 6e5 to 1e6, or an SE of 3e5, divided by
# `items_averaged`,
# which no study reports for so few values. One number is decided exactly in
# `grimmer_candidate()`.
check_grimmer_attainable_exact <- function(upper, n) {
  if (is_big(upper) || upper >= 2^40) {
    abort_in_export(
      "The SD is too large for GRIMMER with `n` from 2 to 4.",
      "x" = "With `n = {n}`, the sums of squares to test reach 2^40, about \\
      1.1e12, beyond which telling which of them {n} whole numbers can have \\
      is slow.",
      "i" = "With one or five or more values, GRIMMER has no such limit."
    )
  }
}


grimmer_scalar <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  data_type,
  show_reason = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
) {
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

  if (missing(items_averaged)) {
    error_items_missing()
  }
  # A plain `NA` is logical, but missing all the same:
  if (!is.numeric(items_averaged) && !is.na(items_averaged)) {
    check_type(items_averaged, c("double", "integer"))
  }

  if (missing(data_type)) {
    error_data_type_missing()
  }

  scale <- rounding_scale(rounding, threshold, symmetric)
  check_newly_numeric(x, digits_x, scale = scale)
  check_newly_numeric(sd, digits_sd, scale = scale)

  # `arg_match_in_export()` returns at once for a valid string or the default,
  # which matters here: this runs once per row. `SD_MEANINGS` is the default:
  sd_meaning <- arg_match_in_export(sd_meaning, SD_MEANINGS)

  if (!identical(data_type, "integer")) {
    verdict <- verdict_continuous(
      n,
      items_averaged,
      min_n = sd_min_n(sd_meaning),
      x = x,
      sd = sd,
      digits_sd = digits_sd,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )
    return(test_data_type(
      data_type,
      test_integer = function() {
        grimmer_scalar(
          x = x,
          sd = sd,
          n = n,
          digits_x = digits_x,
          digits_sd = digits_sd,
          items_averaged = items_averaged,
          data_type = "integer",
          show_reason = show_reason,
          rounding = rounding,
          threshold = threshold,
          symmetric = symmetric,
          sd_meaning = sd_meaning
        )
      },
      continuous = if (show_reason) {
        list(unname(verdict), names(verdict))
      } else {
        unname(verdict)
      },
      missing = if (show_reason) list(NA, "Missing value") else NA
    ))
  }

  if (sd_meaning == "sd_sample_or_pop") {
    test_type <- function(sd_meaning) {
      grimmer_scalar(
        x = x,
        sd = sd,
        n = n,
        digits_x = digits_x,
        digits_sd = digits_sd,
        items_averaged = items_averaged,
        data_type = "integer",
        show_reason = show_reason,
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric,
        sd_meaning = sd_meaning
      )
    }
    sample <- test_type("sd_sample")
    if (isTRUE(sample[[1L]])) {
      if (show_reason) {
        sample[[2L]] <- "Passed all (sample SD)"
      }
      return(sample)
    }
    return(
      grimmer_sd_sample_or_pop(sample, test_type("sd_pop"), show_reason, n)
    )
  }

  # A value with its decimal places missing has at least those it shows, and a
  # value set that is inconsistent with these is inconsistent with more; see
  # `digits_least()`. Otherwise, the value is as good as missing, as below. An
  # error is about a size that the test refuses, which leaves the case open:
  digits_x_least <- digits_least(x, digits_x)
  digits_sd_least <- digits_least(sd, digits_sd)
  if (
    (is.na(digits_x) && !is.na(digits_x_least)) ||
      (is.na(digits_sd) && !is.na(digits_sd_least))
  ) {
    at_least <- tryCatch(
      grimmer_scalar(
        x = x,
        sd = sd,
        n = n,
        digits_x = digits_x_least,
        digits_sd = digits_sd_least,
        items_averaged = items_averaged,
        data_type = "integer",
        show_reason = show_reason,
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric,
        sd_meaning = sd_meaning
      ),
      error = function(cnd) NULL
    )
    if (isFALSE(at_least[[1L]])) {
      return(at_least)
    }
  }

  # A verdict with a missing value is `NA` only if the value could decide the
  # question either way. A missing `n` or `items_averaged` always could: with
  # `n = 10^digits_x`, any mean passes GRIM, and some sum of squares fits. A
  # missing decimal count is a missing value like any other: no rounding bounds
  # follow from it.
  # ...but not next to a negative SD, which no data set of any size has. An
  # `n` or `items_averaged` that is given but describes no data set still
  # leaves the case undecidable, as further down:
  if (
    !is.na(sd) &&
      sd < 0 &&
      is_decidable_continuous(n, items_averaged, sd_min_n(sd_meaning))
  ) {
    if (show_reason) {
      return(list(FALSE, "Negative SD"))
    }
    return(FALSE)
  }

  if (is.na(n) || is.na(items_averaged)) {
    if (show_reason) {
      return(list(NA, "Missing value"))
    }
    return(NA)
  }

  # GRIMMER inherits GRIM's requirement that `n` and `items_averaged` be
  # positive whole numbers. A sample SD divides by `n - 1`, so it adds `min_n =
  # 2`; a population SD of a single value is zero, so it does not.
  if (!is_decidable_n_items(n, items_averaged, min_n = sd_min_n(sd_meaning))) {
    if (show_reason) {
      return(list(NA, "No testable value set"))
    }
    return(NA)
  }
  check_n_exact(n)

  # With both values missing, nothing is left to decide on. With one of them,
  # the other may still rule the case out: a mean that fails GRIM does so for
  # any SD, and an SD that no sum of whole numbers can have does so for any
  # mean.
  x_known <- !is.na(x) && !is.na(digits_x)
  sd_known <- !is.na(sd) && !is.na(digits_sd)

  if (!x_known && !sd_known) {
    if (show_reason) {
      return(list(NA, "Missing value"))
    }
    return(NA)
  }

  # No data set has an infinite mean or SD, and an infinity has no decimal
  # places to be reported with.
  if ((x_known && is.infinite(x)) || (sd_known && is.infinite(sd))) {
    if (show_reason) {
      return(list(NA, "Infinite value"))
    }
    return(NA)
  }

  # No data set has a negative SD, so no data set has this one. Squaring it
  # below would hide that.
  if (sd_known && sd < 0) {
    if (show_reason) {
      return(list(FALSE, "Negative SD"))
    }
    return(FALSE)
  }

  n_items <- as.numeric(n) * items_averaged

  # GRIM TEST: the original `n` because `items_averaged` is passed down
  # separately.
  if (x_known) {
    pass_grim <- grim_scalar(
      x = x,
      n = n,
      digits_x = digits_x,
      items_averaged = items_averaged,
      data_type = "integer",
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )

    # The guards above rule this out, but the branch below would fail on an
    # `NA`:
    if (is.na(pass_grim)) {
      if (show_reason) {
        return(list(NA, "GRIM undecidable"))
      }
      return(NA)
    }

    if (!pass_grim) {
      if (show_reason) {
        return(list(FALSE, "GRIM inconsistent"))
      }
      return(FALSE)
    }
  }

  # The mean passed GRIM, or is unknown, and the SD is unknown: it could be
  # anything.
  if (!sd_known) {
    if (show_reason) {
      return(list(NA, "Missing value"))
    }
    return(NA)
  }

  sd_bounds <- grimmer_sd_bounds(
    sd = sd,
    n = n,
    items_averaged = items_averaged,
    digits_sd = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    sd_meaning = sd_meaning
  )

  # Undefined only for a missing SD, already caught above. No bounds, no
  # verdict:
  if (is.null(sd_bounds)) {
    if (show_reason) {
      return(list(NA, "SD rounding bounds undefined"))
    }
    return(NA)
  }

  if (x_known) {
    # All integer sums consistent with the reported mean. `sum_range()` is the
    # same helper `grim_scalar()` uses, so the two always agree on which sums
    # are admissible, and it works in exact integer arithmetic --
    # floating-point products like `floor(x_bounds$upper * n_items)` could
    # drop a legitimate sum or admit a phantom one (#86).
    sums_consistent <- roundwork::sum_range(
      x_num = x,
      n_items = n_items,
      digits = digits_x,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )
  } else {
    # With the mean unknown, any whole-number sum is a candidate:
    sums_consistent <- c(0, n - 1)
  }
  sums_consistent <- grimmer_sums_reachable(sums_consistent, n, sd_bounds)
  check_enumeration_size(
    sums_consistent,
    n = n,
    digits_x = if (x_known) digits_x
  )

  passed <- grimmer_sums_pass(sums_consistent, n, items_averaged, sd_bounds)

  if (passed$any_passed) {
    # With the mean unknown, a sum that admits the SD only says that some mean
    # would be consistent:
    verdict <- if (x_known) TRUE else NA
    if (show_reason) {
      return(list(verdict, if (x_known) "Passed all" else "Missing value"))
    }
    return(verdict)
  }

  # No candidate sum passed test 1 and test 3.
  if (show_reason) {
    if (passed$any_passed_test1) {
      return(list(FALSE, "GRIMMER inconsistent (test 3)"))
    }
    return(list(FALSE, "GRIMMER inconsistent (test 1)"))
  }

  FALSE
}


# The candidate sums among `sums[1]` to `sums[2]` that can pass, as pairs of
# bounds, each pair a range of sums; the others fail. Adding 1 to each number
# adds `n` to the sum and keeps the deviations, so whether a sum admits the SD
# depends only on its remainder `r` modulo `n` (see `grimmer_candidate()`), and
# the first `n` sums stand for all of them. Their least sum of squared
# deviations is `r * (n - r) / n`, at least `min(r, n - r) / 2`, so a sum
# further than twice the SD's term from a multiple of `n` can't pass. Skipping
# these keeps a tiny SD with a large `n` from taking minutes. `approx` is the
# term as a double, so the reach is widened against its rounding error. Shared
# with `grimmer_explain()` and `grimmer_values()`:
grimmer_sums_reachable <- function(sums, n, sd_bounds) {
  lower <- sums[[1L]]
  upper <- min(sums[[2L]], lower + n - 1)
  reach <- grimmer_reach(sd_bounds)
  if (2 * reach + 1 >= n) {
    return(c(lower, upper))
  }
  # One more multiple on each side, in case a division is a hair off:
  multiples <- seq(floor(lower / n) - 1, floor(upper / n) + 1) * n
  starts <- pmax(multiples - reach, lower)
  ends <- pmin(multiples + reach, upper)
  keep <- starts <= ends
  as.vector(rbind(starts[keep], ends[keep]))
}


# How far from a multiple of `n` a sum can be and still pass; see
# `grimmer_sums_reachable()`. `grimmer_explain()` names it:
grimmer_reach <- function(sd_bounds) {
  floor(2 * sd_bounds$term_upper$approx * (1 + 2^-40)) + 2
}


# The sums from pairs of bounds as returned by `grimmer_sums_reachable()`:
sums_from_bounds <- function(bounds) {
  ranges <- lapply(
    seq_len(length(bounds) / 2L) * 2L - 1L,
    function(i) bounds[[i]]:bounds[[i + 1L]]
  )
  as.numeric(unlist(ranges))
}


# Run the GRIMMER tests for each candidate sum from `sums[1]` to `sums[2]`,
# and so on for further pairs of bounds, stopping as soon as one passes them
# all. Also says whether any candidate passed test 1, for the `reason` of a
# failure.
grimmer_sums_pass <- function(sums, n, items_averaged, sd_bounds) {
  any_passed_test1 <- FALSE

  for (i in seq_len(length(sums) / 2L) * 2L - 1L) {
    for (s in sums[i]:sums[i + 1L]) {
      candidate <- grimmer_candidate(
        s = s,
        n = n,
        items_averaged = items_averaged,
        sd_bounds = sd_bounds
      )

      if (!candidate$pass_test1) {
        next
      }

      any_passed_test1 <- TRUE

      if (candidate$pass_test3) {
        return(list(any_passed = TRUE, any_passed_test1 = TRUE))
      }
    }
  }

  list(any_passed = FALSE, any_passed_test1 = any_passed_test1)
}


# Combines the results of `grimmer_scalar()` under `sd_meaning = "sd_sample"`,
# which did not pass, and `"sd_pop"`. The verdict is the population SD's,
# unless a missing value left the sample SD undecided while the population SD
# fails: the value set may then still be consistent as a sample SD, so the
# verdict is `NA`. Not at `n = 1`, where a sample SD does not exist and the
# population SD is the only SD there is. A reason that differs between the two
# names both, unless the sample SD was undecidable. Shared with
# `debit_sd_sample_or_pop()`, which has no reasons.
grimmer_sd_sample_or_pop <- function(sample, population, show_reason, n) {
  verdict <- combine_sd_sample_or_pop(sample[[1L]], population[[1L]], n)

  if (!show_reason) {
    return(verdict)
  }

  reason <- if (isTRUE(verdict)) {
    "Passed all (population SD)"
  } else if (is.na(verdict)) {
    if (is.na(sample[[1L]])) sample[[2L]] else population[[2L]]
  } else if (is.na(sample[[1L]]) || identical(sample[[2L]], population[[2L]])) {
    # The sample SD was undecidable, as at `n = 1`:
    population[[2L]]
  } else {
    paste0(
      "Sample SD: ",
      sample[[2L]],
      "; population SD: ",
      population[[2L]]
    )
  }

  list(verdict, reason)
}


combine_sd_sample_or_pop <- function(sample, population, n) {
  if (is.na(sample) && isFALSE(population) && isTRUE(n > 1)) {
    NA
  } else {
    population
  }
}


#' The GRIMMER test (granularity-related inconsistency of means mapped to error
#' repeats)
#'
#' @description `grimmer()` checks whether a reported mean and standard
#'   deviation of integer data are consistent with the reported sample size and
#'   the number of items that compose them. It extends [`grim()`]: the mean
#'   must pass GRIM, and the SD must then be reachable from one of the integer
#'   sums that GRIM admits.
#'
#'   The function is vectorized, but [`grimmer_map()`] is more convenient for
#'   testing many cases.
#'
#' @details `x`, `sd`, `n`, `digits_x`, `digits_sd`, `items_averaged`, and
#'   `data_type` are vectorized and recycled to a common length by the
#'   tidyverse rules. All other arguments must have length 1.
#'
#'   The implementation follows Allard's (2018) Analytic-GRIMMER algorithm, as
#'   does `rsprite2::GRIMMER_test()`, with a stricter first test: the sum of
#'   squares must be one that `n` whole numbers with the reconstructed sum can
#'   have. For `n >= 5`, this means at least the smallest such sum of squares;
#'   for smaller `n`, some larger ones are impossible, too. With this test,
#'   `grimmer()` is `TRUE` if and only if a sample of whole numbers has the
#'   reported mean and SD. The one exception is `rounding = "even"`, or its
#'   synonym `"ties_even"`, which takes a value on a rounding bound to round
#'   either way, as in
#'   [`roundwork::unround()`], so it may also accept a value set that a sample
#'   only rounds to if ties are broken the other way. GRIMMER was first
#'   proposed by Anaya (2016).
#'
#'   For data on a bounded scale, such as a Likert scale, see the
#'   [strait](https://github.com/ianhussey/strait) package. Its `brimmest()`
#'   decides exactly whether a mean and SD are possible within the bounds.
#'
#'   `x` and `sd` are limited to about 15 significant digits, and `x` with
#'   `n` and `items_averaged` as in [`grim()`]. Within these limits,
#'   `grimmer()` decides value sets of any size exactly, but refuses some it
#'   would take too long to decide, none of which occur in practice: an SD of
#'   about 6e5 or more for `n` from 2 to 4, or 3e5 if it is an SE (divided by
#'   `items_averaged`), and a value set that leaves more than a million
#'   candidate sums to test, which takes an `n` of more than a million.
#'
#' @param x Numeric. The reported mean.
#' @param sd Numeric. The reported standard deviation.
#' @param n Integer. The reported sample size.
#' @param digits_sd Integer. The number of decimal places in `sd`, including
#'   trailing zeros. There is no default, since a numeric value does not
#'   keep them (`1.40` is `1.4`).
#' @param items_averaged Integer. As in [`grim()`], for `x` and `sd`.
#' @param data_type String. As in [`grim()`]: `"integer"` or `"continuous"`,
#'   the latter of which can have any mean
#'   and SD, though no negative SD, and a single value only an SD of 0. No
#'   default.
#' @param sd_meaning String. What `sd` really is. `"sd_sample"` (the
#'   default; see *Defaults* in [`grimmer()`]) is the sample SD, which
#'   divides by `n - 1`, as in R, SPSS, Stata, or Excel's `STDEV.S`.
#'   `"sd_pop"` is the population SD, which divides by `n`, as in NumPy's
#'   `np.std()` or Excel's `STDEV.P`. `"sd_sample_or_pop"`
#'   accepts a value set that is consistent as one or the other. `"se"` is the
#'   standard error of the mean, the sample SD divided by `sqrt(n)`, reported as
#'   if it were an SD. See *SD meaning*.
#'
#' @inheritParams grim
#'
#' @section Defaults: `sd_meaning = "sd_sample"` because the sample SD is what
#'   nearly all software reports by default: R's `sd()`, SPSS, Stata, SAS,
#'   Excel's `STDEV()` and `STDEV.S()`, and pandas' `.std()`. The main
#'   exception is NumPy, whose `np.std()` returns the population SD unless
#'   `ddof = 1` is given; so does Python's `statistics.pstdev()`. If the data
#'   may have been analyzed that way, or a value set fails only at a small
#'   `n`, test it under `"sd_pop"` as well. See *SD meaning*.
#'
#'   The rounding defaults are explained in [`grim()`].
#'
#' @section SD meaning: The sample and population SDs differ by a factor of
#'   `sqrt(n / (n - 1))`, which matters most at small `n`, where GRIMMER is
#'   most powerful. A population SD tested as a sample SD will often seem
#'   inconsistent, so `sd_meaning` lets you check whether a value set is
#'   consistent only under the other convention.
#'
#'   `"sd_sample_or_pop"` is `TRUE` if the value set is consistent as a sample
#'   SD or as a population SD, and `FALSE` if it is neither. A single value has
#'   no sample SD, so at `n = 1`, `"sd_sample_or_pop"` is the same as
#'   `"sd_pop"`. The `reason` column of [`grimmer_map()`] names the convention a
#'   value set passed under, trying the sample SD first.
#'
#'   `"sd_sample_or_pop"` accepts far more SDs than either convention alone, so
#'   it is not the default. It also judges each value set on its own, although a
#'   single study usually computed all its SDs the same way. To tell which
#'   convention fits a set of values, test them under `"sd_sample"` and
#'   `"sd_pop"` separately.
#'
#'   Some papers report a standard error as if it were an SD. `"se"` tests
#'   whether a value set is consistent under that premise. The SE was rounded,
#'   not the SD, so the SD may lie anywhere in the SE's rounding range times
#'   `sqrt(n)`. This range is wide: from about `n = 30` on, nearly every value
#'   set whose mean passes GRIM is consistent under `"se"`. Such a result says
#'   little, and it is no evidence that an SE was reported.
#'
#'   `"se"` is the SE of this group's mean, computed from its own sample SD and
#'   `n`. Model-based SEs, such as those of estimated marginal means, or SEs
#'   based on a pooled variance, are not. A value set that is inconsistent
#'   under `"se"` may still have been reported with one of them.
#'
#' @return Logical. `TRUE` if `x`, `sd`, `n`, and `items_averaged` are
#'   consistent, `FALSE` if not, and `NA` if the case cannot be decided: if a
#'   value is infinite, if `n` or `items_averaged` is missing or not a
#'   positive whole number, or if a missing `x` or `sd` (or its decimal count)
#'   could decide the case either way. It is `FALSE` with a missing `sd` if
#'   `x` fails GRIM, and with a missing `x` if no sum of whole numbers admits
#'   `sd`. A sample SD divides by `n - 1`, so under `sd_meaning =
#'   "sd_sample"` (the default) and `"se"`, `n` must also be at least 2.
#'
#' @references Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
#'   possibility of standard deviations.
#'   https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/
#'
#'   Anaya, J. (2016). The GRIMMER test: A method for testing the validity of
#'   reported measures of variability. *PeerJ Preprints.*
#'   https://peerj.com/preprints/2400v1/
#'
#' @seealso [`grimmer_explain()`] to explain a result in plain words,
#'   [`grimmer_map()`] for data frames, and [`grimmer_values()`] for the means
#'   and SDs behind a consistent value set.
#'
#' @export
#'
#' @examples
#' # A mean of 5.23 is not consistent with an SD of 2.55
#' # and a sample size of 35:
#' grimmer(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 35,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1, data_type = "integer"
#' )
#'
#' # However, mean and SD are consistent with a
#' # sample size of 31:
#' grimmer(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 31,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1, data_type = "integer"
#' )
#'
#' # For a scale composed of two items:
#' grimmer(
#'   x = 2.74,
#'   sd = 0.96,
#'   n = 63,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 2, data_type = "integer"
#' )

# The signature mirrors `grimmer_scalar()`'s minus `show_reason`; see
# `map_value_sets()`.
grimmer <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  data_type,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
) {
  map_value_sets(
    fn = grimmer_scalar,
    along = c(
      "x",
      "sd",
      "n",
      "digits_x",
      "digits_sd",
      "items_averaged",
      "data_type"
    )
  )
}
