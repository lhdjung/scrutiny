# Introductory notes ------------------------------------------------------

# Analytic-GRIMMER (A-GRIMMER) was developed by Aurélien Allard
# (https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/).
# His original algorithm received some modifications here, for these reasons:
# -- Tapping scrutiny's infrastructure for implementing error detection
# techniques; for example, functions like `reround()` and
# `decimal_places_scalar()`.
# -- Changing the return value to logical, which is the expected output from the
# basic implementation of any consistency test within scrutiny.
# -- Adjusting variable names to the tidyverse style guide and scrutiny's
# domain-specific conventions.
# -- Adding support for multi-item scales (see `items` argument) via
# `rsprite2::GRIMMER_test()`.

# Translation of variable names -------------------------------------------

# original           --> scrutiny
# ********               ********
#
# aGrimmer           --> grimmer_scalar
# mean               --> x
# SD                 --> sd
# decimals_mean      --> digits_x  (argument removed; counting internally)
# decimals_SD        --> digits_sd (argument removed; counting internally)
# realmean           --> x_real    (computed per candidate sum inside loop)
# realsum            --> s         (loop variable over consistent_sums)
# effective_n        --> n_items
# Lsigma             --> sd_lower
# Usigma             --> sd_upper
# Lowerbound         --> sum_squares_lower
# Upperbound         --> sum_squares_upper
# Possible_Integers  --> integers_possible
# Predicted_Variance --> var_predicted
# Predicted_SD       --> sd_predicted
# Matches_Oddness    --> matches_parity
# FirstTest          --> pass_test1 (evaluated per candidate sum inside loop)
# Matches_SD         --> matches_sd (to which a `pass_test2` object was added)
# Third_Test         --> pass_test3 (evaluated per candidate sum inside loop)

# # Example inputs 1:
# x <- "1.03"
# sd <- "0.41"
# n <- 40
# items <- 1
# show_reason <- TRUE
# rounding <- "up_or_down"
# threshold <- 5
# symmetric <- FALSE
# tolerance <- .Machine$double.eps^0.5
# # decimals_mean <- 2
# # decimals_SD <- 2

# # Example inputs 2:
# # (actually derived from this distribution: c(1, 1, 2, 3, 3, 4, 4, 4, 4, 5))
# x <- "3.10"
# sd <- "1.37"
# n <- 10
# items <- 1
# show_reason <- TRUE
# rounding <- "up_or_down"
# threshold <- 5
# symmetric <- FALSE
# tolerance <- .Machine$double.eps^0.5
# # decimals_mean <- 2
# # decimals_SD <- 2

# # Example inputs 3:
# # (edge case from `pigs5`)
# x <- "2.57"
# sd <- "2.57"
# n <- 30
# items <- 1
# show_reason <- TRUE
# rounding <- "up_or_down"
# threshold <- 5
# symmetric <- FALSE
# tolerance <- .Machine$double.eps^0.5
# # decimals_mean <- 2
# # decimals_SD <- 2

# # Example inputs 4:
# # (edge case with bug in scrutiny <= 0.5.0):
# x <- "4.67"
# sd <- "0.00"
# n <- 2
# items <- 3
# show_reason <- FALSE
# rounding <- "up_or_down"
# threshold <- 5
# symmetric <- FALSE
# tolerance <- .Machine$double.eps^0.5

# # Example inputs 5 (by Nathanael):
# x <- 2.1
# sd <- 0.4
# n <- 17
# digits_x <- 1
# digits_sd <- 1
# items <- 1
# show_reason <- TRUE
# rounding <- "up_or_down"
# threshold <- 5
# symmetric <- FALSE
# tolerance <- .Machine$double.eps^0.5

# # To reproduce issue #85
# x <- 0.11
# sd <- 0.87
# n <- 64
# digits_x <- 2
# digits_sd <- 2
# items <- 1
# show_reason <- FALSE
# rounding <- "up"
# threshold <- 5
# symmetric <- FALSE
# tolerance <- .Machine$double.eps^0.5

# Implementation ----------------------------------------------------------

# Validate the optional bounds of the scale that `x` and `sd` were measured on,
# and report whether they were given at all. They only make sense as a pair:
# with just one of them, the values could still spread out without limit in the
# other direction, so nothing would follow about the SD.

check_scale_bounds <- function(min_val, max_val) {
  if (is.null(min_val) && is.null(max_val)) {
    return(FALSE)
  }

  if (is.null(min_val) || is.null(max_val)) {
    name_missing <- if (is.null(min_val)) "min_val" else "max_val"
    name_given <- if (is.null(min_val)) "max_val" else "min_val"
    cli::cli_abort(c(
      "`min_val` and `max_val` must be specified together.",
      "x" = "`{name_given}` was specified, but `{name_missing}` was not.",
      "i" = "A single bound doesn't limit how far the values can spread out \\
      around their mean, so it says nothing about the standard deviation."
    ))
  }

  check_type(min_val, c("double", "integer"))
  check_type(max_val, c("double", "integer"))
  check_length(min_val, 1L)
  check_length(max_val, 1L)

  if (!is_whole_number(min_val) || !is_whole_number(max_val)) {
    cli::cli_abort(c(
      "`min_val` and `max_val` must be whole numbers.",
      "x" = "They are {min_val} and {max_val}.",
      "i" = "GRIMMER assumes that the individual values are whole numbers, \\
      so the bounds of the scale they were measured on are, as well."
    ))
  }

  if (min_val >= max_val) {
    cli::cli_abort(c(
      "`max_val` must be greater than `min_val`.",
      "x" = "`min_val` is {min_val} and `max_val` is {max_val}."
    ))
  }

  TRUE
}


grimmer_scalar <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items = 1,
  min_val = NULL,
  max_val = NULL,
  show_reason = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  tolerance = .Machine$double.eps^0.5
) {
  check_type(items, c("double", "integer"))
  has_scale <- check_scale_bounds(min_val, max_val)

  if (missing(digits_x)) {
    error_digits_missing(x)
  }

  if (missing(digits_sd)) {
    error_digits_missing(sd)
  }

  check_newly_numeric(x, digits_x)
  check_newly_numeric(sd, digits_sd)

  x_orig <- x
  x <- as.numeric(x)
  sd <- as.numeric(sd)

  # A missing value makes the test undecidable, just like the undefined rounding
  # bounds below. It has to be caught before the GRIM test rather than after it,
  # because `grim_scalar()` returns `NA` for it and the branch on that result
  # would fail on a missing value:
  if (is.na(x) || is.na(sd) || is.na(n)) {
    if (show_reason) {
      return(list(NA, "Missing value"))
    }
    return(NA)
  }

  # With the scale's bounds known, a mean outside of them is inconsistent
  # before any reconstruction: no set of values within the range has it.
  if (has_scale && (x < min_val || x > max_val)) {
    if (show_reason) {
      return(list(FALSE, "Mean out of scale range"))
    }
    return(FALSE)
  }

  n_items <- n * items

  # GRIM TEST: It says `x_orig` because the `x` object has been coerced from
  # character to numeric, but `grim_scalar()` needs the original number-string.
  # Similarly, since this function also gets `items` passed down, it needs the
  # original `n`, not `n_items`.
  pass_grim <- grim_scalar(
    x = x_orig,
    n = n,
    digits_x = digits_x,
    items = items,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    tolerance = tolerance
  )

  # GRIM itself can be undecidable -- with a non-positive `n` -- and then so is
  # GRIMMER, which builds on it. The missing-value guard above catches only the
  # other route to an `NA`
  # verdict, so this one has to be here rather than folded into it, and it has
  # to precede the branch below, which would fail on an `NA`:
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

  # SD bounds as exact integer numerators over a common denominator, the same
  # way `sum_range()` derives the mean's bounds. This handles all rounding
  # modes and their boundary inclusion, unlike the earlier hardcoded `5 /
  # 10^(digits_sd + 1)` approach which was only exact for "up_or_down" with
  # threshold = 5.
  sd_bounds <- bound_numerators(
    x_num = sd,
    digits = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # The bounds are undefined for a missing SD, which the guard above has already
  # caught, so this is a belt-and-braces check on the same contract that
  # `grim_scalar()` honors: no bounds, no verdict.
  if (is.null(sd_bounds)) {
    if (show_reason) {
      return(list(NA, "SD rounding bounds undefined"))
    }
    return(NA)
  }

  sd_num_lower <- sd_bounds$lower
  sd_incl_lower <- sd_bounds$incl_lower

  # An SD cannot be negative, so a negative lower bound is really a bound of
  # zero -- and that one is attainable, hence inclusive:
  if (sd_num_lower < 0) {
    sd_num_lower <- 0
    sd_incl_lower <- TRUE
  }

  # The `(n - 1) * sd^2 * items^2` part of the sum of squares does not depend on
  # the candidate sum, so both bounds of it are computed once, here:
  term_lower <- sd_square_term(sd_num_lower, n, items, sd_bounds$denom)
  term_upper <- sd_square_term(sd_bounds$upper, n, items, sd_bounds$denom)

  # Enumerate all integer sums consistent with the reported mean by mapping the
  # mean's rounding interval into sum space. This replaces the earlier
  # `round(mean * n)` approach, which only ever produced a single candidate sum
  # and could miss the other when two consecutive integers both round to the
  # reported mean. `sum_range()` is the same helper that `grim_scalar()` uses,
  # so the two functions always agree on which sums are admissible, and it
  # works in exact integer arithmetic: deriving the range from floating-point
  # products like `floor(x_bounds$upper * n_items)` could drop a legitimate sum
  # or admit a phantom one whenever the product was mathematically an exact
  # integer (#86).
  sums_consistent <- sum_range(
    x_num = x,
    n_items = n_items,
    digits = digits_x,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  consistent_sums <- sums_consistent[1L]:sums_consistent[2L]

  # Loop over all candidate sums, running all three GRIMMER tests for each.
  # Each candidate corresponds to one possible integer sum of the original data
  # that is consistent with the reported mean. We return TRUE as soon as one
  # candidate passes all three tests. For show_reason, we track the furthest
  # test any candidate has reached before failing:
  # -- 0: all candidates failed test 1 (report test 1 failure)
  # -- 1: some candidate passed test 1 but not test 2 (report test 2 failure)
  # -- 2: some candidate passed tests 1 and 2 but not test 3 (report test 3 failure)
  furthest_test_passed <- 0L

  # Whether the scale's bounds are what ruled a candidate sum out. This only
  # affects the reason given for an inconsistency, not the verdict:
  blocked_by_scale <- FALSE

  # The values that are summed and squared below are the `n` respondents'
  # whole-number totals across all items, so the scale's bounds apply to them
  # multiplied by the number of items:
  totals_lower <- min_val * items
  totals_upper <- max_val * items

  for (s in consistent_sums) {
    # TEST 1: Check that there is at least one integer between the lower and
    # upper bounds (of the reconstructed sum of squares of the -- most likely
    # unknown -- values for which `x` was reported as a mean). Like the mean's
    # candidate sums above, these bounds are derived in exact integer
    # arithmetic: `round(sum_squares_lower, 12)` used to stand in for that, but
    # it cannot repair anything once the sum of squares exceeds about 1000,
    # because the spacing between neighboring doubles is larger than 1e-12 from
    # there on. A bound that is mathematically an exact integer was then
    # ceilinged to the next one up, dropping the only viable sum of squares
    # (#86).
    sum_squares <- sum_squares_range(
      s = s,
      n = n,
      term_lower = term_lower,
      term_upper = term_upper,
      incl_lower = sd_incl_lower,
      incl_upper = sd_bounds$incl_upper
    )

    # If the scale's bounds are known, the values are not merely whole numbers
    # but whole numbers within a fixed range, which caps how far they can
    # spread out around their mean -- and hence how large the sum of squares
    # can get. Lowering the ceiling of the range that the reported SD admits is
    # all that has to happen here: every test below operates on that range.
    if (has_scale) {
      sum_squares_ceiling <- sum_squares_scale_max(
        s = s,
        n = n,
        val_lower = totals_lower,
        val_upper = totals_upper
      )
      if (is.null(sum_squares_ceiling)) {
        # No set of values within the scale's range adds up to this candidate
        # sum, so it is out of reach whatever the SD is:
        blocked_by_scale <- TRUE
        next
      }
      if (sum_squares_ceiling < sum_squares[2L]) {
        # Only a ceiling that actually bites can be the reason for a failure:
        sum_squares[2L] <- sum_squares_ceiling
        blocked_by_scale <- blocked_by_scale ||
          sum_squares[1L] > sum_squares[2L]
      }
    }

    if (sum_squares[1L] > sum_squares[2L]) {
      next
    }

    furthest_test_passed <- max(furthest_test_passed, 1L)

    # Create a vector of all possible integers between the lower and upper
    # bounds of the sum of squares:
    integers_possible <- sum_squares[1L]:sum_squares[2L]

    # Create the predicted variance. Subtracting `s^2 / n` from the integer sum
    # of squares directly is much better conditioned than the equivalent
    # `integers_possible / items^2 - n * (s / n_items)^2`, which cancels two
    # large, nearly equal floating-point numbers:
    var_predicted <- (integers_possible - s^2 / n) / (items^2 * (n - 1))

    # The bounds above guarantee `var_predicted >= sd_lower^2 >= 0`, so anything
    # negative here is floating-point noise from that division:
    var_predicted <- pmax(var_predicted, 0)

    # Derive the predicted SD:
    sd_predicted <- sqrt(var_predicted)

    # Reconstruct the SD:
    sd_rec_rounded <- reround(
      x = sd_predicted,
      digits = digits_sd,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )

    # `reround()` returns one value per element of `sd_predicted` for
    # deterministic rounding methods, but two interleaved values (rounded up and
    # down) per element for "up_or_down" and similar methods -- i.e.,
    # `sd_rec_rounded` is `[up(cand_1), down(cand_1), up(cand_2), ...]`. `reps`
    # recovers the block size so each candidate integer's own reconstructed
    # SD(s) can be checked against the reported SD, instead of pooling all
    # candidates' reconstructed SDs together. The latter behavior used to be a
    # bug that let a match for one candidate and a parity match for a
    # *different* candidate combine into a false pass, see:
    # https://github.com/lhdjung/scrutiny/issues/85
    reps <- length(sd_rec_rounded) / length(integers_possible)

    # Check the reported SD for near-equality with the reconstructed SD values,
    # separately for each candidate integer. The comparison goes through
    # `dplyr::near()` rather than `==` to absorb spurious floating-point
    # precision in the reconstructed values:
    matches_sd <- vapply(
      seq_along(integers_possible),
      function(i) {
        # `sd_rec_rounded` is `reps` values per candidate, laid out back to back
        # (candidate 1's `reps` values, then candidate 2's, etc.), so candidate
        # `i`'s block starts right after candidate `i - 1`'s block ends, i.e. at
        # `(i - 1) * reps + 1`, and runs for `reps` values, i.e., up to `i *
        # reps`.
        block <- ((i - 1L) * reps + 1L):(i * reps)
        any(
          dplyr::near(sd_rec_rounded[block], sd, tol = tolerance),
          na.rm = TRUE
        )
      },
      logical(1)
    )

    # TEST 2: If none of the reconstructed SDs matches the reported one, this
    # candidate sum is not viable.
    if (!any(matches_sd)) {
      next
    }

    furthest_test_passed <- max(furthest_test_passed, 2L)

    # TEST 3: Determine if any integer between the lower and upper bounds has
    # both a matching reconstructed SD (`matches_sd`) and the same parity (i.e.,
    # the property of being even or odd) as s, the candidate sum -- both
    # conditions checked against the *same* candidate integer.
    matches_parity <- s %% 2 == integers_possible %% 2
    matches_sd_and_parity <- matches_sd & matches_parity

    if (!any(matches_sd_and_parity)) {
      next
    }

    # All three tests passed for this candidate sum
    if (show_reason) {
      return(list(TRUE, "Passed all"))
    }
    return(TRUE)
  }

  # No candidate sum passed all three tests.
  if (show_reason) {
    # The scale's bounds are reported in their own right, but only if no
    # candidate sum got past the first test without them. Otherwise the tests
    # below are the more specific reason:
    if (furthest_test_passed == 0L && blocked_by_scale) {
      return(list(FALSE, "GRIMMER inconsistent (scale range)"))
    }
    reason <- switch(
      as.character(furthest_test_passed),
      "0" = "GRIMMER inconsistent (test 1)",
      "1" = "GRIMMER inconsistent (test 2)",
      "2" = "GRIMMER inconsistent (test 3)",
      cli::cli_abort("Internal error: Invalid `furthest_test_passed` value.")
    )
    return(list(FALSE, reason))
  }

  FALSE
}


#' The GRIMMER test (granularity-related inconsistency of means mapped to error
#' repeats)
#'
#' @description `grimmer()` checks if reported mean and SD values of integer
#'   data are mathematically consistent with the reported sample size and the
#'   number of items that compose the mean value. It works much like [`grim()`].
#'
#'   The function is vectorized, but it is recommended to use [`grimmer_map()`]
#'   for testing multiple cases.
#'
#' @param x Numeric. The reported mean value.
#' @param sd Numeric. The reported standard deviation.
#' @param n Integer. The reported sample size.
#' @param digits_sd Integer. The number of decimal places in `sd`, including
#'   trailing zeros. As with `digits_x`, there is no default, because trailing
#'   zeros don't survive in a numeric value.
#' @param items Integer. The number of items composing the `x` and `sd` values.
#'   Default is `1`, the most common case.
#' @param min_val,max_val Integer. Optionally, the minimum and maximum value
#'   that an individual response could take, as with the endpoints of a Likert
#'   scale. If both are specified, GRIMMER also tests whether values within
#'   that range could have produced the reported `x` and `sd` in the first
#'   place. Both default to `NULL`, i.e., an unbounded scale. See *Scale
#'   bounds* below.
#' @param show_reason Logical. For internal use only. If set to `TRUE`, the
#'   output is a list of length-2 lists which also contain the reasons for
#'   inconsistencies. Don't specify this manually; instead, use `show_reason` in
#'   [`grimmer_map()`]. See there for explanation. Default is `FALSE`.
#'
#' @inheritParams grim
#'
#' @return Logical. `TRUE` if `x`, `sd`, `n`, and `items` are mutually
#'   consistent, `FALSE` if not, and `NA` if the case cannot be decided: if any
#'   of the values is missing, or if the rounding bounds are undefined.

#' @details GRIMMER was originally devised by Anaya (2016). The present
#'   implementation follows Allard's (2018) refined Analytic-GRIMMER algorithm.
#'   It uses a variant of Analytic-GRIMMER also implemented in
#'   \href{https://lukaswallrich.github.io/rsprite2/reference/GRIMMER_test.html}{`rsprite2::GRIMMER_test()`}
#'   that can be applied to multi-item scales and oiptionally takes scale ranges
#'   into account.
#'
#'   The scrutiny version embeds GRIMMER in the broader system of consistency
#'   testing, as laid out in
#'   \href{https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.html}{*Consistency
#'   tests in depth*}. The `grimmer()` function
#'   is a vectorized (multiple-case) version of this basic implementation. For
#'   more context and variable name translations, see the top of the R/grimmer.R
#'   source file.

#' @section Scale bounds: GRIMMER assumes that the individual values behind `x`
#'   and `sd` are whole numbers. If they were also confined to a fixed range --
#'   the endpoints of a Likert scale, say -- state that range using `min_val`
#'   and `max_val`. The test then knows how far the values could spread out
#'   around their mean at most, which rules out standard deviations that are
#'   possible on an unbounded scale. A reported `x` outside of the range is
#'   inconsistent by itself.
#'
#'   Both bounds refer to a single response, so they don't depend on `items`:
#'   for a five-point scale, `min_val` is `1` and `max_val` is `5` whether the
#'   mean was composed of one item or of ten. They must be specified together,
#'   because a single bound places no limit on the standard deviation.
#'
#'   Scale bounds can only turn a `TRUE` verdict into `FALSE`, never the other
#'   way around. Like GRIMMER's other tests, the condition they add is
#'   necessary but not sufficient, so a value set that passes it may still be
#'   impossible.
#'
#'   This is the least of what a known scale range implies, and it is here
#'   because it costs GRIMMER one comparison per candidate sum. The
#'   \href{https://github.com/ianhussey/strait}{strait} package is devoted to
#'   the subject and goes much further: bounds sharpened by attained extremes,
#'   by response granularity, or by a reported Cronbach's alpha
#'   (`strait::sd_bounds()`), and an exact decision procedure that is
#'   sufficient as well as necessary (`strait::brimmest()`). For the
#'   simulation-based SPRITE technique, see
#'   \href{https://lukaswallrich.github.io/rsprite2/}{rsprite2}.

#' @references Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
#'   possibility of standard deviations.
#'   https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/
#'
#'   Anaya, J. (2016). The GRIMMER test: A method for testing the validity of
#'   reported measures of variability. *PeerJ Preprints.*
#'   https://peerj.com/preprints/2400v1/
#'
#'   Mestdagh, M., Pe, M., Pestman, W., Verdonck, S., Kuppens, P., &
#'   Tuerlinckx, F. (2018). Sidelining the mean: The relative variability index
#'   as a generic mean-corrected variability measure for bounded variables.
#'   *Psychological Methods*, 23(4), 690-707.
#'   https://doi.org/10.1037/met0000153

#' @export
#'
#' @examples
#' # A mean of 5.23 is not consistent with an SD of 2.55
#' # and a sample size of 35:
#' grimmer(x = 5.23, sd = 2.55, n = 35, digits_x = 2, digits_sd = 2)
#'
#' # However, mean and SD are consistent with a
#' # sample size of 31:
#' grimmer(x = 5.23, sd = 2.55, n = 31, digits_x = 2, digits_sd = 2)
#'
#' # For a scale composed of two items:
#' grimmer(x = 2.74, sd = 0.96, n = 63, digits_x = 2, digits_sd = 2, items = 2)
#'
#' # A mean of 3.00 with an SD of 2.08 is possible for 20 whole numbers...
#' grimmer(x = 3.00, sd = 2.08, n = 20, digits_x = 2, digits_sd = 2)
#'
#' # ...but not if they were responses on a scale from 1 to 5. Even the most
#' # extreme such sample, ten 1s and ten 5s, has an SD of only 2.05:
#' grimmer(
#'   x = 3.00, sd = 2.08, n = 20, digits_x = 2, digits_sd = 2,
#'   min_val = 1, max_val = 5
#' )

grimmer <- Vectorize(grimmer_scalar)
