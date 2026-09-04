# Analytic-GRIMMER (A-GRIMMER) was developed by Aurélien Allard
# (https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/).
# Changes here: reuse scrutiny's infrastructure (`reround()`, `sum_range()`,
# `bound_numerators()`), return a logical value like every other basic
# consistency test, follow scrutiny's naming conventions, and support
# multi-item scales via `rsprite2::GRIMMER_test()`.

# Translation of variable names -------------------------------------------

# original           --> scrutiny
# ********               ********
#
# aGrimmer           --> grimmer_scalar
# mean               --> x
# SD                 --> sd
# decimals_mean      --> digits_x
# decimals_SD        --> digits_sd
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

# Implementation ----------------------------------------------------------

# Validate the optional scale bounds and report whether they were given at all.
# They only make sense as a pair: one bound alone doesn't limit the spread, so
# nothing would follow about the SD.

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

  # Undecidable. Caught before the GRIM test below, whose `NA` result the
  # branch on `pass_grim` could not handle:
  if (is.na(x) || is.na(sd) || is.na(n)) {
    if (show_reason) {
      return(list(NA, "Missing value"))
    }
    return(NA)
  }

  # No data set has an infinite mean or SD, and an infinity has no decimal
  # places to be reported with.
  if (is.infinite(x) || is.infinite(sd)) {
    if (show_reason) {
      return(list(NA, "Infinite value"))
    }
    return(NA)
  }

  # With the bounds known, a mean outside of them is inconsistent before any
  # reconstruction: no set of values within the range has it.
  if (has_scale && (x < min_val || x > max_val)) {
    if (show_reason) {
      return(list(FALSE, "Mean out of scale range"))
    }
    return(FALSE)
  }

  # GRIMMER inherits GRIM's requirement that `n` and `items` be positive whole
  # numbers, and adds `min_n = 2`: it reconstructs a *sample* SD, so it divides
  # by `n - 1`.
  if (!is_decidable_n_items(n, items, min_n = 2)) {
    if (show_reason) {
      return(list(NA, "No testable value set"))
    }
    return(NA)
  }

  n_items <- n * items

  # GRIM TEST: `x_orig` because `x` has been coerced to numeric, and the
  # original `n` because `items` is passed down separately. `tolerance` is
  # deliberately not passed on -- it is deprecated in `grim()`, so forwarding it
  # would fire that warning for every `grimmer()` call. GRIMMER's own use of it
  # below is unaffected.
  pass_grim <- grim_scalar(
    x = x_orig,
    n = n,
    digits_x = digits_x,
    items = items,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # The guards above rule out every case where this happens today, so it is
  # belt and braces -- but it must precede the branch below, which would fail
  # on an `NA`:
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
  # way `sum_range()` derives the mean's bounds. Exact for every rounding
  # method and its boundary inclusion.
  sd_bounds <- bound_numerators(
    x_num = sd,
    digits = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # Undefined only for a missing SD, already caught above. No bounds, no
  # verdict:
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

  # All integer sums consistent with the reported mean. `sum_range()` is the
  # same helper `grim_scalar()` uses, so the two always agree on which sums are
  # admissible, and it works in exact integer arithmetic -- floating-point
  # products like `floor(x_bounds$upper * n_items)` could drop a legitimate sum
  # or admit a phantom one (#86).
  sums_consistent <- sum_range(
    x_num = x,
    n_items = n_items,
    digits = digits_x,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  check_enumeration_size(
    sums_consistent,
    what = "integer sums consistent with the reported mean",
    n = n
  )

  consistent_sums <- sums_consistent[1L]:sums_consistent[2L]

  # Run all three GRIMMER tests for each candidate sum, returning `TRUE` as
  # soon as one passes all three. For `show_reason`, track the furthest test
  # any candidate reached before failing (0 = none passed test 1, and so on):
  furthest_test_passed <- 0L

  # Whether the scale's bounds ruled a candidate sum out. Affects the reason
  # given, not the verdict:
  blocked_by_scale <- FALSE

  # The values summed below are the `n` respondents' totals across all items,
  # so the bounds apply multiplied by `items`:
  totals_lower <- min_val * items
  totals_upper <- max_val * items

  for (s in consistent_sums) {
    # TEST 1: Is there at least one integer between the bounds of the
    # reconstructed sum of squares? Derived in exact integer arithmetic, like
    # the candidate sums above: a floating-point tolerance cannot repair a bound
    # once the sum of squares exceeds about 1000 (#86).
    sum_squares <- sum_squares_range(
      s = s,
      n = n,
      term_lower = term_lower,
      term_upper = term_upper,
      incl_lower = sd_incl_lower,
      incl_upper = sd_bounds$incl_upper
    )

    # Known bounds cap how far the values can spread out, hence how large the
    # sum of squares can get. Lowering the ceiling is all that is needed here:
    # every test below operates on that range.
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

    check_enumeration_size(
      sum_squares,
      what = "integer sums of squares consistent with the reported SD",
      n = n
    )

    integers_possible <- sum_squares[1L]:sum_squares[2L]

    # Subtracting `s^2 / n` from the integer sum of squares directly is much
    # better conditioned than the equivalent
    # `integers_possible / items^2 - n * (s / n_items)^2`, which cancels two
    # large, nearly equal floating-point numbers:
    var_predicted <- (integers_possible - s^2 / n) / (items^2 * (n - 1))

    # The bounds above guarantee `var_predicted >= sd_lower^2 >= 0`, so anything
    # negative here is floating-point noise from that division:
    var_predicted <- pmax(var_predicted, 0)

    sd_predicted <- sqrt(var_predicted)

    sd_rec_rounded <- reround(
      x = sd_predicted,
      digits = digits_sd,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )

    # `reround()` returns one value per element of `sd_predicted` for
    # deterministic rounding methods, but two interleaved ones for "up_or_down"
    # and friends: `[up(cand_1), down(cand_1), up(cand_2), ...]`. `reps` is that
    # block size, so each candidate is checked against its own reconstructed
    # SD(s). Pooling them let a match for one candidate combine with a parity
    # match for another into a false pass (#85).
    reps <- length(sd_rec_rounded) / length(integers_possible)

    # Near-equality of reported and reconstructed SD, per candidate integer.
    # `dplyr::near()` rather than `==` to absorb floating-point noise:
    matches_sd <- vapply(
      seq_along(integers_possible),
      function(i) {
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

    # TEST 3: Does any *single* integer both match the reported SD and have the
    # same parity (even- or oddness) as the candidate sum `s`?
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
    # Report the bounds only if no candidate got past test 1 without them.
    # Otherwise the tests are the more specific reason:
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
#'   `x`, `sd`, `n`, `digits_x`, `digits_sd`, and `items` are vectorized: they
#'   may have any length, and shorter ones are recycled to the length of the
#'   longest, as long as they have length 1. All other arguments describe the
#'   test as a whole and must have length 1.
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
#' @param tolerance Numeric. Tolerance of the comparison between the reported
#'   `sd` and the reconstructed SDs, via [`dplyr::near()`]. Default is circa
#'   0.000000015 (1.490116e-08). This is documented here rather than inherited
#'   from [`grim()`], where the argument is deprecated: GRIM compares exact
#'   integers and has nothing for a tolerance to loosen, whereas GRIMMER
#'   reconstructs SDs in floating point and does.
#'
#' @inheritParams grim
#'
#' @return Logical. `TRUE` if `x`, `sd`, `n`, and `items` are mutually
#'   consistent, `FALSE` if not, and `NA` if the case cannot be decided: if any
#'   of the values is missing, if the rounding bounds are undefined, if `items`
#'   is not a positive whole number, or if `n` is not a whole number greater
#'   than `1`. GRIMMER reconstructs a *sample* SD, so it divides by `n - 1`.

#' @details GRIMMER was originally devised by Anaya (2016). The present
#'   implementation follows Allard's (2018) refined Analytic-GRIMMER algorithm.
#'   It uses a variant of Analytic-GRIMMER also implemented in
#'   \href{https://lukaswallrich.github.io/rsprite2/reference/GRIMMER_test.html}{`rsprite2::GRIMMER_test()`}
#'   that can be applied to multi-item scales and optionally takes scale ranges
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

# Vectorized version. The signature mirrors `grimmer_scalar()`'s minus
# `show_reason`; see `vectorize_test()`:
grimmer <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items = 1,
  min_val = NULL,
  max_val = NULL,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  tolerance = .Machine$double.eps^0.5
) {
  vectorize_test(
    .fun = grimmer_scalar,
    .frame = environment(),
    .along = c("x", "sd", "n", "digits_x", "digits_sd", "items")
  )
}
