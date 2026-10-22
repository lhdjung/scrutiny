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

# The SD's rounding bounds as exact integer numerators over a common
# denominator, the same way `sum_range()` derives the mean's bounds, plus the
# `(n - 1) * sd^2 * items_averaged^2` part of the sum of squares at each bound.
# That part does not depend on the candidate sum, so it is computed once, here.
# `NULL` if the bounds are undefined, i.e. for a missing SD. Shared with
# `grimmer_explain()`.

grimmer_sd_bounds <- function(
  sd,
  n,
  items_averaged,
  digits_sd,
  rounding,
  threshold,
  symmetric
) {
  sd_bounds <- bound_numerators(
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

  sd_bounds$term_lower <- sd_square_term(
    sd_bounds$lower,
    n,
    items_averaged,
    sd_bounds$denom
  )
  sd_bounds$term_upper <- sd_square_term(
    sd_bounds$upper,
    n,
    items_averaged,
    sd_bounds$denom
  )

  sd_bounds
}


# GRIMMER's tests for a single candidate sum `s`. Returns the range of whole
# sums of squares that the SD admits and whether `s` passes each test. `grimmer_explain()` reports these very values, so it cannot
# contradict the verdict.

grimmer_candidate <- function(
  s,
  n,
  items_averaged,
  sd_bounds
) {
  # TEST 1: Is there at least one integer between the bounds of the
  # reconstructed sum of squares? Derived in exact integer arithmetic, like the
  # candidate sums: a floating-point tolerance cannot repair a bound once the
  # sum of squares exceeds about 1000 (#86).
  sum_squares <- sum_squares_range(
    s = s,
    n = n,
    term_lower = sd_bounds$term_lower,
    term_upper = sd_bounds$term_upper,
    incl_lower = sd_bounds$incl_lower,
    incl_upper = sd_bounds$incl_upper
  )

  # Test 1 goes beyond Allard (2018) in also requiring a sum of squares that
  # whole numbers with the sum `s` can reach. Otherwise, it would accept value
  # sets like `x = 3.6`, `sd = 0.2`, `n = 11`: the sum must be 40, the SD admits
  # a sum of squares of 146, but no 11 whole numbers summing to 40 go below 148.
  minimum <- sum_squares_min(s, n)
  lower <- max(sum_squares[1L], minimum)

  out <- list(
    sum_squares = sum_squares,
    sum_squares_min = minimum,
    pass_test1 = FALSE,
    pass_test3 = FALSE
  )

  if (lower > sum_squares[2L]) {
    return(out)
  }

  out$pass_test1 <- TRUE

  # TEST 2 of Allard (2018) rounds each admissible sum of squares back to an SD
  # and compares it to the reported one. Test 1 implies it: the bounds are the
  # exact inverse of that rounding, so every integer between them passes. It
  # used to be run anyway, with a floating-point tolerance that could only ever
  # lose matches, so it is not run at all.

  # TEST 3: The sum of squares of whole numbers has the parity of their sum
  # (`k^2` is odd exactly if `k` is). Two or more admissible integers always
  # include one of each parity; a single one must have that of `s`. If that is
  # the minimum, it does, being the sum of squares of whole numbers:
  out$pass_test3 <-
    sum_squares[2L] > lower ||
    lower %% 2 == s %% 2

  # For five or more numbers, the minimum and the parity suffice: they can have
  # every sum of squares that passes both. For fewer, some of these are still
  # impossible. A range with none that is possible fails test 1 after all, like
  # one that ends below the minimum:
  if (n < 5 && out$pass_test3) {
    out$attained <- sum_squares_attainable(s, n, lower, sum_squares[2L])
    if (is.na(out$attained)) {
      out$pass_test1 <- FALSE
      out$pass_test3 <- FALSE
    }
  }

  out
}


grimmer_scalar <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  show_reason = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  tolerance = lifecycle::deprecated()
) {
  # GRIMMER decides in exact integer arithmetic, like GRIM: see test 2 below.
  if (lifecycle::is_present(tolerance)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "grimmer(tolerance)",
      details = "GRIMMER compares exact integers, so `tolerance` has no \\
      effect on its results.",
      user_env = globalenv()
    )
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

  if (missing(items_averaged)) {
    error_items_missing()
  }
  # A plain `NA` is logical, but missing all the same:
  if (!is.numeric(items_averaged) && !is.na(items_averaged)) {
    check_type(items_averaged, c("double", "integer"))
  }

  check_newly_numeric(x, digits_x)
  check_newly_numeric(sd, digits_sd)

  # Undecidable. Caught before the GRIM test below, whose `NA` result the branch
  # on `pass_grim` could not handle. A missing decimal count is a missing value
  # like any other: no rounding bounds follow from it.
  if (
    is.na(x) || is.na(sd) || is.na(n) || is.na(digits_x) || is.na(digits_sd)
  ) {
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

  # GRIMMER inherits GRIM's requirement that `n` and `items_averaged` be
  # positive whole numbers, and adds `min_n = 2`: it reconstructs a *sample* SD,
  # so it divides by `n - 1`.
  if (!is_decidable_n_items(n, items_averaged, min_n = 2)) {
    if (show_reason) {
      return(list(NA, "No testable value set"))
    }
    return(NA)
  }

  # No data set has a negative SD, so no data set has this one. Squaring it
  # below would hide that.
  if (sd < 0) {
    if (show_reason) {
      return(list(FALSE, "Negative SD"))
    }
    return(FALSE)
  }

  n_items <- as.numeric(n) * items_averaged

  # GRIM TEST: the original `n` because `items_averaged` is passed down
  # separately.
  pass_grim <- grim_scalar(
    x = x,
    n = n,
    digits_x = digits_x,
    items_averaged = items_averaged,
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

  sd_bounds <- grimmer_sd_bounds(
    sd = sd,
    n = n,
    items_averaged = items_averaged,
    digits_sd = digits_sd,
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

  check_enumeration_size(sums_consistent, n = n, digits_x = digits_x)

  consistent_sums <- sums_consistent[1L]:sums_consistent[2L]

  # Run the GRIMMER tests for each candidate sum, returning `TRUE` as soon as
  # one passes them all. For `show_reason`, track whether any candidate passed
  # test 1:
  any_passed_test1 <- FALSE

  for (s in consistent_sums) {
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
      if (show_reason) {
        return(list(TRUE, "Passed all"))
      }
      return(TRUE)
    }
  }

  # No candidate sum passed test 1 and test 3.
  if (show_reason) {
    if (any_passed_test1) {
      return(list(FALSE, "GRIMMER inconsistent (test 3)"))
    }
    return(list(FALSE, "GRIMMER inconsistent (test 1)"))
  }

  FALSE
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
#' @details `x`, `sd`, `n`, `digits_x`, `digits_sd`, and `items_averaged` are
#'   vectorized and recycled to a common length by the tidyverse rules. All
#'   other arguments must have length 1.
#'
#'   The implementation follows Allard's (2018) Analytic-GRIMMER algorithm, as
#'   does `rsprite2::GRIMMER_test()`, with a stricter first test: the sum of
#'   squares must be one that `n` whole numbers with the reconstructed sum can
#'   have. For `n >= 5`, this means at least the smallest such sum of squares;
#'   for smaller `n`, some larger ones are impossible, too. With this test,
#'   `grimmer()` is `TRUE` if and only if a sample of whole numbers has the
#'   reported mean and SD. The one exception is `rounding = "even"`, which
#'   takes a value on a rounding bound to round either way, as in [`unround()`],
#'   so it may also accept a value set that a sample only rounds to if ties are
#'   broken the other way. GRIMMER was first proposed by Anaya (2016).
#'
#'   For data on a bounded scale, such as a Likert scale, see the
#'   [strait](https://github.com/ianhussey/strait) package. Its `brimmest()`
#'   decides exactly whether a mean and SD are possible within the bounds.
#'
#' @param x Numeric. The reported mean.
#' @param sd Numeric. The reported standard deviation.
#' @param n Integer. The reported sample size.
#' @param digits_sd Integer. The number of decimal places in `sd`, including
#'   trailing zeros. There is no default, since a numeric value does not
#'   keep them (`1.40` is `1.4`).
#' @param items_averaged Integer. As in [`grim()`], for `x` and `sd`.
#' @param tolerance `r lifecycle::badge("deprecated")` This argument has no
#'   effect: the test decides in exact integer arithmetic.
#'
#' @inheritParams grim
#'
#' @return Logical. `TRUE` if `x`, `sd`, `n`, and `items_averaged` are
#'   consistent, `FALSE` if not, and `NA` if the case cannot be decided: if a
#'   value is missing or infinite, if `items_averaged` is not a positive whole
#'   number, or if `n` is not a whole number greater than `1`. GRIMMER
#'   reconstructs a sample SD and so divides by `n - 1`.
#'
#' @references Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
#'   possibility of standard deviations.
#'   https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/
#'
#'   Anaya, J. (2016). The GRIMMER test: A method for testing the validity of
#'   reported measures of variability. *PeerJ Preprints.*
#'   https://peerj.com/preprints/2400v1/
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
#'   items_averaged = 1
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
#'   items_averaged = 1
#' )
#'
#' # For a scale composed of two items:
#' grimmer(
#'   x = 2.74,
#'   sd = 0.96,
#'   n = 63,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 2
#' )

# Vectorized version. The signature mirrors `grimmer_scalar()`'s minus
# `show_reason`; see `vectorize_test()`:
grimmer <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  tolerance = lifecycle::deprecated()
) {
  vectorize_test(
    fun = grimmer_scalar,
    frame = environment(),
    along = c("x", "sd", "n", "digits_x", "digits_sd", "items_averaged")
  )
}
