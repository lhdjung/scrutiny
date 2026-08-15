# Helpers to check input ranges (not exported) ----------------------------

check_debit_inputs <- function(input, type, symbol) {
  # For all input values, check if they are between 0 and 1:
  input_in_range <- input |>
    as.numeric() |>
    dplyr::between(0, 1)

  # If at least one of the values is outside of that range, this will lead to an
  # error. First, the error message is prepared... Missing values are not
  # offenders: they are undecidable, not out of range, and the test functions
  # return `NA` for them.
  offenders <- input[!is.na(input_in_range) & !input_in_range]

  if (length(offenders) > 0L) {
    # Since the check moved into `debit_scalar()`, it most often runs on a
    # single value, one row at a time. Counting offenders out of a total is then
    # vestigial -- the value itself is the whole message:
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
# original values, so the test reconstructs the SD at each bound of the mean's
# range, rounds the results the same way the reported SD was presumably rounded,
# and checks whether the reported SD's own range is met.
#
# `bound_numerators()` supplies both ranges. It is the same helper that
# `grim_scalar()` and `grimmer_scalar()` derive their candidate ranges from, so
# all three tests agree on the bounds of a rounded number, on which rounding
# methods exist, and on what `threshold` and `symmetric` mean. It expresses each
# bound as an exact integer numerator over a common denominator, which is what
# allows the comparison below to be exact rather than tolerance-based.

#' @include utils.R sd-binary.R round.R unround.R reround.R

debit_scalar <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  formula = "mean_n",
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  show_rec = FALSE
) {
  # `reconstruct_sd()` supports four formulas, but three of them need `group_0`
  # or `group_1`, which DEBIT does not have: it works from the reported mean and
  # sample size. Passing one of them used to reach `reconstruct_sd_scalar()` and
  # fail there with R's own "argument "group_0" is missing" -- an error the
  # package didn't write, about an argument the user never saw:
  if (!identical(formula, "mean_n")) {
    cli::cli_abort(c(
      "`formula` must be \"mean_n\".",
      "x" = "It is {wrong_spec_string(formula)}.",
      "i" = "DEBIT reconstructs the SD from the reported mean and sample \\
      size. The other formulas that `reconstruct_sd()` knows need the size \\
      of one of the two groups, which DEBIT is not given.",
      "i" = "The argument is kept for the case that such data become \\
      available to the test."
    ))
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

  x_num <- as.numeric(x)
  sd_num <- as.numeric(sd)

  # A missing value makes the test undecidable, and it is returned in the same
  # shape as the undefined-bounds case below:
  if (anyNA(c(x_num, sd_num, n))) {
    if (!show_rec) {
      return(NA)
    }
    return(list(
      NA,
      rounding,
      NA_real_,
      NA,
      NA_real_,
      NA,
      NA_real_,
      NA_real_
    ))
  }

  # DEBIT reconstructs the *sample* SD of `n` binary values, so it divides by
  # `n - 1`. `n` has to be a whole number greater than 1 for that to describe
  # anything. It used to return a verdict regardless: at `n = 1`,
  # `sd_binary_mean_n()` returned `Inf`, which compared as "above the upper
  # bound" and yielded `FALSE`; at `n = 0` the factor `n / (n - 1)` was `0`, so
  # the reconstructed SD was `0`, again `FALSE`. Both are noise presented as
  # evidence. `grim_scalar()` and `grimmer_scalar()` have always reported an
  # untestable `n` as undecidable, and DEBIT now agrees with them:
  if (!is_decidable_n_items(n, min_n = 2)) {
    if (!show_rec) {
      return(NA)
    }
    return(list(
      NA,
      rounding,
      NA_real_,
      NA,
      NA_real_,
      NA,
      NA_real_,
      NA_real_
    ))
  }

  bounds_x <- bound_numerators(
    x_num = x_num,
    digits = digits_x,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  bounds_sd <- bound_numerators(
    x_num = sd_num,
    digits = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # The bounds are undefined for a missing value, and consistency is then
  # undecidable, just as it is for `grim_scalar()` and `grimmer_scalar()` in the
  # same situation:
  if (is.null(bounds_x) || is.null(bounds_sd)) {
    if (!show_rec) {
      return(NA)
    }
    return(list(
      NA,
      rounding,
      NA_real_,
      NA,
      NA_real_,
      NA,
      NA_real_,
      NA_real_
    ))
  }

  # A mean of binary data cannot lie outside of 0 and 1, so neither can the
  # original value behind a reported one, however the rounding bounds fall. Both
  # bounds are therefore clamped to that range, which only ever narrows it.
  # Without this, a mean reported as 0.00 or 1.00 had a bound just outside the
  # range, `sd_binary_mean_n()` returned `NaN` for it, and the comparison below
  # was undecidable: `debit(x = 0, sd = 0, n = 50)` was `NA` although the value
  # set is perfectly consistent -- every value is 0, so the SD is 0.
  x_lower <- max(bounds_x$lower / bounds_x$denom, 0)
  x_upper <- min(bounds_x$upper / bounds_x$denom, 1)

  # An SD cannot be negative, so a negative lower bound is really a bound of
  # zero -- and that one is attainable, hence inclusive. This is what
  # `grimmer_scalar()` does with the same bounds; DEBIT used to report the
  # negative number instead, in the `sd_lower` output column of `debit_map()`:
  if (bounds_sd$lower < 0) {
    bounds_sd$lower <- 0
    bounds_sd$incl_lower <- TRUE
  }

  sd_lower <- bounds_sd$lower / bounds_sd$denom
  sd_upper <- bounds_sd$upper / bounds_sd$denom

  # The means at which the SD is reconstructed. The two bounds are the obvious
  # candidates, but they are not enough by themselves: the verdict below reasons
  # from the reconstructed values at these means to every mean in between, and
  # that step needs the reconstruction to be monotonic in the mean.
  #
  # It isn't. `sd_binary_mean_n()` is `sqrt((n / (n - 1)) * mean * (1 - mean))`,
  # a downward parabola with its maximum at a mean of 0.5, so an interval that
  # contains 0.5 reaches SDs *above* both of its endpoints. For a mean reported
  # as exactly 0.50 the interval is symmetric around 0.5, both endpoints give
  # the same SD, and the whole attainable band collapses to a single point --
  # which is how `debit(x = 0.50, sd = 0.503, n = 100, digits_x = 2, digits_sd =
  # 3)` came to be `FALSE` for 50 ones and 50 zeros.
  #
  # The peak is the only interior extremum, so adding it where it falls inside
  # the interval makes these means span the entire attainable range again, and
  # the intermediate-value step below is sound:
  x_eval <- c(x_lower, x_upper)

  if (x_lower <= 0.5 && 0.5 <= x_upper) {
    x_eval <- c(x_eval, 0.5)
  }

  # Reconstruct the SD from each of those means... (`group_0` and `group_1`
  # would have to be passed on here to support formulas other than "mean_n")
  sd_rec <- reconstruct_sd(formula, x_eval, n)

  # ...and round it the same way the reported SD was presumably rounded, to the
  # same number of decimal places:
  sd_rec <- reround(
    x = sd_rec,
    digits = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # Test whether the reconstructed SD values meet the range of the reported SD.
  # `reround()` returned values on the `digits_sd` decimal grid, so multiplying
  # them by the bounds' denominator and rounding to the nearest integer recovers
  # their exact numerators over that same denominator: the comparison below is
  # therefore between integers. This replaces the `dustify()` fudge of +/-1e-12
  # that DEBIT used to compare bounds with -- the last floating-point comparison
  # of this kind in the package (#86).
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

  # The two conditions need not be met by the same reconstructed value: if one
  # of them is below the reported SD's range and another one is above it, some
  # mean in between reconstructs into that range. This is an intermediate-value
  # argument, and it holds because `x_eval` above spans the attainable range of
  # reconstructed SDs -- which is why the peak had to be added to it.
  consistency <- any(above_lower) && any(below_upper)

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
    x_upper
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
#' @param formula String. Formula used to compute the SD of the binary
#'   distribution. Currently, only the default, `"mean_n"`, is supported.
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
#'   always equal. Default is `FALSE`.

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

# Vectorized version. The signature mirrors `debit_scalar()`'s minus `show_rec`,
# which only the mapper tier has any use for; see `vectorize_test()`:
debit <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  formula = "mean_n",
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
