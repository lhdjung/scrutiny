# Single-case function; not exported but used as a basis for the vectorized
# `grim()` as well as within `grim_map()`:
grim_scalar <- function(
  x,
  n,
  digits_x,
  items = 1,
  percent = FALSE,
  show_rec = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  # `lifecycle::deprecated()`, not the bare `deprecated()` that the import
  # provides: this default is copied into every mapper's formals by
  # `function_map()`, and R CMD check reads it there, outside the namespace that
  # resolves the bare name. `grim()` states it in its own signature.
  tolerance = lifecycle::deprecated()
) {
  # GRIM decides which sums are consistent in exact integer arithmetic, so there
  # is no floating-point comparison for a tolerance to loosen. `grimmer()` does
  # use it; `debit()` never had it.
  if (lifecycle::is_present(tolerance)) {
    # `user_env` is explicit because the user never calls this directly: left to
    # infer the caller, lifecycle finds a scrutiny frame and tells the user to
    # report an issue about their own argument.
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "grim(tolerance)",
      details = "GRIM compares exact integers, so `tolerance` has no effect \\
      on its results. `grimmer()` still takes it, and uses it.",
      user_env = globalenv()
    )
  }

  check_type(items, c("double", "integer"))
  check_type(percent, "logical")

  if (missing(digits_x)) {
    error_digits_missing(x)
  }

  check_newly_numeric(x, digits_x)

  x_num <- as.numeric(x)

  # The `percent` argument allows for easy conversion of percentages to decimal
  # numbers:
  if (percent) {
    x_num <- x_num / 100
    digits_x <- digits_x + 2L
  }

  # GRIM asks which integer sums of `n * items` integer values have a mean that
  # would be reported as `x`. A fractional or non-positive `n` or `items`
  # describes no such data set, so the case is undecidable rather than
  # inconsistent. So is an infinite `x`: no data set has an infinite mean, and
  # it has no decimal places to be reported with -- left to `sum_range()` it
  # would make the range infinitely wide, i.e. consistent with everything.
  if (!is_decidable_n_items(n, items) || is.infinite(x_num)) {
    if (!show_rec) {
      return(NA)
    }
    return(list(NA, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
  }

  # Prepare further objects for reconstructing the original values:
  n_items <- n * items
  rec_sum <- x_num * n_items

  # The integer sums whose mean would have been reported as `x_num` at
  # `digits_x` decimal places, in exact integer arithmetic: a sum sitting right
  # on a rounding boundary is included or excluded as the rounding method
  # demands, not as representation error happens to dictate.
  sums_consistent <- sum_range(
    x_num = x_num,
    n_items = n_items,
    digits = digits_x,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # `x` is GRIM-consistent if at least one integer sum falls into that range.
  # Equivalent to the classic formulation via the granules below: the range
  # contains `rec_sum`, so if it contains any integer, it contains one of the
  # two closest to `rec_sum`.
  consistency <- sums_consistent[1L] <= sums_consistent[2L]

  if (!show_rec) {
    return(consistency)
  }

  # The possible mean or percentage values ("granules"): `floor(rec_sum)` and
  # `ceiling(rec_sum)` over `n_items`, but via exact division so that a
  # mathematically integral `rec_sum` is not moved to its neighbor by
  # floating-point error. They depend on `rec_sum` and `n_items` alone, not on
  # the rounding method:
  denom <- 10^(digits_x + 1L)
  rec_sum_num <- round(x_num * denom) * n_items

  # These are read against `x`, so they are returned on its scale -- percentages
  # if `x` is one, like `grim_values()` and `grim_closest()`. `rec_sum`,
  # `sum_lower`, and `sum_upper` are not converted: they are sums of the
  # underlying data, which `percent` does not change. The factor multiplies the
  # integer sum rather than the quotient, keeping the granule the result of a
  # single division; converting afterwards would round twice.
  scale_x <- if (percent) 100 else 1
  rec_x_upper <- ceiling_div(rec_sum_num, denom) * scale_x / n_items
  rec_x_lower <- floor_div(rec_sum_num, denom) * scale_x / n_items

  # The same six values for every rounding method. `sum_lower` and `sum_upper`
  # are the numbers that decided `consistency` above, so the display cannot
  # contradict the verdict -- which a separately derived, granule-based one did.
  # An empty range means an inconsistent value set, and it is then empty by
  # exactly one, the two bounds straddling `rec_sum`. So the gap is no measure
  # of how far off a value set is; `x` against `rec_x_lower`/`rec_x_upper` is.
  list(
    consistency,
    rec_sum,
    sums_consistent[1L],
    sums_consistent[2L],
    rec_x_upper,
    rec_x_lower
  )
}


#' The GRIM test (granularity-related inconsistency of means)
#'
#' @description `grim()` checks if a reported mean value of integer data is
#'   mathematically consistent with the reported sample size and the number of
#'   items that compose the mean value.
#'
#'   Set `percent` to `TRUE` if `x` is a percentage. This will convert `x` to a
#'   decimal number and adjust the decimal count accordingly.
#'
#'   The function is vectorized, but it is recommended to use [`grim_map()`] for
#'   testing multiple cases.
#'
#' @details `digits_x` needs to be specified because trailing zeros are as
#'   important for the GRIM test as any other decimal digits, and numeric values
#'   don't retain them: `1.40` is stored as the number `1.4`. State the number
#'   of decimal places that `x` was reported with, not the number that happen to
#'   survive in the numeric value.
#'
#'   Browse the source code in the grim.R file. `grim()` is a vectorized version
#'   of the internal `grim_scalar()` function found there.
#'
#'   `x`, `n`, `digits_x`, and `items` are vectorized: they may have any length,
#'   and shorter ones are recycled to the length of the longest, as long as they
#'   have length 1. All other arguments describe the test as a whole and must
#'   have length 1.
#'
#'   `grim()` decides which reconstructed means are consistent with `x` using
#'   exact integer arithmetic, so there is no floating-point comparison for a
#'   `tolerance` to loosen. The argument is deprecated for that reason.
#'   [`grimmer()`] does compare reconstructed SDs with [`dplyr::near()`] and
#'   still takes it; [`debit()`] compares exact integers, like `grim()`, and
#'   never had it.
#'
#' @param x Numeric. The reported mean or percentage value.
#' @param n Integer. The reported sample size.
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default because it cannot be inferred from a
#'   numeric `x`, which has no trailing zeros: both `1.4` and `1.40` are the
#'   number `1.4`, but only the latter has `digits_x = 2`.
#' @param items Numeric. The number of items composing `x`. Default is 1, the
#'   most common case.
#' @param percent Logical. Set `percent` to `TRUE` if `x` is a percentage. This
#'   will convert it to a decimal number and adjust the decimal count (i.e.,
#'   increase it by 2). Default is `FALSE`.
#' @param rounding String. Rounding method or methods to be used for
#'   reconstructing the values to which `x` will be compared. Default is
#'   `"up_or_down"` (from 5).
#' @param threshold Numeric. If `rounding` is set to `"up_from"`, `"down_from"`,
#'   or `"up_from_or_down_from"`, set `threshold` to the number from which the
#'   reconstructed values should then be rounded up or down. Otherwise, this
#'   argument plays no role. Default is `5`.
#' @param symmetric Logical. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers with `"up"`, `"down"`, `"up_from"`, or `"down_from"`
#'   should mirror that of positive numbers so that their absolute values are
#'   always equal. Default is `FALSE`. It must not be given with any of the
#'   `"ties_*"` methods, which already name a complete tie-breaking procedure;
#'   see [`reround()`].
#' @param tolerance `r lifecycle::badge("deprecated")` GRIM compares exact
#'   integers, so this never had an effect on its results. See *Details*.
#'
#' @return Logical. `TRUE` if `x`, `n`, and `items` are mutually consistent,
#'   `FALSE` if not, and `NA` if the case cannot be decided: if any of the
#'   values is missing, or if `n` or `items` is not a positive whole number.
#'   GRIM asks which integer sums of `n * items` integer values have a mean
#'   that would be reported as `x`, and a fractional or non-positive `n` or
#'   `items` describes no such data set.
#'
#' @seealso [`grim_map()`] applies `grim()` to any number of cases at once.
#'
#' @include utils.R decimal-places.R reround.R
#'
#' @rdname grim
#'
#' @export
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876
#'
#' @examples
#' # A mean of 5.19 is not consistent with a sample size of 28:
#' grim(x = 5.19, n = 28, digits_x = 2)
#'
#' # However, it is consistent with a sample size of 32:
#' grim(x = 5.19, n = 32, digits_x = 2)
#'
#' # For a scale composed of two items:
#' grim(x = 2.84, n = 16, digits_x = 2, items = 2)
#'
#' # With percentages instead of means -- here, 71%:
#' grim(x = 71, n = 43, digits_x = 0, percent = TRUE)

# Vectorized version. The signature mirrors `grim_scalar()`'s minus `show_rec`;
# see `vectorize_test()`:
grim <- function(
  x,
  n,
  digits_x,
  items = 1,
  percent = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  tolerance = lifecycle::deprecated()
) {
  vectorize_test(
    .fun = grim_scalar,
    .frame = environment(),
    .along = c("x", "n", "digits_x", "items")
  )
}
