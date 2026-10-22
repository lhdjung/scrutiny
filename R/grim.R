# Single-case function; not exported but used as a basis for the vectorized
# `grim()` as well as within `grim_map()`:
grim_scalar <- function(
  x,
  n,
  digits_x,
  items_averaged,
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
  # is no floating-point comparison for a tolerance to loosen.
  if (lifecycle::is_present(tolerance)) {
    # `user_env` is explicit because the user never calls this directly: left to
    # infer the caller, lifecycle finds a scrutiny frame and tells the user to
    # report an issue about their own argument.
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = paste0(name_last_export(), "(tolerance)"),
      details = "GRIM compares exact integers, so `tolerance` has no effect \\
      on its results.",
      user_env = globalenv()
    )
  }

  # `percent` describes the call as a whole. A missing value or a vector used
  # to fail in `if ()` with base R's message:
  if (!isTRUE(percent) && !isFALSE(percent)) {
    abort_in_export(
      "`percent` must be `TRUE` or `FALSE`.",
      "x" = "It is {wrap_in_quotes_or_backticks(percent)}."
    )
  }
  # A missing `n` is undecidable, but a string is a mistake: comparing it with
  # numbers used to make the verdict an unexplained `NA`.
  if (!is.numeric(n) && !is.na(n)) {
    check_type(n, c("double", "integer"))
  }

  if (missing(digits_x)) {
    error_digits_missing(x)
  }

  if (missing(items_averaged)) {
    error_items_missing()
  }
  # A plain `NA` is logical, but missing all the same:
  if (!is.numeric(items_averaged) && !is.na(items_averaged)) {
    check_type(items_averaged, c("double", "integer"))
  }

  check_newly_numeric(x, digits_x)

  # The `percent` argument allows for easy conversion of percentages to decimal
  # numbers. `x` itself stays on the reported scale for the `show_rec` values:
  x_num <- x
  if (percent) {
    x_num <- x_num / 100
    digits_x <- digits_x + 2L
  }

  # GRIM asks which integer sums of `n * items_averaged` integer values have a
  # mean that would be reported as `x`. A fractional or non-positive `n` or
  # `items_averaged` describes no such data set, so the case is undecidable
  # rather than inconsistent. So is an infinite `x`: no data set has an infinite
  # mean, and it has no decimal places to be reported with -- left to
  # `sum_range()` it would make the range infinitely wide, i.e. consistent with
  # everything. And so is a missing `digits_x`: without the decimal count there
  # are no rounding bounds to derive.
  if (
    !is_decidable_n_items(n, items_averaged) ||
      is.infinite(x_num) ||
      is.na(digits_x)
  ) {
    if (!show_rec) {
      return(NA)
    }
    return(list(NA, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
  }

  # Prepare further objects for reconstructing the original values:
  n_items <- as.numeric(n) * items_averaged
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
  rec_x_upper <- ceiling(rec_sum_num / denom) * scale_x / n_items
  rec_x_lower <- floor(rec_sum_num / denom) * scale_x / n_items

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
#' @description `grim()` checks whether a reported mean of integer data is
#'   consistent with the reported sample size and the number of items that
#'   compose the mean. The mean of `n * items_averaged` integers is a whole
#'   number divided by `n * items_averaged`, so only certain values are
#'   possible; the test asks whether any of them would have been reported as
#'   `x`.
#'
#'   Set `percent` to `TRUE` if `x` is a percentage. It is then divided by 100
#'   and tested with two more decimal places.
#'
#'   The function is vectorized, but [`grim_map()`] is more convenient for
#'   testing many cases.
#'
#' @details `digits_x` must be specified because a numeric value does not
#'   retain trailing zeros: `1.40` is stored as `1.4`. State the number of
#'   decimal places that `x` was reported with.
#'
#'   `items_averaged` must be specified because a mean over several items is a
#'   mean of more values, which changes the test. It replaces `items`, whose
#'   default of 1 let multi-item scales pass unnoticed.
#'
#'   `x`, `n`, `digits_x`, and `items_averaged` are vectorized and recycled to a
#'   common length by the tidyverse rules: an argument has length 1 or the
#'   length of the longest. All other arguments must have length 1.
#'
#'   The test decides in exact integer arithmetic, so `tolerance` has no
#'   effect and is deprecated.
#'
#' @param x Numeric. The reported mean or percentage.
#' @param n Integer. The reported sample size.
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default, since a numeric value does not
#'   keep them (`1.40` is `1.4`).
#' @param items_averaged Integer. The number of items averaged into each
#'   respondent's score: 1 for a single item or a percentage, 4 for the mean of
#'   a four-item scale. Not the number of scale points. There is no default.
#' @param percent Logical. If `TRUE`, `x` is a percentage: it is divided by
#'   100 and the decimal count is increased by 2. Default is `FALSE`.
#' @param rounding String. The rounding method assumed to have produced `x`.
#'   Default is `"up_or_down"`, which accepts either direction. See
#'   `vignette("rounding-options")`.
#' @param threshold Numeric. For `rounding = "up_from"`, `"down_from"`, or
#'   `"up_from_or_down_from"`, the point within a step at which rounding
#'   switches direction, in tenths of a step. Default is `5`. Ignored
#'   otherwise.
#' @param symmetric Logical. If `TRUE`, negative numbers are rounded like
#'   their absolute values (`-2.5` like `2.5`) under `"up"`, `"down"`, and the
#'   `"*_from"` methods. Default is `FALSE`. Must not be given with a
#'   `"ties_*"` rounding method, which already fixes the direction; see
#'   [`reround()`].
#' @param tolerance `r lifecycle::badge("deprecated")` This argument has no
#'   effect; see *Details*.
#'
#' @return Logical. `TRUE` if `x`, `n`, and `items_averaged` are consistent,
#'   `FALSE` if not, and `NA` if the case cannot be decided: if a value is
#'   missing or infinite, or if `n` or `items_averaged` is not a positive whole
#'   number.
#'
#' @seealso [`grim_map()`] for data frames, [`grim_values()`] for the means
#'   that `n` integers can produce, and [`grim_probability()`] for how
#'   informative the test is in a given case.
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
#' grim(x = 5.19, n = 28, digits_x = 2, items_averaged = 1)
#'
#' # However, it is consistent with a sample size of 32:
#' grim(x = 5.19, n = 32, digits_x = 2, items_averaged = 1)
#'
#' # For a scale composed of two items:
#' grim(x = 2.84, n = 16, digits_x = 2, items_averaged = 2)
#'
#' # With percentages instead of means -- here, 71%:
#' grim(x = 71, n = 43, digits_x = 0, items_averaged = 1, percent = TRUE)

# Vectorized version. The signature mirrors `grim_scalar()`'s minus `show_rec`;
# see `vectorize_test()`:
grim <- function(
  x,
  n,
  digits_x,
  items_averaged,
  percent = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  tolerance = lifecycle::deprecated()
) {
  vectorize_test(
    fun = grim_scalar,
    frame = environment(),
    along = c("x", "n", "digits_x", "items_averaged")
  )
}
