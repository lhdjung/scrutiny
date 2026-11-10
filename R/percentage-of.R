# Single-case function behind `percentage_of()` and `percentage_of_map()`.
percentage_of_scalar <- function(
  absolute,
  percentage,
  n,
  digits_percentage,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  # A missing value is undecidable, but a string is a mistake:
  if (!is.numeric(absolute) && !is.na(absolute)) {
    check_type(absolute, c("double", "integer"))
  }
  if (!is.numeric(n) && !is.na(n)) {
    check_type(n, c("double", "integer"))
  }

  if (missing(digits_percentage)) {
    error_digits_missing(percentage)
  }

  check_newly_numeric(
    percentage,
    digits_percentage,
    scale = rounding_scale(rounding, threshold, symmetric)
  )

  # Counts are whole numbers, and `n` must be a positive one. Anything else
  # describes no data set that the percentage could have been computed from, so
  # the case is undecidable, as a fractional `n` is under GRIM. A count above
  # its total is tested like any other, as for answers to a question that allows
  # several, whose percentages can sum to more than 100. A missing count is
  # checked below:
  absolute_missing <- is.na(absolute)
  n_missing <- is.na(n)
  if (
    (!n_missing && !is_decidable_n_items(n)) ||
      (!absolute_missing &&
        (!is.finite(absolute) ||
          absolute < 0 ||
          abs(absolute - round(absolute)) >= WHOLE_NUMBER_TOLERANCE)) ||
      !is.finite(percentage)
  ) {
    return(NA)
  }

  # A missing decimal count decides nothing if the case is inconsistent even
  # with the fewest decimal places that `percentage` can have, or consistent
  # even with so many that only `percentage` itself is left; see
  # `digits_least()` and `digits_exact_mean()`. Without `n`, that is a
  # percentage of 0, which a count of 0 has out of any `n`. An error is about a
  # size that the test refuses, which leaves the case open:
  if (is.na(digits_percentage)) {
    test_digits <- function(digits) {
      tryCatch(
        percentage_of_scalar(
          absolute = absolute,
          percentage = percentage,
          n = n,
          digits_percentage = digits,
          rounding = rounding,
          threshold = threshold,
          symmetric = symmetric
        ),
        error = function(cnd) NA
      )
    }
    digits_shown <- digits_least(percentage, digits_percentage)
    at_least <- test_digits(digits_shown)
    if (!isTRUE(at_least)) {
      return(if (isFALSE(at_least)) FALSE else NA)
    }
    digits_most <- if (!n_missing) {
      digits_exact_mean(digits_shown, as.numeric(n))
    } else if (percentage == 0) {
      digits_shown
    }
    if (!is.null(digits_most) && isTRUE(test_digits(digits_most))) {
      return(TRUE)
    }
    return(NA)
  }

  if (n_missing) {
    return(percentage_of_any_n(
      absolute,
      percentage,
      digits_percentage,
      rounding,
      threshold,
      symmetric
    ))
  }

  # The counts out of `n` whose share would be reported as `percentage`, in
  # exact integer arithmetic -- the same range that GRIM, with `x_meaning =
  # "percentage"`, checks for containing any count at all. Here, it must contain
  # `absolute`:
  check_sum_range_exact(
    percentage / 100,
    n,
    digits_percentage + 2L,
    arg = "percentage",
    hint_items = FALSE,
    scale = rounding_scale(rounding, threshold, symmetric)
  )
  counts <- roundwork::sum_range(
    x_num = percentage / 100,
    n_items = n,
    digits = digits_percentage + 2L,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # A missing count could decide the case either way only if some count has
  # the percentage:
  if (absolute_missing) {
    return(if (max(counts[1L], 0) <= counts[2L]) NA else FALSE)
  }

  counts[1L] <= absolute && absolute <= counts[2L]
}


# `percentage_of_scalar()` with `n` missing. A count of 0 has a share of 0
# out of any `n`, so it is consistent with every `n` or with none. Any other
# count is a share of `absolute / n`, which is within the rounding bounds
# `lower / denom` to `upper / denom` exactly for an `n` from
# `absolute * denom / upper` to `absolute * denom / lower`. If no whole number
# is in that range, no `n` passes; if one is, larger and smaller ones fail,
# so the case is open. Products past 2^53 are not exact, which leaves it open
# as well: the least such `n` times each bound, and `n` is at most two more than
# the ratio of the count's numerator to the upper bound.
percentage_of_any_n <- function(
  absolute,
  percentage,
  digits,
  rounding,
  threshold,
  symmetric
) {
  bounds <- roundwork::bound_numerators(
    percentage / 100,
    digits + 2L,
    rounding,
    threshold,
    symmetric
  )
  lower <- bounds$lower
  upper <- bounds$upper

  # With the count missing, too, some count and `n` give any share from 0 on,
  # and others don't; none give a share below 0:
  if (is.na(absolute)) {
    below_zero <- upper < 0 || (upper == 0 && !bounds$incl_upper)
    return(if (below_zero) FALSE else NA)
  }

  if (absolute == 0) {
    above_lower <- lower < 0 || (lower == 0 && bounds$incl_lower)
    below_upper <- upper > 0 || (upper == 0 && bounds$incl_upper)
    return(above_lower && below_upper)
  }
  if (upper <= 0) {
    return(FALSE)
  }

  target <- absolute * bounds$denom
  if (target >= 2^53 || (target / upper + 2) * max(abs(lower), upper) >= 2^53) {
    return(NA)
  }
  # The least `n` whose share is below the upper bound, or at it if that is
  # inclusive:
  n_least <- max(1, ceiling(target / upper))
  if (n_least * upper < target) {
    n_least <- n_least + 1
  }
  if (n_least * upper == target && !bounds$incl_upper) {
    n_least <- n_least + 1
  }
  # Larger ones only lower the share, so it passes if this one does:
  passes <- if (bounds$incl_lower) {
    n_least * lower <= target
  } else {
    n_least * lower < target
  }
  if (passes) NA else FALSE
}


#' Is a reported percentage consistent with its count?
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `percentage_of()` checks whether `percentage` is what
#'   `absolute` out of `n` would have been reported as, given the number of
#'   decimal places and the rounding method.
#'
#'   This is stricter than [`grim()`] with `x_meaning = "percentage"`, which
#'   only asks whether some count out of `n` produces the percentage. Here, the
#'   count is reported too.
#'
#'   The function is vectorized, but [`percentage_of_map()`] is more
#'   convenient for testing many cases.
#'
#' @details The test runs in exact integer arithmetic. `absolute`,
#'   `percentage`, `n`, and `digits_percentage` are vectorized; all other
#'   arguments must have length 1.
#'
#'   An `absolute` above `n` is tested like any other, as for answers to a
#'   question that allows several, whose percentages can sum to more than 100.
#'
#' @section Causes of inconsistencies: A `FALSE` result means that the three
#'   values cannot all be correct as reported, but not why. The cause is often
#'   trivial: the percentage may be based on another denominator than `n`,
#'   such as the number of valid responses, or on an unconventional rounding
#'   method. [`percentage_of_map_seq()`] with `var = "n"` shows which nearby
#'   denominators would make the percentage consistent.
#'
#' @param absolute Integer. The reported count.
#' @param percentage Numeric. The reported percentage of `n` that `absolute`
#'   is said to be, e.g., `33.3`.
#' @param n Integer. The reported total.
#' @param digits_percentage Integer. The number of decimal places in
#'   `percentage`, including trailing zeros. There is no default, since a
#'   numeric value does not keep them (`1.40` is `1.4`).
#' @param rounding String. The rounding method assumed to have produced
#'   `percentage`. Default is `"up_or_down"`, which accepts either direction.
#'   See [`grim()`] for the other methods.
#' @inheritParams grim
#'
#' @return Logical. `TRUE` if the three values are consistent, `FALSE` if not,
#'   and `NA` if the case cannot be decided: if a value is infinite, if
#'   `absolute` is not a non-negative whole number, if `n` is not a positive
#'   whole number, or if a missing value could decide the case either way. A
#'   missing `absolute` gives `FALSE` if no count out of `n` has the
#'   percentage, and a missing `n` gives `FALSE` if no `n` makes `absolute`
#'   that percentage. A missing `digits_percentage` is treated as in
#'   [`grim()`].
#'
#' @seealso [`grim()`] with `x_meaning = "percentage"`, for a percentage without
#'   a count.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # 40 out of 120 is 33.33...%, so it can be
#' # reported as 33 or 33.3, but not as 34:
#' percentage_of(40, 33, 120, digits_percentage = 0)
#' percentage_of(40, 33.3, 120, digits_percentage = 1)
#' percentage_of(40, 34, 120, digits_percentage = 0)
percentage_of <- function(
  absolute,
  percentage,
  n,
  digits_percentage,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  map_value_sets(
    fn = percentage_of_scalar,
    along = c("absolute", "percentage", "n", "digits_percentage")
  )
}
