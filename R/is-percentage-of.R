# Single-case function behind `is_percentage_of()` and `is_percentage_of_map()`.
is_percentage_of_scalar <- function(
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

  check_newly_numeric(percentage, digits_percentage)

  # Counts are whole numbers, `n` must be a positive one, and a count is at most
  # its total. Anything else describes no data set that the percentage could
  # have been computed from, so the case is undecidable, as a fractional `n` is
  # under GRIM:
  if (
    !is_decidable_n_items(n) ||
      !is.finite(absolute) ||
      absolute < 0 ||
      absolute > n ||
      abs(absolute - round(absolute)) >= WHOLE_NUMBER_TOLERANCE ||
      !is.finite(percentage) ||
      is.na(digits_percentage)
  ) {
    return(NA)
  }

  # The counts out of `n` whose share would be reported as `percentage`, in
  # exact integer arithmetic -- the same range that GRIM, with `percent = TRUE`,
  # checks for containing any count at all. Here, it must contain `absolute`:
  counts <- roundwork::sum_range(
    x_num = percentage / 100,
    n_items = n,
    digits = digits_percentage + 2L,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  counts[1L] <= absolute && absolute <= counts[2L]
}


#' Is a reported percentage consistent with its count?
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `is_percentage_of()` checks whether `percentage` is what
#'   `absolute` out of `n` would have been reported as, given the number of
#'   decimal places and the rounding method.
#'
#'   This is stricter than [`grim()`] with `percent = TRUE`, which only asks
#'   whether some count out of `n` produces the percentage. Here, the count is
#'   reported too.
#'
#'   The function is vectorized, but [`is_percentage_of_map()`] is more
#'   convenient for testing many cases.
#'
#' @details The test runs in exact integer arithmetic. `absolute`,
#'   `percentage`, `n`, and `digits_percentage` are vectorized; all other
#'   arguments must have length 1.
#'
#' @section Causes of inconsistencies: A `FALSE` result means that the three
#'   values cannot all be correct as reported, not why. The cause is often
#'   trivial: the percentage may be based on another denominator than `n`,
#'   such as the number of valid responses, or on an unconventional rounding
#'   method. [`is_percentage_of_map_seq()`] with `var = "n"` shows which nearby
#'   denominators would make the percentage consistent.
#'
#' @param absolute Integer. The reported count.
#' @param percentage Numeric. The reported percentage of `n` that `absolute`
#'   is said to be, e.g., `33.3`.
#' @param n Integer. The reported total.
#' @param digits_percentage Integer. The number of decimal places in
#'   `percentage`, including trailing zeros. There is no default, since a
#'   numeric value does not keep them (`1.40` is `1.4`).
#' @inheritParams grim
#'
#' @return Logical. `TRUE` if the three values are consistent, `FALSE` if not,
#'   and `NA` if the case cannot be decided: if a value is missing or infinite,
#'   if `absolute` is not a whole number from 0 to `n`, or if `n` is not a
#'   positive whole number.
#'
#' @seealso [`grim()`] with `percent = TRUE`, for a percentage without a count.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # 40 out of 120 is 33.33...%, so it can be
#' # reported as 33 or 33.3, but not as 34:
#' is_percentage_of(40, 33, 120, digits_percentage = 0)
#' is_percentage_of(40, 33.3, 120, digits_percentage = 1)
#' is_percentage_of(40, 34, 120, digits_percentage = 0)
is_percentage_of <- function(
  absolute,
  percentage,
  n,
  digits_percentage,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  map_value_sets(
    fn = is_percentage_of_scalar,
    along = c("absolute", "percentage", "n", "digits_percentage")
  )
}
