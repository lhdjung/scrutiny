# Single-case function; not exported but used as a basis for the vectorized
# `is_percentage_of()` as well as within `is_percentage_of_map()`:
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

  # Counts are whole numbers, and `n` must be a positive one. Anything else
  # describes no data set that the percentage could have been computed from, so
  # the case is undecidable, as a fractional `n` is under GRIM:
  if (
    !is_decidable_n_items(n) ||
      !is.finite(absolute) ||
      absolute < 0 ||
      abs(absolute - round(absolute)) >= WHOLE_NUMBER_TOLERANCE ||
      !is.finite(percentage) ||
      is.na(digits_percentage)
  ) {
    return(NA)
  }

  # The counts out of `n` whose share would be reported as `percentage`, in
  # exact integer arithmetic -- the same range that GRIM, with `percent = TRUE`,
  # checks for containing any count at all. Here, it must contain `absolute`:
  counts <- sum_range(
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
#' @description `is_percentage_of()` checks if `percentage` is what `absolute`
#'   out of `n` would have been reported as, given the number of decimal
#'   places in `percentage` and the rounding method.
#'
#'   This is stricter than GRIM with `percent = TRUE`, which only asks whether
#'   *some* count out of `n` produces `percentage`. Here, the count is
#'   reported as well, and it must be that count.
#'
#'   The function is vectorized, but it is recommended to use
#'   [`is_percentage_of_map()`] for testing multiple cases.
#'
#' @details The test runs in exact integer arithmetic, so a count right on a
#'   rounding boundary is included or excluded as `rounding` demands, not as
#'   floating-point error happens to dictate.
#'
#'   `absolute`, `percentage`, `n`, and `digits_percentage` are vectorized;
#'   all other arguments must have length 1.
#'
#' @section Causes of inconsistencies: A `FALSE` result means that the three
#'   values can't all be correct as reported. It doesn't show why. The cause
#'   may be trivial. For example, the percentage may be based on a different
#'   denominator than `n`, such as the number of valid responses. Or the
#'   authors' software may have rounded in a different way; try other
#'   `rounding` methods.
#'
#'   [`is_percentage_of_map_seq()`] with `var = "n"` shows which nearby
#'   denominators would make the percentage consistent.
#'
#' @param absolute Integer. The reported count, e.g., the number of
#'   participants in a subgroup.
#' @param percentage Numeric. The reported percentage of `n` that
#'   `absolute` is said to be, e.g., `33.3`.
#' @param n Integer. The reported total, such as the sample size.
#' @param digits_percentage Integer. The number of decimal places in
#'   `percentage`, including trailing zeros. There is no default because it
#'   cannot be inferred from a numeric `percentage`: both `33.3` and `33.30` are
#'   the number `33.3`.
#' @inheritParams grim
#'
#' @return Logical. `TRUE` if the three values are mutually consistent,
#'   `FALSE` if not, and `NA` if the case cannot be decided: if any value is
#'   missing, if `absolute` is not a non-negative whole number, or if `n`
#'   is not a positive whole number.
#'
#' @seealso [`grim()`] with `percent = TRUE`, for a percentage without a count.
#'
#' @include utils.R unround.R
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
  vectorize_test(
    fun = is_percentage_of_scalar,
    frame = environment(),
    along = c("absolute", "percentage", "n", "digits_percentage")
  )
}
