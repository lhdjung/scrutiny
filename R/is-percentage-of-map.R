#' Test many reported percentages against their counts at once
#'
#' @description Call `is_percentage_of_map()` to apply [`is_percentage_of()`]
#'   to every row of a data frame with reported counts, percentages, and
#'   totals.
#'
#'   For summary statistics, call [`audit()`] on the results.
#'
#'   For possible causes of inconsistencies, see [`is_percentage_of()`].
#'
#' @param data Tibble with columns `absolute`, `percentage`, and `n`. Any
#'   other columns in `data` will be returned alongside the test results.
#' @param digits_percentage Integer. The number of decimal places in
#'   `percentage`, including trailing zeros. Use a single number if the whole
#'   column was reported with the same number of decimal places, or one number
#'   per row of `data` if it varies.
#' @param absolute,percentage,n Optionally, specify these arguments as
#'   column names in `data`.
#' @param rounding,threshold,symmetric Further parameters of the test; see
#'   [`grim()`].
#' @param ... Arguments passed down to [`is_percentage_of()`].
#'
#' @return A tibble with the columns `absolute`, `percentage`, `n`,
#'   `digits_percentage`, `consistency`, and any other columns from `data`. It
#'   has the `scrutiny_is_percentage_of_map` class, which is recognized by the
#'   [`audit()`] generic.
#'
#' @include is-percentage-of.R function-map.R
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   absolute = c(40, 37, 25),
#'   percentage = c(33, 31, 20),
#'   n = 120
#' )
#'
#' df |>
#'   is_percentage_of_map(digits_percentage = 0)
is_percentage_of_map <- function_map(
  .fun = is_percentage_of_scalar,
  .reported = c("absolute", "percentage", "n"),
  .name_test = "is_percentage_of",
  .args_by_row = "digits_percentage"
)
