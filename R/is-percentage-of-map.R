#' Test many reported percentages against their counts
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `is_percentage_of_map()` applies [`is_percentage_of()`] to
#'   every row of a data frame with counts, percentages, and totals.
#'
#'   Call [`audit()`] on the output for summary statistics. See
#'   [`is_percentage_of()`] for causes of inconsistencies.
#'
#' @param data Tibble with columns `absolute`, `percentage`, and `n`. Any
#'   other columns are returned alongside the results.
#' @param digits_percentage Integer. The number of decimal places in
#'   `percentage`, including trailing zeros. There is no default, since a
#'   numeric value does not keep them (`1.40` is `1.4`). Either a single
#'   number for the whole column or one number per row of `data`.
#' @param absolute,percentage,n Optionally, the names of the columns in `data`
#'   that contain these values, if they differ from the defaults.
#' @param rounding,threshold,symmetric The same as in [`grim()`].
#' @param ... These dots must be empty. Every argument after them must be
#'   named in full.
#'
#' @return A tibble with the columns `absolute`, `percentage`, `n`,
#'   `digits_percentage`, `consistency`, and any other columns from `data`. It
#'   has the class `scrutiny_is_percentage_of_map`, which is recognized by
#'   [`audit()`].
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
