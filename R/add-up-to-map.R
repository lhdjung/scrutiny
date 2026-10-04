#' Test if many reported counts add up to their totals
#'
#' @description Call `add_up_to_map()` to apply [`add_up_to()`] to every row of
#'   a data frame. `summands` selects the columns to add up, as many as the
#'   data has, using tidyselect syntax.
#'
#'   For summary statistics, call [`audit()`] on the results.
#'
#' @param data Tibble with a `total` column and the columns to add up. Any
#'   other columns in `data` will be returned alongside the test results.
#' @param summands Columns of `data` whose values in each row should add up to
#'   that row's `total`, selected with tidyselect, such as `starts_with("n_")`.
#'   There is no default.
#' @param total Optionally, specify this argument as a column name in `data`.
#' @param ... These dots must be empty.
#'
#' @return A tibble with the `summands` columns, `total`, `consistency`, and
#'   any other columns from `data`. It has the `scrutiny_add_up_to_map` class, which
#'   is recognized by the [`audit()`] generic.
#'
#' @include add-up-to.R function-map.R
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   n_control = c(40, 25, 31),
#'   n_treatment = c(37, 26, 30),
#'   n_waitlist = c(43, 24, 30),
#'   total = c(120, 75, 90)
#' )
#'
#' df |>
#'   add_up_to_map(summands = starts_with("n_"))
add_up_to_map <- function_map(
  .fun = add_up_to_scalar,
  .reported = "total",
  .reported_variadic = "summands",
  .name_test = "add_up_to"
)
