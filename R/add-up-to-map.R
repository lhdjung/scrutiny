#' Test whether reported counts add up to their totals
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `add_up_to_map()` applies [`add_up_to()`] to every row of a data
#'   frame. `summands` selects the columns to add up with tidyselect, so any
#'   number of columns can be added up.
#'
#'   Call [`audit()`] on the output for summary statistics.
#'
#' @param data Tibble with a `total` column and the columns to add up. Any
#'   other columns are returned alongside the results.
#' @param summands Columns of `data` to add up, selected with tidyselect, such
#'   as `starts_with("n_")`. There is no default.
#' @param total Optionally, the name of the column in `data` that contains the
#'   totals, if it differs from the default.
#' @param ... These dots must be empty. Every argument after them must be
#'   named in full.
#'
#' @return A tibble with the `summands` columns, `total`, `consistency`, and
#'   any other columns from `data`. It has the class `scrutiny_add_up_to_map`,
#'   which is recognized by [`audit()`].
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
