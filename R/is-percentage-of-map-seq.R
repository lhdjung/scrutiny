#' Test reported percentages against their counts with dispersed inputs
#'
#' @description `is_percentage_of_map_seq()` applies [`is_percentage_of()`] to
#'   values surrounding the reported ones. This shows whether a small error in
#'   the count, the percentage, or the sample size may be responsible for an
#'   inconsistency.
#'
#'   Call [`audit_seq()`] on the results for summary statistics.
#'
#' @param data A data frame that [`is_percentage_of_map()`] could take.
#' @param var String. Names of the columns that will be dispersed. Default is
#'   `c("absolute", "percentage", "n")`.
#' @param out_min,out_max If specified, output will be restricted so that it's
#'   not below `out_min` or above `out_max`. Both default to `"auto"`, which
#'   keeps `absolute` and `percentage` at 0 or above and `n` at 1 or above.
#'   A number applies to every dispersed variable, and `NULL` removes the limit.
#' @param ... Arguments passed down to [`is_percentage_of_map()`].
#' @inheritParams is_percentage_of_map
#' @inheritParams grim_map_seq
#'
#' @return A tibble (data frame) with detailed test results. `diff_var` is the
#'   number of steps between the tested value and the reported one, `case` the
#'   row number of the reported value set in `data`, and `var` the dispersed
#'   variable.
#'
#' @include is-percentage-of-map.R function-map-seq.R
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
#'   is_percentage_of_map_seq(digits_percentage = 0) |>
#'   audit_seq()
is_percentage_of_map_seq <- function_map_seq(
  .fun = is_percentage_of_map,
  .reported = c("absolute", "percentage", "n"),
  .name_test = "is_percentage_of",
  .var_bounds = list(absolute = c(0, NA), percentage = c(0, NA), n = c(1, NA))
)
