#' Test percentages against their counts with values around the reported ones
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `percentage_of_map_seq()` varies the reported counts,
#'   percentages, and totals a few steps up and down and applies
#'   [`percentage_of()`] to each resulting value set. This shows whether a
#'   small error in one of them could explain an inconsistency.
#'
#'   Call [`audit()`] on the output for summary statistics.
#'
#' @param data A data frame that [`percentage_of_map()`] could take.
#' @param var String. The variables to disperse. Default is
#'   `c("absolute", "percentage", "n")`.
#' @param out_min,out_max Limits on the dispersed values. Both default to
#'   `"auto"`, which keeps `absolute` and `percentage` at 0 or above and `n`
#'   at 1 or above. A number applies to every dispersed variable, and `NULL`
#'   removes the limit.
#' @param digits_percentage Integer (length 1). The number of decimal places in
#'   `percentage`, including trailing zeros. There is no default, since a
#'   numeric value does not keep them (`1.40` is `1.4`). It also sets the step
#'   size for dispersing `percentage`.
#' @param ... These dots must be empty. Every argument after them must be
#'   named in full.
#' @inheritParams percentage_of_map
#' @inheritParams grim_map_seq
#'
#' @return A tibble with the columns of [`percentage_of_map()`] output plus
#'   `diff_var`, `case`, and `var`, as in [`grim_map_seq()`]. As there, the
#'   other columns of `data` are dropped.
#'
#' @include percentage-of-map.R function-map-seq.R
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
#'   percentage_of_map_seq(digits_percentage = 0) |>
#'   audit()
percentage_of_map_seq <- function_map_seq(
  .fn = percentage_of_map,
  .reported = c("absolute", "percentage", "n"),
  .name_test = "percentage_of",
  .var_bounds = list(absolute = c(0, NA), percentage = c(0, NA), n = c(1, NA))
)
