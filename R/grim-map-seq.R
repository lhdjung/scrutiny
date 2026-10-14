#' GRIM-test values around the reported ones
#'
#' @description `grim_map_seq()` varies the reported means and sample sizes a
#'   few steps up and down and GRIM-tests each resulting value set. This shows
#'   whether a small error in reporting or computing could explain an
#'   inconsistency.
#'
#'   Call [`audit_seq()`] on the output for summary statistics.
#'
#' @param data A data frame that [`grim_map()`] could take.
#' @param digits_x Integer (length 1). The number of decimal places in `x`,
#'   including trailing zeros. There is no default, since a numeric value does
#'   not keep them (`1.40` is `1.4`). It also sets the step size for
#'   dispersing `x`.
#' @param x,n Optionally, the names of the columns in `data` that contain these
#'   values, if they differ from the defaults.
#' @param var String. The variables to disperse. Default is `c("x", "n")`.
#' @param dispersion Numeric. Steps up and down from the reported values, in
#'   units of the last decimal place. Default is `1:5`.
#' @param out_min,out_max Limits on the dispersed values. Both default to
#'   `"auto"`, which keeps `n` at 1 or above and leaves `x` unbounded. A number
#'   applies to every dispersed variable, and `NULL` removes the limit.
#' @param include_reported Logical. Should the reported values themselves be
#'   part of the sequences? Default is `FALSE`.
#' @param include_consistent Logical. Should consistent value sets be dispersed
#'   as well, not just inconsistent ones? Default is `FALSE`.
#' @param ... Arguments passed on to [`grim_map()`].
#'
#' @return A tibble with the columns of [`grim_map()`] output plus `diff_var`,
#'   the number of steps between the tested value and the reported one; `case`,
#'   the row number of the reported value set in `data`; and `var`, the
#'   dispersed variable.
#'
#' @section Summaries with [`audit_seq()`]: `audit_seq()` on `grim_map_seq()`
#'   output returns a tibble with one row per case and these columns:
#'   - `x` and `n`: the reported values, with their `consistency`.
#'   - `hits_total`: the number of GRIM-consistent value sets found within the
#'   `dispersion` range.
#'   - `hits_x`, `hits_n`: the number found by varying `x` and `n`,
#'   respectively. If both `include_reported` and `include_consistent` are
#'   `TRUE`, consistent reported cases are counted as well.
#'   - `diff_x`: the distance from `x` to the nearest consistent dispersed
#'   value, in dispersion steps. `diff_x_up` and `diff_x_down` are the
#'   distances to the nearest consistent value above and below.
#'   - `diff_n`, `diff_n_up`, `diff_n_down`: the same for `n`.
#'
#'   Call `audit()` on `audit_seq()` output to summarize these columns further.
#'   `na_count` and `na_rate` there count the cases without a consistent value
#'   within the `dispersion` range.
#'
#' @include grim-map.R
#'
#' @export
#'
#' @examples
#' # `grim_map_seq()` takes the same input as `grim_map()`:
#' pigs1
#'
#' out <- grim_map_seq(pigs1, digits_x = 2, include_consistent = TRUE)
#' out
#'
#' # Case-wise summaries:
#' out |>
#'   audit_seq()

grim_map_seq <- function_map_seq(
  .fun = grim_map,
  .reported = c("x", "n"),
  .name_test = "GRIM",
  # A mean is not bounded in either direction -- GRIM tests negative means as
  # readily as positive ones -- so only `n` limits the dispersion here:
  .var_bounds = list(n = c(1, NA))
)
