#' GRIMMER-test values around the reported ones
#'
#' @description `grimmer_map_seq()` varies the reported means, standard
#'   deviations, and sample sizes a few steps up and down and GRIMMER-tests
#'   each resulting value set. This shows whether a small error in reporting or
#'   computing could explain an inconsistency.
#'
#'   Call [`audit_seq()`] on the output for summary statistics.
#'
#' @param data A data frame that [`grimmer_map()`] could take.
#' @param digits_x,digits_sd Integer (length 1). The number of decimal places in
#'   `x` and `sd`, including trailing zeros. There are no defaults, since a
#'   numeric value does not keep them (`1.40` is `1.4`). They also set the step
#'   sizes for dispersing `x` and `sd`.
#' @param x,sd,n Optionally, the names of the columns in `data` that contain
#'   these values, if they differ from the defaults.
#' @param var String. The variables to disperse. Default is
#'   `c("x", "sd", "n")`.
#' @param out_min,out_max Limits on the dispersed values. Both default to
#'   `"auto"`, which keeps `n` at 2 or above and `sd` at 0 or above, and
#'   leaves `x` unbounded. A number applies to every dispersed variable, and
#'   `NULL` removes the limit.
#' @param items_averaged Integer. As in [`grimmer_map()`]: the number of items
#'   averaged into each score, given as an argument or as a column of `data`.
#'   There is no default.
#' @param ... Arguments passed on to [`grimmer_map()`].
#' @inheritParams grim_map_seq
#'
#' @return A tibble with the columns of [`grimmer_map()`] output plus
#'   `diff_var`, `case`, and `var`, as in [`grim_map_seq()`].
#'
#' @section Summaries with [`audit_seq()`]: `audit_seq()` on
#'   `grimmer_map_seq()` output returns a tibble with one row per case and
#'   these columns:
#'   - `x`, `sd`, and `n`: the reported values, with their `consistency`.
#'   - `hits_total`: the number of GRIMMER-consistent value sets found within
#'   the `dispersion` range.
#'   - `hits_x`, `hits_sd`, `hits_n`: the number found by varying each
#'   variable. If both `include_reported` and `include_consistent` are `TRUE`,
#'   a consistent reported value set is counted as well: once per variable
#'   here, and once in `hits_total`.
#'   - `diff_x`: the distance from `x` to the nearest consistent dispersed
#'   value, in dispersion steps. `diff_x_up` and `diff_x_down` are the
#'   distances to the nearest consistent value above and below.
#'   - `diff_sd*` and `diff_n*`: the same for `sd` and `n`.
#'
#'   Call `audit()` on `audit_seq()` output to summarize these columns further.
#'
#' @include grimmer-map.R
#'
#' @export
#'
#' @examples
#' # `grimmer_map_seq()` takes the same input as `grimmer_map()`:
#' pigs5
#'
#' out <- grimmer_map_seq(
#'   pigs5,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1,
#'   include_consistent = TRUE
#' )
#' out
#'
#' # Case-wise summaries:
#' out |>
#'   audit_seq()

grimmer_map_seq <- function_map_seq(
  .fun = grimmer_map,
  .reported = c("x", "sd", "n"),
  .name_test = "GRIMMER",
  # A standard deviation cannot be negative, but it can be `0`. The mean is
  # unbounded, as it is for GRIM:
  .var_bounds = list(n = c(2, NA), sd = c(0, NA))
)
