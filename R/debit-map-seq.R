#' Apply DEBIT to values around the reported ones
#'
#' @description `debit_map_seq()` varies the reported means, standard
#'   deviations, and sample sizes a few steps up and down and applies DEBIT to
#'   each resulting value set. This shows whether a small error in reporting or
#'   computing could explain an inconsistency.
#'
#'   Call [`audit_seq()`] on the output for summary statistics.
#'
#' @param data A data frame that [`debit_map()`] could take.
#' @param digits_x,digits_sd Integer (length 1). The number of decimal places in
#'   `x` and `sd`, including trailing zeros. There are no defaults, since a
#'   numeric value does not keep them (`1.40` is `1.4`). They also set the step
#'   sizes for dispersing `x` and `sd`.
#' @param x,sd,n Optionally, the names of the columns in `data` that contain
#'   these values, if they differ from the defaults.
#' @param var String. The variables to disperse. Default is
#'   `c("x", "sd", "n")`.
#' @param out_min,out_max Limits on the dispersed values. Both default to
#'   `"auto"`, which keeps `n` at 2 or above and `x` and `sd` between 0 and 1.
#'   A number applies to every dispersed variable, and `NULL` removes the
#'   limit.
#' @param ... Arguments passed on to [`debit_map()`].
#' @inheritParams grim_map_seq
#'
#' @return A tibble with the columns of [`debit_map()`] output plus
#'   `diff_var`, `case`, and `var`, as in [`grim_map_seq()`].
#'
#' @section Summaries with [`audit_seq()`]: `audit_seq()` on `debit_map_seq()`
#'   output returns a tibble with one row per case and these columns:
#'   - `x`, `sd`, and `n`: the reported values, with their `consistency`.
#'   - `hits_total`: the number of DEBIT-consistent value sets found within the
#'   `dispersion` range.
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
#' @include function-map-seq.R
#'
#' @export
#'
#' @examples
#' # `debit_map_seq()` takes the same input as `debit_map()`:
#' pigs3
#'
#' out <- pigs3 |>
#'   dplyr::slice(3:4) |>
#'   debit_map_seq(digits_x = 2, digits_sd = 2)
#' out
#'
#' # Case-wise summaries:
#' out |>
#'   audit_seq()

debit_map_seq <- function_map_seq(
  .fun = debit_map,
  .reported = c("x", "sd", "n"),
  .name_test = "DEBIT",
  # DEBIT works on binary data, where the mean is a proportion and the SD is
  # bounded by it, so both live in `[0, 1]`. `debit()` rejects anything outside
  # that range, which used to make `debit_map_seq()` fail outright whenever a
  # reported value sat close enough to a bound for the dispersion to cross it:
  .var_bounds = list(n = c(2, NA), x = c(0, 1), sd = c(0, 1))
)
