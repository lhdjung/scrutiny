#' Apply DEBIT with hypothetical group sizes
#'
#' @description Some studies report group means and standard deviations of
#'   binary data but only the total sample size, not the group sizes. For two
#'   groups, `debit_map_total_n()` tests hypothetical splits of the total, as
#'   [`grim_map_total_n()`] does.
#'
#'   Call [`audit_total_n()`] on the output for summary statistics.
#'
#' @param data Data frame with numeric columns `x1`, `x2`, `sd1`, `sd2`, and
#'   `n`: the two group means, the two group SDs, and the total sample size.
#'   Which group is 1 does not matter, since both pairings with the group sizes
#'   are tested, but `x1` must belong with `sd1` and `x2` with `sd2`.
#' @param digits_x,digits_sd Integer. The number of decimal places in the means
#'   and SDs, including trailing zeros. There are no defaults, since a numeric
#'   value does not keep them (`1.40` is `1.4`). Either a single number for both
#'   groups, or two numbers, one per group. They apply to every row of `data`;
#'   if the decimal places of a group differ by row, call the function on each
#'   set of rows separately.
#' @param x1,x2,sd1,sd2,n Optionally, the names of the columns in `data` that
#'   contain these values, if they differ from the defaults.
#' @param n_min Numeric. Minimal group size. Default is `NULL`, which stands
#'   for 2 under `sd_meaning = "sd_sample"` or `"se"`, since a sample SD
#'   divides by `n - 1`, so a group of 1 has no test result; and for 1 under
#'   `"sd_pop"` or `"sd_sample_or_pop"`, where it does.
#' @param ... These dots must be empty. Every argument after them must be
#'   named in full.
#' @inheritParams debit_map
#' @inheritParams grim_map_total_n
#'
#' @include debit-map.R function-map-total-n.R
#'
#' @references Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It
#'   Light or Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://journals.sagepub.com/doi/10.1177/09567976211058727
#'
#'   Heathers, J. A. J., & Brown, N. J. L. (2019). DEBIT: A Simple Consistency
#'   Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @seealso [`function_map_total_n()`], which created this function from
#'   [`debit_map()`].
#'
#' @return A tibble like that of [`grim_map_total_n()`], with `x` and `sd`
#'   columns for the reported values, `dir` recording which group is paired
#'   with the larger group size (the equal split of an even total appears under
#'   `"forth"` only), the other columns of [`debit_map()`] output, and the
#'   other columns of `data`.
#'
#' @section Summaries with [`audit_total_n()`]: `audit_total_n()` on
#'   `debit_map_total_n()` output returns a tibble with one row per case and
#'   these columns:
#'  - `x1`, `x2`, `sd1`, `sd2`, and `n`: the reported values.
#'  - `hits_total`: the number of splits in which both groups are
#'  DEBIT-consistent, summing `hits_forth` and `hits_back`.
#'  - `hits_forth`: the number of such splits with `x2` and `sd2` paired with
#'  the larger group.
#'  - `hits_back`: the same with `x1` and `sd1` paired with the larger group.
#'  - `scenarios_total`: the number of splits tested, leaving out those that
#'  could not be decided.
#'  - `hit_rate`: `hits_total / scenarios_total`, or `NA` if no split could
#'  be decided.
#'
#' @export
#'
#' @examples
#' df <- tibble::tribble(
#'   ~x1,  ~x2,  ~sd1,  ~sd2,  ~n,
#'   0.30, 0.28, 0.17, 0.10, 70,
#'   0.41, 0.39, 0.09, 0.15, 65
#' )
#'
#' debit_map_total_n(df, digits_x = 2, digits_sd = 2)
#'
#' df |>
#'   debit_map_total_n(digits_x = 2, digits_sd = 2) |>
#'   audit_total_n()

debit_map_total_n <- function_map_total_n(
  .fn = debit_map,
  .reported = c("x", "sd"),
  .name_test = "DEBIT",
  .n_min = function(args) sd_min_n(args$sd_meaning)
)
