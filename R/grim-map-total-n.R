#' GRIM-test with hypothetical group sizes
#'
#' @description Some studies report group means but only the total sample
#'   size, not the group sizes. For two groups, `grim_map_total_n()` tests
#'   hypothetical splits of the total: it starts from an even split, moves one
#'   participant at a time from one group to the other, and GRIM-tests both
#'   means against both group sizes, in both pairings.
#'
#'   Call [`audit_total_n()`] on the output for summary statistics.
#'
#' @param data Data frame with numeric columns `x1`, `x2`, and `n`: the two
#'   group means (or percentages) and the total sample size. Which mean is `x1`
#'   does not matter, since both pairings with the group sizes are tested.
#' @param digits_x Integer. The number of decimal places in `x1` and `x2`,
#'   including trailing zeros. There is no default, since a numeric value does
#'   not keep them (`1.40` is `1.4`). Either a single number for both groups,
#'   or two numbers, one for `x1` and one for `x2`.
#' @param x1,x2,n Optionally, the names of the columns in `data` that contain
#'   these values, if they differ from the defaults.
#' @param dispersion Numeric. Steps up and down from half the total `n`.
#'   Default is `0:5`, i.e., the even split and five steps away from it.
#' @param n_min Numeric. Minimal group size. Default is 1.
#' @param n_max Numeric. Maximal group size. Default is `NULL`, i.e., no
#'   maximum.
#' @param constant Optionally, values that belong to the two groups, such as
#'   their labels: a length-2 vector for the groups of `x1` and `x2`, or a list
#'   of such vectors. Each becomes a column, as in [`disperse_total()`], and its
#'   values stay with their groups in both directions. Default is `NULL`.
#' @inheritParams disperse_total
#' @param items_averaged Integer. As in [`grim_map()`]: the number of items
#'   averaged into each score, given as an argument or as a column of `data`.
#'   There is no default.
#' @param ... These dots must be empty. Every argument after them must be
#'   named in full.
#' @inheritParams grim_map
#'
#' @include function-map-total-n.R
#'
#' @references Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It
#'   Light or Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://journals.sagepub.com/doi/10.1177/09567976211058727
#'
#'   Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple
#'   Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876
#'
#' @seealso [`function_map_total_n()`], which created this function from
#'   [`grim_map()`].
#'
#' @return A tibble with these columns:
#' - `x`: the reported means, in pairs of rows.
#' - `n`: the hypothetical group sizes, with `n_change` recording the steps
#'   from half the total.
#' - `both_consistent`: whether both means of a pair are consistent with their
#'   hypothetical group sizes.
#' - `case`: the row number in `data`.
#' - `dir`: `"forth"` where `x2` is paired with the larger group size, `"back"`
#'   where `x1` is. The split of an even total into two equal groups is the
#'   same scenario either way, so it appears under `"forth"` only.
#' - The other columns of [`grim_map()`] output, and the other columns of
#'   `data`.
#'
#' @section Summaries with [`audit_total_n()`]: `audit_total_n()` on
#'   `grim_map_total_n()` output returns a tibble with one row per case and
#'   these columns:
#'  - `x1`, `x2`, and `n`: the reported values.
#'  - `hits_total`: the number of splits in which both `x1` and `x2` are
#'  GRIM-consistent, summing `hits_forth` and `hits_back`.
#'  - `hits_forth`: the number of such splits with `x2` paired with the larger
#'  group.
#'  - `hits_back`: the same with `x1` paired with the larger group.
#'  - `scenarios_total`: the number of splits tested, leaving out those that
#'  could not be decided.
#'  - `hit_rate`: `hits_total / scenarios_total`, or `NA` if no split could
#'  be decided.
#'
#'  Call [`audit()`] on `audit_total_n()` output to summarize these columns
#'  further.
#'
#' @export
#'
#' @examples
#' df <- tibble::tribble(
#'   ~x1,  ~x2,  ~n,
#'   3.43, 5.28, 90,
#'   2.97, 4.42, 103
#' )
#'
#' grim_map_total_n(df, digits_x = 2, items_averaged = 1)
#'
#' # `hits_total` counts the splits in which both means are consistent:
#' df |>
#'   grim_map_total_n(digits_x = 2, items_averaged = 1) |>
#'   audit_total_n()
#'
#' # More steps away from the even split tend to find more hits:
#' df |>
#'   grim_map_total_n(digits_x = 2, items_averaged = 1, dispersion = 0:10) |>
#'   audit_total_n()

grim_map_total_n <- function_map_total_n(
  .fun = grim_map,
  .reported = "x",
  .name_test = "GRIM"
)
