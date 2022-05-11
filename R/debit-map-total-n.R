
#' Use DEBIT with hypothetical group sizes
#'
#' @description `debit_map_total_n()` extends DEBIT to cases where only group
#'   means and standard deviations (SDs) were reported, not group sizes.
#'
#'   The function is analogous to `grim_map_total_n()`, relying on the same
#'   infrastructure.

#' @param data Data frame with string columns `x1`, `x2`, `sd1`, and `sd2`, as
#'   well as numeric column `n`. The first two are reported group means. `sd1`
#'   and `sd2` are reported group SDs. `n` is the reported total sample size. It
#'   is not very important whether a value is in `x1` or in `x2` because, after
#'   the first round of tests, the function switches roles between `x1` and
#'   `x2`, and reports the outcomes both ways. The same applies to `sd1` and
#'   `sd2`. However, do make sure the `x*` and `sd*` values are paired
#'   accurately, as reported.
#' @param dispersion Numeric. Steps up and down from half the `n` values.
#'   Default is `0:5`, i.e., half `n` itself followed by five steps up and down.
#' @param n_min Numeric. Minimal group size. Default is 1.
#' @param n_max Numeric. Maximal group size. Default is `NULL`, i.e., no
#'   maximum.
#' @param rounding,threshold,symmetric,extra Arguments passed down to
#'   `debit_map()`.

#' @include debit-map.R function-map-total-n.R

#' @references Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It
#'   Light or Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://doi.org/10.1177/09567976211058727
#'
#'   Heathers, J. A. J., & Brown, N. J. L. (2019). DEBIT: A Simple Consistency
#'   Test For Binary Data. https://osf.io/5vb3u/.

#' @seealso `function_map_total_n()`, which created the present function using
#'   `debit_map()`.

#' @return A tibble with these columns:
#' - `x` and `sd`, the group-wise reported input statistics, are repeated in
#' row pairs.
#' - `n` is dispersed from half the input `n`, with `n_change` tracking the
#' differences.
#' - `both_consistent` flags scenarios where both reported `x` and `sd` values
#' are consistent with the hypothetical `n` values.
#' - `case` corresponds to the row numbers of the input data frame.
#' - `dir` is `"forth"` in the first half of rows and `"back"` in the second
#' half. `"forth"` means that `x2` and `sd2` from the input are paired with the
#' larger dispersed `n`, whereas `"back"` means that `x1` and `sd1` are paired
#' with the larger dispersed `n`.
#' - Other columns from `debit_map()` are preserved.

#' @section Summaries with `audit()`: There is an S3 method for the `audit()`
#'   generic, so you can call `audit()` following up on `debit_map_total_n()`
#'   to get a tibble with summary statistics. It will have these columns:
#'  - `x1`, `x2`, `sd1`, `sd2`, and `n` are the original inputs.
#'  - `hits_forth` is the number of scenarios in which both `x1` and `sd1`, as
#'  well as `x2` and `x2`, are DEBIT- consistent with the dispersed `n` values
#'  when `x2` and `sd2` are paired with the larger dispersed `n`.
#'  - `hits_back` is the same, except `x1` and `sd1` are paired with the larger
#'  dispersed `n` value.
#'  - `hits_total` is the sum of `hits_forth` and `hits_back`, i.e., the total
#'  number of both-consistent scenarios.
#'  - `scenarios_total` is the total number of test scenarios, whether or not
#'  both `x1` / `sd1` and `x2` / `sd2` are consistent.
#'  - `hit_rate` is the ratio of `hits_total` to `scenarios_total`.

#' @export

#' @examples
#' # Run `grim_map_total_n()` on data like these:
#' df <- tibble::tribble(
#'   ~x1,    ~x2,   ~n,
#'   "3.43", "5.28", 90,
#'   "2.97", "4.42", 103
#' )
#'
#' # The `hits_total` column shows all scenarios in
#' # which both divergent `n` values are GRIM-consistent
#' # with the `x*` values when paired with them both ways:
#' grim_map_total_n(data = df)
#'
#' # By default (`dispersion = 0:5`), the function goes
#' # five steps up and down from `n`. If this sequence
#' # gets longer, the number of hits tends to increase:
#' grim_map_total_n(data = df, dispersion = 0:10)
#'
#' # Get all the details with `show_all = TRUE`:
#' grim_map_total_n(data = df, show_all = TRUE)


debit_map_total_n <- function_map_total_n(
  .fun = debit_map,
  .reported = c("x", "sd"),
  .name_test = "DEBIT",
  .name_class = "scr_debit_map_total_n"
)

