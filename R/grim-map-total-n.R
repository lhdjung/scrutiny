
#' GRIM-testing with hypothetical group sizes
#'
#' @description When reporting group means, some published studies only report
#'   the total sample size but no group sizes corresponding to each mean.
#'   However, group sizes are crucial for GRIM-testing.
#'
#'   In the two-groups case, `grim_map_total_n()` helps in these ways:

#' - It creates hypothetical group sizes. With an even total sample size, it
#' incrementally moves up and down from half the total sample size. For example,
#' with a total sample size of 40, it starts at 20, goes on to 19 and 21, then
#' to 18 and 22, etc. With odd sample sizes, it starts from the two integers
#' around half.
#' - It GRIM-tests all of these values together with the group means.
#' - It reports all the scenarios in which both "dispersed" hypothetical group
#' sizes are GRIM-consistent with the group means.
#'
#' All of this works with one or more total sample sizes at a time. Call
#' `audit_total_n()` for summary statistics.

#' @param data Data frame with string columns `x1` and `x2`, and numeric column
#'   `n`. The first two are group mean or percentage values with unknown group
#'   sizes, and `n` is the total sample size. It is not very important whether a
#'   value is in `x1` or in `x2` because, after the first round of tests, the
#'   function switches roles between `x1` and `x2`, and reports the outcomes
#'   both ways.
#' @param x1,x2 Optionally, specify these arguments as column names in `data`.
#' @param dispersion Numeric. Steps up and down from half the `n` values.
#'   Default is `0:5`, i.e., half `n` itself followed by five steps up and down.
#' @param n_min Numeric. Minimal group size. Default is 1.
#' @param n_max Numeric. Maximal group size. Default is `NULL`, i.e., no
#'   maximum.
#' @inheritParams disperse_total
#' @param ... Arguments passed down to `grim_map()`.

# # Used to have this instead of `@param ...`:
# @param
# items,percent,show_rec,show_prob,rounding,threshold,symmetric,tolerance,extra
# Arguments passed down to `grim_map()` via `...` (the dots).

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

#' @seealso `function_map_total_n()`, which created the present function using
#'   `grim_map()`.

#' @return A tibble with these columns:
#' - `x`, the group-wise reported input statistic, is repeated in row pairs.
#' - `n` is dispersed from half the input `n`, with `n_change` tracking the
#' differences.
#' - `both_consistent` flags scenarios where both reported `x` values are
#' consistent with the hypothetical `n` values.
#' - `case` corresponds to the row numbers of the input data frame.
#' - `dir` is `"forth"` in the first half of rows and `"back"` in the second
#' half. `"forth"` means that `x2` from the input is paired with the larger
#' dispersed `n`, whereas `"back"` means that `x1` is paired with the larger
#' dispersed `n`.
#' - Other columns from `grim_map()` are preserved.

#' @section Summaries with `audit_total_n()`: You can call
#'   `audit_total_n()` following up on `grim_map_total_n()`
#'   to get a tibble with summary statistics. It will have these columns:
#'  - `x1`, `x2`, and `n` are the original inputs.
#'  - `hits_total` is the number of scenarios in which both
#'  `x1` and `x2` are GRIM-consistent. It is the sum
#'  of `hits_forth` and `hits_back` below.
#'  - `hits_forth` is the number of both-consistent cases that result
#'  from pairing `x2` with the larger dispersed `n` value.
#'  - `hits_back` is the same, except `x1` is
#'  paired with the larger dispersed `n` value.
#'  - `scenarios_total` is the total number of test scenarios,
#'  whether or not both `x1` and `x2`
#'  are GRIM-consistent.
#'  - `hit_rate` is the ratio of `hits_total` to `scenarios_total`.
#'
#'  Call `audit()` following `audit_total_n()` to summarize results
#'  even further.

#' @export

#' @examples
#' # Run `grim_map_total_n()` on data like these:
#' df <- tibble::tribble(
#'   ~x1,    ~x2,   ~n,
#'   "3.43", "5.28", 90,
#'   "2.97", "4.42", 103
#' )
#' df
#'
#' grim_map_total_n(df)
#'
#' # `audit_total_n()` summaries can be more important than
#' # the detailed results themselves.
#' # The `hits_total` column shows all scenarios in
#' # which both divergent `n` values are GRIM-consistent
#' # with the `x*` values when paired with them both ways:
#' df %>%
#'   grim_map_total_n() %>%
#'   audit_total_n()
#'
#' # By default (`dispersion = 0:5`), the function goes
#' # five steps up and down from `n`. If this sequence
#' # gets longer, the number of hits tends to increase:
#' df %>%
#'   grim_map_total_n(dispersion = 0:10) %>%
#'   audit_total_n()


grim_map_total_n <- function_map_total_n(
  .fun = grim_map,
  .reported = "x",
  .name_test = "GRIM"
)


