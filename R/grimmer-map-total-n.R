
#' GRIMMER-testing with hypothetical group sizes
#'
#' @description When reporting group means, some published studies only report
#'   the total sample size but no group sizes corresponding to each mean.
#'   However, group sizes are crucial for GRIMMER-testing.
#'
#'   In the two-groups case, `grimmer_map_total_n()` helps in these ways:

#' - It creates hypothetical group sizes. With an even total sample size, it
#' incrementally moves up and down from half the total sample size. For example,
#' with a total sample size of 40, it starts at 20, goes on to 19 and 21, then
#' to 18 and 22, etc. With odd sample sizes, it starts from the two integers
#' around half.
#' - It GRIMMER-tests all of these values together with the group means.
#' - It reports all the scenarios in which both "dispersed" hypothetical group
#' sizes are GRIMMER-consistent with the group means.
#'
#' All of this works with one or more total sample sizes at a time. Call
#' [`audit_total_n()`] for summary statistics.

#' @param data Data frame with string columns `x1`, `x2`, `sd1`, and `sd2`, as
#'   well as numeric column `n`. The first two are reported group means. `sd1`
#'   and `sd2` are reported group SDs. `n` is the reported total sample size. It
#'   is not very important whether a value is in `x1` or in `x2` because, after
#'   the first round of tests, the function switches roles between `x1` and
#'   `x2`, and reports the outcomes both ways. The same applies to `sd1` and
#'   `sd2`. However, do make sure the `x*` and `sd*` values are paired
#'   accurately, as reported.
#' @param x1,x2,sd1,sd2 Optionally, specify these arguments as column names in
#'   `data`.
#' @param dispersion Numeric. Steps up and down from half the `n` values.
#'   Default is `0:5`, i.e., half `n` itself followed by five steps up and down.
#' @param n_min Numeric. Minimal group size. Default is 1.
#' @param n_max Numeric. Maximal group size. Default is `NULL`, i.e., no
#'   maximum.
#' @inheritParams disperse_total
#' @param ... Arguments passed down to [`grimmer_map()`]. *(NOTE: Don't use the
#'   `items` argument. It currently contains a bug that will be fixed in the
#'   future.)*

#' @include function-map-total-n.R
#'
#' @references Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It
#'   Light or Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://journals.sagepub.com/doi/10.1177/09567976211058727

#' @seealso [`function_map_total_n()`], which created the present function using
#'   [`grimmer_map()`].

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
#' - Other columns from [`grimmer_map()`] are preserved.

#' @section Summaries with [`audit_total_n()`]: You can call
#'   [`audit_total_n()`] following up on `grimmer_map_total_n()`
#'   to get a tibble with summary statistics. It will have these columns:
#'  - `x1`, `x2`, `sd1`, `sd2`, and `n` are the original inputs.
#'  - `hits_total` is the number of scenarios in which all of
#'  `x1`, `x2`, `sd1`, and `sd2` are GRIMMER-consistent. It is the sum
#'  of `hits_forth` and `hits_back` below.
#'  - `hits_forth` is the number of both-consistent cases that result
#'  from pairing `x2` and `sd2` with the larger dispersed `n` value.
#'  - `hits_back` is the same, except `x1` and `sd1` are
#'  paired with the larger dispersed `n` value.
#'  - `scenarios_total` is the total number of test scenarios,
#'  whether or not both `x1` and `sd1` as well as `x2` and `sd2`
#'  are GRIMMER-consistent.
#'  - `hit_rate` is the ratio of `hits_total` to `scenarios_total`.

#' @references Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
#'   possibility of standard deviations.
#'   https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/
#'
#'   Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It Light or
#'   Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://journals.sagepub.com/doi/10.1177/09567976211058727

#' @export

#' @examples
#' # Run `grimmer_map_total_n()` on data like these:
#' df <- tibble::tribble(
#'   ~x1,    ~x2,    ~sd1,   ~sd2,   ~n,
#'   "3.43", "5.28", "1.09", "2.12", 70,
#'   "2.97", "4.42", "0.43", "1.65", 65
#' )
#' df
#'
#' grimmer_map_total_n(df)
#'
#' # `audit_total_n()` summaries can be more important than
#' # the detailed results themselves.
#' # The `hits_total` column shows all scenarios in
#' # which both divergent `n` values are GRIMMER-consistent
#' # with the `x*` values when paired with them both ways:
#' df %>%
#'   grimmer_map_total_n() %>%
#'   audit_total_n()
#'
#' # By default (`dispersion = 0:5`), the function goes
#' # five steps up and down from `n`. If this sequence
#' # gets longer, the number of hits tends to increase:
#' df %>%
#'   grimmer_map_total_n(dispersion = 0:10) %>%
#'   audit_total_n()


grimmer_map_total_n <- function_map_total_n(
  .fun = grimmer_map,
  .reported = c("x", "sd"),
  .name_test = "GRIMMER"
)

