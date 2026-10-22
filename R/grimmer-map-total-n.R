#' GRIMMER-test with hypothetical group sizes
#'
#' @description Some studies report group means and standard deviations but
#'   only the total sample size, not the group sizes. For two groups,
#'   `grimmer_map_total_n()` tests hypothetical splits of the total, as
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
#'   groups, or two numbers, one per group.
#' @param x1,x2,sd1,sd2,n Optionally, the names of the columns in `data` that
#'   contain these values, if they differ from the defaults.
#' @param items_averaged Integer. As in [`grimmer_map()`]: the number of items
#'   averaged into each score, given as an argument or as a column of `data`.
#'   There is no default.
#' @param n_min Numeric. Minimal group size. Default is 2: GRIMMER divides by
#'   `n - 1`, so a group of 1 has no test result.
#' @param ... Arguments passed on to [`grimmer_map()`].
#' @inheritParams grim_map_total_n
#'
#' @include function-map-total-n.R
#'
#' @references Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
#'   possibility of standard deviations.
#'   https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/
#'
#'   Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It Light or
#'   Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://journals.sagepub.com/doi/10.1177/09567976211058727
#'
#' @seealso [`function_map_total_n()`], which created this function from
#'   [`grimmer_map()`].
#'
#' @return A tibble like that of [`grim_map_total_n()`], with `x` and `sd`
#'   columns for the reported values, `dir` recording which group is paired
#'   with the larger group size (the equal split of an even total appears under
#'   `"forth"` only), the other columns of [`grimmer_map()`] output, and the
#'   other columns of `data`.
#'
#' @section Summaries with [`audit_total_n()`]: `audit_total_n()` on
#'   `grimmer_map_total_n()` output returns a tibble with one row per case and
#'   these columns:
#'  - `x1`, `x2`, `sd1`, `sd2`, and `n`: the reported values.
#'  - `hits_total`: the number of splits in which both groups are
#'  GRIMMER-consistent, summing `hits_forth` and `hits_back`.
#'  - `hits_forth`: the number of such splits with `x2` and `sd2` paired with
#'  the larger group.
#'  - `hits_back`: the same with `x1` and `sd1` paired with the larger group.
#'  - `scenarios_total`: the number of splits tested.
#'  - `hit_rate`: `hits_total / scenarios_total`.
#'
#' @export
#'
#' @examples
#' df <- tibble::tribble(
#'   ~x1,  ~x2,  ~sd1, ~sd2, ~n,
#'   3.43, 5.28, 1.09, 2.12, 70,
#'   2.97, 4.42, 0.43, 1.65, 65
#' )
#'
#' grimmer_map_total_n(df, digits_x = 2, digits_sd = 2, items_averaged = 1)
#'
#' # `hits_total` counts the splits in which both groups are consistent:
#' df |>
#'   grimmer_map_total_n(digits_x = 2, digits_sd = 2, items_averaged = 1) |>
#'   audit_total_n()
#'
#' # More steps away from the even split tend to find more hits:
#' df |>
#'   grimmer_map_total_n(
#'     digits_x = 2,
#'     digits_sd = 2,
#'     items_averaged = 1,
#'     dispersion = 0:10
#'   ) |>
#'   audit_total_n()

grimmer_map_total_n <- function_map_total_n(
  .fun = grimmer_map,
  .reported = c("x", "sd"),
  .name_test = "GRIMMER",
  .n_min = 2L
)
