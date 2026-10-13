#' Apply DEBIT to many cases
#'
#' @description `debit_map()` applies [`debit()`] to every row of a data frame
#'   with means, sample standard deviations, and sample sizes of binary data.
#'
#'   Call [`audit()`] on the output for summary statistics and
#'   [`debit_plot()`] to visualize it.
#'
#' @param data Tibble with columns `x`, `sd`, and `n`. Any other columns are
#'   returned alongside the results.
#' @param digits_x,digits_sd Integer. The number of decimal places in `x` and
#'   `sd`, including trailing zeros. There are no defaults, since a numeric
#'   value does not keep them (`1.40` is `1.4`). Either a single number for the
#'   whole column or one number per row of `data`.
#' @param x,sd,n Optionally, the names of the columns in `data` that contain
#'   these values, if they differ from the defaults.
#' @param formula,rounding,threshold,symmetric Passed on to [`debit()`].
#' @param ... Further arguments passed on to [`debit()`].
#'
#' @importFrom rlang .data
#'
#' @include debit.R function-map.R
#'
#' @return A tibble with these columns:
#' - `x`, `sd`, `n`: the inputs.
#' - `digits_x`, `digits_sd`: the numbers of decimal places, as given.
#' - `consistency`: DEBIT consistency of `x`, `sd`, and `n`. `NA` for a case
#'   that cannot be decided, such as one with a missing value.
#'
#'   Further columns hold the reconstructed values, which [`debit_plot()`]
#'   needs: the bounds of `sd` and `x` (`sd_lower`, `sd_upper`, `x_lower`, `x_upper`) with
#'   `sd_incl_lower` and `sd_incl_upper` saying whether the SD bounds are
#'   inclusive; and `sum_lower` and `sum_upper`, the least and the greatest
#'   number of ones among `n` values whose mean would be reported as `x`. Under
#'   `formula = "exact"`, a value set is consistent exactly if some `k` from
#'   `sum_lower` to `sum_upper` has an SD within the bounds of `sd`.
#'
#'   The tibble has the class `scrutiny_debit_map`, which is recognized by
#'   [`audit()`] and [`debit_plot()`].
#'
#' @section Summaries with [`audit()`]: `audit()` on `debit_map()` output
#'   returns a tibble with one row and these columns:
#'
#' 1. `incons_cases`: number of DEBIT-inconsistent value sets.
#' 2. `all_cases`: total number of value sets.
#' 3. `incons_rate`: proportion of DEBIT-inconsistent value sets.
#' 4. `mean_x`: mean of the `x` values.
#' 5. `mean_sd`: mean of the `sd` values.
#' 6. `distinct_n`: number of distinct `n` values.
#'
#' @references Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A
#'   Simple Consistency Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @export
#'
#' @examples
#' # Call `debit_map()` on binary summary data such as these:
#' pigs3
#'
#' # The `consistency` column shows whether the
#' # values to its left are DEBIT-consistent:
#' pigs3 |>
#'   debit_map(digits_x = 2, digits_sd = 2)
#'
#' # Get test summaries with `audit()`:
#' pigs3 |>
#'   debit_map(digits_x = 2, digits_sd = 2) |>
#'   audit()

debit_map <- function_map(
  .fun = debit_scalar,
  .reported = c("x", "sd", "n"),
  .name_test = "DEBIT",
  .args_by_row = c("digits_x", "digits_sd"),
  # Unlike `debit()`, the mapper shows the reconstructed values by default:
  .args_fixed = list(show_rec = TRUE),
  .col_names = c(
    "consistency",
    "sd_lower",
    "sd_incl_lower",
    "sd_upper",
    "sd_incl_upper",
    "x_lower",
    "x_upper",
    "sum_lower",
    "sum_upper"
  )
)
