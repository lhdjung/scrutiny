#' Apply DEBIT to many cases
#'
#' @description Call `debit_map()` to use DEBIT on multiple combinations of
#'   mean, sample standard deviation, and sample size of binary distributions.
#'   Mapping function for [`debit()`].
#'
#'   For summary statistics, call [`audit()`] on the results.
#'
#' @param data Data frame.
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default because it cannot be inferred from a
#'   numeric `x`, which has no trailing zeros: both `1.4` and `1.40` are the
#'   number `1.4`, but only the latter has `digits_x = 2`. Use a single number
#'   if the whole column was reported with the same number of decimal places,
#'   or one number per row of `data` if it varies.
#' @param digits_sd Integer. The number of decimal places in `sd`, including
#'   trailing zeros. As with `digits_x`, there is no default, because trailing
#'   zeros don't survive in a numeric value, and it may have one value per row
#'   of `data`.
#' @param x,sd,n Optionally, specify these arguments as column names in `data`.
#' @param show_rec If set to `FALSE`, the resulting tibble only includes the
#'   columns `x`, `sd`, `n`, `digits_x`, `digits_sd`, and `consistency`. Default
#'   is `TRUE`.
#' @param formula,rounding,threshold,symmetric Further parameters of DEBIT
#'   testing; see documentation for [`debit()`].
#' @param ... Arguments passed down to [`debit()`].
#'
#' @importFrom rlang .data
#'
#' @include debit.R function-map.R

#' @return A tibble with (at least) these columns --
#' - `x`, `sd`, `n`: the inputs.
#' - `digits_x`, `digits_sd`: the number of decimal places in `x` and `sd`, as
#'   given by `digits_x` and `digits_sd`.
#' - `consistency`: DEBIT consistency of `x`, `sd`, and `n`.
#'
#'   By default, the tibble also includes the rounding method, boundary values,
#'   and information about the boundary values being inclusive or not. The
#'   tibble has the `scrutiny_debit_map` class, which is recognized by the `audit()`
#'   generic.
#'
#' @section Summaries with [`audit()`]: There is an S3 method for the
#'   [`audit()`] generic, so you can call [`audit()`] following `debit_map()`.
#'   It returns a tibble with these columns ---
#'
#' 1. `incons_cases`: the number of DEBIT-inconsistent cases.
#' 2. `all_cases`: the total number of cases.
#' 3. `incons_rate`: the rate of inconsistent cases.
#' 4. `mean_x`: the mean `x` (mean) value.
#' 5. `mean_sd`: the mean `sd` value.
#' 6. `distinct_n`: the number of distinct `n` values.

#' @references Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A
#'   Simple Consistency Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @export
#'
#' @examples
#' # Call `debit_map()` on binary summary
#' # data such as these:
#' pigs3
#'
#' # The `consistency` column shows
#' # whether the values to its left
#' # are DEBIT-consistent:
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
  .args_defaults = list(show_rec = TRUE),
  .col_names = c(
    "consistency",
    "rounding",
    "sd_lower",
    "sd_incl_lower",
    "sd_upper",
    "sd_incl_upper",
    "x_lower",
    "x_upper"
  )
)
