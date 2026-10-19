#' GRIM-test many cases at once
#'
#' @description `grim_map()` applies [`grim()`] to every row of a data frame
#'   with means (or percentages) and sample sizes.
#'
#'   Set `percent` to `TRUE` if `x` contains percentages, and `show_rec` to
#'   `TRUE` to see the reconstructed values behind each verdict. Call
#'   [`audit()`] on the output for summary statistics and [`grim_plot()`] to
#'   visualize it.
#'
#' @param data Tibble with columns `x` and `n`, and optionally
#'   `items_averaged`. Any other columns are returned alongside the results.
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default, since a numeric value does not keep
#'   them (`1.40` is `1.4`). Either a single number for the whole column or
#'   one number per row of `data`.
#' @param x,n Optionally, the names of the columns in `data` that contain these
#'   values, if they differ from the defaults.
#' @param items_averaged Integer. As in [`grim()`]: the number of items
#'   averaged into each score, not the number of scale points. Give it as an
#'   argument or as a column of `data`; there is no default. It is returned as
#'   a column.
#' @param percent Logical. If `TRUE`, the `x` values are percentages: they are
#'   divided by 100 and tested with two more decimal places. Default is
#'   `FALSE`.
#' @param show_rec Logical. If `TRUE`, the reconstructed numbers are returned
#'   as columns; see *Reconstructed numbers*. Default is `FALSE`.
#' @param rounding,threshold,symmetric,tolerance Passed on to [`grim()`].
#' @param ... Further arguments passed on to [`grim()`].
#'
#' @return A tibble with these columns:
#' - `x`, `n`: the inputs.
#' - `digits_x`: the number of decimal places in `x`, as given.
#' - `items_averaged`: the number of items averaged into each score.
#' - `consistency`: GRIM consistency of `x`, `n`, and `items_averaged`. `NA` for
#'   a case that cannot be decided, such as one with a missing value.
#' - `probability`: the probability of GRIM inconsistency; see
#'   [`grim_probability()`].
#' - Any other columns from `data`.
#'
#'   The tibble has the class `scrutiny_grim_map`, which is recognized by
#'   [`audit()`] and [`grim_plot()`].
#'
#' @section Reconstructed numbers: With `show_rec = TRUE`, the output has these
#'   further columns, the same ones for every rounding method:
#'
#' - `rec_sum`: the product of `x` and `n` (times `items_averaged`), i.e., the
#'   sum total implied by the reported mean. It is generally not a whole number.
#' - `sum_lower`, `sum_upper`: the least and the greatest whole-number sum total
#'   that would have been reported as `x`. A value set is consistent exactly if
#'   `sum_lower` is not greater than `sum_upper`. If it is inconsistent, the two
#'   are the whole numbers on either side of `rec_sum`, so their gap is always 1
#'   and does not measure how far off the reported values are.
#' - `rec_x_upper`, `rec_x_lower`: the two achievable means closest to `x`,
#'   i.e., `rec_sum` rounded up and down, divided by `n` (times
#'   `items_averaged`). With `percent = TRUE`, these are percentages like `x`;
#'   the sum columns are not affected.
#'
#' @section Summaries with [`audit()`]: `audit()` on `grim_map()` output
#'   returns a tibble with one row and these columns:
#'
#' 1. `incons_cases`: number of GRIM-inconsistent value sets.
#' 2. `all_cases`: total number of value sets.
#' 3. `incons_rate`: proportion of GRIM-inconsistent value sets.
#' 4. `mean_grim_prob`: mean probability of GRIM inconsistency.
#' 5. `incons_to_prob`: ratio of `incons_rate` to `mean_grim_prob`, both
#'    computed over the value sets that could be decided.
#' 6. `testable_cases`: number of value sets with a positive `probability`.
#' 7. `testable_rate`: proportion of such value sets.
#'
#' @include audit.R grim.R grim-stats.R function-map.R restore-zeros.R
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876
#'
#' @export
#'
#' @examples
#' # Use `grim_map()` on data like these:
#' pigs1
#'
#' # The `consistency` column shows whether the
#' # values to its left are GRIM-consistent:
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1)
#'
#' # Show the reconstructed numbers:
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1, show_rec = TRUE)
#'
#' # Get summaries with `audit()`:
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1) |>
#'   audit()

grim_map <- function_map(
  .fun = grim_scalar,
  .reported = c("x", "n"),
  .name_test = "GRIM",
  .args_by_row = "digits_x",
  .cols_helper = "items_averaged",
  .col_names = c(
    "consistency",
    "rec_sum",
    "sum_lower",
    "sum_upper",
    "rec_x_upper",
    "rec_x_lower"
  ),
  # `probability` is not part of `grim_scalar()`'s return value: it comes from
  # `grim_probability()`, applied to the same per-row input. Making
  # `grim_scalar()` return it as well would change what `grim()` returns:
  .cols_derived = list(probability = grim_probability)
)
