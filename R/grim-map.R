#' GRIM-test many cases at once
#'
#' @description Call `grim_map()` to GRIM-test any number of combinations of
#'   mean/proportion, sample size, and number of items. Mapping function for
#'   GRIM-testing.
#'
#'   Set `percent` to `TRUE` if the `x` values are percentages. This will
#'   convert `x` values to decimals and adjust the decimal count accordingly.
#'
#'   Display intermediary numbers from GRIM-testing in columns by setting
#'   `show_rec` to `TRUE`.
#'
#'   For summary statistics, call [`audit()`] on the results.
#'
#' @param data Data frame with columns `x`, `n`, and optionally `items` (see
#'   documentation for [`grim()`]). Any other columns in `data` will be returned
#'   alongside GRIM test results.
#' @param items Integer. If there is no `items` column in `data`, this specifies
#'   the number of items composing the `x` values. Default is 1, the most common
#'   case. Values from an `items` column or argument are multiplied with those
#'   in the `n` column, so there is no `items` column in the output. This is
#'   only for presentation and does not affect test results.
#' @param percent Logical. Set `percent` to `TRUE` if the `x` values are
#'   percentages. This will convert them to decimal numbers for testing and
#'   adjust the decimal count (i.e., increase it by 2). It also affects the
#'   `probability` column. Default is `FALSE`.
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default because it cannot be inferred from a
#'   numeric `x`, which has no trailing zeros: both `1.4` and `1.40` are the
#'   number `1.4`, but only the latter has `digits_x = 2`. Use a single number
#'   if the whole column was reported with the same number of decimal places,
#'   or one number per row of `data` if it varies.
#' @param x,n Optionally, specify these arguments as column names in `data`.
#' @param show_rec Logical. If set to `TRUE`, the reconstructed numbers from
#'   GRIM-testing are shown as columns. See section *Reconstructed numbers*
#'   below. Default is `FALSE`.
#' @param rounding,threshold,symmetric,tolerance Further parameters of
#'   GRIM-testing; see documentation for [`grim()`].
#' @param ... Arguments passed down to [`grim()`].

#' @return A tibble with these columns --
#' - `x`, `n`: the inputs.
#' - `digits_x`: the number of decimal places in `x`, as given by `digits_x`.
#' - `consistency`: GRIM consistency of `x`, `n`, and `items`. `NA` for a case
#'   that cannot be decided, such as one with a missing value.
#' - `probability`: the probability of GRIM inconsistency; see
#' [`grim_probability()`].
#' - `<extra>`: any columns from `data` other than `x`, `n`, and `items`.
#'
#'   The tibble has the `scrutiny_grim_map` class, which is recognized by the
#'   [`audit()`] generic.

#' @section Reconstructed numbers: If `show_rec` is set to `TRUE`, the output
#'   includes the following additional columns, the same ones for every rounding
#'   method:
#'
#' - `rec_sum`: the unrounded product of `x` and `n` (times `items`). The sum
#'   total of integer data is a whole number, so this is generally not one.
#' - `sum_lower`, `sum_upper`: the least and the greatest whole-number sum total
#'   that would have been reported as `x`. These are the numbers that decide the
#'   test: `x` is GRIM-consistent exactly if `sum_lower` is not greater than
#'   `sum_upper`, i.e., if at least one whole number falls between them. If it
#'   is inconsistent, the gap between the two says how far off the reported
#'   values are.
#' - `rec_x_upper`, `rec_x_lower`: the two reconstructed `x` values ("granules")
#'   closest to `x` -- `rec_sum` rounded up and down, divided by `n` (times
#'   `items`).
#'
#'   With `percent = TRUE`, the last two are percentages, like `x` itself, so
#'   that they can be read against it. The first three are not: a sum total of
#'   the underlying data is what it is, whichever scale the mean is reported on.

#' @section Summaries with [`audit()`]: There is an S3 method for [`audit()`],
#'   so you can call [`audit()`] following `grim_map()` to get a summary of
#'   `grim_map()`'s results. It is a tibble with one row and these columns --
#'
#' 1. `incons_cases`: number of GRIM-inconsistent value sets.
#' 2. `all_cases`: total number of value sets.
#' 3. `incons_rate`: proportion of GRIM-inconsistent value sets.
#' 4. `mean_grim_prob`: average probability of GRIM inconsistency.
#' 5. `incons_to_prob`: ratio of `incons_rate` to `mean_grim_prob`.
#' 6. `testable_cases`: number of GRIM-testable value sets (i.e., those with a
#' positive `probability`).
#' 7. `testable_rate`: proportion of GRIM-testable value sets.

#' @include audit.R grim.R grim-stats.R function-map.R restore-zeros.R

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
#' # The `consistency` column shows
#' # whether the values to its left
#' # are GRIM-consistent:
#' pigs1 |>
#'   grim_map(digits_x = 2)
#'
#' # Display intermediary numbers from
#' # GRIM-testing with `show_rec = TRUE`:
#' pigs1 |>
#'   grim_map(digits_x = 2, show_rec = TRUE)
#'
#' # Get summaries with `audit()`:
#' pigs1 |>
#'   grim_map(digits_x = 2) |>
#'   audit()

grim_map <- function_map(
  .fun = grim_scalar,
  .reported = c("x", "n"),
  .name_test = "GRIM",
  .args_by_row = "digits_x",
  .cols_helper = "items",
  .cols_helper_merge = c(items = "n"),
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
  .cols_derived = list(probability = grim_probability),
  # `grim_plot()` labels the axis differently for percentages, and it reads the
  # class rather than the argument because it only ever sees the output:
  .name_class_flags = c(percent = "scrutiny_percent_true")
)
