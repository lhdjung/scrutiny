#' GRIMMER-test many cases at once
#'
#' @description `grimmer_map()` applies [`grimmer()`] to every row of a data
#'   frame with means, standard deviations, and sample sizes.
#'
#'   Call [`audit()`] on the output for summary statistics. [`grim_plot()`]
#'   accepts it as well.
#'
#' @param data Tibble with columns `x`, `sd`, and `n`, and optionally
#'   `items_averaged`. Any other columns are returned alongside the results.
#' @param digits_x,digits_sd Integer. The number of decimal places in `x` and
#'   `sd`, including trailing zeros. There are no defaults, since a numeric
#'   value does not keep them (`1.40` is `1.4`). Either a single number for the
#'   whole column or one number per row of `data`.
#' @param x,sd,n Optionally, the names of the columns in `data` that contain
#'   these values, if they differ from the defaults.
#' @param items_averaged Integer. As in [`grim_map()`], for `x` and `sd`.
#' @param rounding,threshold,symmetric,sd_type,tolerance Passed on to
#'   [`grimmer()`].
#' @param ... Further arguments passed on to [`grimmer()`].
#'
#' @return A tibble with these columns:
#' - `x`, `sd`, `n`: the inputs.
#' - `digits_x`, `digits_sd`: the numbers of decimal places, as given.
#' - `items_averaged`: the number of items averaged into each score.
#' - `consistency`: GRIMMER consistency of `x`, `sd`, `n`, and `items_averaged`.
#'   `NA` for a case that cannot be decided, such as one with a missing value.
#' - `reason`: `"Passed all"` if consistent; otherwise the test that failed.
#' - Any other columns from `data`.
#'
#' The `reason` column refers to GRIM and the GRIMMER tests of Allard (2018):
#'
#' 1. The reconstructed sum of squared observations must be a whole number,
#'    and one that `n` whole numbers with the reconstructed sum can reach: at
#'    least the sum of squares of these numbers as close together as possible.
#'    For `n < 5`, some larger sums of squares are impossible, too.
#' 2. The reconstructed SD must match the reported one. Test 1 implies this,
#'    so it is never a reason.
#' 3. The parity of the reconstructed sum of squared observations must match
#'    that of the reconstructed sum of the values.
#'
#' A negative `sd` gives `"Negative SD"`.
#'
#' Under `sd_type = "either"`, a consistent value set gives
#' `"Passed all (sample SD)"` or, if it passes as a population SD only,
#' `"Passed all (population SD)"`. If the two conventions fail for different
#' reasons, the `reason` names both, and [`audit()`] counts both.
#'
#' The tibble has the class `scrutiny_grimmer_map`, which is recognized by
#'   [`audit()`] and [`grim_plot()`].
#'
#' @section Summaries with [`audit()`]: `audit()` on `grimmer_map()` output
#'   returns a tibble with one row and these columns:
#'
#' 1. `incons_cases`: number of GRIMMER-inconsistent value sets.
#' 2. `all_cases`: total number of value sets.
#' 3. `incons_rate`: proportion of GRIMMER-inconsistent value sets.
#' 4. `fail_grim`: number of value sets that fail GRIM.
#' 5. `fail_test1`, `fail_test3`: number of value sets that fail GRIMMER's
#'    first or third test. Test 2 is implied by test 1, so it never fails.
#'
#' @include audit.R grimmer.R function-map.R
#'
#' @references Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
#'   possibility of standard deviations.
#'   https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/
#'
#'   Anaya, J. (2016). The GRIMMER test: A method for testing the validity of
#'   reported measures of variability. *PeerJ Preprints.*
#'   https://peerj.com/preprints/2400v1/
#'
#' @export
#'
#' @examples
#' # Use `grimmer_map()` on data like these:
#' pigs5
#'
#' # The `consistency` column shows whether the values to its left are
#' # GRIMMER-consistent. If they aren't, the `reason` column gives the reason:
#' pigs5 |>
#'   grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1)
#'
#' # Get summaries with `audit()`:
#' pigs5 |>
#'   grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1) |>
#'   audit()

grimmer_map <- function_map(
  .fun = grimmer_scalar,
  .reported = c("x", "sd", "n"),
  .name_test = "GRIMMER",
  .args_by_row = c("digits_x", "digits_sd"),
  # Unlike `grimmer()`, the mapper shows the reasons for inconsistencies by
  # default -- they fit into a column, and `audit()` counts them:
  .args_fixed = list(show_reason = TRUE),
  .cols_helper = "items_averaged",
  .col_names = c("consistency", "reason")
)
