#' GRIMMER-test many cases at once
#'
#' @description `grimmer_map()` applies [`grimmer()`] to every row of a data
#'   frame with means, standard deviations, and sample sizes.
#'
#'   Call [`audit()`] on the output for summary statistics. [`grim_plot()`]
#'   accepts it as well.
#'
#' @param data Tibble with columns `x`, `sd`, and `n`, and optionally `items`.
#'   Any other columns are returned alongside the results.
#' @param digits_x,digits_sd Integer. The number of decimal places in `x` and
#'   `sd`, including trailing zeros. There are no defaults, since a numeric
#'   value does not keep them (`1.40` is `1.4`). Either a single number for the
#'   whole column or one number per row of `data`.
#' @param x,sd,n Optionally, the names of the columns in `data` that contain
#'   these values, if they differ from the defaults.
#' @param items Integer. The number of items composing `x` and `sd`, if `data`
#'   has no `items` column. Default is `1`. `items` is multiplied into `n` for
#'   the output.
#' @param min_val,max_val Integer. Optionally, the bounds of the scale; see
#'   the *Scale bounds* section of [`grimmer()`]. Both default to `NULL`.
#' @param show_reason Logical. If `TRUE` (the default), a `reason` column states
#'   the reason for each inconsistency.
#' @param rounding,threshold,symmetric,tolerance Passed on to [`grimmer()`].
#' @param ... Further arguments passed on to [`grimmer()`].
#'
#' @return A tibble with these columns:
#' - `x`, `sd`, `n`: the inputs.
#' - `digits_x`, `digits_sd`: the numbers of decimal places, as given.
#' - `consistency`: GRIMMER consistency of `x`, `sd`, `n`, and `items`. `NA`
#'   for a case that cannot be decided, such as one with a missing value.
#' - `reason`: `"Passed all"` if consistent; otherwise the test that failed.
#' - Any other columns from `data`.
#'
#' The `reason` column refers to GRIM and the GRIMMER tests of Allard (2018):
#'
#' 1. The reconstructed sum of squared observations must be a whole number.
#' 2. The reconstructed SD must match the reported one. Test 1 implies this,
#'    so it is never a reason.
#' 3. The parity of the reconstructed sum of squared observations must match
#'    that of the reconstructed sum of the values.
#'
#' With `min_val` and `max_val`, the reasons `"Mean out of scale range"` and
#' `"GRIMMER inconsistent (scale range)"` can appear. A negative `sd` gives
#' `"Negative SD"`.
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
#' 5. `fail_test1`, `fail_test2`, `fail_test3`: number of value sets that fail
#'    each of the three GRIMMER tests.
#' 6. `fail_scale`: number of value sets inconsistent with `min_val` and
#'    `max_val`. Zero if these were not given.
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
#'   grimmer_map(digits_x = 2, digits_sd = 2)
#'
#' # Get summaries with `audit()`:
#' pigs5 |>
#'   grimmer_map(digits_x = 2, digits_sd = 2) |>
#'   audit()

grimmer_map <- function_map(
  .fun = grimmer_scalar,
  .reported = c("x", "sd", "n"),
  .name_test = "GRIMMER",
  .args_by_row = c("digits_x", "digits_sd"),
  # Unlike `grimmer()`, the mapper shows the reasons for inconsistencies by
  # default -- they fit into a column, and `audit()` counts them:
  .args_defaults = list(show_reason = TRUE),
  .cols_helper = "items",
  .cols_helper_merge = c(items = "n"),
  .col_names = c("consistency", "reason")
)
