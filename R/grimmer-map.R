#' GRIMMER-test many cases at once
#'
#' @description Call `grimmer_map()` to GRIMMER-test any number of combinations
#'   of mean, standard deviation, sample size, and number of items. Mapping
#'   function for GRIMMER testing.
#'
#'   For summary statistics, call [`audit()`] on the results. Visualize results
#'   using [`grim_plot()`], as with GRIM results.
#'
#' @param data Data frame with columns `x`, `sd`, `n`, and optionally `items`
#'   (see documentation for `grim()`). Any other columns in `data` will be
#'   returned alongside GRIMMER test results.
#' @param items Integer. If there is no `items` column in `data`, this specifies
#'   the number of items composing the `x` and `sd` values. Default is `1`, the
#'   most common case.
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
#' @param min_val,max_val Integer. Optionally, the minimum and maximum value
#'   that an individual response could take, as with the endpoints of a Likert
#'   scale. If both are specified, GRIMMER also tests whether values within that
#'   range could have spread out far enough to produce the reported `sd`, and
#'   whether `x` is on the scale at all. See the *Scale bounds* section in
#'   [`grimmer()`]. Both default to `NULL`, i.e., an unbounded scale.
#' @param x,sd,n Optionally, specify these arguments as column names in `data`.
#' @param show_reason Logical (length 1). Should there be a `reason` column that
#'   shows the reasons for inconsistencies and `"Passed all"` for consistent
#'   values? Default is `TRUE`. See below for reference.
#' @param rounding,threshold,symmetric,tolerance Further parameters of
#'   GRIMMER testing; see documentation for [`grimmer()`]. Note that
#'   `tolerance` is a real argument here, unlike in [`grim_map()`], where it is
#'   deprecated.
#' @param ... Arguments passed down to [`grimmer()`].

#' @return A tibble with these columns --
#' - `x`, `sd`, `n`: the inputs.
#' - `digits_x`, `digits_sd`: the number of decimal places in `x` and `sd`, as
#'   given by `digits_x` and `digits_sd`.
#' - `consistency`: GRIMMER consistency of `x`, `n`, and `items`. `NA` for a
#'   case that cannot be decided, such as one with a missing value.
#' - `reason`: If consistent, `"Passed all"`. If inconsistent, it says which
#'   test was failed (see below).
#' - `<extra>`: any columns from `data` other than `x`, `sd`, `n`, and `items`.
#'
#' The `reason` columns refers to GRIM and the three GRIMMER tests (Allard
#' 2018). Briefly, these are:
#'
#' 1. The reconstructed sum of squared observations must be a whole number.
#' 2. The reconstructed SD must match the reported one.
#' 3. The parity of the reconstructed sum of squared observations must match the
#'    parity of the reconstructed sum of integers of which the reported means
#'    are fractions; i.e., either both are even or both are odd.
#'
#' If `min_val` and `max_val` are specified, two further reasons can appear:
#' `"Mean out of scale range"` for an `x` outside of the scale, and `"GRIMMER
#' inconsistent (scale range)"` for a value set whose `sd` no sample within the
#' scale could have produced.
#'
#' The tibble has the `scrutiny_grimmer_map` class, which is recognized by the
#' [`audit()`] generic. [`grim_plot()`] recognizes it as well, so GRIMMER
#' results can be visualized just like GRIM results.

#' @section Summaries with [`audit()`]: There is an S3 method for [`audit()`],
#'   so you can call [`audit()`] following `grimmer_map()` to get a summary of
#'   `grimmer_map()`'s results. It is a tibble with a single row and these
#'   columns --
#'
#' 1. `incons_cases`: number of GRIMMER-inconsistent value sets.
#' 2. `all_cases`: total number of value sets.
#' 3. `incons_rate`: proportion of GRIMMER-inconsistent value sets.
#' 4. `fail_grim`: number of value sets that fail the GRIM test.
#' 5. `fail_test1`: number of value sets that fail the first GRIMMER test (see
#'     below).
#' 6. `fail_test2`: number of value sets that fail the second GRIMMER test.
#' 7. `fail_test3`: number of value sets that fail the third GRIMMER test.
#' 8. `fail_scale`: number of value sets that are inconsistent with the scale
#'     bounds given by `min_val` and `max_val`. Zero if these were not
#'     specified.
#'
#' The `reason` columns refers to the three GRIMMER tests (see Allard 2018).
#' These are:
#'
#' 1. The reconstructed sum of squared observations must be a whole number.
#' 2. The reconstructed SD must match the reported one.
#' 3. The parity of the reconstructed sum of squared observations must match the
#'    parity of the reconstructed sum of integers of which the reported means
#'    are fractions; i.e., either both are even or both are odd.

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
#' # The `consistency` column shows whether
#' # the values to its left are GRIMMER-consistent.
#' # If they aren't, the `reason` column says why:
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
