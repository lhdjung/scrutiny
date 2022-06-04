
#' Using DEBIT with dispersed inputs
#'
#' `debit_map_seq()` applies DEBIT with values surrounding the input values.
#' This provides an easy and powerful way to assess whether small errors in
#' computing or reporting may be responsible for debit-inconsistencies in
#' published statistics.
#'
#' @param data A data frame that `debit_map()` could take.
#' @param var String. Names of the columns that will be dispersed. Default is
#'   `c("x", "sd", "n")`.
#' @param dispersion Numeric. Sequence with steps up and down from the `var`
#'   inputs. It will be adjusted to these values' decimal levels. For example,
#'   with a reported `8.34`, the step size is `0.01`. Default is `1:5`, for five
#'   steps up and down.
#' @param out_min,out_max If specified, output will be restricted so that it's
#'   not below `out_min` or above `out_max`. Defaults are `"auto"` for
#'   `out_min`, i.e., a minimum of one decimal unit above zero; and `NULL` for
#'   `out_max`, i.e., no maximum.
#' @param include_reported Boolean. Should the reported values themselves be
#'   included in the sequences originating from them? Default is `FALSE` because
#'   this might be redundant and bias the results.
#' @param include_consistent Boolean. Should the function also process
#'   consistent cases (from among those reported), not just inconsistent ones?
#'   Default is `FALSE` because the focus should be on clarifying
#'   inconsistencies.
#' @param reported,fun,name_test,name_class Arguments passed down to
#'   `function_map_seq()` but not documented here.
#' @param rounding,threshold,symmetric,show_rec Arguments passed down to
#'   `debit_map()`.

#' @section Summaries with `audit_seq()`: You can call `audit_seq()` following
#'   `debit_map_seq()`. It will return a data frame with these columns:
#'   - `x`, `sd`, and `n` are the original inputs, tested for `consistency` here.
#'   - `hits` is the number of DEBIT-consistent value combinations found within
#'   the specified `dispersion` range.
#'   - `diff_x` reports the absolute difference between `x` and the next
#'   consistent dispersed value (in dispersion steps, not the actual numeric
#'   difference). `diff_x_up` and `diff_x_down` report the difference to the
#'   next higher or lower consistent value, respectively.
#'   - `diff_sd`, `diff_sd_up`, and `diff_sd_down` do the same for `sd`.
#'   - Accordingly for `n`.

#' @include function-map-seq.R
#'
#' @export
#'
#' @examples
#' # `debit_map_seq()` can take any input
#' # that `debit_map()` can take:
#' pigs3
#'
#' # All the results:
#' out <- debit_map_seq(pigs3, include_consistent = TRUE)
#' out
#'
#' # Case-wise summaries with `audit_seq()`
#' # can be more important than the raw results:
#' audit_seq(out)



debit_map_seq <- function_map_seq(
  .fun = debit_map,
  .reported = c("x", "sd", "n"),
  .name_test = "DEBIT",
  .name_class = "scr_debit_map_seq"
)

