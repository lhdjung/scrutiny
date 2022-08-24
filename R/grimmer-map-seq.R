
#' GRIMMER-testing with dispersed inputs
#'
#' @description `grimmer_map_seq()` performs GRIMMER-testing with values
#'   surrounding the input values. This provides an easy and powerful way to
#'   assess whether small errors in computing or reporting may be responsible
#'   for GRIMMER-inconsistencies in published statistics.
#'
#'   Call `audit_seq()` on the results for summary statistics.
#'
#' @param data A data frame that `grimmer_map()` could take.
#' @param x,sd,n Optionally, specify these arguments as column names in `data`.
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
#' @param ... Arguments passed down to `grimmer_map()`. *(NOTE: Don't use the
#'   `items` argument. It currently contains a bug that will be fixed in
#'   scrutiny's next CRAN release.)*

#' @return A tibble (data frame) with detailed test results.

#' @section Summaries with `audit_seq()`: You can call `audit_seq()` following
#'   `grimmer_map_seq()`. It will return a data frame with these columns:
#'   - `x`, `sd`, and `n` are the original inputs,
#'   tested for `consistency` here.
#'   - `hits_total` is the total number of GRIMMER-consistent value sets
#'   found within the specified `dispersion` range.
#'   - `hits_x` is the number of GRIMMER-consistent value sets
#'   found by varying `x`.
#'   - Accordingly with `sd` and `hits_sd` as well as `n` and `hits_n`.
#'   - (Note that any consistent reported cases will be counted by the
#'   `hits_*` columns if both `include_reported` and `include_consistent`
#'   are set to `TRUE`.)
#'   - `diff_x` reports the absolute difference between `x` and the next
#'   consistent dispersed value (in dispersion steps, not the actual numeric
#'   difference). `diff_x_up` and `diff_x_down` report the difference to the
#'   next higher or lower consistent value, respectively.
#'   - `diff_sd`, `diff_sd_up`, and `diff_sd_down` do the same for `sd`.
#'   -  Likewise with `diff_n`, `diff_n_up`, and `diff_n_down`.

#' @include grimmer-map.R

#' @export

#' @examples
#' # `grimmer_map_seq()` can take any input
#' # that `grimmer_map()` can take:
#' pigs5
#'
#' # All the results:
#' out <- grimmer_map_seq(pigs5, include_consistent = TRUE)
#' out
#'
#' # Case-wise summaries with `audit_seq()`
#' # can be more important than the raw results:
#' out %>%
#'   audit_seq()


grimmer_map_seq <- function_map_seq(
  .fun = grimmer_map,
  .reported = c("x", "sd", "n"),
  .name_test = "GRIMMER"
)
