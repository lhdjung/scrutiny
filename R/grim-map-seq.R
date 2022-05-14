
#' GRIM-testing with dispersed inputs
#'
#' `grim_map_seq()` performs GRIM-testing with values surrounding the input
#' values. This provides an easy and powerful way to assess whether small errors
#' in computing or reporting may be responsible for GRIM-inconsistencies in
#' published statistics.
#'
#' @param data A data frame that `grim_map()` could take.
#' @param var String. Names of the columns that will be dispersed. Default is
#'   `c("x", "n")`.
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
#' @param
#' items,percent,show_rec,show_prob,rounding,threshold,symmetric,tolerance,extra
#' Arguments passed down to `grim_map()`.
#'
#' @return
#' @export
#'
#' @examples


grim_map_seq <- function_map_seq(
  .fun = grim_map,
  .reported = c("x", "n"),
  .name_test = "GRIM",
  .name_class = "scr_grim_map_seq"
)
