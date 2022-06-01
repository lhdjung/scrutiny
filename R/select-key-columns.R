

#' Select the key columns from a consistency-tested tibble
#'
#' @description If a data frame is the output of a consistency test mapper
#'   function such as `grim_map()`, call `select_key_columns()` to get all
#'   columns before `"consistency"`. These are the columns that were tested for
#'   consistency row by row within the mapper function.
#'
#'   Make sure the `*_map()` function fulfills scrutiny's requirements for
#'   mapper functions. See `vignette("consistency-tests")`.
#'
#' @param data Data frame.
#' @param before String (length 1). Name of the `data` column before which the
#'   columns will be selected. Default is `"consistency"`.
#'
#' @export
#'
#' @examples
#' # Output of scrutiny's `*_map()` functions
#' # looks like this:
#' df <- grim_map(pigs1)
#'
#' # Get the columns before `"consistency"`:
#' select_key_columns(df)


select_key_columns <- function(data, before = "consistency") {
  index_last_key_col  <- match(before, colnames(data)) - 1
  data[1:index_last_key_col]
}
