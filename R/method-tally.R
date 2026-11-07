#' @include audit.R
#' @export

audit.scrutiny_dup_tally <- function(data, ...) {
  # The tally columns are integer ones named with an `"_n"` suffix. Not every
  # second column: `select()` and `relocate()` keep the class, and `mutate()`
  # may add other integer columns.
  is_tally_col <- vapply(data, is.integer, logical(1L)) &
    endsWith(colnames(data), "_n")
  data_dup <- data[is_tally_col]
  audit_summary_stats(data_dup, everything(), total = TRUE)
}
