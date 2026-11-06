#' @include audit.R
#' @export

audit.scrutiny_dup_tally <- function(data, ...) {
  # The tally columns are the integer ones; the values are strings. Not every
  # second column: `select()` and `relocate()` keep the class.
  is_tally_col <- vapply(data, is.integer, logical(1L))
  data_dup <- data[is_tally_col]
  audit_summary_stats(data_dup, everything(), total = TRUE)
}
