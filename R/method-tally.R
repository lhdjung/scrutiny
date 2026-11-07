#' @include audit.R
#' @export

audit.scrutiny_dup_tally <- function(data, ...) {
  # The tally columns are the integer ones that the `duplicate_tally()` call
  # named. Not every second column, nor every one with the suffix: `select()`
  # and `relocate()` keep the class, and `mutate()` may add other integer
  # columns, such as `extra_n`.
  cols_tally <- attr(data, "scrutiny", exact = TRUE)$cols_tally
  is_tally_col <- vapply(data, is.integer, logical(1L)) &
    colnames(data) %in% cols_tally
  if (!any(is_tally_col)) {
    abort_in_export(
      "No tally columns found in `data`.",
      "x" = "Tally columns are the integer columns that \\
      {.fn duplicate_tally} added, such as {.val {cols_tally[1L]}}, under \\
      the names it gave them."
    )
  }
  data_dup <- data[is_tally_col]
  audit_summary_stats(data_dup, everything(), total = TRUE)
}
