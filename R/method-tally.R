#' @include audit.R
#' @export

audit.scrutiny_dup_tally <- function(data, ...) {
  # The tally columns are integer ones named with the suffix of the
  # `duplicate_tally()` call. Not every second column: `select()` and
  # `relocate()` keep the class, and `mutate()` may add other integer columns.
  colname_end <- attr(data, "scrutiny", exact = TRUE)$colname_end
  suffix <- paste0("_", if (is.null(colname_end)) "n" else colname_end)
  is_tally_col <- vapply(data, is.integer, logical(1L)) &
    endsWith(colnames(data), suffix)
  if (!any(is_tally_col)) {
    abort_in_export(
      "No tally columns found in `data`.",
      "x" = "Tally columns are integer columns whose names end in \\
      {.val {suffix}}, as {.fn duplicate_tally} names them."
    )
  }
  data_dup <- data[is_tally_col]
  audit_summary_stats(data_dup, everything(), total = TRUE)
}
