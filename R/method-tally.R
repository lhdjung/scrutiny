
#' @include audit.R
#' @export

audit.scr_dup_tally <- function(data) {
  # Select the logical test columns (i.e., every second column):
  data_dup <- data[is_even(seq_len(ncol(data)))]
  # Summarize these columns:
  audit_summary_stats(data_dup, everything(), total = TRUE)
}
