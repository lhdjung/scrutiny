#' @include audit.R
#' @export

audit.scrutiny_dup_tally <- function(data) {
  # Select the logical test columns (i.e., every second column):
  is_test_col <- data |>
    ncol() |>
    seq_len() |>
    is_even()
  data_dup <- data[is_test_col]
  # Summarize these columns:
  audit_summary_stats(data_dup, everything(), total = TRUE)
}
