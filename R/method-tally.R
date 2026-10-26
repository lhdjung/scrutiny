#' @include audit.R
#' @export

audit.scrutiny_dup_tally <- function(data, ...) {
  # The logical test columns are every second one:
  is_test_col <- data |>
    ncol() |>
    seq_len() |>
    is_even()
  data_dup <- data[is_test_col]
  audit_summary_stats(data_dup, everything(), total = TRUE)
}
