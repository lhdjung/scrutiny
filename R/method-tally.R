
#' @include audit.R
#' @export


audit.scr_dup_tally <- function(data) {

  # Select the Boolean test columns (i.e., every second column):
  data_dup <- data[is_even(seq_len(ncol(data)))]

  # Boolean columns get original variable names (the "_n" would be redundant):
  names(data_dup) <- data %>%
    dplyr::select(-names(data_dup)) %>%
    names()

  audit_summary_stats(data_dup, everything(), total = TRUE)
}

