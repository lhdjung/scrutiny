
#' @include audit.R duplicate-detect.R grim-map.R
#' @export


audit.scr_dup_detect <- function(data) {

  # Select the Boolean test columns (i.e., every second column):
  data_dup <- data[is_even(seq_len(ncol(data)))]

  # Extract original term names:
  orig_names <- data %>%
    dplyr::select(-names(data_dup)) %>%
    names()

  # Boolean columns get original term names (the "_dup" would be redundant):
  names(data_dup) <- orig_names

  # After saving the number of its rows, `data` is no longer needed:
  orig_nrow <- nrow(data)
  rm(data)

  # Tidying to long format makes the table more manageable. Then, group the
  # table by the original terms, count duplicates, fashion it a little, and
  # compute the duplicate rate:
  out <- data_dup %>%
    tidyr::pivot_longer(
      cols      = dplyr::everything(),
      names_to  = "term",
      values_to = "value_duplicated"
    ) %>%
    dplyr::group_by(.data$term) %>%
    dplyr::count(.data$value_duplicated) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$value_duplicated) %>%
    dplyr::select("term", "n") %>%
    dplyr::rename(dup_count = n) %>%
    dplyr::mutate(
      total_count = orig_nrow,
      dup_rate = .data$dup_count / orig_nrow
    )

  dplyr::add_row(
    out, term = ".total", dup_count = sum(out$dup_count),
    total_count = sum(out$total_count), dup_rate = .data$dup_count / .data$total_count
  )
}

