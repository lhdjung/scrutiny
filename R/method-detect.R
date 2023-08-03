
#' @include audit.R duplicate-detect.R grim-map.R
#' @export


audit.scr_dup_detect <- function(data) {

  # Select the Boolean test columns (i.e., every second column):
  data_dup <- data[is_even(seq_len(ncol(data)))]

  # Extract original variable names:
  orig_names <- data %>%
    dplyr::select(-names(data_dup)) %>%
    names()

  # Boolean columns get original variable names (the "_dup" would be redundant):
  names(data_dup) <- orig_names

  # After saving the number of its rows, `data` is no longer needed:
  orig_nrow <- nrow(data)
  rm(data)

  # Tidying to long format makes the table more manageable. Then, group the
  # table by the original variables, count duplicates, fashion it a little, and
  # compute the duplicate rate:
  out <- data_dup %>%
    tidyr::pivot_longer(
      cols      = dplyr::everything(),
      names_to  = "variable",
      values_to = "value_duplicated"
    ) %>%
    dplyr::group_by(variable) %>%
    dplyr::count(.data$value_duplicated) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$value_duplicated) %>%
    dplyr::select(variable, n) %>%
    dplyr::rename(n_duplicated = n) %>%
    dplyr::mutate(
      n_total  = orig_nrow,
      dup_rate = .data$n_duplicated / orig_nrow
    )

  dplyr::add_row(
    out, variable = ".total", n_duplicated = sum(out$n_duplicated),
    n_total = sum(out$n_total),
    dup_rate = .data$n_duplicated / .data$n_total
  )
}

