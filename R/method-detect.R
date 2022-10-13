
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

  # Tidying to long format makes the table more manageable:
  data_dup_tidy <- data_dup %>%
    tidyr::pivot_longer(
      cols      = dplyr::everything(),
      names_to  = "variable",
      values_to = "value_duplicated"
    )

  # Finally, group the table by the original variables, count duplicates,
  # fashion it a little, and compute the duplicate rate:
  out <- data_dup_tidy %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::count(value_duplicated) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$value_duplicated) %>%
    dplyr::select(variable, n) %>%
    dplyr::rename(n_duplicated = n) %>%
    dplyr::mutate(
      n_total  = nrow(data),
      dup_rate = .data$n_duplicated / nrow(data)
    )

  dup_total <- sum(out$n_duplicated)

  out <- dplyr::add_row(
    out, variable = ".total", n_duplicated = dup_total,
    n_total = sum(out$n_total),
    dup_rate = .data$n_duplicated / .data$n_total
  )

  return(out)
}


