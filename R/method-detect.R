#' @include audit.R duplicate-detect.R grim-map.R
#' @export

audit.scrutiny_dup_detect <- function(data) {
  # Select the logical test columns (i.e., every second column):
  is_test_col <- data |>
    ncol() |>
    seq_len() |>
    is_even()
  data_dup <- data[is_test_col]

  # Extract original term names:
  orig_names <- data |>
    dplyr::select(-names(data_dup)) |>
    names()

  # Logical columns get original term names (the "_dup" would be redundant):
  names(data_dup) <- orig_names

  # Count per term. A value that is missing or was ignored has an `NA` test
  # result, so it is not known to be duplicated or not; it counts toward neither
  # `dup_count` nor `total_count`. A term without any duplicates still gets a
  # row, with a `dup_count` of zero:
  count_by_term <- function(f) vapply(data_dup, f, 1L, USE.NAMES = FALSE)
  out <- tibble::tibble(
    term = names(data_dup),
    dup_count = count_by_term(function(x) sum(x, na.rm = TRUE)),
    total_count = count_by_term(function(x) sum(!is.na(x))),
    dup_rate = .data$dup_count / .data$total_count
  ) |>
    dplyr::arrange(.data$term)

  out |>
    dplyr::add_row(
      term = ".total",
      dup_count = sum(out$dup_count),
      total_count = sum(out$total_count),
      dup_rate = .data$dup_count / .data$total_count
    )
}
