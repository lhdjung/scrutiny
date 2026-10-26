#' @include debit-map.R
#' @export

audit.scrutiny_debit_map <- function(data, ...) {
  check_cols_needed(data, c("consistency", "x", "sd", "n"))
  out <- audit_cols_minimal(data)

  # A missing value makes a case undecidable; it should not also erase the
  # summary of the cases that could be decided:
  mean_x <- mean(data$x, na.rm = TRUE)

  mean_sd <- mean(data$sd, na.rm = TRUE)

  distinct_n <- dplyr::n_distinct(data$n, na.rm = TRUE)

  tibble::tibble(out, mean_x, mean_sd, distinct_n)
}
