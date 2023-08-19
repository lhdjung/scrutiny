
#' @include debit-map.R
#' @export

audit.scr_debit_map <- function(data) {

  # Compute the summary values of interest --

  # 1. the number of DEBIT-inconsistent cases;
  # 2. the total number of cases;
  # 3. the proportion of DEBIT-inconsistent cases:
  out <- audit_cols_minimal(data, "DEBIT")

  # 4. the mean `x` value:
  mean_x <- data$x %>%
    as.numeric() %>%
    mean()

  # 5. the mean `sd` value:
  mean_sd <- data$sd %>%
    as.numeric() %>%
    mean()

  # 6. the number of distinct `n` values:
  distinct_n <- data$n %>%
    unique() %>%
    length()

  # Finally, collect all of these values in a resulting tibble --
  tibble::tibble(out, mean_x, mean_sd, distinct_n)
}
