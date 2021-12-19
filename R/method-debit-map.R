
#' @include debit-map.R
#' @export

audit.scr_debit_map <- function(data) {

  # Compute the summary values of interest --

  # 1. the number of DEBIT-inconsistent cases:
  incons_cases <- data %>%
    dplyr::filter(.data$consistency == FALSE) %>%
    dplyr::count() %>%
    as.numeric()

  # 2. the total number of cases:
  all_cases <- nrow(data)

  # 3. the proportion of DEBIT-inconsistent cases:
  incons_rate <- as.numeric(incons_cases / all_cases)

  # 4. the mean `x` value:
  mean_x <- data %>%
    dplyr::summarise(mean_x = mean(as.numeric(.data$x))) %>%
    as.numeric()

  # 5. the mean `sd` value:
  mean_sd <- data %>%
    dplyr::summarise(mean_sd = mean(as.numeric(.data$sd))) %>%
    as.numeric()

  # 6. the number of distinct `n` values:
  distinct_n <- data %>%
    dplyr::distinct(.data$n) %>%
    nrow() %>%
    as.numeric()


  # Finally, collect all of these values in a resulting tibble --

  # (Number:)         1            2           3          4
  tibble::tibble(incons_cases, all_cases, incons_rate, mean_x,
  # (Number:)        5        6
                 mean_sd, distinct_n)

}

