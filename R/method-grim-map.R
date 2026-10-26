#' @include audit.R grim-map.R
#' @export

audit.scrutiny_grim_map <- function(data, ...) {
  check_cols_needed(data, c("consistency", "probability"))
  out <- audit_cols_minimal(data)

  # An undecidable case has no probability either, and must not turn the whole
  # summary into `NA`:
  mean_grim_prob <- data |>
    dplyr::summarise(mean_grim_prob = mean(.data$probability, na.rm = TRUE)) |>
    as.numeric()

  # Both rates must be taken over the same, decidable cases:
  decidable <- !is.na(data$consistency) & !is.na(data$probability)
  incons_to_prob <-
    mean(!data$consistency[decidable]) / mean(data$probability[decidable])

  testable_cases <- data |>
    dplyr::filter(.data$probability > 0) |>
    nrow()

  all_cases <- out[[2L]]
  testable_rate <- testable_cases / all_cases

  tibble::new_tibble(
    x = list(
      incons_cases = out$incons_cases,
      all_cases = out$all_cases,
      incons_rate = out$incons_rate,
      mean_grim_prob = mean_grim_prob,
      incons_to_prob = incons_to_prob,
      testable_cases = testable_cases,
      testable_rate = testable_rate
    ),
    nrow = 1L,
    class = NULL
  )
}
