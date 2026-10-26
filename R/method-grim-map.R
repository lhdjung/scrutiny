#' @include audit.R grim-map.R
#' @export

audit.scrutiny_grim_map <- function(data, ...) {
  check_cols_needed(data, c("consistency", "exp_incon"))
  out <- audit_cols_minimal(data)

  testable_cases <- data |>
    dplyr::filter(.data$exp_incon > 0) |>
    nrow()

  # An undecidable case has no `exp_incon` either, and must not turn the whole
  # summary into `NA`:
  exp_incon <- mean(data$exp_incon, na.rm = TRUE)

  # Both rates must be taken over the same, decidable cases:
  decidable <- !is.na(data$consistency) & !is.na(data$exp_incon)
  ratio <- mean(!data$consistency[decidable]) / mean(data$exp_incon[decidable])

  tibble::new_tibble(
    x = list(
      all_cases = out$all_cases,
      testable_cases = testable_cases,
      incon_cases = out$incon_cases,
      emp_incon = out$emp_incon,
      exp_incon = exp_incon,
      ratio = ratio
    ),
    nrow = 1L,
    class = NULL
  )
}
