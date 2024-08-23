
#' @include audit.R grim-map.R
#' @export

audit.scr_grim_map <- function(data) {

  # Compute the summary values of interest ---

  # 1. the number of GRIM-inconsistent cases;
  # 2. the total number of cases;
  # 3. the proportion of GRIM-inconsistent cases:
  out <- audit_cols_minimal(data, "GRIM")

  # 4. the average of GRIM probabilitys:
  mean_grim_prob <- data %>%
    dplyr::summarise(mean_grim_prob = mean(.data$probability)) %>%
    as.numeric()

  # 5. the ratio of the proportion of GRIM-inconsistent cases to the average of
  # GRIM probabilities:
  incons_rate <- out[[3L]]
  incons_to_prob <- incons_rate / mean_grim_prob

  # 6. the number of GRIM-testable cases:
  testable_cases <- data %>%
    dplyr::filter(.data$probability > 0) %>%
    nrow()

  # 7. the proportion of GRIM-testable cases:
  all_cases <- out[[2L]]
  testable_rate <- testable_cases / all_cases

  # Finally, collect all of these values in a resulting tibble --
  tibble::tibble(
    out, mean_grim_prob, incons_to_prob, testable_cases, testable_rate
  )
}
