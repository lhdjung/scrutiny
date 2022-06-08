
#' @include audit.R grim-map.R
#' @export


audit.scr_grim_map <- function(data) {

  # If `data` is the output of `grim_map_seq()` or `grim_map_total_n()`, point
  # the user to the dedicated summary function for such output, i.e.,
  # `audit_seq()` or `audit_total_n()`, respectively:
  check_audit_special(data, "GRIM")

  # Compute the summary values of interest ---

  # 1. the number of GRIM-inconsistent cases:
  incons_cases <- nrow(data[!data$consistency, ])

  # 2. the total number of cases:
  all_cases <- nrow(data)

  # 3. the proportion of GRIM-inconsistent cases:
  incons_rate <- incons_cases / all_cases

  # 4. the average of GRIM ratios:
  mean_grim_ratio <- data %>%
    dplyr::summarise(mean_grim_ratio = mean(.data$ratio)) %>%
    as.numeric()

  # 5. the ratio of the proportion of GRIM-inconsistent cases to the average of
  # GRIM ratios:
  incons_to_ratio <- incons_rate / mean_grim_ratio

  # 6. the number of GRIM-testable cases:
  testable_cases <- data %>%
    dplyr::filter(.data$ratio > 0) %>%
    nrow()

  # 7. the proportion of GRIM-testable cases:
  testable_rate <- testable_cases / all_cases


  # Finally, collect all of these values in a resulting tibble --

  # (Number:)                1            2           3             4
  out <- tibble::tibble(incons_cases, all_cases, incons_rate, mean_grim_ratio,
  # (Number:)            5                6              7
                 incons_to_ratio, testable_cases, testable_rate)

  return(out)
}



# Alternative output tibble:

#  t <- tibble::tibble(incons_cases, all_cases, incons_rate, mean_grim_ratio,
#                      incons_to_ratio, testable_cases, testable_rate)
#  tibble::tibble(metric = names(t), value = as.numeric(t))


