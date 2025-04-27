

# tibble::tibble(status = c("dead", "live"), kids = c(15, 137), adolescents = c(12, 203))

odds_ratio_map <- function(data, x1, x2, or = or) {
  data %>%
    dplyr::summarize(
      {{ x1 }} := sum({{ x1 }}),
      {{ x2 }} := sum({{ x2 }})
    ) %>%
    dplyr::mutate()
}
