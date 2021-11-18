

#' @include duplicate-count.R
#' @export


audit.scr_dup_count <- function(data) {

  term <- names(data)
  n <- rep(nrow(data), 2)

  count_max <- data %>%
    dplyr::slice(1) %>%
    as.numeric()

  count_min <- data %>%
    dplyr::slice(nrow(data)) %>%
    as.numeric()

  value <- data$value
  count <- data$count

  mean <- c(mean(value), mean(count))
  sd <- c(sd(value), sd(count))
  median <- c(median(value), median(count))

  tibble::tibble(term, count_max, count_min, n, mean, sd, median)
}
