

#' @include duplicate-count-colpair.R
#' @export

audit.scr_dup_count_colpair <- function(data) {
  n <- nrow(data)

  count <- data$count
  rate_x <- data$rate_x
  rate_y <- data$rate_y

  count_min <- min(count)
  count_max <- max(count)
  count_mean <- mean(count)
  count_sd <- stats::sd(count)
  count_median <- median(count)

  rate_x_min <- min(rate_x)
  rate_x_max <- max(rate_x)
  rate_x_mean <- mean(rate_x)
  rate_x_sd <- stats::sd(rate_x)
  rate_x_median <- median(rate_x)

  rate_y_min <- min(rate_y)
  rate_y_max <- max(rate_y)
  rate_y_mean <- mean(rate_y)
  rate_y_sd <- stats::sd(rate_y)
  rate_y_median <- median(rate_y)

  tibble::tibble(
    # 1.
    n,
    # 2.-6.
    count_min, count_max, count_mean, count_sd, count_median,
    # 7.-11.
    rate_x_min, rate_x_max, rate_x_mean, rate_x_sd, rate_x_median,
    # 12.-16.
    rate_y_min, rate_y_max, rate_y_mean, rate_y_sd, rate_y_median
  )

}

