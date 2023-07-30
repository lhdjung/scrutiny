
#' @include duplicate-count.R
#' @export

audit.scr_dup_count <- function(data) {

  count <- data$count

  if (any(colnames(data) == "locations_n")) {
    locations_n <- data$locations_n
    tibble::tibble(
      term = c("count", "locations_n"),
      mean = c(mean(count), mean(locations_n)),
      sd = c(stats::sd(count), stats::sd(locations_n)),
      median = c(median(count), median(locations_n)),
      min = c(count[[nrow(data)]], min(locations_n)),
      max = c(count[[1L]], max(locations_n))
    )
  } else {
    tibble::tibble(
      term = "count",
      mean = mean(count),
      sd = stats::sd(count),
      median = median(count),
      min = count[[nrow(data)]],
      max = count[[1L]]
    )
  }
}


# # Possible additions, but they would require the return value to be a list
# of (two) tibbles:
# locations_modes <- mode_all_if_no_na(data$locations)
# locations_mode_frequency <- length(
#   data$locations[data$locations == locations_modes[1L]]
# )
# locations_mode_count <- length(locations_modes)
