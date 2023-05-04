
#' @include function-map.R
#' @include function-map-seq.R
#' @include function-map-total-n.R

is_percentage_of <- function(absolute, percentage, total,
                             tolerance = .Machine$double.eps^0.5) {
  dplyr::near(absolute, (percentage * total) / 100, tol = tolerance)
}

add_up_to <- function(summands, total, tolerance = .Machine$double.eps^0.5) {
  dplyr::near(sum(summands), total, tol = tolerance)
}


# Basic mappers -----------------------------------------------------------

is_percentage_of_map <- function_map(
  .fun = is_percentage_of,
  .reported = c("absolute", "percentage", "total"),
  .name_test = "percentage"
)

add_up_to_map <- function_map(
  .fun = add_up_to,
  .reported = c("summands", "total"),
  .name_test = "add-up"
)


# Sequence mappers --------------------------------------------------------

is_percentage_of_map_seq <- function_map_seq(
  .fun = is_percentage_of_map,
  .reported = c("absolute", "percentage", "total"),
  .name_test = "percentage"
)

add_up_to_map_seq <- function_map_seq(
  .fun = add_up_to_map,
  .reported = c("summands", "total"),
  .name_test = "add-up"
)


# (No total-n mappers here; they don't seem useful for these tests.)

