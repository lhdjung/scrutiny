
#' @include function-map.R
#' @include function-map-seq.R
#' @include function-map-total-n.R

is_percentage_of <- function(absolute, percentage, total,
                             rounding = "up_or_down", threshold = 5,
                             symmetric = FALSE,
                             tolerance = .Machine$double.eps^0.5) {
  # Reconstruct the rounded percentage. This counts two extra decimal places to
  # offset those that are lost by transforming the raw `absolute / total` ratio
  # into a percentage.
  percentage_rec <- 100 * reround(
    x = absolute / total,
    digits = decimal_places_scalar(percentage) + 2L,
    rounding = rounding,
    symmetric = symmetric
  )
  # Test for equality. This uses `dplyr::near()` instead of `==` to avoid
  # spurious differences due to floating-point number issues. It uses `any()`
  # because `reround()` above returns a length-two vector by default of
  # `rounding = "up_or_down"`: one number rounds up, the other one rounds down.
  any(dplyr::near(percentage, percentage_rec, tol = tolerance))
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



# Sequence mappers --------------------------------------------------------

is_percentage_of_map_seq <- function_map_seq(
  .fun = is_percentage_of_map,
  .reported = c("absolute", "percentage", "total"),
  .name_test = "percentage"
)


# (No total-n mappers here; they don't seem useful for these tests.)

