
# NOTE: This file contains some tests that will likely be part of the next minor
# version, i.e., scrutiny 0.3.0. I'm putting them on hold for now because some
# questions about numeric tolerance have yet to be answered.


# @include function-map.R
# @include function-map-seq.R
# @include function-map-total-n.R


# Basic functions ---------------------------------------------------------

# Is an absolute a given percentage of a total?
#
# This divides a reported absolute value by the total and rounds it to the
# number of decimal places with which the percentage was reported. It then
# checks
#
# @param absolute
# @param percentage
# @param total
# @param rounding
# @param threshold
# @param symmetric
# @param tolerance
#
# @return
# @export
#
# @examples

# check_percentage <- function(absolute, percentage, total,
#                              rounding = "up_or_down", threshold = 5,
#                              symmetric = FALSE,
#                              tolerance = .Machine$double.eps^0.5) {
#   # Reconstruct the rounded percentage. This counts two extra decimal places to
#   # offset those that are lost by transforming the raw `absolute / total` ratio
#   # into a percentage.
#   percentage_rec <- 100 * reround(
#     x = absolute / total,
#     digits = decimal_places_scalar(percentage) + 2L,
#     rounding = rounding,
#     symmetric = symmetric
#   )
#   # Test for equality. This uses `dplyr::near()` instead of `==` to avoid
#   # spurious differences due to floating-point number issues. It uses `any()`
#   # because `reround()` above returns a length-two vector by default of
#   # `rounding = "up_or_down"`: one number rounds up, the other one rounds down.
#   any(dplyr::near(percentage, percentage_rec, tol = tolerance))
# }

# # Mapping `check_sum()` to a data frame will require some work on the function
# # factories because the number and names of key columns (i.e., columns that
# # contain the summands) needs to be variable.
# check_sum <- function(summands, total, tolerance = .Machine$double.eps^0.5) {
#   dplyr::near(sum(summands), total, tol = tolerance)
# }


# Basic mappers -----------------------------------------------------------

# check_percentage_map <- function_map(
#   .fun = check_percentage,
#   .reported = c("absolute", "percentage", "total"),
#   .name_test = "percentage"
# )



# Sequence mappers --------------------------------------------------------

# check_percentage_map_seq <- function_map_seq(
#   .fun = check_percentage_map,
#   .reported = c("absolute", "percentage", "total"),
#   .name_test = "percentage"
# )


# (No total-n mappers here; they don't seem useful for these tests.)

