#
#
# # Inspired by `janitor::round_to_fraction()`;
# # https://github.com/sfirke/janitor/blob/main/R/round_to_fraction.R:
# x <- 0.22
# fractions <- 4
# digits <- 2
#
# # frac_factor <- fractions * (10^digits)
#
# x <- x * (10 ^ (digits - 1))
#
# # Here is the core functionality of `janitor::round_to_fraction()`. The question
# # now is: Simply refer to this function, or find a way to generalize it across
# # levels of decimal depth? (`janitor::round_to_fraction()` only applies that
# # fractional logic to integers.)
#
# round(x = (x * fractions), digits = 0) / fractions / (10 ^ (digits - 1))
#
#
#
# # round_fraction <- function(x = NULL, decimals = NULL, rounding = "up",
# #                            threshold = NULL, symmetric = FALSE)
#
