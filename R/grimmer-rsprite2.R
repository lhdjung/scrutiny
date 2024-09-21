
# NOTE: This file was taken from the rsprite2 package
# (https://github.com/LukasWallrich/rsprite2/blob/master/R/core-functions.R).
# However, it only contains two functions from the original file.

# The idea is to take them as a basis for revamping scrutiny's implementation of
# GRIMMER, notably fixing the `items` argument.

# # All the variable names:
# grimmer_scalar
# x
# sd
# n
# items
# digits_x
# digits_sd
# x_real
# sum_real
# n_items
# sd_lower
# sd_upper
# sum_squares_lower
# sum_squares_upper
# integers_possible
# var_predicted
# sd_predicted
# matches_parity
# pass_test1
# matches_sd
# pass_test3


# Helper function ---------------------------------------------------------

# # Determine minimum and maximum SDs for given scale ranges, N, and x.
# sd_bounds_measure <- function(n, x, min_val, max_val, sd_prec = NULL, items = 1) {
#
#   if (is.null(sd_prec)) {
#     sd_prec <- max(nchar(sub("^[0-9]*", "", x)) - 1, 0)
#   }
#
#   result <- c(-Inf, Inf)
#
#   aMax <- min_val                                # "aMax" means "value of a to produce the max sd"
#   aMin <- floor(x*items)/items
#   bMax <- max(max_val, min_val + 1, aMin + 1)   # sanity check (just max_val would normally be ok)
#   bMin <- aMin + 1/items
#   total <- round(x * n * items)/items
#
#   poss_values <- max_val
#   for (i in seq_len(items)) {
#     poss_values <- c(poss_values, min_val:(max_val-1) + (1 / items) * (i - 1))
#   }
#   poss_values <- sort(poss_values)
#
#   for (abm in list(c(aMin, bMin, 1), c(aMax, bMax, 2))) {
#
#     a <- abm[1]
#     b <- abm[2]
#     m <- abm[3]
#
#
#     k <- round((total - (n * b)) / (a - b))
#     k <- min(max(k, 1), n - 1)               # ensure there is at least one of each of two numbers
#     vec <- c(rep(a, k), rep(b, n - k))
#     diff <- sum(vec) - total
#
#     if ((diff < 0)) {
#       vec <- c(rep(a, k - 1), a + abs(diff), rep(b, n - k))
#     }
#     else if ((diff > 0)) {
#       vec <- c(rep(a, k), b - diff, rep(b, n - k - 1))
#     }
#
#     if (round(x(vec), sd_prec) != round(x, sd_prec) | !all(floor(vec*10e9) %in% floor(poss_values*10e9))) {
#       stop("Error in calculating range of possible standard deviations")
#     }
#
#     result[m] <- round(sd(vec), sd_prec)
#   }
#
#   return(result)
# }



# Main function -----------------------------------------------------------

# grimmer_scalar <- function(x, sd, n, m_prec = NULL, sd_prec = NULL, items = 1, min_val = NULL, max_val = NULL) {
#   if (is.null(m_prec)) {
#     m_prec <- max(nchar(sub("^[0-9]*", "", x)) - 1, 0)
#   }
#
#   if (is.null(sd_prec)) {
#     sd_prec <- max(nchar(sub("^[0-9]*", "", sd)) - 1, 0)
#   }
#
#   n_items = n * items
#
#   # Applies the GRIM test, and computes the possible x.
#   sum <- x * n_items
#   sum_real <- round(sum)
#   x_real <- sum_real / n_items
#
#   #Checks whether x and sd are within possible range
#   if (!is.null(min_val) & !is.null(max_val)) {
#     if (x < min_val | x > max_val) {
#       warning("The x must be between the scale minimum and maximum")
#       return(FALSE)
#     }
#     sd_bounds <- sd_bounds_measure(n, x, min_val, max_val, sd_prec, items)
#     if (sd < sd_bounds[1] | sd > sd_bounds[2]) {
#       warning("Given the scale minimum and maximum, the standard deviation has to be between ", sd_bounds[1], " and ", sd_bounds[2], ".")
#       return(FALSE)
#     }
#   }
#   # Creates functions to round a number consistently up or down, when the last digit is 5
#   round_down <- function(number, decimals = 2) {
#     to_round <- number * 10^(decimals + 1) - floor(number * 10^(decimals)) * 10
#     number_rounded <- ifelse(to_round == 5,
#                              floor(number * 10^decimals) / 10^decimals,
#                              round(number, digits = decimals))
#     return(number_rounded)
#   }
#
#   round_up <- function(number, decimals = 2) {
#     to_round <- number * 10^(decimals + 1) - floor(number * 10^(decimals)) * 10
#     number_rounded <- ifelse(to_round == 5,
#                              ceiling(number * 10^decimals) / 10^decimals,
#                              round(number, digits = decimals))
#     return(number_rounded)
#   }
#
#   # Applies the GRIM test, to see whether the reconstituted x is the same as the reported x (with both down and up rounding)
#
#   consistent_down <- round_down(number = x_real, decimals = m_prec) == x
#   consistent_up <- round_up(number = x_real, decimals = m_prec) == x
#
#   if (!consistent_down & !consistent_up) {
#     warning("GRIM inconsistent - so GRIMMER test cannot be run. See ?GRIM_test")
#     return(FALSE)
#   }
#
#   # Computes the lower and upper bounds for the sd.
#
#   sd_lower <- ifelse(sd < 5 / (10^(sd_prec+1)), 0, sd - 5 / (10^(sd_prec+1)))
#   sd_upper <- sd + 5 / (10^(sd_prec+1))
#
#   # Computes the lower and upper bounds for the sum of squares of items.
#
#   lower_bound <- ((n - 1) * sd_lower^2 + n * x_real^2)*items^2
#   upper_bound <- ((n - 1) * sd_upper^2 + n * x_real^2)*items^2
#
#   # Checks that there is at least an integer between the lower and upperbound
#
#   if (ceiling(lower_bound) > floor(upper_bound)) {
#     return(FALSE)
#   }
#
#   # Takes a vector of all the integers between the lowerbound and upperbound
#
#   integers_possible <- ceiling(lower_bound):floor(upper_bound)
#
#   # Creates the predicted variance and sd
#
#   var_predicted <- (integers_possible/items^2 - n * x_real^2) / (n - 1)
#   sd_predicted <- sqrt(var_predicted)
#
#   # Computes whether one sd_predicted matches the sd (trying to round both down and up)
#
#   sd_rounded_down <- round_down(sd_predicted, sd_prec)
#   sd_rounded_up <- round_up(sd_predicted, sd_prec)
#
#   matches_sd <- sd_rounded_down == sd | sd_rounded_up == sd
#
#   if (!any(matches_sd)) {
#     return(FALSE)
#   }
#
#   # Computes whether there is an integer of the correct parity between the lower and upper bounds.
#   parity <- sum_real %% 2
#   matches_parity <- integers_possible %% 2 == parity
#   return(any(matches_sd & matches_parity))
#
#   return(TRUE)
# }

