# grimmer_scalar <- function(x, sd, n, items = 1, show_reason = FALSE,
#                            rounding = "up_or_down", threshold = 5,
#                            symmetric = FALSE,
#                            tolerance = .Machine$double.eps^0.5) {
#
#   # Checks ---
#
#   check_type(x,  "character")
#   check_type(sd, "character")
#   check_type(n,  c("double", "integer"))
#
#
#   # Main part ---
#
#   digits_x  <- decimal_places_scalar(x)
#   digits_sd <- decimal_places_scalar(sd)
#
#   # The following check has first been modified and then outcommented because I
#   # don't think it's really necessary. If it was, the same check (and, depending
#   # on the inputs, potentially the same early return or error) would have to be
#   # included within GRIM functions themselves. However, if the size of `n`
#   # relative to `10 ^ digits_x` makes GRIM-inconsistencies impossible a priori,
#   # the fact that the value set is not testable simply means that GRIM will
#   # never find any inconsistencies. The bottom line, then, is that the value set
#   # is consistent, and the function should first and foremost state this fact.
#   # (It might still inform about the reason for GRIM consistencies, like the
#   # `show_reason` argument below does for GRIMMER *in*consistencies. Indeed, the
#   # `ratio` column in `grim_map()'s` also output holds such information.) If the
#   # check does end up being performed, please consider the correction below
#   # (`>=` versus `>`):
#
#   # # Departing from the original algorithm, this check also fails if `n` is equal
#   # # to `10 ^ digits_x`, not just if `n` is greater. That is because a value set
#   # # is not GRIM-testable if these two numbers are equal, either -- the numerator
#   # # of the "GRIM ratio" formula
#   # # (https://lhdjung.github.io/scrutiny/articles/grim.html#the-grim-ratio) will
#   # # then be zero, as will be the resulting probability of GRIM inconsistency.
#   # if (n >= 10 ^ digits_x) {
#   #   cli::cli_abort(c(
#   #     "Not GRIM-testable.",
#   #     "x" = "GRIMMER is only applicable to GRIM-testable value sets.",
#   #     "x" = "Here, the sample size ({n}) is too large to be tested \\
#   #     against a mean with only {digits_x} decimal places."
#   #   ))
#   # }
#
#   grim_consistency <- grim_scalar(
#     x = x, n = n, items = items, rounding = rounding, threshold = threshold,
#     symmetric = symmetric, tolerance = tolerance, show_rec = FALSE
#   )
#
#   # At this point, any trailing zeros in `x` and `sd` have already been
#   # captured, so that these variables are only relevant in terms of their
#   # numeric values going forward:
#   x  <- as.numeric(x)
#   sd <- as.numeric(sd)
#
#   # Lower and upper bounds for the SD:
#
#   p10 <- 10 ^ digits_sd
#   p10_by_5 <- 5 / p10
#
#   # Lower bound...
#   if (sd < p10_by_5) {
#     sd_lower <- 0
#   } else {
#     sd_lower <- sd - p10_by_5
#   }
#
#   # ...and upper bound:
#   sd_upper <- sd + p10_by_5
#
#   #Computes the lower and upper bounds for the sum of squares of items.
#
#   # In the original, this block is further up:
#   n_items <- n * items
#   rec_sum <- x * n_items
#
#   rec_sum_rounded <- reround(
#     x = rec_sum,
#     digits = digits_sd,
#     rounding = rounding,
#     threshold = threshold,
#     symmetric = symmetric
#   )
#
#   rec_sum_rounded <- round(rec_sum)
#   x_real <- rec_sum_rounded / n
#
#   sum_squares_lower <- (n - 1) * sd_lower ^ 2 + n * x_real ^ 2
#   sum_squares_upper <- (n - 1) * sd_upper ^ 2 + n * x_real ^ 2
#
#   # Lowerbound <- (n-1)*Lsigma^2+n*x_real^2
#   # Upperbound <- (n-1)*Usigma^2+n*x_real^2
#
#   #Checks that there is at least an integer between the lower and upperbound
#
#   pass_test1 <- ceiling(sum_squares_lower) > floor(sum_squares_upper)
#
#   if (!pass_test1) {
#     if (show_reason) {
#       out <- list(FALSE, "no_integer_diff")
#       return(out)
#     }
#     return(FALSE)
#   }
#
#   # FirstTest<- ifelse(ceiling(Lowerbound)>floor(Upperbound), FALSE, TRUE)
#   #
#   # if(FirstTest==FALSE){
#   #   return("GRIMMER inconsistent")
#   # }
#
#
#   #Takes a vector of all the integers between the lowerbound and upperbound
#
#   integers_possible <- ceiling(sum_squares_lower):floor(sum_squares_upper)
#
#   #Creates the predicted variance and sd
#
#   var_predicted <- (integers_possible - n * x_real ^ 2) / (n - 1)
#   sd_predicted  <- sqrt(var_predicted)
#
#   # var_predicted <- (integers_possible-n*x_real^2)/(n-1)
#   # sd_predicted <- sqrt(var_predicted)
#
#
#   #Computes whether one sd_predicted matches the sd (trying to round both down and up)
#
#   sd_rec <- reround(
#     x = sd_predicted,
#     digits = digits_sd,
#     rounding = rounding,
#     threshold = threshold,
#     symmetric = symmetric
#   )
#
#   pass_test2 <- any(sd_rec == sd)
#
#   if (!pass_test2) {
#     if (show_reason) {
#       out <- list(FALSE, "no_sd_match")
#       return(out)
#     }
#     return(FALSE)
#   }
#
#   # Rounded_sd_down <- round_down(sd_predicted, digits_sd)
#   # Rounded_sd_up <- round_up(sd_predicted, digits_sd)
#   #
#   # Matches_sd <- Rounded_sd_down==sd | Rounded_sd_up==sd
#
#   # if(sum(Matches_sd)==0){
#   #   return("GRIMMER inconsistent")
#   # }
#
#   #Computes first whether there is any integer between lower and upper bound, and then whether there is
#   #an integer of the correct oddness between the lower and upper bounds.
#
#   rec_sum_rounded_is_even    <- is_even(rec_sum_rounded)
#   integers_possible_are_even <- is_even(integers_possible)
#
#   # Compare the Boolean values that denote even parity:
#   pass_test3 <- rec_sum_rounded_is_even == integers_possible_are_even
#
#   if (!pass_test3) {
#     if (show_reason) {
#       out <- list(FALSE, "no_parity_match")
#       return(out)
#     }
#     return(FALSE)
#   }
#
#   # If the inputs have passed all these tests, consistency will be `TRUE`, but a
#   # `reason` column within `grimmer_map()` still needs a value. As there is no
#   # inconsistency, there is no reason that explains an inconsistency, and the
#   # `reason` value is therefore `NA`:
#   if (show_reason) {
#     out <- list(TRUE, NA)
#     return(out)
#   }
#
#   return(TRUE)
#
#   # oddness <- rec_sum_rounded%%2
#   # Matches_Oddness <- integers_possible%%2==oddness
#   # Third_Test <- Matches_sd&Matches_Oddness
#   # return(ifelse(
#   #   sum(Third_Test)==0, "GRIMMER inconsistent", "The x and sd are consistent.")
#   # )
#
# }
