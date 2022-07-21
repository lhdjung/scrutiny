
# Notes: Analytic-GRIMMER (A-GRIMMER) was developed by Aurélien Allard
# (https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/).
# His original algorithm received some modifications here, for three reasons:
# First, tapping scrutiny's infrastructure for implementing error detection
# techniques; for example, functions like `reround()` and
# `decimal_places_scalar()`. Second, changing the return value to Boolean, which
# is the expected output from the basic implementation of any consistency test
# within scrutiny. Third, adjusting variable names to the tidyverse style guide


# Translation of variables:
#
# original           --> scrutiny
# --------               --------
#
# aGrimmer           --> grimmer_scalar
# mean               --> x
# sd                 --> sd
# decimals_mean      --> digits_x  (argument removed; counting internally)
# decimals_SD        --> digits_sd (argument removed; counting internally)
# Lsigma             --> sd_lower
# Usigma             --> sd_upper
# Lowerbound         --> sum_squares_lower
# Upperbound         --> sum_squares_upper
# Possible_Integers  --> integers_possible
# Predicted_Variance --> var_predicted
# Predicted_SD       --> sd_predicted
# FirstTest          --> pass_test1
# Matches_SD         --> pass_test2 (filling in a naming gap)
# Third_Test         --> pass_test3




# Example data frame:
df <- tibble::tibble(
  n = 10:50,
  mean = seq(1.00, length.out = 41, by = 0.1),
  sd = round(mean * (3 / 5), 2)
)


# Copy & paste; leave outcommented:       test(filter = "grimmer")



# # Example inputs:
# n <- 40
# mean <- "1.03"
# sd <- "0.41"
# items = 1
# rounding = "up_or_down"
# threshold = 5
# symmetric = FALSE


# My take -----------------------------------------------------------------

grimmer_scalar <- function(n, mean, SD, items = 1, rounding = "up_or_down",
                           threshold = 5, symmetric = FALSE,
                           tolerance = .Machine$double.eps^0.5,
                           decimals_mean = 2, decimals_SD = 2) {

  # if(n>10^decimals_mean){
  #   print("The sample size is too big compared to the precision of the reported mean, it is not possible to apply GRIM.")
  # }

  #Applies the GRIM test, and computes the possible mean.

  sum <- mean*n
  realsum <- round(sum)
  realmean <- realsum/n



  # # Creates functions to round a number consistently up or down, when the last digit is 5
  #
  # round_down <- function(number, decimals=2){
  #   is_five <- number*10^(decimals+1)-floor(number*10^(decimals))*10
  #   number_rounded <- ifelse(is_five==5, floor(number*10^decimals)/10^decimals, round(number, digits = decimals))
  #   return(number_rounded)
  # }
  #
  # round_up <- function(number, decimals=2){
  #   is_five <- number*10^(decimals+1)-floor(number*10^(decimals))*10
  #   number_rounded <- ifelse(is_five==5, ceiling(number*10^decimals)/10^decimals, round(number, digits = decimals))
  #   return(number_rounded)
  # }
  #
  # # Applies the GRIM test, to see whether the reconstituted mean is the same as the reported mean (with both down and up rounding)
  #
  # consistency_down <- round_down(number = realmean, decimals = decimals_mean)==mean
  # consistency_up <- round_up(number = realmean, decimals = decimals_mean)==mean
  #
  # if(consistency_down+consistency_up==0){
  #   return(FALSE)
  # }


  grim_consistency <- grim_scalar(
    x = as.character(mean), n = n, items = items, rounding = rounding,
    threshold = threshold, symmetric = symmetric, tolerance = tolerance
  )

  if (!grim_consistency) {
    return(FALSE)
  }


  # #Computes the lower and upper bounds for the sd.
  #
  # Lsigma <- ifelse(SD<5/(10^decimals_SD), 0, SD-5/(10^decimals_SD))
  # Usigma <- SD+5/(10^decimals_SD)
  #
  # #Computes the lower and upper bounds for the sum of squares of items.
  #
  # Lowerbound <- (n-1)*Lsigma^2+n*realmean^2
  # Upperbound <- (n-1)*Usigma^2+n*realmean^2
  #
  # #Checks that there is at least an integer between the lower and upperbound
  #
  # FirstTest<- ifelse(ceiling(Lowerbound)>floor(Upperbound), FALSE, TRUE)
  #
  # if(FirstTest==FALSE){
  #   return(FALSE)
  # }


  p10 <- 10 ^ decimals_SD
  p10_frac <- 5 / p10

  # SD bounds, lower and upper:
  if (SD < p10_frac) {
    sd_lower <- 0
  } else {
    sd_lower <- SD - p10_frac
  }

  sd_upper <- SD + p10_frac

  # Sum of squares bounds, lower and upper:
  sum_squares_lower <- (n - 1) * sd_lower ^ 2 + n * realmean ^ 2
  sum_squares_upper <- (n - 1) * sd_upper ^ 2 + n * realmean ^ 2


  # For the moment, continue to use the old variable names by assigning the
  # newly derived -- and newly named -- values to them:
  Lowerbound <- sum_squares_lower
  Upperbound <- sum_squares_upper



  #Takes a vector of all the integers between the lowerbound and upperbound

  Possible_Integers <- ceiling(Lowerbound):floor(Upperbound)

  #Creates the predicted variance and sd

  Predicted_Variance <- (Possible_Integers-n*realmean^2)/(n-1)
  Predicted_SD <- sqrt(Predicted_Variance)

  # #Computes whether one Predicted_SD matches the SD (trying to round both down and up)
  #
  # Rounded_SD_down <- round_down(Predicted_SD, decimals_SD)
  # Rounded_SD_up <- round_up(Predicted_SD, decimals_SD)
  #
  # Matches_SD <- Rounded_SD_down==SD | Rounded_SD_up==SD

  sd_rec_rounded <- reround(
    x         = Predicted_SD,
    digits    = decimals_SD,
    rounding  = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  Matches_SD <- any(SD == sd_rec_rounded)

  if(sum(Matches_SD)==0){
    return(FALSE)
  }

  #Computes first whether there is any integer between lower and upper bound, and then whether there is
  #an integer of the correct oddness between the lower and upper bounds.
  oddness <- realsum%%2
  Matches_Oddness <- Possible_Integers%%2==oddness
  Third_Test <- Matches_SD&Matches_Oddness
  return(ifelse(
    sum(Third_Test)==0, FALSE, TRUE)
  )
}









# # Earlier attempts --------------------------------------------------------
#
#
#
# grimmer_scalar_old2 <- function(n, mean, sd, items = 1, rounding = "up_or_down",
#                                 threshold = 5, symmetric = FALSE) {
#
#   x <- mean
#
#   # Note: Move these checks to some higher-level function when this one's done
#   check_type(x,  "character")
#   check_type(sd, "character")
#   check_type(n, c("double", "integer"))
#
#   digits_x  <- decimal_places_scalar(x)
#   digits_sd <- decimal_places_scalar(sd)
#
#   # digits_x  <- decimals_mean
#   # digits_sd <- decimals_SD
#
#   # if(n>10^digits_x){
#   #   print("The sample size is too big compared to the precision of the reported x, it is not possible to apply GRIM.")
#   # }
#
#
#   x  <- as.numeric(x)
#   sd <- as.numeric(sd)
#
#
#   #Applies the GRIM test, and computes the possible x.
#
#   sum <- x*n
#   realsum <- round(sum)
#   realx <- realsum/n
#
#
#
#   # Creates functions to round a number consistently up or down, when the last digit is 5
#
#   round_down <- function(number, decimals=2){
#     is_five <- number*10^(decimals+1)-floor(number*10^(decimals))*10
#     number_rounded <- ifelse(is_five==5, floor(number*10^decimals)/10^decimals, round(number, digits = decimals))
#     return(number_rounded)
#   }
#
#   round_up <- function(number, decimals=2){
#     is_five <- number*10^(decimals+1)-floor(number*10^(decimals))*10
#     number_rounded <- ifelse(is_five==5, ceiling(number*10^decimals)/10^decimals, round(number, digits = decimals))
#     return(number_rounded)
#   }
#
#   # Applies the GRIM test, to see whether the reconstituted x is the same as the reported x (with both down and up rounding)
#
#   consistency_down <- round_down(number = realx, decimals = digits_x)==x
#   consistency_up <- round_up(number = realx, decimals = digits_x)==x
#
#   if(consistency_down+consistency_up==0){
#     return("GRIM inconsistent")
#   }
#
#
#   # #Computes the lower and upper bounds for the sd.
#   #
#   # sd_lower <- ifelse(sd<5/(10^digits_sd), 0, sd-5/(10^digits_sd))
#   # sd_upper <- sd+5/(10^digits_sd)
#   #
#   # #Computes the lower and upper bounds for the sum of squares of items.
#
#   # Lower and upper bounds for the SD:
#
#   sd_p10  <- 10 ^ digits_sd
#   sd_diff <- 5 / sd_p10
#
#   # Lower bound...
#   if (sd < sd_diff) {
#     sd_lower <- 0
#   } else {
#     sd_lower <- sd - sd_diff
#   }
#
#   # ...and upper bound:
#   sd_upper <- sd + sd_diff
#
#
#   # sum_squares_lower <- (n-1)*sd_lower^2+n*realx^2
#   # sum_squares_upper <- (n-1)*sd_upper^2+n*realx^2
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
#   # rec_sum_rounded <- round(rec_sum)
#   x_real <- rec_sum_rounded / n_items
#
#   # Lowerbound <- (n-1)*Lsigma^2+n*x_real^2
#   # Upperbound <- (n-1)*Usigma^2+n*x_real^2
#
#   sum_squares_lower <- (n - 1) * sd_lower ^ 2 + n * x_real ^ 2
#   sum_squares_upper <- (n - 1) * sd_upper ^ 2 + n * x_real ^ 2
#
#
#
#   #Checks that there is at least an integer between the lower and sum_squares_upper
#
#   # pass_test1<- ifelse(ceiling(sum_squares_lower)>floor(sum_squares_upper), FALSE, TRUE)
#   #
#   # if(pass_test1==FALSE){
#   #   return("GRIMMER inconsistent")
#   # }
#
#   integer_diff <- ceiling(sum_squares_lower) > floor(sum_squares_upper)
#   pass_test1   <- !any(integer_diff)
#
#   if (!pass_test1) {
#     return("GRIMMER inconsistent (test 1)")
#   }
#
#
#   #Takes a vector of all the integers between the sum_squares_lower and sum_squares_upper
#
#   # integers_possible <- ceiling(sum_squares_lower):floor(sum_squares_upper)
#
#   # (Jung) These values might have length > 1, so they are mapped onto below.
#   # Move them further up later
#   ceiling_lower <- ceiling(sum_squares_lower)
#   floor_upper   <- floor(sum_squares_upper)
#
#   integers_possible_list <- purrr::map2(ceiling_lower, floor_upper, seq)
#
#   #Creates the predicted variance and sd
#
#   # var_predicted <- (integers_possible-n*realx^2)/(n-1)
#   # sd_predicted <- sqrt(var_predicted)
#
#   sd_from_integers_possible <- function(integers_possible_list) {
#     out <- integers_possible_list - n * realx ^ 2
#     out <- out / (n - 1)
#     sqrt(out)
#   }
#
#   sd_rec_list <- purrr::map(integers_possible_list, sd_from_integers_possible)
#
#   #Computes whether one sd_predicted matches the sd (trying to round both down and up)
#
#   # Rounded_sd_down <- round_down(sd_predicted, digits_sd)
#   # Rounded_sd_up <- round_up(sd_predicted, digits_sd)
#
#   sd_rec_rounded_list <- purrr::map(
#     .x = sd_rec_list,
#     .f = reround,
#     digits = digits_sd,
#     rounding = rounding,
#     threshold = threshold,
#     symmetric = symmetric
#   )
#
#   sd_rec_rounded <- unlist(sd_rec_rounded_list)
#   sd_rec_rounded <- unique(sd_rec_rounded)
#
#   # pass_test2 <- Rounded_sd_down==sd | Rounded_sd_up==sd
#
#   sd_rec_is_sd <- dplyr::near(sd_rec_rounded, sd)
#   pass_test2   <- any(sd_rec_is_sd)
#
#   # if(sum(pass_test2)==0){
#   #   return("GRIMMER inconsistent")
#   # }
#
#   if(!pass_test2){
#     return("GRIMMER inconsistent (test 2)")
#   }
#
#   # test_integers_possible_for_parity <- function(integers_possible, realsum) {
#   #   integers_possible %% 2 == realsum %% 2
#   # }
#   #
#   # pass_test3 <- integers_possible_list %>%
#   #   purrr::map(test_integers_possible_for_parity, realsum) %>%
#   #   unlist() %>%
#   #   any()
#   #
#   # if (!pass_test3) {
#   #   return("GRIMMER inconsistent")
#   # }
#
#   # return(TRUE)
#
#
#   integers_possible <- unlist(integers_possible_list)
#
#
#   #Computes first whether there is any integer between lower and upper bound, and then whether there is
#   #an integer of the correct oddness between the lower and upper bounds.
#   oddness <- realsum%%2
#   Matches_Oddness <- integers_possible%%2==oddness
#   pass_test3 <- pass_test2&Matches_Oddness
#   return(ifelse(
#     sum(pass_test3)==0, "GRIMMER inconsistent", "The mean and SD are consistent.")
#   )
#
#   # GO ON HERE: Rewrite the code below with `sd_rec_rounded`
#
#   # From my earlier attempt:
#
#   rec_sum_rounded_is_even    <- is_even(rec_sum_rounded)
#   integers_possible_are_even <- is_even(integers_possible)
#
#   # Compare the Boolean values that denote even parity:
#   pass_test3 <- rec_sum_rounded_is_even == integers_possible_are_even
#
#
# }




# # Example inputs:
# n <- as.numeric(n)
# mean <- as.numeric(mean)
# SD <- as.numeric(sd)
# decimals_mean <- 2
# decimals_SD <- 2




# # My earlier attempt ------------------------------------------------------
#
# grimmer_scalar_old <- function(x, sd, n, items = 1, show_reason = FALSE,
#                                rounding = "up_or_down", threshold = 5,
#                                symmetric = FALSE,
#                                tolerance = .Machine$double.eps^0.5) {
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
#   sd_p10 <- 10 ^ digits_sd
#   sd_diff <- 5 / sd_p10
#
#   # Lower bound...
#   if (sd < sd_diff) {
#     sd_lower <- 0
#   } else {
#     sd_lower <- sd - sd_diff
#   }
#
#   # Lower bound...
#   sd_lower <- sd - sd_diff
#   sd_lower <- max(sd_lower, 0)
#
#   # ...and upper bound:
#   sd_upper <- sd + sd_diff
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
#   # Lowerbound <- (n-1)*Lsigma^2+n*x_real^2
#   # Upperbound <- (n-1)*Usigma^2+n*x_real^2
#
#   sum_squares_lower <- (n - 1) * sd_lower ^ 2 + n * x_real ^ 2
#   sum_squares_upper <- (n - 1) * sd_upper ^ 2 + n * x_real ^ 2
#
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



# Original by Allard ------------------------------------------------------

aGrimmer <- function(n, mean, SD, decimals_mean = 2, decimals_SD = 2){

  # if(n>10^decimals_mean){
  #   print("The sample size is too big compared to the precision of the reported mean, it is not possible to apply GRIM.")
  # }

  #Applies the GRIM test, and computes the possible mean.

  sum <- mean*n
  realsum <- round(sum)
  realmean <- realsum/n



  # Creates functions to round a number consistently up or down, when the last digit is 5

  round_down <- function(number, decimals=2){
    is_five <- number*10^(decimals+1)-floor(number*10^(decimals))*10
    number_rounded <- ifelse(is_five==5, floor(number*10^decimals)/10^decimals, round(number, digits = decimals))
    return(number_rounded)
  }

  round_up <- function(number, decimals=2){
    is_five <- number*10^(decimals+1)-floor(number*10^(decimals))*10
    number_rounded <- ifelse(is_five==5, ceiling(number*10^decimals)/10^decimals, round(number, digits = decimals))
    return(number_rounded)
  }

  # Applies the GRIM test, to see whether the reconstituted mean is the same as the reported mean (with both down and up rounding)

  consistency_down <- round_down(number = realmean, decimals = decimals_mean)==mean
  consistency_up <- round_up(number = realmean, decimals = decimals_mean)==mean

  if(consistency_down+consistency_up==0){
    return("GRIM inconsistent")
  }


  #Computes the lower and upper bounds for the sd.

  Lsigma <- ifelse(SD<5/(10^decimals_SD), 0, SD-5/(10^decimals_SD))
  Usigma <- SD+5/(10^decimals_SD)

  #Computes the lower and upper bounds for the sum of squares of items.

  Lowerbound <- (n-1)*Lsigma^2+n*realmean^2
  Upperbound <- (n-1)*Usigma^2+n*realmean^2

  #Checks that there is at least an integer between the lower and upperbound

  FirstTest<- ifelse(ceiling(Lowerbound)>floor(Upperbound), FALSE, TRUE)

  if(FirstTest==FALSE){
    return("GRIMMER inconsistent (test 1)")
  }

  #Takes a vector of all the integers between the lowerbound and upperbound

  Possible_Integers <- ceiling(Lowerbound):floor(Upperbound)

  #Creates the predicted variance and sd

  Predicted_Variance <- (Possible_Integers-n*realmean^2)/(n-1)
  Predicted_SD <- sqrt(Predicted_Variance)

  #Computes whether one Predicted_SD matches the SD (trying to round both down and up)

  Rounded_SD_down <- round_down(Predicted_SD, decimals_SD)
  Rounded_SD_up <- round_up(Predicted_SD, decimals_SD)

  Matches_SD <- Rounded_SD_down==SD | Rounded_SD_up==SD

  if(sum(Matches_SD)==0){
    return("GRIMMER inconsistent (test 2)")
  }

  #Computes first whether there is any integer between lower and upper bound, and then whether there is
  #an integer of the correct oddness between the lower and upper bounds.
  oddness <- realsum%%2
  Matches_Oddness <- Possible_Integers%%2==oddness
  Third_Test <- Matches_SD&Matches_Oddness
  return(ifelse(
    sum(Third_Test)==0, "GRIMMER inconsistent (test 3)", "The mean and SD are consistent.")
  )
}





#' The GRIMMER test
#'
#' @param x
#' @param sd
#' @param n
#' @param items
#' @param show_reason
#' @param rounding
#' @param threshold
#' @param symmetric
#' @param tolerance
#'
#' @return
#' @export
#'
#' @examples


# grimmer <- Vectorize(grimmer_scalar)

