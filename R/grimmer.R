
# Notes: Analytic-GRIMMER (A-GRIMMER) was developed by Aurélien Allard
# (https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/).
# His original algorithm received some modifications here, for three reasons:
# First, tapping scrutiny's infrastructure for implementing error detection
# techniques; for example, functions like `reround()` and
# `decimal_places_scalar()`. Second, changing the return value to Boolean, which
# is the expected output from the basic implementation of any consistency test
# within scrutiny. Third, adjusting variable names to the tidyverse style guide.



# Translation of variable names:
#
# original           --> scrutiny
# --------               --------
#
# aGrimmer           --> grimmer_scalar
# mean               --> x
# SD                 --> sd
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
# Matches_SD         --> matches_sd (to which a `pass_test2` object was added)
# Third_Test         --> pass_test3



# # Example inputs:
# n <- 40
# mean <- 1.03
# SD <- 0.41
# items <- 1
# rounding <- "up_or_down"
# threshold <- 5
# symmetric <- FALSE
# tolerance <- .Machine$double.eps^0.5
# decimals_mean <- 2
# decimals_SD <- 2




grimmer_scalar <- function(x, sd, n, items = 1, rounding = "up_or_down",
                           threshold = 5, symmetric = FALSE,
                           tolerance = .Machine$double.eps^0.5) {

  check_type(x,  "character")
  check_type(sd, "character")

  digits_sd <- decimal_places_scalar(sd)

  x_orig <- x
  x  <- as.numeric(x)
  sd <- as.numeric(sd)

  n_orig <- n
  n <- n * items

  sum <- x*n
  realsum <- round(sum)
  real_x <- realsum/n


  # GRIM test. It says `x_orig` because the `x` object has been coerced from
  # character to numeric, but the original string is needed here. Likewise, it
  # says `n_orig` because the `n` object has been multiplied by `items`, so if
  # `items` is not zero, taking `n` here would lead to wrong results:
  grim_consistency <- grim_scalar(
    x = x_orig, n = n_orig, items = items, rounding = rounding,
    threshold = threshold, symmetric = symmetric, tolerance = tolerance
  )

  if (!grim_consistency) {
    return(FALSE)
  }

  p10 <- 10 ^ digits_sd
  p10_frac <- 5 / p10

  # SD bounds, lower and upper:
  if (sd < p10_frac) {
    sd_lower <- 0
  } else {
    sd_lower <- sd - p10_frac
  }

  sd_upper <- sd + p10_frac

  # Sum of squares bounds, lower and upper:
  sum_squares_lower <- (n - 1) * sd_lower ^ 2 + n * real_x ^ 2
  sum_squares_upper <- (n - 1) * sd_upper ^ 2 + n * real_x ^ 2

  pass_test1 <- !ceiling(sum_squares_lower) > floor(sum_squares_upper)

  if (!pass_test1) {
    return(FALSE)
  }




  # Create a vector of all possible integers between the lower and upper bounds
  # of the sum of squares:
  integers_possible <- ceiling(sum_squares_lower):floor(sum_squares_upper)

  # Create the predicted variance and SD:
  var_predicted <- (integers_possible - n * real_x ^ 2) / (n - 1)
  sd_predicted <- sqrt(var_predicted)

  # Reconstruct the SD:
  sd_rec_rounded <- reround(
    x         = sd_predicted,
    digits    = digits_sd,
    rounding  = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  matches_sd <- dplyr::near(sd, sd_rec_rounded, tol = tolerance)
  pass_test2 <- any(matches_sd)

  if (!pass_test2) {
    return(FALSE)
  }

  # Determine if any integer between the lower and upper bounds has the correct
  # parity (the property of being even or odd):
  parity_realsum <- realsum %% 2
  parity_integers_possible <- integers_possible %% 2

  matches_parity <- parity_realsum == parity_integers_possible

  pass_test3 <- any(matches_sd & matches_parity)

  if (!pass_test3) {
    return(FALSE)
  }

  return(TRUE)
}



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

