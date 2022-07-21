


grimmer_scalar <- function(x, sd, n, items = 1, rounding = "up_or_down",
                           threshold = 5, symmetric = FALSE,
                           tolerance = .Machine$double.eps^0.5) {

  check_type(x,  "character")
  check_type(sd, "character")

  # if(n>10^decimals_x){
  #   print("The sample size is too big compared to the precision of the reported x, it is not possible to apply GRIM.")
  # }

  #Applies the GRIM test, and computes the possible x.

  digits_sd <- decimal_places_scalar(sd)

  x_orig <- x
  x  <- as.numeric(x)
  sd <- as.numeric(sd)

  n_orig <- n
  n <- n * items

  sum <- x*n
  realsum <- round(sum)
  real_x <- realsum/n



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
  # # Applies the GRIM test, to see whether the reconstituted x is the same as the reported x (with both down and up rounding)
  #
  # consistency_down <- round_down(number = real_x, decimals = decimals_x)==x
  # consistency_up <- round_up(number = real_x, decimals = decimals_x)==x
  #
  # if(consistency_down+consistency_up==0){
  #   return(FALSE)
  # }



  # It says `x_orig` because the `x` object has been coerced from character to
  # numeric, but the original string is needed here. Likewise, it says `n_orig`
  # because the `n` object has been multiplied by `items`, so if `items` is not
  # zero, taking `n` here would lead to wrong results:
  grim_consistency <- grim_scalar(
    x = x_orig, n = n_orig, items = items, rounding = rounding,
    threshold = threshold, symmetric = symmetric, tolerance = tolerance
  )

  if (!grim_consistency) {
    return(FALSE)
  }


  # #Computes the lower and upper bounds for the sd.
  #
  # sd_lower <- ifelse(sd<5/(10^digits_sd), 0, sd-5/(10^digits_sd))
  # sd_upper <- sd+5/(10^digits_sd)
  #
  # #Computes the lower and upper bounds for the sum of squares of items.
  #
  # sum_squares_lower <- (n-1)*sd_lower^2+n*real_x^2
  # sum_squares_upper <- (n-1)*sd_upper^2+n*real_x^2
  #
  # #Checks that there is at least an integer between the lower and sum_squares_upper

  p10 <- 10 ^ digits_sd
  p10_frac <- 5 / p10

  # sd bounds, lower and upper:
  if (sd < p10_frac) {
    sd_lower <- 0
  } else {
    sd_lower <- sd - p10_frac
  }

  sd_upper <- sd + p10_frac

  # Sum of squares bounds, lower and upper:
  sum_squares_lower <- (n - 1) * sd_lower ^ 2 + n * real_x ^ 2
  sum_squares_upper <- (n - 1) * sd_upper ^ 2 + n * real_x ^ 2


  # For the moment, continue to use the old variable names by assigning the
  # newly derived -- and newly named -- values to them:
  sum_squares_lower <- sum_squares_lower
  sum_squares_upper <- sum_squares_upper


  pass_test1 <- !ceiling(sum_squares_lower) > floor(sum_squares_upper)

  if (!pass_test1) {
    return(FALSE)
  }




  #Takes a vector of all the integers between the sum_squares_lower and sum_squares_upper

  integers_possible <- ceiling(sum_squares_lower):floor(sum_squares_upper)

  #Creates the predicted variance and sd

  var_predicted <- (integers_possible - n * real_x ^ 2) / (n - 1)
  sd_predicted <- sqrt(var_predicted)

  # #Computes whether one sd_predicted matches the sd (trying to round both down and up)
  #
  # Rounded_sd_down <- round_down(sd_predicted, digits_sd)
  # Rounded_sd_up <- round_up(sd_predicted, digits_sd)
  #
  # matches_sd <- Rounded_sd_down==sd | Rounded_sd_up==sd

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

  # #Computes first whether there is any integer between lower and upper bound, and then whether there is
  # #an integer of the correct oddness between the lower and upper bounds.
  # oddness <- realsum%%2
  # Matches_Oddness <- integers_possible%%2==oddness
  # pass_test3 <- matches_sd&Matches_Oddness
  # return(ifelse(
  #   sum(pass_test3)==0, FALSE, TRUE)
  # )

  parity_realsum <- realsum %% 2
  parity_integers_possible <- integers_possible %% 2

  parity_matches <- parity_realsum == parity_integers_possible

  pass_test3 <- any(matches_sd & parity_matches)

  # return(ifelse(
  #   sum(pass_test3)==0, FALSE, TRUE)
  # )

  if (!pass_test3) {
    return(FALSE)
  }

  return(TRUE)
}

