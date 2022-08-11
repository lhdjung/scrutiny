

# Original function by Allard ---------------------------------------------

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





# Preparations ------------------------------------------------------------

# Randomly generating a great number of values in the first step leaves nearly
# as many values with exactly 2 decimal places in the second. There are the
# first values by that number greater than 1 that have exactly two decimal
# places and where the second decimal place is not zero (so it counts as a
# decimal place even without a string transformation):
df1_mean <- seq(1, length.out = 1000, by = 0.01)
df1_mean <- df1_mean[decimal_places(df1_mean) == 2]

length(df1_mean)

# Random `n` values with the same number as the mean values, truncated because
# they can only be whole numbers:
df1_n <- runif(length(df1_mean), 10, 150)
df1_n <- trunc(df1_n)

# Create an example data frame:
df1 <- tibble::tibble(
  n    = df1_n,
  mean = as.character(df1_mean),
  sd   = as.character(round(as.numeric(mean) * (2 / 5), 2))
)

# # The same data frame but with a different name for the `sd` column; this is
# # just due to a naming difference between the two functions:
df1 <- df1 %>%
  dplyr::rename(SD = sd) %>%
  dplyr::mutate(mean = as.numeric(mean), SD = as.numeric(SD))

df2 <- df1

# Helper that turns the original function's string output into `TRUE` if
# consistent and `FALSE` if inconsistent:
as_logical_consistency <- function(x) {
  !stringr::str_detect(x, "inconsistent")
}



names(df1) <- c("n", "x", "sd")
df1 <- df1 %>%
  dplyr::mutate(
    x  = restore_zeros(x, width = 2),
    sd = restore_zeros(sd, width = 2)
  )



# Testing -----------------------------------------------------------------

# Apply both functions, modified and original, to data frames containing the
# same data but (possibly) different column names:
out1 <- purrr::pmap_lgl(df1, grimmer_scalar)
out2 <- purrr::pmap_chr(df2, aGrimmer)

# Convert the original function's string output to logical so that it will be
# comparable to scrutiny-style Boolean output:
out2 <- as_logical_consistency(out2)


df_out <- tibble::tibble(df1, out1, out2)
df_out <- dplyr::mutate(df_out, digits_sd = decimal_places(sd))

# The problem seems to be restricted to cases where `out1` is consistent and
# `out2` is not, and where `n` is either `40` or `80`:
df_disagree <- df_out %>%
  dplyr::filter(out1 != out2)


df_disagree


