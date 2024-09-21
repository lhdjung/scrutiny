

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


# Modified function in rsprite2 -------------------------------------------

# (Note the few changes I made here, explained at the appropriate places within
# the function in comments starting on "IN SCRUTINY".)

GRIMMER_test <- function(mean, sd, n_obs, m_prec = NULL, sd_prec = NULL, n_items = 1, min_val = NULL, max_val = NULL) {
  if (is.null(m_prec)) {
    m_prec <- max(nchar(sub("^[0-9]*", "", mean)) - 1, 0)
  }

  if (is.null(sd_prec)) {
    sd_prec <- max(nchar(sub("^[0-9]*", "", sd)) - 1, 0)
  }

  # IN SCRUTINY: removed calls to functions from the checkmate package --
  # scrutiny shouldn't depend on it, and they only checked input formats that
  # were a given here anyway.

  effective_n = n_obs * n_items

  # Applies the GRIM test, and computes the possible mean.
  sum <- mean * effective_n
  realsum <- round(sum)
  realmean <- realsum / effective_n

  #Checks whether mean and SD are within possible range
  if (!is.null(min_val) & !is.null(max_val)) {
    if (mean < min_val | mean > max_val) {
      warning("The mean must be between the scale minimum and maximum")
      return(FALSE)
    }
    sd_limits <- .sd_limits(n_obs, mean, min_val, max_val, sd_prec, n_items)
    if (sd < sd_limits[1] | sd > sd_limits[2]) {
      warning("Given the scale minimum and maximum, the standard deviation has to be between ", sd_limits[1], " and ", sd_limits[2], ".")
      return(FALSE)
    }
  }
  # Creates functions to round a number consistently up or down, when the last digit is 5
  round_down <- function(number, decimals = 2) {
    to_round <- number * 10^(decimals + 1) - floor(number * 10^(decimals)) * 10
    number_rounded <- ifelse(to_round == 5,
                             floor(number * 10^decimals) / 10^decimals,
                             round(number, digits = decimals))
    return(number_rounded)
  }

  round_up <- function(number, decimals = 2) {
    to_round <- number * 10^(decimals + 1) - floor(number * 10^(decimals)) * 10
    number_rounded <- ifelse(to_round == 5,
                             ceiling(number * 10^decimals) / 10^decimals,
                             round(number, digits = decimals))
    return(number_rounded)
  }

  # Applies the GRIM test, to see whether the reconstituted mean is the same as the reported mean (with both down and up rounding)

  consistent_down <- round_down(number = realmean, decimals = m_prec) == mean
  consistent_up <- round_up(number = realmean, decimals = m_prec) == mean

  if (!consistent_down & !consistent_up) {
    # IN SCRUTINY: outcommented the below warning (and turned it into a message)
    # that was thrown when the inputs were GRIM-inconsistent. I think one
    # inconsistency should (essentially) be treated like any other, and such a
    # warning is not desirable when testing. It can be incommented to check when
    # this function thinks the inputs are GRIM-inconsistent!

    # message("GRIM inconsistent - so GRIMMER test cannot be run. See ?GRIM_test")
    return(FALSE)
  }

  # Computes the lower and upper bounds for the sd.

  Lsigma <- ifelse(sd < 5 / (10^(sd_prec+1)), 0, sd - 5 / (10^(sd_prec+1)))
  Usigma <- sd + 5 / (10^(sd_prec+1))

  # Computes the lower and upper bounds for the sum of squares of items.

  lower_bound <- ((n_obs - 1) * Lsigma^2 + n_obs * realmean^2)*n_items^2
  upper_bound <- ((n_obs - 1) * Usigma^2 + n_obs * realmean^2)*n_items^2

  # Checks that there is at least an integer between the lower and upperbound

  if (ceiling(lower_bound) > floor(upper_bound)) {
    # # IN SCRUTINY: added message
    # message("Failed test 1")
    return(FALSE)
  }

  # Takes a vector of all the integers between the lowerbound and upperbound

  possible_integers <- ceiling(lower_bound):floor(upper_bound)

  # Creates the predicted variance and sd

  Predicted_Variance <- (possible_integers/n_items^2 - n_obs * realmean^2) / (n_obs - 1)
  Predicted_SD <- sqrt(Predicted_Variance)

  # Computes whether one Predicted_SD matches the SD (trying to round both down and up)

  Rounded_SD_down <- round_down(Predicted_SD, sd_prec)
  Rounded_SD_up <- round_up(Predicted_SD, sd_prec)

  Matches_SD <- Rounded_SD_down == sd | Rounded_SD_up == sd

  if (!any(Matches_SD)) {
    # # IN SCRUTINY: added message
    # message("Failed test 2")
    return(FALSE)
  }

  # Computes whether there is an integer of the correct oddness between the lower and upper bounds.
  oddness <- realsum %% 2
  Matches_Oddness <- possible_integers %% 2 == oddness

  if (!any(Matches_SD & Matches_Oddness)) {
    # # IN SCRUTINY: added message
    # message("Failed test 3")
    return(FALSE)
  }

  return(TRUE)
}



# Preparations ------------------------------------------------------------

tested_cases_orig <- 7500

# Randomly generating a great number of values in the first step leaves nearly
# as many values with exactly 2 decimal places in the second. There are the
# first values by that number greater than 1 that have exactly two decimal
# places and where the second decimal place is not zero (so it counts as a
# decimal place even without a string transformation):
df1_mean <- seq(1, length.out = tested_cases_orig, by = 0.01)
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

start1 <- Sys.time()
out1 <- purrr::pmap_lgl(df1, grimmer_scalar)
end1 <- Sys.time()
diff1 <- difftime(end1, start1, units = "secs")
# message("\nApplying `grimmer_scalar()` took:\n", round(diff1, 2), " seconds\n")

start2 <- Sys.time()
out2 <- purrr::pmap_chr(df2, aGrimmer)
end2 <- Sys.time()
diff2 <- difftime(end2, start2, units = "secs")
# message("Applying `aGrimmer()` took:\n", round(diff2, 2), " seconds\n")

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


disagree_rate <- nrow(df_disagree) / nrow(df_out)

disagree_rate

# message("The rate of disagreement between implementations is ", round(disagree_rate, 2))


df_disagree_out1_true <- df_disagree %>%
  dplyr::filter(out1)

# Proportion of cases within the disagreements where `grimmer_scalar()` thinks
# the inputs are consistent but `aGrimmer()` thinks they are not:
disagree_new_impl_true_rate <- nrow(df_disagree_out1_true) / nrow(df_disagree)

disagree_new_impl_true_rate


# The reason behind this test is that `grimmer_scalar()`, and thereby all of
# scrutiny's GRIMMER implementation, is somewhat different from the original
# `aGrimmer()` function -- circa 70 percent of the disagreements are due to
# `grimmer_scalar()` being more lenient. The overall rate of disagreement
# revolves around 0.3 percent, but due to the element of randomness and the
# relatively low number of tested cases (equal to `nrow(df_out)`), the test only
# requires the rate to be below 5 percent. This will be met absent some
# significant changes in `grimmer_scalar()`. In such a situation, it would be
# better for the test to fail.
test_that("the two functions disagree on less than 3 percent of cases", {
  disagree_rate %>% expect_lt(0.05)
})



# Resolve disagreements ---------------------------------------------------

# TODO: resolve disagreements between the implementations! Here are the only
# disagreements that occurred in hundreds of thousands of simulated test cases:
# (Note that `out1` is the result of `grimmer_scalar()`, and `out2` of
# `aGrimmer()`. Most important is that `n` is always 40 or 80!)
c(n = "40", x = "16.03",  sd = "6.41",   out1 = "TRUE", out2 = "FALSE", digits_sd = "2")
c(n = "40", x = "64.73",  sd = "25.89",  out1 = "TRUE", out2 = "FALSE", digits_sd = "2")
c(n = "80", x = "64.73",  sd = "25.89",  out1 = "TRUE", out2 = "FALSE", digits_sd = "2")
c(n = "80", x = "32.68",  sd = "13.07",  out1 = "TRUE", out2 = "FALSE", digits_sd = "2")
c(n = "40", x = "64.27",  sd = "25.71",  out1 = "TRUE", out2 = "FALSE", digits_sd = "2")
c(n = "80", x = "16.22",  sd = "6.49",   out1 = "TRUE", out2 = "FALSE", digits_sd = "2")
c(n = "40", x = "256.03", sd = "102.41", out1 = "TRUE", out2 = "FALSE", digits_sd = "2")
c(n = "40", x = "519.93", sd = "207.97", out1 = "TRUE", out2 = "FALSE", digits_sd = "2")

# # Use this to get a vector such as above:
# df_disagree %>% dplyr::slice(1) %>% unlist() %>% constructive::construct(one_liner = TRUE)

# Here they are in tibble form. Run `GRIMMER_test()` on them and see whether
# this is all due to GRIM's 40/80 leniency!
df_disagree_all <- tibble::tibble(
  n = c(40, 40, 80, 80, 40, 80, 40, 40, 40, 40, 80, 40, 40, 40),
  x = c(
    "16.03", "64.73", "64.73", "32.68", "64.27", "16.22", "256.03", "519.93",
    "32.32", "256.03", "512.33", "512.93", "513.07", "518.93"
  ),
  sd = c(
    "6.41", "25.89", "25.89", "13.07", "25.71", "6.49", "102.41", "207.97",
    "12.93", "102.41", "204.93", "205.17", "205.23", "207.57"
  ),
) %>%
  dplyr::relocate(x, sd, n) %>%
  dplyr::mutate(n = as.numeric(n))

# See if there are warnings about GRIM (!) when mapping `GRIMMER_test()`:
df_disagree_all %>%
  dplyr::rename(mean = x, n_obs = n) %>%
  dplyr::mutate(mean = as.numeric(mean), sd = as.numeric(sd)) %>%
  purrr::pmap(GRIMMER_test)



# New implementation from rsprite2 ----------------------------------------

test_that("GRIMMER works correctly by default", {
  expect_true(grimmer_scalar("5.21", "1.6", 28))
  expect_false(grimmer_scalar("3.44", "2.47", 18))
})

test_that("GRIMMER works correctly when compared to the rsprite2 implementation", {
  grimmer_scalar("1.2", "0.3",  57) %>% expect_equal(GRIMMER_test(1.2, 0.3,  57))
  grimmer_scalar("8.3", "7.5", 103) %>% expect_equal(GRIMMER_test(8.3, 7.5, 103))

  # Dealing with test-3 inconsistencies:
  grimmer_scalar("5.23", "2.55", 35)  %>% expect_equal(GRIMMER_test(5.23, 2.55, 35))
  grimmer_scalar("5.23", "2.55", 127) %>% expect_equal(GRIMMER_test(5.23, 2.55, 127))
  grimmer_scalar("5.2" , "2.5" , 35)  %>% expect_equal(GRIMMER_test(5.2 , 2.5 , 35))

  # This value set is from `pigs5`. It used to be flagged as a test-3
  # inconsistency by `grimmer_scalar()`, but it is consistent according to both
  # the new version and rsprite2:
  grimmer_scalar("2.57", "2.57", 30) %>% expect_equal(GRIMMER_test(2.57, 2.57, 30))

  # Some finer variations:
  grimmer_scalar("3.756", "4.485", 89) %>% expect_equal(GRIMMER_test(3.756, 4.485, 89))
  grimmer_scalar("3.756", "4.485", 12) %>% expect_equal(GRIMMER_test(3.756, 4.485, 12))
  grimmer_scalar("3.75",  "4.48",  12) %>% expect_equal(GRIMMER_test(3.75, 4.48, 12))
  grimmer_scalar("3.75",  "4.48",  89) %>% expect_equal(GRIMMER_test(3.75, 4.48, 89))
})

test_that("GRIMMER works correctly with `items = 2`", {
  grimmer_scalar("5.21", "1.60", 28, items = 2) %>% expect_equal(GRIMMER_test(5.21, 1.6 , 28, n_items = 2))
  grimmer_scalar("3.44", "2.47", 18, items = 2) %>% expect_equal(GRIMMER_test(3.44, 2.47, 18, n_items = 2))
})

test_that("GRIMMER works correctly with `items = 3`", {
  grimmer_scalar("5.21", "1.60", 28, items = 3) %>% expect_equal(GRIMMER_test(5.21, 1.6 , 28, n_items = 3))
  grimmer_scalar("3.44", "2.47", 18, items = 3) %>% expect_equal(GRIMMER_test(3.44, 2.47, 18, n_items = 3))
})



# test_that("sd_bounds_measure works", {
#   expect_equal(c(.45, 3.03), sd_bounds_measure(n = 5, x = 4.2, min_val = 1, max_val = 7, sd_prec = 2))
#   expect_equal(c(.27, 3.03), sd_bounds_measure(n = 5, x = 4.2, min_val = 1, max_val = 7, sd_prec = 2, items = 2))
#   expect_equal(c(0, 0), sd_bounds_measure(n = 100, x = 1, min_val = 1, max_val = 7))
# })

