
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




grimmer_scalar <- function(x, sd, n, items = 1, show_reason = FALSE,
                           rounding = "up_or_down",
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
    if (show_reason) {
      return(list(
        FALSE,
        "GRIM inconsistent"
      ))
    }
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
    if (show_reason) {
      return(list(
        FALSE,
        "GRIMMER inconsistent (test 1)"
      ))
    }
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
    if (show_reason) {
      return(list(
        FALSE,
        "GRIMMER inconsistent (test 2)"
      ))
    }
    return(FALSE)
  }

  # Determine if any integer between the lower and upper bounds has the correct
  # parity (the property of being even or odd):
  parity_realsum <- realsum %% 2
  parity_integers_possible <- integers_possible %% 2

  matches_parity <- parity_realsum == parity_integers_possible

  pass_test3 <- any(matches_sd & matches_parity)

  if (!pass_test3) {
    if (show_reason) {
      return(list(
        FALSE,
        "GRIMMER inconsistent (test 3)"
      ))
    }
    return(FALSE)
  }

  return(TRUE)
}




#' The GRIMMER test (granularity-related inconsistency of means mapped to error
#' repeats)
#'
#' @description `grimmer()` checks if reported mean and SD values of integer
#'   data are mathematically consistent with the reported sample size and the
#'   number of items that compose the mean value. It works much like `grim()`.
#'
#'   The function is vectorized, but it is recommended to use `grimmer_map()`
#'   for testing multiple cases.
#'
#' @param x String. The reported mean value.
#' @param sd String. The reported standard deviation.
#' @param n Integer. The reported sample size.
#' @param show_reason Not currently used.
#'
#' @inheritParams grim
#'
#' @return Boolean. `TRUE` if `x`, `sd`, `n`, and `items` are mutually
#'   consistent, `FALSE` if not.
#'
#' @export
#'
#' @examples


grimmer <- Vectorize(grimmer_scalar)

