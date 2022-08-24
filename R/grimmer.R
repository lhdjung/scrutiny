

# Introductory notes ------------------------------------------------------

# Analytic-GRIMMER (A-GRIMMER) was developed by Aurélien Allard
# (https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/).
# His original algorithm received some modifications here, for three reasons:
# First, tapping scrutiny's infrastructure for implementing error detection
# techniques; for example, functions like `reround()` and
# `decimal_places_scalar()`. Second, changing the return value to Boolean, which
# is the expected output from the basic implementation of any consistency test
# within scrutiny. Third, adjusting variable names to the tidyverse style guide.



# Translation of variable names -------------------------------------------

# original           --> scrutiny
# ********               ********
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



# Implementation ----------------------------------------------------------

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
      return(list(FALSE, "GRIM inconsistent"))
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
      return(list(FALSE, "GRIMMER inconsistent (test 1)"))
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

  # Introduce some numeric tolerance to the SD values before comparing them:
  sd <- dustify(sd)
  sd_rec_rounded <- dustify(sd_rec_rounded)

  matches_sd <- dplyr::near(sd, sd_rec_rounded, tol = tolerance)
  pass_test2 <- any(matches_sd[!is.na(matches_sd)])

  if (!pass_test2) {
    if (show_reason) {
      return(list(FALSE, "GRIMMER inconsistent (test 2)"))
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
      return(list(FALSE, "GRIMMER inconsistent (test 3)"))
    }
    return(FALSE)
  }

  if (show_reason) {
    return(list(TRUE, "Passed all"))
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
#' @param items *(NOTE: Don't use the `items` argument. It currently contains a
#'   bug that will be fixed in scrutiny's next CRAN release.)* Integer. The
#'   number of items composing the `x` and `sd` values. Default is 1, the most
#'   common case.
#' @param show_reason Boolean. For internal use only. If set to `TRUE`, the
#'   output is a list of length-2 lists which also contain the reasons for
#'   inconsistencies. Don't specify this manually; instead, use `show_reason` in
#'   `grimmer_map()`. Default is `FALSE`.
#'
#' @inheritParams grim
#'
#' @return Boolean. `TRUE` if `x`, `sd`, `n`, and `items` are mutually
#'   consistent, `FALSE` if not.

#' @details GRIMMER was originally devised by Anaya (2016). The present
#'   implementation follows Allard's (2018) refined Analytic-GRIMMER (A-GRIMMER)
#'   algorithm. It adapts the R function `aGrimmer()` provided by Allard and
#'   modifies it to accord with scrutiny's standards, as laid out in
#'   `vignette("consistency-tests")`, sections 1-2. The resulting `grimmer()`
#'   function, then, is a vectorized version of this basic implementation. For
#'   more context and variable name translations, see the top of the
#'   R/grimmer.R, the source file.
#'
#'   The present implementation can differ from Allard's in a small number of
#'   cases. In most cases, this means that the original flags a value set as
#'   inconsistent, but scrutiny's `grimmer*()` functions don't. For details, see
#'   the end of tests/testthat/test-grimmer.R, the `grimmer()` test file.

#' @references Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
#'   possibility of standard deviations.
#'   https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/
#'
#'   Anaya, J. (2016). The GRIMMER test: A method for testing the validity of
#'   reported measures of variability. *PeerJ Preprints.*
#'   https://peerj.com/preprints/2400v1/

#' @export
#'
#' @examples
#' # A mean of 5.23 is not consistent with an SD of 2.55
#' # and a sample size of 35:
#' grimmer(x = "5.23", sd = "2.55", n = 35)
#'
#' # However, mean and SD are consistent with a
#' # sample size of 31:
#' grimmer(x = "5.23", sd = "2.55", n = 31)
#'
#' # For a scale composed of two items:
#' grimmer(x = "2.74", sd = "0.96", n = 63, items = 2)


grimmer <- Vectorize(grimmer_scalar)

